#' Detect Thermokarst from Elevation
#'
#' Detect thermokarst features by comparing the elevation in each cell of a
#' digital terrain model to the median elevation in a circular neighborhood.
#'
#' @param elev A raster of elevation.
#' @param radii A number or numeric vector of neighborhood sizes in number of
#' cells.
#' @param cutoff The cut-off value to use when reclassifying microtopography as
#' thermokarst.
#' @param n.cores The number of cores to use.
#'
#' @return A list containing the cutoff value used, the radii used, elev.crop,
#' med.elev, microtopography, and thermokarst. Elev.crop is an elevation
#' raster cropped to the final output size. Med.elev is a raster of the median
#' elevation values. Microtopography is the cropped elevation minus the median
#' elevation, such that negative values indicate a low spot on the landscape
#' which may be thermokarst. Thermokarst is a raster of the thermokarst
#' classification: thermokarst (1) or non-thermokarst (0).
#' @export
#'
#' @importFrom stats median
#' @importFrom foreach %dopar%
#' @importFrom foreach %:%
#'
#' @examples
tk_detect <- function(elev, radii = 15, cutoff = 0, n.cores = 1) {

  if (parallel::detectCores() < n.cores) {

    print('You have requested more cores than are available.')

  } else {

    # raster information
    elev.extent <- raster::extent(elev)
    resolution <- raster::res(elev)
    n.layers <- raster::nlayers(elev)

    # calculate output extent based on extent of non-NA values after
    # calculating median elevation with the largest radius
    new.extent <- raster::extend(elev.extent,
                                 c(rep(-1*(max(radii)*resolution[[1]] + resolution[[1]]), 2),
                                   rep(-1*(max(radii)*resolution[[2]] + resolution[[2]]), 2)))

    # Crop elevation to output extent
    elev.crop <- raster::crop(elev,
                              new.extent)

    # build matrices of weights which correspond to the desired radii
    weights <- list()
    for (i in 1:length(radii)) {

      weights[[i]] <- raster::focalWeight(elev, radii[i], type = 'circle')
      weights[[i]][weights[[i]] > 0] <- 1

    }

    # create reclassification matrix using the provided cut-off value
    reclass.matrix <- matrix(c(-Inf,cutoff,1, cutoff,Inf,0), ncol = 3, byrow = TRUE)


    ### Calculate median elevation, microtopography, and thermokarst
    # different methods used depending on number of cores requested
    # and number of layers in input elevation data
    if (n.cores == 1) {

      # create empty output lists for each radius
      med.elev <- list()
      microtopography <- list()
      thermokarst <- list()


      # iterate over all of the desire radii
      for (i in 1:length(radii)) {

        if (n.layers == 1) {

          # calculate median and crop output
          med.elev[[i]] <- raster::crop(raster::focal(elev, weights[[i]], fun = median),
                                        new.extent)
          names(med.elev)[[i]] <- paste0('med.elev.', radii[i])
          names(med.elev[[i]]) <- paste0('med.elev.', radii[i], '.', names(elev))

          # calculate microtopography
          microtopography[[i]] <- elev.crop - med.elev[[i]]
          names(microtopography)[[i]] <- paste0('microtopography.', radii[i])
          names(microtopography[[i]]) <- paste0('microtopography.', radii[i], '.', names(elev))

          # reclassify microtopography as thermokarst
          thermokarst[[i]] <- raster::reclassify(microtopography[[i]], reclass.matrix)
          names(thermokarst)[[i]] <- paste0('thermokarst.', radii[i])
          names(thermokarst[[i]]) <- paste0('thermokarst.', radii[i], '.', names(elev))


        } else if (n.layers > 1) {

          # create empty output lists for each layer nested within radius
          med.elev[[i]] <- list()
          microtopography[[i]] <- list()
          thermokarst[[i]] <- list()

          for (j in 1:n.layers) {

            # calculate median and crop output
            med.elev[[i]][[j]] <- raster::crop(raster::focal(elev[[j]], weights[[i]], fun = median),
                                               new.extent)
            names(med.elev[[i]])[[j]] <- paste0('med.elev.', radii[i], '.', names(elev[[j]]))

            # calculate microtopography
            microtopography[[i]][[j]] <- elev.crop[[j]] - med.elev[[i]][[j]]
            names(microtopography[[i]])[[j]] <- paste0('microtopography.', radii[i], '.', names(elev[[i]]))

            # reclassify microtopography as thermokarst
            thermokarst[[i]][[j]] <- raster::reclassify(microtopography[[i]][[j]], reclass.matrix)
            names(thermokarst[[i]])[[j]] <- paste0('thermokarst.', radii[i], '.', names(elev[[i]]))

          }

          # convert lists of layers to rasterBrick and name list elements
          med.elev[[i]] <- raster::brick(med.elev[[i]])
          names(med.elev)[[i]] <- paste0('med.elev.', radii[i])
          microtopography[[i]] <- raster::brick(microtopography[[i]])
          names(microtopography)[[i]] <- paste0('microtopography.', radii[i])
          thermokarst[[i]] <- raster::brick(thermokarst[[i]])
          names(thermokarst)[[i]] <- paste0('thermokarst.', radii[i])

        }

      }


    } else if (n.cores > 1) {

      # Register CoreCluster
      cl <- parallel::makeCluster(n.cores)
      doParallel::registerDoParallel(cl)

      if (n.layers == 1) {

        # calculate median and crop output
        med.elev <- foreach::foreach(i=1:length(radii), .packages = 'raster') %dopar% {
          raster::crop(raster::focal(elev, weights[[i]], fun = median),
                       new.extent)
        }

        # calculate microtopography
        microtopography <- foreach::foreach(i=1:length(radii), .packages = 'raster') %dopar% {
          elev.crop - med.elev[[i]]
        }

        # reclassify microtopography as thermokarst
        thermokarst <- foreach::foreach(i=1:length(radii), .packages = 'raster') %dopar% {
          raster::reclassify(microtopography[[i]], reclass.matrix)
        }

      } else if (n.layers > 1) {

        # calculate median and crop output
        med.elev <- foreach::foreach(i=1:length(radii), .packages = 'raster') %:%
          foreach::foreach(j=1:n.layers, .packages = 'raster', .combine = 'brick') %dopar% {
            raster::crop(raster::focal(elev[[j]], weights[[i]], fun = median),
                         new.extent)
          }

        # calculate microtopography
        microtopography <- foreach::foreach(i=1:length(radii), .packages = 'raster') %:%
          foreach::foreach(j=1:n.layers, .packages = 'raster', .combine = 'brick') %dopar% {
            elev.crop[[j]] - med.elev[[i]][[j]]
          }

        # reclassify microtopography as thermokarst
        thermokarst <- foreach::foreach(i=1:length(radii), .packages = 'raster') %:%
          foreach::foreach(j=1:n.layers, .packages = 'raster', .combine = 'brick') %dopar% {
            raster::reclassify(microtopography[[i]][[j]], reclass.matrix)
          }

      }

      # end cluster
      parallel::stopCluster(cl)

      # name output
      for (i in 1:length(radii)) {
        names(med.elev[[i]]) <- paste0('med.elev.', radii[i], '.', names(elev))
        names(microtopography[[i]]) <- paste0('microtopography.', radii[i], '.', names(elev))
        names(thermokarst[[i]]) <- paste0('thermokarst.', radii[i], '.', names(elev))
      }

      names(med.elev) <- paste0('med.elev.', radii)
      names(microtopography) <- paste0('microtopography.', radii)
      names(thermokarst) <- paste0('thermokarst.', radii)

    }

    ### Create list of output
    output <- list(cutoff,
                   radii,
                   elev.crop,
                   med.elev,
                   microtopography,
                   thermokarst)
    names(output) <- c('cutoff.value',
                       'radii',
                       'elev.crop',
                       'med.elev',
                       'microtopography',
                       'thermokarst')
    return(output)

  }

}
