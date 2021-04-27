#' Detect Thermokarst from Elevation
#'
#' Detect thermokarst features by comparing the elevation in each cell of a
#' digital terrain model to the average elevation in a circular neighborhood.
#'
#' @param elev A raster of elevation.
#' @param radii A number or numeric vector of neighborhood sizes (in units of
#' the crs).
#' @param fun The function to use to determine average elevation. Either median
#' or mean.
#' @param cutoff The cut-off value to use when reclassifying microtopography as
#' thermokarst.
#' @param n.cores The number of cores to use.
#'
#' @return A list containing the cutoff value used, the radii used, elev.crop,
#' avg.elev, microtopography, and thermokarst. Elev.crop is an elevation
#' raster cropped to the final output size. avg.elev is a raster of the median
#' elevation values. Microtopography is the cropped elevation minus the median
#' elevation, such that negative values indicate a low spot on the landscape
#' which may be thermokarst. Thermokarst is a raster of the thermokarst
#' classification: thermokarst (1) or non-thermokarst (0).
#' @export
#'
#' @importFrom stats median
#' @importFrom foreach %dopar%
#' @importFrom foreach %:%
#' @importFrom raster brick
#'
#' @examples
tk_detect <- function(elev, radii = 15, fun = 'median', cutoff = 0,
                      n.cores = 1) {

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
                                 c(rep(-1*max(radii), 2),
                                   rep(-1*max(radii), 2)))

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

    # create category table
    rat <- data.frame(ID = seq(0, 1),
                      Class = c('Non-Thermokarst',
                                'Thermokarst'))

    ### Calculate median elevation, microtopography, and thermokarst
    # different methods used depending on number of cores requested
    # and number of layers in input elevation data
    if (n.cores == 1) {

      # create empty output lists for each radius
      avg.elev <- list()
      microtopography <- list()
      thermokarst <- list()


      # iterate over all of the desire radii
      for (i in 1:length(radii)) {

        if (n.layers == 1) {

          # calculate median and crop output
          if (fun == 'median') {
            avg.elev[[i]] <- raster::crop(raster::focal(elev, weights[[i]], fun = median),
                                          new.extent)
          } else if (fun == 'mean') {
            avg.elev[[i]] <- raster::crop(raster::focal(elev, weights[[i]], fun = mean),
                                          new.extent)
          }
          names(avg.elev)[[i]] <- paste0('avg.elev.', radii[i])
          names(avg.elev[[i]]) <- paste0('avg.elev.', radii[i], '.', names(elev))

          # calculate microtopography
          microtopography[[i]] <- elev.crop - avg.elev[[i]]
          names(microtopography)[[i]] <- paste0('microtopography.', radii[i])
          names(microtopography[[i]]) <- paste0('microtopography.', radii[i], '.', names(elev))

          # reclassify microtopography as thermokarst
          thermokarst[[i]] <- raster::reclassify(microtopography[[i]], reclass.matrix)
          thermokarst[[i]] <- raster::ratify(thermokarst[[i]])
          levels(thermokarst[[i]]) <- rat
          names(thermokarst)[[i]] <- paste0('thermokarst.', radii[i])
          names(thermokarst[[i]]) <- paste0('thermokarst.', radii[i], '.', names(elev))


        } else if (n.layers > 1) {

          # create empty output lists for each layer nested within radius
          avg.elev[[i]] <- list()
          microtopography[[i]] <- list()
          thermokarst[[i]] <- list()

          for (j in 1:n.layers) {

            # calculate median and crop output
            if (fun == 'median') {
              avg.elev[[i]][[j]] <- raster::crop(raster::focal(elev[[j]], weights[[i]], fun = median),
                                            new.extent)
            } else if (fun == 'mean') {
              avg.elev[[i]][[j]] <- raster::crop(raster::focal(elev[[j]], weights[[i]], fun = mean),
                                            new.extent)
            }
            names(avg.elev[[i]])[[j]] <- paste0('avg.elev.', radii[i], '.', names(elev[[j]]))

            # calculate microtopography
            microtopography[[i]][[j]] <- elev.crop[[j]] - avg.elev[[i]][[j]]
            names(microtopography[[i]])[[j]] <- paste0('microtopography.', radii[i], '.', names(elev[[i]]))

            # reclassify microtopography as thermokarst
            thermokarst[[i]][[j]] <- raster::reclassify(microtopography[[i]][[j]], reclass.matrix)
            thermokarst[[i]][[j]] <- raster::ratify(thermokarst[[i]][[j]])
            levels(thermokarst[[i]][[j]]) <- rat
            names(thermokarst[[i]])[[j]] <- paste0('thermokarst.', radii[i], '.', names(elev[[i]]))

          }

          # convert lists of layers to rasterBrick and name list elements
          avg.elev[[i]] <- raster::brick(avg.elev[[i]])
          names(avg.elev)[[i]] <- paste0('avg.elev.', radii[i])
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
        avg.elev <- foreach::foreach(i=1:length(radii), .packages = 'raster') %dopar% {
          if (fun == 'median') {
          raster::crop(raster::focal(elev, weights[[i]], fun = median),
                       new.extent)
          } else if (fun == 'mean') {
            raster::crop(raster::focal(elev, weights[[i]], fun = mean),
                         new.extent)
          }
        }

        # calculate microtopography
        microtopography <- foreach::foreach(i=1:length(radii), .packages = 'raster') %dopar% {
          elev.crop - avg.elev[[i]]
        }

        # reclassify microtopography as thermokarst
        thermokarst <- foreach::foreach(i=1:length(radii), .packages = 'raster') %dopar% {
          raster::ratify(raster::reclassify(microtopography[[i]], reclass.matrix))
          }

        # Set raster category levels
        for (i in 1:length(thermokarst)) {
          levels(thermokarst[[i]]) <- rat
        }

      } else if (n.layers > 1) {

        # calculate median and crop output
        avg.elev <- foreach::foreach(i=1:length(radii), .packages = 'raster') %:%
          foreach::foreach(j=1:n.layers, .packages = 'raster', .combine = 'brick') %dopar% {
            if (fun == 'median') {
              raster::crop(raster::focal(elev[[j]], weights[[i]], fun = median),
                           new.extent)
            } else if (fun == 'mean') {
              raster::crop(raster::focal(elev[[j]], weights[[i]], fun = mean),
                           new.extent)
            }
          }

        # calculate microtopography
        microtopography <- foreach::foreach(i=1:length(radii), .packages = 'raster') %:%
          foreach::foreach(j=1:n.layers, .packages = 'raster', .combine = 'brick') %dopar% {
            elev.crop[[j]] - avg.elev[[i]][[j]]
          }

        # reclassify microtopography as thermokarst
        thermokarst <- foreach::foreach(i=1:length(radii), .packages = 'raster') %:%
          foreach::foreach(j=1:n.layers, .packages = 'raster', .combine = 'brick') %dopar% {
            raster::ratify(raster::reclassify(microtopography[[i]][[j]], reclass.matrix))
            }

        # Set raster category levels
        for (i in 1:length(thermokarst)) {
          for (j in 1:raster::nlayers(thermokarst[[i]])) {
            levels(thermokarst[[i]][[j]]) <- rat
          }
        }

      }

      # end cluster
      parallel::stopCluster(cl)

      # name output
      for (i in 1:length(radii)) {
        names(avg.elev[[i]]) <- paste0('avg.elev.', radii[i], '.', names(elev))
        names(microtopography[[i]]) <- paste0('microtopography.', radii[i], '.', names(elev))
        names(thermokarst[[i]]) <- paste0('thermokarst.', radii[i], '.', names(elev))
      }

      names(avg.elev) <- paste0('avg.elev.', radii)
      names(microtopography) <- paste0('microtopography.', radii)
      names(thermokarst) <- paste0('thermokarst.', radii)

    }

    ### Make thermokarst output categorical

    ### Create list of output
    output <- list(cutoff,
                   radii,
                   elev.crop,
                   avg.elev,
                   microtopography,
                   thermokarst)
    names(output) <- c('cutoff.value',
                       'radii',
                       'elev.crop',
                       'avg.elev',
                       'microtopography',
                       'thermokarst')
    return(output)

  }

}
