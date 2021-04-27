test_that("output is of correct type when input is a RasterLayer", {
  output <- tk_detect(tk_ponds[[1]])
  expect_type(output$cutoff.value, "double")
  expect_type(output$radii, "double")
  expect_s4_class(output$elev.crop, "RasterLayer")
  expect_s4_class(output$avg.elev[[1]], "RasterLayer")
  expect_s4_class(output$microtopography[[1]], "RasterLayer")
  expect_s4_class(output$thermokarst[[1]], "RasterLayer")
  expect_true(all(raster::getValues(output$thermokarst[[1]]) == 0 |
                    raster::getValues(output$thermokarst[[1]]) == 1))
})

test_that("output is of correct type when input is a RasterBrick", {
  output <- tk_detect(tk_ponds)
  expect_type(output$cutoff.value, "double")
  expect_type(output$radii, "double")
  expect_s4_class(output$elev.crop, "RasterBrick")
  expect_s4_class(output$avg.elev[[1]], "RasterBrick")
  expect_s4_class(output$microtopography[[1]], "RasterBrick")
  expect_s4_class(output$thermokarst[[1]], "RasterBrick")
  expect_true(all(raster::getValues(output$thermokarst[[1]]) == 0 |
                    raster::getValues(output$thermokarst[[1]]) == 1))
})

test_that("output is of correct type when input is a RasterLayer and n.cores > 1", {
  output <- tk_detect(tk_ponds[[1]], n.cores = 2)
  expect_type(output$cutoff.value, "double")
  expect_type(output$radii, "double")
  expect_s4_class(output$elev.crop, "RasterLayer")
  expect_s4_class(output$avg.elev[[1]], "RasterLayer")
  expect_s4_class(output$microtopography[[1]], "RasterLayer")
  expect_s4_class(output$thermokarst[[1]], "RasterLayer")
  expect_true(all(raster::getValues(output$thermokarst[[1]]) == 0 |
                    raster::getValues(output$thermokarst[[1]]) == 1))
})

test_that("output is of correct type when input is a RasterBrick and
          n.cores > 1", {
  output <- tk_detect(tk_ponds, n.cores = 2)
  expect_type(output$cutoff.value, "double")
  expect_type(output$radii, "double")
  expect_s4_class(output$elev.crop, "RasterBrick")
  expect_s4_class(output$avg.elev[[1]], "RasterBrick")
  expect_s4_class(output$microtopography[[1]], "RasterBrick")
  expect_s4_class(output$thermokarst[[1]], "RasterBrick")
  expect_true(all(raster::getValues(output$thermokarst[[1]]) == 0 |
                    raster::getValues(output$thermokarst[[1]]) == 1))
})

test_that("multiple radii give same result as single radius", {
  output1 <- tk_detect(tk_ponds[[1]])
  output2 <- tk_detect(tk_ponds[[1]], radii = c(10, 15))
  expect_identical(raster::getValues(output1$thermokarst[[1]]),
                   raster::getValues(output2$thermokarst[[2]]))
})

test_that("multiple layers give same result as single layer", {
  output1 <- tk_detect(tk_ponds[[1]])
  output2 <- tk_detect(tk_ponds)
  expect_identical(raster::getValues(output1$thermokarst[[1]]),
                   raster::getValues(output2$thermokarst[[1]][[1]]))
})

test_that("different cutoff values give different results", {
  output1 <- tk_detect(tk_ponds[[1]], cutoff = 0)
  output2 <- tk_detect(tk_ponds[[1]], cutoff = -0.05)
  expect_true(all(raster::getValues(output1$thermokarst[[1]]) == 0 |
                    raster::getValues(output1$thermokarst[[1]]) == 1))
  expect_true(all(raster::getValues(output2$thermokarst[[1]]) == 0 |
                    raster::getValues(output2$thermokarst[[1]]) == 1))
  expect_false(all(raster::getValues(output1$thermokarst[[1]]) ==
                     raster::getValues(output2$thermokarst[[1]])))
})

test_that("different neighborhood functions give different results", {
  output1 <- tk_detect(tk_ponds[[1]], fun = 'median')
  output2 <- tk_detect(tk_ponds[[1]], fun = 'mean')
  expect_true(all(raster::getValues(output1$thermokarst[[1]]) == 0 |
                    raster::getValues(output1$thermokarst[[1]]) == 1))
  expect_true(all(raster::getValues(output2$thermokarst[[1]]) == 0 |
                    raster::getValues(output2$thermokarst[[1]]) == 1))
  expect_false(all(raster::getValues(output1$thermokarst[[1]]) ==
                     raster::getValues(output2$thermokarst[[1]])))
})

test_that("output is being cropped to exclude NA values", {
  output <- tk_detect(tk_ponds[[1]])
  expect_false(any(is.na(raster::getValues(output$thermokarst[[1]]))))
})

test_that("neighborhood size is correct" {
  tk_ponds_coarse <- raster::aggregate(tk_ponds, fact = 2)
  output1 <- tk_detect(tk_ponds[[1]])
  output2 <- tk_detect(tk_ponds_coarse[[1]])
  expect_equal(extent(output1$thermokarst[[1]]),
               extent(output2$thermokarst[[1]]),
               tolerance = 1)
})
