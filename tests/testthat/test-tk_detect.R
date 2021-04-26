test_that("output is of correct type when input is a RasterLayer", {
  output <- tk_detect(tk_ponds[[1]])
  expect_type(output$cutoff.value, "double")
  expect_type(output$radii, "double")
  expect_s4_class(output$elev.crop, "RasterLayer")
  expect_s4_class(output$med.elev[[1]], "RasterLayer")
  expect_s4_class(output$microtopography[[1]], "RasterLayer")
  expect_s4_class(output$thermokarst[[1]], "RasterLayer")
})

test_that("output is of correct type when input is a RasterBrick", {
  output <- tk_detect(tk_ponds)
  expect_type(output$cutoff.value, "double")
  expect_type(output$radii, "double")
  expect_s4_class(output$elev.crop, "RasterBrick")
  expect_s4_class(output$med.elev[[1]], "RasterBrick")
  expect_s4_class(output$microtopography[[1]], "RasterBrick")
  expect_s4_class(output$thermokarst[[1]], "RasterBrick")
})
