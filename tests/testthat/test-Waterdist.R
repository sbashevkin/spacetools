distance<-Waterdist(Water_map = spacetools::Delta, Points = spacetools::stations, Latitude_column = Latitude,
                                         Longitude_column = Longitude, PointID_column = Station,
                                         Water_map_transitioned = spacetools::Delta_transitioned)

test_that("Waterdist produces something", {
  expect_gt(nrow(distance), 0)
  expect_gt(ncol(distance), 0)
})

test_that("Waterdist does not produce infinities", {
  expect_true(all(is.finite(distance)))
})