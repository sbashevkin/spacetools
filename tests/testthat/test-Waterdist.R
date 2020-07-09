Map<-spacetools::Delta
st_crs(Map)<-4269

distance<-Waterdist(Water_map = spacetools::Delta, Points = spacetools::Stations, Latitude_column = Latitude,
                                         Longitude_column = Longitude, PointID_column = Station,
                                         Water_map_transitioned = Delta_transitioned)

test_that("Waterdist produces something", {
  expect_gt(nrow(distance), 0)
  expect_gt(ncol(distance), 0)
})

test_that("Waterdist does not produce infinities", {
  expect_true(all(is.finite(distance)))
})
