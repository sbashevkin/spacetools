Map<-spacetools::Delta
sf::st_crs(Map)<-4269

distance<-GGdist(Water_map = Map, Points = spacetools::Stations, Latitude_column = Latitude,
                    Longitude_column = Longitude, PointID_column = Station,
                    Water_map_transitioned = Delta_transitioned)

test_that("GGdist produces something", {
  expect_gt(nrow(distance), 0)
  expect_gt(ncol(distance), 0)
})

test_that("GGdist does not produce infinities", {
  expect_true(all(is.finite(distance$Distance)))
})
