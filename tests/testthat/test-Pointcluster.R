Points_clust_exp<-Pointcluster(spacetools::Stations, 1000, FALSE, Latitude, Longitude, Station, Expand=TRUE)

Points_clust<-Pointcluster(spacetools::Stations, 1000, FALSE, Latitude, Longitude, Station, Expand=FALSE)

test_that("Pointcluster produces something", {
  expect_gt(nrow(Points_clust_exp), 0)
  expect_gt(ncol(Points_clust), 0)
})

test_that("Expanded dataset is larger and stations are actually getting clustered", {
  expect_gt(nrow(Points_clust_exp), ncol(Points_clust))
})
