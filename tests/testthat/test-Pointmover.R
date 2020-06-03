Points <- spacetools::Stations%>%
  st_as_sf(coords=c("Longitude", "Latitude"), crs=4326)%>%
  st_transform(crs=32610)

Map <- st_union(spacetools::Delta)%>%
  st_as_sf()%>%
  mutate(Inside=TRUE)%>%
  rename(geometry = x)%>%
  st_transform(crs=32610)

Points_joined <- st_join(Points, Map, join = st_intersects)

Points_fixed<-Pointmover(Points_joined, Inside, Map)

test_that("Pointmover produces something", {
  expect_gt(nrow(Points_fixed), 0)
})

test_that("Pointmover fixes all points", {
  expect_equal(length(which(is.na(Points_fixed$Inside))), 0)
})
