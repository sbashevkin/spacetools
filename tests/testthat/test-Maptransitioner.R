Map<-spacetools::Delta
st_crs(Map)<-4269
Map <- Maptransitioner(filter(Map, HNAME=="SAN FRANCISCO BAY"), Plot=TRUE)

test_that("Maptransitioner produces a transition layer", {
  expect_s4_class(Map, "TransitionLayer")
})
