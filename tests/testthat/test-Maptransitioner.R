Map <- Maptransitioner(filter(spacetools::Delta, HNAME=="SAN FRANCISCO BAY"), Plot=TRUE)

test_that("Maptransitioner produces a transition layer", {
  expect_s4_class(Map, "TransitionLayer")
})
