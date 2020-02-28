Map <- Maptransitioner(spacetools::Delta, Plot=TRUE)

test_that("Maptransitioner produces a transition layer", {
  expect_s4_class(Map, "TransitionLayer")
})

test_that("Maptransitioner produces same object stored in Delta_transitioned", {
  expect_equal(Map, spacetools::Delta_transitioned)
})
