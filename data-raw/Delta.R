## code to prepare `Delta` dataset goes here

Delta <- sf::read_sf(file.path("data-raw", "DeltaShapefile"))

Delta_transitioned <- spacetools::Maptransitioner(Delta)

usethis::use_data(Delta, overwrite = TRUE)

save(Delta_transitioned, file="tests/Delta_transitioned.rda")
