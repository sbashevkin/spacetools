## code to prepare `Delta` dataset goes here

Delta <- sf::read_sf(file.path("data-raw", "DeltaShapefile"))

Delta_transitioned <- spacetools::Maptransitioner(Delta)

usethis::use_data(Delta, Delta_transitioned, overwrite = TRUE)
