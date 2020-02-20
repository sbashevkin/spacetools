## code to prepare `Delta` dataset goes here

Delta <- sf::read_sf(file.path("data-raw", "DeltaShapefile"))

usethis::use_data(Delta)
