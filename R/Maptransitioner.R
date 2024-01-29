#' Rasterize and transition a shapefile
#'
#' Rasterize and transition a shapefile for input into \code{\link{Waterdist}}
#' @inheritParams Waterdist
#' @param Process_map Should the \code{Water_map} be processed by unioning all features and transforming to the \code{Calculation_crs}? This should almost always be set to \code{TRUE}, the default.
#' @param Plot Should the rasterized map be plotted for inspection?
#' @return A rasterized, transitioned, and geo-corrected map.
#' @importFrom magrittr %>%
#' @seealso \code{\link{Waterdist}}
#' @examples
#' \dontrun{
#' Map <- Maptransitioner(spacetools::Delta)
#' }
#'
#' @export

Maptransitioner<-function(Water_map, Calculation_crs= 32610, Grid_size = 75, Process_map = TRUE, Plot = FALSE){

  pb<-utils::txtProgressBar(min = 0, max = 100, style=3)

  if(Process_map){
    Water_map <- sf::st_union(Water_map)%>%
      sf::st_as_sf()%>%
      dplyr::mutate(Inside=TRUE)%>%
      dplyr::rename(geometry = "x")%>%
      sf::st_transform(crs=Calculation_crs)
  }

  utils::setTxtProgressBar(pb, 5)

  # rasterize the polygon and designate water = 1, land = 0
  # using Grid_size m x Grid_size m grid squares and rasterizing the extent of the Water_map

  rp <- stars::st_rasterize(Water_map, template=stars::st_as_stars(sf::st_bbox(Water_map), values=0, dx=Grid_size, dy=Grid_size), options="ALL_TOUCHED=TRUE")

  if(Plot){
    graphics::plot(rp)
  }


  rp <- methods::as(rp, Class = "Raster")

  utils::setTxtProgressBar(pb, 20)

  # measure distances between points within the polygon (i.e. water distances)
  # using the mean function in 16 directions (knight and one-cell queen moves)
  # first need to measure transitions between grid squares
  # requires geographic correction
  tp <- gdistance::transition(rp, mean, 16)

  utils::setTxtProgressBar(pb, 80)

  Water_map_transitioned <- gdistance::geoCorrection(tp, "c", scl = FALSE)

  utils::setTxtProgressBar(pb, 100)

  return(Water_map_transitioned)
}
