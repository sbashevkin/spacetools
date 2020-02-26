#' In-water distances between points
#'
#' Calculate a distance matrix for a set of points based on in-water distances, using a raster-based approach
#' @param Water_map Object of class sf representing a map of all waterways in your region of interest
#' @param Water_map_transitioned A rasterized, transitioned, and geo-corrected version of the water map. This is optional to save time if you will be running this function frequently with the same base map.
#' @param Points A dataframe of points with latitude and longitude
#' @param Latitude_column The unquoted name of the column in the Points dataframe representing Latitude.
#' @param Longitude_column The unquoted name of the column in the Points dataframe representing Longitude.
#' @param PointID_column The unquoted name of the column in the Points dataframe with the unique identifier of each point.
#' @param Points_crs Coordinate reference system for your points dataframe. Integer with the EPSG code or character with proj4string
#' @param Grid_size Gride size (in meters) used to rasterize the map. Defaults to 75.
#' @param Calculation_crs Coordinate reference system used for the distance calculation. If a latitude/longitude system are used, errors will be returned since these calculations assume a planar surface. Defaults to \code{Calculation_crs = "+proj=utm +zone=10 ellps=WGS84"}.
#' @keywords spatial distance water raster
#' @importFrom magrittr %>%
#' @importFrom methods as
#' @importFrom rlang .data
#' @import stars
#' @return Distance matrix.
#' @examples
#' Points <- tibble::tibble(Latitude = c(38.04813, 38.05920, 37.94900, 38.23615, 38.47387),
#' Longitude = c(-121.9149, -121.8684, -121.5591, -121.6735, -121.5844),
#' ID = c("FMWT 508", "FMWT 513", "FMWT 915", "FMWT 723", "FMWT 796"))
#' distance<-Waterdist(Water_map = spacetools::Delta, Points = Points, Latitude_column = Latitude,
#' Longitude_column = Longitude, PointID_column = ID)
#' @export

Waterdist <- function(Water_map,
                      Points,
                      Latitude_column,
                      Longitude_column,
                      PointID_column,
                      Points_crs = 4326,
                      Water_map_transitioned=NULL,
                      Calculation_crs = "+proj=utm +zone=10 ellps=WGS84",
                      Grid_size = 75){

  pb<-utils::txtProgressBar(min = 0, max = 100, style=3)

  Latitude_column<-rlang::enquo(Latitude_column)
  Longitude_column<-rlang::enquo(Longitude_column)
  PointID_column<-rlang::enquo(PointID_column)

  if(rlang::as_name(PointID_column)%in%names(Water_map)){
    stop("There cannot be a column in the Water_map with the same name as the PointID_column.")
  }

  #Collapse water map into 1 multipolygon

  Water_map <- sf::st_union(Water_map)%>%
    sf::st_as_sf()%>%
    dplyr::mutate(Inside=TRUE)%>%
    dplyr::rename(geometry = .data$x)

  utils::setTxtProgressBar(pb, 5)

  Points <- Points%>%
    dplyr::select(!!Longitude_column, !!Latitude_column, !!PointID_column)%>%
    sf::st_as_sf(coords=c(rlang::as_name(Longitude_column), rlang::as_name(Latitude_column)), crs=Points_crs)%>%
    sf::st_transform(crs=Calculation_crs)

  Water_map <- sf::st_transform(Water_map, crs=Calculation_crs)

  # Are all points in the water polygon? (st_intersects returns a null object when there is no intersection)

  #Join the points to the polygon to identify the land-based point(s)

  Points_joined<-sf::st_join(Points, Water_map, join = sf::st_intersects)%>%
    dplyr::arrange(!!PointID_column)

  utils::setTxtProgressBar(pb, 10)

  # If all points are not within polygon, replace point outside polygon with closest point within polygon.
  if(!all(!is.na(Points_joined$Inside))){

    Inside <- rlang::sym("Inside")
    Inside <- rlang::enquo(Inside)
    Points_joined<-Pointmover(Points_joined, Inside, Water_map)%>%
      dplyr::arrange(!!PointID_column)
  }

  utils::setTxtProgressBar(pb, 15)

  # Are all points in the water polygon now?

  if(!all(!is.na(Points_joined$Inside))){
    stop("Points could not be moved within shapefile.")
  }

  if(is.null(Water_map_transitioned)){
    # rasterize the polygon and designate water = 1, land = 0
    # using Grid_size m x Grid_size m grid squares and rasterizing the extent of the Water_map

    rp <- stars::st_rasterize(Water_map, template=stars::st_as_stars(sf::st_bbox(Water_map), values=0, dx=75, dy=75), options="ALL_TOUCHED=TRUE")
    rp <- as(rp, Class = "Raster")

    utils::setTxtProgressBar(pb, 20)

    # measure distances between points within the polygon (i.e. water distances)
    # using the mean function in 16 directions (knight and one-cell queen moves)
    # first need to measure transitions between grid squares
    # requires geographic correction
    tp <- gdistance::transition(rp, mean, 16)

    utils::setTxtProgressBar(pb, 80)

    Water_map_transitioned <- gdistance::geoCorrection(tp, "c", scl = FALSE)

  }

  utils::setTxtProgressBar(pb, 90)

  waterDist <- gdistance::costDistance(Water_map_transitioned, sf::st_coordinates(Points_joined))

  waterDist <- as.matrix(waterDist)
  colnames(waterDist) <- rownames(waterDist) <- Points_joined%>% sf::st_drop_geometry()%>% dplyr::pull(!!PointID_column)

  utils::setTxtProgressBar(pb, 100)

  return(waterDist)
}
