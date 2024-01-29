#' In-water distances between a set of points and the Golden Gate
#'
#' Calculate a distance matrix for a set of points based on in-water distances, using a raster-based approach.
#' @param Water_map Object of class sf representing a map of all waterways in your region of interest
#' @param Points A dataframe of points with latitude and longitude which you would like to calculate distance to a given reference point.
#' @param EndPoint A dataframe containing a single point with latitude and longitude. Defaults to a point 92 m East of the Golden Gate (37.819539, -122.477). Latitude and longitude column names must be the same as the points data frame. This point must fall within the bounds of the \code{Water_map}.
#' @param Latitude_column The unquoted name of the column in the \code{Points} and \code{EndPoint} dataframe representing Latitude.
#' @param Longitude_column The unquoted name of the column in the \code{Points} and \code{EndPoint} dataframe representing Longitude.
#' @param PointID_column The unquoted name of the column in the \code{Points} dataframe with the unique identifier of each point.
#' @param Points_crs Coordinate reference system for your \code{Points} dataframe. Integer with the EPSG code or character with proj4string.
#' @param Water_map_transitioned A rasterized, transitioned, and geo-corrected version of the water map. This is optional to save time if you will be running this function frequently with the same base map.
#' @param Grid_size Grid size (in meters) used to rasterize the map. Defaults to 75.
#' @param Calculation_crs Coordinate reference system used for the calculation. If a latitude/longitude system are used, errors may be returned since some calculations assume a planar surface. Defaults to \code{Calculation_crs = 32610}.
#' @keywords spatial distance water raster
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @return A tibble with 1 column for the PointID_column values and another with the distances from the EndPoint.
#' @seealso \code{\link{Maptransitioner}} \code{\link{Pointmover}}
#' @examples
#' library(tibble)
#'
#' Points <- tibble(Latitude = c(38.23333, 38.04813, 38.05920,
#'                                       37.94900, 38.23615, 38.47387),
#'                          Longitude = c(-121.4889, -121.9149, -121.8684,
#'                                        -121.5591, -121.6735, -121.5844),
#'                          ID = c("EMP NZP02", "FMWT 508", "FMWT 513",
#'                                 "FMWT 915", "FMWT 723", "FMWT 796"))
#'
#' \dontrun{
#' distance<GGdist(Water_map = spacetools::Delta, Points = Points, Latitude_column = Latitude,
#'                     Longitude_column = Longitude, PointID_column = ID)
#'
#'
#' # Including a pre-transitioned map to save time.
#' # See Maptransitioner for creating this pre-transitioned map.
#'
#' distance<-GGdist(Water_map = spacetools::Delta, Points = Points, Latitude_column = Latitude,
#'                     Longitude_column = Longitude, PointID_column = ID,
#'                     Water_map_transitioned = Delta_transitioned)
#'                     }
#' @export

GGdist <- function(Water_map,
                   Points,
                   EndPoint = NULL,
                   Latitude_column,
                   Longitude_column,
                   PointID_column,
                   Points_crs = 4326,
                   Water_map_transitioned = NULL,
                   Calculation_crs = 32610,
                   Grid_size = 75){

  pb<-utils::txtProgressBar(min = 0, max = 100, style=3)
  Latitude_column<-rlang::enquo(Latitude_column)
  Longitude_column<-rlang::enquo(Longitude_column)
  PointID_column<-rlang::enquo(PointID_column)

  if(rlang::as_name(PointID_column)%in%names(Water_map)){
    stop("There cannot be a column in the Water_map with the same name as the PointID_column.")
  }

  #Collapse water map into 1 multipolygon

  Water_map <- Water_map%>%
    sf::st_transform(crs=Calculation_crs)%>%
    sf::st_union()%>%
    sf::st_as_sf()%>%
    dplyr::mutate(Inside=TRUE)%>%
    dplyr::rename(geometry = "x")

  utils::setTxtProgressBar(pb, 5)

  Points <- Points%>%
    dplyr::select(!!Longitude_column, !!Latitude_column, !!PointID_column)%>%
    sf::st_as_sf(coords=c(rlang::as_name(Longitude_column), rlang::as_name(Latitude_column)), crs=Points_crs)%>%
    sf::st_transform(crs=Calculation_crs)

  #Create a data frame containing a single point (the Golden Gate)
  #with the same names as the latitude and longitude columns in the main data set.


  if(is.null(EndPoint)){
    EndPoint <- tibble::tibble(!!Longitude_column := -122.477,
                               !!Latitude_column := 37.819539) %>%
      sf::st_as_sf(coords=c(rlang::as_name(Longitude_column), rlang::as_name(Latitude_column)), crs=Points_crs)%>%
      sf::st_transform(crs=Calculation_crs)

  } else {
    EndPoint<-EndPoint%>%
      dplyr::select(!!Longitude_column, !!Latitude_column)%>%
      sf::st_as_sf(coords=c(rlang::as_name(Longitude_column), rlang::as_name(Latitude_column)), crs=Points_crs)%>%
      sf::st_transform(crs=Calculation_crs)

  }

  EndPoint_joined<-sf::st_join(EndPoint, Water_map, join = sf::st_intersects)

  if(!all(!is.na(EndPoint_joined$Inside))){
    stop("The Endpoint is not within the water shapefile. Please choose an Endpoint within the water shapefile")
  }

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
  } else{
    message("Not all points were within the water shapefile so they were moved to fall within the water shapefile using spacetools::Pointmover.")
  }

  if(is.null(Water_map_transitioned)){

    Water_map_transitioned <- Maptransitioner(Water_map = Water_map, Calculation_crs = Calculation_crs,
                                              Grid_size = Grid_size, Process_map = TRUE)

  }

  utils::setTxtProgressBar(pb, 70)

  waterDist <- gdistance::costDistance(Water_map_transitioned,
                                       fromCoords =  sf::st_coordinates(Points_joined),
                                       toCoords = sf::st_coordinates(EndPoint))
  waterDist<-tibble::tibble(!!PointID_column:=Points_joined%>% sf::st_drop_geometry()%>% dplyr::pull(!!PointID_column), Distance=as.numeric(waterDist))

  utils::setTxtProgressBar(pb, 100)

  return(waterDist)
}

