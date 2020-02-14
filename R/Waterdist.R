#' In-water distances between points
#'
#' Calculate a distance matrix for a set of points based on in-water distances, using a raster-based approach
#' @param Water_map Object of class sf representing a map of all waterways in your region of interest
#' @param Points A dataframe of points with latitude and longitude
#' @param Attribute The name of a shapefile attribute that has values for all water bodies.
#' @param Latitude_column The unquoted name of the column in the Points dataframe representing Latitude.
#' @param Longitude_column The unquoted name of the column in the Points dataframe representing Longitude.
#' @param PointID_column The unquoted name of the column in the Points dataframe with the unique identifier of each point.
#' @param Points_crs Coordinate reference system for your points dataframe. Integer with the EPSG code or character with proj4string
#' @keywords spatial distance water raster
#' @importFrom magrittr %>%
#' @return Distance matrix.
#'
#' @export

Waterdist <- function(Water_map,
                      Points,
                      Attribute,
                      Latitude_column,
                      Longitude_column,
                      PointID_column,
                      Points_crs = 4326){

  pb <- progress::progress_bar$new(total = 100)
  pb$tick(0)

  Attribute<-rlang::enquo(Attribute)
  Latitude_column<-rlang::enquo(Latitude_column)
  Longitude_column<-rlang::enquo(Longitude_column)
  PointID_column<-rlang::enquo(PointID_column)

  Points <- Points%>%
    dplyr::select(!!Longitude_column, !!Latitude_column, !!PointID_column)%>%
    sf::st_as_sf(coords=c(rlang::as_name(Longitude_column), rlang::as_name(Latitude_column)), crs=Points_crs)%>%
    sf::st_transform(crs="+proj=utm +zone=10 ellps=WGS84")

  # Are all points in the water polygon? (st_intersects returns a null object when there is no intersection)

  #Join the points to the polygon to identify the land-based point(s)

  Points_joined<-sf::st_join(Points, Water_map, join = sf::st_intersects)%>%
    dplyr::arrange(!!PointID_column)

  # If all points are not within polygon, replace point outside polygon with closest point within polygon.
  if(!all(!is.na(dplyr::pull(Points_joined, !!Attribute)))){
    Points_joined<-pointmover(Points_joined, !!Attribute, Water_map)%>%
      dplyr::arrange(!!PointID_column)
  }

  pb$tick(10)

  # Are all points in the water polygon now?

  if(!(length(unlist(sf::st_intersects(Points_joined, Water_map)))==nrow(Points))){
    stop("Points could not be moved within shapefile.")
  }

  # rasterize the polygon and designate water = 1, land = 0
  # using 75 m x 75 m grid squares and rasterizing the extent of the Water_map

  mapextent<-sf::st_bbox(Water_map)

  cols <- round((mapextent["xmax"] - mapextent["xmin"]) / 75) # 1394 columns
  rows <- round((mapextent["ymax"] - mapextent["ymin"]) / 75) # 1500 rows

  r <- raster::raster(ncol = cols, nrow = rows)
  raster::extent(r) <- raster::extent(c(mapextent["xmin"], mapextent["xmax"], mapextent["ymin"], mapextent["ymax"]))
  rp <- fasterize::fasterize(Water_map, r)
  rp[is.na(rp)] <- 0

  # measure distances between points within the polygon (i.e. water distances)
  # using the mean function in 16 directions (knight and one-cell queen moves)
  # first need to measure transitions between grid squares
  # requires geographic correction

  tp <- gdistance::transition(rp, mean, 16)

  pb$tick(80)

  tpc <- gdistance::geoCorrection(tp, "c", scl = FALSE)

  pb$tick(90)

  waterDist <- gdistance::costDistance(tpc, sf::st_coordinates(Points_joined))

  waterDist <- as.matrix(waterDist)
  colnames(waterDist) <- rownames(waterDist) <- Points_joined%>% sf::st_drop_geometry()%>% dplyr::pull(!!PointID_column)

  return(waterDist)
}
