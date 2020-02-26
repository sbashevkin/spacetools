#' Move points within a shapefile
#'
#' Move points to closest location within shapefile
#' @param Data Dataframe of points with sf geometry column and an attribute column with NAs for points that need to be moved.
#' @param Attribute Name of the column in Data that indicates points that need to be moved (by the presence of NAs)
#' @param Shapefile Object of class sf representing the shapefile you want all points to fall within.
#' @seealso \code{\link{Waterdist}}
#' @examples
#' library(tibble)
#' library(dplyr)
#' library(sf)
#' Points <- tibble(Latitude = c(38.23333, 38.04813, 38.05920,
#'                               37.94900, 38.23615, 38.47387),
#'                  Longitude = c(-121.4889, -121.9149, -121.8684,
#'                                -121.5591, -121.6735, -121.5844),
#'                  ID = c("EMP NZP02", "FMWT 508", "FMWT 513",
#'                         "FMWT 915", "FMWT 723", "FMWT 796"))%>%
#' st_as_sf(coords=c("Longitude", "Latitude"), crs=4326)%>%
#'   st_transform(crs="+proj=utm +zone=10 ellps=WGS84")
#'
#' Map <- st_union(spacetools::Delta)%>%
#' st_as_sf()%>%
#'   mutate(Inside=TRUE)%>%
#'   rename(geometry = x)%>%
#'   st_transform(crs="+proj=utm +zone=10 ellps=WGS84")
#'
#' Points_joined <- st_join(Points, Map, join = st_intersects)
#'
#' Points_fixed<-Pointmover(Points_joined, Inside, Map)
#' @export

Pointmover <- function(Data, Attribute, Shapefile){
  Attribute <- rlang::enquo(Attribute)
  mappoints<-sf::st_cast(Shapefile, "POINT", warn=F)
  badpoints<-dplyr::filter(Data, is.na(!!Attribute))
  new<-mappoints[sf::st_nearest_feature(badpoints, mappoints),]

  new<-new%>%
    dplyr::bind_cols(badpoints%>%
                sf::st_drop_geometry()%>%
                dplyr::select(-!!Attribute))

  out<-rbind(new, dplyr::filter(Data, !is.na(!!Attribute)))
  return(out)

}
