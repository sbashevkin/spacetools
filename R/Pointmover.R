#' Move points within a shapefile
#'
#' Move points to closest location within shapefile
#' @param Data Dataframe of points with sf geometry column and an attribute column with NAs for points that need to be moved.
#' @param Attribute Name of the column in Data that indicates points that need to be moved (by the presence of NAs)
#' @param Shapefile Object of class sf representing the shapefile you want all points to fall within.
#' @export

pointmover <- function(Data, Attribute, Shapefile){
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
