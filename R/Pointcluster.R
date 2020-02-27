#' Cluster nearby points
#'
#' Cluster points within a given distance of one another
#' @inheritParams Waterdist
#' @param Distance Clustering distance in meters.
#' @param Expand Should data be expanded at the end? If \code{TRUE} (the default), the result will be expanded so there is 1 row per unique value of the \code{PointID_column}. If \code{FALSE} , the returned object will have 1 row per cluster and a list column with the names of each \code{PointID_column} value contained in each cluster.
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @importFrom rlang :=
#' @return A tibble relating each point to its assigned cluster. The returned Latitude and Longitude values represent the mean values across all points in each cluster.
#' @examples
#' library(tibble)
#' Points <- tibble(Latitude = c(38.07194, 38.09306, 38.11722,
#'                               38.11528, 38.07020, 38.09383,
#'                               38.11783, 38.06481, 38.11400,
#'                               38.06750, 38.11556),
#'                  Longitude = c(-122.0961, -122.0692, -122.0472,
#'                                -122.0519, -122.0941, -122.0697,
#'                                -122.0418, -122.0978, -122.0462,
#'                                -122.0956, -122.0424),
#'                  ID = c("EMP NZ022", "EMP NZ024", "EMP NZ028",
#'                         "EMP NZ030", "FMWT 416", "FMWT 418",
#'                         "FMWT 602", "TNS 418", "TNS 602",
#'                         "twentymm 418", "twentymm 602"))
#' Points_clust<-Pointcluster(Points, 1000, Latitude, Longitude, ID, Expand=TRUE)
#' @export

Pointcluster <- function(Points,
                         Distance,
                         Latitude_column,
                         Longitude_column,
                         PointID_column,
                         Points_crs = 4326,
                         Calculation_crs = "+proj=utm +zone=10 ellps=WGS84",
                         Expand = TRUE){

  Latitude_column <- rlang::enquo(Latitude_column)
  Longitude_column <- rlang::enquo(Longitude_column)
  PointID_column <- rlang::enquo(PointID_column)

  Points<-Points%>%
    dplyr::select(!!Latitude_column, !!Longitude_column, !!PointID_column)%>%
    tidyr::drop_na()

  Points_clust <- Points%>%
    sf::st_as_sf(coords=c(rlang::as_name(Longitude_column), rlang::as_name(Latitude_column)), crs=Points_crs)%>%
    sf::st_transform(crs=Calculation_crs)

  clusters <- stats::hclust(stats::dist(sf::st_coordinates(Points_clust)), method="complete")

  # Compute distance
  clusters <- stats::cutree(clusters, h=Distance)

  # Join clusters to original Point IDs
  Points_clust <- Points_clust%>%
    dplyr::mutate(Clust = clusters)%>%
    sf::st_drop_geometry()%>%
    dplyr::select(!!PointID_column, .data$Clust)

  # Join clusters to original dataset
  Points_clust<-Points%>%
    dplyr::left_join(Points_clust, by=rlang::as_name(PointID_column))%>%
    dplyr::group_by(.data$Clust)%>%
    dplyr::summarise(!!Latitude_column := mean(!!Latitude_column), !!Longitude_column := mean(!!Longitude_column), !!PointID_column := list(!!PointID_column))%>%
    dplyr::ungroup()%>%
    {if(Expand){
      tidyr::unnest(data=., cols=!!PointID_column)
    } else{
      .
    }}
}

utils::globalVariables(".")
