#' Delta shapefile
#'
#' Shapefile of delta waterways
#'
#' @format a sf tibble with 282 rows and 10 columns.
#' \describe{
#'   \item{AREA}{Area.}
#'   \item{PERIMETER}{Perimeter.}
#'   \item{HYDRO_POLY}{HYDRO_POLY.}
#'   \item{HYDRO_PO_1}{HYDRO_PO_1.}
#'   \item{HYDRO_24K_}{HYDRO_24K_.}
#'   \item{HNAME}{HNAME.}
#'   \item{Shape_Leng}{Shape_Leng.}
#'   \item{Shape_Area}{Shape_Area.}
#'   \item{geometry}{Shapefile polygon coordinates.}
#' }
"Delta"

#' Rasterized and transitioned Delta shapefile
#'
#' Shapefile of delta waterways that has been rasterized and transitioned for Waterdist.
#'
#' @format a TransitionLayer from package \link{gdistance}.
"Delta_transitioned"
