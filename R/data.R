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
#'   \item{Shape_Leng}{Shape_Length}
#'   \item{Shape_Area}{Shape_Area.}
#'   \item{geometry}{Shapefile polygon coordinates.}
#' }
"Delta"

#' Zooplankton sampling stations
#'
#' Zooplankton sampling stations in the Sacramento San Joaquin Delta from the zooper package.
#'
#' @format a tibble with 362 rows and 3 columns
#' \describe{
#'   \item{Station}{Sampling station name}
#'   \item{Latitude}{Latitude in decimal degrees}
#'   \item{Longitude}{Longitude in decimal degrees}
#' }
#'
#' @seealso \href{https://github.com/InteragencyEcologicalProgram/zooper}{zooper}
"stations"
