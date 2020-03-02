#' spacetools: Easy spatial tools for R
#'
#' This package contains functions for easy spatial operations in R, focused on aquatic distance related applications. Internal datasets for the Sacramento San Joaquin Delta are also included to make operations very easy for this location.
#'
#' @section Functions:
#' \itemize{
#'   \item \code{\link{Waterdist}}
#'   \item \code{\link{Pointmover}}
#'   \item \code{\link{Maptransitioner}}
#'   \item \code{\link{Pointcluster}}
#' }
#'
#' @section Internal datasets:
#' \itemize{
#'   \item \code{\link{Delta}}
#'   \item \code{\link{Stations}}
#' }
#' @docType package
#' @name spacetools
NULL

## quiets concerns of R CMD check re: the .'s that appear in pipelines
if(getRversion() >= "2.15.1")  utils::globalVariables(c("."))
