#' Standardize tree data for use in TreeMap matching
#'
#' Converts a table of tree data into a standardized spatial format with consistent naming,
#' DBH units, and coordinate reference system to work with any user data storage convention.
#' This function prepares the input data to be used as part of a `TreeMapMatchinh` object for tree
#' matching and analysis.
#'
#' @param data A `data.frame` with tree records.
#' @param xname Name of the column containing X coordinates.
#' @param yname Name of the column containing Y coordinates.
#' @param dbhname Name of the column containing DBH (Diameter at Breast Height) values.
#' @param dbhunits Unit of the DBH values. Either `"cm"` (default) or `"m"`. If "cm" the value will
#' be converted to meters.
#' @param crs Coordinate Reference System to assign to the output geometry (as a `sf::st_crs()` object).
#'
#' @return An `sf` object with:
#' \itemize{
#'   \item Geometry from the X and Y columns.
#'   \item A renamed column `DBH` in meters
#'   \item CRS set as specified.
#'   \item An attribute `"standardized" = TRUE`.
#'   \item Class modified to include `"TreeMap"`.
#' }
#'
#' @details
#' The function renames the user-provided DBH column to a standardized `DBH` name and ensures
#' the unit is in centimeters. If DBH was originally in meters, it is converted by multiplying by 100.
#' The object returned inherits from class `"TreeMap"`, making it compatible with downstream
#' functions that expect standardized input.
#'
#' @examples
#' data(PRF025_Field)
#' tree_map <- standardize(
#'   PRF025_Field,
#'   xname = "Field_Xpj",
#'   yname = "Field_Ypj",
#'   dbhname = "DBH",
#'   dbhunits = "cm",
#'   crs = 2959)
#' @seealso \code{\link[sf]{st_as_sf}}, \code{\link[sf]{st_crs}}, \code{\link[dplyr]{rename_with}}
#' @export
standardize = function(data, xname, yname, dbhname, dbhunits = "cm", crs = sf::NA_crs_)
{
  match.arg(dbhunits, c("m", "cm"))
  data = sf::st_as_sf(data, coords = c(xname, yname))
  names(data)[names(data) == dbhname] <- "DBH"
  if (dbhunits == "cm") data$DBH = data$DBH/100
  sf::st_crs(data) = crs
  attr(data, "standardized") = TRUE
  data
}

is_standardized = function(x)
{
  if (!isTRUE(attr(x, "standardized")))
    stop("The function 'standardize()' has not been called on the data")
}

get_center = function(x)
{
  return(x$center)
}
