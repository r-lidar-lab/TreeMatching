#' Standardize tree data for use in TreeMap matching
#'
#' Converts a table of tree data into a standardized spatial format with consistent naming,
#' and units and coordinate reference system to work with any user data storage convention.
#' This function prepares the input data to be used as part of a `TreeMapMatchinh` object for tree
#' matching and analysis.
#'
#' @param data A `data.frame` or `sf` object with tree records .
#' @param xname Name of the column containing X coordinates (no used with sf objects).
#' @param yname Name of the column containing Y coordinates (no used with sf objects).
#' @param zname Name of the column containing the Z dimension. Usually DBH (Diameter at Breast Height)
#' values or tree height.
#' @param zunits Unit of the zname values. Either `"cm"` (default) or `"m"` or `"mm"`. If "cm" or "mm"
#' the value will be converted to meters.
#' @param crs Coordinate Reference System to assign to the output geometry (as a `sf::st_crs()` object).
#'
#'
#' @return An `sf` object with:
#' \itemize{
#'   \item Geometry from the X and Y columns.
#'   \item A new column `ZDIM` in meters
#'   \item CRS set as specified.
#'   \item An attribute `"standardized" = TRUE`.
#' }
#'
#'
#' @examples
#' data(PRF025_Field)
#' tree_map <- standardize(
#'   PRF025_Field,
#'   xname = "Field_Xpj",
#'   yname = "Field_Ypj",
#'   zname = "DBH",
#'   zunits = "cm",
#'   crs = 2959)
#' @seealso \code{\link[sf]{st_as_sf}}, \code{\link[sf]{st_crs}}, \code{\link[dplyr]{rename_with}}
#' @export
standardize = function(data, xname, yname, zname, zunits = "cm", crs = sf::NA_crs_)
{
  match.arg(zunits, c("m", "cm", "mm"))

  if (!methods::is(data, "sf"))
  {
    data = sf::st_as_sf(data, coords = c(xname, yname))
    sf::st_crs(data) = crs
  }

  data$ZDIM = data[[zname]]
  if (zunits == "cm") data$ZDIM = data$ZDIM/100
  if (zunits == "mm") data$ZDIM = data$ZDIM/1000

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
