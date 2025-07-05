#' Create a TreeMapMatching object for tree pairing
#'
#' Prepares a structured object containing inventory trees, measured trees, a center point,
#' and a matching radius. This object is used as input for tree matching algorithms.
#'
#' @param inventory An `sf` object of class `"TreeMap"` representing the ground truth inventory trees.
#' @param measured An `sf` object of class `"TreeMap"` representing the lidar measured trees.
#' @param center A numeric vector of length 2 representing the X and Y coordinates of the center point.
#' @param radius A numeric value indicating the plot radius (in projection units). Default is 11.28.
#'
#' @return A `TreeMapMatching` object: a list with components:
#' \itemize{
#'   \item \code{inventory}: the standardized inventory `sf` object
#'   \item \code{measured}: the standardized measured `sf` object
#'   \item \code{center}: an `sfc_POINT` geometry representing the center
#'   \item \code{radius}: the radius used for filtering measured points
#' }
#'
#' @details
#' Both `inventory` and `measured` must be standardized using the \code{\link{standardize}} function.
#' The function checks that both inputs use the same coordinate reference system (CRS).
#' The center point is converted into an `sf` geometry with the same CRS as the input data.
#'
#' @seealso \code{\link{standardize}}
#' @export
#' @examples
#' data(PRF025_Field)
#' PRF025_Field <- standardize(
#'     PRF025_Field,
#'     xname = "Field_Xpj",
#'     yname = "Field_Ypj",
#'     zname = "DBH",
#'     zunits = "cm",
#'     crs = 2959)
#' center = c(PRF025_Field$Easting[1], PRF025_Field$Northing[1])
#'
#' data(PRF025_Lidar)
#' PRF025_Lidar <- standardize(
#'    PRF025_Lidar,
#'    xname = "X",
#'    yname = "Y",
#'    zname = "DBH",
#'    zunits = "m",
#'    crs = 2959)
#'
#' treemap = make_mapmatching(PRF025_Field, PRF025_Lidar, center, 11.28)
#' plot(treemap)
make_mapmatching = function(inventory, measured, center, radius = 11.28)
{
  is_standardized(inventory)
  is_standardized(measured)
  stopifnot(sf::st_crs(inventory) == sf::st_crs(measured))

  center = sf::st_point(center)
  center = sf::st_sfc(center, crs = sf::st_crs(inventory))

  ans = list(inventory = inventory, measured = measured, center = center, radius = radius)
  class(ans) = "TreeMapMatching"

  ans
}
