#' Match trees using a specified matching method
#'
#' Applies a tree matching algorithm to a `TreeMapMatching` object and stores the result
#' in the `match_table` field.
#'
#' @param treemap A `TreeMapMatching` object created with \code{\link{make_mapmatching}}.
#' @param method A function that performs the matching. It must accept a `TreeMapMatching` object
#'   as its first argument and return a match table (typically a `data.table`).
#'   Defaults to \code{\link{lsap_matching}} and is the recommended method.
#' @param ... Additional arguments passed to the matching method.
#'
#' @return A `TreeMapMatching` object, identical to the input but with an added
#'   \code{match_table} field containing the matching results.
#'
#' @details
#' This function provides a flexible interface for applying different tree matching algorithms.
#' By default, it uses a 2 nearest-neighbors 3D matching function, but custom methods can be supplied.
#' The result is returned as an enriched `TreeMapMatching` object, ready for further analysis or plotting.
#'
#' @seealso \code{\link{make_mapmatching}}, \code{\link{bidirectionnal_double_matching}}, \code{\link{lsap_matching}}
#' @export
#' @examples
#' data(PRF025_Field)
#' PRF025_Field <- standardize(
#'   PRF025_Field,
#'   xname = "Field_Xpj",
#'   yname = "Field_Ypj",
#'   zname = "DBH",
#'   zunits = "cm",
#'   crs = 2959)
#' center = c(PRF025_Field$Easting[1], PRF025_Field$Northing[1])
#'
#' data(PRF025_Lidar)
#' PRF025_Lidar <- standardize(
#'   PRF025_Lidar,
#'   xname = "X",
#'   yname = "Y",
#'   zname = "DBH",
#'   zunits = "m",
#'   crs = 2959)
#'
#' treemap = make_mapmatching(PRF025_Field, PRF025_Lidar, center, 11.28)
#' plot(treemap, scale = 2)
#'
#' treemap = match_trees(treemap, method = lsap_matching, dxymax = 2, dzmax = 0.1)
#' plot(treemap, scale = 2)
#' plot(treemap, rgl = TRUE)
#'
#' treemap$match_table
match_trees = function(treemap, method = lsap_matching, ...)
{
  treemap$match_table = method(treemap, ...)
  return(treemap)
}
