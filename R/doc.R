#' Tree Map From Mobile Lidar Data
#'
#' Tree Map From Mobile Lidar Data
#'
#' @format A data frame with 2 rows and 9 variables:
#' \describe{
#'   \item{treeID}{Integer. Unique identifier for the measured tree.}
#'   \item{plotID}{Character. Identifier of the plot where the tree is located.}
#'   \item{TH}{Numeric. Tree height in meters.}
#'   \item{DBH}{Numeric. Diameter at breast height in meters.}
#'   \item{X}{Numeric. X coordinate (e.g., UTM Easting).}
#'   \item{Y}{Numeric. Y coordinate (e.g., UTM Northing).}
#'   \item{Z}{Numeric. Elevation above sea level in meters.}
#'   \item{rmse}{Numeric. Not documented.}
#'   \item{angle_range}{Numeric. Not documented.}
#' }
#'
#' @source Generated from MLS lidar data.
#'
#' @examples
#' data(PRF025_Lidar)
#' head(PRF025_Lidar)
"PRF025_Lidar"

#' Tree Map From Field Measurement
#'
#' Tree Map From Field Measureme
#'
#' @format A data frame with one row and 19 variables:
#' \describe{
#'   \item{Plot}{Character. Plot identifier.}
#'   \item{Tree}{Integer. Tree identifier within the plot.}
#'   \item{Spp}{Character. Species code.}
#'   \item{Status}{Character. Tree status (e.g., L = living, D = dead).}
#'   \item{Origin}{Character. Tree origin (e.g., N = natural, P = planted).}
#'   \item{Dist}{Numeric. Distance from plot center to the tree (in meters).}
#'   \item{Azim}{Numeric. Azimuth from plot center to the tree (in degrees).}
#'   \item{DBH}{Numeric. Diameter at breast height (in cm).}
#'   \item{DbhHt}{Numeric. Height at which DBH was measured (in meters).}
#'   \item{Ht}{Numeric. Tree height (in meters).}
#'   \item{LI}{Not documented}
#'   \item{LO}{Not documented}
#'   \item{BT}{Not documented}
#'   \item{Plotsize}{Character. Size class of the plot (e.g., "Large").}
#'   \item{Dbh_meas}{Numeric. Measured DBH (in cm). Redundant with `DBH`?}
#'   \item{Field_X}{Numeric. Local X coordinate of tree in field measurement system.}
#'   \item{Field_Y}{Numeric. Local Y coordinate of tree in field measurement system.}
#'   \item{Field_Xpj}{Numeric. Projected X coordinate (e.g., UTM Easting).}
#'   \item{Field_Ypj}{Numeric. Projected Y coordinate (e.g., UTM Northing).}
#'   \item{Easting}{Numeric. Projected X coordinate plot center.}
#'   \item{Northing}{Numeric. Projected Y coordinate plot center.}
#' }
#'
#' @source Derived from field surveys in forestry plots.
#'
#' @examples
#' data(PRF025_Field)
#' head(PRF025_Field)
"PRF025_Field"
