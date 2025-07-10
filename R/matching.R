#' Match trees using a specified matching method
#'
#' Applies a tree matching algorithm to a `TreeMapMatching` object and stores the result
#' in the `match_table` field.
#'
#' @param treemap A `TreeMapMatching` object created with \code{\link{make_mapmatching}}.
#' @param method A function that performs the matching. It must accept a `TreeMapMatching` object
#'   as its first argument and return a match table. Defaults to \code{\link{lsap_matching}} and
#'   is the recommended method. Previous other methods were removed from the package.
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
#' @seealso \code{\link{make_mapmatching}}, \code{\link{lsap_matching}}
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
#' treemap = match_trees(treemap, method = lsap_matching, dxymax = 2, dzmax = 30)
#' plot(treemap, scale = 2)
#' plot(treemap, rgl = TRUE)
#'
#' treemap$match_table
match_trees = function(treemap, method = lsap_matching, ...)
{
  match_table = method(treemap, ...)

  zrel = attr(match_table, "zrel")

  # Add omissions
  n <- nrow(treemap$inventory)
  full_index <- data.frame(index_inventory = 1:n)
  match_table <- merge(full_index, match_table, by = "index_inventory", all.x = TRUE)
  match_table <- match_table[order(match_table$index_inventory), ]

  # Add commission
  full_index <- 1:nrow(treemap$measured)
  b = full_index %in% match_table$index_measure
  index = full_index[!b]
  if (length(index) > 0)
  {
    missing = data.frame(index_inventory = NA, index_measure = index, cost = NA)
    match_table = rbind(match_table, missing)
  }

  cost = match_table$cost
  match_table$cost = NULL

  if (!is.null(treemap$inventory$TREEUID))
    match_table$id_inventory = treemap$inventory$TREEUID[match_table$index_inventory]

  if (!is.null(treemap$measured$TREEUID))
    match_table$id_measure = treemap$measured$TREEUID[match_table$index_measure]

  match_table$cost = cost

  # Label Omission/Commission
  match_table$state = "Matched"
  match_table$state[is.na(match_table$index_measure)] = "Omission"
  match_table$state[is.na(match_table$index_inventory)] = "Commission"
  match_table$state = as.factor(match_table$state)

  # Remove trees outside the limits
  d = as.numeric(sf::st_distance(treemap$measured, treemap$center))
  outside = d > (treemap$radius)
  outside = which(outside)
  idx = which(match_table$index_measure %in% outside)
  match_table$outside = FALSE
  match_table$outside[idx] = TRUE
  match_table = match_table[match_table$outside == FALSE | (match_table$outside == TRUE & match_table$state == "Matched"), ]
  match_table$outside = NULL

  attr(match_table, "zrel") = zrel

  # Compute scores
  tp <- sum(match_table$state == "Matched")
  fn <- sum(match_table$state == "Omission")
  fp <- sum(match_table$state == "Commission")
  precision <- tp / (tp + fp)
  recall    <- tp / (tp + fn)
  f1        <- 2 * precision * recall / (precision + recall)
  scores = list(TP = tp, FN = fn, FP = fp, precision = precision, recall = recall, Fscore = f1)

  treemap$match_table = match_table
  treemap$scores = scores

  return(treemap)
}
