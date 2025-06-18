#' Match Trees Between Measured and Inventory Maps Using 3D 2-Nearest Neighbors
#'
#' This function performs a bidirectional 3D 2-nearest-neighbors matching between two sets of trees.
#' It is bidirectional because it performs an inventory to measure matching and a measure to
#' inventory matching. It used 2-nearest-neighbors instead of 1-nearest-neighbors to give a chance to
#' each tree to get matched twice so each tree as a fall back matching reducing probabilities of collision
#' and therefore ommission and commission between from trees that would be matched to the same reference
#' using a 1-nn approache.\cr\cr
#' It uses a scaled 3D distance (based on X, Y, and DBH as a proxy for Z) to identify the best match
#' for each tree in both directions. The final match is the most consistent and closest association
#' between the two sets.
#'
#' @details
#' The algorithm works in two passes:
#' \enumerate{
#'   \item From measured trees to inventory: for each measured tree, it finds its two closest neighbors
#'         in the inventory using 3D Euclidean distance. The third dimension (Z) is synthesized from the DBH
#'         (diameter at breast height), scaled to be comparable with X and Y.
#'   \item From inventory to measured: the same process is repeated in reverse.
#' }
#' For both passes, only matches within a maximum distance (`dmax`) in the X/Y plane and a maximum DBH
#' difference (`dz`) are retained. Each inventory tree can only be assigned to one measured tree
#' (and vice versa), to prevent duplicates.\cr\cr
#' If a tree has two possible matches (one in each direction), the closest in full 3D distance is chosen.
#' In case of ambiguous matches (i.e., multiple measured trees selecting the same inventory tree), the
#' one with the shortest 3D distance is kept, and the others are discarded.\cr\cr
#' This approach is particularly useful when traditional 2D proximity does not yield reliable matches
#' due to spatial shifts and inaccuracies. The use of DBH as a proxy for vertical position improves
#' the separation of nearby stems.
#'
#' @param treemap A `TreeMapMatching` object created with [make_mapmatching()], containing the `inventory`,
#' `measured`, `center`, and `radius`.
#' @param dmax Maximum allowed horizontal (XY) distance for matching. Default is 2.
#' @param dz Maximum allowed vertical (Z/DBH) difference. Default is 0.1.
#'
#' @return A `data.table` with two columns:
#' \describe{
#'   \item{index_measure}{Index of the measured tree}
#'   \item{index_inventory}{Index of the matched inventory tree, or `NA` if no match was found}
#' }
#'
#' @importFrom data.table :=
#' @export
bidirectionnal_double_matching <- function(treemap, dmax = 2, dz = 0.1)
{
  . <- dist1 <- dist2 <- index_inv1 <- index_inv2 <- .I <- index_measure <- index_inventory <- matched_index <- distance <- NULL
  stopifnot(inherits(treemap$measured, "sf"))
  stopifnot(inherits(treemap$inventory, "sf"))

  factor = scale_z_factor(treemap$radius, treemap$inventory$DBH)

  d = as.numeric(sf::st_distance(treemap$measured, treemap$center))
  inside = d < treemap$radius
  treemap$measured$DBH[!inside] = 9999999 # They will never match

  # Extract 2D coordinates and add a synthetic Z dimension from DBH
  coords_inventory <- sf::st_coordinates(treemap$inventory)
  coords_measure   <- sf::st_coordinates(treemap$measured)

  coords_inventory_3d <- cbind(coords_inventory, treemap$inventory$DBH * factor)
  coords_measure_3d   <- cbind(coords_measure,   treemap$measured$DBH   * factor)
  dz = dz * factor

  # Extract X, Y, Z for fast access
  x_inv <- coords_inventory_3d[, 1]
  y_inv <- coords_inventory_3d[, 2]
  z_inv <- coords_inventory_3d[, 3]

  x_meas <- coords_measure_3d[, 1]
  y_meas <- coords_measure_3d[, 2]
  z_meas <- coords_measure_3d[, 3]

  # === match mls with field ======

  # Build KD-tree on inventory and get two nearest neighbors for each measured tree
  knn  <- RANN::nn2(coords_inventory_3d, coords_measure_3d, k = 2L)
  first_nn  <- knn$nn.idx[, 1]
  second_nn <- knn$nn.idx[, 2]

  # Create match table with initial distances
  match_table <- data.table::data.table(
    index_measure = seq_along(first_nn),
    index_inv1    = first_nn,
    index_inv2    = second_nn)

  # Compute 2D distances to the first and second neighbor
  match_table$dist1 <- sqrt((x_inv[first_nn]  - x_meas)^2 +  (y_inv[first_nn]  - y_meas)^2)
  match_table$dist2 <- sqrt((x_inv[second_nn] - x_meas)^2 +  (y_inv[second_nn] - y_meas)^2)

  # Discard too-distant neighbors
  match_table[dist1 > dmax, index_inv1 := NA_integer_]
  match_table[dist2 > dmax, index_inv2 := NA_integer_]

  # Compute Z distances to the first and second neighbor
  match_table$dist1 <- abs(z_inv[first_nn]  - z_meas)
  match_table$dist2 <- abs(z_inv[second_nn] - z_meas)

  # Discard too-distant neighbors
  match_table[dist1 > dz, index_inv1 := NA_integer_]
  match_table[dist2 > dz, index_inv2 := NA_integer_]

  # Compute 3D distances to the first and second neighbor
  match_table$dist1 <- sqrt((x_inv[first_nn]  - x_meas)^2 +  (y_inv[first_nn]  - y_meas)^2 + (z_inv[first_nn]  - z_meas)^2)
  match_table$dist2 <- sqrt((x_inv[second_nn] - x_meas)^2 +  (y_inv[second_nn] - y_meas)^2 + (z_inv[second_nn]  - z_meas)^2)

  # Keep only the closest measure per inventory tree for first neighbors
  best1 <- match_table[!is.na(index_inv1), .I[which.min(dist1)], by = index_inv1]
  match_table$index_inv1 <- NA_integer_
  match_table[best1$V1, index_inv1 := best1$index_inv1]

  match_table[!is.na(index_inv1), index_inv2 := NA_integer_]

  # Same for second neighbors
  best2 <- match_table[!is.na(index_inv2), .I[which.min(dist2)], by = index_inv2]
  match_table$index_inv2 <- NA_integer_
  match_table[best2$V1, index_inv2 := best2$index_inv2]

  # Avoid double assignments
  match_table[index_inv2 %in% match_table$index_inv1, index_inv2 := NA_integer_]

  # Final selection: prefer index_inv1, fallback to index_inv2
  match_table$index_inventory <- ifelse(
    is.na(match_table$index_inv1),
    match_table$index_inv2,
    match_table$index_inv1
  )

  match_table1 = match_table[, .(index_measure, index_inventory)]

  # === match field with mls ======

  tmp = coords_inventory_3d
  coords_inventory_3d = coords_measure_3d
  coords_measure_3d = tmp

  # Extract X, Y, Z for fast access
  x_inv <- coords_inventory_3d[, 1]
  y_inv <- coords_inventory_3d[, 2]
  z_inv <- coords_inventory_3d[, 3]

  x_meas <- coords_measure_3d[, 1]
  y_meas <- coords_measure_3d[, 2]
  z_meas <- coords_measure_3d[, 3]

  # Build KD-tree on inventory and get two nearest neighbors for each measured tree
  knn  <- RANN::nn2(coords_inventory_3d, coords_measure_3d, k = 2L)
  first_nn  <- knn$nn.idx[, 1]
  second_nn <- knn$nn.idx[, 2]

  # Create match table with initial distances
  match_table <- data.table::data.table(
    index_measure = seq_along(first_nn),
    index_inv1    = first_nn,
    index_inv2    = second_nn)

  # Compute 2D distances to the first and second neighbor
  match_table$dist1 <- sqrt((x_inv[first_nn]  - x_meas)^2 +  (y_inv[first_nn]  - y_meas)^2)
  match_table$dist2 <- sqrt((x_inv[second_nn] - x_meas)^2 +  (y_inv[second_nn] - y_meas)^2)

  # Discard too-distant neighbors
  match_table[dist1 > dmax, index_inv1 := NA_integer_]
  match_table[dist2 > dmax, index_inv2 := NA_integer_]

  # Compute Z distances to the first and second neighbor
  match_table$dist1 <- abs(z_inv[first_nn]  - z_meas)
  match_table$dist2 <- abs(z_inv[second_nn] - z_meas)

  # Discard too-distant neighbors
  match_table[dist1 > dz, index_inv1 := NA_integer_]
  match_table[dist2 > dz, index_inv2 := NA_integer_]

  # Compute 3D distances to the first and second neighbor
  match_table$dist1 <- sqrt((x_inv[first_nn]  - x_meas)^2 +  (y_inv[first_nn]  - y_meas)^2 + (z_inv[first_nn]  - z_meas)^2)
  match_table$dist2 <- sqrt((x_inv[second_nn] - x_meas)^2 +  (y_inv[second_nn] - y_meas)^2 + (z_inv[second_nn]  - z_meas)^2)

  # Keep only the closest measure per inventory tree for first neighbors
  best1 <- match_table[!is.na(index_inv1), .I[which.min(dist1)], by = index_inv1]
  match_table$index_inv1 <- NA_integer_
  match_table[best1$V1, index_inv1 := best1$index_inv1]

  match_table[!is.na(index_inv1), index_inv2 := NA_integer_]

  # Same for second neighbors
  best2 <- match_table[!is.na(index_inv2), .I[which.min(dist2)], by = index_inv2]
  match_table$index_inv2 <- NA_integer_
  match_table[best2$V1, index_inv2 := best2$index_inv2]

  # Avoid double assignments
  match_table[index_inv2 %in% match_table$index_inv1, index_inv2 := NA_integer_]

  # Final selection: prefer index_inv1, fallback to index_inv2
  match_table$index_inventory <- ifelse(
    is.na(match_table$index_inv1),
    match_table$index_inv2,
    match_table$index_inv1
  )

  match_table2 = match_table[, .(index_measure, index_inventory)]
  names(match_table2) = rev(names(match_table2))

  # ==== Now we need to find which best field -> mls or mls ->field

  match_table2 = match_table2[!is.na(index_measure)]
  match_table = match_table1
  match_table$index_inventory2 = NA_integer_
  match_table$index_inventory2[match_table2$index_measure] = match_table2$index_inventory

  identical = match_table$index_inventory == match_table$index_inventory2
  identical[is.na(identical)] = FALSE
  different = match_table$index_inventory != match_table$index_inventory2
  different[is.na(different)] = FALSE
  onematch = xor(is.na(match_table$index_inventory), is.na(match_table$index_inventory2))


  match_table$matched_index = NA_integer_
  match_table$matched_index[identical] = match_table$index_inventory[identical]
  match_table$matched_index[onematch] = pmax(match_table$index_inventory, match_table$index_inventory2, na.rm = TRUE)[onematch]


  # Keep only the closest measure per inventory tree for first neighbors
  tmp = match_table[different]
  xyz = sf::st_coordinates(treemap$measured)[tmp$index_measure,, drop = FALSE]
  xyz = cbind(xyz, treemap$measured$DBH[tmp$index_measure] * factor)
  XYZ1 = sf::st_coordinates(treemap$inventory)[tmp$index_inventory,, drop = FALSE]
  XYZ1 = cbind(XYZ1, treemap$inventory$DBH[tmp$index_inventory] * factor)
  XYZ2 = sf::st_coordinates(treemap$inventory)[tmp$index_inventory2,, drop = FALSE]
  XYZ2 = cbind(XYZ2, treemap$inventory$DBH[tmp$index_inventory2] * factor)
  d1 = sqrt((xyz[,1] - XYZ1[,1])^2 + (xyz[,2] - XYZ1[,2])^2 + (xyz[,3] - XYZ1[,3])^2)
  d2 = sqrt((xyz[,1] - XYZ2[,1])^2 + (xyz[,2] - XYZ2[,2])^2 + (xyz[,3] - XYZ2[,3])^2)
  idx = ifelse(d1 < d2, tmp$index_inventory, tmp$index_inventory2)
  match_table$matched_index[tmp$index_measure] = idx

  match_table = match_table[, .(index_measure, matched_index)]

  if (any(duplicated(match_table$matched_index)))
  {
    dup = duplicated(match_table$matched_index, incomparables=NA)
    dup_idx = match_table$matched_index[dup]
    dup = match_table[matched_index %in% dup_idx]

    xyz = sf::st_coordinates(treemap$measured)[dup$index_measure,, drop = FALSE]
    xyz = cbind(xyz, treemap$measured$DBH[dup$index_measure] * factor)
    XYZ = sf::st_coordinates(treemap$inventory)[dup$matched_index,, drop = FALSE]
    XYZ = cbind(XYZ, treemap$inventory$DBH[dup$matched_index] * factor)
    d = sqrt((xyz[,1] - XYZ[,1])^2 + (xyz[,2] - XYZ[,2])^2 + (xyz[,3] - XYZ[,3])^2)
    dup$distance = d

    best <- dup[, .I[which.min(distance)], by = matched_index]
    valid = dup[best$V1]

    match_table[matched_index %in% dup$matched_index, matched_index := NA_integer_]
    match_table$matched_index[valid$index_measure] = valid$matched_index
  }

  # Return final result

  names(match_table) = c("index_measure", "index_inventory")
  return(match_table)
}

#' Match measured and inventory trees using the LSAP algorithm
#'
#' This function matches trees from a measured dataset to trees in an inventory dataset
#' by solving a Linear Sum Assignment Problem (LSAP) in 3D space. The third dimension
#' is synthetically created using the DBH (Diameter at Breast Height) to improve matching accuracy.
#' Unmatched points are assigned to a dummy column with a fixed cost, allowing partial matching.
#'
#' @param treemap A TreeMapMatching object. See \link{make_mapmatching}
#' @param dmax Maximum allowed 2D distance (in projection units) to consider a match valid.
#' @param dz Maximum allowed Z (DBH or height) difference to consider a match valid.
#' @param unmatch_cost See details.
#'
#' @return A `data.table` with columns:
#'   \describe{
#'     \item{\code{index_measure}}{The index of the matched measured tree.}
#'     \item{\code{index_inventory}}{The index of the matched inventory tree, or `NA` if unmatched.}
#'   }
#' Only valid matches within distance thresholds are retained. Invalid matches are assigned `NA`.
#'
#' @details
#' This function matches trees based on their 3D positions. Since actual tree height is often unavailable,
#' the DBH (Diameter at Breast Height) is used to simulate a third dimension (Z) that helps distinguish
#' trees that may be close in 2D but differ in size. DBH values are scaled to be comparable
#' to spatial coordinates (X, Y), and distances are computed in this 3D space.\cr\cr
#' The matching is performed using the LSAP (Linear Sum Assignment Problem) algorithm
#' (\link[clue:solve_LSAP]{solve_LSAP}), which finds the best one-to-one pairings between trees
#' in the measured and inventory datasets by minimizing the total distance.\cr\cr
#' However, by design, the LSAP algorithm always attempts to match all items, even when no suitable match exists.
#' To avoid forcing poor matches, "dummy matches" are added to the cost matrix. These dummy matches represent
#' the option for a measured tree to remain unmatched, with a fixed cost (`unmatch_cost`).\cr\cr
#' If the cost of a match exceeds this fixed cost, the algorithm will prefer the dummy option.
#' After solving, these unmatched trees are identified and marked as `NA` in the result.\cr\cr
#' Choosing an appropriate `unmatch_cost` is nontrivial. It should be higher than the cost of a good match
#' to encourage valid pairings, but lower than the cost of a bad match to penalize poor assignments.
#' When `unmatch_cost = "auto"`, the algorithm prints the automatically determined value.
#' Users can then adjust this value to fine-tune the matching behavior.

#' @seealso \code{\link[clue]{solve_LSAP}} \code{\link{make_mapmatching}}
#' @export
#' @md
lsap_matching = function(treemap, dmax = 2, dz = 0.1, unmatch_cost = "auto")
{

  measured = treemap$measured
  inventory = treemap$inventory

  factor = scale_z_factor(treemap$radius, inventory$DBH)

  if (is.character(unmatch_cost))
  {
    unmatch_cost = dmax + dz * factor * 0.5
    cat("unmatch_cost =", unmatch_cost, "\n")
  }

  d = as.numeric(sf::st_distance(measured, treemap$center))
  inside = d < treemap$radius
  measured$DBH[!inside] = 99999 # They will never match

  # Extract 2D coordinates and add a synthetic Z dimension from DBH
  coords_inventory <- sf::st_coordinates(treemap$inventory)
  coords_measure   <- sf::st_coordinates(measured)

  coords_inventory_1d = treemap$inventory$DBH * factor
  coords_measure_1d = treemap$measured$DBH *factor

  coords_inventory_3d <- cbind(coords_inventory, treemap$inventory$DBH * factor)
  coords_measure_3d   <- cbind(coords_measure,   treemap$measured$DBH   * factor)

  dist_matrix <- function(A, B)
  {
    # Compute squared Euclidean distance between all pairs
    a2 <- rowSums(A^2)
    b2 <- rowSums(B^2)
    d2 <- outer(a2, b2, "+") - 2 * tcrossprod(A, B)
    sqrt(pmax(d2, 0))  # Ensure no negative values due to floating point
  }

  x1 = dist_matrix(cbind(0, coords_inventory_1d), cbind(0, coords_measure_1d))
  x2 = dist_matrix(coords_inventory, coords_measure)
  x3 = dist_matrix(coords_inventory_3d, coords_measure_3d)

  unmatch = matrix(unmatch_cost, ncol = ncol(x3), nrow = nrow(x3))
  x3 = cbind(x3, unmatch)

  y <- clue::solve_LSAP(x3)
  y

  match_table = cbind(seq_along(y), y)
  rm = match_table[,1] > nrow(inventory) |  match_table[,2] > nrow(measured)
  match_table = match_table[!rm,]

  cost1 = x1[match_table]
  cost2 = x2[match_table]

  invalid2 = which(cost2 > dmax)
  invalid1 = which(cost1 > dz * factor)
  invalid = c(invalid1, invalid2)

  match_table =  data.table::as.data.table(match_table)
  names(match_table) = c("index_inventory", "index_measure")
  match_table$index_inventory[invalid] = NA

  return(match_table)
}

scale_z_factor = function(radius, z)
{
  r = range(z)
  r = r-r[1]
  return((2*radius)/r[2])
}
