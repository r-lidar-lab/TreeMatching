#' Match Trees Between Measured and Inventory Maps Using 3D 2-Nearest Neighbors
#'
#' This function performs a bidirectional 3D 2-nearest-neighbors matching between two sets of trees.
#' It is bidirectional because it performs an inventory to measure matching and a measure to
#' inventory matching. It used 2-nearest-neighbors instead of 1-nearest-neighbors to give a chance to
#' each tree to get matched twice so each tree as a fall back matching reducing probabilities of collision
#' and therefore ommission and commission between from trees that would be matched to the same reference
#' using a 1-nn approach.\cr\cr
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
#' For both passes, only matches within a maximum distance (`dxymax`) in the X/Y plane and a maximum DBH
#' difference (`dzmax`) are retained. Each inventory tree can only be assigned to one measured tree
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
#' @param dxymax Maximum allowed horizontal (XY) distance for matching. Default is 2.
#' @param dzmax Maximum allowed vertical (Z/DBH) difference. Default is 0.1.
#'
#' @return A `data.table` with two columns:
#' \describe{
#'   \item{index_measure}{Index of the measured tree}
#'   \item{index_inventory}{Index of the matched inventory tree, or `NA` if no match was found}
#' }
#'
#' @importFrom data.table :=
#' @export
bidirectionnal_double_matching <- function(treemap, dxymax = 2, dzmax = 0.05)
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
  dzmax = dzmax * factor

  # Extract X, Y, Z for fast access
  x_inv <- coords_inventory_3d[, 1]
  y_inv <- coords_inventory_3d[, 2]
  z_inv <- coords_inventory_3d[, 3]

  x_meas <- coords_measure_3d[, 1]
  y_meas <- coords_measure_3d[, 2]
  z_meas <- coords_measure_3d[, 3]

  # match mls with field

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
  match_table[dist1 > dxymax, index_inv1 := NA_integer_]
  match_table[dist2 > dxymax, index_inv2 := NA_integer_]

  # Compute Z distances to the first and second neighbor
  match_table$dist1 <- abs(z_inv[first_nn]  - z_meas)
  match_table$dist2 <- abs(z_inv[second_nn] - z_meas)

  # Discard too-distant neighbors
  match_table[dist1 > dzmax, index_inv1 := NA_integer_]
  match_table[dist2 > dzmax, index_inv2 := NA_integer_]

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

  # match field with mls

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
  match_table[dist1 > dxymax, index_inv1 := NA_integer_]
  match_table[dist2 > dxymax, index_inv2 := NA_integer_]

  # Compute Z distances to the first and second neighbor
  match_table$dist1 <- abs(z_inv[first_nn]  - z_meas)
  match_table$dist2 <- abs(z_inv[second_nn] - z_meas)

  # Discard too-distant neighbors
  match_table[dist1 > dzmax, index_inv1 := NA_integer_]
  match_table[dist2 > dzmax, index_inv2 := NA_integer_]

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

#' Match measured and inventory trees solving LSAP
#'
#' Matches trees from a LiDAR-derived dataset (e.g., ALS or TLS) to trees from a field inventory
#' by solving a Linear Sum Assignment Problem (LSAP) in 3D space. The third dimension (Z)
#' is artificially constructed from DBH or tree height to improve the accuracy of positional matching.
#' To allow partial matching (i.e., unmatched trees), dummy trees are added with a fixed cost
#' (`unmatch_cost`). This prevents the algorithm from forcing poor matches.
#'
#' @param treemap A `TreeMapMatching` object. See \link{make_mapmatching}.
#' @param dxymax Maximum allowed 2D (XY) distance (in projection units) for a valid match.
#' @param dzmax Maximum allowed Z difference (e.g., DBH or height, in meters) for a valid match.
#' @param zrel Relative importance of the Z dimension compared to XY. Since Z and XY units differ,
#'   Z is scaled to be comparable. A value of 0 means Z is ignored; a value of 1 gives Z and XY equal weight.
#'   Default is 0.5 (i.e., 50% of XY weight).
#' @param unmatch_cost Fixed cost of assigning a tree to a dummy (i.e., unmatched). See Details.
#'
#' @return A `data.table` with the following columns:
#' \describe{
#'   \item{\code{index_measure}}{Index of the matched tree in the measured (LiDAR) dataset.}
#'   \item{\code{index_inventory}}{Index of the matched tree in the inventory dataset, or `NA` if unmatched.}
#' }
#' Only matches that satisfy the distance thresholds are returned. Invalid matches are marked as `NA`.
#'
#' @details
#' This function computes a matching between trees in a 3D space, where Z is not an actual height
#' but a synthetic dimension created from DBH (or tree height). The goal is to improve discrimination
#' between nearby trees of different sizes. Because DBH and spatial coordinates (XY) are not directly
#' comparable, the Z dimension is scaled using the `zrel` parameter.\cr\cr
#' Matching is performed solving LSAP using Hungarian algorithm via \link[clue:solve_LSAP]{solve_LSAP},
#' which finds the lowest-cost one-to-one pairings. However, LSAP matches all items by default,
#' even when no good match exists. To avoid poor assignments, dummy trees are introduced in the cost
#' matrix with a fixed cost (`unmatch_cost`).\cr\cr'
#' If the cost of a match exceeds `unmatch_cost`, the algorithm prefers leaving the tree unmatched.
#' These unmatched entries are then returned with `index_inventory = NA`.\cr\cr
#' Choosing a good value for `unmatch_cost` is important:
#' \itemize{
#'   \item It must be **higher than the cost of a good match**, to allow valid pairings.
#'   \item It must be **lower than the cost of a bad match**, to avoid forced poor pairings.
#' }
#' When `unmatch_cost = "auto"`, a simple heuristic is used to estimate a reasonable value,
#' which is printed to the console. Users can refine this value manually if needed.
#'
#' @seealso \link[clue]{solve_LSAP}, \link{make_mapmatching}, \link{match_trees}
#' @export
#' @md
lsap_matching = function(treemap, dxymax = 2, dzmax = 0.05, zrel = 40, unmatch_cost = "auto")
{
  inf = 9999999

  measured = treemap$measured
  inventory = treemap$inventory

  factor = scale_z_factor(treemap$radius, inventory$DBH, zrel/100)

  if (!is.numeric(unmatch_cost))
  {
    unmatch_cost = guess_unmatch_cost(treemap, dxymax, dzmax, zrel)
    cat("unmatch_cost =", unmatch_cost, "\n")
  }

  # Excessive cost for trees beyond the limit of the plot prevents
  # matching
  d = as.numeric(sf::st_distance(measured, treemap$center))
  inside = d < treemap$radius
  measured$DBH[!inside] = inf

  # Extract coordinates and add a synthetic Z dimension from DBH
  coords_inventory_1d = treemap$inventory$DBH * factor
  coords_measure_1d = treemap$measured$DBH *factor

  coords_inventory_2d <- sf::st_coordinates(treemap$inventory)
  coords_measure_2d   <- sf::st_coordinates(measured)

  coords_inventory_3d <- cbind(coords_inventory_2d, coords_inventory_1d)
  coords_measure_3d   <- cbind(coords_measure_2d,   coords_measure_1d)

  dist_matrix <- function(A, B)
  {
    # Compute squared Euclidean distance between all pairs
    a2 <- rowSums(A^2)
    b2 <- rowSums(B^2)
    d2 <- outer(a2, b2, "+") - 2 * tcrossprod(A, B)
    sqrt(pmax(d2, 0))  # Ensure no negative values due to floating point
  }

  # 1D 2D and 3D distance matrices
  # 1D allows to remove connections between trees that have a too big difference in Z (DBH)
  # 2D allows to remove connection between trees that have a too big difference in distance
  # 3D is solved as Linear Sum Assignment Problem this is the cost of assignment
  d1 = dist_matrix(cbind(0, coords_inventory_1d), cbind(0, coords_measure_1d))
  d2 = dist_matrix(coords_inventory_2d, coords_measure_2d)
  d3 = dist_matrix(coords_inventory_3d, coords_measure_3d)

  # We add an excessive cost for un matchable pair (too far, too big)
  d3[d2 > dxymax] = inf
  d3[d1 > dzmax * factor] = inf

  # We may have different number of trees to match in each dataset. LSAP is a one-to-one assignment
  # solution and does not have the capability to not match entries. Here we are adding as many placeholder
  # entries as there are trees. Thus each tree can either be matched to another tree or be matched to
  # a placeholder representing a non match. The cost of the placeholder must be carefully chosen. It
  # must be cheaper than a bad match, but more expensive than a good match.
  unmatch = matrix(unmatch_cost, ncol = ncol(d3), nrow = nrow(d3))
  d3 = cbind(d3, unmatch)

  # Now we can solve optimal assignment
  y <- clue::solve_LSAP(d3)

  match_table = cbind(seq_along(y), y)

  # We remove all placeholder assignments
  rm = match_table[,1] > nrow(inventory) |  match_table[,2] > nrow(measured)
  match_table = match_table[!rm,]

  # We check the 1d and 2d costs to remove invalid assignments
  cost1 = d1[match_table]
  cost2 = d2[match_table]
  cost3 = d3[match_table]
  invalid2 = which(cost2 > dxymax)
  invalid1 = which(cost1 > dzmax * factor)
  invalid = c(invalid1, invalid2)

  match_table =  data.table::as.data.table(match_table)
  match_table$cost = cost3
  names(match_table) = c("index_inventory", "index_measure", "cost")
  #match_table$index_inventory[invalid] = NA

  attr(match_table, "zrel") = zrel

  return(match_table)
}

scale_z_factor = function(radius, z, zrel = 1)
{
  r = range(z)
  r = r[2]-r[1]
  return(zrel*(2*radius)/r)
}

#' @rdname lsap_matching
#' @param ... unused
#' @export
guess_unmatch_cost = function(treemap, dxymax, dzmax, zrel, ...)
{
  p = list(...)
  factor = scale_z_factor(treemap$radius, treemap$inventory$DBH, zrel/100)
  unmatch_cost = dxymax + dzmax * factor * 0.5
  matched = lsap_matching(treemap, dxymax, dzmax, zrel, unmatch_cost)
  q = stats::quantile(matched$cost, probs = 0.95)
  res = as.numeric(round(q+0.5,2))

  if (!is.null(p$plot))
  {
    matched = matched[!is.na(matched$index_inventory),]
    h = graphics::hist(
      matched$cost,
      freq = FALSE,
      probability = TRUE,
      breaks = seq(0, max(matched$cost+0.5), length.out = 20),
      xlab = "Cost",
      main = "Histogram of costs with automatic unmatch_cost")
    graphics::abline(v = res, col = "red", lty = 3)
    z = max(h$density)
    graphics::text(res -0.2, z, label = "Suggested value", col = "red")
  }

  return(res)
}
