#' @exportS3Method
print.TreeMapMatching <- function(x, ...)
{
  inv <- x$inventory
  cat("Inventory:", nrow(inv), "trees\n")

  meas <- x$measured
  cat("Measured trees:",nrow(meas), "trees\n")

  cat("Plot center: (", sf::st_coordinates(x$center)[1], ", ", sf::st_coordinates(x$center)[2], ")\n", sep = "")
  cat("Plot radius:", x$radius, "\n")

  if (!is.null(x$match_table))
  {

  # Matching
  cat("Matching Table:\n")
  states <- table(x$match_table$state, useNA = "ifany")
  for (s in names(states)) cat(" - ", s, ": ", states[[s]], "\n", sep = "")

  # Scores
  s <- x$scores
  cat("Scores:\n")
  cat(sprintf(" - Precision: %.3f\n - Recall: %.3f\n - F-score: %.3f\n",  s$precision, s$recall, s$Fscore))
  }

  invisible(x)
}
