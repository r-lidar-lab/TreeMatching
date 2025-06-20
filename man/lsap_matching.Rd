% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/matching_functions.R
\name{lsap_matching}
\alias{lsap_matching}
\title{Match measured and inventory trees using the LSAP algorithm}
\usage{
lsap_matching(treemap, dmax = 2, dz = 0.1, unmatch_cost = "auto")
}
\arguments{
\item{treemap}{A TreeMapMatching object. See \link{make_mapmatching}}

\item{dmax}{Maximum allowed 2D distance (in projection units) to consider a match valid.}

\item{dz}{Maximum allowed Z (DBH or height) difference to consider a match valid.}

\item{unmatch_cost}{See details.}
}
\value{
A \code{data.table} with columns:
\describe{
\item{\code{index_measure}}{The index of the matched measured tree.}
\item{\code{index_inventory}}{The index of the matched inventory tree, or \code{NA} if unmatched.}
}
Only valid matches within distance thresholds are retained. Invalid matches are assigned \code{NA}.
}
\description{
This function matches trees from a measured dataset to trees in an inventory dataset
by solving a Linear Sum Assignment Problem (LSAP) in 3D space. The third dimension
is synthetically created using the DBH (Diameter at Breast Height) to improve matching accuracy.
Unmatched points are assigned to a dummy column with a fixed cost, allowing partial matching.
}
\details{
This function matches trees based on their 3D positions. Since actual tree height is often unavailable,
the DBH (Diameter at Breast Height) is used to simulate a third dimension (Z) that helps distinguish
trees that may be close in 2D but differ in size. DBH values are scaled to be comparable
to spatial coordinates (X, Y), and distances are computed in this 3D space.\cr\cr
The matching is performed using the LSAP (Linear Sum Assignment Problem) algorithm
(\link[clue:solve_LSAP]{solve_LSAP}), which finds the best one-to-one pairings between trees
in the measured and inventory datasets by minimizing the total distance.\cr\cr
However, by design, the LSAP algorithm always attempts to match all items, even when no suitable match exists.
To avoid forcing poor matches, "dummy matches" are added to the cost matrix. These dummy matches represent
the option for a measured tree to remain unmatched, with a fixed cost (\code{unmatch_cost}).\cr\cr
If the cost of a match exceeds this fixed cost, the algorithm will prefer the dummy option.
After solving, these unmatched trees are identified and marked as \code{NA} in the result.\cr\cr
Choosing an appropriate \code{unmatch_cost} is nontrivial. It should be higher than the cost of a good match
to encourage valid pairings, but lower than the cost of a bad match to penalize poor assignments.
When \code{unmatch_cost = "auto"}, the algorithm prints the automatically determined value.
Users can then adjust this value to fine-tune the matching behavior.
}
\seealso{
\code{\link[clue]{solve_LSAP}} \code{\link{make_mapmatching}}
}
