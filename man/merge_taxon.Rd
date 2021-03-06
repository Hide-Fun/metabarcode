% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/merge_taxon.R
\name{merge_taxon}
\alias{merge_taxon}
\title{Merging taxon in one column.}
\usage{
merge_taxon(.identify_list, .id = F)
}
\arguments{
\item{.identify_list}{table (see example).}

\item{.id}{logical, make id against each taxon. This is useful in makeing labels by each OTU.}
}
\description{
Merging taxon in one column by deepest taxonomic level.
}
\examples{
identify_list <- data.frame(
   otu = c("otu1", "otu2", "otu3", NA),
   phylum = c("p1", "p2", "p3", NA),
   order = c("o1", "o2", "o3", NA),
   family = c("f1", NA, "f3", NA),
   genus = c(NA, NA, "g3", NA)
)
merge_taxon(.identify_list = identify_list, .id = TRUE)
merge_taxon(.identify_list = identify_list, .id = FALSE)
}
