% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/make_summary_table.R
\name{make_summary_table}
\alias{make_summary_table}
\title{Make summary table}
\usage{
make_summary_table(
  .id,
  .funguild,
  .seq,
  .sample_info,
  .tax_pat = "unite",
  .taxon,
  .remove = T
)
}
\arguments{
\item{.id}{siz002}

\item{.funguild}{funguild}

\item{.seq}{clustered.fasta}

\item{.sample_info}{sample information}

\item{.tax_pat}{UNITE DB or overall_genus or another. If you specify "manual"}

\item{.taxon}{manual}

\item{.remove}{remove size.}
}
\description{
This function make summary tabel.
}
\examples{
identify_list <- data.frame(
otu = c("otu1", "otu2"),
phylum = c("p1", "p2"),
order = c("o1", "o2"),
family = c("f1", "f2"),
genus = c("g1", "g2")
)

otu_table <- data.frame(
  samplename = c("id_sample1", "id_sample2"),
  otu1 = c(1, 0),
  otu2 = c(0, 39)
)

seq <- data.frame(
  otu = c("otu1", "otu2"),
  seq = c("seq1", "seq2")
)

sample_info <- data.frame(
  samplename = c("id_sample1", "id_sample2"),
  sample_info = c("sample_info1", "sample_info2")
)


summary <- make_summary_table(
  .id = "id",
  .funguild = funguild_db(otu_table, identify_list, c("phylum", "order", "family", "genus")),
  .seq = seq,
  .tax_pat = "manual",
  .taxon = c("phylum", "order", "family", "genus"),
  .sample_info = sample_info,
  .remove = FALSE
)

summary[[1]]
summary[[2]]
}
