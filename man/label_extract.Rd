% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/label_extract.R
\name{label_extract}
\alias{label_extract}
\title{Extracting label number.}
\usage{
label_extract(string)
}
\arguments{
\item{string}{A character}
}
\description{
this function extraxt number from chracter.
accepting number type is "2-10", "1-9-3", "1-9-10", "3" ...etc.
}
\examples{
label_extract("ABC_2-1-10_DE")
}
