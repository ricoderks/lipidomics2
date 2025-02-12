% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fct_qc.R
\name{show_overall_hist}
\alias{show_overall_hist}
\title{Create RSD histogram}
\usage{
show_overall_hist(data = NULL, rsd_cutoff = 0.3)
}
\arguments{
\item{data}{data.frame with the RSD data.}

\item{rsd_cutoff}{numeric(1), the RSD cut off value.}
}
\value{
ggplot2 object, histogram of the RSD values.
}
\description{
Create RSD histogram.
}
\details{
qc

data should contain the columns RSD and polarity.
}
\author{
Rico Derks
}
