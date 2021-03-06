% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/type-custom_thresholds.R
\name{classify_thresholds}
\alias{classify_thresholds}
\title{Analysis step: Classify droplets}
\usage{
classify_thresholds(plate)
}
\arguments{
\item{plate}{A ddPCR plate}
}
\value{
A ddPCR plate with all the droplets assigned to a quadrant. The plate's
metadata will have a few new variables relating to the number of droplets
in each quadrant.
}
\description{
The main analysis step for ddPCR plates of type \code{custom_thresholds}.
Assign each droplet into one of four quadrants based on the thresholds.\cr\cr
\href{https://github.com/daattali/ddpcr#advanced-topic-2-algorithms-used-in-each-step}{See the README} for
more information.
}
\details{
This function is recommended to be run as part of an analysis pipeline (ie.
within the \code{\link[ddpcr]{analyze}} function) rather than being called
directly.
}
\seealso{
\code{\link[ddpcr]{custom_thresholds}}\cr
\code{\link[ddpcr]{analyze}}\cr
\code{\link[ddpcr]{thresholds}}
}
\keyword{internal}
