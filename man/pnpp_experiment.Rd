% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/type-pnpp_experiment.R
\name{pnpp_experiment}
\alias{pnpp_experiment}
\title{Plate type: PNPP experiment}
\description{
PNPP stands for "Positive-Negative;Positive-Positive", which is a reflection
of the clusters of non-empty droplets in the wells. Use this plate type when
your ddPCR data has three main clusters: double-negative (FAM-HEX-; empty droplets),
double-positive (FAM+HEX+; represent the "PP" in PNPP), and singly-positive
(either FAM+HEX- or HEX+FAM-; represent the "NP" in PNPP).
}
\details{
Every \code{pnpp_experiment} plate must define which dimension is its \emph{positive
dimension}.  The positive dimension is defined as the dimension that corresponds
to the dye that has a high fluoresence intensity in all non-empty droplets. The other
dimension is defined as the \emph{variable dimension}. For example, assuming
the HEX dye is plotted along the X axis and the FAM dye is along the Y axis,
a FAM+/FAM+HEX+ plate will have "Y" as its positive dimension because both
non-empty clusters have FAM+ droplets. Similarly, a HEX+/FAM+HEX+ plate will
have "X" as its positive dimension.

The positive dimension must be set in order to use a \code{pnpp_experiment}.
It is not recommended to use this type directly; instead you should use one
of the subtypes (\code{\link[ddpcr]{fam_positive_pnpp}} or
\code{\link[ddpcr]{hex_positive_pnpp}}). If you do use this type directly,
you must set the positive dimension with \code{\link[ddpcr]{positive_dim}}.

Plates with this type have the following analysis steps: \code{INITIALIZE},
\code{REMOVE_FAILURES}, \code{REMOVE_OUTLIERS}, \code{REMOVE_EMPTY},
\code{CLASSIFY}, \code{RECLASSIFY}.

Plates with this type have the following droplet clusters:
\code{UNDEFINED}, \code{FAILED}, \code{OUTLIER}, \code{EMPTY} (double-negative),
\code{RAIN}, \code{POSITIVE}, \code{NEGATIVE}.

\href{https://github.com/daattali/ddpcr#advanced-topic-3-creating-new-plate-types}{See the README} for
more information on plate types.
}
\examples{
\dontrun{
plate <- new_plate(sample_data_dir(), type = plate_types$pnpp_experiment)
type(plate)
}
}
\seealso{
\code{\link[ddpcr]{plate_types}}\cr
\code{\link[ddpcr]{fam_positive_pnpp}}\cr
\code{\link[ddpcr]{hex_positive_pnpp}}\cr
\code{\link[ddpcr]{wildtype_mutant_pnpp}}\cr
\code{\link[ddpcr]{positive_dim}}\cr
\code{\link[ddpcr]{wells_positive}}\cr
\code{\link[ddpcr]{wells_negative}}\cr
\code{\link[ddpcr]{analyze}}\cr
\code{\link[ddpcr]{remove_failures}}\cr
\code{\link[ddpcr]{remove_outliers}}\cr
\code{\link[ddpcr]{remove_empty}}\cr
\code{\link[ddpcr]{classify_droplets}}\cr
\code{\link[ddpcr]{reclassify_droplets}}
}
