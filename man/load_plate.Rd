% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/save_load.R
\name{load_plate}
\alias{load_plate}
\title{Load a ddPCR plate}
\usage{
load_plate(file)
}
\arguments{
\item{file}{Name of the file where the plate was saved.}
}
\value{
The plate that was saved in the given file.
}
\description{
Reloads a plate that has been saved with \code{\link[ddpcrS3]{save_plate}}.
}
\examples{
dir <- system.file("sample_data", "small", package = "ddpcrS3")
plate <- new_plate(dir)
save_plate(plate, "myplate")
plate2 <- load_plate("myplate")
plate3 <- load_plate("myplate.rds")
identical(plate, plate2)
identical(plate, plate3)
unlink("myplate.rds")
}
\seealso{
\code{\link[ddpcrS3]{save_plate}}
}
