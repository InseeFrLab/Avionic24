% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/av_create_MRIO_LF.R
\encoding{UTF-8}
\name{av_create_ICIO_2021}
\alias{av_create_ICIO_2021}
\title{av_create_ICIO_2021
ICIO 2021 release, CSV flat format : link : https://www.oecd.org/industry/ind/inter-country-input-output-tables.htm
OECD (2021), OECD Inter-Country Input-Output Database, http://oe.cd/icio}
\usage{
av_create_ICIO_2021(
  Path_ICIO,
  Path_out,
  OptAnnual = FALSE,
  ConvertToEuro = TRUE
)
}
\arguments{
\item{Path_ICIO}{path of ICIO flat CSV format files}

\item{Path_out}{path to save normalized data}

\item{OptAnnual}{option to keep one files by year instead of one file with all years. Can provide a degraded solution if a single database requires too much memory.}

\item{ConvertToEuro}{Option to convert Dollars to Euros}
}
\value{
Nothing. Only save data into normalized format.
}
\description{
av_create_ICIO_2021
ICIO 2021 release, CSV flat format : link : https://www.oecd.org/industry/ind/inter-country-input-output-tables.htm
OECD (2021), OECD Inter-Country Input-Output Database, http://oe.cd/icio
}
\examples{
\dontrun{ Path_ICIO <- PathTemp
Path_out <- PathTest
av_create_ICIO_2021(Path_ICIO, Path_out, OptAnnual = TRUE, ConvertToEuro = TRUE)
test1998Euro <- readRDS(paste0(Path_out, "/BDn_ICIO_", 1998, ".rds"))
av_create_ICIO_2021(Path_ICIO, Path_out, OptAnnual = TRUE, ConvertToEuro = FALSE)
test1998Dolls <- readRDS(paste0(Path_out, "/BDn_ICIO_", 1998, ".rds"))
av_create_ICIO_2021(Path_ICIO, Path_out, OptAnnual = FALSE, ConvertToEuro = TRUE)
testEuro <- readRDS(paste0(Path_out, "/BDn_ICIO.rds"))
av_create_ICIO_2021(Path_ICIO, Path_out, OptAnnual = FALSE, ConvertToEuro = FALSE)
testDolls <- readRDS(paste0(Path_out, "/BDn_ICIO.rds"))
head(testEuro)
head(testDolls)}
}
