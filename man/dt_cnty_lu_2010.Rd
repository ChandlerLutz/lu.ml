% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{dt_cnty_lu_2010}
\alias{dt_cnty_lu_2010}
\title{County-Level Land Unavailability Data (2010) with CBSA and Principal City Information}
\format{
A data.table with 3109 rows and numerous variables, including:
\describe{
\item{\code{GISJOIN_cnty}}{Character. GISJOIN identifier for the county.}
\item{\code{GEOID}}{Character. FIPS county code.}
\item{\code{NAME}}{Character. County name.}
\item{\code{year}}{Numeric. Year of the data (2010).}
\item{\code{slope_unavailable_cnty_000_pct_buffer}, \code{water_unavailable_cnty_000_pct_buffer}, \code{wetlands_unavailable_cnty_000_pct_buffer}, \code{total_unavailable_cnty_000_pct_buffer}}{Numeric.  Percentage of land unavailable within the county due to slope, water, wetlands, and total, respectively, using a 0\% buffer.}
\item{...}{Other variables measuring land unavailability at county, CBSA, and principal city levels, with various buffer sizes and radii.}
}
}
\source{
\itemize{
\item Lutz and Sand
}
}
\usage{
data(dt_cnty_lu_2010)
}
\description{
This dataset provides county-level land unavailability measures for the year 2010,
sourced from Lutz and Sand. It also includes information on the closest Core Based
Statistical Area (CBSA) and its corresponding principal city for each county.
}
\details{
This dataset allows for the analysis of land unavailability due to various
geographic factors at different spatial levels (county, CBSA, and principal city).
The inclusion of buffers and circles around the principal city provides flexibility
in defining the relevant geographic area for analysis. The dataset includes percentages of land unavailable due to slope, water, and wetlands, as well as the total percentage of unavailable land. These measures are provided for counties, CBSAs, and areas surrounding principal cities, with varying buffer sizes and circle radii.
}
\examples{
data(dt_cnty_lu_2010)
head(dt_cnty_lu_2010)
summary(dt_cnty_lu_2010$total_unavailable_cnty_000_pct_buffer)
}
\keyword{data}
