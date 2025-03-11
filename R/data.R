#' County-Level Employment and Housing Net Worth Data from Mian and Sufi (2014)
#'
#' This dataset contains county-level economic and demographic data used in
#' Mian and Sufi (2014), "Households, Leverage, and the Macroeconomic Adjustment".
#' It includes variables related to housing, employment, demographics, and
#' income.
#'
#' @format A data.table with 3135 rows and the following variables:
#' \describe{
#'   \item{\code{fips}}{Character. FIPS county code.}
#'   \item{\code{house.net.worth}}{Numeric. Change in house net worth.}
#'   \item{\code{elasticity}}{Numeric. Housing supply elasticity.}
#'   \item{\code{housing.units}}{Numeric. Number of housing units.}
#'   \item{C2D06share1-C2D06share23}{Numeric. Shares of 23 different credit origination categories.}
#'   \item{\code{non.trade.emp.retail.rest}}{Numeric. Change in non-trade employment in retail and restaurants.}
#'   \item{\code{non.trade.emp.geography}}{Numeric. Change in non-trade employment in geography-related sectors.}
#'   \item{\code{white}}{Numeric. Proportion of white population.}
#'   \item{\code{medhhinc}}{Numeric. Median household income.}
#'   \item{\code{owner}}{Numeric. Homeownership rate.}
#'   \item{\code{educ_lths}}{Numeric. Proportion of population with less than high school education.}
#'   \item{\code{educ_hs}}{Numeric. Proportion of population with high school education.}
#'   \item{\code{unemp}}{Numeric. Unemployment rate.}
#'   \item{\code{pov}}{Numeric. Poverty rate.}
#'   \item{\code{urban}}{Numeric. Proportion of urban population.}
#' }
#'
#' @details
#' This dataset is used to analyze the relationship between household leverage,
#' housing wealth, and macroeconomic outcomes at the county level. The credit
#' origination share variables (C2D06share1-C2D06share23) represent the shares
#' of different credit origination categories in 2006.
#'
#' @source Mian, Atif, and Amir Sufi. 2014. "What Explains the 2007–2009 Drop in Employment?"
#'
#' @name dt_mian_sufi_2014
#' @docType data
#' @keywords data
#' @usage data(dt_mian_sufi_2014)
#'
#' @examples
#' data(dt_mian_sufi_2014)
#' head(dt_mian_sufi_2014)
"dt_mian_sufi_2014"

#' County-Level Land Unavailability Data (2010) with CBSA and Principal City Information
#'
#' This dataset provides county-level land unavailability measures for the year 2010,
#' sourced from Lutz and Sand. It also includes information on the closest Core Based
#' Statistical Area (CBSA) and its corresponding principal city for each county.
#'
#' @format A data.table with 3109 rows and numerous variables, including:
#' \describe{
#'   \item{\code{GISJOIN_cnty}}{Character. GISJOIN identifier for the county.}
#'   \item{\code{GEOID}}{Character. FIPS county code.}
#'   \item{\code{NAME}}{Character. County name.}
#'   \item{\code{year}}{Numeric. Year of the data (2010).}
#'   \item{\code{slope_unavailable_cnty_000_pct_buffer}, \code{water_unavailable_cnty_000_pct_buffer}, \code{wetlands_unavailable_cnty_000_pct_buffer}, \code{total_unavailable_cnty_000_pct_buffer}}{Numeric.  Percentage of land unavailable within the county due to slope, water, wetlands, and total, respectively, using a 0% buffer.}
#'   \item{...}{Other variables measuring land unavailability at county, CBSA, and principal city levels, with various buffer sizes and radii.}
#' }
#'
#' @details
#' This dataset allows for the analysis of land unavailability due to various
#' geographic factors at different spatial levels (county, CBSA, and principal city).
#' The inclusion of buffers and circles around the principal city provides flexibility
#' in defining the relevant geographic area for analysis. The dataset includes percentages of land unavailable due to slope, water, and wetlands, as well as the total percentage of unavailable land. These measures are provided for counties, CBSAs, and areas surrounding principal cities, with varying buffer sizes and circle radii.
#'
#' @source 
#'  * Lutz and Sand
#'
#' @name dt_cnty_lu_2010
#' @docType data
#' @keywords data
#' @usage data(dt_cnty_lu_2010)
#'
#' @examples
#' data(dt_cnty_lu_2010)
#' head(dt_cnty_lu_2010)
#' summary(dt_cnty_lu_2010$total_unavailable_cnty_000_pct_buffer)
"dt_cnty_lu_2010"
