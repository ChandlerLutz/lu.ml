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

#' Data on Real Estate Shocks and Corporate Investment
#'
#' @description
#' A panel dataset of U.S. Metropolitan Statistical Areas (MSAs) from 1993 to 2006.
#' This data is used to study the "collateral channel," which examines how shocks
#' to the value of commercial real estate affect corporate investment decisions.
#'
#' @format A `data.table` with observations for multiple MSAs over several years,
#' containing 19 variables:
#' \describe{
#'   \item{\code{msacode}}{Integer. The Metropolitan Statistical Area (MSA) code.}
#'   \item{\code{year}}{Integer. The year of the observation (1993-2006).}
#'   \item{\code{msa}}{Character. The name of the MSA.}
#'   \item{\code{msacode_orig}}{Integer. The original MSA code, likely for merging or reference.}
#'   \item{\code{index_msa}}{Numeric. The MSA-level house price index for the given year.}
#'   \item{\code{index06}}{Numeric. The MSA-level house price index in the base year 2006.}
#'   \item{\code{index_normalized}}{Numeric. The house price index normalized by the 2006 value (`index_msa` / `index06`).}
#'   \item{\code{mortgage}}{Numeric. The average annual mortgage rate.}
#'   \item{\code{inflation}}{Numeric. The annual inflation rate.}
#'   \item{\code{real_mtg_rate}}{Numeric. The real mortgage rate, calculated as `mortgage - inflation`.}
#'   \item{\code{msaname_saiz}}{Character. The MSA name from the Saiz (2010) housing supply elasticity data.}
#'   \item{\code{elasticity}}{Numeric. A measure of housing supply elasticity.}
#'   \item{\code{elasticity2}}{Numeric. An alternative measure of housing supply elasticity.}
#'   \item{\code{elasticity3}}{Numeric. An alternative measure of housing supply elasticity.}
#'   \item{\code{elasticity4}}{Numeric. An alternative measure of housing supply elasticity.}
#'   \item{\code{saiz_rate}}{Numeric. A variable related to the Saiz (2010) data.}
#'   \item{\code{year_char}}{Character. The year of observation as a character string.}
#'   \item{\code{msa_char}}{Character. The MSA code as a character string.}
#'   \item{\code{id}}{Character. A unique identifier for each MSA.}
#' }
#'
#' @source
#' Chaney, T., Sraer, D., & Thesmar, D. (2012). The collateral channel: How real estate shocks affect corporate investment. *American Economic Review*, 102(6), 2381–2409.
#'
#' @keywords datasets
#'
#' @name dt_chaneyetal_2012
#' @docType data
#' @usage data(dt_chaneyetal_2012)
"dt_chaneyetal_2012"

#' Highly Disaggregated Land Unavailability Data for U.S. CBSAs
#'
#' @description
#' This dataset provides measures of land unavailability for U.S. Core-Based
#' Statistical Areas (CBSAs) based on the 2009 delineations. The data quantifies
#' the percentage of land that is unavailable for development due to physical
#' constraints such as steep slopes, water bodies, and wetlands. Measures are
#' provided for both the entire CBSA and the principal city within it, using
#' various buffer and radius specifications.
#'
#' The data is formatted to be easily merged with the Chaney, Sraer, and Thesmar (2012)
#' dataset, hence the name `..._for_chaneyetal`.
#'
#' @format A `data.table` with observations for U.S. CBSAs, containing 56 variables:
#' \describe{
#'   \item{\code{msacode}}{Character. The Metropolitan Statistical Area (MSA/CBSA) code. Acts as the key.}
#'   \item{\code{id}}{Character. A unique identifier for each MSA, consistent with the Chaney et al. (2012) data.}
#'   \item{\code{slope_unavailable_cbsa_..._pct_buffer}}{Numeric. Percentage of land in the **CBSA** unavailable due to steep slopes, calculated with different percentage buffers (e.g., `000` for 0%, `005` for 0.5%, `010` for 1.0%, `015` for 1.5%, `020` for 2.0%).}
#'   \item{\code{water_unavailable_cbsa_..._pct_buffer}}{Numeric. Percentage of land in the **CBSA** unavailable due to water bodies, with various percentage buffers.}
#'   \item{\code{wetlands_unavailable_cbsa_..._pct_buffer}}{Numeric. Percentage of land in the **CBSA** unavailable due to wetlands, with various percentage buffers.}
#'   \item{\code{slope_unavailable_prin_city_..._pct_buffer}}{Numeric. Percentage of land in the **principal city** unavailable due to steep slopes, with different percentage buffers (from `000` to `150`, representing 0% to 15.0%).}
#'   \item{\code{water_unavailable_prin_city_..._pct_buffer}}{Numeric. Percentage of land in the **principal city** unavailable due to water bodies, with various percentage buffers.}
#'   \item{\code{wetlands_unavailable_prin_city_..._pct_buffer}}{Numeric. Percentage of land in the **principal city** unavailable due to wetlands, with various percentage buffers.}
#'   \item{\code{slope_unavailable_prin_city_circle_radius_...km}}{Numeric. Percentage of land unavailable due to steep slopes within a circular area around the **principal city's** center, with different radii (from `20km` to `100km`).}
#'   \item{\code{water_unavailable_prin_city_circle_radius_...km}}{Numeric. Percentage of land unavailable due to water bodies within a circular area around the **principal city's** center, with various radii.}
#'   \item{\code{wetlands_unavailable_prin_city_circle_radius_...km}}{Numeric. Percentage of land unavailable to wetlands within a circular area around the **principal city's** center, with various radii.}
#' }
#'
#' @source
#' Lutz, C., & Sand, B. (2025). Highly disaggregated land unavailability. *Journal of Urban Economics*, 145, 103721. \doi{10.1016/j.jue.2024.103721}
#'
#' The data was accessed from Chandler Lutz's website.
#'
#' @keywords datasets
#'
#' @name dt_lu_cbsa09_for_chaneyetal
#' @docType data
#' @usage data(dt_lu_cbsa09_for_chaneyetal)
"dt_lu_cbsa09_for_chaneyetal"
