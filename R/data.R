#' Meteorological Data from the Solling Beech and Spruce experimental site
#'
#' A dataset containing daily weather variables for the period 1960-2013
#'
#' @format A data.frame with 19724 rows and 9 variables
#' \describe{
#'   \item{dates}{date}
#'   \item{tmin}{daily minimum temperature, deg C}
#'   \item{tmax}{daily maximum temperature, deg C}
#'   \item{tmean}{daily mean temperature, deg C}
#'   \item{prec}{daily sum of precipitation, mm}
#'   \item{relhum}{relative Humidity, \%}
#'   \item{globrad}{daily sum of global radiation, MJ/m²}
#'   \item{windspeed}{daily mean wind speed measured at 10 m above ground, m/s}
#'   \item{vappres}{daily vapour pressure, kPa}
#' }
"slb1_meteo"

#' Soil profile data from the Solling Beech experimental site 'SLB1'
#'
#' A dataset containing the soil horizons' physical properties
#'
#' @format A data.frame with 21 rows and 10 variables
#' \describe{
#'   \item{horizon}{horizon symbol}
#'   \item{upper}{upper layer boundary, m}
#'   \item{lower}{lower layer boundary, m}
#'   \item{texture}{soil texture according to German soil texture classification system}
#'   \item{bd}{bulk density of the fine earth, g/cm³}
#'   \item{gravel}{fraction of coarse material }
#'   \item{sand}{sand content, mass-\% }
#'   \item{silt}{silt content, mass-\% }
#'   \item{clay}{clay content, mass-\% }
#'   \item{c_org}{organic carbon content, mass-\%}
#' }
"slb1_soil"

#' Annual stand properties of the Solling Beech experimental site 'SLB1'
#'
#' A dataset containing the forests's stand properties
#'
#' @format A data.frame with 49 rows and 7 variables
#' \describe{
#'   \item{year}{Year of observation}
#'   \item{species}{Tree species}
#'   \item{age}{age of the main stand}
#'   \item{height}{average height of the trees, m}
#'   \item{maxlai}{maximum leaf area index, m²/m²}
#'   \item{sai}{stem area index, m²/m²}
#'   \item{densef}{stand density}
#' }
"slb1_standprop"

#' Hourly precipitation data from Solling Beech experimental site 'SLB1' for year 2013
#'
#' @format A data.frame with 8760 rows and 2 variables
#' \describe{
#'   \item{dates}{date}
#'   \item{prec}{hourly sum of precipitation, mm}
#' }
"slb1_prec2013_hh"


