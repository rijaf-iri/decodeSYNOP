#' Saturation Vapour Pressure.
#'
#' Compute the saturation vapour pressure.
#' 
#' @param tmp the air temperature in degree Celsius.
#'  
#' @return The saturation vapour pressure in hectopascal. 
#' 
#' @export

saturation_vapour_pressure <- function(tmp){
    6.112 * exp((17.67 * tmp)/(tmp + 243.5))
}

#' Relative humidity.
#'
#' Compute the relative humidity.
#' 
#' @param tm the air temperature in degree Celsius.
#' @param td the dew point temperature in degree Celsius.
#'  
#' @return The relative humidity in percentage. 
#' 
#' @export

relative_humidity <- function(tm, td){
    es <- saturation_vapour_pressure(tm)
    ea <- saturation_vapour_pressure(td)
    rh <- 100 * ea/es
    rh[rh > 100] <- 100
    rh
}

#' Specific humidity.
#'
#' Compute the specific humidity.
#' 
#' @param td the dew point temperature in degree Celsius.
#' @param pr the surface pressure in hectopascal.
#'  
#' @return The specific humidity in kg/kg. 
#' 
#' @export

specific_humidity <- function(td, pr){
    ea <- saturation_vapour_pressure(td)
    (0.622 * ea)/(pr - (0.378 * ea))
}

#' Dew Point Temperature.
#'
#' Compute the dew point temperature.
#' 
#' @param tm the air temperature in degree Celsius.
#' @param rh the relative humidity in percentage.
#'  
#' @return The dew point temperature in degree Celsius. 
#' 
#' @export

dewpoint_temperature <- function(tm, rh){
    es <- saturation_vapour_pressure(tm)
    ea <- es * (rh/100)
    (log(ea/6.112) * 243.5)/(17.67 - log(ea/6.112))
}
