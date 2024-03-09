#' Surface observation report from land station (FM-12).
#'
#' Get the data from a selected variables from a SYNOP (AAXX) strings of weather reports.
#' 
#' @param synop a synop data object, output of the function \code{getSynopSections}.
#'  
#' @return A data frame of one row, the columns contain the variables. 
#' 
#' @examples
#' 
#' \dontrun{
#' aaxx <- "AAXX 07181 33837 11583 83102 10039 21007 30049 40101 52035 60012 70282 8255/="
#' synop <- getSynopSections(aaxx)
#' df <- get_Synop_Data(synop)
#' }
#' 
#' @export

get_Synop_Data <- function(synop){
    tm <- getAirTemperature(synop)
    tx <- getMaximumTemperature(synop)
    tn <- getMinimumTemperature(synop)
    td <- getDewPointTemperature(synop)
    rh <- getRelativeHumidity(synop)
    pres <- getStationPressure(synop)
    slp <- getSeaLevelPressure(synop)
    wnd <- getWindData(synop)
    prcp <- getPrecipitation(synop)

    precip <- c(prcp$section1, prcp$section3)

    if(is.na(td) & !is.na(rh) & !is.na(tm)){
        td <- round(dewpoint_temperature(tm, rh), 1)
    }
    if(!is.na(td) & is.na(rh) & !is.na(tm)){
        rh <- round(relative_humidity(tm, td), 1)
    }

    stn <- getStationID(synop)
    ddhh <- getObsDateTime(synop)

    dat <- c(stn, ddhh, tm, tx, tn, td, rh, pres, slp, wnd, precip)
    dat <- as.data.frame(matrix(dat, nrow = 1))
    dat[, -(1:3)] <- as.numeric(dat[, -(1:3)])
    names(dat) <- c('WMOID', 'Day', 'Hour', 'AirTemp', 'Tmax', 'Tmin',
                    'TempDew', 'RH', 'SurfPres', 'SLPres', 'Wind_dd', 'Wind_ff',
                    'RR_Duration_Sec1', 'RR_Amount_Sec1', 'RR_Duration_Sec3',
                    'RR_Amount_Sec3', 'RR_Amount_24h')
    return(dat)
}
