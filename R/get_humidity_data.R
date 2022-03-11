#' Get Dew Point Temperature.
#'
#' Get the dew point temperature, group \code{2s_nT_dT_dT_d} from section 1.
#' 
#' @param synop a synop data object, output of the function \code{getSynopSections}.
#'  
#' @return The dew point temperature in degree Celsius. 
#' 
#' @examples
#' 
#' \dontrun{
#' aaxx <- "AAXX 07181 33837 11583 83102 10039 21007 30049 40101 52035 60012 70282 8255/="
#' synop <- getSynopSections(aaxx)
#' td <- getDewPointTemperature(synop)
#' }
#' 
#' @export

getDewPointTemperature <- function(synop){
    if(!inherits(synop, "synop"))
        stop("synop is not a 'synop' data object")

    ix <- 5
    if(isWindFF99(synop$section1)) ix <- 6
    x <- synop$section1[-(1:ix)]
    p <- grep("^2", x)
    
    td <- NA
    if(length(p) != 0){
        p <- p[1]
        sn <- substr(x[p], 2, 2)
        t2 <- substr(x[p], 3, 5)
        if(t2 != "///" & sn != "/"){
            if(sn != "9"){
                sgn <- switch(sn, "0" = 1, "1" = -1)
                td <- sgn * as.numeric(t2) * 0.1
            }
        }
    }

    return(td)
}

#' Get Relative Humidity.
#'
#' Get the relative humidity, group \code{29UUU} from section 1.
#' 
#' @param synop a synop data object, output of the function \code{getSynopSections}.
#'  
#' @return The relative humidity in percentage. 
#' 
#' @examples
#' 
#' \dontrun{
#' aaxx <- "AAXX 07181 33837 11583 83102 10039 29072 30049 40101 52035 60012="
#' synop <- getSynopSections(aaxx)
#' rh <- getRelativeHumidity(synop)
#' }
#' 
#' @export

getRelativeHumidity <- function(synop){
    if(!inherits(synop, "synop"))
        stop("synop is not a 'synop' data object")

    ix <- 5
    if(isWindFF99(synop$section1)) ix <- 6
    x <- synop$section1[-(1:ix)]
    p <- grep("^2", x)
    
    rh <- NA
    if(length(p) != 0){
        p <- p[1]
        sn <- substr(x[p], 2, 2)
        t2 <- substr(x[p], 3, 5)
        if(t2 != "///" & sn != "/"){
            if(sn == "9")
                rh <- as.numeric(t2)
        }
    }

    return(rh)
}
