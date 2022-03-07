#' Get Air Temperature.
#'
#' Get the air temperature observation, group \code{1s_nTTT} from section 1.
#' 
#' @param synop a synop data object, output of the function \code{getSynopSections}.
#'  
#' @return The air temperature observation in degree Celsius. 
#' 
#' @examples
#' 
#' \dontrun{
#' aaxx <- "AAXX 28154 89022 42698 72506 11360 21382 39758 49808="
#' synop <- getSynopSections(aaxx)
#' tmp <- getAirTemperature(synop)
#' }
#' 
#' @export

getAirTemperature <- function(synop){
    if(!inherits(synop, "synop"))
        stop("synop is not a 'synop' data object")

    ix <- 5
    if(isWindFF99(synop$section1)) ix <- 6
    x <- synop$section1[-(1:ix)]
    tmp <- NA

    p <- grep("^1", x)
    if(length(p) != 0)
        tmp <- parse_temperatureGroup(x[p])

    return(tmp)
}

#' Get Maximum Temperature.
#'
#' Get the maximum temperature observation, group \code{1s_nT_xT_xT_x} from section 333.
#' 
#' @param synop a synop data object, output of the function \code{getSynopSections}.
#'  
#' @return The maximum temperature observation in degree Celsius. 
#' 
#' @examples
#' 
#' \dontrun{
#' aaxx <- "AAXX 28154 89022 42698 72506 333 10312 20195="
#' synop <- getSynopSections(aaxx)
#' tmax <- getMaximumTemperature(synop)
#' }
#' 
#' @export

getMaximumTemperature <- function(synop){
    if(!inherits(synop, "synop"))
        stop("synop is not a 'synop' data object")

    tx <- NA
    x <- synop$section3
    if(length(x) == 0) return(tx)

    p <- grep("^1", x)
    if(length(p) != 0)
        tx <- parse_temperatureGroup(x[p])

    return(tx)
}

#' Get Minimum Temperature.
#'
#' Get the minimum temperature observation, group \code{2s_nT_nT_nT_n} from section 333.
#' 
#' @param synop a synop data object, output of the function \code{getSynopSections}.
#'  
#' @return The minimum temperature observation in degree Celsius. 
#' 
#' @examples
#' 
#' \dontrun{
#' aaxx <- "AAXX 28154 89022 42698 72506 333 10312 20195="
#' synop <- getSynopSections(aaxx)
#' tmin <- getMinimumTemperature(synop)
#' }
#' 
#' @export

getMinimumTemperature <- function(synop){
    if(!inherits(synop, "synop"))
        stop("synop is not a 'synop' data object")

    tn <- NA
    x <- synop$section3
    if(length(x) == 0) return(tn)

    p <- grep("^2", x)
    if(length(p) != 0)
        tn <- parse_temperatureGroup(x[p])

    return(tn)
}

parse_temperatureGroup <- function(x){
    sgn <- substr(x, 2, 2)
    tmp <- substr(x, 3, 5)
    if(tmp != "///" & sgn != "/"){
        sgn <- switch(sgn, "0" = 1, "1" = -1)
        tm <- sgn * as.numeric(tmp) * 0.1
    }else tm <- NA

    return(tm)
}
