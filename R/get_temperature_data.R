#' Get Air Temperature.
#'
#' Get the air temperature observation, group \code{1s_nTTT} from section 1.
#' 
#' @param synop a synop data object, output of the function \code{get_sectionSynop}.
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
    p <- grep("^1", x)
    
    tmp <- NA
    if(length(p) != 0){
        sgn <- switch(substr(x[p], 2, 2), "0" = 1, "1" = -1)
        tmp <- sgn * as.numeric(substr(x[p], 3, 5)) * 0.1
    }

    return(tmp)
}

#' Get Maximum Temperature.
#'
#' Get the maximum temperature observation, group \code{1s_nT_xT_xT_x} from section 333.
#' 
#' @param synop a synop data object, output of the function \code{get_sectionSynop}.
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

    x <- synop$section3
    tx <- NA
    if(length(x) == 0) return(tx)

    p <- grep("^1", x)
    if(length(p) != 0){
        sn <- substr(x[p], 2, 2)
        if(sn != "9"){
            sgn <- switch(sn, "0" = 1, "1" = -1)
            tx <- sgn * as.numeric(substr(x[p], 3, 5)) * 0.1
        }
    }

    return(tx)
}

#' Get Minimum Temperature.
#'
#' Get the minimum temperature observation, group \code{2s_nT_nT_nT_n} from section 333.
#' 
#' @param synop a synop data object, output of the function \code{get_sectionSynop}.
#'  
#' @return The minimum temperature observation in degree Celsius. 
#' 
#' @examples
#' 
#' \dontrun{
#' aaxx <- "AAXX 28154 89022 42698 72506 333 10312 20195="
#' synop <- getSynopSections(aaxx)
#' tmax <- getMinimumTemperature(synop)
#' }
#' 
#' @export

getMinimumTemperature <- function(synop){
    if(!inherits(synop, "synop"))
        stop("synop is not a 'synop' data object")

    x <- synop$section3
    tn <- NA
    if(length(x) == 0) return(tn)

    p <- grep("^2", x)
    if(length(p) != 0){
        sn <- substr(x[p], 2, 2)
        if(sn != "9"){
            sgn <- switch(sn, "0" = 1, "1" = -1)
            tn <- sgn * as.numeric(substr(x[p], 3, 5)) * 0.1
        }
    }

    return(tn)
}
