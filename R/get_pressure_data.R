#' Get Station Pressure.
#'
#' Get the station pressure, group \code{3P_oP_oP_oP_o} from section 1.
#' 
#' @param synop a synop data object, output of the function \code{get_sectionSynop}.
#'  
#' @return The station pressure in hectopascal. 
#' 
#' @examples
#' 
#' \dontrun{
#' aaxx <- "AAXX 28154 89022 42698 72506 11360 21382 39758 49808="
#' synop <- getSynopSections(aaxx)
#' pres <- getStationPressure(synop)
#' }
#' 
#' @export

getStationPressure <- function(synop){
    if(!inherits(synop, "synop"))
        stop("synop is not a 'synop' data object")

    ix <- 5
    if(isWindFF99(synop$section1)) ix <- 6
    x <- synop$section1[-(1:ix)]
    p <- grep("^3", x)
    
    pres <- NA
    if(length(p) != 0){
        pres <- as.numeric(substr(x[p], 2, 5)) * 0.1
        if(pres < 400) pres <- 1000 + pres
    }

    return(pres)
}

#' Get Sea Level Pressure.
#'
#' Get the sea level pressure, group \code{4PPPP} from section 1.
#' 
#' @param synop a synop data object, output of the function \code{get_sectionSynop}.
#'  
#' @return The sea level pressure in hectopascal. 
#' 
#' @examples
#' 
#' \dontrun{
#' aaxx <- "AAXX 28154 89022 42698 72506 11360 21382 39758 49808="
#' synop <- getSynopSections(aaxx)
#' slp <- getSeaLevelPressure(synop)
#' }
#' 
#' @export

getSeaLevelPressure <- function(synop){
    if(!inherits(synop, "synop"))
        stop("synop is not a 'synop' data object")

    ix <- 5
    if(isWindFF99(synop$section1)) ix <- 6
    x <- synop$section1[-(1:ix)]
    p <- grep("^4", x)
    
    pres <- NA
    if(length(p) != 0){
        pres <- as.numeric(substr(x[p], 2, 5)) * 0.1
        if(pres < 400) pres <- 1000 + pres
    }

    return(pres)
}
