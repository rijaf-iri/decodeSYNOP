#' Get Station Pressure.
#'
#' Get the station pressure, group \code{3P_oP_oP_oP_o} from section 1.
#' 
#' @param synop a synop data object, output of the function \code{getSynopSections}.
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

    pres <- NA
    p <- grep("^3", x)
    if(length(p) != 0){
        p <- p[1]
        pres <- parse_PressureGroup(x[p])
    }

    return(pres)
}

#' Get Sea Level Pressure.
#'
#' Get the sea level pressure, group \code{4PPPP} from section 1.
#' 
#' @param synop a synop data object, output of the function \code{getSynopSections}.
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

    pres <- NA
    p <- grep("^4", x)
    if(length(p) != 0){
        p <- p[1]
        pres <- parse_PressureGroup(x[p])
    }

    return(pres)
}

parse_PressureGroup <- function(x){
    pres <- substr(x, 2, 5)
    if(!grepl("/", pres)){
        pr <- as.numeric(pres) * 0.1
        if(pr < 400) pr <- 1000 + pr
    }else pr <- NA

    return(pr)
}

