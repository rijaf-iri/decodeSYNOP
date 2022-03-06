#' Get WMO station ID.
#'
#' Get WMO station ID, group \code{IIiii} of section 1.
#' 
#' @param synop a synop data object, output of the function \code{get_sectionSynop}.
#'  
#' @return A character, the WMO station ID.
#' 
#' @examples
#' 
#' \dontrun{
#' aaxx <- "AAXX 28154 89022 42698 72506 11360 21382 39758 49808="
#' synop <- getSynopSections(aaxx)
#' stnID <- getStationID(synop)
#' }
#' 
#' @export

getStationID <- function(synop){
    if(!inherits(synop, "synop"))
        stop("synop is not a 'synop' data object")

    synop$section1[3]
}

#' Get Observation Date-Time.
#'
#' Get the day and hour of a meteorological report, group \code{YYGGiw} of section 1.
#' 
#' @param synop a synop data object, output of the function \code{get_sectionSynop}.
#'  
#' @return A character vector of length 2, c(day, hour).
#' 
#' @examples
#' 
#' \dontrun{
#' aaxx <- "AAXX 28154 89022 42698 72506 11360 21382 39758 49808="
#' synop <- getSynopSections(aaxx)
#' ddhh <- getObsDateTime(synop)
#' }
#' 
#' @export

getObsDateTime <- function(synop){
    if(!inherits(synop, "synop"))
        stop("synop is not a 'synop' data object")

    day <- substr(synop$section1[2], 1, 2)
    hour <- substr(synop$section1[2], 3, 4)

    c(day, hour)
}

#' Get the actual time of observation.
#'
#' Get the actual time of observation if exist, group \code{9GGgg} of section 1.
#' 
#' @param synop a synop data object, output of the function \code{get_sectionSynop}.
#'  
#' @return A character vector of length 2, c(hour, minute). If the \code{9GGgg} group does not exist it returns the hour from the \code{YYGGiw} group and the minute is set to "00". 
#' 
#' @export

getObsActualTime <- function(synop){
    if(!inherits(synop, "synop"))
        stop("synop is not a 'synop' data object")

    ix <- 5
    if(isWindFF99(synop$section1)) ix <- 6
    x <- synop$section1[-(1:ix)]
    p <- grep("^9", x)
    if(length(p) == 0){
        hour <- substr(synop$section1[2], 3, 4)
        min <- "00"
    }else{
        hour <- substr(x[p], 2, 3)
        min <- substr(x[p], 4, 5)
    }

    c(hour, min)
}
