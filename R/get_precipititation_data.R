#' Get precipitation data.
#'
#' Get precipitation data, group \code{6RRRt_R} from section 1 and group \code{7R_24R_24R_24R_24} from section 333.
#' 
#' @param synop a synop data object, output of the function \code{get_sectionSynop}.
#'  
#' @return A named list of numeric vector of length 2, c(duration[in hour], amount [in mm]).\cr 
#' If \code{i_R} from the group \code{i_Ri_XhVV} of section 1 equals 0, it returns a list of length 2,\cr
#' first element is the precipitation from section 1 group \code{6RRRt_R} \cr
#' and the second element from section 333 group \code{7R_24R_24R_24R_24}. 
#' 
#' @examples
#' 
#' \dontrun{
#' aaxx <- "AAXX 07181 33837 11583 83102 10039 29072 30049 40101 52035 60012="
#' synop <- getSynopSections(aaxx)
#' pres <- getPrecipitation(synop)
#' }
#' 
#' @export

getPrecipitation <- function(synop){
    if(!inherits(synop, "synop"))
        stop("synop is not a 'synop' data object")

    ir <- substr(synop$section1[4], 1, 1)
    if(ir == "4"){
        out <- list(section1 = c(NA, NA))
    }else if(ir == "3"){
        out <- list(section1 = c(NA, 0))
    }else{
        if(ir == "2"){
            out <- get_precip_section3(synop)
            out <- list(section3 = out)
        }else{
            if(ir == "1"){
                out <- get_precip_section1(synop)
                out <- list(section1 = out)
            }else{
                ## code 0
                out0 <- get_precip_section1(synop)
                out3 <- get_precip_section3(synop)
                out <- list(section1 = out0, section3 = out3)
            }
        }
    }

    return(out)
}

get_precip_section1 <- function(synop){
    if(!inherits(synop, "synop"))
        stop("synop is not a 'synop' data object")

    ix <- 5
    if(isWindFF99(synop$section1)) ix <- 6
    x <- synop$section1[-(1:ix)]
    p <- grep("^6", x)
    rrr <- as.numeric(substr(x[p], 2, 4))
    tr <- substr(x[p], 5, 5)
    duration <- switch(tr, "1" = 6, "2" = 12, "3" = 18, "4" = 24,
                    "5" = 1, "6" = 2, "7" = 3, "8" = 9, "9" = 15)

    if(rrr == 990){
        ## Trace
        precip <- 0.01
    }else if(rrr > 990){
        precip <- (rrr - 990) * 0.1
    }else{
        precip <- rrr
    }
 
    return(c(duration, precip))
}

get_precip_section3 <- function(synop){
    if(!inherits(synop, "synop"))
        stop("synop is not a 'synop' data object")

    x <- synop$section3
    p <- grep("^7", x)
    rrrr <- as.numeric(substr(x[p], 2, 5))

    if(rrrr == 9999){
        ## Trace
        precip <- 0.01
    }else if(rrrr == 9998){
        precip <- 999.8
    }else{
        precip <- rrrr
    }

    duration <- 24

    return(c(duration, precip))
}