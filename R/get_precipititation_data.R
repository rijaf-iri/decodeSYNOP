#' Get precipitation data.
#'
#' Get precipitation data, group \code{6RRRt_R} from section 1 and 3, and group \code{7R_{24}R_{24}R_{24}R_{24}} from section 3.
#' 
#' @param synop a synop data object, output of the function \code{getSynopSections}.
#'  
#' @return A named list of numeric vector of length 2,\cr
#' the first element of the list is a vector of length 2: c(duration[in hour], amount [in mm]) from section 1,
#' the second element is a vector length 3: c(duration[in hour], amount [in mm], amount past 24 hour [in mm]) from section 3.
#' 
#' @examples
#' 
#' \dontrun{
#' aaxx <- "AAXX 07181 33837 11583 83102 10039 29072 30049 40101 52035 60012="
#' synop <- getSynopSections(aaxx)
#' precip <- getPrecipitation(synop)
#' }
#' 
#' @export

getPrecipitation <- function(synop){
    if(!inherits(synop, "synop"))
        stop("synop is not a 'synop' data object")

    tmp <- c(NA, NA)
    rr24h <- get_precip_section3d24(synop)
    sec3 <- c(tmp, rr24h)

    if(is.na(synop$section1[4])){
        out <- list(section1 = tmp,
                    section3 = sec3)
        return(out)
    }

    ir <- substr(synop$section1[4], 1, 1)
    if(ir == "4"){
        out <- list(section1 = tmp,
                    section3 = sec3)
    }else if(ir == "3"){
        out <- list(section1 = c(NA, 0),
                    section3 = c(NA, 0, rr24h))
    }else{
        if(ir == "2"){
            sec3 <- get_precip_section3(synop)
            out <- list(section1 = tmp,
                        section3 = c(sec3, rr24h))
        }else{
            if(ir == "1"){
                sec1 <- get_precip_section1(synop)
                out <- list(section1 = sec1,
                            section3 = sec3)
            }else{
                ## code 0
                sec1 <- get_precip_section1(synop)
                sec3 <- get_precip_section3(synop)
                out <- list(section1 = sec1,
                            section3 = c(sec3, rr24h))
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

    get_precip_group6(x)
}

get_precip_section3 <- function(synop){
    if(!inherits(synop, "synop"))
        stop("synop is not a 'synop' data object")

    if(is.null(synop$section3)) return(c(NA, NA))
    get_precip_group6(synop$section3)
}

get_precip_group6 <- function(x){
    p <- grep("^6", x)
    if(length(p) == 0) return(c(NA, NA))
    p <- p[1]
    rrr <- substr(x[p], 2, 4)
    if(grepl('\\/', rrr)) return(c(NA, NA))

    rrr <- as.numeric(rrr)
    if(is.na(rrr)) return(c(NA, NA))

    if(rrr == 990){
        ## Trace
        precip <- 0.01
    }else if(rrr > 990){
        precip <- (rrr - 990) * 0.1
    }else{
        precip <- rrr
    }

    tr <- substr(x[p], 5, 5)
    duration <- NA
    if(tr != "/" && tr != ""){
        duration <- switch(tr, "0" = NA, "1" = 6,
                           "2" = 12, "3" = 18, "4" = 24,
                           "5" = 1, "6" = 2, "7" = 3,
                           "8" = 9, "9" = 15)
    }

    return(c(duration, precip))
}

get_precip_section3d24 <- function(synop){
    if(!inherits(synop, "synop"))
        stop("synop is not a 'synop' data object")

    if(is.null(synop$section3)) return(NA)

    x <- synop$section3
    p <- grep("^7", x)
    if(length(p) == 0) return(NA)
    p <- p[1]
    rrrr <- substr(x[p], 2, 5)
    if(grepl('\\/', rrrr)) return(NA)

    rrrr <- as.numeric(rrrr)
    if(is.na(rrrr)) return(NA)

    if(rrrr == 9999){
        ## Trace
        precip <- 0.01
    }else if(rrrr == 9998){
        precip <- 999.8
    }else{
        precip <- rrrr
    }

    return(precip)
}