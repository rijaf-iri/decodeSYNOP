#' Parse SYNOP code.
#'
#' Parse SYNOP (AAXX) strings of weather reports.
#' 
#' @param synop character, the full strings of the synop code.\cr
#'              Example: "AAXX 28154 89022 42698 72506 11360 21382 39758 49808="
#'  
#' @return A synop data objects. It is a list object with elements the sections [1, 222//, 333, 555] of the synop.
#' 
#' @examples
#' 
#' \dontrun{
#' library(decodeSYNOP)
#' 
#' aaxx <- "AAXX 28154 89022 42698 72506 11360 21382 39758 49808="
#' synop <- getSynopSections(aaxx)
#' }
#' 
#' @export

getSynopSections <- function(synop){
    synop <- split_rawSynop_Data(synop)
    i2 <- grep("^222//$", synop)
    i3 <- grep("^333$", synop)
    i5 <- grep("^555$", synop)

    out <- vector('list', 4)

    if(length(i2) == 0){
        if(length(i3) == 0){
            if(length(i5) == 0){
                out[[1]] <- synop
            }else{
                out[[1]] <- synop[1:(i5 - 1)]
                out[[4]] <- synop[(i5 + 1):length(synop)]
            }
        }else{
            out[[1]] <- synop[1:(i3 - 1)]
            if(length(i5) != 0){
                out[[3]] <- synop[(i3 + 1):(i5 - 1)]
                out[[4]] <- synop[(i5 + 1):length(synop)]
            }else{
                out[[3]] <- synop[(i3 + 1):length(synop)]
            }
        }
    }else{
        out[[1]] <- synop[1:(i2 - 1)]
        if(length(i3) == 0){
            if(length(i5) == 0){
                out[[2]] <- synop[(i2 + 1):length(synop)]
            }else{
                out[[2]] <- synop[(i2 + 1):(i5 - 1)]
                out[[4]] <- synop[(i5 + 1):length(synop)]
            }
        }else{
            out[[2]] <- synop[(i2 + 1):(i3 - 1)]
            if(length(i5) == 0){
                out[[3]] <- synop[(i3 + 1):length(synop)]
            }else{
                out[[3]] <- synop[(i3 + 1):(i5 - 1)]
                out[[4]] <- synop[(i5 + 1):length(synop)]
            }
        }
    }
    names(out) <- paste0('section', c(1, 2, 3, 5))
    class(out) <- append(class(out), "synop")

    return(out)
}

split_rawSynop_Data <- function(synop){
    synop <- strsplit(synop, ' ')
    synop <- trimws(synop[[1]])
    synop <- gsub("[^ -~]+", "", synop)
    nl <- length(synop)
    synop[nl] <- gsub('=', '', synop[nl])
    synop <- synop[synop != ""]
    iw <- substr(synop[2], 5, 5)
    if(iw != "/"){
        if(grepl('[A-Z]', synop[2])){
            synop <- synop[-2]
        }
    }

    return(synop)
}

isWindFF99 <- function(x){
    iw <- substr(x[2], 5, 5)
    out <- FALSE
    if(iw != "/"){
        if(length(x) >= 5){
            ff <- substr(x[5], 4, 5)
            if(ff != "//"){
                if(ff == "99") out <- TRUE
            }
        }
    }

    return(out)
}
