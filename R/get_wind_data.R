#' Get wind measurement.
#'
#' Get wind data, group \code{Nddff} and \code{00fff} if exist.
#' 
#' @param synop a synop data object, output of the function \code{get_sectionSynop}.
#'  
#' @return A numeric vector of length 2, c(direction [in degree (0-360)], speed [in m/s]). 
#' 
#' @examples
#' 
#' \dontrun{
#' aaxx <- "AAXX 28154 89022 42698 72506 11360 21382 39758 49808 52006 875// 333 87635="
#' synop <- getSynopSections(aaxx)
#' pres <- getWindData(synop)
#' }
#' 
#' @export

getWindData <- function(synop){
    if(!inherits(synop, "synop"))
        stop("synop is not a 'synop' data object")

    dd <- substr(synop$section1[5], 2, 3)
    out_dd <- NA
    if(dd != "//"){
        if(dd != "99"){
           out_dd <- as.numeric(dd) * 10
        }
    }

    iw <- substr(synop$section1[2], 5, 5)
    out_ff <- NA
    if(iw != "/"){
        ff <- substr(synop$section1[5], 4, 5)
        if(ff != "//"){
            if(ff == "99"){
                ff <- substr(synop$section1[6], 3, 5)
            }

            out_ff <- as.numeric(ff)
            if(iw %in% c("3", "4")){
                out_ff <- 0.514444 * out_ff
            }
        }
    }

    return(c(out_dd, out_ff))
}
