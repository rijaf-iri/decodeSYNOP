#' Get wind measurement.
#'
#' Get wind data, group \code{Nddff} and \code{00fff} if exist.
#' 
#' @param synop a synop data object, output of the function \code{getSynopSections}.
#'  
#' @return A numeric vector of length 2, c(direction [in degree (0-360)], speed [in m/s]). 
#' 
#' @examples
#' 
#' \dontrun{
#' aaxx <- "AAXX 28154 89022 42698 72506 11360 21382 39758 49808 52006 875// 333 87635="
#' synop <- getSynopSections(aaxx)
#' wnd <- getWindData(synop)
#' }
#' 
#' @export

getWindData <- function(synop){
    if(!inherits(synop, "synop"))
        stop("synop is not a 'synop' data object")

    if(!isWindFF99(synop$section1)) return(c(NA, NA))

    dd <- substr(synop$section1[5], 2, 3)
    out_dd <- NA
    if(!grepl('\\/', dd)){
        if(dd != "99"){
           out_dd <- as.numeric(dd) * 10
        }
    }

    iw <- substr(synop$section1[2], 5, 5)
    out_ff <- NA
    if(iw != "/" && iw != ""){
        ff <- substr(synop$section1[5], 4, 5)
        if(!grepl('\\/', ff)){
            if(ff == "99"){
                grp0 <- synop$section1[6]
                if(substr(grp0, 1, 1) == '0'){
                    ff <- substr(grp0, 3, 5)
                }else{
                    ff <- NA
                }
            }

            out_ff <- as.numeric(ff)
            if(iw %in% c("3", "4")){
                out_ff <- round(0.514444 * out_ff, 1)
            }
        }
    }

    return(c(out_dd, out_ff))
}
