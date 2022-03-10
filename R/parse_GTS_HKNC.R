#' Parse GTS Bulletins.
#'
#' Parse GTS bulletin files from Regional Telecommunication Hub (RTH).
#' 
#' @param bulletin_file full path to the file containing the GTS bulletin.
#'  
#' @return A vector of the SYNOP (AAXX) strings of weather reports.
#' 
#' @examples
#' 
#' \dontrun{
#' # GTS bulletin file from Kenya Meteorological Department (HKNC)
#' file_HKNC <- "~/HKNC_bulletins/HKNC50044537.a"
#' synop_strings <- parse_GTS_Bulletin(file_HKNC)
#' }
#' 
#' @export

parse_GTS_Bulletin <- function(bulletin_file){
    rawData <- readLines(bulletin_file, warn = FALSE)
    rawData <- trimws(rawData)
    rawData <- rawData[rawData != ""]

    ## remove on windows?
    rawData <- gsub("\\x*", "", rawData)

    iblk1 <- grep("\\\001", rawData)
    if(length(iblk1) == 0) return(NULL)
    iblk2 <- c(iblk1[-1] - 1, length(rawData))
    rawData <- lapply(seq_along(iblk1), function(j){
        rawData[iblk1[j]:iblk2[j]]
    })
    ix <- sapply(rawData, function(x){
        ii <- grep("AAXX", x)
        if(length(ii > 0)) TRUE else FALSE
    })
    if(!any(ix)) return(NULL)

    rawData <- rawData[ix]

    rawData <- lapply(rawData, function(x){
        ix <- grep("^AAXX", x)
        x <- x[ix:length(x)]
        s <- x[1]
        y <- x[-1]
        ie <- grep("=", y)
        iq1 <- ie[length(ie)] + 1
        if(iq1 < length(y))
            y <- y[-(iq1:length(y))]

        is <- c(1, ie[-length(ie)] + 1)
        y <- lapply(seq_along(is), function(j){
            paste0(y[is[j]:ie[j]], collapse = " ")
        })
        y <- do.call(c, y)
        paste(s, y)
    })

    rawData <- do.call(c, rawData)
    ix <- grep("callto|N I L", rawData)
    if(length(ix) > 0) rawData <- rawData[-ix]

    return(rawData)
}
