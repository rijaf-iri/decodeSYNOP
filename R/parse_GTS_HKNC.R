#' Parse GTS HKNC Bulletins.
#'
#' Parse GTS bulletins file from Kenya Meteorological Department (HKNC).
#' 
#' @param file_HKNC full path to the file containing the GTS bulletins.
#'  
#' @return A vector of the SYNOP (AAXX) strings of weather reports.
#' 
#' @examples
#' 
#' \dontrun{
#' file_HKNC <- "~/HKNC_bulletins/HKNC50044537.a"
#' synop_strings <- parse_GTS_HKNC_Bulletins(file_HKNC)
#' }
#' 
#' @export

parse_GTS_HKNC_Bulletins <- function(file_HKNC){
    rawData <- readLines(file_HKNC, skipNul = TRUE, warn = FALSE)
    rawData <- trimws(rawData)
    rawData <- rawData[rawData != ""]

    istart <- grep("AAXX", rawData)
    if(length(istart) == 0) return(NULL)

    iend <- c(istart[-1] - 1, length(rawData))
    rawData <- lapply(seq_along(istart), function(j){
        rawData[istart[j]:iend[j]]
    })
    rawData <- lapply(rawData, function(x){
        s <- x[1]
        y <- x[-1]
        ie <- grep("=", y)
        y <- y[-((ie[length(ie)] + 1):length(y))]
        is <- c(1, ie[-length(ie)] + 1)
        y <- lapply(seq_along(is), function(j){
            paste0(y[is[j]:ie[j]], collapse = " ")
        })
        y <- do.call(c, y)
        paste(s, y)
    })

    rawData <- do.call(c, rawData)

    return(rawData)
}
