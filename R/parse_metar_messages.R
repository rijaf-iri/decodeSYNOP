#' Parse METAR code.
#'
#' Get data from METAR strings of weather reports.
#' 
#' @param metar character, the full strings of the metar code.\cr
#'              Example: "METAR HUEN 230700Z 08008KT 9999 SCT022 FEW025 26/22 Q1016 NOSIG="
#'  
#' @return a list of the ICAO location indicator, day, hour, minute, wind speed and direction, 
#' temperature, dew point and QNH.
#' 
#' @examples
#' 
#' \dontrun{
#' library(decodeSYNOP)
#' 
#' metar <- "METAR HUEN 230700Z 08008KT 9999 SCT022 FEW025 26/22 Q1016 NOSIG="
#' out <- get_Metar_Data(metar)
#' }
#' 
#' @export

get_Metar_Data <- function(metar){
    msg <- split_rawMetar_Data(metar)
    if(is.null(msg)) return(NULL)
    if(msg[4] == "NIL") return(NULL)

    out <- list(icaoLOC = NA, Day = NA,
                Hour = NA, Minute = NA,
                indicator = NA,
                wind_dir = NA, wind_gust = NA,
                wind_spd = NA, wind_dir_var1 = NA,
                wind_dir_var2 = NA, air_temp = NA,
                dew_temp = NA, QNH = NA)

    out$icaoLOC <- msg[2]
    out$Day <- substr(msg[3], 1, 2)
    out$Hour <- substr(msg[3], 3, 4)
    out$Minute <- substr(msg[3], 5, 6)

    indicator <- FALSE
    if(msg[4] == "AUTO"){
        out$indicator <- msg[4]
        msg <- msg[-(1:4)]
        indicator <- TRUE
    }

    if(nchar(msg[4]) == 3){
        out$indicator <- msg[4]
        msg <- msg[-(1:4)]
        indicator <- TRUE
    }

    if(!indicator) msg <- msg[-(1:3)]

    ## wind direction and speed
    dd <- substr(msg[1], 1, 3)
    out$wind_dir <- dd
    if(grepl('\\/', dd)) out$wind_dir <- NA
    if(dd == "VRB") out$wind_dir <- "VRB"

    if(substr(msg[1], 4, 4) == "P"){
        # ff <- substr(msg[1], 5, 6)
        # fu <- substr(msg[1], 7, nchar(msg[1]))
        # if(fu == "KT" & ff == '99')
        # if(fu == "MPS" & ff == '49')
        # if(fu == "KPH" & ff == '49')
        out$wind_spd <- 50
    }else{
        ff <- substr(msg[1], 4, 5)
        if(!grepl('\\/', ff)){
            if(!grepl('[^[:digit:]]', ff)){
                out$wind_spd <- as.numeric(ff)
            }
        }

        if(substr(msg[1], 6, 6) == "G"){
            gust <- substr(msg[1], 7, 8)
            if(!grepl('[^[:digit:]]', gust)){
                out$wind_gust <- as.numeric(gust)
            }
            fu <- substr(msg[1], 9, nchar(msg[1]))
        }else{
            fu <- substr(msg[1], 6, nchar(msg[1]))
        }

        if(fu == "KT"){
            out$wind_spd <- out$wind_spd * 0.5144444444
            out$wind_gust <- out$wind_gust * 0.5144444444
        }
    }

    out$wind_gust <- round(out$wind_gust, 1)
    out$wind_spd <- round(out$wind_spd, 1)

    ## Variable wind direction group
    pattern_wd <- "^[0-9]{3}[V][0-9]{3}$"
    wind_dir_var <- grep(pattern_wd, msg, value = TRUE)
    if(length(wind_dir_var) > 0){
        out$wind_dir_var1 <- substr(wind_dir_var[1], 1, 3)
        out$wind_dir_var2 <- substr(wind_dir_var[1], 5, 7)
    }

    ## Temperature/dew point
    pattern_tt <- "^[M]?([0-9]{2}|//)/[M]?([0-9]{2}|//)$"
    temp <- grep(pattern_tt, msg, value = TRUE)
    if(length(temp) > 0){
        temp <- temp[1]
        tt1 <- substr(temp, 1, 1)
        if(tt1 == 'M'){
            out$air_temp <- -1 * as.numeric(substr(temp, 2, 3))
            tt2 <- substr(temp, 5, 5)
            if(tt2 == 'M'){
                out$dew_temp <- -1 * as.numeric(substr(temp, 6, 7))
            }else{
                if(grepl("[0-9]", tt2)){
                out$dew_temp <- as.numeric(substr(temp, 5, 6))
                }
            }
        }else{
            if(grepl("[0-9]", tt1)){
                out$air_temp <- as.numeric(substr(temp, 1, 2))
                tt2 <- substr(temp, 4, 4)
                if(tt2 == 'M'){
                    out$dew_temp <- -1 * as.numeric(substr(temp, 5, 6))
                }else{
                    if(grepl("[0-9]", tt2)){
                        out$dew_temp <- as.numeric(substr(temp, 4, 5))
                    }
                }
            }
        }
    }

    ## QNH
    pattern_pr <- "^[Q][0-9]{4}"
    pres <- grep(pattern_pr, msg, value = TRUE)
    if(length(pres) > 0){
       out$QNH <- as.numeric(substr(pres[1], 2, 5))
    }

    if(!'CAVOK' %in% msg){
        ## Visibility
        # pattern_vs <- "^[0-9]{4}$"
        # vis <- grep(pattern_vs, msg, value = TRUE)
        # if(length(vis) > 0){
        #     if(vis = '9999'){
        #         out$visibility <- 10000
        #     }else{
        #         if(vis = '0000'){
        #             out$visibility <- 50
        #         }else{
        #             out$visibility <- as.numeric(vis)
        #         }
        #     }
        # }

        ## visibility in statute miles 1SM

        ## Cloud (sky condition)
        # FEW = few (1-2 oktas)
        # SCT = scattered (3-4 oktas)
        # BKN = broken (5-7 oktas)
        # OVC = overcast (8 oktas)

        # FEW023CB: few clouds, 
        #           height of the cloud base 2300 feet
        #           Significant convective cloud 
        # FEW023TCU
        # FEW022, SCT022, BKN019, ...

        ## weather phenomena
    }
    ## Runway Visual Range (RVR)

    return(out)
}

split_rawMetar_Data <- function(metar){
    metar <- strsplit(metar, ' ')
    metar <- trimws(metar[[1]])
    nl <- length(metar)
    metar[nl] <- gsub("=", "", metar[nl])
    metar <- metar[metar != ""]
    metar <- gsub("[^ -~]+", "", metar)
    if(length(metar) < 4) return(NULL)

    # remove code COR: correction to a previously 
    # disseminated observation
    if(metar[2] == 'COR') metar <- metar[-2]

    is_icao <- grepl("^[A-Z]{4}$", metar[2])
    if(!is_icao) return(NULL)
    is_timeZ <- grepl("^[0-9]{6}[Z]$", metar[3])
    if(!is_timeZ) return(NULL)

    return(metar)
}

