#' Parse ACTUALS messages.
#'
#' Get data from ACTUALS VEKN strings.
#' 
#' @param vekn character, the full strings of the \code{VEKN} code.\cr
#'              Example: "VEKN20 HKNC 260600 63741 MAX 26.3 MIN 17.7 A/R NIL RH 81\%="
#'  
#' @return a list of the ICAO location indicator, WMO ID , day, hour, minute, precipitation,  
#' maximum and minimum temperature, dry and wet bulb, dew point and vapor pressure.
#' 
#' @examples
#' 
#' \dontrun{
#' library(decodeSYNOP)
#' 
#' vekn <- "VEKN20 HKNC 260600 63741 MAX 26.3 MIN 17.7 A/R NIL RH 81%="
#' out <- get_Vekn_Data(vekn)
#' }
#' ## different forms of vekn messages
#' # "VEKN20 HKNC 281200 63740 DBT=28.0 WBT=18.9 VP=15.9 RH=42%="
#' # "VEKN72 HKGA 250600 63619 MAX-28.0 MIN-19.5 A/R-NIL R/H-85%="
#' # "VEKN71 HKJK 250600 63737 MAX: 27.9; MIN: 13.5; A/R: 0.0MM; R/H: 67%="
#' # "VEKN72 HKGA 250600 63723 TX 34.9, TN 25.7, A/RF TR,R.H 75%="
#' # "VEKN73 HKKI 250600 HKKR(63710) MX=27.5,MN=8.5,R/H=74%,A/RF=NIL="
#' # "VEKN75 HKNC 250600 63746 MAX27.9 MIN18.0 GMIN15.1 ARF1.8 RH87%="
#' # "VEKN73 HKKI 290600 63686 MAX.22.0 MIN.11.1 A/RF.0.0MM RH.60%="
#' # "VEKN75 HKNC 280600 KANGEMA RANET ARFNILL DB20.1 DB16.2 MAX24.8 MIN13.7 DP14.0 RH67% CLD1CI VV30KM WIND24002 GM13.0="
#' 
#' @export

get_Vekn_Data <- function(vekn){
    dec_comma <- gregexpr('[0-9]{2}\\,[0-9]', vekn)
    dec_comma <- dec_comma[[1]]
    if(dec_comma[1] != -1){
        for(i in dec_comma){
            substr(vekn, i + 2, i + 2) <- '.'
        }
    }

    ptrn <- '[A-Z]{4,}\\s*\\(\\s*[0-9]{5,}\\s*\\)'
    vekn <- replace_gregexpr(vekn, ptrn, '\\s+', '')
    ptrn <- '[0-9]+\\.\\.+[0-9]+'
    vekn <- replace_gregexpr(vekn, ptrn, '\\.+', '.')
    vekn <- gsub('\\.\\.+', ' ', vekn)
    ptrn <- '[0-9]+\\.[0-9]+\\.'
    vekn <- replace_gregexpr(vekn, ptrn, '\\.$', '')

    vk <- trimws(strsplit(vekn, " ")[[1]])
    if(length(vk) >= 5){
        if(vk[5] == "NIL=") return(NULL)
    }

    ###
    out <- list(icaoLOC = NA, wmoID = NA)
    out$icaoLOC <- vk[2]

    if(grepl("^[0-9]{6}$", vk[3])){
        out$Day <- substr(vk[3], 1, 2)
        out$Hour <- substr(vk[3], 3, 4)
        out$Minute <- substr(vk[3], 5, 6)
    }else{
        return(NULL)
    }

    if(grepl("^[0-9]{5,}$", vk[4])){
        out$wmoID <- vk[4]
        vekn <- paste0(vk[-(1:4)], collapse = " ")
    }else{
        if(grepl("[A-Z]{4,}\\([0-9]{5,}\\)", vk[4])){
            # tmp <- regexec("\\((.*?)\\)", vk[4])
            # out$wmoID <- regmatches(vk[4], tmp)[[1]][2]
            out$wmoID <- gsub(".*\\((.+)\\).*", "\\1", vk[4])
            vekn <- paste0(vk[-(1:4)], collapse = " ")
        }else{
            vekn <- paste0(vk[-(1:3)], collapse = " ")
        }
    }

    ###
    rr <- c('A\\/RF', 'ARF', 'A\\(RF', 'A\\/FF','A\\/R', 'AR', 'AC\\/RF', 'A\\/RAINFALL')
    rh <- c('R\\/H', 'RH', 'R\\.H')
    tx <- c('MAX', 'Max', 'MX', 'TX')
    tn <- c('MIN', 'Min', 'MN', 'TN')
    gn <- c('G\\/MIN', 'GMIN', 'LGM', 'G\\/M', 'G\\/MN', 'LGMIN', 'GM', 'G/\\s+MIN')
    db <- c('DB', 'DBT')
    wb <- c('WB', 'WBT')
    dp <- c('DP', 'DEW\\s*POINT\\s*TEMP')
    vp <- 'VP'
    patterns <- list(rr, rh, tx, tn, gn, db, wb, dp, vp)
    precip <- paste0(rr, collapse = '|')
    dewpoint <- paste0(dp, collapse = '|')

    var_name <- c('rainfall', 'relative_humidity',
                  'temperature_max', 'temperature_min',
                  'G_MIN', 'dry_bulb', 'wet_bulb',
                  'dew_point', 'vapor_pressure')

    ###
    if(grepl('G/\\s+MIN', vekn)){
        vekn <- gsub('G/\\s+MIN', 'G/MIN', vekn)
    }

    ###
    vekn <- clean_vekn_variables(vekn, patterns)

    tmp <- lapply(patterns, function(ptr){
        ploc <- paste0(ptr, collapse = '|')
        if(ploc == dewpoint){
            dp <- vekn %in% c("DEW", "POINT", "TEMP")
            if(any(dp)){
                id <- which(dp)
                r <- rle(diff(id))
                if(r$lengths == 2 & r$values == 1){
                    vekn <- c(vekn[1:(id[1] - 1)], 'DP',
                        vekn[(id[length(id)] + 1):length(vekn)])
                }
            }
        }

        ptrn <- sapply(ptr, function(p) paste0('^', p, '$'))
        ptrn <- paste0(ptrn, collapse = '|')
        ix <- grep(ptrn, vekn)
        if(length(ix) == 0) return(NA)
        vr <- vekn[ix + 1][1]
        if(ploc == precip & !is.na(vr)){
            rtrace <- tolower(substr(vr, 1, 2))
            if(rtrace[1] == 'tr') vr <- '0.1'
        }
        if(grepl('[a-zA-Z]', vr)) return(NA)
        vr <- gsub("[^0-9.]", "", vr)
        as.numeric(vr)
    })
    names(tmp) <- var_name

    out <- c(out, tmp)

    return(out)
}

replace_gregexpr <- function(string, search, pattern, replace){
    expr <- gregexpr(search, string)[[1]]
    len <- attr(expr, 'match.length')
    ret <- string
    if(expr[1] != -1){
        re <- FALSE
        ss <- 1
        se <- nchar(string)
        nl <- length(expr)
        for(i in 1:nl){
            re <- c(re, TRUE, FALSE)
            ss <- c(ss, expr[i], expr[i] + len[i])
            j <- nl - i + 1
            se <- c(expr[j] - 1, expr[j] + len[j] - 1, se)
        }

        tmp <- lapply(seq_along(re), function(i){
            v <- substr(string, ss[i], se[i])
            if(re[i]) v <- gsub(pattern, replace, v)
            v
        })

        ret <- do.call(paste0, c(tmp, list(collapse = '')))
    }

    return(ret)
}

clean_vekn_variables <- function(strings, patterns){
    strings <- trimws(strings)
    ns <- nchar(strings)
    end <- substr(strings, ns, ns)
    if(end == '=') substr(strings, ns, ns) <- ' '

    strings <- gsub("\\;", " ", strings)
    strings <- gsub("\\,", " ", strings)
    strings <- replace_gregexpr(strings, '[A-Za-z]\\:', '\\:', ' ')
    strings <- replace_gregexpr(strings, '[A-Za-z]\\=', '\\=', ' ')
    strings <- replace_gregexpr(strings, '[A-Za-z]\\-', '\\-', ' ')
    strings <- replace_gregexpr(strings, '[A-Za-z]\\.', '\\.', ' ')

    # obs name and value concatenated (ex: MAX26.4)
    vk <- trimws(strsplit(strings, " ")[[1]])
    for(i in seq_along(patterns)){
        ptrn <- sapply(patterns[[i]], function(p){
            paste0('^', p, '[0-9]')
        })
        ptrn <- paste0(ptrn, collapse = "|")
        rexpr <- gregexpr(ptrn, vk)
        ix <- which(sapply(rexpr, function(x) x[1] != -1))
        if(length(ix) > 0){
            for(j in ix){
                expr <- rexpr[j][[1]]
                n <- attr(expr, 'match.length')
                vr <- substr(vk[j], 1, n - 1)
                vl <- sub(vr, '', vk[j])
                vk[j] <- paste(vr, vl)
            }
        }
    }

    strings <- paste0(vk, collapse = " ")
    strings <- trimws(strsplit(strings, " ")[[1]])
    strings[strings != ""]
}
