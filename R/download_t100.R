#' Download Data from T100 files
#'
#' Downloads data from BTS/RITA/Transtats and imports it into R
#'
#' Note: The BTS often changes the way we can access these files. So please be warned that this is still an experimental feature.
#'
#' @param y year to be imported
#' @param type "mkt" for Market, "seg" for Segment databases respectively
#'
#' @examples
#' \dontrun{
#'
#' download_t100(2010, "mkt")
#'
#' }
#' @export
#'
#'

download_t100 <- function(y = NULL, type = NULL){

  y <- paste(y)

  if(is.null(type)){
    message("Please select mkt - market or seg - segment database.")
    stop()
  }
  if(type == "mkt"){

    # Get viewstate, and event
    curl <- getCurlHandle()
    curlSetOpt(cookiejar = paste(tempdir(), "/", "cookies.txt", sep = ""),
               followlocation = TRUE, autoreferer = TRUE, curl = curl)

    message("Connecting to T100")
    html <- getURL('https://www.transtats.bts.gov/DL_SelectFields.aspx?gnoyr_VQ=FIL&QO_fu146_anzr=Nv4%20Pn44vr45', curl = curl)
    message("Done")

    viewstate <- as.character(sub('.*id="__VIEWSTATE" value="([0-9a-zA-Z+/=]*).*', '\\1', html))
    viewstategenerator <- as.character(sub('.*id="__VIEWSTATEGENERATOR" value="([0-9a-zA-Z+/=]*).*', '\\1', html))

    eventvalidation <- as.character(sub('.*id="__EVENTVALIDATION" value="([0-9a-zA-Z+/=]*).*', '\\1', html))

    message("Downloading file")
    httr::POST(
      config = progress(),
      url = "https://www.transtats.bts.gov/DL_SelectFields.aspx?gnoyr_VQ=FIL&QO_fu146_anzr=Nv4+Pn44vr45",
      httr::add_headers(
        Referer = "https://www.transtats.bts.gov/DL_SelectFields.aspx?gnoyr_VQ=FIL&QO_fu146_anzr=Nv4%20Pn44vr45"
      ),
      body = list(
        '__EVENTARGUMENT' = "",
        '__LASTFOCUS' = "",
        "__VIEWSTATE" = viewstate,
        '__VIEWSTATEGENERATOR' = viewstategenerator,
        '__EVENTVALIDATION' = eventvalidation,
        txtSearch = "",
        btnDownload = "Download",
        cboGeography = "All",
        cboYear = y,
        cboPeriod = "All",
        chkAllVars = "on",
        UNIQUE_CARRIER = "on",
        UNIQUE_CARRIER_NAME = "on",
        ORIGIN_AIRPORT_ID = "on",
        ORIGIN = "on",
        DEST_AIRPORT_ID = "on",
        DEST = "on",
        MONTH = "on"

      ),
      encode = "form",
      query = list(
        gnoyr_VQ = "FIL",
        QO_fu146_anzr = "Nv4+Pn44vr45"
      ),
      postData = list(
        text = paste("__EVENTTARGET=&__EVENTARGUMENT=&__LASTFOCUS=&__VIEWSTATE=", viewstate,
                     "&__VIEWSTATEGENERATOR=", viewstategenerator,
                     "&__EVENTVALIDATION=", eventvalidation,
                     "&txtSearch=&cboGeography=All&cboYear=",y,"&cboPeriod=All&btnDownload=Download&UNIQUE_CARRIER=on&UNIQUE_CARRIER_NAME=on&ORIGIN_AIRPORT_ID=on&ORIGIN=on&DEST_AIRPORT_ID=on&DEST=on&MONTH=on",
                     sep = "")
      )
    ) -> res

  }

  if(type == "seg"){

    # Get viewstate, and event
    curl <- getCurlHandle()
    curlSetOpt(cookiejar = paste(tempdir(), "/", "cookies.txt", sep = ""),
               followlocation = TRUE, autoreferer = TRUE, curl = curl)

    message("Connecting to T100")
    html <- getURL("https://www.transtats.bts.gov/DL_SelectFields.aspx?gnoyr_VQ=FIM&QO_fu146_anzr=Nv4%25Pn44vr45", curl = curl)
    message("Done")

    viewstate <- as.character(sub('.*id="__VIEWSTATE" value="([0-9a-zA-Z+/=]*).*', '\\1', html))
    viewstategenerator <- as.character(sub('.*id="__VIEWSTATEGENERATOR" value="([0-9a-zA-Z+/=]*).*', '\\1', html))

    eventvalidation <- as.character(sub('.*id="__EVENTVALIDATION" value="([0-9a-zA-Z+/=]*).*', '\\1', html))

    message("Downloading file")

    httr::POST(
      config = progress(),
      url = "https://www.transtats.bts.gov/DL_SelectFields.aspx?gnoyr_VQ=FIM&QO_fu146_anzr=Nv4+Pn44vr45",
      httr::add_headers(
        Referer = "https://www.transtats.bts.gov/DL_SelectFields.aspx?gnoyr_VQ=FIM&QO_fu146_anzr=Nv4%25Pn44vr45"
      ),
      body = list(
        '__EVENTARGUMENT' = "",
        '__LASTFOCUS' = "",
        "__VIEWSTATE" = viewstate,
        '__VIEWSTATEGENERATOR' = viewstategenerator,
        '__EVENTVALIDATION' = eventvalidation,
        txtSearch = "",
        btnDownload = "Download",
        cboGeography = "All",
        cboYear = y,
        cboPeriod = "All",
        chkAllVars = "on",
        UNIQUE_CARRIER = "on",
        UNIQUE_CARRIER_NAME = "on",
        ORIGIN_AIRPORT_ID = "on",
        ORIGIN = "on",
        DEST_AIRPORT_ID = "on",
        DEST = "on",
        MONTH = "on"

      ),
      encode = "form", query = list(
        gnoyr_VQ = "FIM",
        QO_fu146_anzr = "Nv4+Pn44vr45"
      ),
      postData = list(
        text = paste("__EVENTTARGET=&__EVENTARGUMENT=&__LASTFOCUS=&__VIEWSTATE=", viewstate,
                     "&__VIEWSTATEGENERATOR=", viewstategenerator,
                     "&__EVENTVALIDATION=", eventvalidation,
                     "&txtSearch=&cboGeography=All&cboYear=",y,"&cboPeriod=All&btnDownload=Download&UNIQUE_CARRIER=on&UNIQUE_CARRIER_NAME=on&ORIGIN_AIRPORT_ID=on&ORIGIN=on&DEST_AIRPORT_ID=on&DEST=on&MONTH=on",
                     sep = "")
      )
    ) -> res
  }

if(httr::http_error(res)){
  message("No internet connection or data source broken")
}else{

(save_to <- file.path(tempdir(),
                      basename(grep("\\.zip",
                                    unlist(res$all_headers), value=TRUE))))

writeBin(httr::content(res, as="raw"), save_to)
unzip(save_to, exdir = tempdir())

t100path <- paste(tempdir(), "/", unzip(save_to, list = TRUE)$Name
                  ,sep = "")

file.rename(t100path, paste(tempdir(), "/", "T100_", y,
                            "_", type, ".csv", sep = ""))

t100path <- paste(tempdir(), "/", "T100_", y,
                  "_", type, ".csv", sep = "")

do.call(import_t100, list(t100path))
}
}

globalVariables(c("writeBin", "unzip"))


pos <- 1
envir <- as.environment(pos)

options(timeout = max(800, getOption("timeout"))) #Set larger timeout
