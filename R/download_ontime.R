#' Download On-Time
#'
#' Download On-Time Performance Data directly from BTS/RITA/Transtats website raw data (prezipped file),
#' for SKYNET's import function.
#'
#' @param y year to be imported
#' @param m month to be imported
#' @param auto Automatically assigns object
#'
#' @return A data frame with one row per flight (see \code{\link{import_ontime}}).
#' With \code{auto = TRUE} it is assigned to the global environment as
#' \code{ontime_year_month} (e.g. \code{ontime_2011_1}) and returned invisibly,
#' otherwise it is returned. If the BTS website can't be reached a message is
#' shown and \code{NULL} is returned.
#'
#' @examples
#' \donttest{
#' # Downloads a large file from the BTS website
#' if(interactive()){
#' download_ontime(2011, 1, auto = FALSE)
#' }
#' }
#' @export
#'


download_ontime <- function(y,m, auto = TRUE){

  ontime <- paste("https://transtats.bts.gov/PREZIP/On_Time_Reporting_Carrier_On_Time_Performance_1987_present_",
                  y, "_", m, ".zip", sep = "")

  ontime_path <- paste(tempdir(), "/ontime.zip", sep = "")

  #oldw <- getOption("warn")
  #options(warn = -1)


  #tryCatch(download.file(ontime, ontime_path),
  #         error = function(e) print('Download failed. Please try again'))

  #options(warn = oldw)

  if(httr::http_error(ontime)){
    message("No internet connection or data source broken")
  }else{
  download.file(ontime, ontime_path)


  unzip(ontime_path, paste("On_Time_Reporting_Carrier_On_Time_Performance_(1987_present)_",
                           y, "_", m, ".csv", sep = ""), exdir = tempdir())

  ontime_path <- paste(tempdir(), "/On_Time_Reporting_Carrier_On_Time_Performance_(1987_present)_",
                       y, "_", m, ".csv", sep = "")

  do.call(import_ontime, list(ontime_path, auto))

  }
}

globalVariables(c("auto", "m", "y", "oldw"))

pos <- 1
envir <- as.environment(pos)

