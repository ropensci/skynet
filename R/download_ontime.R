#' Download On-Time
#'
#' Download On-Time Performance Data directly from BTS/RITA/Transtats website raw data (prezipped file),
#' for SKYNET's import function.
#'
#' @param y year to be imported
#' @param m month to be imported
#' @param auto Automatically assigns object
#'
#'
#' @examples
#' \dontrun{
#'
#' import_ontime(skynet_example("Ontime.csv"))
#'
#' }
#' @export
#'


download_ontime <- function(y,m, auto = TRUE){

  ontime <- paste("https://transtats.bts.gov/PREZIP/On_Time_Reporting_Carrier_On_Time_Performance_1987_present_",
                  y, "_", m, ".zip", sep = "")

  ontime_path <- paste(tempdir(), "/ontime.zip", sep = "")
  download.file(ontime, ontime_path)
  unzip(ontime_path, paste("On_Time_Reporting_Carrier_On_Time_Performance_(1987_present)_",
                           y, "_", m, ".csv", sep = ""), exdir = paste(tempdir(),
                                                                       "/", sep = ""))

  ontime_path <- paste(tempdir(), "/On_Time_Reporting_Carrier_On_Time_Performance_(1987_present)_",
                       y, "_", m, ".csv", sep = "")

  do.call(import_ontime, list(ontime_path, auto))

}

globalVariables(c("auto", "m", "y"))

pos <- 1
envir <- as.environment(pos)

