#' Download Data from DB1B files
#'
#' Downloads data from BTS/RITA/Transtats and imports it into R
#'
#' Coupon files can be found at \url{https://www.transtats.bts.gov/Fields.asp?gnoyr_VQ=FLM}.
#' Ticket files can be found at \url{https://www.transtats.bts.gov/Fields.asp?gnoyr_VQ=FKF}.
#'
#' Note: The BTS often changes the way we can access these files. So please be warned that this is still an experimental feature.
#'
#' @param y year to be imported
#' @param q quarter to be imported
#'
#' @examples
#' \dontrun{
#'
#' download_db1b(2010, 1)
#'
#' }
#' @export
#'
#'

download_db1b <- function(y = NULL, q = NULL){

  options(timeout = max(800, getOption("timeout"))) #Set larger timeout

  couponname <- paste("https://transtats.bts.gov/PREZIP/Origin_and_Destination_Survey_DB1BCoupon_",
                      y, "_", q, ".zip", sep = "")
  ticketname <- paste("https://transtats.bts.gov/PREZIP/Origin_and_Destination_Survey_DB1BTicket_",
                      y, "_", q, ".zip", sep = "")

  couponpath <- paste(tempdir(), "/coupon.zip", sep = "")
  ticketpath <- paste(tempdir(), "/ticket.zip", sep = "")

  #oldw <- getOption("warn")
  #options(warn = -1)
  #tryCatch(download.file(couponname, couponpath),
  #         error = function(e) print('Download failed. Please try again'))
  #tryCatch(download.file(ticketname, ticketpath),
  #         error = function(e) print('Download failed. Please try again'))
  #options(warn = oldw)


  if(httr::http_error(couponname)){
    message("No internet connection or data source broken")
  }else{

  download.file(couponname, couponpath)
  download.file(ticketname, ticketpath)

  unzip(couponpath, paste("Origin_and_Destination_Survey_DB1BCoupon_",
                          y, "_", q, ".csv", sep = ""),
        exdir = tempdir())
  unzip(ticketpath, paste("Origin_and_Destination_Survey_DB1BTicket_",
                          y, "_", q, ".csv", sep = ""),
       exdir = tempdir())

  couponpath <- paste(tempdir(),"/Origin_and_Destination_Survey_DB1BCoupon_",
                      y, "_", q, ".csv", sep = "")
  ticketpath <- paste(tempdir(), "/Origin_and_Destination_Survey_DB1BTicket_",
                      y, "_", q, ".csv", sep = "")

  do.call(import_db1b, list(couponpath, ticketpath, zip = TRUE))

  }
}

globalVariables(c("download.file", "unzip"))


pos <- 1
envir <- as.environment(pos)
