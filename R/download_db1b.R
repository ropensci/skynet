#' Download Data from DB1B files
#'
#' Downloads data from BTS/RITA/Transtats and imports it into R
#'
#' Coupon files are downloaded from \url{https://www.transtats.bts.gov/DL_SelectFields.asp?Table_ID=289}.
#' Ticket files are downloaded from  \url{https://www.transtats.bts.gov/DL_SelectFields.asp?Table_ID=272}.
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

  dir.create(paste(path))

  couponname <- paste("https://transtats.bts.gov/PREZIP/Origin_and_Destination_Survey_DB1BCoupon_",
                      y, "_", q, ".zip", sep = "")
  ticketname <- paste("https://transtats.bts.gov/PREZIP/Origin_and_Destination_Survey_DB1BTicket_",
                      y, "_", q, ".zip", sep = "")

  couponpath <- paste(tempdir(), "/coupon.zip", sep = "")
  ticketpath <- paste(tempdir(), "/ticket.zip", sep = "")

  download.file(couponname, couponpath)
  download.file(ticketname, ticketpath)

  unzip(couponpath, paste("Origin_and_Destination_Survey_DB1BCoupon_",
                          y, "_", q, ".csv", sep = ""),
        exdir = paste(tempdir(), "/", sep = ""))
  unzip(ticketpath, paste("Origin_and_Destination_Survey_DB1BTicket_",
                          y, "_", q, ".csv", sep = ""),
       exdir = paste(tempdir(), "/", sep = ""))

  couponpath <- paste(tempdir(),"/Origin_and_Destination_Survey_DB1BCoupon_",
                      y, "_", q, ".csv", sep = "")
  ticketpath <- paste(tempdir(), "/Origin_and_Destination_Survey_DB1BTicket_",
                      y, "_", q, ".csv", sep = "")

  do.call(import_db1b, list(couponpath, ticketpath, zip = TRUE))

}

globalVariables(c("download.file", "unzip"))


pos <- 1
envir <- as.environment(pos)
