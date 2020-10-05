#' Download Data from T100 international files
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
#' download_t100_int(2010, "mkt")
#'
#' }
#' @export
#'
#'

download_t100_int <- function(y = NULL, type = NULL){

  y <- paste(y)

  if(is.null(type)){
    message("Please select mkt - market or seg - segment database.")
    stop()
  }
  if(type == "mkt"){
    httr::POST(
      url = "https://www.transtats.bts.gov/DownLoad_Table.asp",
      httr::add_headers(
        Referer = "https://www.transtats.bts.gov/DL_SelectFields.asp?Table_ID=260"
      ),
      body = list(
        UserTableName = "T_100_International_Market__All_Carriers",
        DBShortName = "", RawDataTable = "T_T100I_MARKET_ALL_CARRIER",
        sqlstr = paste(" SELECT PASSENGERS,FREIGHT,MAIL,DISTANCE,UNIQUE_CARRIER,AIRLINE_ID,UNIQUE_CARRIER_NAME,UNIQUE_CARRIER_ENTITY,REGION,CARRIER,CARRIER_NAME,CARRIER_GROUP,CARRIER_GROUP_NEW,ORIGIN_AIRPORT_ID,ORIGIN_AIRPORT_SEQ_ID,ORIGIN_CITY_MARKET_ID,ORIGIN,ORIGIN_CITY_NAME,ORIGIN_STATE_ABR,ORIGIN_STATE_FIPS,ORIGIN_STATE_NM,ORIGIN_WAC,DEST_AIRPORT_ID,DEST_AIRPORT_SEQ_ID,DEST_CITY_MARKET_ID,DEST,DEST_CITY_NAME,DEST_STATE_ABR,DEST_STATE_FIPS,DEST_STATE_NM,DEST_WAC,YEAR,QUARTER,MONTH,DISTANCE_GROUP,CLASS FROM T_T100D_MARKET_US_CARRIER_ONLY WHERE YEAR=", y),
        varlist = "PASSENGERS,FREIGHT,MAIL,DISTANCE,UNIQUE_CARRIER,AIRLINE_ID,UNIQUE_CARRIER_NAME,UNIQUE_CARRIER_ENTITY,REGION,CARRIER,CARRIER_NAME,CARRIER_GROUP,CARRIER_GROUP_NEW,ORIGIN_AIRPORT_ID,ORIGIN_AIRPORT_SEQ_ID,ORIGIN_CITY_MARKET_ID,ORIGIN,ORIGIN_CITY_NAME,ORIGIN_STATE_ABR,ORIGIN_STATE_FIPS,ORIGIN_STATE_NM,ORIGIN_WAC,DEST_AIRPORT_ID,DEST_AIRPORT_SEQ_ID,DEST_CITY_MARKET_ID,DEST,DEST_CITY_NAME,DEST_STATE_ABR,DEST_STATE_FIPS,DEST_STATE_NM,DEST_WAC,YEAR,QUARTER,MONTH,DISTANCE_GROUP,CLASS",
        grouplist = "", suml = "",
        sumRegion = "", filter1 = "title=",
        filter2 = "title=", geo = "All\xa0",
        time = "All\xa0Months",
        timename = "Month", GEOGRAPHY = "All",
        XYEAR = y, FREQUENCY = "All",
        AllVars = "All", VarName = "PASSENGERS",
        VarDesc = "Passengers",
        VarType = "Num", VarName = "FREIGHT",
        VarDesc = "Freight", VarType = "Num",
        VarName = "MAIL", VarDesc = "Mail",
        VarType = "Num", VarName = "DISTANCE",
        VarDesc = "Distance", VarType = "Num",
        VarName = "UNIQUE_CARRIER",
        VarDesc = "UniqueCarrier",
        VarType = "Char", VarName = "AIRLINE_ID",
        VarDesc = "AirlineID",
        VarType = "Num", VarName = "UNIQUE_CARRIER_NAME",
        VarDesc = "UniqueCarrierName",
        VarType = "Char", VarName = "UNIQUE_CARRIER_ENTITY",
        VarDesc = "UniqCarrierEntity",
        VarType = "Char", VarName = "REGION",
        VarDesc = "CarrierRegion",
        VarType = "Char", VarName = "CARRIER",
        VarDesc = "Carrier", VarType = "Char",
        VarName = "CARRIER_NAME",
        VarDesc = "CarrierName",
        VarType = "Char", VarName = "CARRIER_GROUP",
        VarDesc = "CarrierGroup",
        VarType = "Num", VarName = "CARRIER_GROUP_NEW",
        VarDesc = "CarrierGroupNew",
        VarType = "Num", VarName = "ORIGIN_AIRPORT_ID",
        VarDesc = "OriginAirportID",
        VarType = "Num", VarName = "ORIGIN_AIRPORT_SEQ_ID",
        VarDesc = "OriginAirportSeqID",
        VarType = "Num", VarName = "ORIGIN_CITY_MARKET_ID",
        VarDesc = "OriginCityMarketID",
        VarType = "Num", VarName = "ORIGIN",
        VarDesc = "Origin", VarType = "Char",
        VarName = "ORIGIN_CITY_NAME",
        VarDesc = "OriginCityName",
        VarType = "Char", VarName = "ORIGIN_STATE_ABR",
        VarDesc = "OriginState",
        VarType = "Char", VarName = "ORIGIN_STATE_FIPS",
        VarDesc = "OriginStateFips",
        VarType = "Char", VarName = "ORIGIN_STATE_NM",
        VarDesc = "OriginStateName",
        VarType = "Char", VarName = "ORIGIN_WAC",
        VarDesc = "OriginWac",
        VarType = "Num", VarName = "DEST_AIRPORT_ID",
        VarDesc = "DestAirportID",
        VarType = "Num", VarName = "DEST_AIRPORT_SEQ_ID",
        VarDesc = "DestAirportSeqID",
        VarType = "Num", VarName = "DEST_CITY_MARKET_ID",
        VarDesc = "DestCityMarketID",
        VarType = "Num", VarName = "DEST",
        VarDesc = "Dest", VarType = "Char",
        VarName = "DEST_CITY_NAME",
        VarDesc = "DestCityName",
        VarType = "Char", VarName = "DEST_STATE_ABR",
        VarDesc = "DestState",
        VarType = "Char", VarName = "DEST_STATE_FIPS",
        VarDesc = "DestStateFips",
        VarType = "Char", VarName = "DEST_STATE_NM",
        VarDesc = "DestStateName",
        VarType = "Char", VarName = "DEST_WAC",
        VarDesc = "DestWac", VarType = "Num",
        VarName = "YEAR", VarDesc = "Year",
        VarType = "Num", VarName = "QUARTER",
        VarDesc = "Quarter", VarType = "Num",
        VarName = "MONTH", VarDesc = "Month",
        VarType = "Num", VarName = "DISTANCE_GROUP",
        VarDesc = "DistanceGroup",
        VarType = "Num", VarName = "CLASS",
        VarDesc = "Class", VarType = "Char"
      ),
      encode = "form", query = list(
        Table_ID = "260",
        Has_Group = "0", Is_Zipped = "0"
      )
    ) -> res
  }
  if(type == "seg"){
    httr::POST(
      url = "https://www.transtats.bts.gov/DownLoad_Table.asp",
      httr::add_headers(
        Referer = "https://www.transtats.bts.gov/DL_SelectFields.asp?Table_ID=261"
      ),
      body = list(
        UserTableName = "T_100_International_Segment__All_Carriers",
        DBShortName = "Air_Carriers",
        RawDataTable = "T_T100I_SEGMENT_ALL_CARRIER",
        sqlstr = paste(" SELECT DEPARTURES_SCHEDULED,DEPARTURES_PERFORMED,PAYLOAD,SEATS,PASSENGERS,FREIGHT,MAIL,DISTANCE,RAMP_TO_RAMP,AIR_TIME,UNIQUE_CARRIER,AIRLINE_ID,UNIQUE_CARRIER_NAME,UNIQUE_CARRIER_ENTITY,REGION,CARRIER,CARRIER_NAME,CARRIER_GROUP,CARRIER_GROUP_NEW,ORIGIN_AIRPORT_ID,ORIGIN_AIRPORT_SEQ_ID,ORIGIN_CITY_MARKET_ID,ORIGIN,ORIGIN_CITY_NAME,ORIGIN_STATE_ABR,ORIGIN_STATE_FIPS,ORIGIN_STATE_NM,ORIGIN_WAC,DEST_AIRPORT_ID,DEST_AIRPORT_SEQ_ID,DEST_CITY_MARKET_ID,DEST,DEST_CITY_NAME,DEST_STATE_ABR,DEST_STATE_FIPS,DEST_STATE_NM,DEST_WAC,AIRCRAFT_GROUP,AIRCRAFT_TYPE,AIRCRAFT_CONFIG,YEAR,QUARTER,MONTH,DISTANCE_GROUP,CLASS FROM  T_T100D_SEGMENT_US_CARRIER_ONLY WHERE YEAR=",y),
        varlist = "DEPARTURES_SCHEDULED,DEPARTURES_PERFORMED,PAYLOAD,SEATS,PASSENGERS,FREIGHT,MAIL,DISTANCE,RAMP_TO_RAMP,AIR_TIME,UNIQUE_CARRIER,AIRLINE_ID,UNIQUE_CARRIER_NAME,UNIQUE_CARRIER_ENTITY,REGION,CARRIER,CARRIER_NAME,CARRIER_GROUP,CARRIER_GROUP_NEW,ORIGIN_AIRPORT_ID,ORIGIN_AIRPORT_SEQ_ID,ORIGIN_CITY_MARKET_ID,ORIGIN,ORIGIN_CITY_NAME,ORIGIN_STATE_ABR,ORIGIN_STATE_FIPS,ORIGIN_STATE_NM,ORIGIN_WAC,DEST_AIRPORT_ID,DEST_AIRPORT_SEQ_ID,DEST_CITY_MARKET_ID,DEST,DEST_CITY_NAME,DEST_STATE_ABR,DEST_STATE_FIPS,DEST_STATE_NM,DEST_WAC,AIRCRAFT_GROUP,AIRCRAFT_TYPE,AIRCRAFT_CONFIG,YEAR,QUARTER,MONTH,DISTANCE_GROUP,CLASS",
        grouplist = "", suml = "",
        sumRegion = "", filter1 = "title=",
        filter2 = "title=", geo = "All\xa0",
        time = "All\xa0Months",
        timename = "Month", GEOGRAPHY = "All",
        XYEAR = y, FREQUENCY = "All",
        AllVars = "All", VarName = "DEPARTURES_SCHEDULED",
        VarDesc = "DepScheduled",
        VarType = "Num", VarName = "DEPARTURES_PERFORMED",
        VarDesc = "DepPerformed",
        VarType = "Num", VarName = "PAYLOAD",
        VarDesc = "Payload", VarType = "Num",
        VarName = "SEATS", VarDesc = "Seats",
        VarType = "Num", VarName = "PASSENGERS",
        VarDesc = "Passengers",
        VarType = "Num", VarName = "FREIGHT",
        VarDesc = "Freight", VarType = "Num",
        VarName = "MAIL", VarDesc = "Mail",
        VarType = "Num", VarName = "DISTANCE",
        VarDesc = "Distance", VarType = "Num",
        VarName = "RAMP_TO_RAMP",
        VarDesc = "RampTime", VarType = "Num",
        VarName = "AIR_TIME", VarDesc = "AirTime",
        VarType = "Num", VarName = "UNIQUE_CARRIER",
        VarDesc = "UniqueCarrier",
        VarType = "Char", VarName = "AIRLINE_ID",
        VarDesc = "AirlineID",
        VarType = "Num", VarName = "UNIQUE_CARRIER_NAME",
        VarDesc = "UniqueCarrierName",
        VarType = "Char", VarName = "UNIQUE_CARRIER_ENTITY",
        VarDesc = "UniqCarrierEntity",
        VarType = "Char", VarName = "REGION",
        VarDesc = "CarrierRegion",
        VarType = "Char", VarName = "CARRIER",
        VarDesc = "Carrier", VarType = "Char",
        VarName = "CARRIER_NAME",
        VarDesc = "CarrierName",
        VarType = "Char", VarName = "CARRIER_GROUP",
        VarDesc = "CarrierGroup",
        VarType = "Num", VarName = "CARRIER_GROUP_NEW",
        VarDesc = "CarrierGroupNew",
        VarType = "Num", VarName = "ORIGIN_AIRPORT_ID",
        VarDesc = "OriginAirportID",
        VarType = "Num", VarName = "ORIGIN_AIRPORT_SEQ_ID",
        VarDesc = "OriginAirportSeqID",
        VarType = "Num", VarName = "ORIGIN_CITY_MARKET_ID",
        VarDesc = "OriginCityMarketID",
        VarType = "Num", VarName = "ORIGIN",
        VarDesc = "Origin", VarType = "Char",
        VarName = "ORIGIN_CITY_NAME",
        VarDesc = "OriginCityName",
        VarType = "Char", VarName = "ORIGIN_STATE_ABR",
        VarDesc = "OriginState",
        VarType = "Char", VarName = "ORIGIN_STATE_FIPS",
        VarDesc = "OriginStateFips",
        VarType = "Char", VarName = "ORIGIN_STATE_NM",
        VarDesc = "OriginStateName",
        VarType = "Char", VarName = "ORIGIN_WAC",
        VarDesc = "OriginWac",
        VarType = "Num", VarName = "DEST_AIRPORT_ID",
        VarDesc = "DestAirportID",
        VarType = "Num", VarName = "DEST_AIRPORT_SEQ_ID",
        VarDesc = "DestAirportSeqID",
        VarType = "Num", VarName = "DEST_CITY_MARKET_ID",
        VarDesc = "DestCityMarketID",
        VarType = "Num", VarName = "DEST",
        VarDesc = "Dest", VarType = "Char",
        VarName = "DEST_CITY_NAME",
        VarDesc = "DestCityName",
        VarType = "Char", VarName = "DEST_STATE_ABR",
        VarDesc = "DestState",
        VarType = "Char", VarName = "DEST_STATE_FIPS",
        VarDesc = "DestStateFips",
        VarType = "Char", VarName = "DEST_STATE_NM",
        VarDesc = "DestStateName",
        VarType = "Char", VarName = "DEST_WAC",
        VarDesc = "DestWac", VarType = "Num",
        VarName = "AIRCRAFT_GROUP",
        VarDesc = "AircraftGroup",
        VarType = "Num", VarName = "AIRCRAFT_TYPE",
        VarDesc = "AircraftType",
        VarType = "Char", VarName = "AIRCRAFT_CONFIG",
        VarDesc = "AircraftConfig",
        VarType = "Num", VarName = "YEAR",
        VarDesc = "Year", VarType = "Num",
        VarName = "QUARTER", VarDesc = "Quarter",
        VarType = "Num", VarName = "MONTH",
        VarDesc = "Month", VarType = "Num",
        VarName = "DISTANCE_GROUP",
        VarDesc = "DistanceGroup",
        VarType = "Num", VarName = "CLASS",
        VarDesc = "Class", VarType = "Char"
      ),
      encode = "form", query = list(
        Table_ID = "261",
        Has_Group = "3", Is_Zipped = "0"
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
  unzip(save_to, exdir = paste(tempdir(), "/", sep = ""))

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
