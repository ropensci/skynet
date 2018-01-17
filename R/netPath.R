#' Path and OD Network
#'
#' Generates an OD network and a Leg Count data frame(on demand)
#'
#' @param x Data frame
#' @param leg Generates Leg Count Data frame, based on Path taken.
#' @param zero Displays percentage of 0 usd tickets
#' @param carrier Groups data per airline
#'
#' For example, all passengers doing the BOS-ATL-LAX path, are summed by Air Carrier.
#'
#' @examples
#' \dontrun{
#' make.Path(OD_2016Q1)
#'
#' # Generate Leg Count
#' make.Path(OD_2016Q1, leg = TRUE)
#' }
#' @export
#'
#'

make.Path <- function(x, leg = FALSE, zero = FALSE, carrier = FALSE){

  print("This code might take longer than usual to execute")

# Gets lasts values by Market and Sequence number
  DT <- as.data.table(x)
  DT <- DT[order(mkt_id, seq_num)]

  if(zero == TRUE){

    DT <- DT[, .(origin=origin[1], dest=dest[.N], itin_fare=itin_fare[1], passengers = passengers[1],
                 roundtrip = roundtrip[1], itin_yield = itin_yield[1], num_stops = .N,
                 pct_zero = ifelse(itin_fare == 0, 1, 0)), by=mkt_id]

    # Calculates averages and sums
    DT <- DT[, itin_fare := itin_fare/(1+roundtrip)]
    netOD <- DT[, .(passengers = sum(passengers),fare_sd = round(sd(itin_fare), 2), itin_fare = round(sum(itin_fare)/(.N), 2),
                    itin_yield = round(sum(itin_yield)/(.N), 3), mean_stops = round(sum(num_stops)/(.N)),
                    pct_zero = round((sum(pct_zero)*100), 2)/(.N)), by=.(origin, dest)][order(origin, dest)]

  }else{

    if(carrier == TRUE){

      DT <- DT[, .(origin=origin[1], dest=dest[.N], itin_fare=itin_fare[1], passengers = passengers[1],
                   roundtrip = roundtrip[1], itin_yield = itin_yield[1], op_carrier = op_carrier[1], num_stops = .N), by=mkt_id]

      # Calculates averages and sums
      DT <- DT[, itin_fare := itin_fare/(1+roundtrip)]
      netOD <- DT[, .(passengers = sum(passengers),fare_sd = round(sd(itin_fare), 2),
                      itin_fare = round(sum(itin_fare)/(.N), 2), itin_yield = round(sum(itin_yield)/(.N), 3),
                      mean_stops = round(sum(num_stops)/(.N))), by=.(origin, dest, op_carrier)][order(origin, dest, op_carrier)]


    }else{

  DT <- DT[, .(origin=origin[1], dest=dest[.N], itin_fare=itin_fare[1], passengers = passengers[1],
               roundtrip = roundtrip[1], itin_yield = itin_yield[1], num_stops = .N), by=mkt_id]

  # Calculates averages and sums
  DT <- DT[, itin_fare := itin_fare/(1+roundtrip)]
  netOD <- DT[, .(passengers = sum(passengers),fare_sd = round(sd(itin_fare), 2),
                  itin_fare = round(sum(itin_fare)/(.N), 2), itin_yield = round(sum(itin_yield)/(.N), 3),
                  mean_stops = round(sum(num_stops)/(.N))), by=.(origin, dest)][order(origin, dest)]

  }

  # Add city name
  netOD <- netOD %>%
    left_join(airportCode, by = "origin") %>%
    rename(origin_city = city, origin_city_mkt_id = city_mkt_id)

  airportCode <- airportCode %>%
    rename(dest = origin, dest_city = city, dest_city_mkt_id = city_mkt_id)

  netOD <- netOD %>%
    left_join(airportCode, by = "dest") %>%
    select(-latitude.x, -latitude.y, -longitude.x, -longitude.y) %>%
    mutate(fare_sd = ifelse(is.na(fare_sd), 0, fare_sd))

  if(leg == FALSE){
    return(netOD)
  }

  if(leg == TRUE){

  DT <- data.table(x)

  DT <- DT[order(mkt_id, seq_num)]
  DT <- DT[,.(passengers = passengers[1], op_carrier,
              path = paste(origin[1], paste(dest, collapse = " "),
                           collapse = " ")), by=mkt_id]

# Filter carriers
  y <- as.numeric(x[1, "year"])
  car <- carriers %>%
    filter(from < y & (to > y | is.na(to))) %>%
    select(op_carrier, carrier_name)

  DT <- DT[,.(passengers = sum(passengers)), by = .(path, op_carrier) ]
  DT <- DT %>%
    left_join(car, by = "op_carrier") %>%
    select(path, op_carrier, carrier_name, passengers) %>%
    arrange(path)


  # Count words
  DT$legCount <- (stringr::str_count(DT$path, "\\S+"))-1

  return(list(netOD = netOD, netLegCount = DT))

  }
  }
}

globalVariables(c("mkt_id", "seq_num", "num_stops", "pct_zero", "latitude.y", "carriers",
                  "description", ".", "carrier_name"))
