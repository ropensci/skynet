#' Path and OD Network
#'
#' Generates an OD network and a Leg Count data frame(on demand)
#'
#' @param x Data frame
#' @param leg Generates Leg Count Data frame, based on Path taken.
#' For example, all passengers doing the BOS-ATL-LAX path, are summed by Air Carrier.
#'
#' @examples
#' make.Path(OD_2016Q1)
#'
#' # Generate Leg Count
#' make.Path(OD_2016Q1, leg = TRUE)
#'
#' @export
#'
#'

make.Path <- function(x, leg = FALSE){

  netOD <<- x %>%
    #data.frame() %>%
    filter(TRIP_BREAK != "") %>%
    group_by(ORIGIN, DEST) %>%
    summarise(weight = sum(PASSENGERS))

  assign("netOD", netOD, .GlobalEnv)

  if(leg == TRUE){

    # Group into different paths (MKT_ID)
  netPath <- x %>%
    #data.frame() %>%
    select(MKT_ID, ORIGIN, DEST, PASSENGERS, SEQ_NUM, OPERATING_CARRIER) %>%
    arrange(MKT_ID, SEQ_NUM)

  # Selects and merges
  # Data.table method

  DT <- data.table(netPath)

  netPath <- DT[,.(PASSENGERS, SEQ_NUM, OPERATING_CARRIER, Path = paste(ORIGIN[1],paste(DEST, collapse = " "),
                                                                        collapse = " ")), by=MKT_ID]

  # Merge everything
  netPath <- netPath %>%
    group_by(Path, OPERATING_CARRIER) %>%
    summarise(Passengers = sum(PASSENGERS)) %>%
    left_join(carriers, by = "OPERATING_CARRIER") %>%
    select(Path, OPERATING_CARRIER, Description, Passengers) %>%
    arrange(Path)


  # Count words
  netPath$legCount <- stringr::str_count(netPath$Path, "\\S+")


  assign("netLegCount",netPath, .GlobalEnv)
}


}
