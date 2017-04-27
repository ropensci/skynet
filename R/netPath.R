#' netPath
#' @export
#'


netPath <- function(x = netMerged){

# Group into different paths (MKT_ID)
netPath <- x %>%
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
              mutate(OPERATING_CARRIER = as.character(OPERATING_CARRIER)) %>%
              left_join(carriers, "OPERATING_CARRIER") %>%
              select(Path, OPERATING_CARRIER, Description, Passengers, legCount) %>%
              arrange(Path)

# Count words
netPath$legCount <- stringr::str_count(netPath$Path, "\\S+")

}

