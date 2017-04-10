# netDir
###don't forget to add the filtering option###

netDir <- function(x = netMerged, disp = FALSE, cap = FALSE){
  netDir_all <- x %>%
    select(ORIGIN, DEST, PASSENGERS) %>%
    group_by(ORIGIN, DEST) %>%
    summarise(weight = sum(PASSENGERS))

  gDir <- graph_from_data_frame(netDir_all, directed = TRUE, vertices = nodes)

  if(disp == TRUE){
    # Run disparity filter

    # Create lookup table
    value_edges <- netDir_all %>%
      select(ORIGIN, DEST) %>%
      mutate(ORIGIN_char = as.character(ORIGIN), DEST_char = as.character(DEST))

    # Create igraph
    gDir_disp <<- semnet::getBackboneNetwork(gDir, delete.isolates = F, alpha = 0.003)
    netDir_disp <<- get.data.frame(gDir_disp)

    # Recode with Factor info
    netDir_disp <- netDir_disp %>%
      rename(ORIGIN = from, DEST = to)

    netDir_disp <- left_join(netDir_disp, value_edges,
                             by = c("ORIGIN" = "ORIGIN_char", "DEST" = "DEST_char"))

    netDir_disp <- netDir_disp %>%
      select(ORIGIN.y, DEST.y, weight, alpha) %>%
      mutate(ORIGIN = ORIGIN.y, DEST = DEST.y, ORIGIN.y = NULL, DEST.y = NULL) %>%
      select(ORIGIN, DEST, weight, alpha)

    gDir_disp <<- gDir_disp
    netDir_disp <<- netDir_disp

    # ----------------------------------------------------------------------------- #
                          # End of disp filter command #
    # ----------------------------------------------------------------------------- #

  }else if(cap == TRUE){

    # Applies 10% cap
    gDir_cap <- graph_from_data_frame(netDir_all, directed = TRUE, vertices = nodes)
    gDir_cap <- subgraph.edges(gDir_cap, which(E(gDir_cap)$weight > max(E(gDir_cap)$weight)*.10), delete.vertices = TRUE)
    ###netDir_gr <- delete_vertices(netDir_gr, degree(netDir_gr, mode = "in")==0)

    #Create lookup table
    value_edges <- netDir_all %>%
      select(ORIGIN, DEST) %>%
      mutate(ORIGIN_char = as.character(ORIGIN), DEST_char = as.character(DEST))

    #Creates Dataframe from graph
    netDir_cap <- igraph::as_data_frame(gDir_cap)

    netDir_cap <- netDir_cap %>%
      rename(ORIGIN = from, DEST = to)

    netDir_cap <- netDir_cap %>%
      left_join(value_edges,
                by = c("ORIGIN" = "ORIGIN_char", "DEST" = "DEST_char")) %>%
      select(ORIGIN.y, DEST.y, weight) %>%
      rename(ORIGIN = ORIGIN.y, DEST = DEST.y, PASSENGERS = weight)

    gDir_cap <<- gDir_cap
    netDir_cap <<- netDir_cap

    # ----------------------------------------------------------------------------- #
                           # End of 10% filter command #
    # ----------------------------------------------------------------------------- #


  }else{

    # Runs network with full data
    gDir <- graph_from_data_frame(netDir_all, directed = TRUE, vertices = nodes)

    gDir <<- gDir
  }
  netDir_all <<- netDir_all

}

# ----------------------------------------------------------------------------- #
# ----------------------------------------------------------------------------- #
                            # End of netDir command #
# ----------------------------------------------------------------------------- #
# ----------------------------------------------------------------------------- #
