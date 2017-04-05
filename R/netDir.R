# netDir
###don't forget to add the filtering option###

netDir <- function(x = netMerged, disp = FALSE, cap = FALSE){
  netDir_all <- x %>%
    select(ORIGIN, DEST, PASSENGERS) %>%
    group_by(ORIGIN, DEST) %>%
    summarise(weight = sum(PASSENGERS))


  if(disp == TRUE){
    # Applies disparity filter
    # Creates numeric version for dispfilter issue

    #Create lookup table
    value_edges <- netDir_all %>%
      select(ORIGIN, DEST) %>%
      mutate(ORIGIN_num = as.numeric(ORIGIN), DEST_num = as.numeric(DEST))
    ##distinct()## to remove later

    edges <- netDir_all %>%
      as.data.frame() %>%
      mutate(ORIGIN = as.numeric(ORIGIN), DEST = as.numeric(DEST))

    nodestemp <- nodes %>%
      mutate(ORIGIN = as.numeric(ORIGIN))

    # ---------------------------------------------------------------------------------- #

    #Creates igraph object
    gDir_disp <- graph_from_data_frame(edges, directed = TRUE, vertices = nodestemp)
    netDir_temp <- backbone(gDir_disp, weights = E(gDir_disp)$weight, alpha = 0.003)

    #Recode with Factor info
    netDir_disp <- netDir_temp %>%
      mutate(ORIGIN = as.numeric(from), DEST = as.numeric(to))

    netDir_disp <- left_join(netDir_disp, value_edges,
                             by = c("ORIGIN" = "ORIGIN_num", "DEST" = "DEST_num"))

    netDir_disp <- netDir_disp %>%
      select(ORIGIN.y, DEST.y, weight, alpha) %>%
      mutate(ORIGIN = ORIGIN.y, DEST = DEST.y, ORIGIN.y = NULL, DEST.y = NULL) %>%
      select(ORIGIN, DEST, weight, alpha)

    gDir_disp <- graph_from_data_frame(netDir_disp, directed = TRUE, vertices = nodes)

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
