# netUnd
netUnd <- function(x = netMerged, disp = FALSE, cap = FALSE, merge = TRUE){

  netUnd_all <- x %>%
    select(ORIGIN, DEST, PASSENGERS) %>%
    group_by(ORIGIN, DEST) %>%
    summarise(weight = sum(PASSENGERS))

  value_edges <- netUnd_all %>%
    select(ORIGIN, DEST) %>%
    mutate(ORIGIN_char = as.character(ORIGIN), DEST_char = as.character(DEST))

  value_edges2 <- netUnd_all %>%
    select(DEST, ORIGIN) %>%
    mutate(ORIGIN_char = as.character(ORIGIN), DEST_char = as.character(DEST)) %>%
    select(DEST, ORIGIN, DEST_char, ORIGIN_char) %>%
    rename(ORIGIN = DEST, DEST = ORIGIN, ORIGIN_char = DEST_char, DEST_char = ORIGIN_char)

  gUnd <<- graph_from_data_frame(netUnd_all, directed = TRUE, vertices = nodes)
  gUnd <<- as.undirected(gUnd, mode = "collapse", edge.attr.comb=list(weight = "sum"))

    if(disp == TRUE){

    # Run disparity filter

    # Creates numeric version for dispfilter issue

    #Create lookup table
    value_edges <- netUnd_all %>%
      select(ORIGIN, DEST) %>%
      mutate(ORIGIN_num = as.numeric(ORIGIN), DEST_num = as.numeric(DEST))
    ##distinct()## to remove later

    edges <- netUnd_all %>%
      as.data.frame() %>%
      mutate(ORIGIN = as.numeric(ORIGIN), DEST = as.numeric(DEST))

    nodestemp <- nodes %>%
      mutate(ORIGIN = as.numeric(ORIGIN))

    # ---------------------------------------------------------------------------------- #

    #Creates igraph object
    gUnd_disp <- graph_from_data_frame(edges, directed = TRUE, vertices = nodestemp)
    netUnd_disp <- backbone(gUnd_disp, weights = E(gUnd_disp)$weight, alpha = 0.003)

    #Recode with Factor info
    netUnd_disp <- netUnd_disp %>%
      mutate(ORIGIN = as.numeric(from), DEST = as.numeric(to))

    netUnd_disp <- left_join(netUnd_disp, value_edges,
                             by = c("ORIGIN" = "ORIGIN_num", "DEST" = "DEST_num"))

    netUnd_disp <- netUnd_disp %>%
      select(ORIGIN.y, DEST.y, weight, alpha) %>%
      mutate(ORIGIN = ORIGIN.y, DEST = DEST.y, ORIGIN.y = NULL, DEST.y = NULL) %>%
      select(ORIGIN, DEST, weight, alpha)

    gDir_disp <- graph_from_data_frame(netUnd_disp, directed = TRUE, vertices = nodes)

    gUnd_disp <<- gUnd_disp
    netUnd_disp <<- netUnd_disp



    # ----------------------------------------------------------------------------- #
                           # End of 10% filter command #
    # ----------------------------------------------------------------------------- #


  }else if(cap == TRUE){

    #Run 10% cap
    gUnd_cap <- subgraph.edges(gUnd, which(E(gUnd)$weight > max(E(gUnd)$weight)*.10), delete.vertices = TRUE)

    # Create datafram based on collapsed edges graph
    netUnd_cap <- as_data_frame(gUnd_cap)

    netUnd_cap <- netUnd_cap %>%
      rename(ORIGIN = from, DEST = to)

    netUnd_cap <- netUnd_cap %>%
      left_join(value_edges,
                by = c("ORIGIN" = "ORIGIN_char", "DEST" = "DEST_char")) %>%
      left_join(value_edges2,
                by = c("ORIGIN" = "ORIGIN_char", "DEST" = "DEST_char")) %>%
      mutate(Col1 = if_else(is.na(ORIGIN.y),ORIGIN.y.y, ORIGIN.y),
             Col2 = if_else(is.na(DEST.y),DEST.y.y, DEST.y)) %>%
      select(Col1, Col2, weight) %>%
      rename(ORIGIN = Col1, DEST = Col2)

    netUnd_cap <<- netUnd_cap
    gUnd_cap <<- gUnd_cap

    # ----------------------------------------------------------------------------- #
    # End of 10% filter command #
    # ----------------------------------------------------------------------------- #


  }else if(merge == FALSE){

    # Run undirected with merge
    netUnd_all <- x %>%
      select(ORIGIN, DEST, PASSENGERS) %>%
      group_by(ORIGIN, DEST) %>%
      summarise(weight = sum(PASSENGERS))

    gUnd <<- graph_from_data_frame(netUnd_all, directed = FALSE, vertices = nodes)
    netUnd_all <<- netUnd_all

  }else{

    # Create datafram based on collapsed edges graph
    netUnd_all <- as_data_frame(gUnd)

    netUnd_all <- netUnd_all %>%
      rename(ORIGIN = from, DEST = to)

    netUnd_all <- netUnd_all %>%
      left_join(value_edges,
                by = c("ORIGIN" = "ORIGIN_char", "DEST" = "DEST_char")) %>%
      left_join(value_edges2,
                by = c("ORIGIN" = "ORIGIN_char", "DEST" = "DEST_char")) %>%
      mutate(Col1 = if_else(is.na(ORIGIN.y),ORIGIN.y.y, ORIGIN.y),
             Col2 = if_else(is.na(DEST.y),DEST.y.y, DEST.y)) %>%
      select(Col1, Col2, weight) %>%
      rename(ORIGIN = Col1, DEST = Col2)

    netUnd_all <<- netUnd_all

  }
}


# ----------------------------------------------------------------------------- #
# ----------------------------------------------------------------------------- #
                            # End of netUnd command #
# ----------------------------------------------------------------------------- #
# ----------------------------------------------------------------------------- #

