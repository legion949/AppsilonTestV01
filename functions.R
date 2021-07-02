


# Distance Calculator - WGS84 - Meters Units
DistCalculator <- function(input_coords = NULL, input_round = 4){
  
  # Library
  library("geosphere")
  
  
  # Ships distances between two points
  ship_distance <- rep(NA, nrow(input_coords))
  
  ship_distance[1] <- 0
  
  for (k in 2:length(ship_distance)) {
    
    p01 <- input_coords[k-1, c("LON","LAT")]
    p02 <- input_coords[k, c("LON","LAT")]
    
    
    # Warning!
    # We use:
    #    1) Default RCS:  WGS84
    #    2) Default units: meters
    
    
    # Distance calculator
    the_distance <- distGeo(p01, p02, a=6378137, f=1/298.257223563) 
    
    
    # Save the distances
    ship_distance[k] <- abs(round(the_distance, input_round))
    
    # Remove objets
    remove(p01, p02, the_distance)
    
  }
  
  # Return
  return(ship_distance)
  
  
}




# Change ID errors
ChangeID <- function(data_set = NULL) { 
  
  #rejunte <- data_set[, c("SHIPNAME", "SHIP_ID")]
  fusion <-  unique(paste0(data_set$"SHIPNAME", "z8z",  data_set$"SHIP_ID"))  #### OK!
  metralla <- strsplit(fusion, "z8z") #### OK!
  
  mirada <- unlist(metralla)   #### OK!
  
  id_view <- mirada[c(F,T)]    #### OK!
  name_view <- mirada[c(T,F)]    #### OK!
  
  table_king <- table(id_view, name_view)    #### OK!
  
  dt_id_with_2names <- rowSums(table_king) >= 2     #### OK!
  
  # If really we have problems...
  if (sum(dt_id_with_2names) > 0) {
    
    id_with_2names <- rownames(table_king)[dt_id_with_2names]    #### OK!
    
    mini_id_problems <- table_king[dt_id_with_2names, ]    #### OK!
    
    ##########################################################
    
    # First Case: id with 2 names...
    #             We must change the id of each name...
    
    # Detect names from id_with_2names
    names_from_id_with_2names <- list()
    
    for (k in 1:nrow(mini_id_problems)) {
      
      dt_names <- mini_id_problems[k, ] > 0
      names_from_id_with_2names[[k]] <- colnames(mini_id_problems)[dt_names]
    } # ENd for k
    
    names(names_from_id_with_2names) <- id_with_2names
    
    # Id correction for each name
    # We must change the id of each name...
    for (n in 1:length(names_from_id_with_2names)) for (p in 1:length(names_from_id_with_2names[[n]])) {
      
      the_name <- names_from_id_with_2names[[n]][p]
      
      new_id <- paste0(names(names_from_id_with_2names)[n], " ***V", p, " of ", length(names_from_id_with_2names[[n]]), "***")
      dt_name_version <- data_set$"SHIPNAME" == the_name
      if (sum(dt_name_version) > 0) data_set$"SHIP_ID"[dt_name_version] <- new_id
      
      remove(the_name, new_id, dt_name_version)
      
    } # End double for...
    
  } else names_from_id_with_2names <- list()
  
  
  #Return
  my_exit <- list(data_set, names_from_id_with_2names)
  names(my_exit) <- c("NewData", "id_problems")
  return(my_exit)
  
}

# Change Names errors
ChangeNames <- function(data_set = NULL) { 
  
  
  fusion <-  unique(paste0(data_set$"SHIPNAME", "z8z",  data_set$"SHIP_ID"))  #### OK!
  metralla <- strsplit(fusion, "z8z") #### OK!
  
  mirada <- unlist(metralla)   #### OK!
  
  id_view <- mirada[c(F,T)]    #### OK!
  name_view <- mirada[c(T,F)]    #### OK!
  
  table_king <- table(id_view, name_view)    #### OK!
  
  dt_name_with_2ids <- colSums(table_king) >= 2     #### OK!
  
  # If really have problems with ID...
  if (sum(dt_name_with_2ids)) { 
    
    name_with_2ids <- colnames(table_king)[dt_name_with_2ids]     #### OK!
    
    mini_name_problems <- table_king[,dt_name_with_2ids ]     #### OK!
    
    ##########################################################
    
    # Second Case: a name with 2 or more id
    #             We must change the name of each id...
    
    # Detect id from name_with_2ids
    ids_from_name_with_2ids <- list()
    
    for (k in 1:ncol(mini_name_problems)) {
      
      dt_ids <- mini_name_problems[,k ] > 0
      ids_from_name_with_2ids[[k]] <- rownames(mini_name_problems)[dt_ids]
    } # ENd for k
    
    names(ids_from_name_with_2ids) <- name_with_2ids
    
    # Name correction for each id
    # We must change the name of each id...
    for (n in 1:length(ids_from_name_with_2ids)) for (p in 1:length(ids_from_name_with_2ids[[n]])) {
      
      the_id <- ids_from_name_with_2ids[[n]][p]
      
      new_name <- paste0(names(ids_from_name_with_2ids)[n], " ***V", p, " of ", length(ids_from_name_with_2ids[[n]]), "***")
      dt_id_version <- data_set$"SHIP_ID" == the_id
      if (sum(dt_id_version) > 0) data_set$"SHIPNAME"[dt_id_version] <- new_name
      
      remove(the_id, new_name, dt_id_version)
      
    } # End double for...
    
    
  } else ids_from_name_with_2ids <- list()
  #Return
  my_exit <- list(data_set, ids_from_name_with_2ids)
  names(my_exit) <- c("NewData", "names_problem")
  return(my_exit)
  
}

# Change ID and Names errors (All in One)
ChangeDataSet <- function(data_set = NULL) {
  
  # ID problems correction...
  step01 <- ChangeID(data_set = data_set)
  id_problems <- step01[[2]]
  data1 <- step01$NewData
  
  
  step02 <- ChangeNames(data_set = data1)
  name_problems <- step02[[2]]
  data2 <- step02$NewData
  
  #Return
  my_exit <- list(data2, id_problems, name_problems)
  names(my_exit) <- c("FinalData", "id_problems", "name_problems")
  return(my_exit)
  
  
}