reprow <- function(x, m) {
  x[rep(seq_len(nrow(x)), times=m),,drop=FALSE]
}

check_tolvas<-function(position, Tolvas, n.truck, n.trailer){

  if(sum(Tolvas[,1]==(position-1))>0){
    pos.min <- min(which(Tolvas[,1]==(position-1)))
    pos.max <- max(which(Tolvas[,1]==(position-1)))
    if (Tolvas[pos.min,3] == "truck" ){
      n.truck   <- Tolvas[pos.min,4];
      n.truck   <- as.numeric(n.truck)
    }
    if (Tolvas[pos.max,3] == "truck"){
      n.truck   <- Tolvas[pos.max,4];
      n.truck   <- as.numeric(n.truck)
    }
    if (Tolvas[pos.min,3] == "trailer"){
      n.trailer <- Tolvas[pos.min,4];
      n.trailer <- as.numeric(n.trailer)
    }
    if (Tolvas[pos.max,3] == "trailer"){
      n.trailer <- Tolvas[pos.max,4];
      n.trailer <- as.numeric(n.trailer)
    }
  }

  result <- list()
  result$n.truck   <- n.truck
  result$n.trailer <- n.trailer

  return(result)
}

delete_zeros_tolvas<-function(Tolvas,t){
  for(ii in (t-1):1){
    if (Tolvas[ii,1]==0){
      contador <- 0
      for (jj in ii:(t-1)){
        Tolvas[ii+contador,] <- Tolvas[ii+contador+1,]
        contador <- contador + 1
      }
      Tolvas[t,] <- rep(0,dim(Tolvas)[2])
    }
  }
  t <- min(which(Tolvas[,1]==0))

  results<-list()
  results$Tolvas<-Tolvas
  results$t<-t

  return(results)
}

is_parking<-function(Rhat, Tolvas, input, CWTTRP_struct, pos1, pos2, n.truck, n.trailer, parking, option){

  if (option=="row") {
    rr <- 1
  } else {
    rr <- 3
  }

  if (Rhat[pos1,rr]!=0) {
    if (parking == 1) {
      CWTTRP_struct$parking[CWTTRP_struct$park_index] <- pos1
      CWTTRP_struct$park_index <- CWTTRP_struct$park_index + 1
    }
    pos2 <- pos1-1

    result_sub <- addWorkload_Rhat(Rhat,Tolvas, input,   pos1,
                                   pos2, CWTTRP_struct$CargaT,
                                   n.truck, n.trailer, option)
    CWTTRP_struct$CargaT <- result_sub$CargaT
    pos1 <- result_sub$pos1
    n.truck <- result_sub$n.truck
    n.trailer <- result_sub$n.trailer

  }

  result <- list()
  result$CWTTRP_struct <- CWTTRP_struct
  result$pos1 <- pos1
  result$pos2 <- pos2
  result$n.truck <- n.truck
  result$n.trailer <- n.trailer

  return(result)
}

is_in_parking_list<-function(CWTTRP_struct, position) {
  result <- 0
  for (i in 1:length(CWTTRP_struct$parking_list)) {
      if (position == CWTTRP_struct$parking_list[i]) {
        result <- 1
        break
      }
  }
  return(result)
}


check_in_parking_list<-function(CWTTRP_struct, position, R, input, option) {

  if (option == "left") dir <-1
  else if (option == "right") dir <-3

  subroute <- c(position-1)
  condition <- is_in_parking_list(CWTTRP_struct, position)
  while ((R[position,dir]!=0) && (condition == 0)) {
    position<-R[position,dir]+1
    condition <- is_in_parking_list(CWTTRP_struct, position)
  }


  return(condition)
}

check_in_parking_list_and_tc<-function(CWTTRP_struct, position1, position2,
                                                                  R, input) {
  result <- 1

  condition1 <- exist_tc(input, R, position1, "row")
  condition2 <- check_in_parking_list(CWTTRP_struct, position2, R, input, "right")
  if (condition1 && condition2) {
    result <- 0
  }

  condition2 <- check_in_parking_list(CWTTRP_struct, position1, R, input, "left")
  condition1 <- exist_tc(input, R, position2, "col")
  if (condition1 && condition2) {
    result <- 0
  }

  return(result)
}


exist_tc<-function(input, R, position, option){

  if (option=="row") {
    subroute <- R[position,2]
    while(R[position,1]!=0){
      subroute <- append(subroute, R[position,1])
      position<-R[position,1]+1
    }
  }

  else if (option=="col") {
    subroute <- R[position,2]
    while(R[position,3]!=0){
      subroute <- append(subroute, R[position,3])
      position<-R[position,3]+1
    }
  }

  current_type <- 0
  for (i in 1:length(subroute)) {
    if (subroute[i] > input$n1) {
      current_type <- 1
    }
  }

  return(current_type)

}

only_tc<-function(input, R, position, option){

  if (option=="row") {
    subroute <- R[position,2]
    while(R[position,1]!=0){
      subroute <- append(subroute, R[position,1])
      position<-R[position,1]+1
    }
  }

  else if (option=="col") {
    subroute <- R[position,2]
    while(R[position,3]!=0){
      subroute <- append(subroute, R[position,3])
      position<-R[position,3]+1
    }
  }

  current_type <- 0
  for (i in 1:length(subroute)) {
    if (subroute[i] > input$n1) {
      current_type <- current_type + 1
    }
  }

  result <- 0
  if (current_type == length(subroute)) {
    result <- 1
  }
  return(result)

}

load_manager<-function(CWTTRP_struct, input, vc_load_route1 , vc_load_route2 ,
                       tc_load_route1 , tc_load_route2 , vc_load_subroute1,
                       vc_load_subroute2, tc_load_subroute1, tc_load_subroute2){
  unfeasibility <- 1
  capacity <- 0
  load_truck <- 0

  if ((vc_load_subroute1+vc_load_subroute2+tc_load_subroute1+tc_load_subroute2) == 0) {
    if ((tc_load_route1 > 0)||(tc_load_route2 > 0)) {
      capacity <- input$capacidad.truck
      CWTTRP_struct$CargaT = vc_load_route1 + vc_load_route2 + tc_load_route1 + tc_load_route2
      load_truck <- tc_load_route1 + tc_load_route2
      unfeasibility <- CWTTRP_struct$CargaT - capacity
    } else {
      capacity <- input$capacidad.vehiculo
      CWTTRP_struct$CargaT = vc_load_route1 + vc_load_route2
      unfeasibility <- CWTTRP_struct$CargaT - capacity
    }
  }
  else {
    capacity <- input$capacidad.vehiculo
    CWTTRP_struct$CargaT <- vc_load_route1 + vc_load_route2 + tc_load_route1 + tc_load_route2 +
      vc_load_subroute1 + vc_load_subroute2 + tc_load_subroute1 + tc_load_subroute2
    unfeasibility <- CWTTRP_struct$CargaT - capacity
    load_truck <- tc_load_route1 + tc_load_route2 + vc_load_subroute1 + vc_load_subroute2 +
      tc_load_subroute1 + tc_load_subroute2
    if (load_truck > input$capacidad.truck) unfeasibility <- 1
  }

  result <- list()
  result$unfeasibility <- unfeasibility
  result$capacity <- capacity
  result$load_truck <- load_truck
  result$CWTTRP_struct <- CWTTRP_struct

  return(result)
}


load_manager_create_subroute<-function(CWTTRP_struct, input, vc_load_route1 ,
                                       vc_load_route2 , tc_load_route1 , tc_load_route2,
                                       option){
  unfeasibility <- 0
  capacity <- 0
  load_truck <- 0

  subroute_load <- 0
  main_route_load <- 0

  if (option == "vctc"){
    subroute_load <- vc_load_route2 + tc_load_route2
    main_route_load <- vc_load_route1 + tc_load_route1
  }
  else if (option == "tcvc") {
    subroute_load <- vc_load_route1 + tc_load_route1
    main_route_load <- vc_load_route2 + tc_load_route2
  }

  load_truck <- tc_load_route2 + tc_load_route1
  if (subroute_load > input$capacidad.truck ) unfeasibility <- 1
  if ((main_route_load+subroute_load) > input$capacidad.vehiculo ) unfeasibility <- 1
  if (load_truck > input$capacidad.truck ) unfeasibility <- 1

  capacity <- input$capacidad.truck
  CWTTRP_struct$CargaT <- vc_load_route1 + tc_load_route1 + vc_load_route2 + tc_load_route2

  result <- list()
  result$unfeasibility <- unfeasibility
  result$capacity <- capacity
  result$load_truck <- load_truck
  result$CWTTRP_struct <- CWTTRP_struct

  return(result)
}

insert_hoppers_MCTTRP_PTR<-function(solution, old_solution, input) {
  route <- solution$route
  
  n_hoppers_truck <- length(input$H.camion[1,])
  
  cap_hoppers_truck <- input$H.camion[1,1]
  
  clients_tc <- list()
  clients_vc <- list()
  counter_clients_tc <- 0
  counter_clients_vc <- 0
  counter_hoppers_truck <- 0
  for (i in 2:(length(route)-1)) {
    i_client <- route[i]
    if (i_client<=input$n1) {
      counter_clients_vc <- counter_clients_vc + 1
      clients_vc[[counter_clients_vc]] <- list()
      clients_vc[[counter_clients_vc]]$id <- i_client
      clients_vc[[counter_clients_vc]]$demands <- input$matriz.demandas[i_client+1,]
      clients_vc[[counter_clients_vc]]$hoppers_trucks <- list()
      clients_vc[[counter_clients_vc]]$hoppers_trailers <- list()
      i_type <- "vc"
    } else {
      counter_clients_tc <- counter_clients_tc + 1
      clients_tc[[counter_clients_tc]] <- list()
      clients_tc[[counter_clients_tc]]$id <- i_client
      clients_tc[[counter_clients_tc]]$demands <- input$matriz.demandas[i_client+1,]
      clients_tc[[counter_clients_tc]]$hoppers_trucks <- list()
      clients_tc[[counter_clients_tc]]$hoppers_trailers <- list()
      i_type <- "tc"
    }
    
    for (j in 1:length(input$matriz.demandas[i_client+1,])){
      demand <- input$matriz.demandas[i_client+1,j]
      while (demand > 0) {
        save_demand <- demand
        demand <- demand - cap_hoppers_truck
        if (demand > 0) quantity <- cap_hoppers_truck
        else quantity <- save_demand
        if (i_type == "tc") {
          size_c <- length(clients_tc[[counter_clients_tc]]$hoppers_trucks) + 1
          clients_tc[[counter_clients_tc]]$hoppers_trucks[[size_c]] <- c(j, quantity, quantity/cap_hoppers_truck)
        }
        else {
          size_c <- length(clients_vc[[counter_clients_vc]]$hoppers_trucks) + 1
          clients_vc[[counter_clients_vc]]$hoppers_trucks[[size_c]] <- c(j, quantity, quantity/cap_hoppers_truck)
        }
        counter_hoppers_truck <- counter_hoppers_truck + 1
      }
    }
  }
  
  result_h <- list()
  result_h$clients_tc <- clients_tc
  result_h$clients_vc <- clients_vc
  result_h$used_hoppers_truck <- counter_hoppers_truck
  result_h$used_hoppers_trailer <- 0
  
  return(result_h)
  
}


insert_hoppers_MCTTRP_CVR_new<-function(solution, old_route, input) {
  
  route_i <- solution$route
  n_hoppers_truck <- length(input$H.camion[1,])
  n_hoppers_trailer <- length(input$H.trailer[1,])
  
  cap_hoppers_truck <- input$H.camion[1,1]
  cap_hoppers_trailer <- input$H.trailer[1,1]
  
  subroutes <- return_subroutes(route_i, input$n1)
  main_route <- return_main_route(route_i)
  
  clients_tc <- list()
  clients_vc <- list()
  counter_clients_tc <- 0
  counter_clients_vc <- 0
  counter_hoppers_truck <- 0
  for (i in 1:(length(subroutes))) {
    for (j in 2:(length(subroutes[[i]]$tour)-1)) {
        j_client <- subroutes[[i]]$tour[j]
        if (j_client<=input$n1) {
          counter_clients_vc <- counter_clients_vc + 1
          clients_vc[[counter_clients_vc]] <- list()
          clients_vc[[counter_clients_vc]]$id <- j_client
          clients_vc[[counter_clients_vc]]$demands <- input$matriz.demandas[j_client+1,]
          clients_vc[[counter_clients_vc]]$hoppers_trucks <- list()
          clients_vc[[counter_clients_vc]]$hoppers_trailers <- list()
          i_type <- "vc"
        } 
        else {
          counter_clients_tc <- counter_clients_tc + 1
          clients_tc[[counter_clients_tc]] <- list()
          clients_tc[[counter_clients_tc]]$demands <- input$matriz.demandas[j_client+1,]
          clients_tc[[counter_clients_tc]]$id <- j_client
          clients_tc[[counter_clients_tc]]$hoppers_trucks <- list()
          clients_tc[[counter_clients_tc]]$hoppers_trailers <- list()
          i_type <- "tc"
        }
        
        for (j in 1:length(input$matriz.demandas[j_client+1,])){
          demand <- input$matriz.demandas[j_client+1,j]
          while (demand > 0) {
            save_demand <- demand
            demand <- demand - cap_hoppers_truck
            if (demand > 0) quantity <- cap_hoppers_truck
            else quantity <- save_demand
            if (i_type == "tc") {
              size_c <- length(clients_tc[[counter_clients_tc]]$hoppers_trucks) + 1
              clients_tc[[counter_clients_tc]]$hoppers_trucks[[size_c]] <- c(j, quantity, quantity/cap_hoppers_truck)
            }
            else {
              size_c <- length(clients_vc[[counter_clients_vc]]$hoppers_trucks) + 1
              clients_vc[[counter_clients_vc]]$hoppers_trucks[[size_c]] <- c(j, quantity, quantity/cap_hoppers_truck)
            }
            counter_hoppers_truck <- counter_hoppers_truck + 1
          }
        }
        
    }
  }
  
  counter_hoppers_trailer <- 0
  for (i in 2:(length(main_route)-1)) {
    i_client <- main_route[i]
    if (i_client<=input$n1) {
      counter_clients_vc <- counter_clients_vc + 1
      clients_vc[[counter_clients_vc]] <- list()
      clients_vc[[counter_clients_vc]]$id <- i_client
      clients_vc[[counter_clients_vc]]$demands <- input$matriz.demandas[i_client+1,]
      clients_vc[[counter_clients_vc]]$hoppers_trucks <- list()
      clients_vc[[counter_clients_vc]]$hoppers_trailers <- list()
      i_type <- "vc"
    } else {
      counter_clients_tc <- counter_clients_tc + 1
      clients_tc[[counter_clients_tc]] <- list()
      clients_tc[[counter_clients_tc]]$id <- i_client
      clients_tc[[counter_clients_tc]]$demands <- input$matriz.demandas[i_client+1,]
      clients_tc[[counter_clients_tc]]$hoppers_trucks <- list()
      clients_tc[[counter_clients_tc]]$hoppers_trailers <- list()
      i_type <- "tc"
    }
    
    for (j in 1:length(input$matriz.demandas[i_client+1,])){
      demand <- input$matriz.demandas[i_client+1,j]
      #trailers
      while ((demand > 0) && (counter_hoppers_trailer < n_hoppers_trailer)){
        save_demand <- demand
        demand <- demand - cap_hoppers_trailer
        if (demand > 0) quantity <- cap_hoppers_trailer
        else quantity <- save_demand
        if (i_type == "tc") {
          size_c <- length(clients_tc[[counter_clients_tc]]$hoppers_trailers) + 1
          clients_tc[[counter_clients_tc]]$hoppers_trailers[[size_c]] <- c(j, quantity, quantity/cap_hoppers_trailer)
        }
        else {
          size_c <- length(clients_vc[[counter_clients_vc]]$hoppers_trailers) + 1
          clients_vc[[counter_clients_vc]]$hoppers_trailers[[size_c]] <- c(j, quantity, quantity/cap_hoppers_trailer)
        }
        counter_hoppers_trailer <- counter_hoppers_trailer + 1
      }
      # trucks
      while ((demand > 0)) {
        save_demand <- demand
        demand <- demand - cap_hoppers_truck
        if (demand > 0) quantity <- cap_hoppers_truck
        else quantity <- save_demand
        if (i_type == "tc") {
          size_c <- length(clients_tc[[counter_clients_tc]]$hoppers_trucks) + 1
          clients_tc[[counter_clients_tc]]$hoppers_trucks[[size_c]] <- c(j, quantity, quantity/cap_hoppers_truck)
        }
        else {
          size_c <- length(clients_vc[[counter_clients_vc]]$hoppers_trucks) + 1
          clients_vc[[counter_clients_vc]]$hoppers_trucks[[size_c]] <- c(j, quantity, quantity/cap_hoppers_truck)
        }
        counter_hoppers_truck <- counter_hoppers_truck + 1
      }
    }
  }
  
  result_h <- list()
  result_h$clients_tc <- clients_tc
  result_h$clients_vc <- clients_vc
  result_h$used_hoppers_truck <- counter_hoppers_truck
  result_h$used_hoppers_trailer <- counter_hoppers_trailer
  
  return(result_h)
  
}


insert_hoppers_MCTTRP_PVR_new<-function(solution, old_route, input) {
  route <- solution$route
  
  n_hoppers_truck <- length(input$H.camion[1,])
  n_hoppers_trailer <- length(input$H.trailer[1,])
  
  cap_hoppers_truck <- input$H.camion[1,1]
  cap_hoppers_trailer <- input$H.trailer[1,1]
  
  clients_tc <- list()
  clients_vc <- list()
  counter_clients_tc <- 0
  counter_clients_vc <- 0
  counter_hoppers_truck <- 0
  counter_hoppers_trailer <- 0
  for (i in 2:(length(route)-1)) {
    i_client <- route[i]
    if (i_client<=input$n1) {
      counter_clients_vc <- counter_clients_vc + 1
      clients_vc[[counter_clients_vc]] <- list()
      clients_vc[[counter_clients_vc]]$id <- i_client
      clients_vc[[counter_clients_vc]]$demands <- input$matriz.demandas[i_client+1,]
      clients_vc[[counter_clients_vc]]$hoppers_trucks <- list()
      clients_vc[[counter_clients_vc]]$hoppers_trailers <- list()
      i_type <- "vc"
    } else {
      counter_clients_tc <- counter_clients_tc + 1
      clients_tc[[counter_clients_tc]] <- list()
      clients_tc[[counter_clients_tc]]$id <- i_client
      clients_tc[[counter_clients_tc]]$demands <- input$matriz.demandas[i_client+1,]
      clients_tc[[counter_clients_tc]]$hoppers_trucks <- list()
      clients_tc[[counter_clients_tc]]$hoppers_trailers <- list()
      i_type <- "tc"
    }
    
    for (j in 1:length(input$matriz.demandas[i_client+1,])){
      demand <- input$matriz.demandas[i_client+1,j]
      #trailers
      while ((demand > 0) && (counter_hoppers_trailer < n_hoppers_trailer)){
        save_demand <- demand
        demand <- demand - cap_hoppers_trailer
        if (demand > 0) quantity <- cap_hoppers_trailer
        else quantity <- save_demand
        if (i_type == "tc") {
          size_c <- length(clients_tc[[counter_clients_tc]]$hoppers_trailers) + 1
          clients_tc[[counter_clients_tc]]$hoppers_trailers[[size_c]] <- c(j, quantity, quantity/cap_hoppers_trailer)
        }
        else {
          size_c <- length(clients_vc[[counter_clients_vc]]$hoppers_trailers) + 1
          clients_vc[[counter_clients_vc]]$hoppers_trailers[[size_c]] <- c(j, quantity, quantity/cap_hoppers_trailer)
        }
        counter_hoppers_trailer <- counter_hoppers_trailer + 1
      }
      # trucks
      while ((demand > 0)) {
        save_demand <- demand
        demand <- demand - cap_hoppers_truck
        if (demand > 0) quantity <- cap_hoppers_truck
        else quantity <- save_demand
        if (i_type == "tc") {
          size_c <- length(clients_tc[[counter_clients_tc]]$hoppers_trucks) + 1
          clients_tc[[counter_clients_tc]]$hoppers_trucks[[size_c]] <- c(j, quantity, quantity/cap_hoppers_truck)
        }
        else {
          size_c <- length(clients_vc[[counter_clients_vc]]$hoppers_trucks) + 1
          clients_vc[[counter_clients_vc]]$hoppers_trucks[[size_c]] <- c(j, quantity, quantity/cap_hoppers_truck)
        }
        counter_hoppers_truck <- counter_hoppers_truck + 1
      }
    }
  }
  
  result_h <- list()
  result_h$clients_tc <- clients_tc
  result_h$clients_vc <- clients_vc
  result_h$used_hoppers_truck <- counter_hoppers_truck
  result_h$used_hoppers_trailer <- counter_hoppers_trailer
  
  return(result_h)
  
}


insert_hoppers_MCTTRP_CVR_update<-function(route_i, old_solution, input) {

  
  n_hoppers_truck <- length(input$H.camion[1,])
  n_hoppers_trailer <- length(input$H.trailer[1,])

  cap_hoppers_truck <- input$H.camion[1,1]
  cap_hoppers_trailer <- input$H.trailer[1,1]
  
  old_route <- old_solution$route
  
  
  clients_to_add <- setdiff(route_i, old_solution$route)
  clients_to_delete <- setdiff(old_solution$route, route_i)

  counter_clients_tc <- length(old_solution$clients_tc)
  counter_clients_vc <- length(old_solution$clients_vc)
  counter_hoppers_truck <- old_solution$used_hoppers_truck
  counter_hoppers_trailer <- old_solution$used_hoppers_trailer

  
  if (length(clients_to_delete)) {
    #print("if (length(clients_to_delete))")
    clients_vc <- list()
    clients_tc <- list()
    
    counter_hoppers_truck <- 0
    counter_hoppers_trailer <- 0
    counter_clients_tc <- 1
    counter_clients_vc <- 1
    #print("1if (length(clients_to_delete))")
    
    if (length(old_solution$clients_vc)) {
      for (i in 1:length(old_solution$clients_vc)) {
        if ( sum(old_solution$clients_vc[[i]]$id ==  clients_to_delete) == 0) {
          clients_vc[[counter_clients_vc]] <-  old_solution$clients_vc[[i]]
          counter_hoppers_truck <-   counter_hoppers_truck + length(old_solution$clients_vc[[i]]$hoppers_trucks)
          counter_hoppers_trailer <- counter_hoppers_trailer + length(old_solution$clients_vc[[i]]$hoppers_trailers)
          counter_clients_vc <- counter_clients_vc + 1
        }
      }
    }
    #print("2if (length(clients_to_delete))")
    
    if (length(old_solution$clients_tc)) {
      for (i in 1:length(old_solution$clients_tc)) {
        if ( sum(old_solution$clients_tc[[i]]$id ==  clients_to_delete) == 0) {
          clients_tc[[counter_clients_tc]] <-  old_solution$clients_tc[[i]]
          counter_hoppers_truck <-   counter_hoppers_truck + length(old_solution$clients_tc[[i]]$hoppers_trucks)
          counter_clients_tc <- counter_clients_tc + 1
        }
      }
    }
    counter_clients_tc <- counter_clients_tc - 1
    counter_clients_vc <- counter_clients_vc - 1
    
    old_solution$clients_tc <- clients_tc
    old_solution$clients_vc <- clients_vc
    old_solution$used_hoppers_truck <- counter_hoppers_truck
    old_solution$used_hoppers_trailer <- counter_hoppers_trailer
    
  }
  
  if (length(clients_to_add)) {
    #print("if (length(clients_to_add))")
    
    counter_hoppers_truck <- old_solution$used_hoppers_truck
    counter_hoppers_trailer <- old_solution$used_hoppers_trailer
    
    clients_tc <- old_solution$clients_tc
    clients_vc <- old_solution$clients_vc
    
    logic_clients_to_add <- position_to_add_in_truck( clients_to_add , route_i, input)
    #print("1if (length(clients_to_add))")
    
    for (i in 1:length(clients_to_add)) {
      i_client <- clients_to_add[i]
      
      if (i_client<=input$n1) {
        counter_clients_vc <- counter_clients_vc + 1
        clients_vc[[counter_clients_vc]] <- list()
        clients_vc[[counter_clients_vc]]$id <- i_client
        clients_vc[[counter_clients_vc]]$demands <- input$matriz.demandas[i_client+1,]
        clients_vc[[counter_clients_vc]]$hoppers_trucks <- list()
        clients_vc[[counter_clients_vc]]$hoppers_trailers <- list()
        i_type <- "vc"
      } else {
        counter_clients_tc <- counter_clients_tc + 1
        clients_tc[[counter_clients_tc]] <- list()
        clients_tc[[counter_clients_tc]]$id <- i_client
        clients_tc[[counter_clients_tc]]$demands <- input$matriz.demandas[i_client+1,]
        clients_tc[[counter_clients_tc]]$hoppers_trucks <- list()
        clients_tc[[counter_clients_tc]]$hoppers_trailers <- list()
        i_type <- "tc"
      }
      
      # add only to truck
      if (logic_clients_to_add[[i]] == 1) {
        for (j in 1:length(input$matriz.demandas[i_client+1,])){
          demand <- input$matriz.demandas[i_client+1,j]
          # trailer
          while ((demand > 0)) {
            save_demand <- demand
            demand <- demand - cap_hoppers_truck
            if (demand > 0) quantity <- cap_hoppers_truck
            else quantity <- save_demand
            if (i_type == "tc") {
              size_c <- length(clients_tc[[counter_clients_tc]]$hoppers_trucks) + 1
              clients_tc[[counter_clients_tc]]$hoppers_trucks[[size_c]] <- c(j, quantity, quantity/cap_hoppers_truck)
            }
            else {
              size_c <- length(clients_vc[[counter_clients_vc]]$hoppers_trucks) + 1
              clients_vc[[counter_clients_vc]]$hoppers_trucks[[size_c]] <- c(j, quantity, quantity/cap_hoppers_truck)
            }
            counter_hoppers_truck <- counter_hoppers_truck + 1
          }
        }
      }
      
      # add both
      else {
        for (j in 1:length(input$matriz.demandas[i_client+1,])){
            demand <- input$matriz.demandas[i_client+1,j]
            #trailers
            while ((demand > 0) && (counter_hoppers_trailer < n_hoppers_trailer)){
              save_demand <- demand
              demand <- demand - cap_hoppers_trailer
              if (demand > 0) quantity <- cap_hoppers_trailer
              else quantity <- save_demand
              if (i_type == "tc") {
                size_c <- length(clients_tc[[counter_clients_tc]]$hoppers_trailers) + 1
                clients_tc[[counter_clients_tc]]$hoppers_trailers[[size_c]] <- c(j, quantity, quantity/cap_hoppers_trailer)
              }
              else {
                size_c <- length(clients_vc[[counter_clients_vc]]$hoppers_trailers) + 1
                clients_vc[[counter_clients_vc]]$hoppers_trailers[[size_c]] <- c(j, quantity, quantity/cap_hoppers_trailer)
              }
              counter_hoppers_trailer <- counter_hoppers_trailer + 1
            }
            # trucks
            while ((demand > 0)) {
              save_demand <- demand
              demand <- demand - cap_hoppers_truck
              if (demand > 0) quantity <- cap_hoppers_truck
              else quantity <- save_demand
              if (i_type == "tc") {
                size_c <- length(clients_tc[[counter_clients_tc]]$hoppers_trucks) + 1
                clients_tc[[counter_clients_tc]]$hoppers_trucks[[size_c]] <- c(j, quantity, quantity/cap_hoppers_truck)
              }
              else {
                size_c <- length(clients_vc[[counter_clients_vc]]$hoppers_trucks) + 1
                clients_vc[[counter_clients_vc]]$hoppers_trucks[[size_c]] <- c(j, quantity, quantity/cap_hoppers_truck)
              }
              counter_hoppers_truck <- counter_hoppers_truck + 1
            }
        }
        
      }
      
    } 
    #print("2if (length(clients_to_add))")
    
  }
  
  result_h <- list()
  #print("1result_h")
  result_h$clients_tc <- clients_tc
  result_h$clients_vc <- clients_vc
  result_h$used_hoppers_truck <- counter_hoppers_truck
  result_h$used_hoppers_trailer <- counter_hoppers_trailer
  #print("2result_h")
  
  return(result_h)
}


insert_hoppers_MCTTRP_PVR_update<-function(route_i, old_solution, input) {
  n_hoppers_truck <- length(input$H.camion[1,])
  n_hoppers_trailer <- length(input$H.trailer[1,])
  
  cap_hoppers_truck <- input$H.camion[1,1]
  cap_hoppers_trailer <- input$H.trailer[1,1]
  
  old_route <- old_solution$route
  
  clients_to_add <- setdiff(route_i, old_solution$route)
  clients_to_delete <- setdiff(old_solution$route, route_i)
  
  counter_clients_vc <- length(old_solution$clients_vc)
  counter_hoppers_truck <- old_solution$used_hoppers_truck
  counter_hoppers_trailer <- old_solution$used_hoppers_trailer
  

  if (length(clients_to_delete)) {

    clients_vc <- list()

    counter_hoppers_truck <- 0
    counter_hoppers_trailer <- 0
    counter_clients_vc <- 1
    
    for (i in 1:length(old_solution$clients_vc)) {
      if ( sum(old_solution$clients_vc[[i]]$id ==  clients_to_delete) == 0) {
        clients_vc[[counter_clients_vc]] <-  old_solution$clients_vc[[i]]
        counter_clients_vc <- counter_clients_vc + 1
        counter_hoppers_truck <-   counter_hoppers_truck + length(old_solution$clients_vc[[i]]$hoppers_trucks)
        counter_hoppers_trailer <- counter_hoppers_trailer + length(old_solution$clients_vc[[i]]$hoppers_trailers)
      }
    }
    
    counter_clients_vc <- counter_clients_vc - 1 
    old_solution$clients_vc <- clients_vc
    old_solution$used_hoppers_truck <- counter_hoppers_truck
    old_solution$used_hoppers_trailer <- counter_hoppers_trailer
  }
  
  if (length(clients_to_add)) {
      
      counter_hoppers_truck <- old_solution$used_hoppers_truck
      counter_hoppers_trailer <- old_solution$used_hoppers_trailer
      
      clients_vc <- old_solution$clients_vc#list()
      
      
      for (i in 1:length(clients_to_add)) {
        i_client <- clients_to_add[i]
        
        if (i_client<=input$n1) {
          counter_clients_vc <- counter_clients_vc + 1
          clients_vc[[counter_clients_vc]] <- list()
          clients_vc[[counter_clients_vc]]$id <- i_client
          clients_vc[[counter_clients_vc]]$demands <- input$matriz.demandas[i_client+1,]
          clients_vc[[counter_clients_vc]]$hoppers_trucks <- list()
          clients_vc[[counter_clients_vc]]$hoppers_trailers <- list()
          i_type <- "vc"
        } 
        
        for (j in 1:length(input$matriz.demandas[i_client+1,])){
          demand <- input$matriz.demandas[i_client+1,j]
          #trailers
          while ((demand > 0) && (counter_hoppers_trailer < n_hoppers_trailer)){
            save_demand <- demand
            demand <- demand - cap_hoppers_trailer
            if (demand > 0) quantity <- cap_hoppers_trailer
            else quantity <- save_demand
            if (i_type == "vc") {
              size_c <- length(clients_vc[[counter_clients_vc]]$hoppers_trailers) + 1
              clients_vc[[counter_clients_vc]]$hoppers_trailers[[size_c]] <- c(j, quantity, quantity/cap_hoppers_trailer)
            }
            counter_hoppers_trailer <- counter_hoppers_trailer + 1
          }
          # trucks
          while ((demand > 0)) {
            save_demand <- demand
            demand <- demand - cap_hoppers_truck
            if (demand > 0) quantity <- cap_hoppers_truck
            else quantity <- save_demand
            if (i_type == "vc") {
              size_c <- length(clients_vc[[counter_clients_vc]]$hoppers_trucks) + 1
              clients_vc[[counter_clients_vc]]$hoppers_trucks[[size_c]] <- c(j, quantity, quantity/cap_hoppers_truck)
            }
            counter_hoppers_truck <- counter_hoppers_truck + 1
          }
        }
        }   
  }
  
  result_h <- list()
  result_h$clients_tc <- list()
  result_h$clients_vc <- clients_vc
  result_h$used_hoppers_truck <- counter_hoppers_truck
  result_h$used_hoppers_trailer <- counter_hoppers_trailer
  
  return(result_h)
  
}



position_to_add_in_truck<-function(clients, route, input) {
  
  subroutes <- return_subroutes(route, input$n1)
  logic_vector <- list()
  for (i in 1:length(clients)) {
    is_sub <- 0
    for (j in 1:length(subroutes)) {
      if ((sum(subroutes[[j]]$tour[2:(length(subroutes[[j]]$tour)-1)] == clients[i]))) {
        is_sub <- 1
        break
      }
    }
    logic_vector[[i]] <- is_sub
  }
  
  return(logic_vector)  
}


return_id_route<-function(route, result_res) {
  index <- 0
  for (i in 1:length(result_res)) {
    if (length(result_res[[i]]$route)==length(route)) {
      if (all(result_res[[i]]$route == route)) {
        index <- i
        break
      }
    }
  }
  
  return(index)
}



