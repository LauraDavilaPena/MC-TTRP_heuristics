calc_penalty<-function(input, routes_res, type_problem) {
  
  exc <- 0
  if (type_problem == "TTRP") {
      # CARGA TOTAL
      for (i in 1:length(routes_res)) {
        if (routes_res[[i]]$type != "PTR") {
          exc <- exc +  max(0, calc_load2(routes_res[[i]]$route, input$vector.demandas) - input$capacidad.vehiculo)
        }
        else {
          exc <- exc + max(0, calc_load2(routes_res[[i]]$route, input$vector.demandas) - input$capacidad.truck)
        }
      }
      
      for (i in 1:length(routes_res)) {
        if ((sum(duplicated(routes_res[[i]]$route[2:(length(routes_res[[i]]$route)-1)])))) {
          subroutes <- return_subroutes(routes_res[[i]]$route, input$n1)
          for (s in 1:length(subroutes)) {
            subroute_i <- subroutes[[s]]$tour[2:(length(subroutes[[s]]$tour)-1)]
            exc <- exc + max(0, calc_load2(subroute_i, input$vector.demandas) - input$capacidad.truck)
          }
        }
      }
  }
  
  if (type_problem == "MCTTRP") {
    # CARGA TOTAL
    #for (i in 1:length(routes_res)) {
    #  if (routes_res[[i]]$type != "PTR") {
    #    exc <- exc +  max(0, calc_load2_MC(routes_res[[i]]$route, input$matriz.demandas) - input$capacidad.vehiculo[1])
    #  }
    #  else {
    #    exc <- exc + max(0, calc_load2_MC(routes_res[[i]]$route, input$matriz.demandas) - input$capacidad.truck[1])
    #  }
    #}
    # CARGA EN SUBRUTAS
    #for (i in 1:length(routes_res)) {
    #  if ((sum(duplicated(routes_res[[i]]$route[2:(length(routes_res[[i]]$route)-1)])))) {
    #    subroutes <- return_subroutes(routes_res[[i]]$route, input$n1)
    #    subroute_load <- 0
    #    for (s in 1:length(subroutes)) {
    #      subroute_i <- subroutes[[s]]$tour[2:(length(subroutes[[s]]$tour)-1)]
    #      subroute_load <- subroute_load + calc_load2_MC(subroute_i, input$matriz.demandas)
    #    }
    #    exc <- exc + max(0, subroute_load - input$capacidad.truck[1])
    #  }
    #}
    # CARGA EN TOLVAS
    n_hoppers_truck <- length(input$H.camion[1,])
    n_hoppers_trailer <- length(input$H.trailer[1,])

    for (i in 1:length(routes_res)) {
      exc <- exc + max(0, routes_res[[i]]$used_hoppers_truck   - n_hoppers_truck)
      exc <- exc + max(0, routes_res[[i]]$used_hoppers_trailer - n_hoppers_trailer)
    }
    
  }
  
  return(exc)
}

calc_penalty_route<-function(input, routes_res, capacity, type_problem) {
  
  exc <- 0
  
  if (type_problem == "TTRP") {
      # CARGA TOTAL
      exc <- max(0, calc_load2(routes_res$route, input$vector.demandas) - capacity)
      
      if ((sum(duplicated(routes_res$route[2:(length(routes_res$route)-1)])))) {
          subroutes <- return_subroutes(routes_res$route, input$n1)
          for (s in 1:length(subroutes)) {
            subroute_i <- subroutes[[s]]$tour[2:(length(subroutes[[s]]$tour)-1)]
            exc <- exc + max(0, calc_load2(subroute_i, input$vector.demandas) - input$capacidad.truck)
          }
      }
  }
  
  if (type_problem == "MCTTRP") {

      # CARGA TOTAL
      #exc <- exc +  max(0, calc_load2_MC(routes_res$route, input$matriz.demandas) - capacity)

      
      # CARGA EN SUBRUTAS
      #for (i in 1:length(routes_res)) {
      #  if ((sum(duplicated(routes_res$route[2:(length(routes_res$route)-1)])))) {
      #    subroutes <- return_subroutes(routes_res$route, input$n1)
      #    subroute_load <- 0
      #    for (s in 1:length(subroutes)) {
      #      subroute_i <- subroutes[[s]]$tour[2:(length(subroutes[[s]]$tour)-1)]
      #      subroute_load <- subroute_load + calc_load2_MC(subroute_i, input$matriz.demandas)
      #    }
      #    exc <- exc + max(0, subroute_load - input$capacidad.truck[1])
      #  }
      #}
      # CARGA EN TOLVAS
      n_hoppers_truck <- length(input$H.camion[1,])
      n_hoppers_trailer <- length(input$H.trailer[1,])
      
      for (i in 1:length(routes_res)) {
        exc <- exc + max(0, routes_res$used_hoppers_truck   - n_hoppers_truck)
        exc <- exc + max(0, routes_res$used_hoppers_trailer - n_hoppers_trailer)
      }

      
  }
  
  
  return(exc)
}



calc_penalty_unique<-function(input, type, route) {
  
  exc <- 0
  
  if (type != "PTR")  exc <- max(0, calc_load2(route, input$vector.demandas) - input$capacidad.vehiculo)
  else   exc <- max(0, calc_load2(route, input$vector.demandas) - input$capacidad.truck)
  
  return(exc)
}


check_feasibility<-function(routes_res, route, input, type_root, type_problem, penalty_capacity, option_up) {
  all_vc <- 1
  subroutes <- 0

  all_vc <- sum(route[2:(length(route)-1)]>input$n1) == length(route)
  subroutes <- sum(duplicated(route[2:(length(route)-1)]))

  # determine new type
  if ((all_vc)&&(!subroutes)&&(type_root == "CVR")) type_root <- "PVR"
  if ((!all_vc)&&(!subroutes)&&(type_root == "CVR")) type_root <- "PTR"
  else if (subroutes) type_root <- "CVR"
  
    
  if (type_problem == "TTRP") {
    load1 <- calc_load2(route, input$vector.demandas)
    
    if (type_root == "PTR") total_capacity <- input$capacidad.truck + penalty_capacity
    else total_capacity <- input$capacidad.vehiculo + penalty_capacity
    subroute_total_capacity <- input$capacidad.truck + penalty_capacity
  }
  
  if (type_problem == "MCTTRP") {
    load1 <- calc_load2_MC(route, input$matriz.demandas)
    if (type_root == "PTR") total_capacity <- input$capacidad.truck[1] + penalty_capacity
    else total_capacity <- input$capacidad.vehiculo[1] + penalty_capacity
    subroute_total_capacity <- input$capacidad.truck[1]  + penalty_capacity
  }

  # check total load
  if (load1 <= total_capacity) feasible <- 1
  else feasible <- 0
  
  # check subroute capacity
  if ((type_root == "CVR") && (feasible)) {

    subroutes <- return_subroutes(route, input$n1)
    unfeasible <- 0
    for (i in 1:length(subroutes)) {
      subroute_i <- subroutes[[i]]$tour[2:(length(subroutes[[i]]$tour)-1)]

      if (type_problem == "TTRP") load_subroute <- calc_load2(subroute_i, input$vector.demandas)
      if (type_problem == "MCTTRP") load_subroute <- calc_load2_MC(subroute_i, input$matriz.demandas)
      if (load_subroute > subroute_total_capacity) {
        unfeasible <- 1
        break
      }
    }

    if (unfeasible) feasible <- 0
  }
  
  # check hoppers
  if ((type_problem == "MCTTRP") && (feasible) && (option_up == "update")){
    if (type_root == "CVR") feasible <- check_capacity_hoppers_MCTTRP_CVR_update(route, routes_res, input) 
    if (type_root == "PVR") feasible <- check_capacity_hoppers_MCTTRP_PVR_update(route, routes_res, input) 
    if (type_root == "PTR") feasible <- check_capacity_hoppers_MCTTRP_PR (route, routes_res, input, total_capacity ) 
  } 
  
  if ((type_problem == "MCTTRP") && (feasible) && (option_up == "new")){
    if (type_root == "CVR") feasible <- check_capacity_hoppers_MCTTRP_CVR_new(route, routes_res, input) 
    if (type_root == "PVR") feasible <- check_capacity_hoppers_MCTTRP_PVR_new(route, routes_res, input) 
    if (type_root == "PTR") feasible <- check_capacity_hoppers_MCTTRP_PR (route, routes_res, input, total_capacity ) 
  } 
  
  return(feasible)
}

check_capacity_TTRP_total_routes_res<-function(routes_res_i, routes_res_j, input, total_capacity) {
  
  route_i <- routes_res_i$route
  route_j <- routes_res_j$route
  
  return(check_capacity_TTRP_total_routes(route_i, route_j, input, total_capacity))
}

check_capacity_total_routes<-function(route_i, route_j, input, total_capacity, penalty_max, type_problem) {
  
  if (type_problem == "TTRP") {
    load1 <- calc_load2(route_i, input$vector.demandas)
    load2 <- calc_load2(route_j, input$vector.demandas)
  }
  
  if (type_problem == "MCTTRP") {
    load1 <- calc_load2_MC(route_i, input$matriz.demandas)
    load2 <- calc_load2_MC(route_j, input$matriz.demandas)  
    total_capacity <- total_capacity[1]
  }
    
  new_load <- load1 + load2
  
  if (new_load <= (total_capacity+penalty_max)) {
    
    return(1)
    
  } else {
    
    return(0)
    
  }
}

check_capacity_TTRP_subroute_routes_res<-function(routes_res_i, routes_res_j, subtours, subroute_index, input) {
  
  route_i <- routes_res_i$route
  route_j <- routes_res_j$route
  
  return(check_capacity_TTRP_subroute_routes(route_i, route_j, subtours, subroute_index, input))
}

# Fallo 27/04/2021. Añadi el argumento penalty_max en la siguiente funcion 
# y tambien en dos "ifs"
check_capacity_subroute_routes<-function(route_i, route_j, subtours, subroute_index, input, type_problem, penalty_max) {
  
  subroute_i <- subtours[[subroute_index]]$tour[2:(length(subtours[[subroute_index]]$tour)-1)]
  
  if (type_problem == "TTRP") {
    load1 <- calc_load2(route_i, input$vector.demandas)
    load2 <- calc_load2(route_j, input$vector.demandas)
    limit_truck <- input$capacidad.truck
    limit_total <- input$capacidad.vehiculo 
    load2_subroute_i <- calc_load2(subroute_i, input$vector.demandas)
    
  }
  
  if (type_problem == "MCTTRP") {
    load1 <- calc_load2_MC(route_i, input$matriz.demandas)
    load2 <- calc_load2_MC(route_j, input$matriz.demandas)   
    limit_truck <- input$capacidad.truck[1]
    limit_total <- input$capacidad.vehiculo[1]
    load2_subroute_i <- calc_load2_MC(subroute_i, input$matriz.demandas)
  }

  new_load <- load1 + load2

  if (new_load <= (limit_total + penalty_max)) {
    new_load <- load1 + load2_subroute_i
    if (new_load <= (limit_truck + penalty_max)) {
      return(1)
    } else {
      return(0)
    }
    
  } else {
    return(0)
  }
}

check_capacity_TTRP_merge_subroute_routes_res<-function(routes_res_i, routes_res_j, input) {
  
  route_i <- routes_res_i$route
  route_j <- routes_res_j$route
  
  return(check_capacity_TTRP_merge_subroute_routes(route_i, route_j, input))
}

check_capacity_merge_subroute_routes<-function(route_i, route_j, input, type_problem) {
  
  
  if (type_problem == "TTRP") {
    load1 <- calc_load2(route_i, input$vector.demandas)
    load2 <- calc_load2(route_j, input$vector.demandas)
    limit_truck <- input$capacidad.truck
    limit_total <- input$capacidad.vehiculo 

  }
  
  if (type_problem == "MCTTRP") {
    load1 <- calc_load2_MC(route_i, input$matriz.demandas)
    load2 <- calc_load2_MC(route_j, input$matriz.demandas)   
    limit_truck <- input$capacidad.truck[1]
    limit_total <- input$capacidad.vehiculo[1]
  }
  
  new_load <- load1 + load2

  if ((new_load <= limit_total)&&(load2 <= limit_truck)) {
    return(1)
  } 
  else {
    return(0)
  }
}

check_capacity_hoppers_MCTTRP_CVR_update<-function(route_i, old_solution, input) {
  
  
  n_hoppers_truck <- length(input$H.camion[1,])
  n_hoppers_trailer <- length(input$H.trailer[1,])
  
  cap_hoppers_truck <- input$H.camion[1,1]
  cap_hoppers_trailer <- input$H.trailer[1,1]
  
  clients_to_add <- setdiff(route_i, old_solution$route)
  clients_to_delete <- setdiff(old_solution$route, route_i)
  
  counter_hoppers_truck <- old_solution$used_hoppers_truck
  counter_hoppers_trailer <- old_solution$used_hoppers_trailer
  
  
  if (length(clients_to_delete)) {

      counter_hoppers_truck <- 0
      counter_hoppers_trailer <- 0
      
      for (i in 1:length(old_solution$clients_vc)) {
        if ( sum(old_solution$clients_vc[[i]]$id ==  clients_to_delete) == 0) {
          counter_hoppers_truck <-   counter_hoppers_truck + length(old_solution$clients_vc[[i]]$hoppers_trucks)
          counter_hoppers_trailer <- counter_hoppers_trailer + length(old_solution$clients_vc[[i]]$hoppers_trailers)
        }
      }
      
      if (length(old_solution$clients_tc)) {
        for (i in 1:length(old_solution$clients_tc)) {
          if ( sum(old_solution$clients_tc[[i]]$id ==  clients_to_delete) == 0) {
            counter_hoppers_truck <-   counter_hoppers_truck + length(old_solution$clients_tc[[i]]$hoppers_trucks)
          }
        }
      }
      
      if (counter_hoppers_truck == 0)   counter_hoppers_truck <- old_solution$used_hoppers_truck
      if (counter_hoppers_trailer == 0) counter_hoppers_trailer <- old_solution$used_hoppers_trailer  
  }
  
  if (length(clients_to_add)) {
    
      logic_clients_to_add <- position_to_add_in_truck( clients_to_add , route_i, input)
      
      for (i in 1:length(clients_to_add)) {
        i_client <- clients_to_add[i]
        
        # add clients in subroute
        if (logic_clients_to_add[[i]] == 1) {
          # demands
          for (z in 1:length(input$matriz.demandas[i_client+1,])) {
            demand <- input$matriz.demandas[i_client+1,z]
            while (demand > 0) {
              demand <- demand - cap_hoppers_truck
              counter_hoppers_truck <- counter_hoppers_truck + 1
            } 
          }
        }
        # add clients in main tour
        else {
          for (j in 1:length(input$matriz.demandas[i_client+1,])){
            demand <- input$matriz.demandas[i_client+1,j]
            #trailers
            while ((demand > 0) && (counter_hoppers_trailer < n_hoppers_trailer)){
              demand <- demand - cap_hoppers_trailer
              counter_hoppers_trailer <- counter_hoppers_trailer + 1
            }
            # trucks
            while ((demand > 0)) {
              demand <- demand - cap_hoppers_truck
              counter_hoppers_truck <- counter_hoppers_truck + 1
            }
          }
        }
      }
  
  }

  if ((counter_hoppers_trailer <= n_hoppers_trailer)&&(counter_hoppers_truck <= n_hoppers_truck)) {
    return (1)
  } else {
    return (0)
  }
  
}



check_capacity_hoppers_MCTTRP_return_hoppers<-function(route_i, old_solution, input) {
  
  
  n_hoppers_truck <- length(input$H.camion[1,])
  n_hoppers_trailer <- length(input$H.trailer[1,])
  
  cap_hoppers_truck <- input$H.camion[1,1]
  cap_hoppers_trailer <- input$H.trailer[1,1]
  
  clients_to_add <- setdiff(route_i, old_solution$route)
  clients_to_delete <- setdiff(old_solution$route, route_i)
  
  counter_hoppers_truck <- old_solution$used_hoppers_truck
  counter_hoppers_trailer <- old_solution$used_hoppers_trailer
  
  
  if (length(clients_to_delete)) {
    
    counter_hoppers_truck <- 0
    counter_hoppers_trailer <- 0
    
    if (length(old_solution$clients_vc)) {
      for (i in 1:length(old_solution$clients_vc)) {
        if ( sum(old_solution$clients_vc[[i]]$id ==  clients_to_delete) == 0) {
          counter_hoppers_truck <-   counter_hoppers_truck + length(old_solution$clients_vc[[i]]$hoppers_trucks)
          counter_hoppers_trailer <- counter_hoppers_trailer + length(old_solution$clients_vc[[i]]$hoppers_trailers)
        }
      }
    }
    
    if (length(old_solution$clients_tc)) {
      if (length(old_solution$clients_tc)) {
        for (i in 1:length(old_solution$clients_tc)) {
          if ( sum(old_solution$clients_tc[[i]]$id ==  clients_to_delete) == 0) {
            counter_hoppers_truck <-   counter_hoppers_truck + length(old_solution$clients_tc[[i]]$hoppers_trucks)
          }
        }
      }
    }
    
    if (counter_hoppers_truck == 0)    counter_hoppers_truck <- old_solution$used_hoppers_truck
    if (counter_hoppers_trailer == 0)  counter_hoppers_trailer <- old_solution$used_hoppers_trailer
  }
  
  if (length(clients_to_add)) {
    new_type <- old_solution$type
    rep_exist <- sum(duplicated(route_i[2:(length(route_i)-1)]))
    if ((old_solution$type == "PTR") && (rep_exist))  new_type <- "CVR"
    if ((old_solution$type == "PVR") && (rep_exist))  new_type <- "CVR"
    if ((old_solution$type == "CVR") && (!rep_exist)) {
      count_tc <- 0
      for (i in 2:(length(route_i)-1)){
        if (route_i[i]>input$n1) count_tc <- count_tc + 1
      }
      if (count_tc==0) new_type <- "PVR"
      else new_type <- "PTR"
    }
    
    if (new_type == "PTR") logic_clients_to_add <- rep(1, length(clients_to_add))
    if (new_type == "PVR") logic_clients_to_add <- rep(0, length(clients_to_add))
    if (new_type == "CVR") logic_clients_to_add <- position_to_add_in_truck( clients_to_add , route_i, input)
    
    for (i in 1:length(clients_to_add)) {
      i_client <- clients_to_add[i]
      
      # add clients in subroute
      if (logic_clients_to_add[[i]] == 1) {
        # demands
        for (z in 1:length(input$matriz.demandas[i_client+1,])) {
          demand <- input$matriz.demandas[i_client+1,z]
          while (demand > 0) {
            demand <- demand - cap_hoppers_truck
            counter_hoppers_truck <- counter_hoppers_truck + 1
          } 
        }
      }
      # add clients in main tour
      else {
        for (j in 1:length(input$matriz.demandas[i_client+1,])){
          demand <- input$matriz.demandas[i_client+1,j]
          #trailers
          while ((demand > 0) && (counter_hoppers_trailer < n_hoppers_trailer)){
            demand <- demand - cap_hoppers_trailer
            counter_hoppers_trailer <- counter_hoppers_trailer + 1
          }
          # trucks
          while ((demand > 0)) {
            demand <- demand - cap_hoppers_truck
            counter_hoppers_truck <- counter_hoppers_truck + 1
          }
        }
      }
    }
    
  }
  
  res <- list()
  res$hoppers_trailer <- counter_hoppers_trailer
  res$hoppers_truck   <- counter_hoppers_truck
  
  return(res)
  
}



check_capacity_hoppers_MCTTRP_return_hoppers_new<-function(route_i, old_solution, input) {
  
  
  n_hoppers_truck <- length(input$H.camion[1,])
  n_hoppers_trailer <- length(input$H.trailer[1,])
  
  cap_hoppers_truck <- input$H.camion[1,1]
  cap_hoppers_trailer <- input$H.trailer[1,1]
  
  clients_to_add <- unique(route_i[2:(length(route_i)-1)])

  counter_hoppers_truck <- 1
  counter_hoppers_trailer <- 1
  
  new_type <- old_solution$type
  rep_exist <- sum(duplicated(route_i[2:(length(route_i)-1)]))
  
  if ((old_solution$type == "PTR") && (rep_exist))  new_type <- "CVR"
  if ((old_solution$type == "PVR") && (rep_exist))  new_type <- "CVR"
  if ((old_solution$type == "CVR") && (!rep_exist)) {
    count_tc <- 0
    for (i in 2:(length(route_i)-1)){
      if (route_i[i]>input$n1) count_tc <- count_tc + 1
    }
    if (count_tc==0) new_type <- "PVR"
    else new_type <- "PTR"
  }
    
  if (new_type == "PTR") logic_clients_to_add <- rep(1, length(clients_to_add))
  if (new_type == "PVR") logic_clients_to_add <- rep(0, length(clients_to_add))
  if (new_type == "CVR") logic_clients_to_add <- position_to_add_in_truck( clients_to_add , route_i, input)
    
  for (i in 1:length(clients_to_add)) {
      i_client <- clients_to_add[i]
      
      # add clients in subroute
      if (logic_clients_to_add[[i]] == 1) {
        # demands
        for (z in 1:length(input$matriz.demandas[i_client+1,])) {
          demand <- input$matriz.demandas[i_client+1,z]
          while (demand > 0) {
            demand <- demand - cap_hoppers_truck
            counter_hoppers_truck <- counter_hoppers_truck + 1
          } 
        }
      }
      # add clients in main tour
      else {
        for (j in 1:length(input$matriz.demandas[i_client+1,])){
          demand <- input$matriz.demandas[i_client+1,j]
          #trailers
          while ((demand > 0) && (counter_hoppers_trailer < n_hoppers_trailer)){
            demand <- demand - cap_hoppers_trailer
            counter_hoppers_trailer <- counter_hoppers_trailer + 1
          }
          # trucks
          while ((demand > 0)) {
            demand <- demand - cap_hoppers_truck
            counter_hoppers_truck <- counter_hoppers_truck + 1
          }
        }
      }
    }
    
  
  
  res <- list()
  res$hoppers_trailer <- counter_hoppers_trailer
  res$hoppers_truck   <- counter_hoppers_truck
  
  return(res)
  
}



check_capacity_hoppers_MCTTRP_PVR_update<-function(route_i, old_solution, input){
  
  n_hoppers_truck <- length(input$H.camion[1,])
  n_hoppers_trailer <- length(input$H.trailer[1,])
  
  cap_hoppers_truck <- input$H.camion[1,1]
  cap_hoppers_trailer <- input$H.trailer[1,1]
  
  clients_to_add <- setdiff(route_i, old_solution$route)
  clients_to_delete <- setdiff(old_solution$route, route_i)
  
  
  counter_hoppers_truck <- old_solution$used_hoppers_truck
  counter_hoppers_trailer <- old_solution$used_hoppers_trailer
  
  
  if (length(clients_to_delete)) {
    
    counter_hoppers_truck <- 0
    counter_hoppers_trailer <- 0
    
    for (i in 1:length(old_solution$clients_vc)) {
      if ( sum(old_solution$clients_vc[[i]]$id ==  clients_to_delete) == 0) {
        counter_hoppers_truck <-   counter_hoppers_truck + length(old_solution$clients_vc[[i]]$hoppers_trucks)
        counter_hoppers_trailer <- counter_hoppers_trailer + length(old_solution$clients_vc[[i]]$hoppers_trailers)
      }
    }
    
    old_solution$used_hoppers_truck <- counter_hoppers_truck
    old_solution$used_hoppers_trailer <- counter_hoppers_trailer
  }
  
  if (length(clients_to_add)) {
    
    counter_hoppers_truck <- old_solution$used_hoppers_truck
    counter_hoppers_trailer <- old_solution$used_hoppers_trailer
    
    for (i in 1:length(clients_to_add)) {
      i_client <- clients_to_add[i]
      
        for (j in 1:length(input$matriz.demandas[i_client+1,])){
          demand <- input$matriz.demandas[i_client+1,j]
          #trailers
          while ((demand > 0) && (counter_hoppers_trailer < n_hoppers_trailer)){
            demand <- demand - cap_hoppers_trailer
            counter_hoppers_trailer <- counter_hoppers_trailer + 1
          }
          # trucks
          while ((demand > 0)) {
            demand <- demand - cap_hoppers_truck
            counter_hoppers_truck <- counter_hoppers_truck + 1
          }
        }
      
    }
    
  }
  
  if ((counter_hoppers_trailer <= n_hoppers_trailer)&&(counter_hoppers_truck <= n_hoppers_truck)) {
    return (1)
  } else {
    return (0)
  }
  
}

check_capacity_hoppers_MCTTRP_CVR_new<-function(route_i, routes_res, input) {
  
  n_hoppers_truck <- length(input$H.camion[1,])
  n_hoppers_trailer <- length(input$H.trailer[1,])
  
  cap_hoppers_truck <- input$H.camion[1,1]
  cap_hoppers_trailer <- input$H.trailer[1,1]
  
  subroutes <- return_subroutes(route_i, input$n1)
  main_route <- return_main_route(route_i)
  
  counter_hoppers_truck <- 0
  for (i in 1:length(subroutes)) {
    subroutei <- subroutes[[i]]
    for (j in 2:(length(subroutei$tour)-1)) {
      j_client <- subroutei$tour[j]
      for (z in 1:length(input$matriz.demandas[j_client+1,])) {
        demand <- input$matriz.demandas[j_client+1,z]
        while (demand > 0) {
          demand <- demand - cap_hoppers_truck
          counter_hoppers_truck <- counter_hoppers_truck + 1
        } 
      }
    }
  }
  
  counter_hoppers_trailer <- 0
  for (i in 2:(length(main_route)-1)) {
    i_client <- main_route[i]
    for (j in 1:length(input$matriz.demandas[i_client+1,])){
      demand <- input$matriz.demandas[i_client+1,j]
      #trailers
      while ((demand > 0) && (counter_hoppers_trailer < n_hoppers_trailer)){
        demand <- demand - cap_hoppers_trailer
        counter_hoppers_trailer <- counter_hoppers_trailer + 1
      }
      # trucks
      while ((demand > 0)) {
        demand <- demand - cap_hoppers_truck
        counter_hoppers_truck <- counter_hoppers_truck + 1
      }
    }
  }

  if ((counter_hoppers_trailer <= n_hoppers_trailer)&&(counter_hoppers_truck <= n_hoppers_truck)) {
    return (1)
  } else {
    return (0)
  }
  
}

check_capacity_hoppers_MCTTRP_PVR_new<-function(route_i, routes_res, input){
  
  for (i in 1:(length(routes_res))) {
    if ( sum(routes_res[[i]]$route==route_i[2]) > 0 ) {
      index <- i
      break
    }
  }
  
  
  new_order <- c(0)
  for (i in 2:(length(route_i)-1)) {
    res <- return_size_hoppers(routes_res, index, route_i[i])
    size_tra <-  res$size_tra 
    size_tru <-  res$size_tru
    if ((size_tra > 0)&&(size_tru == 0)) {
      new_order <- c(new_order, route_i[i])
    }
  }
  
  for (i in 2:(length(route_i)-1)) {
    res <- return_size_hoppers(routes_res, index, route_i[i])
    size_tra <-  res$size_tra 
    size_tru <-  res$size_tru
    if ((size_tra > 0)&&(size_tru > 0)) {
      new_order <- c(new_order, route_i[i])
    }
  }
  
  for (i in 2:(length(route_i)-1)) {
    res <- return_size_hoppers(routes_res, index, route_i[i])
    size_tra <-  res$size_tra 
    size_tru <-  res$size_tru
    if ((size_tra == 0)&&(size_tru > 0)) {
      new_order <- c(new_order, route_i[i])
    }
  }
  
  new_order <- c(new_order, 0)
  
  flag_exit <- check_capacity_hoppers_MCTTRP_generic(new_order, input, 1) 
  
  return(flag_exit)
}

check_capacity_hoppers_MCTTRP_PR<-function(route_i, old_solution, input, capacity) {
  
  if (capacity[1] == input$capacidad.vehiculo[1] ) {
    flag_exit <- check_capacity_hoppers_MCTTRP_PVR_update(route_i, old_solution, input)
  } else {
    flag_exit <- check_capacity_hoppers_MCTTRP_generic(route_i, input, 0) 
  }
  
  
  return(flag_exit)
}

check_capacity_hoppers_MCTTRP_generic<-function(route_i, input, check_trailer) {
  
  n_hoppers_truck <- length(input$H.camion[1,])
  n_hoppers_trailer <- length(input$H.trailer[1,])
  
  cap_hoppers_truck <- input$H.camion[1,1]
  cap_hoppers_trailer <- input$H.trailer[1,1]
  
  route <- route_i
  
  counter_hoppers_truck <- 0
  counter_hoppers_trailer <- 0
  for (i in 2:(length(route)-1)) {
    i_client <- route[i]
    for (j in 1:length(input$matriz.demandas[i_client+1,])){
      demand <- input$matriz.demandas[i_client+1,j]
      #trailers
      while ((demand > 0) && (check_trailer) && (counter_hoppers_trailer < n_hoppers_trailer)){
        demand <- demand - cap_hoppers_trailer
        counter_hoppers_trailer <- counter_hoppers_trailer + 1
      }
      # trucks
      while ((demand > 0)) {
        demand <- demand - cap_hoppers_truck
        counter_hoppers_truck <- counter_hoppers_truck + 1
      }
    }
  }
  
  if (check_trailer) {
    if ((counter_hoppers_trailer <= n_hoppers_trailer)&&(counter_hoppers_truck <= n_hoppers_truck)) {
      return (1)
    } else {
      return (0)
    }
  }
  else {
    if (counter_hoppers_truck <= n_hoppers_truck) {
      return (1)
    } else {
      return (0)
    }
  }
}

check_capacity_hoppers_MCTTRP_analysis<-function(solution, input, pos) {
  
  error <- 0
  
  n_hoppers_truck <- length(input$H.camion[1,])
  n_hoppers_trailer <- length(input$H.trailer[1,])
  
  cap_hoppers_truck <- input$H.camion[1,1]
  cap_hoppers_trailer <- input$H.trailer[1,1]
  
  hcounter_truck <- 0
  hcounter_trailer <- 0
  
  if (length(solution$clients_vc)) {
    for (i in 1:length(solution$clients_vc)) {
      client <- solution$clients_vc[[i]]
      if (length(client$hoppers_trucks)) {
        for (j in 1:length(client$hoppers_trucks)) {
          if (as.numeric(client$hoppers_trucks[[j]][[2]]) > cap_hoppers_truck) error <- error + 1
        }
      }
      if ((solution$type != "PTR") && length(client$hoppers_trailers)) {
        for (j in 1:length(client$hoppers_trailers)) {
          if (as.numeric(client$hoppers_trailers[[j]][[2]]) > cap_hoppers_trailer) error <- error + 1
        }    
      }
      
      hcounter_truck <- hcounter_truck + length(client$hoppers_trucks)
      hcounter_trailer <- hcounter_trailer + length(client$hoppers_trailers)
    }
  }
  
  
  if (length(solution$clients_tc)) {
    for (i in 1:length(solution$clients_tc)) {
      client <- solution$clients_tc[[i]]
      if (length(client$hoppers_trucks)) {
        for (j in 1:length(client$hoppers_trucks)) {
          if (as.numeric(client$hoppers_trucks[[j]][[2]]) > cap_hoppers_truck) error <- error + 1
        }
      }
      if ((solution$type != "PTR") && length(client$hoppers_trailers)) {
        for (j in 1:length(client$hoppers_trailers)) {
          if (as.numeric(client$hoppers_trailers[[j]][[2]]) > cap_hoppers_trailer) error <- error + 1
        }    
      }
      
      hcounter_truck <- hcounter_truck + length(client$hoppers_trucks)
      hcounter_trailer <- hcounter_trailer + length(client$hoppers_trailers)
    }
  }
  
  if (hcounter_trailer!=solution$used_hoppers_trailer ) print(paste0("discrepancia en used_hoppers_trailers in route ", pos))
  if (hcounter_truck!=solution$used_hoppers_truck)   print(paste0("discrepancia en used_hoppers_truck ", pos))
  
  if ((hcounter_trailer != 0) && (hcounter_trailer > n_hoppers_trailer)) error <- error + 1
  if ((hcounter_truck != 0) && (hcounter_truck > n_hoppers_truck)) error <- error + 1

  
  return(!error)
}

return_size_hoppers<-function(routes_res, index, route_i) {
  size_tra <- 0
  size_tru <- 0
  
  for (i in 1:length(routes_res[[index]]$clients_vc)) {
    if (routes_res[[index]]$clients_vc[[i]]$id == route_i) {
      size_tra <- size_tra + length(routes_res[[index]]$clients_vc[[i]]$hoppers_trailers)
      size_tru <- size_tru + length(routes_res[[index]]$clients_vc[[i]]$hoppers_trucks)
    }
  }
  
  res <- list()
  res$size_tra <- size_tra
  res$size_tru <- size_tru
  
  return(res)
}

return_occupied_hoppers_MCTTRP<-function(type, hoppers, id_vehicle) {
  counter <- 0
  
  for (i in 1:length(hoppers[,1])) {
    if ((hoppers[i,4]==id_vehicle)&&(hoppers[i,3]==type)) {
      counter <- counter + 1
    }
  }
  
  return(counter)
}

return_i_vehicle<-function(route_i, route_res) {
  node <- route_i[2]
  for (i in 1:length(route_res)) {
    if (sum(route_res[[i]]$route==node)>0) {
      id <- i
      break;
    }
  }
  
  return(id)
}

