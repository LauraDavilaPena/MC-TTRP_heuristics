# Algoritmo Tabu
tabu_search <- function(input, current_solution, current_cost, best_cost, type_problem, maxi, counter_i, phi, pen_max) {
  #print("TABU SEARCH")
  # Extract tabulist_data vars
  tabulist_data <- init_tabulist_data(input, current_solution, pen_max)
  
  alpha <- 1
  vecinity <-  input$vecinity
  
  # inititalize parameters
# comprobar seed 
  tau <- return_tau(input$n, length(current_solution)) + 1
  
  zeta <- runif(1)
  gamma <- runif(1)
  
  stop_tabu <- ceiling(sqrt((maxi-counter_i)*phi))
  # solutions que hai na TS: 
  #   best_local_solution: mellor infact/fact local
  #   best_f_solution: mellor fact local
  #   current_solution: solution actual sobre a que iteras
  best_local_solution <- current_solution
  best_local_cost <- current_cost
  best_f_solution <- current_solution
  best_f_cost <- Inf
  changed_routes <- 1:length(current_solution)
  counter_not_improve <- 0
  # non facerlle caso
  perc_v <- 1 #max (min (rnorm(1, mean = 0.3, sd = 0.1), 1.0), 0.1)

  # create mov_list C
  .Call('createMovsList', PACKAGE = "mcttrpalns")
  
  while ( counter_not_improve < stop_tabu) {
    
    start_time <- Sys.time()
    current_solution <- result_improvement(input, current_solution, type_problem)
    current_cost <- calculateTotalDistanceTS(input, alpha, current_solution, type_problem)
    
    # execucion dos movementos --> ruta <- 0 1 4 3 0
    movements_imp_new(input, current_solution, type_problem, vecinity, perc_v, 
                              tabulist_data$penalty_capacity, changed_routes, 0)

    size_mov_list <- .Call('return_size_mov_list', PACKAGE = "mcttrpalns")

    evaluate_cost_mov_list_new(input, changed_routes, current_solution, alpha, size_mov_list, type_problem)

    if (size_mov_list) {
      
      order_movs_new("cost")
      
      counter_index_order <- 1
      not_in_tabu_list <- 0
      enter_penalty_loop <- 0
      
      #print(" init cost values")
      
      while ((!not_in_tabu_list)&&(counter_index_order <= size_mov_list)) {
        
        movid <- return_mov_struct(counter_index_order)

        if (!check_result_in_tabu_list_new(tabulist_data$tabulist, movid, best_local_cost)) {
          
          not_in_tabu_list <- 1
          if ( movid$mov_list_cost < current_cost ) {
            
            changed_routes <- modified_changed_list(changed_routes, movid$indexr1,  movid$indexr2 )
            current_solution <- insert_selected_mov_new(input, movid, current_solution, type_problem)
            tabulist_data <- insert_result_in_tabulist_new(movid, tabulist_data, tau)
            modified_mov_list_using_changed_list_new(changed_routes)
            
            #print(paste0("INSERT cost -> ",  movid$mov_list_cost, 
            #             " mov ",     movid$string1, 
            #             " nopen ",   movid$mov_list_cost_nopen,
            #             " cost_feas ", movid$mov_list_cost_feas,  " feas ", calc_penalty(input, current_solution, type_problem),
            #             " indexr1 ", movid$indexr1, " indexr2 ", movid$indexr2))
            
            #print(paste0(" hopptrucks1  ", movid$hoppers_truck1, "  hopptrailer1  ", movid$hoppers_trailer1,
            #             "  hopptrucks2  ", movid$hoppers_truck2, "  hopptrailer2 ", movid$hoppers_trailer2))

            #for (jj in 1:length(current_solution)){
            #  print(paste0("used_h_trailers ", current_solution[[jj]]$used_hoppers_trailer, 
            #               "  used_hoppers_truck ", current_solution[[jj]]$used_hoppers_truck ))
            #  
            #}
            
            #print("ROUTES")
            #for (jj in 1:length(current_solution)){
            #  print(current_solution[[jj]]$route )
            #}
            
            #readline()
            
          } else enter_penalty_loop <- 1
          
        }
        else {
          not_in_tabu_list <- 0
        }
        
        counter_index_order <- counter_index_order + 1
        
      }
      
      
      if (enter_penalty_loop){
        
        .Call('c_eval_movs_pen',  tabulist_data$table_freq, zeta, counter_i, PACKAGE = "mcttrpalns")
        
        order_movs_new("cost_pen")
        
        counter_index_order2 <- 1
        not_in_tabu_list <- 0
        
        start_time <- Sys.time()
        
        while((enter_penalty_loop)&&(!not_in_tabu_list)&&(counter_index_order2 <= size_mov_list))  {
          
          movid <- return_mov_struct(counter_index_order2) 

          if ((!check_result_in_tabu_list_new(tabulist_data$tabulist, movid, best_local_cost))) {
            
            not_in_tabu_list <- 1
            changed_routes <- modified_changed_list(changed_routes, movid$indexr1, movid$indexr2 )
            current_solution <- insert_selected_mov_new(input, movid, current_solution, type_problem)
            tabulist_data <- insert_result_in_tabulist_new(movid, tabulist_data, tau)
            modified_mov_list_using_changed_list_new(changed_routes)
            
            #print(paste0("INSERT pen cost -> ",  movid$mov_list_cost, 
            #             " mov ",     movid$string1, 
            #           " nopen ",   movid$mov_list_cost_nopen,
            #           " cost_feas ", movid$mov_list_cost_feas, " feas ", calc_penalty(input, current_solution, type_problem)))
            #readline()
            
          }
          else not_in_tabu_list <- 0
          
          counter_index_order2 <- counter_index_order2 + 1
        }        
      }
      
      stop <- 0
      
    } else stop <- 1
    
    #print(current_solution[[1]])
    #print("xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx")
    
    current_solution <- update_solution(current_solution, input, type_problem)
    current_cost <- calculateTotalDistanceTS(input, alpha, current_solution, type_problem)

    
    if ((current_cost < best_local_cost)) { #&&(!calc_penalty(input, current_solution))) {
      
      best_local_solution <- current_solution
      best_local_cost <- current_cost
      counter_not_improve <- 0
    }
    else {
      counter_not_improve <- counter_not_improve + 1
    } 
    
    if ((current_cost < best_f_cost)&&(calc_penalty(input, current_solution, type_problem)==0)) {
      best_f_solution <- current_solution
      best_f_cost <- current_cost
    }
    
    if (stop) counter_not_improve <- stop_tabu
    
    alpha <- update_penalties(input,  alpha, gamma, current_solution, type_problem)
    tabulist_data$tabulist <- update_counters_tabu_list(tabulist_data$tabulist)
    
    #print(paste0("global solution ", best_f_cost, "   penalty ", calc_penalty(input, best_f_solution, type_problem)))
    #all_routes(best_f_solution)
    #analyse(all_routes(best_f_solution), input, best_f_solution, "MCTTRP")
    #readline()
    
    # print(paste0("   end iteration ", difftime(Sys.time(), start_time, units = "secs")))
    # print("")
  }
  
  result <- list()
  result$current_solution <- best_local_solution
  result$current_cost <- best_local_cost
  result$best_f_solution <- best_f_solution
  result$best_f_cost <- best_f_cost
  
  
  return(result)
}

return_mov_struct<-function(counter_index_order){
  
  mov_list <- list()
  mov_list$indexr1 <- .Call('return_mov_list_indexr1', as.integer(counter_index_order), PACKAGE = "mcttrpalns")
  mov_list$indexr2 <- .Call('return_mov_list_indexr2', as.integer(counter_index_order), PACKAGE = "mcttrpalns")
  mov_list$client1 <- .Call('return_mov_list_client1', as.integer(counter_index_order), PACKAGE = "mcttrpalns")
  mov_list$client2 <- .Call('return_mov_list_client2', as.integer(counter_index_order), PACKAGE = "mcttrpalns")
  mov_list$route1 <-  .Call('return_mov_list_route1' , as.integer(counter_index_order), PACKAGE = "mcttrpalns")
  mov_list$route2 <-  .Call('return_mov_list_route2' , as.integer(counter_index_order), PACKAGE = "mcttrpalns")
  mov_list$hoppers_truck1 <-  .Call('return_mov_list_hoppers_truck1' , as.integer(counter_index_order), PACKAGE = "mcttrpalns")
  mov_list$hoppers_trailer1 <-  .Call('return_mov_list_hoppers_trailer1' , as.integer(counter_index_order), PACKAGE = "mcttrpalns")
  mov_list$hoppers_truck2 <-  .Call('return_mov_list_hoppers_truck2' , as.integer(counter_index_order), PACKAGE = "mcttrpalns")
  mov_list$hoppers_trailer2 <-  .Call('return_mov_list_hoppers_trailer2' , as.integer(counter_index_order), PACKAGE = "mcttrpalns")
  mov_list$root1 <-  .Call('return_mov_list_root1' , as.integer(counter_index_order), PACKAGE = "mcttrpalns")
  mov_list$root2 <-  .Call('return_mov_list_root2' , as.integer(counter_index_order), PACKAGE = "mcttrpalns")
  mov_list$string1 <- .Call('return_mov_list_string' , as.integer(counter_index_order), PACKAGE = "mcttrpalns")
  mov_list$mov_list_cost <- .Call('return_mov_list_cost' , as.integer(counter_index_order), PACKAGE = "mcttrpalns")
  mov_list$mov_list_cost_pen <- .Call('return_mov_list_cost_pen' , as.integer(counter_index_order), PACKAGE = "mcttrpalns")
  mov_list$mov_list_cost_feas  <- .Call('return_mov_list_cost_feas' , as.integer(counter_index_order), PACKAGE = "mcttrpalns")
  mov_list$mov_list_cost_nopen  <- .Call('return_mov_list_cost_nopen' , as.integer(counter_index_order), PACKAGE = "mcttrpalns")
  mov_list$opt_reconf  <- .Call('return_mov_list_opt_reconf' , as.integer(counter_index_order), PACKAGE = "mcttrpalns")
  
  return(mov_list)
}


# order movs
order_movs<-function(movs, option="cost") {
  
  if (option == "cost")  {
    
    .Call('order', as.integer(1), PACKAGE = "mcttrpalns")
    
    start_time <- Sys.time()
    
    index_order <- order(unlist(movs$mov_list_cost), decreasing = FALSE)
    
    # print(paste0("   order_TIME ", difftime(Sys.time(), start_time, units = "secs")))
    
    
  }
  
  if (option == "cost_pen")  {
    
    .Call('order', as.integer(0), PACKAGE = "mcttrpalns")
    
    start_time <- Sys.time()
    
    
    index_order <- order(unlist(movs$mov_list_cost_pen), decreasing = FALSE)
    
    
    # print(paste0("   order_TIME_pen ", difftime(Sys.time(), start_time, units = "secs")))
    
  }
  
  
  return( index_order)
}




# order movs
order_movs_new<-function(option="cost") {
  
  if (option == "cost")  {
    
    .Call('order', as.integer(1), PACKAGE = "mcttrpalns")
    
  }
  
  if (option == "cost_pen")  {
    
    .Call('order', as.integer(0), PACKAGE = "mcttrpalns")
    
  }
  
  
}






# check_result_in_tabu_list 
check_result_in_tabu_list<-function(tabulist, movs, index_order, counter_index_order, bestcost) {
  
  if (movs$mov_list_cost[[index_order[counter_index_order]]] >= bestcost) {
    
    
    if ((length(movs$mov_list[[index_order[counter_index_order]]]$client2)>=1)&&(movs$mov_list[[index_order[counter_index_order]]]$client2 != 0)) {
      
      tabu_check <- check_in_tabulist(tabulist, movs$mov_list[[index_order[counter_index_order]]]$client1,
                                      movs$mov_list[[index_order[counter_index_order]]]$indexr2)
      
      tabu_check <- tabu_check + check_in_tabulist(tabulist, movs$mov_list[[index_order[counter_index_order]]]$client2, 
                                                   movs$mov_list[[index_order[counter_index_order]]]$indexr1)
    } else {
      
      tabu_check <- check_in_tabulist(tabulist, movs$mov_list[[index_order[counter_index_order]]]$client1,
                                      movs$mov_list[[index_order[counter_index_order]]]$indexr1)
      
    }
    
  } else {
    tabu_check <- 0
  }
  
  return (tabu_check)
}



# check_result_in_tabu_list 
check_result_in_tabu_list_new<-function(tabulist, movid, bestcost) {
  
  if (movid$mov_list_cost >= bestcost) {
    
    
    if ((length(movid$client2)>=1)&&(movid$client2 != 0)&&(movid$root2 != -1)) {
      
      tabu_check <- check_in_tabulist(tabulist, movid$client1, movid$indexr2, movid$root1)
      
      tabu_check <- tabu_check + check_in_tabulist(tabulist, movid$client2, movid$indexr1, movid$root2)
      
    } else {
      
      tabu_check <- check_in_tabulist(tabulist, movid$client1,  movid$indexr1, movid$root1)
      
    }
    
  } else {
    tabu_check <- 0
  }
  
  return (tabu_check)
}


#
insert_result_in_tabulist_new<-function(movid, tabulist_data, tau) {
  
  
  if ((length(movid$client2)>=1)&&(movid$client2!=0)&&(movid$root2!=-1)) {
    
    # tabu list
    tabulist_data$tabulist <- insert_in_tabu_list(movid$client1, movid$indexr1, tau, tabulist_data$tabulist, movid$root1) 
    # table freq
    for (ii in 1:length(movid$client1)) {
      client <- movid$client1[ii]
      tabulist_data$table_freq <- update_table_freq(tabulist_data$table_freq, client, movid$indexr2)
    }
    
    # tabu list
    tabulist_data$tabulist <- insert_in_tabu_list(movid$client2, movid$indexr2, tau, tabulist_data$tabulist, movid$root2) 
    # table freq
    for (ii in 1:length(movid$client2)) {
      client <- movid$client2[ii]
      tabulist_data$table_freq <- update_table_freq(tabulist_data$table_freq, client, movid$indexr1)
    }
  } else {
    
    # tabu list
    tabulist_data$tabulist <- insert_in_tabu_list(movid$client1, movid$indexr1, tau, tabulist_data$tabulist, movid$root1) 
    # table freq
    for (ii in 1:length(movid$client1)) {
      client <- movid$client1[ii]
      tabulist_data$table_freq <- update_table_freq(tabulist_data$table_freq, client, movid$indexr1)
    }
    
  }
  
  return(tabulist_data)
}

# movements_imp
movements_imp_new <- function(input, current_solution, type_problem, vecinity, perc_vecinity, penalty_capacity, changed_routes, check_feas) {
  
  
  start_time1 <- Sys.time()
  start_time <- Sys.time()
  
  # exchange moves
  exchange_movement_client_subtour_and_vc_creating_subtour(input, current_solution, changed_routes, type_problem, 
                                                                   vecinity, perc_vecinity, penalty_capacity, check_feas) 
  
  #print(paste0("   exchange_movement_client_subtour_and_vc_creating_subtour ", difftime(Sys.time(), start_time, units = "secs")))
  #start_time <- Sys.time()
  
  
  exchange_movement_change_parking(input, current_solution, changed_routes, type_problem, vecinity, perc_vecinity ,
                                           penalty_capacity, check_feas) 
  
  #print(paste0("   exchange_movement_change_parking ", difftime(Sys.time(), start_time, units = "secs")))
  #start_time <- Sys.time()
  
  # MAS LENTA 1
  exchange_movement_vc_main_tour_tc_subtour(input, current_solution, changed_routes, type_problem, vecinity, perc_vecinity, 
                                                    penalty_capacity, check_feas)  
  
  #print(paste0("   exchange_movement_vc_main_tour_tc_subtour ", difftime(Sys.time(), start_time, units = "secs")))
  #start_time <- Sys.time()
  
  # MAS LENTA 3
  exchange_movement_client_short_subtour_and_client_in_main_tour(input, current_solution, changed_routes,  type_problem, 
                                                                         vecinity, perc_vecinity, penalty_capacity, check_feas)  
  
  #print(paste0("   exchange_movement_client_short_subtour_and_client_in_main_tour ", difftime(Sys.time(), start_time, units = "secs")))
  #start_time <- Sys.time()
  
  move_subroute(input, current_solution, changed_routes, type_problem, penalty_capacity, check_feas) 
  
  #print(paste0("   move_subroute ", difftime(Sys.time(), start_time, units = "secs")))
  #start_time <- Sys.time()
  
  
  exchange_ptr_and_subtour(input, current_solution, changed_routes, type_problem, penalty_capacity, check_feas) 
  
  
  #print(paste0("   exchange_ptr_and_subtour ", difftime(Sys.time(), start_time, units = "secs")))
  #start_time <- Sys.time()
  
  
  # MAS LENTA 2
  exchange_movement_tc_PTR_and_vc_in_main_tour(input, current_solution, changed_routes, type_problem, vecinity, 
                                                       perc_vecinity, penalty_capacity, check_feas) 
  
  
  #print(paste0("   exchange_movement_tc_PTR_and_vc_in_main_tour ", difftime(Sys.time(), start_time, units = "secs")))
  #start_time <- Sys.time()
  
  
  # moves in the same root
  move_subroute_same_route(input, current_solution, changed_routes, type_problem, penalty_capacity, check_feas) 
  
  
  #print(paste0("   move_subroute_same_route ", difftime(Sys.time(), start_time, units = "secs")))
  #start_time <- Sys.time()
  
  create_new_subtour_vc_same_route(input, current_solution, changed_routes, type_problem, penalty_capacity, check_feas)  
  
  
  #print(paste0("   create_new_subtour_vc_same_route ", difftime(Sys.time(), start_time, units = "secs")))
  #start_time <- Sys.time()
  
  move_vc_client_subroute_to_main_tour_and_split_same_route(input, current_solution, changed_routes, type_problem, 
                                                                    penalty_capacity, check_feas)  
  
  
  #print(paste0("   move_vc_client_subroute_to_main_tour_and_split_same_route ", difftime(Sys.time(), start_time, units = "secs")))
  #start_time <- Sys.time()
  
  
  # basic moves
  exchange_tc_two_routes(input, current_solution, changed_routes,  type_problem, vecinity, perc_vecinity, 
                                 penalty_capacity, check_feas) 
  
  
  #print(paste0("   exchange_tc_two_routes ", difftime(Sys.time(), start_time, units = "secs")))
  #start_time <- Sys.time()
  
  
  # MAS LENTA 4
  exchange_vc_two_routes(input, current_solution, changed_routes,  type_problem, vecinity, perc_vecinity, 
                                 penalty_capacity, check_feas) 
  
  
  #print(paste0("   exchange_vc_two_routes ", difftime(Sys.time(), start_time, units = "secs")))
  #start_time <- Sys.time()
  # move function
  move_tc(input, current_solution, changed_routes, type_problem, vecinity, perc_vecinity, 
                  penalty_capacity, check_feas)  
  
  
  #print(paste0("   move_tc ", difftime(Sys.time(), start_time, units = "secs")))
  #start_time <- Sys.time()
  
  # move function
  move_vc(input, current_solution, changed_routes, type_problem, vecinity, perc_vecinity, 
                  penalty_capacity, check_feas) 
  
  #print(paste0("   move_vc ", difftime(Sys.time(), start_time, units = "secs")))
  #print(paste0("TOTAL TIME -> ", difftime(Sys.time(), start_time1, units = "secs")))
  
}



insert_selected_mov<-function(input, mov, current_solution, type_problem){
  
  if (length(mov$route2)>1) {
    
    #print(paste0("R ->  indexr1 ", mov$indexr1, " indexr2 ", mov$indexr2))
    
    # first  modification
    current_solution <- insert_element_in_solution(input, mov$route1, mov$indexr1, current_solution, type_problem)
    
    # second modification
    current_solution <- insert_element_in_solution(input, mov$route2, mov$indexr2, current_solution, type_problem)
    
  } else {
    
    #print(paste0("R ->  indexr1 ", mov$indexr1))
    
    # modification
    current_solution <- insert_element_in_solution(input, mov$route1, mov$indexr1, current_solution, type_problem)
    
  }
  
  #readline()
  
  return(current_solution)
}


insert_selected_mov_new<-function(input, movid, current_solution, type_problem){
  
  if (length(movid$route2)>1) {
    reconf1 <- 0
    reconf2 <- 0
    if ((type_problem == "MCTTRP")&&(movid$opt_reconf==1)) {
      reconf1 <- 1
      reconf2 <- 0
    }
    if ((type_problem == "MCTTRP")&&(movid$opt_reconf==2)) {
      reconf1 <- 0
      reconf2 <- 1
    }
    if ((type_problem == "MCTTRP")&&(movid$opt_reconf==3)) {
      reconf1 <- 1
      reconf2 <- 1
    }
    
    # first  modification
    current_solution <- insert_element_in_solution(input, movid$route1, movid$indexr1, current_solution, type_problem, reconf1)
    
    # second modification
    current_solution <- insert_element_in_solution(input, movid$route2, movid$indexr2, current_solution, type_problem, reconf2)
    
  } else {
    reconf <- 0
    if ((type_problem == "MCTTRP")&&(movid$opt_reconf)) reconf <- 1
    
    # modification
    current_solution <- insert_element_in_solution(input, movid$route1, movid$indexr1, current_solution, type_problem, reconf)
    
  }
  
  return(current_solution)
}


insert_element_in_solution<-function(input, new_route, pos, current_solution, type_problem, reconf){
  
  # first modification
  all_vc <- 1
  subroutes <- 0
  old_solution <- current_solution
  type_root1 <- current_solution[[pos]]$type
  for (i in 2:(length(new_route)-1)) {
    if (new_route[i] >  input$n1) all_vc <- 0
    if (sum(new_route==new_route[i])>1) subroutes <- 1
  }
  
  # determine new type
  if ((all_vc)&&(!subroutes)&& (type_root1 == "CVR")) type_root1 <- "PVR"
  if ((!all_vc)&&(!subroutes)&&(type_root1 == "CVR")) type_root1 <- "PTR"
  else if (subroutes) type_root1 <- "CVR"
  
  current_solution[[pos]]$route <- new_route
  current_solution[[pos]]$type <-  type_root1
  
  
  
  if (type_root1 == "CVR") {
    current_solution[[pos]]$main_tour <- return_main_route(current_solution[[pos]]$route)
    current_solution[[pos]]$subtours <- return_subroutes(current_solution[[pos]]$route, input$n1)
  } else {
    current_solution[[pos]]$main_tour <- NULL
    current_solution[[pos]]$subtours <- NULL
  }
  if (type_problem == "TTRP") {
    current_solution[[pos]]$total_load <- calc_load2(current_solution[[pos]]$route, input$vector.demandas)
    current_solution[[pos]]$total_load_tc_clients <- calc_load_only_truck(current_solution[[pos]]$route, input$vector.demandas, input)
  }
  if (type_problem == "MCTTRP") {
    current_solution[[pos]]$total_load <- calc_load2_MC(current_solution[[pos]]$route, input$matriz.demandas)
    current_solution[[pos]]$total_load_tc_clients <- calc_load_only_truck_MC(current_solution[[pos]]$route, input$matriz.demandas, input)
  }
  current_solution[[pos]]$cost <- local_cost(current_solution[[pos]]$route, input$matriz.distancia)
  if (type_problem == "MCTTRP") {
    if (reconf == 0) {
      if (type_root1 == "PTR") res_r <- insert_hoppers_MCTTRP_PTR       (current_solution[[pos]], old_solution[[pos]], input)
      if (type_root1 == "PVR") res_r <- insert_hoppers_MCTTRP_PVR_update(current_solution[[pos]]$route, old_solution[[pos]], input)
      if (type_root1 == "CVR") res_r <- insert_hoppers_MCTTRP_CVR_update(current_solution[[pos]]$route, old_solution[[pos]], input)
    } else {
      if (type_root1 == "PTR") res_r <- insert_hoppers_MCTTRP_PTR       (current_solution[[pos]], old_solution[[pos]], input)
      if (type_root1 == "PVR") res_r <- insert_hoppers_MCTTRP_PVR_new(current_solution[[pos]], old_solution[[pos]], input)
      if (type_root1 == "CVR") res_r <- insert_hoppers_MCTTRP_CVR_new(current_solution[[pos]], old_solution[[pos]], input)
    }
    current_solution[[pos]]$clients_tc <- res_r$clients_tc
    current_solution[[pos]]$clients_vc <- res_r$clients_vc
    current_solution[[pos]]$used_hoppers_truck <- res_r$used_hoppers_truck
    current_solution[[pos]]$used_hoppers_trailer <- res_r$used_hoppers_trailer
  }
  

  return(current_solution)
}

calc_root_tc_client<-function(result_i, client_i, input) {
  if(result_i$type == "CVR") {
    subroutes <- return_subroutes(result_i$route, input$n1)
    for (i in 1:length(subroutes)) {
      subroutes_i <-  subroutes[[i]]$tour[2:(length(subroutes[[i]]$tour)-1)]
      if (sum(subroutes_i == client_i)>0) {
        rooti <- subroutes_i[1]
        break
      }
    }
  }
  else rooti <- 0
  
  return(rooti)
}

calc_root_vc_client<-function(result_i, client_i, input) {
  if(result_i$type == "CVR") {
    main_root <- return_main_route(result_i$route)
    if (sum(main_root == client_i)>0) {
      rooti <- 0
    } else  {
      subroutes <- return_subroutes(result_i$route, input$n1)
      for (i in 1:length(subroutes)) {
        subroutes_i <-  subroutes[[i]]$tour[2:(length(subroutes[[i]]$tour)-1)]
        if (sum(subroutes_i == client_i)>0) {
          rooti <- subroutes_i[1]
          break
        }
      } 
    }
  }
  else rooti <- 0
  
  return(rooti)
}

exchange_tc_two_routes<-function(input, result, changed_routes, type_problem, vecinity, perc_vecinity, 
                                 penalty_capacity, check_feas) {
  
  for (i in 1:(length(result)-1)) {
    if (result[[i]]$type != "PVR") {
      for (j in 2:(length(result[[i]]$route)-1)) {
        if ( sum(result[[i]]$route == result[[i]]$route[j]) == 1 ) {
          clienti <- result[[i]]$route[j]
          if (clienti > input$n1) {
            rooti <- calc_root_tc_client(result[[i]], clienti, input) 
            for (w in (i+1):length(result)) {
              if ((result[[w]]$type != "PVR")&&(i!=w)&&((w %in% changed_routes)||(i %in% changed_routes))) {
                for (t in 2:(length(result[[w]]$route)-1)) {
                  if ( sum(result[[w]]$route == result[[w]]$route[t]) == 1 ) {
                    clientw <- result[[w]]$route[t]
                    if (clientw > input$n1) {
                      rootw <- calc_root_tc_client(result[[w]], clientw, input)
                      # create routes  
                      route1 <- replace_route_client(clienti, clientw, result[[i]]$route)
                      route2 <- replace_route_client(clientw, clienti, result[[w]]$route)
                      # feasibility
                      feasible_route1 <- 1
                      feasible_route2 <- 1
                      if (check_feas) {
                        feasible_route1 <- check_feasibility(result[[i]], route1, input, result[[i]]$type, type_problem, penalty_capacity, "update") 
                        feasible_route2 <- check_feasibility(result[[w]], route2, input, result[[w]]$type, type_problem, penalty_capacity, "update") 
                      }
                      # add to mov list
                      if (feasible_route1 && feasible_route2) {
                        if (type_problem == "TTRP") {
                          
                          .Call('insertMovsList', as.integer(i), as.integer(w), 
                                as.integer(c(clienti)), as.integer(c(clientw)), 
                                as.integer(c(route1)), as.integer(c(route2)), 
                                "exchange_tc_two_routes", as.integer(rooti), as.integer(rootw), 
                                PACKAGE = "mcttrpalns")
                        }
                        
                        
                        if (type_problem == "MCTTRP") {
                          res1 <- check_capacity_hoppers_MCTTRP_return_hoppers(route1, result[[i]], input)
                          res2 <- check_capacity_hoppers_MCTTRP_return_hoppers(route2, result[[w]], input)
                          hoppers_truck1 <- res1$hoppers_truck
                          hoppers_trailer1 <- res1$hoppers_trailer
                          hoppers_truck2 <- res2$hoppers_truck                          
                          hoppers_trailer2 <- res2$hoppers_trailer
                          
                          .Call('insertMovsList_MCTTRP', as.integer(i), as.integer(w), 
                                as.integer(c(clienti)), as.integer(c(clientw)), 
                                as.integer(c(route1)), as.integer(c(route2)), 
                                "exchange_tc_two_routes", as.integer(rooti), as.integer(rootw), 
                                as.integer(hoppers_truck1), as.integer(hoppers_trailer1), as.integer(hoppers_truck2), as.integer(hoppers_trailer2), 
                                as.integer(0),
                                PACKAGE = "mcttrpalns")
                        }  
                      } 
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
  }
  
  
}


move_tc<-function(input, result, changed_routes, type_problem, vecinity, perc_vecinity, 
                  penalty_capacity, check_feas) {
  
  for (i in 1:(length(result)-1)) {
    if ((result[[i]]$type != "PVR") && (length(result[[i]]$route)>3)) {
      for (j in 2:(length(result[[i]]$route)-1)) {
        # is not a depot and is a tc
        if (( sum(result[[i]]$route == result[[i]]$route[j]) == 1 ) &&  (result[[i]]$route[j] > input$n1)) {
          clienti <- result[[i]]$route[j]
          rooti <- calc_root_tc_client(result[[i]], clienti, input) 
          rootw <- -1
          for (w in (1:length(result))) {
            if ((i!=w)&&((w %in% changed_routes)||(i %in% changed_routes))) {
              # subroute
              if (result[[w]]$type == "CVR") {
                # tc in subroute
                subroutes <- return_subroutes(result[[w]]$route, input$n1)
                for (s in 1:length(subroutes)) {
                  for (t in 1:(length(subroutes[[s]]$tour)-1)) {
                    # create routes 
                    route1 <- delete_node_in_route(j, result[[i]]$route)
                    route2 <- add_node_in_route(which(subroutes[[s]]$tour[t]==result[[w]]$route), clienti, result[[w]]$route)
                    # feasibility
                    feasible_route1 <- 1
                    feasible_route2 <- 1
                    if (check_feas) {
                      if ( result[[i]]$type == "CVR" && (sum(duplicated(route1[2:(length(route1)-1)]))==0) ) opt_check <- "new"
                      else opt_check <- "update"
                      feasible_route1 <- check_feasibility(result[[i]], route1, input, result[[i]]$type, type_problem, penalty_capacity, opt_check) 
                      feasible_route2 <- check_feasibility(result[[w]], route2, input, result[[w]]$type, type_problem, penalty_capacity, "update") 
                    }
                    # add to mov list
                    if (feasible_route1 && feasible_route2) {

                      if (type_problem == "TTRP") {
                        
                        .Call('insertMovsList', as.integer(i), as.integer(w), 
                              as.integer(c(clienti)), as.integer(0), 
                              as.integer(c(route1)), as.integer(c(route2)), 
                              "move_tc", as.integer(rooti), as.integer(rootw), 
                              PACKAGE = "mcttrpalns")
                        
                      }
                      
                      if (type_problem == "MCTTRP") {
                        if ( result[[i]]$type == "CVR" && (sum(duplicated(route1[2:(length(route1)-1)]))==0) ) opt_check <- 1
                        else opt_check <- 0
                        
                        res1 <- check_capacity_hoppers_MCTTRP_return_hoppers(route1, result[[i]], input)
                        res2 <- check_capacity_hoppers_MCTTRP_return_hoppers(route2, result[[w]], input)
                        hoppers_truck1 <- res1$hoppers_truck
                        hoppers_trailer1 <- res1$hoppers_trailer
                        hoppers_truck2 <- res2$hoppers_truck                          
                        hoppers_trailer2 <- res2$hoppers_trailer
                        
                        .Call('insertMovsList_MCTTRP',  as.integer(i), as.integer(w), 
                              as.integer(c(clienti)), as.integer(0), 
                              as.integer(c(route1)), as.integer(c(route2)), 
                              "move_tc", as.integer(rooti), as.integer(rootw), 
                              as.integer(hoppers_truck1), as.integer(hoppers_trailer1), as.integer(hoppers_truck2), as.integer(hoppers_trailer2), 
                              as.integer(opt_check),
                              PACKAGE = "mcttrpalns")
                      }
                    } 
                  }
                }
                # tc in main route
                main_root <- return_main_route(result[[w]]$route)
                for (t in 2:(length(main_root)-1)) {
                  if (sum(main_root[t] == result[[w]]$route)==1) {
                    pos1 <-  which(main_root[t]==result[[w]]$route)
                  } 
                  if (sum(main_root[t] == result[[w]]$route)>1) {
                    pos1 <-  which(main_root[t]==result[[w]]$route)
                    pos1 <- pos1[length(pos1)]
                  } 
                  # create routes 
                  route1 <- delete_node_in_route(j, result[[i]]$route)
                  route2 <- add_subroute_mov(pos1, clienti, result[[w]]$route)
                  # feasibility
                  feasible_route1 <- 1
                  feasible_route2 <- 1
                  if (check_feas) {
                    if ( result[[i]]$type == "CVR" && (sum(duplicated(route1[2:(length(route1)-1)]))==0) ) opt_check <- "new"
                    else opt_check <- "update"
                    feasible_route1 <- check_feasibility(result[[i]], route1, input, result[[i]]$type, type_problem, penalty_capacity, opt_check) 
                    feasible_route2 <- check_feasibility(result[[w]], route2, input, result[[w]]$type, type_problem, penalty_capacity, "update") 
                  }
                  # add to mov list
                  if (feasible_route1 && feasible_route2) {
                    if (type_problem == "TTRP") {
                      
                      .Call('insertMovsList', as.integer(i), as.integer(w), 
                            as.integer(c(clienti)), as.integer(0), 
                            as.integer(c(route1)), as.integer(c(route2)), 
                            "move_tc", as.integer(rooti), as.integer(rootw), 
                            PACKAGE = "mcttrpalns")
                      
                    }
                    
                    if (type_problem == "MCTTRP") {
                      if ( result[[i]]$type == "CVR" && (sum(duplicated(route1[2:(length(route1)-1)]))==0) ) opt_check <- 1
                      else opt_check <- 0
                      
                      res1 <- check_capacity_hoppers_MCTTRP_return_hoppers(route1, result[[i]], input)
                      res2 <- check_capacity_hoppers_MCTTRP_return_hoppers(route2, result[[w]], input)
                      hoppers_truck1 <- res1$hoppers_truck
                      hoppers_trailer1 <- res1$hoppers_trailer
                      hoppers_truck2 <- res2$hoppers_truck                          
                      hoppers_trailer2 <- res2$hoppers_trailer
                      
                      .Call('insertMovsList_MCTTRP',  as.integer(i), as.integer(w), 
                            as.integer(c(clienti)), as.integer(0), 
                            as.integer(c(route1)), as.integer(c(route2)), 
                            "move_tc", as.integer(rooti), as.integer(rootw), 
                            as.integer(hoppers_truck1), as.integer(hoppers_trailer1), as.integer(hoppers_truck2), as.integer(hoppers_trailer2), 
                            as.integer(opt_check),
                            PACKAGE = "mcttrpalns")
                    }
                  } 
                }
              }
              # PTR
              else if (result[[w]]$type == "PTR") {
                for (t in 1:(length(result[[w]]$route)-1)) {
                  # create routes 
                  route1 <- delete_node_in_route(j, result[[i]]$route)
                  route2 <- add_node_in_route(t, clienti, result[[w]]$route)
                  # feasibility
                  feasible_route1 <- 1
                  feasible_route2 <- 1
                  if (check_feas) {
                    if ( result[[i]]$type == "CVR" && (sum(duplicated(route1[2:(length(route1)-1)]))==0) ) opt_check <- "new"
                    else opt_check <- "update"
                    feasible_route1 <- check_feasibility(result[[i]], route1, input, result[[i]]$type, type_problem, penalty_capacity, opt_check) 
                    feasible_route2 <- check_feasibility(result[[w]], route2, input, result[[w]]$type, type_problem, penalty_capacity, "update") 
                  }
                  # add to mov list
                  if (feasible_route1 && feasible_route2) {
                    if (type_problem == "TTRP") {
                      
                      .Call('insertMovsList', as.integer(i), as.integer(w), 
                            as.integer(c(clienti)), as.integer(0), 
                            as.integer(c(route1)), as.integer(c(route2)), 
                            "move_tc", as.integer(rooti), as.integer(rootw), 
                            PACKAGE = "mcttrpalns")
                      
                    }
                    
                    if (type_problem == "MCTTRP") {
                      if ( result[[i]]$type == "CVR" && (sum(duplicated(route1[2:(length(route1)-1)]))==0) ) opt_check <- 1
                      else opt_check <- 0
                      
                      res1 <- check_capacity_hoppers_MCTTRP_return_hoppers(route1, result[[i]], input)
                      res2 <- check_capacity_hoppers_MCTTRP_return_hoppers(route2, result[[w]], input)
                      hoppers_truck1 <- res1$hoppers_truck
                      hoppers_trailer1 <- res1$hoppers_trailer
                      hoppers_truck2 <- res2$hoppers_truck                          
                      hoppers_trailer2 <- res2$hoppers_trailer
                      
                      .Call('insertMovsList_MCTTRP',  as.integer(i), as.integer(w), 
                            as.integer(c(clienti)), as.integer(0), 
                            as.integer(c(route1)), as.integer(c(route2)), 
                            "move_tc", as.integer(rooti), as.integer(rootw), 
                            as.integer(hoppers_truck1), as.integer(hoppers_trailer1), as.integer(hoppers_truck2), as.integer(hoppers_trailer2), 
                            as.integer(opt_check),
                            PACKAGE = "mcttrpalns")
                    }
                  } 
                }
              }
              # PVR
              else {
                for (t in 2:(length(result[[w]]$route)-1)) {
                  # create routes 
                  route1 <- delete_node_in_route(j, result[[i]]$route)
                  route2 <- add_subroute_mov(t, clienti, result[[w]]$route)
                  
                  # feasibility
                  feasible_route1 <- 1
                  feasible_route2 <- 1
                  if (check_feas) {
                    if ( result[[i]]$type == "CVR" && (sum(duplicated(route1[2:(length(route1)-1)]))==0) ) opt_check <- "new"
                    else opt_check <- "update"
                    feasible_route1 <- check_feasibility(result[[i]], route1, input, result[[i]]$type, type_problem, penalty_capacity, opt_check) 
                    feasible_route2 <- check_feasibility(result[[w]], route2, input, result[[w]]$type, type_problem, penalty_capacity, "update") 
                  }
                  # add to mov list
                  if (feasible_route1 && feasible_route2) {
                    if (type_problem == "TTRP") {
                      
                      .Call('insertMovsList', as.integer(i), as.integer(w), 
                            as.integer(c(clienti)), as.integer(0), 
                            as.integer(c(route1)), as.integer(c(route2)), 
                            "move_tc", as.integer(rooti), as.integer(rootw), 
                            PACKAGE = "mcttrpalns")
                      
                    }
                    
                    if (type_problem == "MCTTRP") {
                      if ( result[[i]]$type == "CVR" && (sum(duplicated(route1[2:(length(route1)-1)]))==0) ) opt_check <- 1
                      else opt_check <- 0
                      
                      res1 <- check_capacity_hoppers_MCTTRP_return_hoppers(route1, result[[i]], input)
                      res2 <- check_capacity_hoppers_MCTTRP_return_hoppers(route2, result[[w]], input)
                      hoppers_truck1 <- res1$hoppers_truck
                      hoppers_trailer1 <- res1$hoppers_trailer
                      hoppers_truck2 <- res2$hoppers_truck                          
                      hoppers_trailer2 <- res2$hoppers_trailer
                      
                      .Call('insertMovsList_MCTTRP',  as.integer(i), as.integer(w), 
                            as.integer(c(clienti)), as.integer(0), 
                            as.integer(c(route1)), as.integer(c(route2)), 
                            "move_tc", as.integer(rooti), as.integer(rootw), 
                            as.integer(hoppers_truck1), as.integer(hoppers_trailer1), as.integer(hoppers_truck2), as.integer(hoppers_trailer2), 
                            as.integer(opt_check),
                            PACKAGE = "mcttrpalns")
                    }
                  } 
                }
              }
            }
          }
          
        } 
      }
    }
  }
  
  
}


move_vc<-function(input, result, changed_routes, type_problem, vecinity, perc_vecinity, 
                  penalty_capacity, check_feas) {
  
  for (i in 1:(length(result)-1)) {
    if ((length(result[[i]]$route)>3)) {
      for (j in 2:(length(result[[i]]$route)-1)) {
        # is not a depot and is a vc
        if (( sum(result[[i]]$route == result[[i]]$route[j]) == 1 ) &&  (result[[i]]$route[j] <= input$n1)) {
          clienti <- result[[i]]$route[j]
          rooti <- calc_root_vc_client(result[[i]], clienti, input)
          rootw <- -1
          for (w in (1:length(result))) {
            if ((i!=w)&&((w %in% changed_routes)||(i %in% changed_routes))) {
              if (result[[w]]$type == "CVR") main_root <- return_main_route(result[[w]]$route)
              else main_root <- result[[w]]$route
              for (t in 1:(length(main_root)-1)) {
                if (sum(main_root[t] == result[[w]]$route)==1) {
                  # create routes 
                  route1 <- delete_node_in_route(j, result[[i]]$route)
                  route2 <- add_node_in_route(which(main_root[t]==result[[w]]$route), clienti, result[[w]]$route)
                  
                  # feasibility
                  feasible_route1 <- 1
                  feasible_route2 <- 1
                  if (check_feas) {
                    if ( result[[i]]$type == "CVR" && (sum(duplicated(route1[2:(length(route1)-1)]))==0) ) opt_check <- "new"
                    else opt_check <- "update"
                    feasible_route1 <- check_feasibility(result[[i]], route1, input, result[[i]]$type, type_problem, penalty_capacity, opt_check) 
                    feasible_route2 <- check_feasibility(result[[w]], route2, input, result[[w]]$type, type_problem, penalty_capacity, "update") 
                  }
                  # add to mov list
                  if (feasible_route1 && feasible_route2) {
                    
                    if (type_problem == "TTRP") {
                      
                      .Call('insertMovsList', as.integer(i), as.integer(w), 
                            as.integer(c(clienti)), as.integer(0), 
                            as.integer(c(route1)), as.integer(c(route2)), 
                            "move_vc", as.integer(rooti), as.integer(rootw), 
                            PACKAGE = "mcttrpalns")
                      
                    }
                    
                    if (type_problem == "MCTTRP") {
                      if ( result[[i]]$type == "CVR" && (sum(duplicated(route1[2:(length(route1)-1)]))==0) ) opt_check <- 1
                      else opt_check <- 0
                      
                      res1 <- check_capacity_hoppers_MCTTRP_return_hoppers(route1, result[[i]], input)
                      res2 <- check_capacity_hoppers_MCTTRP_return_hoppers(route2, result[[w]], input)
                      hoppers_truck1 <- res1$hoppers_truck
                      hoppers_trailer1 <- res1$hoppers_trailer
                      hoppers_truck2 <- res2$hoppers_truck                          
                      hoppers_trailer2 <- res2$hoppers_trailer
                      
                      .Call('insertMovsList_MCTTRP',  as.integer(i), as.integer(w), 
                            as.integer(c(clienti)), as.integer(0), 
                            as.integer(c(route1)), as.integer(c(route2)), 
                            "move_vc", as.integer(rooti), as.integer(rootw), 
                            as.integer(hoppers_truck1), as.integer(hoppers_trailer1), as.integer(hoppers_truck2), as.integer(hoppers_trailer2), 
                            as.integer(opt_check),
                            PACKAGE = "mcttrpalns")
                    }
                  
                  } 
                }
              }
            }
          }
          
        } 
      }
    }
  }
  

}

exchange_vc_two_routes<-function(input, result, changed_routes, type_problem, vecinity,
                                 perc_vecinity, penalty_capacity, check_feas) {
  
  for (i in 1:(length(result)-1)) {
    
    for (j in 2:(length(result[[i]]$route)-1)) {
      if ( sum(result[[i]]$route == result[[i]]$route[j]) == 1 ) {
        clienti <- result[[i]]$route[j]
        if (clienti <= input$n1) {
          rooti <- calc_root_vc_client(result[[i]], clienti, input)
          for (w in (i+1):length(result)) {
            if ((w %in% changed_routes)||(i %in% changed_routes)) {
              for (t in 2:(length(result[[w]]$route)-1)) {
                if ((i!=w)&&(sum(result[[w]]$route == result[[w]]$route[t]) == 1)) {
                  clientw <- result[[w]]$route[t]
                  if ((clientw <= input$n1)) {
                    rootw <- calc_root_vc_client(result[[w]], clientw, input)
                    
                    # create routes  
                    route1 <- replace_route_client(clienti, clientw, result[[i]]$route)
                    route2 <- replace_route_client(clientw, clienti, result[[w]]$route)
                    
                    # feasibility
                    feasible_route1 <- 1
                    feasible_route2 <- 1
                    if (check_feas) {
                      feasible_route1 <- check_feasibility(result[[i]], route1, input, result[[i]]$type, type_problem, penalty_capacity, "update") 
                      feasible_route2 <- check_feasibility(result[[w]], route2, input, result[[w]]$type, type_problem, penalty_capacity, "update") 
                    }
                    
                    # add to mov list
                    if (feasible_route1 && feasible_route2) {

                      if (type_problem == "TTRP") {
                        
                        .Call('insertMovsList', as.integer(i), as.integer(w), 
                              as.integer(c(clienti)), as.integer(clientw), 
                              as.integer(c(route1)), as.integer(c(route2)), 
                              "exchange_vc_two_routes", as.integer(rooti), as.integer(rootw), 
                              PACKAGE = "mcttrpalns")
                        
                      }
                      
                      if (type_problem == "MCTTRP") {
                        res1 <- check_capacity_hoppers_MCTTRP_return_hoppers(route1, result[[i]], input)
                        res2 <- check_capacity_hoppers_MCTTRP_return_hoppers(route2, result[[w]], input)
                        hoppers_truck1 <- res1$hoppers_truck
                        hoppers_trailer1 <- res1$hoppers_trailer
                        hoppers_truck2 <- res2$hoppers_truck                          
                        hoppers_trailer2 <- res2$hoppers_trailer
                        
                        .Call('insertMovsList_MCTTRP',  as.integer(i), as.integer(w), 
                              as.integer(c(clienti)), as.integer(clientw), 
                              as.integer(c(route1)), as.integer(c(route2)), 
                              "exchange_vc_two_routes", as.integer(rooti), as.integer(rootw), 
                              as.integer(hoppers_truck1), as.integer(hoppers_trailer1), as.integer(hoppers_truck2), as.integer(hoppers_trailer2), 
                              as.integer(0),
                              PACKAGE = "mcttrpalns")
                      }
                    } 
                  }
                }
              }
            }
          }
        }
      }
    }
    
  }
  
  #return(movs)
  
}


exchange_ptr_and_subtour<-function(input, result, changed_routes, type_problem, penalty_capacity,
                                   check_feas) {
  
  for (i in 1:length(result)) {
    if (result[[i]]$type == "PTR") {
      subroutei <- result[[i]]$route
      rooti <- 0
      for (z in 1:length(result)){
        if ((result[[z]]$type == "CVR")&&((z %in% changed_routes)||(i %in% changed_routes))) {
          if (i!=z) {
            subroutes <- return_subroutes(result[[z]]$route, input$n1)
            for (s in 1:length(subroutes)) {
              subroutez <- subroutes[[s]]$tour
              rootz <- subroutes[[s]]$tour[1]
              
              route1 <- c(0, subroutez[2:(length(subroutez)-1)], 0)
              route2 <- replace_subroute(subroutez, subroutei, result[[z]]$route)
              
              # feasibility
              feasible_route1 <- 1
              feasible_route2 <- 1
              if (check_feas) {
                feasible_route1 <- check_feasibility(result[[i]], route1, input, result[[i]]$type, type_problem, penalty_capacity, "update") 
                feasible_route2 <- check_feasibility(result[[z]], route2, input, result[[z]]$type, type_problem, penalty_capacity, "update") 
              }
              # add to mov list
              if (feasible_route1 && feasible_route2) {

                if (type_problem == "TTRP") {
                  
                  .Call('insertMovsList', as.integer(i), as.integer(z), 
                        as.integer(subroutei[2:(length(subroutei)-1)]), 
                        as.integer(subroutez[2:(length(subroutez)-1)]), 
                        as.integer(c(route1)), as.integer(c(route2)), 
                        "exchange_ptr_and_subtour", as.integer(rooti), as.integer(rootz),
                        PACKAGE = "mcttrpalns")
                  
                }
                
                if (type_problem == "MCTTRP") {
                  res1 <- check_capacity_hoppers_MCTTRP_return_hoppers(route1, result[[i]], input)
                  res2 <- check_capacity_hoppers_MCTTRP_return_hoppers(route2, result[[z]], input)
                  hoppers_truck1 <- res1$hoppers_truck
                  hoppers_trailer1 <- res1$hoppers_trailer
                  hoppers_truck2 <- res2$hoppers_truck                          
                  hoppers_trailer2 <- res2$hoppers_trailer
                  
                  .Call('insertMovsList_MCTTRP',  as.integer(i), as.integer(z), 
                        as.integer(subroutei[2:(length(subroutei)-1)]), 
                        as.integer(subroutez[2:(length(subroutez)-1)]), 
                        as.integer(c(route1)), as.integer(c(route2)), 
                        "exchange_ptr_and_subtour", as.integer(rooti), as.integer(rootz),
                        as.integer(hoppers_truck1), as.integer(hoppers_trailer1), as.integer(hoppers_truck2), as.integer(hoppers_trailer2), 
                        as.integer(0),
                        PACKAGE = "mcttrpalns")
                }
              } 
            }
          }
        }
      }
    }
  }
  
  #return(movs)
  
}


move_vc_client_subroute_to_main_tour_and_split_same_route<-function(input, result, changed_routes, type_problem, 
                                                                    penalty_capacity, check_feas) {
  
  for (i in 1:length(result)) {
    if ((result[[i]]$type == "CVR")&&(i %in% changed_routes)) {
      subroutes <- return_subroutes(result[[i]]$route, input$n1)
      for (s in 1:length(subroutes)) {
        for (j in 2:(length(subroutes[[s]]$tour)-1)) {
          clienti <- subroutes[[s]]$tour[j]
          rooti <- subroutes[[s]]$tour[1]
          rootw <- -1
          prev <- subroutes[[s]]$tour[j-1]
          post <- subroutes[[s]]$tour[j+1]
          if ((clienti <= input$n1) && (sum(prev==subroutes[[s]]$tour)==1) &&(sum(post==subroutes[[s]]$tour)==1)){
            
            route1 <- split_subroute(clienti, result[[i]]$route)
            
            # feasibility
            feasible_route1 <- 1
            if (check_feas) {
              feasible_route1 <- check_feasibility(result[[i]], route1, input, result[[i]]$type, type_problem, penalty_capacity, "new") 
            }
            # add to mov list
            if (feasible_route1) {

              if (type_problem == "TTRP") {
                
                .Call('insertMovsList', as.integer(i), as.integer(0), 
                      as.integer(clienti), as.integer(0), 
                      as.integer(c(route1)), as.integer(c(0)), 
                      "move_vc_client_subroute_to_main_tour_and_split_same_route", as.integer(rooti), as.integer(rootw),
                      PACKAGE = "mcttrpalns")
                
              }
              
              if (type_problem == "MCTTRP") {
                res1 <- check_capacity_hoppers_MCTTRP_return_hoppers_new(route1, result[[i]], input)
                hoppers_truck1 <- res1$hoppers_truck
                hoppers_trailer1 <- res1$hoppers_trailer
                hoppers_truck2 <- 0                         
                hoppers_trailer2 <- 0
                
                .Call('insertMovsList_MCTTRP',  as.integer(i), as.integer(0), 
                      as.integer(clienti), as.integer(0), 
                      as.integer(c(route1)), as.integer(c(0)), 
                      "move_vc_client_subroute_to_main_tour_and_split_same_route", as.integer(rooti), as.integer(rootw),
                      as.integer(hoppers_truck1), as.integer(hoppers_trailer1), as.integer(hoppers_truck2), as.integer(hoppers_trailer2),  
                      as.integer(1),
                      PACKAGE = "mcttrpalns")
              }
            } 
            
          }
        }
      }
    }
    
  }
  
  #return(movs)
  
}


move_subroute<-function(input, result, changed_routes, type_problem, penalty_capacity, check_feas) {
  
  for (i in 1:length(result)) {
    if (result[[i]]$type == "CVR") {
      subroutes <- return_subroutes(result[[i]]$route, input$n1)
      for (s in 1:length(subroutes))  {
        subroutei <- subroutes[[s]]$tour
        rooti <- subroutes[[s]]$tour[1]
        rootw <- -1
        for (z in 1:length(result)) {
          if ((i!=z)&&(result[[z]]$type != "PTR")&&((i %in% changed_routes)||(z %in% changed_routes))) {
            if (result[[z]]$type == "CVR") main_root <- return_main_route(result[[z]]$route)
            else main_root <- result[[z]]$route
            for (t in 2:(length(main_root)-1)) {
              if ((main_root[t]<= input$n1)&&(i!=z)){
                clientw <- main_root[t]
                
                # routes
                route1 <- delete_subroute(subroutei, result[[i]]$route)
                route2 <- add_subroute(clientw, subroutei, result[[z]]$route)
                
                # feasibility
                feasible_route1 <- 1
                feasible_route2 <- 1
                if (check_feas) {  
                  if ( result[[i]]$type == "CVR" && (sum(duplicated(route1[2:(length(route1)-1)]))==0) ) opt_check <- "new"
                  else opt_check <- "update"
                  feasible_route1 <- check_feasibility(result[[i]], route1, input, result[[i]]$type, type_problem, penalty_capacity, opt_check) 
                  feasible_route2 <- check_feasibility(result[[z]], route2, input, result[[z]]$type, type_problem, penalty_capacity, "update") 
                }
                # add to mov list
                if (feasible_route1 && feasible_route2) {
                  
                  if (type_problem == "TTRP") {
                    
                    .Call('insertMovsList', as.integer(i), as.integer(z), 
                          as.integer(subroutei[2:(length(subroutei)-1)]), as.integer(0), 
                          as.integer(c(route1)), as.integer(c(route2)), 
                          "move_subroute", as.integer(rooti), as.integer(rootw),
                          PACKAGE = "mcttrpalns")
                    
                  }
                  
                  if (type_problem == "MCTTRP") {
                    if ( result[[i]]$type == "CVR" && (sum(duplicated(route1[2:(length(route1)-1)]))==0) ) opt_check <- 1
                    else opt_check <- 0
                    res1 <- check_capacity_hoppers_MCTTRP_return_hoppers(route1, result[[i]], input)
                    res2 <- check_capacity_hoppers_MCTTRP_return_hoppers(route2, result[[z]], input)
                    hoppers_truck1 <- res1$hoppers_truck
                    hoppers_trailer1 <- res1$hoppers_trailer
                    hoppers_truck2 <- res2$hoppers_truck                          
                    hoppers_trailer2 <- res2$hoppers_trailer
                    
                    .Call('insertMovsList_MCTTRP',  as.integer(i), as.integer(z), 
                          as.integer(subroutei[2:(length(subroutei)-1)]), as.integer(0), 
                          as.integer(c(route1)), as.integer(c(route2)), 
                          "move_subroute", as.integer(rooti), as.integer(rootw),
                          as.integer(hoppers_truck1), as.integer(hoppers_trailer1), as.integer(hoppers_truck2), as.integer(hoppers_trailer2), 
                          as.integer(opt_check),
                          PACKAGE = "mcttrpalns")
                  }
                  
                } 
              }
            }
          }
        }
      }
    }
  }
  
  #return(movs)
  
}


move_subroute_same_route<-function(input, result, changed_routes, type_problem, penalty_capacity, 
                                   check_feas) {
  
  for (i in 1:length(result)) {
    if ((result[[i]]$type == "CVR")&&(i %in% changed_routes)) {
      subroutes <- return_subroutes(result[[i]]$route, input$n1)
      for (s in 1:length(subroutes))  {
        subroutei <- subroutes[[s]]$tour
        rooti <- subroutes[[s]]$tour[1]
        main_root <- return_main_route(result[[i]]$route)
        for (t in 2:(length(main_root)-1)) {
          if ((main_root[t]<= input$n1)&&(sum(main_root[t]==result[[i]]$route)==1)){
            clientw <- main_root[t]
            # routes
            route1 <- delete_subroute(subroutei, result[[i]]$route)
            route1 <- add_subroute(clientw, subroutei, route1)

            # feasibility
            feasible_route1 <- 1
            if (check_feas) {  
              feasible_route1 <- check_feasibility(result[[i]], route1, input, result[[i]]$type, type_problem, penalty_capacity, "new") 
            }
            # add to mov list
            if (feasible_route1) {
              
              if (type_problem == "TTRP") {
                
                .Call('insertMovsList', as.integer(i), as.integer(0), 
                      as.integer(subroutei[2:(length(subroutei)-1)]), as.integer(c(0)), 
                      as.integer(route1), as.integer(c(0)), 
                      "move_subroute_same_route", as.integer(rooti), as.integer(-1),
                      PACKAGE = "mcttrpalns")
                
              }
              
              if (type_problem == "MCTTRP") {
                res1 <- check_capacity_hoppers_MCTTRP_return_hoppers_new(route1, result[[i]], input)
                hoppers_truck1 <- res1$hoppers_truck
                hoppers_trailer1 <- res1$hoppers_trailer
                hoppers_truck2 <- 0                         
                hoppers_trailer2 <- 0
                
                .Call('insertMovsList_MCTTRP', as.integer(i), as.integer(0), 
                      as.integer(subroutei[2:(length(subroutei)-1)]), as.integer(c(0)), 
                      as.integer(route1), as.integer(c(0)), 
                      "move_subroute_same_route", as.integer(rooti), as.integer(-1),
                      as.integer(hoppers_truck1), as.integer(hoppers_trailer1), as.integer(hoppers_truck2), as.integer(hoppers_trailer2), 
                      as.integer(1),
                      PACKAGE = "mcttrpalns")
              }
            } 
          }
        }
      }
    }
  }
  
  #return(movs)
  
}


create_new_subtour_vc_same_route<-function(input, result, changed_routes, type_problem, 
                                           penalty_capacity, check_feas) {
  
  for (i in 1:length(result)) {
    if ((result[[i]]$type != "PTR")&&(i %in% changed_routes)) {
      if (result[[i]]$type == "CVR") main_root <- return_main_route(result[[i]]$route)
      else main_root <- result[[i]]$route
      for (t in 2:(length(main_root)-1)) {
        if ((main_root[t]<= input$n1)&&(sum(main_root[t]==result[[i]]$route)==1)){
          clienti <- main_root[t]
          rooti <- 0
          for (z in 2:(length(main_root)-1)) {
            if ((main_root[z]<= input$n1)&&(sum(main_root[z]==result[[i]]$route)==1)&&(main_root[z]!=main_root[t])){
              
              clientz <- main_root[z]
              # routes
              route1 <- delete_client(clientz, result[[i]]$route)
              route1 <- add_subroute(clienti, c(clienti, clientz, clienti), route1)
              # feasibility
              feasible_route1 <- 1
              if (check_feas) {  
                feasible_route1 <- check_feasibility(result[[i]], route1, input, result[[i]]$type, type_problem, penalty_capacity, "new") 
              }
              # add to mov list
              if (feasible_route1) {

                if (type_problem == "TTRP") {
                  
                  .Call('insertMovsList', as.integer(i), as.integer(0), 
                        as.integer(c(clientz)), as.integer(0), 
                        as.integer(c(route1)), as.integer(c(0)), 
                        "create_new_subtour_vc_unique", as.integer(rooti), as.integer(-1),
                        PACKAGE = "mcttrpalns")
                  
                }
                
                if (type_problem == "MCTTRP") {
                  res1 <- check_capacity_hoppers_MCTTRP_return_hoppers_new(route1, result[[i]], input)
                  hoppers_truck1 <- res1$hoppers_truck
                  hoppers_trailer1 <- res1$hoppers_trailer
                  hoppers_truck2 <- 0                         
                  hoppers_trailer2 <- 0
                  
                  .Call('insertMovsList_MCTTRP', as.integer(i), as.integer(0), 
                        as.integer(c(clientz)), as.integer(0), 
                        as.integer(c(route1)), as.integer(c(0)), 
                        "create_new_subtour_vc_unique", as.integer(rooti), as.integer(-1),
                        as.integer(hoppers_truck1), as.integer(hoppers_trailer1), as.integer(hoppers_truck2), as.integer(hoppers_trailer2), 
                        as.integer(1),
                        PACKAGE = "mcttrpalns")
                }
              } 
            }
          }
        }
      }
    }
  }
  
}


exchange_movement_change_parking<-function(input, result, changed_routes, type_problem, 
                                           vecinity, perc_vecinity, penalty_capacity, check_feas) {
  
  for (i in 1:length(result)) {
    if (result[[i]]$type == "CVR") {
      subroutes <- return_subroutes(result[[i]]$route, input$n1)
      for (s in 1:length(subroutes))  {
        clienti <- subroutes[[s]]$root
        rooti <- subroutes[[s]]$root
        if ( clienti <= input$n1 ) {
          for (z in 1:length(result)){
            if ((i!=z)&&(result[[z]]$type != "PTR")&&((z %in% changed_routes)||(i %in% changed_routes))) {
              if (result[[z]]$type == "CVR") main_root <- return_main_route(result[[z]]$route)
              else main_root <- result[[z]]$route
              for (t in 2:(length(main_root)-1)) {
                if (main_root[t] <= input$n1) {
                  clientz <- main_root[t]
                  rootz <- 0
                  if (( clientz <= input$n1 )) {
                    
                    route1 <- replace_subroute_vc(subroutes[[s]], clientz, result[[i]]$route)
                    route2 <- replace_route_client(clientz, clienti, result[[z]]$route)
                    route2 <- add_subroute(clienti, subroutes[[s]]$tour, route2)
                    
                    # feasibility
                    feasible_route1 <- 1
                    feasible_route2 <- 1
                    if (check_feas) {  
                      feasible_route1 <- check_feasibility(result[[i]], route1, input, result[[i]]$type, type_problem, penalty_capacity, "new") 
                      feasible_route2 <- check_feasibility(result[[z]], route2, input, result[[z]]$type, type_problem, penalty_capacity, "new") 
                    }
                    # add to mov list
                    if (feasible_route1 && feasible_route2) {
                      subroutei <- subroutes[[s]]$tour
                      
                      if (type_problem == "TTRP") {
                        
                        .Call('insertMovsList', as.integer(i), as.integer(z), 
                              as.integer(subroutei[2:(length(subroutei)-1)]), as.integer(0), 
                              as.integer(c(route1)), as.integer(c(route2)), 
                              "exchange_movement_change_parking", as.integer(rooti), as.integer(rootz), 
                              PACKAGE = "mcttrpalns")
                        
                      }
                      
                      if (type_problem == "MCTTRP") {
                        
                        res1 <- check_capacity_hoppers_MCTTRP_return_hoppers(route1, result[[i]], input)
                        res2 <- check_capacity_hoppers_MCTTRP_return_hoppers(route2, result[[z]], input)
                        hoppers_truck1 <- res1$hoppers_truck
                        hoppers_trailer1 <- res1$hoppers_trailer
                        hoppers_truck2 <- res2$hoppers_truck                          
                        hoppers_trailer2 <- res2$hoppers_trailer

                        .Call('insertMovsList_MCTTRP',  as.integer(i), as.integer(z), 
                              as.integer(subroutei[2:(length(subroutei)-1)]), as.integer(0), 
                              as.integer(c(route1)), as.integer(c(route2)), 
                              "exchange_movement_change_parking", as.integer(rooti), as.integer(rootz), 
                              as.integer(hoppers_truck1), as.integer(hoppers_trailer1), as.integer(hoppers_truck2), as.integer(hoppers_trailer2), 
                              as.integer(3),
                              PACKAGE = "mcttrpalns")

                      }
                    }   
                  }
                }
              }
            }
          }
        }
      }
    }
  }
  
  #return(movs)
}


exchange_movement_vc_main_tour_tc_subtour<-function(input, result, changed_routes, 
                                                    type_problem, vecinity, perc_vecinity, penalty_capacity, 
                                                    check_feas) {
  for (i in 1:length(result)) {
    if (result[[i]]$type != "PTR") {
      if (result[[i]]$type == "CVR") main_root <- return_main_route(result[[i]]$route)
      else main_root <- result[[i]]$route
      for (j in 2:(length(main_root)-1)) {
        clienti <- main_root[j]
        rooti <- 0
        # vc
        if (clienti <= input$n1) {
          for ( z in 1:length(result)) {
            if ((result[[z]]$type == "CVR") && (i!=z)&&((z %in% changed_routes)||(i %in% changed_routes))) {
              subroutes <- return_subroutes(result[[z]]$route, input$n1)
              for (s in 1:length(subroutes))  {
                for (t in 2:(length(subroutes[[s]]$tour)-1)) {
                  clientz <- subroutes[[s]]$tour[t]
                  rootz <- subroutes[[s]]$tour[1]
                  # tc
                  if ((clientz > input$n1)) {
                    if (result[[i]]$type == "CVR") main_root2 <- return_main_route(result[[i]]$route)
                    else main_root2 <- result[[i]]$route
                    clientw <- return_close_client(clienti, main_root2, vecinity) #main_root2[w]
                    if ((clientw <= input$n1)&&(clientw != clienti)&&
                        (clienti != subroutes[[s]]$root)&&(i!=z)&&(sum(result[[i]]$route==clienti)==1)) {
                      # new routes
                      route1 <- replace_route_client_subroute(clienti, clientz, clientw, result[[i]]$route)
                      route2 <- replace_route_client(clientz, clienti, result[[z]]$route)
                      # feasibility
                      feasible_route1 <- 1
                      feasible_route2 <- 1
                      if (check_feas) {
                        feasible_route1 <- check_feasibility(result[[i]], route1, input, result[[i]]$type, type_problem, penalty_capacity, "update") 
                        
                        if ( result[[z]]$type == "CVR" && (sum(duplicated(route2[2:(length(route2)-1)]))==0) ) opt_check <- "new"
                        else opt_check <- "update"
                        feasible_route2 <- check_feasibility(result[[z]], route2, input, result[[z]]$type, type_problem, penalty_capacity, opt_check) 
                      }
                      # add to mov list
                      if (feasible_route1 && feasible_route2) {

                        if (type_problem == "TTRP") {
                          
                          .Call('insertMovsList', as.integer(i), as.integer(z), 
                                as.integer(c(clienti)), as.integer(c(clientz)), 
                                as.integer(c(route1)), as.integer(c(route2)), 
                                "exchange_movement_vc_main_tour_tc_subtour", as.integer(rooti), as.integer(rootz), 
                                PACKAGE = "mcttrpalns")
                          
                        }
                        
                        if (type_problem == "MCTTRP") {
                          if ( result[[i]]$type == "CVR" && (sum(duplicated(route2[2:(length(route2)-1)]))==0) ) opt_check <- 2
                          else opt_check <- 0
                          
                          res1 <- check_capacity_hoppers_MCTTRP_return_hoppers(route1, result[[i]], input)
                          res2 <- check_capacity_hoppers_MCTTRP_return_hoppers(route2, result[[z]], input)
                          hoppers_truck1 <- res1$hoppers_truck
                          hoppers_trailer1 <- res1$hoppers_trailer
                          hoppers_truck2 <- res2$hoppers_truck                          
                          hoppers_trailer2 <- res2$hoppers_trailer
                          
                          .Call('insertMovsList_MCTTRP',  as.integer(i), as.integer(z), 
                                as.integer(c(clienti)), as.integer(c(clientz)), 
                                as.integer(c(route1)), as.integer(c(route2)), 
                                "exchange_movement_vc_main_tour_tc_subtour", as.integer(rooti), as.integer(rootz), 
                                as.integer(hoppers_truck1), as.integer(hoppers_trailer1), as.integer(hoppers_truck2), as.integer(hoppers_trailer2),
                                as.integer(opt_check),
                                PACKAGE = "mcttrpalns")
                        }
                      }                        
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
  }
  
}



exchange_movement_tc_PTR_and_vc_in_main_tour<-function(input, result, changed_routes, type_problem, vecinity, perc_vecinity, 
                                                       penalty_capacity, check_feas) {
  for (i in 1:length(result)) {
    if ((result[[i]]$type == "PTR")) {
      for (j in 2:(length(result[[i]]$route)-1)) {
        if (result[[i]]$route[j] > input$n1) {
          clienti <- result[[i]]$route[j]
          rooti <- 0
          for (z in 1:length(result)) {
            if ((i!=z)&&(result[[z]]$type != "PTR")&&((z %in% changed_routes)||(i %in% changed_routes))) {
              if (result[[z]]$type == "CVR") main_root <- return_main_route(result[[z]]$route)
              else main_root <- result[[z]]$route
              for (t in 2:(length(main_root)-1)) {
                clientz <- main_root[t]
                rootz <- 0
                if ((clientz <= input$n1)&&(sum(clientz==result[[z]]$route)==1)) {
                  if (result[[z]]$type == "CVR") main_root2 <- return_main_route(result[[z]]$route)
                  else main_root2 <- result[[z]]$route
                  clientw <- return_close_client(clienti, main_root2, vecinity)
                  if ((clientw <= input$n1)&&(clientw != clientz)) {
                    # new routes
                    route1 <- replace_route_client(clienti, clientz, result[[i]]$route)
                    route2 <- replace_route_client_subroute(clientz, clienti, clientw, result[[z]]$route)
                    
                    # feasibility
                    feasible_route1 <- 1
                    feasible_route2 <- 1
                    if (check_feas) {
                      feasible_route1 <- check_feasibility(result[[i]], route1, input, result[[i]]$type, type_problem, penalty_capacity, "update") 
                      feasible_route2 <- check_feasibility(result[[z]], route2, input, result[[z]]$type, type_problem, penalty_capacity, "update") 
                    }
                    # add to mov list
                    if (feasible_route1 && feasible_route2) {
                      
                      if (type_problem == "TTRP") {
                        
                        .Call('insertMovsList', as.integer(i), as.integer(z), 
                              as.integer(c(clienti)), as.integer(c(clientz)), 
                              as.integer(c(route1)), as.integer(c(route2)), 
                              "exchange_movement_tc_PTR_and_vc_in_main_tour", as.integer(rooti), as.integer(rootz), 
                              PACKAGE = "mcttrpalns")
                        
                      }
                      
                      if (type_problem == "MCTTRP") {
                        res1 <- check_capacity_hoppers_MCTTRP_return_hoppers(route1, result[[i]], input)
                        res2 <- check_capacity_hoppers_MCTTRP_return_hoppers(route2, result[[z]], input)
                        hoppers_truck1 <- res1$hoppers_truck
                        hoppers_trailer1 <- res1$hoppers_trailer
                        hoppers_truck2 <- res2$hoppers_truck                          
                        hoppers_trailer2 <- res2$hoppers_trailer
                        
                        .Call('insertMovsList_MCTTRP',  as.integer(i), as.integer(z), 
                              as.integer(c(clienti)), as.integer(c(clientz)), 
                              as.integer(c(route1)), as.integer(c(route2)), 
                              "exchange_movement_tc_PTR_and_vc_in_main_tour", as.integer(rooti), as.integer(rootz), 
                              as.integer(hoppers_truck1), as.integer(hoppers_trailer1), as.integer(hoppers_truck2), as.integer(hoppers_trailer2), 
                              as.integer(0),
                              PACKAGE = "mcttrpalns")
                      }
                    }   
                  }
                }
              }
            }
          }
        }
      }
    }
  }
  
  #return(movs)
  
}


exchange_movement_client_short_subtour_and_client_in_main_tour<-function(input, result, changed_routes, type_problem, 
                                                                         vecinity, perc_vecinity, penalty_capacity, check_feas) {
  
  for (i in 1:length(result)) {
    if ((result[[i]]$type == "CVR")) {
      subroutes <- return_subroutes(result[[i]]$route, input$n1)
      for (s in 1:length(subroutes))  {
        if (length(subroutes[[s]]$tour)==3) {
          clienti <- subroutes[[s]]$tour[2]
          rooti <- subroutes[[s]]$tour[1]
          for (z in 1:length(result)) {
            if ((i!=z)&&((z %in% changed_routes)||(i %in% changed_routes))) {
              if (result[[z]]$type == "CVR") route_z <- result[[z]]$main_tour
              else route_z <- result[[z]]$route
              for (t in 2:(length(route_z)-1)) {
                clientz <- route_z[t]
                rootz <- 0
                if ((clientz <= input$n1)&&(sum(clientz==result[[z]]$route)==1)) {
                  if (result[[z]]$type == "PTR") {
                    # new routes
                    route1 <- replace_subroute_vc_2(subroutes[[s]], clientz, result[[i]]$route)
                    route2 <- replace_route_client(clientz, clienti, result[[z]]$route)
                    
                    # feasibility
                    feasible_route1 <- 1
                    feasible_route2 <- 1
                    if (check_feas) {
                      if ( result[[i]]$type == "CVR" && (sum(duplicated(route1[2:(length(route1)-1)]))==0) ) opt_check <- "new"
                      else opt_check <- "update"
                      feasible_route1 <- check_feasibility(result[[i]], route1, input, result[[i]]$type, type_problem, penalty_capacity, opt_check) 
                      feasible_route2 <- check_feasibility(result[[z]], route2, input, result[[z]]$type, type_problem, penalty_capacity, "update") 
                    }
                    # add to mov list
                    if (feasible_route1 && feasible_route2) {

                      
                      if (type_problem == "TTRP") {
                        
                        .Call('insertMovsList', as.integer(i), as.integer(z), 
                              as.integer(c(clienti)), as.integer(c(clientz)), 
                              as.integer(c(route1)), as.integer(c(route2)), 
                              "exchange_movement_client_short_subtour_and_client_in_main_tour", as.integer(rooti), as.integer(rootz), 
                              PACKAGE = "mcttrpalns")
                        
                      }
                      
                      if (type_problem == "MCTTRP") {
                        if ( result[[i]]$type == "CVR" && (sum(duplicated(route1[2:(length(route1)-1)]))==0) ) opt_check <- 1
                        else opt_check <- 0
                        
                        res1 <- check_capacity_hoppers_MCTTRP_return_hoppers(route1, result[[i]], input)
                        res2 <- check_capacity_hoppers_MCTTRP_return_hoppers(route2, result[[z]], input)
                        hoppers_truck1 <- res1$hoppers_truck
                        hoppers_trailer1 <- res1$hoppers_trailer
                        hoppers_truck2 <- res2$hoppers_truck                          
                        hoppers_trailer2 <- res2$hoppers_trailer
                        
                        .Call('insertMovsList_MCTTRP',  as.integer(i), as.integer(z), 
                              as.integer(c(clienti)), as.integer(c(clientz)), 
                              as.integer(c(route1)), as.integer(c(route2)), 
                              "exchange_movement_client_short_subtour_and_client_in_main_tour", as.integer(rooti), as.integer(rootz), 
                              as.integer(hoppers_truck1), as.integer(hoppers_trailer1), as.integer(hoppers_truck2), as.integer(hoppers_trailer2), 
                              as.integer(opt_check),
                              PACKAGE = "mcttrpalns")
                      }
                      
                    }
                  }
                  else {
                    #for (w in 2:(length(route_z)-1)) {
                    #  clientw <- route_z[w]
                    clientw <- return_close_client(clienti, route_z, vecinity)
                    if ((clientw <= input$n1)&&(clientw != clientz)) {
                      # new routes
                      #route1 <- replace_route_client_vc_subroute(clienti, clientz, result[[i]]$route)
                      route1 <- replace_subroute_vc_2(subroutes[[s]], clientz, result[[i]]$route)
                      route2 <- replace_route_client_subroute(clientz, clienti, clientw, result[[z]]$route)
                      
                      # feasibility
                      # feasibility
                      feasible_route1 <- 1
                      feasible_route2 <- 1
                      if (check_feas) {                     
                        if ( result[[i]]$type == "CVR" && (sum(duplicated(route1[2:(length(route1)-1)]))==0) ) opt_check <- "new"
                        else opt_check <- "update"
                        feasible_route1 <- check_feasibility(result[[i]], route1, input, result[[i]]$type, type_problem, penalty_capacity, opt_check) 
                        feasible_route2 <- check_feasibility(result[[z]], route2, input, result[[z]]$type, type_problem, penalty_capacity, "update") 
                      }
                      # add to mov list
                      if (feasible_route1 && feasible_route2) {
                        
                        if (type_problem == "TTRP") {
                          
                          .Call('insertMovsList', as.integer(i), as.integer(z), 
                                as.integer(c(clienti)), as.integer(c(clientz)), 
                                as.integer(c(route1)), as.integer(c(route2)), 
                                "exchange_movement_client_short_subtour_and_client_in_main_tour",  as.integer(rooti), as.integer(rootz),
                                PACKAGE = "mcttrpalns")
                          
                        }
                        
                        if (type_problem == "MCTTRP") {
                          if ( result[[i]]$type == "CVR" && (sum(duplicated(route1[2:(length(route1)-1)]))==0) ) opt_check <- 1
                          else opt_check <- 0
                          
                          res1 <- check_capacity_hoppers_MCTTRP_return_hoppers(route1, result[[i]], input)
                          res2 <- check_capacity_hoppers_MCTTRP_return_hoppers(route2, result[[z]], input)
                          hoppers_truck1 <- res1$hoppers_truck
                          hoppers_trailer1 <- res1$hoppers_trailer
                          hoppers_truck2 <- res2$hoppers_truck                          
                          hoppers_trailer2 <- res2$hoppers_trailer
                          
                          .Call('insertMovsList_MCTTRP',  as.integer(i), as.integer(z), 
                                as.integer(c(clienti)), as.integer(c(clientz)), 
                                as.integer(c(route1)), as.integer(c(route2)), 
                                "exchange_movement_client_short_subtour_and_client_in_main_tour",  as.integer(rooti), as.integer(rootz),
                                as.integer(hoppers_truck1), as.integer(hoppers_trailer1), as.integer(hoppers_truck2), as.integer(hoppers_trailer2),
                                as.integer(opt_check),
                                PACKAGE = "mcttrpalns")
                        }
                      }
                    }
                    #} 
                  }
                }
              }
            }
          }
          
        }
      }
    }
  }
  
  #return(movs)
}


exchange_movement_client_subtour_and_vc_creating_subtour<-function(input, result, changed_routes, type_problem, 
                                                                   vecinity, perc_vecinity, penalty_capacity, check_feas) {
  
  for (i in 1:length(result)) {
    if (result[[i]]$type == "CVR") {
      subroutes <- return_subroutes(result[[i]]$route, input$n1)
      for (s in 1:length(subroutes))  {
        if (length(subroutes[[s]]$tour)==3) {
          for (j in 2:(length(subroutes[[s]]$tour)-1)) {
            clienti <- subroutes[[s]]$tour[j]
            rooti <- subroutes[[s]]$tour[1]
            for (z in 1:length(result)) {
              if ((i!=z)&&(result[[z]]$type == "CVR")&&((z %in% changed_routes)||(i %in% changed_routes))) {
                subroutes2 <- return_subroutes(result[[z]]$route, input$n1)
                for (ss in 1:length(subroutes2))  {
                  for (t in 2:(length(subroutes2[[ss]]$tour)-1)) {
                    clientz <- subroutes2[[ss]]$tour[t]
                    if (clientz <= input$n1) {
                      rootz <-  subroutes2[[ss]]$tour[1]
                      # new routes
                      route1 <- replace_route_client_vc_subroute(clienti, clientz, result[[i]]$route)
                      route2 <- replace_route_client(clientz, clienti, result[[z]]$route)
                      
                      # feasibility
                      feasible_route1 <- 1
                      feasible_route2 <- 1
                      if (check_feas) { 
                        if ( result[[i]]$type == "CVR" && (sum(duplicated(route1[2:(length(route1)-1)]))==0) ) opt_check <- "new"
                        else opt_check <- "update"
                        feasible_route1 <- check_feasibility(result[[i]], route1, input, result[[i]]$type, type_problem, penalty_capacity, opt_check) 
                        feasible_route2 <- check_feasibility(result[[z]], route2, input, result[[z]]$type, type_problem, penalty_capacity, "update") 
                      }
                      # add to mov list
                      if (feasible_route1 && feasible_route2) {

                        if (type_problem == "TTRP") {
                          
                          .Call('insertMovsList', as.integer(i), as.integer(z), 
                                as.integer(c(clienti)), as.integer(c(clientz)), 
                                as.integer(c(route1)), as.integer(c(route2)), 
                                "exchange_movement_client_subtour_and_vc_creating_subtour",as.integer(rooti), as.integer(rootz),
                                PACKAGE = "mcttrpalns")
                          
                        }
                        
                        if (type_problem == "MCTTRP") {
                          if ( result[[i]]$type == "CVR" && (sum(duplicated(route1[2:(length(route1)-1)]))==0) ) opt_check <- 1
                          else opt_check <- 0
                          res1 <- check_capacity_hoppers_MCTTRP_return_hoppers(route1, result[[i]], input)
                          res2 <- check_capacity_hoppers_MCTTRP_return_hoppers(route2, result[[z]], input)
                          hoppers_truck1 <- res1$hoppers_truck
                          hoppers_trailer1 <- res1$hoppers_trailer
                          hoppers_truck2 <- res2$hoppers_truck                          
                          hoppers_trailer2 <- res2$hoppers_trailer
                          
                          .Call('insertMovsList_MCTTRP',  as.integer(i), as.integer(z), 
                                as.integer(c(clienti)), as.integer(c(clientz)), 
                                as.integer(c(route1)), as.integer(c(route2)), 
                                "exchange_movement_client_subtour_and_vc_creating_subtour",as.integer(rooti), as.integer(rootz),
                                as.integer(hoppers_truck1), as.integer(hoppers_trailer1), as.integer(hoppers_truck2), as.integer(hoppers_trailer2), 
                                as.integer(opt_check),
                                PACKAGE = "mcttrpalns")
                        }

                      }
                      
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
  }
  
}


delete_node_in_route<-function(j, route) {
  
  delete_subtour <- 0
  if ((j>2)&&(j<(length(route)-1))&&(route[j-1]==route[j+1])) delete_subtour <- 1
  
  if (!delete_subtour) {
    route <- c(route[1:(j-1)], route[(j+1):length(route)])
  }
  else {
    route <- c(route[1:(j-1)], route[(j+2):length(route)])
  }
  
  return(route)
}

add_node_in_route<-function(pos, client, route) {
  
  route <- c(route[1:pos[1]], client, route[(pos[1]+1):length(route)])
  
  return(route)
}


add_subroute_mov<-function(pos, client, route) {
  
  route <- c(route[1:pos[1]], client, route[(pos[1]):length(route)])
  
  return(route)
}


replace_subroute_vc<-function(subroute, clientvc, route){
  
  # delete subroutes
  route <- delete_subroute(subroute$tour, route)
  
  for (i in 1:length(route)) {
    if (route[i] == subroute$root) {
      route[i] <- clientvc
    }
  }
  
  
  return(route)
}


replace_subroute_vc_2<-function(subroute, clientvc, route){
  
  # delete subroutes
  route <- delete_subroute(subroute$tour, route)
  
  new_route <- c(0)
  no_more_root <- 1
  for (i in 2:length(route)) {
    if ((route[i] == subroute$root)&&(no_more_root)) {
      new_route <- c(new_route, route[i])
      new_route <- c(new_route, clientvc)
      no_more_root <- 0
    } else new_route <- c(new_route, route[i])
  }
  
  
  return(new_route)
}



replace_route_client<-function(clienti, clientj, route){
  for (i in 1:length(route)) {
    if (route[i] == clienti) {
      route[i] <- clientj
    }
  }
  
  return(route)
}



replace_route_client_subroute<-function(clienti, clientj, clientz, route){
  # delete clienti
  new_route <- c(0)
  counter <- 0
  for (i in 2:length(route)) {
    if (route[i] != clienti) {
      new_route <- c(new_route, route[i])
    }
    if (route[i] == clientz) {
      counter <- counter + 1
    }
  }
  # add clientj as a subtour in clientz
  if (counter == 1) {
    new_route2 <- c(0)
    for (i in 2:length(new_route)) {
      if ((new_route[i] == clientz)) {
        new_route2 <- c(new_route2, clientz)
        new_route2 <- c(new_route2, clientj)
        new_route2 <- c(new_route2, clientz)
      } else {
        new_route2 <- c(new_route2, new_route[i])
      }
    }
  } else {
    index <- which (new_route == clientz)
    new_route2 <- c(0)
    for (i in 2:length(new_route)) {
      if (i == index[1]) {
        new_route2 <- c(new_route2, clientz)
        new_route2 <- c(new_route2, clientj)
        new_route2 <- c(new_route2, clientz)
      } else {
        new_route2 <- c(new_route2, new_route[i])
      }
    }
  }
  
  return(new_route2)
}



replace_route_client_vc_subroute<-function(clienti, clientj, route){
  # delete clienti from subroute
  for (i in 2:length(route)) {
    if (route[i] == clienti) {
      index_client_i <- i    
    }
  }
  
  new_route <- c(route[1:(index_client_i-1)], clientj, route[(index_client_i+2):length(route)])
  
  return(new_route)
}



replace_subroute<-function(old_subroute, subroute, route){
  root <- old_subroute[1]
  
  route <- delete_subroute(old_subroute, route)
  
  route <- add_subroute(root, subroute, route)
  
  return(route)
}



add_subroute<-function(clienti, subroute, route){
  
  counter <- 0
  for (i in 2:length(route)) {
    if (route[i] == clienti) {
      counter <- counter + 1
    }
  }
  
  for (i in 2:length(route)) {
    if (route[i] == clienti) {
      index_client_i <- i
      break;
    }
  }
  
  if (counter == 1) {
    subroute <- subroute[2:(length(subroute)-1)]
    # add subroute
    new_route <- c(route[1:(index_client_i)], subroute, route[(index_client_i):length(route)])
  }
  else {
    subroute <- subroute[2:(length(subroute)-1)]
    # add subroute
    new_route <- c(route[1:(index_client_i)], subroute, route[(index_client_i):length(route)])
    
  }
  return(new_route)
}



delete_subroute<-function(subroute, route){
  
  subroute <- subroute[2:(length(subroute)-1)]
  new_route <- c(0)
  
  for (i in 2:length(route)) {
    if (sum(subroute==route[i])==0) {
      new_route <- c(new_route, route[i])
    }
  }
  
  new_route2 <- c(0)
  for (i in 2:length(new_route)) {
    if (new_route[i]!=new_route[i-1]) {
      new_route2 <- c(new_route2, new_route[i])
    }
  }
  
  return(new_route2)
}



delete_client<-function(client, route){
  
  new_route <- c(0)
  
  for (i in 2:length(route)) {
    if (client!=route[i]) {
      new_route <- c(new_route, route[i])
    }
  }
  
  return(new_route)
}


split_subroute<-function(clienti, route){
  
  for (i in 2:length(route)) {
    if (route[i] == clienti) {
      index_client_i <- i    
    }
  }
  
  for (i in index_client_i:1) {
    if (sum(route[i] == route)>1) {
      root1 <- i    
      break
    }
  }
  
  for (i in index_client_i:length(route)) {
    if (sum(route[i] == route)>1) {
      root2 <- i   
      break
    }
  }
  
  route <-c( route[1:(index_client_i-1)], route[root1],  clienti, route[root2], route[(index_client_i+1):length(route)])
  
  return(route)
}



add_movements_to_list<-function(input, indexr1, indexr2, client1, client2, string, 
                                route1, route2, movs) {
  
  counter <- length(movs$mov_list) + 1
  
  movs$mov_list[[counter]] <- list()
  movs$mov_list[[counter]]$indexr1 <- indexr1
  movs$mov_list[[counter]]$indexr2 <- indexr2
  movs$mov_list[[counter]]$mov_name <- string
  movs$mov_list[[counter]]$route1 <- route1
  movs$mov_list[[counter]]$route2 <- route2
  movs$mov_list[[counter]]$client1 <- client1
  movs$mov_list[[counter]]$client2 <- client2
  
  return(movs)
}


#update_cost_pen
update_cost_pen<-function(movs, table_freq, zeta, alpha, counter_i){
  
  for (counter in 1:length(movs$mov_list)) {
    if ((length(movs$mov_list[[counter]]$route2)>1))  {
      
      penalty_freq <- 0
      for (i in 1:length(movs$mov_list[[counter]]$client1)) {
        penalty_freq <- penalty_freq + return_table_freq(table_freq, movs$mov_list[[counter]]$client1[i], movs$mov_list[[counter]]$indexr1)
      }
      if (length(movs$mov_list[[counter]]$client2)&&(movs$mov_list[[counter]]$client2!=0)) {
        for (i in 1:length(movs$mov_list[[counter]]$client2)) {
          penalty_freq <- penalty_freq + return_table_freq(table_freq, movs$mov_list[[counter]]$client2[i], movs$mov_list[[counter]]$indexr2)
        }
      }
      penalty_freq <- 1 + zeta * penalty_freq / counter_i
      # cost
      movs$mov_list_cost_pen[[counter]] <-  movs$mov_list_cost[[counter]] * penalty_freq
      
    } else {
      
      penalty_freq <- 0
      
      for (i in 1:length(movs$mov_list[[counter]]$client1)) {
        penalty_freq <- penalty_freq + return_table_freq(table_freq, movs$mov_list[[counter]]$client1[i], movs$mov_list[[counter]]$indexr1)
      }
      penalty_freq <- 1 + zeta * penalty_freq / counter_i
      # cost
      movs$mov_list_cost_pen[[counter]] <-  movs$mov_list_cost[[counter]] * penalty_freq
    }
  }
  
  return(movs)
  
}


# check_new_type
check_new_type<-function(type_root, route, input){
  all_vc <- 1
  subroutes <- 0
  for (i in 2:(length(route)-1)) {
    if (route[i] >  input$n1) all_vc <- 0
    if (sum(route==route[i])>1) subroutes <- 1
  }
  
  # determine new type
  if ((all_vc)&&(!subroutes)&&(type_root == "CVR")) type_root <- "PVR"
  if ((!all_vc)&&(!subroutes)&&(type_root == "CVR")) type_root <- "PTR"
  else if (subroutes) type_root <- "CVR"
  
  return(type_root)
}

# is_in_vecinity
is_in_vecinity<-function(clienti, clientw, vecinity, perc_vecinity){
  local_vecinity <- vecinity[[clienti]][1:ceiling(length(vecinity[[clienti]])*perc_vecinity)]
  
  if (sum(clientw %in% local_vecinity)) return (1)
  else return (0)
}

# return_close_client
return_close_client<-function(clienti, route, vecinity) {
  
  index <- Inf
  for (i in 2:(length(route)-1)) {
    index_i <- which(vecinity[[clienti]] == route[i])
    if (index_i < index) {
      index <- index_i
    }
  }
  
  return(vecinity[[clienti]][index])
}


# update_penalties
update_penalties <- function(input, alpha, gamma, current_solution, type_problem){
  
  infeasibility <- calc_penalty(input, current_solution, type_problem)
  
  #print(paste0("CALIBRATE PENALTIES: current solution pen -> ", feasibility, " gamma ", gamma, " alpha ", alpha))
  if (infeasibility > 0) {
    alpha <- min(( 1 + gamma) * alpha, 100)
  }
  else {
    alpha <- max(( 1 + gamma) / alpha, 0.01)
  }
  
  return(alpha)
}

# init_changed_list
init_changed_list <- function(current_solution, values) {
  
  changed_list <- 1:length(current_solution)
  
  return(changed_list)
}

# modified_changed_list
modified_changed_list <- function (changed_list, i, j) {
  
  changed_list <- c(i, j)
  
  return(changed_list)
}

# modified_mov_list_using_changed_list
modified_mov_list_using_changed_list <- function (changed_list, movs) {
  
  index_to_delete <- c(0)
  index_to_remain <- c()
  for (i in 1:length(movs$mov_list)) {
    if ( (movs$mov_list[[i]]$indexr1 %in% changed_list) || (movs$mov_list[[i]]$indexr2 %in% changed_list)) {
      index_to_delete <- c(index_to_delete, i)
    } else {
      index_to_remain <- c(index_to_remain, i)
    }
  }
  
  if (length(index_to_delete)) {
  
    .Call('delete_elements_using_changed_list', index_to_remain, PACKAGE = "mcttrpalns")
    movs$mov_list <- movs$mov_list[-(index_to_delete[2:length(index_to_delete)])]
    movs$mov_list_cost <- movs$mov_list_cost[-(index_to_delete[2:length(index_to_delete)])]
    movs$mov_list_cost_pen <- movs$mov_list_cost_pen[-(index_to_delete[2:length(index_to_delete)])]
    movs$mov_list_cost_feas <- movs$mov_list_cost_feas[-(index_to_delete[2:length(index_to_delete)])]
    movs$mov_list_cost_nopen <- movs$mov_list_cost_nopen[-(index_to_delete[2:length(index_to_delete)])]
  } 
  
  return(movs)
}

# modified_mov_list_using_changed_list
modified_mov_list_using_changed_list_new <- function (changed_list) {
  
  #print("DELETE")
  .Call('delete_elements_using_changed_list', changed_list, PACKAGE = "mcttrpalns")
   
}

# evaluate_cost_mov_list
evaluate_cost_mov_list<-function(input, movs, changed_list, solution, alpha, type_problem) {
  
  
  #.Call("evaluate_cost_mov_list", input, mov_list, changed_list, solution, alpha, PACKAGE = "mcttrpalns")
  
  if ((length(movs$mov_list)>0)&&(length(changed_list)>0)) {
    #static unfeasibility
    #static cost
    static_feas <- c()
    static_cost <- c()
    cap_routes_vector <- c()
    for (index in 1:length(solution)){
      cap <- input$capacidad.truck
      if (solution[[index]]$type != "PTR") cap <- input$capacidad.vehiculo
      cap_routes_vector <- c(cap_routes_vector, cap)
      static_feas <- c(static_feas, calc_penalty_route(input, solution[[index]], cap, type_problem))
      static_cost <- c(static_cost, local_cost(solution[[index]]$route, input$matriz.distancia))
    }

    .Call('c_eval_movs', as.integer(changed_list), cap_routes_vector, input$matriz.distancia,
          input$vector.demandas, input$capacidad.truck, static_feas, static_cost, alpha,
          as.integer(0), PACKAGE = "mcttrpalns")
    
    
    
    for (i in 1:length(movs$mov_list)) {
      new_solution <- solution
      
      if ((movs$mov_list[[i]]$indexr1 %in% changed_list) || (movs$mov_list[[i]]$indexr2 %in% changed_list)) {
        # calc feasibility and cost
        feas <- 0
        cost <- 0
        movs$mov_list_cost_feas[[i]] <- 0
        movs$mov_list_cost_nopen[[i]] <- 0
        for (j in 1:length(solution)){
          cap <- input$capacidad.truck
          if (solution[[j]]$type != "PTR") cap <- input$capacidad.vehiculo
          if (j %in% c(movs$mov_list[[i]]$indexr1, movs$mov_list[[i]]$indexr2)) {
            if (j == movs$mov_list[[i]]$indexr1) route <- movs$mov_list[[i]]$route1
            if (j == movs$mov_list[[i]]$indexr2) route <- movs$mov_list[[i]]$route2
            movs$mov_list_cost_feas[[i]] <- movs$mov_list_cost_feas[[i]] + calc_penalty_route(input, route, cap, type_problem)   
            movs$mov_list_cost_nopen[[i]] <- movs$mov_list_cost_nopen[[i]] + local_cost(route, input$matriz.distancia) 
          } 
          else {
            feas <- feas + static_feas[j]
            cost <- cost + static_cost[j]
          }
        }
        
        movs$mov_list_cost[[i]] <-  (movs$mov_list_cost_nopen[[i]] + cost) + alpha * (movs$mov_list_cost_feas[[i]] + feas)
        movs$mov_list_cost_pen[[i]] <- 0               
        
      } else {
        cost <- 0
        feas <- 0
        for (j in 1:length(solution)){
          if (!(j %in% c(movs$mov_list[[i]]$indexr1, movs$mov_list[[i]]$indexr2))) {
            cost <- cost + static_cost[j]
            feas <- feas + static_feas[j]
          }
        }
        movs$mov_list_cost[[i]]  <-  (movs$mov_list_cost_nopen[[i]] + cost) + alpha * (movs$mov_list_cost_feas[[i]] + feas)
        movs$mov_list_cost_pen[[i]] <- 0  
      }
      
    }
    
  }
  
  return(movs)
}




evaluate_cost_mov_list_new<-function(input, changed_list, solution, alpha, size_mov, type_problem) {
  
  
  if ((size_mov>0)&&(length(changed_list)>0)) {
    #static unfeasibility
    #static cost
    if (type_problem == "TTRP"){
      static_feas <- c()
      static_cost <- c()
      cap_routes_vector <- c()
      for (index in 1:length(solution)){
        cap <- input$capacidad.truck
        if (solution[[index]]$type != "PTR") cap <- input$capacidad.vehiculo
        cap_routes_vector <- c(cap_routes_vector, cap)
        static_feas <- c(static_feas, calc_penalty_route(input, solution[[index]], cap, type_problem))
        static_cost <- c(static_cost, local_cost(solution[[index]]$route, input$matriz.distancia))
      }
      
      .Call('c_eval_movs', as.integer(changed_list), cap_routes_vector, input$matriz.distancia,
            input$vector.demandas, input$capacidad.truck, static_feas, static_cost, alpha,
            as.integer(0), PACKAGE = "mcttrpalns")
    }
    
    if (type_problem == "MCTTRP"){
      static_feas <- c()
      static_cost <- c()
      cap_routes_vector <- c()
      for (index in 1:length(solution)){
        cap <- input$capacidad.truck[1]
        if (solution[[index]]$type != "PTR") cap <- input$capacidad.vehiculo[1]
        cap_routes_vector <- c(cap_routes_vector, cap)
        static_feas <- c(static_feas, calc_penalty_route(input, solution[[index]], cap, type_problem))
        static_cost <- c(static_cost, local_cost(solution[[index]]$route, input$matriz.distancia))
      }

      n_hoppers_truck <- length(input$H.camion[1,])
      n_hoppers_trailer <- length(input$H.trailer[1,])
      
      .Call('c_eval_movs_MCTTRP', as.integer(changed_list), cap_routes_vector, input$matriz.distancia,
            as.integer(length(input$matriz.demandas[1,])), input$matriz.demandas, input$capacidad.truck[1], input$capacidad.trailer[1], 
            as.numeric(n_hoppers_truck), as.numeric(n_hoppers_trailer), static_feas, static_cost, alpha,  as.integer(0), PACKAGE = "mcttrpalns")

      
    }
    
  }
  
}


