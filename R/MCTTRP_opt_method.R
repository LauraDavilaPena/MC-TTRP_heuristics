MCTTRP_opt_method<-function(result, initial_solution, input, init_time, type_problem, seed){
    # Tres solucions importantes:
    # bestsolution -> mellor solucion factible e infactible ata o momento
    # bestsolution_f -> mellor solucion factible
    # current_solution -> solucion actual, que pode ser factible ou non
  
    # init parameters
    stopping_conditions <- 0
    penalty_max <- 100
    iter <- 1
    alpha <- 1
    
    # tabu search
    current_cost <- calculateTotalDistanceTS(input, alpha, initial_solution, type_problem)
    
    # best options pode ser infactible
    bestsolution <- initial_solution 
    bestcost <- current_cost
    # bestsolution_f 
    bestsolution_f <- initial_solution
    bestcost_f <- current_cost    
    
    res_tabu <- tabu_search (input, initial_solution, current_cost, bestcost, 
                             type_problem, input$max_iter, iter, 1, penalty_max)
    current_solution <- res_tabu$current_solution
    current_cost <- res_tabu$current_cost
    candidate_best_f_solution <- res_tabu$best_f_solution
    candidate_best_f_cost <- res_tabu$best_f_cost
    
    # best solution
    newcost <- calculateTotalDistanceTS(input, alpha, current_solution, type_problem)
    if ((bestcost_f >  candidate_best_f_cost)) {
      bestsolution_f <- candidate_best_f_solution 
      bestcost_f <- candidate_best_f_cost
    }
    
    if ((bestcost >  current_cost)) {
      bestsolution <- current_solution 
      bestcost <- current_cost
    }
    
    # print init output
    print(paste0("fobj ", bestcost_f, " iter ", 0, " time ", difftime(Sys.time(), init_time, units = "secs")))
    
    no_improv_counter <- 0
    
    while (!stopping_conditions) {

# current_cost <- calculateTotalDistanceTS(input, alpha, current_solution, type_problem)
# print(paste0("before global solution ", current_cost, "   penalty ", calc_penalty(input, current_solution, type_problem)))
# all_routes(current_solution)
# analyse(all_routes(current_solution), input, current_solution, "MCTTRP")
# readline()
# 
 
 save(current_solution, file="solution_P2.RData")
 print("solution saved")
# readline()

      # perturbation
      res_p <- perturbation_core(input, current_solution, penalty_max, type_problem)
      current_solution <- res_p$current_solution
      phi <- res_p$phi
      
      
# current_cost <- calculateTotalDistanceTS(input, alpha, current_solution, type_problem)
# print(paste0("after global solution ", current_cost, "   penalty ", calc_penalty(input, current_solution, type_problem)))
# print(all_routes(current_solution))
# analyse(all_routes(current_solution), input, current_solution, "MCTTRP")
# readline()
# 
 save(current_solution, file="solution_P2_perturb.RData")
 print("perturbed solution saved")
# readline()


      
      # improvement
      current_solution <- result_improvement(input, current_solution, type_problem)
      current_cost <- calculateTotalDistanceTS(input, alpha, current_solution, type_problem)
      
      # tabu search
      res_tabu <- tabu_search (input, current_solution, current_cost, bestcost, type_problem, input$max_iter, iter, phi, penalty_max)
      current_solution <- res_tabu$current_solution
      current_cost <- res_tabu$current_cost
      candidate_best_f_solution <- res_tabu$best_f_solution
      candidate_best_f_cost <- res_tabu$best_f_cost
      
      # best solution
      newcost <- calculateTotalDistanceTS(input, alpha, current_solution, type_problem)
      if ((bestcost_f >  candidate_best_f_cost)) {
                bestsolution_f <- candidate_best_f_solution 
                bestcost_f <- candidate_best_f_cost
      }
      
      if ((bestcost >  current_cost)) {
        bestsolution <- current_solution 
        bestcost <- current_cost
      }
      else no_improv_counter <- no_improv_counter + 1
      
      # return to the best solution
      if (((runif(1) < (iter/input$max_iter)^2))) { #} || (no_improv_counter == 10)) {
            current_solution <- bestsolution_f
            current_cost <- bestcost_f
            no_improv_counter <- 0
      } 
      
      #print("")
      print(paste0("fobj ", current_cost, " infea ", calc_penalty(input, current_solution, type_problem), " iter ", iter, " (best fobj ", bestcost_f ,
                   " infea ", calc_penalty(input, bestsolution_f, type_problem) , " ) time ", difftime(Sys.time(), init_time, units = "secs"), " s"))
      #readline()

      # check stopping conditions
      stopping_conditions <- check_stoppping_conditions(iter, init_time, bestcost, input)
    
      iter <- iter + 1
    }
    
    
    return(bestsolution_f)
}



check_stoppping_conditions<-function(current_iteration, init_time, current_obj, input){
  current_time <- difftime(Sys.time(), init_time, units = "secs")

  if (current_iteration >= input$max_iter) {
    print(paste0("[[ Stopping criteria ]] Max iterations"))
    res <- 1
  }
  else if (input$max_time <= current_time) {
    print(paste0("[[ Stopping criteria ]] Max time"))
    res <- 1
  }
  else if (input$vtr >= current_obj){
    print(paste0("[[ Stopping criteria ]] The algorithm obtained the Value to reach"))
    res <- 1
  } 
  else {
    res <- 0
  }
    
  return(res) 
}


all_routes<-function(solution) {
  
  route <- c()
  
  if (length(solution)>=2) {
    for (i in 1:length(solution)) {
      route <- c(route, solution[[i]]$route)
    }
  }
  
  route <- delete_dupl_zeros_route(route)
  
  return(route)
}

calc_vecinity<-function(input) {
   clients <- length(input$matriz.distancia[1, 2:input$n])  
  
   vecinity_list_values <- list()
   for (i in 1:clients) {
     vecinity_list_values[[i]] <-  input$matriz.distancia[i+1, 2:clients]
     vecinity_list_values[[i]][i] <- Inf
   }   

   vecinity_index_order <- list()
   for (i in 1:clients) {
     vecinity_index_order[[i]] <- order(vecinity_list_values[[i]], decreasing = FALSE)
   }
   
   
   return(vecinity_index_order)
}



calculateTotalDistanceTS <- function(input, alpha, routes_res, type_problem){
  route <- all_routes(routes_res)
  
  cost <- 0
  for (i in 1:(length(route)-1)){
    cost <- cost + input$matriz.distancia[route[i]+1, route[i+1]+1]
  }
  
  ## F(S,M) -- Diversification
  
  FS  <- cost+alpha*calc_penalty(input, routes_res, type_problem)
  
  return(FS)
}

calculateTotalDistanceTS_nopen <- function(input, routes_res){
  route <- all_routes(routes_res)
  
  cost <- 0
  for (i in 1:(length(route)-1)){
    cost <- cost + input$matriz.distancia[route[i]+1, route[i+1]+1]
  }
  
  return(cost)
}




