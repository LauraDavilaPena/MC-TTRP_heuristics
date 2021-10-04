#ALNS
ALNS_core<-function(input, result, initial_solution, init_time, type_problem, seed) {
    print("INIT") 
    set.seed(seed)
  
    # init parameters
    init_time <- Sys.time()

    stopping_conditions <- 0
    iter <- 1

    # initial solution
    current_solution <- initial_solution
    current_cost <- calculateTotalDistance(input, all_routes(initial_solution))
  
    # removal and insertion heuristics
    Removal_Heuristics <- c("Random", "Worst", "Shaw", "PB", "Neigh", "ShawAli", "WorstExt") 
    Insertion_Heuristics <- c("Greedy", "Random", "pBest", "Regret2") 
    
    removal_weights <- rep(1, length(Removal_Heuristics)) # w_1: random removal, w_2: worst_removal, w_3: shaw_removal
    insertion_weights <- rep(1, length(Insertion_Heuristics)) 
    
    # Create removal_matrix
    removal_matrix <- matrix(0, nrow=4, ncol=length(Removal_Heuristics))
    removal_matrix[2,] <- removal_weights
    removal_matrix[4,] <- removal_matrix[2,]/(sum(removal_matrix[2,]))
    
    colnames(removal_matrix) <- Removal_Heuristics
    rownames(removal_matrix) <- c("Times_selected", "Weight", "Scores", "Probability")
    
    #removal_matrix
    
    # Create insertion_matrix
    insertion_matrix <- matrix(0, nrow=4, ncol=length(Insertion_Heuristics))
    insertion_matrix[2,] <- insertion_weights
    insertion_matrix[4,] <- insertion_matrix[2,]/(sum(insertion_matrix[2,]))
    
    colnames(insertion_matrix) <- Insertion_Heuristics
    rownames(insertion_matrix) <- c("Times_selected", "Weight", "Scores", "Probability")
    
    #insertion_matrix
    
    n <- input$n - 1
    
    bestsolution <- initial_solution 
    bestcost <- current_cost
    
    #temp <- 0.1 #current_cost # initial temperature (we may want to calibrate it; I followed Alinaghian and Shokouhi)
    # Probar a empezar coa temp de Parragh y Cordeau: -(0.005/ln(0.5))*f(s)
    temp <- -(0.005/log(0.5))*current_cost
    
    # grande -> -(0.01/log(0.5))*current_cost
    # pequeno -> -(0.001/log(0.5))*current_cost
    # isto significa que no 50% dos casos (ie, cunha probabilidade de 0.5), 
    # aceptamos solucions que empeoren un 5% o custo da solucion actual
    
    # print init output
   # print(paste0("fobj ", bestcost, " iter ", 0, " time ", difftime(Sys.time(), init_time, units = "secs")))
    
    no_improv_counter <- 0
    #tabu_clients <- numeric()
    tabu_removal <- numeric()
    tabu_insertion <- numeric()
    
    print("while") 
    while (!stopping_conditions) {  #(iter<=174){  
      #print(iter)
      
      candidate_solution <- current_solution
      
      #save(candidate_solution, file="solution_P4_ini.RData")
      #print("current solution saved")
      #readline()
      
      
      # phi <- sample(ceiling(0.01*n):ceiling(0.08*n),1) # randomly selected degree of destroy
      
      # probabilities from a right-skewed beta distribution
      # x_beta <- seq(0,1,length = length((ceiling(0.01*n)-1):(ceiling(0.08*n)+1)))
      # y_beta <- dbeta(x_beta, shape1 = 2, shape2 = 4) 
      # phi_probs <- y_beta/sum(y_beta)
      #phi <- sample(ceiling(0.01*n):ceiling(0.08*n),1, prob = phi_probs[2:(length(phi_probs)-1)]) # randomly selected degree of destroy
    
      # phi = ceiling(50/4*rbeta(1, 1.5, 6, ncp = 0))
      
      phi = ceiling(ceiling(0.08*n)*rbeta(1, 2, 5, ncp = 0))
      # select which destruction method will be employed
      select = runif(1)
      
      if(length(tabu_removal)){
        shared_prob <- removal_matrix[4,tabu_removal]/(length(removal_matrix[4,])-1)
        removal_heuristics_prob <- removal_matrix[4,]
        removal_heuristics_prob[tabu_removal] <- -1
        removal_heuristics_prob[-tabu_removal] <- removal_matrix[4,-tabu_removal] + rep(shared_prob, length(removal_matrix[4,])-1)
        #removal_heuristics_prob
        cumprobs <- cumsum(removal_heuristics_prob[-tabu_removal])
        removal_heuristics_prob[-tabu_removal] <- cumprobs
        
        p_roulette_wheel_removal <- removal_heuristics_prob
        
      }else{
        p_roulette_wheel_removal <- cumsum(removal_matrix[4,])
      }
      
      removal_heuristic <- select_removal_heuristic(select, p_roulette_wheel_removal, removal_matrix, n, phi, input, candidate_solution)
      selected_removal = removal_heuristic$selected_removal
      removal_matrix = removal_heuristic$removal_matrix
      removed_clients = removal_heuristic$removed_clients
      #print(removed_clients)
      
      #tabu_clients <- removed_clients # mirar 
      tabu_removal <- selected_removal
      
      # We destroy the solution, by removing "removed_clients" from it 
      solution_removal <- customers_removal(input, candidate_solution, problem_type, removed_clients)
      # esta funcion es la parte de "eliminacion" de la perturbacion
      
      # select which insertion heuristics will be used   
    if(length(tabu_insertion)){
      shared_prob <- insertion_matrix[4,tabu_insertion]/(length(insertion_matrix[4,])-1)
      insertion_heuristics_prob <- insertion_matrix[4,]
      insertion_heuristics_prob[tabu_insertion] <- -1
      insertion_heuristics_prob[-tabu_insertion] <- insertion_matrix[4,-tabu_insertion] + rep(shared_prob, length(insertion_matrix[4,])-1)
      #insertion_heuristics_prob
      cumprobs <- cumsum(insertion_heuristics_prob[-tabu_insertion])
      insertion_heuristics_prob[-tabu_insertion] <- cumprobs
      
      p_roulette_wheel_insertion <- insertion_heuristics_prob
      
    }else{
      p_roulette_wheel_insertion <- cumsum(insertion_matrix[4,])
    }

      insertion_heuristic <- select_insertion_heuristic(select, p_roulette_wheel_insertion, insertion_matrix, n, phi, input, candidate_solution,
                                                        solution_removal, result, problem_type)
      selected_insertion = insertion_heuristic$selected_insertion
      insertion_matrix = insertion_heuristic$insertion_matrix
      
      candidate_solution <- insertion_heuristic$solution
      tabu_insertion <- selected_insertion
      
      #save(candidate_solution, file="solution_P4_fin.RData")
      #print("candidate solution saved")
      #readline()

      # improvement
      candidate_solution <- result_improvement(input, candidate_solution, type_problem)
      candidate_cost <- calculateTotalDistance(input, all_routes(candidate_solution))
      
      
      
      incumbent_improved <- 0
      best_cost_improved <- 0
      cost_worsened <- 0
      
      # Criterio de aceptacion -> simulated annealing
      
      if(candidate_cost < current_cost){
        current_solution <- candidate_solution
        current_cost <- candidate_cost
        incumbent_improved <- 1

      }else{
        diff <- candidate_cost - current_cost
        
        if(runif(1) < exp((-diff)/temp)){
          #print(paste0("cost_worsened   th", exp((-diff)/temp), " diff  ", diff, "  temp ", temp, " candidate_cost ", candidate_cost, " current_cost ", current_cost, " best ", bestcost))
          current_solution <- candidate_solution
          current_cost <- candidate_cost
          cost_worsened <- 1
	        #readline()
        } else {
          #print(paste0("reject   th", exp((-diff)/temp), " diff  ", diff, "  temp ", temp, " candidate_cost ", candidate_cost, " current_cost ", current_cost, " best ", bestcost))
        }
      }
      
      # Update best solution

      if (current_cost < bestcost) {
        bestsolution <- current_solution 
        bestcost <- current_cost
        best_cost_improved <- 1 
        incumbent_improved <- 0
        print(paste("IMPROVEMENT, best cost", bestcost, "iter", iter, "time", difftime(Sys.time(), init_time, units = "secs"),"s)"))
      }else{
        no_improv_counter <- no_improv_counter + 1
      }
      
      
      # Update weights of removal and insertion heuristics
      
      removal_matrix <- update_removal_weights(iter, removal_matrix, selected_removal, incumbent_improved, best_cost_improved, cost_worsened)
      
      insertion_matrix <- update_insertion_weights(iter, insertion_matrix, selected_insertion, incumbent_improved, best_cost_improved, cost_worsened)
      

      # update temperature
      temp = 0.999*temp
     
      # check stopping conditions
      stopping_conditions <- check_stoppping_conditions(iter, init_time, bestcost, input)
      
      iter <- iter + 1
      
      
    }
    
    total_time <- difftime(Sys.time(), init_time, units = "secs")
  
 return(list(bestcost=bestcost, bestsolution=bestsolution, time=total_time))
}


