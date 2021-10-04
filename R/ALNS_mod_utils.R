
select_removal_heuristic <- function(select, p_roulette_wheel, removal_matrix, n, phi, input, current_solution){

  if(select <= p_roulette_wheel[1]){
    selected_removal <- 1
    removal_matrix[1,1] <- removal_matrix[1,1] + 1 # numero de veces que hemos escogido esta heuristica de eliminacion
    removed_clients <- random_removal(n, phi, current_solution)
    
  }else if(select <= p_roulette_wheel[2]){
    selected_removal <- 2
    removal_matrix[1,2] <- removal_matrix[1,2] + 1
    removed_clients <- worst_removal(input, current_solution, phi)
      
  }else if(select <= p_roulette_wheel[3]){
    selected_removal <- 3
    removal_matrix[1,3] <- removal_matrix[1,3] + 1
    removed_clients <- shaw_removal(input, n, phi)
    
  }else if(select <= p_roulette_wheel[4]){
    selected_removal <- 4
    removal_matrix[1,4] <- removal_matrix[1,4] + 1
    removed_clients <- pb_removal(input, n, phi)
    
  }else if(select <= p_roulette_wheel[5]){
    selected_removal <- 5
    removal_matrix[1,5] <- removal_matrix[1,5] + 1
    removed_clients <- neighborhood_removal(input, phi, current_solution)
    
  }else if(select <= p_roulette_wheel[6]){
    selected_removal <- 6
    removal_matrix[1,6] <- removal_matrix[1,6] + 1
    removed_clients <- shaw.ali_removal(input, n, phi, current_solution)
  }else if(select <= p_roulette_wheel[7]){
    selected_removal <- 7
    removal_matrix[1,7] <- removal_matrix[1,7] + 1
    removed_clients <- worst_extended_removal(input, current_solution, phi)
  }
  
  #which_removal <- Removal_Heuristics[selected_removal]
  

  return(list(selected_removal = selected_removal, removal_matrix = removal_matrix, removed_clients = removed_clients))
}



# Falta añadir mas metodos de insercion
select_insertion_heuristic <- function(select, p_roulette_wheel, insertion_matrix, n, phi, input, candidate_solution,
                                       solution_removal, result, problem_type){
  
  if(select <= p_roulette_wheel[1] ){
    selected_insertion <- 1
    insertion_matrix[1,1] <- insertion_matrix[1,1] + 1 # numero de veces que hemos escogido esta heuristica de eliminacion
    
    # customers_insertion_greedy() -> esta funcion deberia ser la parte de "insercion" 
    # y "reconstruccion" de ruta factible de la actual perturbacion
    
    solution <- customers_insertion(input, result, solution_removal$info_ini, solution_removal$info_after,
                                                     candidate_solution, solution_removal$sol , solution_removal$no_route_left,
                                                     problem_type, 0, selected_insertion)
  }else if(select <= p_roulette_wheel[2]){
    selected_insertion <- 2
       insertion_matrix[1,2] <- insertion_matrix[1,2] + 1
       solution <- customers_insertion(input, result, solution_removal$info_ini, solution_removal$info_after,
                                              candidate_solution, solution_removal$sol , solution_removal$no_route_left,
                                              problem_type, 0, selected_insertion)
       
  
  }else if(select <= p_roulette_wheel[3]){
    selected_insertion <- 3
       insertion_matrix[1,3] <- insertion_matrix[1,3] + 1
       solution <- customers_insertion(input, result, solution_removal$info_ini, solution_removal$info_after,
                                       candidate_solution, solution_removal$sol , solution_removal$no_route_left,
                                       problem_type, 0, selected_insertion)
    
  }else if(select <= p_roulette_wheel[4]){ #Regret2
     selected_insertion <- 4
     insertion_matrix[1,4] <- insertion_matrix[1,4] + 1
     solution <- customers_insertion_regret2(input, result, solution_removal$info_ini, solution_removal$info_after,
                                             candidate_solution, solution_removal$sol , solution_removal$no_route_left,
                                             problem_type, 0, phi)
  
  
   }#else if(select <= p_roulette_wheel[5] ){ #Regret3
  #   selected_insertion <- 5
  #   insertion_matrix[1,5] <- insertion_matrix[1,5] + 1
  #   solution <- customers_insertion_regret3() 
  #
  # }
  

  
  return(list(selected_insertion = selected_insertion, insertion_matrix = insertion_matrix, solution = solution))
}



update_removal_weights <- function(iter, removal_matrix, selected_removal, incumbent_improved, best_cost_improved, cost_worsened){
   # This values are taken from Alinaghian and Shokouhi (Omega, 2018)
   sigma1 <- 20
   sigma2 <- 10
   sigma3 <- 2
   
   if(best_cost_improved){
     removal_matrix[3,selected_removal] <-  removal_matrix[3,selected_removal] + sigma1
   }else if(incumbent_improved){
     removal_matrix[3,selected_removal] <-  removal_matrix[3,selected_removal] + sigma2
   }else if(cost_worsened){
     removal_matrix[3,selected_removal] <-  removal_matrix[3,selected_removal] + sigma3
   }
   
   
   if(iter > 100){
     lambda <- 0.1
     
     removal_matrix[2,selected_removal] <-  lambda*(removal_matrix[3,selected_removal]/removal_matrix[1,selected_removal]) + 
       (1-lambda)*removal_matrix[2,selected_removal]
     
     removal_matrix[4,] <- removal_matrix[2,]/sum(removal_matrix[2,])
   }
  
     
   return(removal_matrix)
 }



update_insertion_weights <- function(iter, insertion_matrix, selected_insertion, incumbent_improved, best_cost_improved, cost_worsened){
   
   # This values are taken from Alinaghian and Shokouhi (Omega, 2018)
   sigma1 <- 20
   sigma2 <- 10
   sigma3 <- 2
   
   if(best_cost_improved){
     insertion_matrix[3,selected_insertion] <-  insertion_matrix[3,selected_insertion] + sigma1
   }else if(incumbent_improved){
     insertion_matrix[3,selected_insertion] <-  insertion_matrix[3,selected_insertion] + sigma2
   }else if(cost_worsened){
     insertion_matrix[3,selected_insertion] <-  insertion_matrix[3,selected_insertion] + sigma3
   }
   
   if(iter > 100){
     lambda <- 0.1
     insertion_matrix[2,selected_insertion] <-  lambda*(insertion_matrix[3,selected_insertion]/insertion_matrix[1,selected_insertion]) + 
       (1-lambda)*insertion_matrix[2,selected_insertion]
     
     insertion_matrix[4,] <- insertion_matrix[2,]/sum(insertion_matrix[2,])
     
   }
   
   return(insertion_matrix)
 }
 
client_type <- function(input, i){
   if(i >= 1 && i <= input$n1){
     type <- "vc"
   }else if(i > input$n1 && i <= (input$n-1)){
     type <- "tc"
   }else{
     print(paste(i, "is not a client"))
   }
   return(type)
 }
 
