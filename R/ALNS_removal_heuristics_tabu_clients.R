# Removal heuristics


random_removal_tabuc <- function(n, phi, tabu_clients){
  
  removed_candidates <- 1:n
  if(length(which(removed_candidates %in% tabu_clients))>0){
    removed_candidates <- removed_candidates[-which(removed_candidates %in% tabu_clients)]
  }
  
  removed_clients <- sample(removed_candidates, phi)
  
  return(removed_clients)
}



worst_removal_tabuc <-  function(input, current_solution, phi, tabu_clients){
  clients <- input$n-1
  rutas <- all_routes(current_solution)
  dist <- numeric(clients)
  
  for(i in 1:clients){
    if(sum(i==rutas)>1){
      i_pos <- which(i==rutas)
      i_times <- length(i_pos)
      i_dist <- numeric(i_times-1)
      for (j in 1:(i_times-1)){
        i_dist <- c(i_dist, input$matriz.distancia[rutas[i_pos[j]-1]+1, rutas[i_pos[j]]+1] 
                            + input$matriz.distancia[rutas[i_pos[j+1]]+1, rutas[i_pos[j+1]+1]+1])
      }
      dist[i] <- max(i_dist)
    }else{
    i_pos <- which(i==rutas)
    dist[i] <- input$matriz.distancia[rutas[i_pos-1]+1, rutas[i_pos]+1] + input$matriz.distancia[rutas[i_pos]+1, rutas[i_pos+1]+1]
    }
  }

  #removed_clients <- rev(order(dist))[1:phi]
  ordered_clients <- rev(order(dist))
  if(length(which(ordered_clients %in% tabu_clients))>0){
    removed_clients <- ordered_clients[-which(ordered_clients %in% tabu_clients)][1:phi]
  }else{
    removed_clients <- ordered_clients[1:phi]
  }
  
  return(removed_clients)
}



shaw_removal_tabuc <- function(input, n, phi, tabu_clients){

  # first we randomly choose a customer
  
  removed_candidates <- 1:n
  if(length(which(removed_candidates %in% tabu_clients))>0){
    removed_candidates <- removed_candidates[-which(removed_candidates %in% tabu_clients)]
  }
  chosen_client <- sample(removed_candidates, 1)
  
  remaining_clients <- removed_candidates[-which(removed_candidates == chosen_client)]
  
  # we calculate all customers' relations wrt the chosen_client
  
  clients_distances <- input$matriz.distancia[-1,-1] # we omit the depot
  
  if(length(tabu_clients)){
    clients_distances <- clients_distances[-tabu_clients,-tabu_clients]
  }
  
  cmax <- max(clients_distances) # the largest distance between two customers (we do not consider
                                 # tabu customers in order to compute cmax)

  
  if(problem_type == "MCTTRP"){
    clients_demands <- input$matriz.demandas[-1,]
    clients_demands <- cbind(1:n, clients_demands)
    if(length(tabu_clients)){
      clients_demands <- clients_demands[-tabu_clients,]
    }
    qmax <- max(rowSums(clients_demands[,-1])) # the highest demands of all non tabu customers
  }else if(problem_type == "TTRP"){
    clients_demands <- input$vector.demandas[-1]
    if(length(tabu_clients)){
      clients_demands <- clients_demands[-tabu_clients]
    }
    qmax <- max(clients_demands) # the highest demands of all non tabu customers
  }
  
  
  # we choose the most related phi-1 clients
  relation <- numeric(length(remaining_clients))
  type <- client_type(input,chosen_client)
 
  for (i in 1:length(remaining_clients)){
   if(type == client_type(input,remaining_clients[i])){
     yij = 0
   }else{
     yij = 1
   }
    c <- input$matriz.distancia[chosen_client+1, remaining_clients[i]+1]
    
    if(problem_type == "MCTTRP"){
      q <- abs(sum(input$matriz.demandas[chosen_client+1,]) - sum(input$matriz.demandas[remaining_clients[i]+1,]))
    }else if(problem_type == "TTRP"){
      q <- abs(input$vector.demandas[chosen_client+1] - input$vector.demandas[remaining_clients[i]+1])
    }
    
    # we may need to calibrate the following parameters (by experimentation?)
    # Alinaghian (but they use a different approach since there is no a TTRP):
    fc = 0.5 
    fq = 0.25
    ft = 0.25
      
    relation[i] <- fc*(c/cmax) + fq*(q/qmax) + ft*yij # Derigs et al. (2013)
      
  }
  
  # we choose the phi clients most related to chosen_client (that is, the ones that have
  # lower values of "relation")
  removed_clients <- remaining_clients[order(relation)][1:(phi-1)]
  
  return(removed_clients)
}


# Proximity-based removal
pb_removal_tabuc <- function(input, n, phi, tabu_clients){
  
  # first we randomly choose a customer
  removed_candidates <- 1:n
  if(length(which(removed_candidates %in% tabu_clients))>0){
    removed_candidates <- removed_candidates[-which(removed_candidates %in% tabu_clients)]
  }
  chosen_client <- sample(removed_candidates, 1)
  
  remaining_clients <- removed_candidates[-which(removed_candidates == chosen_client)]
  
  # we calculate all customers' distances wrt the chosen_client
  
  clients_distances <- input$matriz.distancia[-1,-1] # we omit the depot
  
  if(length(tabu_clients)){
    clients_distances <- clients_distances[-tabu_clients,-tabu_clients]
  }
  
  
  # we choose the closest phi-1 clients
  dist <- numeric(length(remaining_clients))

  for (i in 1:length(remaining_clients)){
    dist[i] <- input$matriz.distancia[chosen_client+1, remaining_clients[i]+1]
  }
  
  # we choose the phi clients most related to chosen_client (that is, the ones that have
  # lower values of "relation")
  removed_clients <- remaining_clients[order(dist)][1:(phi-1)]
  
  return(removed_clients)
}



neighborhood_removal_tabuc <- function(input, phi, current_solution, tabu_clients){
  
  clients <- input$n-1
  rutas <- all_routes(current_solution)
  dist <- numeric(clients)
  
  for(i in 1:clients){
    if(sum(i==rutas)>1){
      i_pos <- which(i==rutas)
      i_times <- length(i_pos)
      i_dist <- numeric(i_times-1)
      for (j in 1:(i_times-1)){
        i_dist <- c(i_dist, input$matriz.distancia[rutas[i_pos[j]-1]+1, rutas[i_pos[j]]+1] 
                    + input$matriz.distancia[rutas[i_pos[j+1]]+1, rutas[i_pos[j+1]+1]+1])
      }
      dist[i] <- max(i_dist)
    }else{
      i_pos <- which(i==rutas)
      dist[i] <- input$matriz.distancia[rutas[i_pos-1]+1, rutas[i_pos]+1] + input$matriz.distancia[rutas[i_pos]+1, rutas[i_pos+1]+1]
    }
  }
  
  calculateTotalDistance(input, rutas)
  
  n_clients <- numeric(length(current_solution))
  average_cost <- numeric(length(current_solution))
  for(i in 1:length(current_solution)){
    n_clients[i] <- length(unique(current_solution[[i]]$route))-1
    average_cost[i] = current_solution[[i]]$cost/n_clients[i]
  }
  
  diff_cost <- numeric(clients)
  for(j in 1:clients){
    r_ind <- route_of_client(j,current_solution)$index
    diff_cost[j] <- abs(average_cost[r_ind] - dist[j])
  }
  
  
  #removed_clients <- rev(order(dist))[1:phi]
  ordered_clients <- rev(order(diff_cost))
  if(length(which(ordered_clients %in% tabu_clients))>0){
    removed_clients <- ordered_clients[-which(ordered_clients %in% tabu_clients)][1:phi]
  }else{
    removed_clients <- ordered_clients[1:phi]
  }
  
  return(removed_clients)
}




shaw.ali_removal_tabuc <- function(input, n, phi, current_solution, tabu_clients){
  
  # first we randomly choose a customer
  removed_candidates <- 1:n
  if(length(which(removed_candidates %in% tabu_clients))>0){
    removed_candidates <- removed_candidates[-which(removed_candidates %in% tabu_clients)]
  }
  chosen_client <- sample(removed_candidates, 1)
  
  remaining_clients <- removed_candidates[-which(removed_candidates == chosen_client)]
  
  # we calculate all customers' relations wrt the chosen_client

  chosen_route <- route_of_client(chosen_client, current_solution)$index
  relation <- numeric(length(remaining_clients))
  
  for(i in 1:length(remaining_clients)){
    if( chosen_route ==  route_of_client(i, current_solution)$index){
      lij = -1
    }else{
      lij = 0
    }
  
  
  # we choose the most related phi-1 clients

    c <- input$matriz.distancia[chosen_client+1, remaining_clients[i]+1]
    
    # we may need to calibrate the following parameters (by experimentation?)
    # Alinaghian (but they use a different approach since there is no a TTRP):
    fi1 = 0.5 
    fi2 = 0.5

    relation[i] <- fi1*c + fi2*lij # Alinaghian and Shokouhui (2018)
  }
  
  # we choose the phi clients most related to chosen_client (that is, the ones that have
  # lower values of "relation")
  removed_clients <- remaining_clients[order(relation)][1:(phi-1)]
  
  return(removed_clients)
}






