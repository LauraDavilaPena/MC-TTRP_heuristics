# check_in_tabulist
check_in_tabulist<-function(tabulist, clients, id_route, root1) {
  
  flag_exit <- 0
  for (j in 1:length(clients)) {
    client <- clients[j]
    if (length(tabulist$tabu_list_clients)) {
      for (i in 1:length(tabulist$tabu_list_counters)) {
        if ((tabulist$tabu_list_clients [[i]] == client) && 
            (tabulist$tabu_list_routes [[i]] == id_route) 
             #&& (tabulist$tabu_list_root [[i]] == root1)
            ) {
          flag_exit <- 1
        }
      }
    }
    if (flag_exit) break
    
  }
  
  return(flag_exit)
}

# insert_in_tabu_list
insert_in_tabu_list<-function(clients, id_route, tau, tabulist, root) {
  
  if (length(tabulist$tabu_list_clients) > tabulist$max_size_tabu_list ) {
    index_to_delete <- return_low_counter(tabulist)
    tabulist <- delete_tabu_list_element(tabulist, index_to_delete)
  }
  
  
  
  for (i in 1:length(clients)) {
    client <- clients[i]
    index <- check_element_exist_in_tabu_list(client, id_route, tabulist, root)
    if(index == 0) {
      end_position <- length(tabulist$tabu_list_clients) 
      tabulist$tabu_list_clients [[end_position+1]] <- client
      tabulist$tabu_list_routes  [[end_position+1]] <- id_route
      tabulist$tabu_list_counters[[end_position+1]] <- tau + 1
      tabulist$tabu_list_root[[end_position+1]] <- root
    } else {
      tabulist$tabu_list_counters[[index]] <- tau + 1
    }
  }
  
  
  return(tabulist)
}

check_element_exist_in_tabu_list<-function(client, route, tabulist, root) {
  index <- 0
  if ( length(tabulist$tabu_list_clients) ) {
    for (i in 1:length(tabulist$tabu_list_clients)) {
      if ((tabulist$tabu_list_clients[[i]] == client) &&
          (tabulist$tabu_list_routes[[i]]  == route)  
           #&& (tabulist$tabu_list_root[[i]]  == root) 
          ) {
        index <- i
      }
    }
  }
  return(index)
}

# return_tau
return_tau<-function(n_clients, n_routes) {
  
  UB <- sqrt(n_clients*n_routes)
  LB <- 1
  
  return (floor(runif(1) *UB + LB))
  
}

# update_counters_tabu_list
update_counters_tabu_list<-function(tabulist) {
  
  if(length(tabulist$tabu_list_counters)) {
    concat_index <- c(0)
    for (i in 1:length(tabulist$tabu_list_counters)) {
      tabulist$tabu_list_counters[[i]] <- tabulist$tabu_list_counters[[i]] - 1
      if (tabulist$tabu_list_counters[[i]] > 0 ) concat_index <- c(concat_index, i)
    }
    concat_index <- concat_index[2:length(concat_index)]
    
    if (is.na(concat_index[1])) tabulist <- create_tabu_list()
    else {
      tabulist$tabu_list_clients  <- tabulist$tabu_list_clients[concat_index]
      tabulist$tabu_list_routes   <- tabulist$tabu_list_routes[concat_index]
      tabulist$tabu_list_counters <- tabulist$tabu_list_counters[concat_index]
      tabulist$tabu_list_root     <- tabulist$tabu_list_root[concat_index]
      size <- length(tabulist$tabu_list_counters)
    }
  }
  

  return(tabulist)
}

# print tabu list
print_tabu_list<-function(tabulist){
  print( tabulist$max_size_tabu_list )
  if(length(tabulist$tabu_list_counters)) {
    for (i in 1:length(tabulist$tabu_list_counters)) {
      
      print(paste0("client: ",  tabulist$tabu_list_clients[[i]],
                   " route: ",   tabulist$tabu_list_routes[[i]],
                   " root: ", tabulist$tabu_list_root[[i]],
                   " counter: ", tabulist$tabu_list_counters[[i]]
      ))
      
      
    }
  }
  
}


# create_tabu_list
create_tabu_list<-function(){
  tabulist <- list()
  tabulist$max_size_tabu_list <- 100
  tabulist$tabu_list_clients<- list()
  tabulist$tabu_list_routes<- list()
  tabulist$tabu_list_counters<- list()
  tabulist$tabu_list_root<- list()
  
  return(tabulist)
}

# return_low_counter
return_low_counter<-function(tabulist){
  min<- Inf
  index <- -1
  for (i in 1:length(tabulist$tabu_list_counters)) {
    if (tabulist$tabu_list_counters[[i]] < min) {
      index <- i
    }
  }
  
  return (index)
}

# delete_tabu_list_element
delete_tabu_list_element<-function(tabulist, index_to_delete) {
  tabulist$tabu_list_clients  <- tabulist$tabu_list_clients[-index_to_delete]
  tabulist$tabu_list_routes   <- tabulist$tabu_list_routes[-index_to_delete]
  tabulist$tabu_list_counters <- tabulist$tabu_list_counters[-index_to_delete]
  tabulist$tabu_list_root     <- tabulist$tabu_list_root[-index_to_delete]
  
  return(tabulist)
}

# create_table_freq
create_table_freq<-function(size_clients, n_routes){
  table_frec <- matrix(0, ncol = n_routes, nrow = (size_clients-1)) 
  
  return(table_frec)
}

# update_table_freq
update_table_freq<-function(table_frec, client, id_route){
  #print("UPDATE -> ")
  #print(client)
  #print(id_route)
  table_frec[as.numeric(client), as.numeric(id_route)] <- table_frec[as.numeric(client), as.numeric(id_route)] + 1
  return(table_frec)
}

# return_table_freq
return_table_freq<-function(table_frec, client, id_route){
  return(table_frec[as.numeric(client), as.numeric(id_route)])
}

# init_tabulist_data
init_tabulist_data<-function(input, initial_solution, penalty){
  
  # tabu search data
  tabulist_data <- list()
  tabulist_data$tabulist <- create_tabu_list()
  tabulist_data$table_freq <- create_table_freq(input$n, length(initial_solution))
  tabulist_data$penalty_capacity <- penalty
  
  return(tabulist_data)
  
}
