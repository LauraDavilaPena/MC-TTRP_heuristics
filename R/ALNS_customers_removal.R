
customers_removal <- function(input, initial_solution, problem_type, removed_clients){
  
  intermediate_solution <- initial_solution
  # Una vez los tenemos, vemos en que rutas estan, y agrupamos aquellos que esten en las mismas rutas
  routes_indices <- numeric()
  for(i in 1:length(removed_clients)){
    routes_indices[i] <- route_of_client(removed_clients[i], initial_solution)$index
  }
  
  aggregated_indices <- list()
  for(i in 1:length(removed_clients)){
    aggregated_indices[[i]] <- which(routes_indices == routes_indices[i])
  }
  
  aggregated_indices <- aggregated_indices[!duplicated(aggregated_indices)]
  
  aggregated_clients <- list()
  for (i in 1:length(aggregated_indices)){
    aggregated_clients[[i]] <- removed_clients[aggregated_indices[[i]]]
  }
  
  aggregated_routes <- list()
  for(i in 1:length(aggregated_clients)){
    aggregated_routes[[i]] <- route_of_client(aggregated_clients[[i]][1], initial_solution)$route
  }
  
  aggregated_routes_index <- list()
  for(i in 1:length(aggregated_clients)){
    aggregated_routes_index[[i]] <- route_of_client(aggregated_clients[[i]][1], initial_solution)$index
  }
  
  # Nuevo 04/05/2021
  aggregated_parking <- list()
  for(i in 1:length(aggregated_clients)){
    aggregated_parking[[i]] <- numeric()
    for(j in 1:length(aggregated_clients[[i]]) ){
      aggregated_parking[[i]][j] <- client_is_parking(aggregated_clients[[i]][j], initial_solution)
    }
  }
  
  aggregated_list_info <- list(aggregated_clients = aggregated_clients, aggregated_routes = aggregated_routes, aggregated_routes_index = aggregated_routes_index, aggregated_parking = aggregated_parking)
  #print(aggregated_list_info)
  
  # Vamos eliminando secuencialmente a los clientes removed_clients de la ruta en la que estan
  aggregated_list_info_after_removal <- aggregated_list_info
  new_routes_after_removal <- list()
  no_route_left <- numeric()
  nr <- 1
  
  for(i in 1:length(aggregated_list_info_after_removal$aggregated_routes)){
    for (j in 1:length(aggregated_list_info_after_removal$aggregated_clients[[i]])){
      change_park <- 0
      if(intermediate_solution[[aggregated_list_info_after_removal$aggregated_routes_index[[i]] ]]$type == "CVR"){
        if(client_in_main_tour(aggregated_list_info_after_removal$aggregated_clients[[i]][j], intermediate_solution)==1){
          if(client_is_parking(aggregated_list_info_after_removal$aggregated_clients[[i]][j], intermediate_solution)==1){
            # Nuevo 04/05/2021 para el caso en el que el cliente sea parking
            # aplicamos a seguinte funcion externa que debe devolver intermediate_solution
            delete_parking <- delete_parking_solution(input, aggregated_list_info_after_removal$aggregated_clients[[i]][j],
                                                      aggregated_list_info_after_removal$aggregated_routes_index[[i]],
                                                      intermediate_solution, problem_type, aggregated_list_info_after_removal$aggregated_clients[[i]],
                                                      aggregated_list_info_after_removal$aggregated_parking[[i]])
            intermediate_solution <- delete_parking$intermediate_solution
            aggregated_list_info_after_removal$aggregated_parking[[i]] <- delete_parking$agg_parking
            change_park <- delete_parking$change_park
            # En este caso la siguiente ruta ya esta completa y no va a haber que cambiar nada
            # (por eso pongo flag_park <- 1)
            new_routes_after_removal[[i]] <- intermediate_solution[[ aggregated_list_info_after_removal$aggregated_routes_index[[i]] ]]$route
            
          }else{
            #si el cliente esta en el main tour, hago lo que tenia 
            # Basicamente aqui lo que hago es: si la ruta es de tipo CVR y el cliente a eliminar esta en el main tour, entonces el "GENI_US" se lo voy a aplicar 
            # solo al main_tour. Pero luego tengo que reconstruir
            # la ruta completa, añadiendo los subtours en el orden en que estaban
            new_routes_after_removal[[i]] <- GENI_US(input, intermediate_solution[[aggregated_list_info_after_removal$aggregated_routes_index[[i]] ]]$main_tour, 
                                                     aggregated_list_info_after_removal$aggregated_clients[[i]][j])
            
            intermediate_solution[[aggregated_list_info_after_removal$aggregated_routes_index[[i]] ]]$main_tour <- new_routes_after_removal[[i]]
            subtours <- intermediate_solution[[aggregated_list_info_after_removal$aggregated_routes_index[[i]] ]]$subtours
            
            for(k in 1:length(subtours)){
              kk <- 1
              if(length(which(new_routes_after_removal[[i]]==subtours[[k]]$root)) > 1){
                kk <- sum(new_routes_after_removal[[i]]  == subtours[[k]]$root)
              }
              new_routes_after_removal[[i]] <- c(new_routes_after_removal[[i]][1:which(new_routes_after_removal[[i]]==subtours[[k]]$root)[kk] ], subtours[[k]]$tour[2:(length(subtours[[k]]$tour))], 
                                                 new_routes_after_removal[[i]][(which(new_routes_after_removal[[i]]==subtours[[k]]$root)[kk]+1): length(new_routes_after_removal[[i]])] ) 
              
            }
            
            intermediate_solution[[aggregated_list_info_after_removal$aggregated_routes_index[[i]] ]]$route <- new_routes_after_removal[[i]]
          }
        }
        else{ 
          # en cambio, si el cliente esta en el subtour, procedo asi:
          # primero debo mirar cual es el subtour en el que se encuentra
          #print(paste0("DELETE -> ", aggregated_list_info_after_removal$aggregated_routes_index[[i]]))
          subtours <- intermediate_solution[[aggregated_list_info_after_removal$aggregated_routes_index[[i]] ]]$subtours
          for(k in 1:length(subtours)){
            if(sum(subtours[[k]]$tour == aggregated_list_info_after_removal$aggregated_clients[[i]][j])>0){
              subtour_origin <- subtours[[k]]
              subtour_index <- k
            }
          }
          tour_origin <- subtour_origin$tour
          if(length(tour_origin) == 3){ # si el subtour tuviese un unico cliente, entonces despues de eliminarlo nos quedamos sin subtour
            intermediate_solution[[aggregated_list_info_after_removal$aggregated_routes_index[[i]] ]]$subtours <-  intermediate_solution[[aggregated_list_info_after_removal$aggregated_routes_index[[i]] ]]$subtours[-subtour_index] 
            rclient_index <- which(intermediate_solution[[aggregated_list_info_after_removal$aggregated_routes_index[[i]] ]]$route == aggregated_list_info_after_removal$aggregated_clients[[i]][j])
            new_routes_after_removal[[i]] <- intermediate_solution[[aggregated_list_info_after_removal$aggregated_routes_index[[i]] ]]$route[-c(rclient_index,rclient_index+1)]
            if(length(subtours)==1){ # si ademas ese fuese el unico subtour de nuestra CVR, despues de eliminarlo tendriamos una PVR
              intermediate_solution[[aggregated_list_info_after_removal$aggregated_routes_index[[i]] ]]$type = "PVR"
            }
          }else{
            new_subtour_after_removal <- GENI_US(input, tour_origin, aggregated_list_info_after_removal$aggregated_clients[[i]][j])
            subtours_after_removal <- subtours
            subtours_after_removal[[subtour_index]]$tour <- new_subtour_after_removal
            if(aggregated_list_info_after_removal$aggregated_clients[[i]][j]<=input$n1){
              vc_index <- which(subtours_after_removal[[subtour_index]]$vc_clients == aggregated_list_info_after_removal$aggregated_clients[[i]][j])
              subtours_after_removal[[subtour_index]]$vc_clients <- subtours_after_removal[[subtour_index]]$vc_clients[-vc_index] 
            }else{
              tc_index <- which(subtours_after_removal[[subtour_index]]$tc_clients == aggregated_list_info_after_removal$aggregated_clients[[i]][j])
              subtours_after_removal[[subtour_index]]$tc_clients <- subtours_after_removal[[subtour_index]]$tc_clients[-tc_index] 
            }
            subtours_after_removal[[subtour_index]]$length <- subtours_after_removal[[subtour_index]]$length - 1
            intermediate_solution[[aggregated_list_info_after_removal$aggregated_routes_index[[i]] ]]$subtours <- subtours_after_removal
            new_routes_after_removal[[i]] <- intermediate_solution[[aggregated_list_info_after_removal$aggregated_routes_index[[i]] ]]$main_tour
            for(ind_k in 1:length(subtours_after_removal)){
              kk <- 1
              if(length(which(new_routes_after_removal[[i]]==subtours_after_removal[[ind_k]]$root)) > 1){
                kk <- sum(new_routes_after_removal[[i]]  == subtours_after_removal[[ind_k]]$root)
              }
              new_routes_after_removal[[i]] <- c(new_routes_after_removal[[i]][1:which(new_routes_after_removal[[i]]==subtours_after_removal[[ind_k]]$root)[kk] ], subtours_after_removal[[ind_k]]$tour[2:(length(subtours_after_removal[[ind_k]]$tour))], 
                                                 new_routes_after_removal[[i]][(which(new_routes_after_removal[[i]]==subtours_after_removal[[ind_k]]$root)[kk]+1): length(new_routes_after_removal[[i]])] ) 
              
            }
          }
          
          intermediate_solution[[aggregated_list_info_after_removal$aggregated_routes_index[[i]] ]]$route <- new_routes_after_removal[[i]]
          #print(intermediate_solution[[aggregated_list_info_after_removal$aggregated_routes_index[[i]] ]]$route)
        }
      }
      else{
        if( length(aggregated_list_info_after_removal$aggregated_routes[[i]]) == 3 ){
          no_route_left[nr] <- aggregated_list_info_after_removal$aggregated_routes_index[[i]]
          nr <- nr + 1
        }
        new_routes_after_removal[[i]] <- GENI_US(input, aggregated_list_info_after_removal$aggregated_routes[[i]], aggregated_list_info_after_removal$aggregated_clients[[i]][j])
      }
      
      aggregated_list_info_after_removal$aggregated_routes[[i]] <- new_routes_after_removal[[i]]
      
      
      
      # lo que antes era del segundo bucle, 05/05/2021
      intermediate_solution[[ aggregated_list_info_after_removal$aggregated_routes_index[[i]] ]]$route <- aggregated_list_info_after_removal$aggregated_routes[[i]]
      intermediate_solution[[ aggregated_list_info_after_removal$aggregated_routes_index[[i]] ]]$cost <- calculateTotalDistance(input,intermediate_solution[[ aggregated_list_info_after_removal$aggregated_routes_index[[i]] ]]$route )
      
      # Nuevo 05/05/2021 el siguiente condicional
      if(!change_park){
        if(aggregated_list_info_after_removal$aggregated_clients[[i]][j] <= input$n1){ # si el cliente que eliminamos es VC
          # Habia un fallo. 26/04/2021
          for(ind_vc in 1:length(intermediate_solution[[ aggregated_list_info_after_removal$aggregated_routes_index[[i]] ]]$clients_vc)){
            if(intermediate_solution[[ aggregated_list_info_after_removal$aggregated_routes_index[[i]] ]]$clients_vc[[ind_vc]]$id == aggregated_list_info_after_removal$aggregated_clients[[i]][j] ){
              clients_vc_index <- ind_vc
            }
          }
          #hasta aqui (el client_vc_index no estaba bien)
          removed_load <- sum(intermediate_solution[[ aggregated_list_info_after_removal$aggregated_routes_index[[i]] ]]$clients_vc[[clients_vc_index]]$demands)
          intermediate_solution[[ aggregated_list_info_after_removal$aggregated_routes_index[[i]] ]]$total_load <- intermediate_solution[[ aggregated_list_info_after_removal$aggregated_routes_index[[i]] ]]$total_load - removed_load
          if (problem_type == "MCTTRP") {
            removed_truck_hoppers <- length(intermediate_solution[[ aggregated_list_info_after_removal$aggregated_routes_index[[i]] ]]$clients_vc[[clients_vc_index]]$hoppers_trucks)
            removed_trailer_hoppers <- length(intermediate_solution[[ aggregated_list_info_after_removal$aggregated_routes_index[[i]] ]]$clients_vc[[clients_vc_index]]$hoppers_trailers)
            intermediate_solution[[ aggregated_list_info_after_removal$aggregated_routes_index[[i]] ]]$used_hoppers_trailer <- intermediate_solution[[ aggregated_list_info_after_removal$aggregated_routes_index[[i]] ]]$used_hoppers_trailer - removed_trailer_hoppers
            intermediate_solution[[ aggregated_list_info_after_removal$aggregated_routes_index[[i]] ]]$used_hoppers_truck <- intermediate_solution[[ aggregated_list_info_after_removal$aggregated_routes_index[[i]] ]]$used_hoppers_truck - removed_truck_hoppers
          }
          # 26/04/2021. Nuevo el VC_index, porque hay que distinguir; en consecuencia, cambiado tambien en la segunda linea
          VC_index <- which(intermediate_solution[[ aggregated_list_info_after_removal$aggregated_routes_index[[i]] ]]$VCs == aggregated_list_info_after_removal$aggregated_clients[[i]][j])
          intermediate_solution[[ aggregated_list_info_after_removal$aggregated_routes_index[[i]] ]]$VCs <- intermediate_solution[[ aggregated_list_info_after_removal$aggregated_routes_index[[i]] ]]$VCs[-VC_index]
          intermediate_solution[[ aggregated_list_info_after_removal$aggregated_routes_index[[i]] ]]$clients_vc <- intermediate_solution[[ aggregated_list_info_after_removal$aggregated_routes_index[[i]] ]]$clients_vc[-clients_vc_index]
          
        }
        else{ # si el cliente que elimino es de tipo TC
          # Habia un fallo. 26/04/2021
          for(ind in 1:length(intermediate_solution[[ aggregated_list_info_after_removal$aggregated_routes_index[[i]] ]]$clients_tc)){
            if(intermediate_solution[[ aggregated_list_info_after_removal$aggregated_routes_index[[i]] ]]$clients_tc[[ind]]$id == aggregated_list_info_after_removal$aggregated_clients[[i]][j] ){
              clients_tc_index <- ind
            }
          }
          # hasta aqui (el clients_tc_index no estaba bien)
          removed_load <- sum(intermediate_solution[[ aggregated_list_info_after_removal$aggregated_routes_index[[i]] ]]$clients_tc[[clients_tc_index]]$demands)
          intermediate_solution[[ aggregated_list_info_after_removal$aggregated_routes_index[[i]] ]]$total_load <- intermediate_solution[[ aggregated_list_info_after_removal$aggregated_routes_index[[i]] ]]$total_load - removed_load
          intermediate_solution[[ aggregated_list_info_after_removal$aggregated_routes_index[[i]] ]]$total_load_tc_clients <- intermediate_solution[[ aggregated_list_info_after_removal$aggregated_routes_index[[i]] ]]$total_load_tc_clients - removed_load
          if (problem_type == "MCTTRP") {
            removed_truck_hoppers <- length(intermediate_solution[[ aggregated_list_info_after_removal$aggregated_routes_index[[i]] ]]$clients_tc[[clients_tc_index]]$hoppers_trucks)
            intermediate_solution[[ aggregated_list_info_after_removal$aggregated_routes_index[[i]] ]]$used_hoppers_truck <- intermediate_solution[[ aggregated_list_info_after_removal$aggregated_routes_index[[i]] ]]$used_hoppers_truck - removed_truck_hoppers
          }
          # 26/04/2021. Nuevo el TC_index, porque hay que distinguir; en consecuencia, cambiado tambien en la segunda linea
          TC_index <- which(intermediate_solution[[ aggregated_list_info_after_removal$aggregated_routes_index[[i]] ]]$TCs == aggregated_list_info_after_removal$aggregated_clients[[i]][j] )
          intermediate_solution[[ aggregated_list_info_after_removal$aggregated_routes_index[[i]] ]]$TCs <- intermediate_solution[[ aggregated_list_info_after_removal$aggregated_routes_index[[i]] ]]$TCs[-TC_index]
          intermediate_solution[[ aggregated_list_info_after_removal$aggregated_routes_index[[i]] ]]$clients_tc <- intermediate_solution[[ aggregated_list_info_after_removal$aggregated_routes_index[[i]] ]]$clients_tc[-clients_tc_index]
        }
      }
      
      
    }
  }


  return(list(sol=intermediate_solution, info_ini = aggregated_list_info, info_after = aggregated_list_info_after_removal, no_route_left = no_route_left))
}
