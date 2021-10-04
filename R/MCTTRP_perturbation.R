#perturbacion
perturbation_core<-function(input, current_solution, penalty_max, type_problem) {
  
  perturbation_not_obtained <- TRUE
  penalty <- 0
  
  while ((perturbation_not_obtained)) {
    print(all_routes(current_solution))
    print("*************************perturbation_not_obtained")
    
    perturbed_solution <-  perturbation(input, current_solution, penalty, type_problem)
    current_solution <- perturbed_solution[["perturbed_solution"]]
    perturbation_not_obtained <- perturbed_solution$perturbation_not_obtained
    print(perturbation_not_obtained)
    phi <- perturbed_solution$phi
    penalty <- 100
  }
  
  current_solution <- update_solution(current_solution, input, type_problem)
  
  res_p <- list()
  res_p$current_solution <- current_solution
  res_p$phi <- phi
  
  return(res_p)
}


perturbation <- function(input, initial_solution, penalty_max, problem_type){
  
  intermediate_solution <- initial_solution
  perturbation_not_obtained <- FALSE
  
  n <- input$n - 1
  
  # Nuestros clientes candidatos a ser eliminados deben estar en PTR, PVR, o CVR y no ser parking
  # 28/04/2021. No tenia permitido eliminar VCs de subrutas. Ahora si. Y tambien voy a permitir
  # que se incluyan en subrutas. 
  # 04/05/2021. Ahora tambien voy a permitir que se eliminen parkings (en cuyo caso hay
  # que hacer la solucion que queda, factible, por supuesto)
  
  # removed_candidates <- numeric()
  # for(i in 1:n){ #numero total de clientes
  #   if(client_is_parking(i, initial_solution)==0){
  #     removed_candidates <- c(removed_candidates,i)
  #   }
  # }
  
  removed_candidates <- numeric()
  for(i in 1:n){
    if(length(route_of_client(i,intermediate_solution)$route)>3){
      removed_candidates <- c(removed_candidates, i)
    }
  }
  
  #removed_candidates <- 1:n # pois agora permito eliminar tamen os parkings
  
  # De esos candidatos, elijo entre un 1% y un 10% aleatoriamente 
   phi <- sample(ceiling(0.01*length(removed_candidates)):ceiling(0.05*length(removed_candidates)),1)
   # phi <- 3
  
  # Ahora que ya sabemos que vamos a eliminar phi clientes, escogemos cuales 
  removed_clients <- sample(removed_candidates, phi)
  
  #removed_clients <- 14
  print(paste0("clientes eliminados: ", removed_clients))
  
  
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
  
  # Ahora voy a modificar la intermediate_solution, poniendo las nuevas rutas, su coste, y eliminando a los removed_clients y todo lo relacionado con ellos 
  # (ojo: para el caso en que los clientes eliminados provienen de CVRs, ya modifique la info relativa al main.tour y subtours en el bucle anterior)
  # (ojo 2: si los clientes eliminados son parking, ya no tengo NADA que modificar aqui porque ya lo 
  #         hice en la funcion externa delete_parking_solution aplicada en el bucle anterior)
  # for(i in 1:length(aggregated_list_info_after_removal$aggregated_routes_index)){
  # 
  #   QUITEI COUSAS DE AQUI TAMEN
  
  #   for(j in 1:length(aggregated_list_info_after_removal$aggregated_clients[[i]])){
  #     QUITEI TODO O QUE HABIA AQUI
  #   }
  #   
  # }
  # 
  # Ahora lo que tengo que hacer es, esos clientes que he eliminado de sus rutas originales, insertarlos en una
  # ruta distinta (donde sea factible; la idea es que tengo que intentar insertar todos)
  
  # Basicamente, para cada uno de los clientes que debo insertar (que son los que he eliminado), tengo que crearme 
  # una lista con sus posibles "destination_routes"
  
  # reordenar por carga ---> aggregated_list_info$aggregated_clients
  
  
  for(k in 1:length(aggregated_list_info$aggregated_clients)){
    for(i in 1:length(aggregated_list_info$aggregated_clients[[k]])){
      inserting_client <- aggregated_list_info$aggregated_clients[[k]][i]
      route_try <- list()
      in_subtour <- list()
      index_ins <- list()
      new_route_ins <- list()
      delta_ins <- list()
      subtour_try <- list()
      t <- 1
      modified_subtours <- list()
      ind_subtour_wrt_route <- list()
      subtour_in_route <- list()
      tt <- 1
      for(r in 1:length(intermediate_solution)){
        if(!(r %in% no_route_left)){
        if(aggregated_list_info$aggregated_routes_index[[k]]!= r){ # hay que mirar en rutas distintas de la ruta origen
          if(inserting_client <= input$n1){ # si el cliente es VC entonces podria insertarse en cualquier ruta que no sea la propia de origen
            candidate_destination_route <- intermediate_solution[[r]]
            # La siguiente funcion nos dice si es (avail=TRUE) o no (avail=FALSE) posible que 
            # Fallo 27/04/2021. Cambie avail<-1 por avail<-0 porque si no, si no llegase a entrar en el if(check...) 
            # no seria avail y sin embargo no cambiaria a 0
            avail <- 0
            avail_truck <- 0
            
            if (problem_type == "MCTTRP") {
              res <- return_cap_and_route_permutation(candidate_destination_route, input, "MCTTRP")
              tcap <- res$tcap
              route <- res$route
              if (check_capacity_total_routes(inserting_client, route, input, tcap, penalty_max, "MCTTRP"))
                avail <- boolean_available_compartments_destination_route(input, result, intermediate_solution, 
                                                                          inserting_client, candidate_destination_route, 
                                                                          initial_solution, penalty_max)$avail
              # 28/04/2021. Añadi esto, ya que tambien ahora hay dos outputs en la siguiente funcion
              if(candidate_destination_route$type=="CVR"){
                avail_truck <- boolean_available_compartments_destination_route(input, result, intermediate_solution, 
                                                                                inserting_client, candidate_destination_route, 
                                                                                initial_solution, penalty_max)$avail_truck
              }
              
            }
            if (problem_type == "TTRP") { 
              res <- return_cap_and_route_permutation(candidate_destination_route, input, "TTRP")
              tcap <- res$tcap
              route <- res$route
              avail <- check_capacity_total_routes(c(inserting_client), route, input, tcap,penalty_max, "TTRP")
              # Aqui posiblemente haya que añadir algo analogo al avail_subtour del MCTTRP,
              # para permitir tambien en el TTRP que los VCs se puedan incluir en subrutas
            }
            
            if(avail){ 
              if(candidate_destination_route$type == "CVR"){
                route_try[[t]] <- candidate_destination_route$main_tour
                index_ins[[t]] <- r
              }else{
                route_try[[t]] <- candidate_destination_route$route
                index_ins[[t]] <- r
              }
              init_time <- Sys.time()
              res_geni <- GENI(input, route_try[[t]], inserting_client)
              #print(paste0("GENI time ", difftime(Sys.time(), init_time, units = "secs","s)")))
              
              new_route_ins[[t]] <- res_geni$best_route
              delta_ins[[t]] <- res_geni$delta_GENI
              route_try[[t]] <- new_route_ins[[t]]
              in_subtour[[t]] <- 0
              
              t <- t + 1
              
            }
            
            # Nuevo 28/04/2021 el siguiente if
            if(avail_truck){ # no entrariamos aqui si la ruta no fuese CVR
              subtour_try <- list()
              index_subtour <- list()
              geni_subtour <- list()
              new_subtour_ins <- list()
              delta_subtour <- list()
              
              avail_specific_subtour <- rep(0,length(intermediate_solution[[r]]$subtours))
              kk <- 1
              for(rr in 1:length(intermediate_solution[[r]]$subtours)){
                if (problem_type == "MCTTRP") {
                  res <- return_cap_and_route_permutation(candidate_destination_route, input, "MCTTRP")
                  tcap <- res$tcap
                  route <- res$route
                  # Fallo 27/04/2021. Meti penalty_max como input en la siguiente funcion
                  if (check_capacity_subroute_routes(inserting_client, route, intermediate_solution[[r]]$subtours, rr, input, "MCTTRP", penalty_max)){
                    avail_specific_subtour[rr] <- boolean_available_compartments_destination_route(input, result, intermediate_solution, 
                                                                                                   inserting_client, candidate_destination_route, 
                                                                                                   initial_solution, penalty_max)$avail_truck
                  }
                }
                if (problem_type == "TTRP") { 
                  res <- return_cap_and_route_permutation(candidate_destination_route, input, "TTRP")
                  tcap <- res$tcap
                  route <- res$route
                  # Fallo 27/04/2021. Meti penalty_max como input en la siguiente funcion
                  avail_subtour[rr] <- check_capacity_subroute_routes(inserting_client, route, intermediate_solution[[r]]$subtours, rr, input, "TTRP", penalty_max)
                }
                
                if(avail_specific_subtour[rr]){
                  
                  subtour_try[[kk]] <- candidate_destination_route$subtours[[rr]]$tour
                  index_subtour[[kk]] <- rr
                  
                  geni_subtour[[kk]] <- GENI(input, subtour_try[[kk]], inserting_client)
                  #print(paste0("GENI time ", difftime(Sys.time(), init_time, units = "secs","s)")))
                  
                  new_subtour_ins[[kk]] <- geni_subtour[[kk]]$best_route
                  delta_subtour[[kk]] <- geni_subtour[[kk]]$delta_GENI
                  
                  subtour_try[[kk]] <- new_subtour_ins[[kk]]
                  
                  kk <- kk + 1
                  
                  
                }
              }
              
              if(sum(avail_specific_subtour)!=0){
                delta_min_subtour_pos <- which(delta_subtour == min(unlist(delta_subtour)))
                if(length(delta_min_subtour_pos) == 1){
                  delta_subtour_chosen_position <- delta_min_subtour_pos
                }else{
                  delta_subtour_chosen_position <- sample(delta_min_subtour_pos,1)
                }
                
                index_subtour_insertion <- index_subtour[[delta_subtour_chosen_position]]
                # La ruta de la solucion inicial cuyo indice es index_route_insertion la hay que actualizar, de modo que se le añade al cliente inserting_client
                
                best_subtour_ins <- new_subtour_ins[[delta_subtour_chosen_position]]
                
                
                modified_subtours[[tt]] <- candidate_destination_route$subtours
                modified_subtours[[tt]][[index_subtour_insertion]]$tour <- best_subtour_ins
                modified_subtours[[tt]][[index_subtour_insertion]]$length <- modified_subtours[[tt]][[index_subtour_insertion]]$length + 1
                modified_subtours[[tt]][[index_subtour_insertion]]$vc_clients <- c(modified_subtours[[tt]][[index_subtour_insertion]]$vc_clients, inserting_client)
                ind_subtour_wrt_route[[tt]] <- t
                subtour_in_route[[tt]]  <- r # nuevo 29/04/2021
                
                info_modified_subtours <- list(modified_subtours = modified_subtours, ind_subtour_wrt_route=ind_subtour_wrt_route, subtour_in_route = subtour_in_route)
                
                new_route_ins[[t]] <- create_route_from_main_route_and_subroutes(modified_subtours[[tt]], candidate_destination_route$main_tour)
                delta_ins[[t]] <- delta_subtour[[delta_subtour_chosen_position]]
                index_ins[[t]] <- r
                route_try[[t]] <- new_route_ins[[t]]
                in_subtour[[t]] <- 1
                
                t <- t + 1
                tt <- tt + 1
              }
            }
            
          }else{ # si el cliente es tc
            if(intermediate_solution[[r]]$type == "PTR"){
              candidate_destination_route <- intermediate_solution[[r]]
              # La siguiente funcion nos dice si es (avail=TRUE) o no (avail=FALSE) posible que 
              
              #Fallo 27/04/2021. Cambie avail<-1 por avail<-0 porque si no, si no llegase a entrar en el if(check...) 
              # no seria avail y sin embargo no cambiaria a 0
              avail <- 0
              if (problem_type == "MCTTRP") {
                res <- return_cap_and_route_permutation(candidate_destination_route, input, "MCTTRP")
                tcap <- res$tcap
                route <- res$route
                # meter penalizacion
                if (check_capacity_total_routes(inserting_client, route, input, tcap, penalty_max, "MCTTRP"))
                  avail <- boolean_available_compartments_destination_route(input, result, intermediate_solution, 
                                                                            inserting_client, candidate_destination_route, 
                                                                            initial_solution, penalty_max)$avail_truck
              }
              if (problem_type == "TTRP") { 
                res <- return_cap_and_route_permutation(candidate_destination_route, input, "TTRP")
                tcap <- res$tcap
                route <- res$route
                # meter penalizacion
                avail <- check_capacity_total_routes(inserting_client, route, input, tcap, penalty_max, "TTRP")
              }
              
              if(avail){ 
                route_try[[t]] <- candidate_destination_route$route
                index_ins[[t]] <- r
                
                init_time <- Sys.time()
                res_geni <- GENI(input, route_try[[t]], inserting_client)
                #print(paste0("GENI time ", difftime(Sys.time(), init_time, units = "secs","s)")))
                
                new_route_ins[[t]] <- res_geni$best_route
                delta_ins[[t]] <- res_geni$delta_GENI
                route_try[[t]] <- new_route_ins[[t]]
                in_subtour[[t]] <- 0
                
                t <- t + 1
              }
            }else if(intermediate_solution[[r]]$type == "CVR"){
              subtour_try <- list()
              index_subtour <- list()
              geni_subtour <- list()
              new_subtour_ins <- list()
              delta_subtour <- list()
              candidate_destination_route <- intermediate_solution[[r]]
              # Fallo 27/04/2021. Tuve que poner aqui 0 en vez de 1 (pues si no es avail,
              # nunca va a entrar en el if check... para cambiarlo)
              avail_subtour <- rep(0,length(intermediate_solution[[r]]$subtours))
              kk <- 1
              for(rr in 1:length(intermediate_solution[[r]]$subtours)){
                if (problem_type == "MCTTRP") {
                  res <- return_cap_and_route_permutation(candidate_destination_route, input, "MCTTRP")
                  tcap <- res$tcap
                  route <- res$route
                  # Fallo 27/04/2021. Meti penalty_max como input en la siguiente funcion
                  if (check_capacity_subroute_routes(inserting_client, route, intermediate_solution[[r]]$subtours, rr, input, "MCTTRP", penalty_max)){
                    avail_subtour[rr] <- boolean_available_compartments_destination_route(input, result, intermediate_solution, 
                                                                                          inserting_client, candidate_destination_route, 
                                                                                          initial_solution, penalty_max)$avail_truck
                  }
                }
                if (problem_type == "TTRP") { 
                  res <- return_cap_and_route_permutation(candidate_destination_route, input, "TTRP")
                  tcap <- res$tcap
                  route <- res$route
                  # Fallo 27/04/2021. Meti penalty_max como input en la siguiente funcion
                  avail_subtour[rr] <- check_capacity_subroute_routes(inserting_client, route, intermediate_solution[[r]]$subtours, rr, input, "TTRP", penalty_max)
                }
                
                if(avail_subtour[rr]){
                  
                  subtour_try[[kk]] <- candidate_destination_route$subtours[[rr]]$tour
                  index_subtour[[kk]] <- rr
                  
                  geni_subtour[[kk]] <- GENI(input, subtour_try[[kk]], inserting_client)
                  #print(paste0("GENI time ", difftime(Sys.time(), init_time, units = "secs","s)")))
                  
                  new_subtour_ins[[kk]] <- geni_subtour[[kk]]$best_route
                  delta_subtour[[kk]] <- geni_subtour[[kk]]$delta_GENI
                  
                  subtour_try[[kk]] <- new_subtour_ins[[kk]]
                  
                  kk <- kk + 1
                  
                  
                }
              }
              
              if(sum(avail_subtour)!=0){
                delta_min_subtour_pos <- which(delta_subtour == min(unlist(delta_subtour)))
                if(length(delta_min_subtour_pos) == 1){
                  delta_subtour_chosen_position <- delta_min_subtour_pos
                }else{
                  delta_subtour_chosen_position <- sample(delta_min_subtour_pos,1)
                }
                
                index_subtour_insertion <- index_subtour[[delta_subtour_chosen_position]]
                # La ruta de la solucion inicial cuyo indice es index_route_insertion la hay que actualizar, de modo que se le añade al cliente inserting_client
                
                best_subtour_ins <- new_subtour_ins[[delta_subtour_chosen_position]]
                
                
                modified_subtours[[tt]] <- candidate_destination_route$subtours
                modified_subtours[[tt]][[index_subtour_insertion]]$tour <- best_subtour_ins
                modified_subtours[[tt]][[index_subtour_insertion]]$length <- modified_subtours[[tt]][[index_subtour_insertion]]$length + 1
                modified_subtours[[tt]][[index_subtour_insertion]]$tc_clients <- c(modified_subtours[[tt]][[index_subtour_insertion]]$tc_clients, inserting_client)
                ind_subtour_wrt_route[[tt]] <- t
                subtour_in_route[[tt]]  <- r # nuevo 29/04/2021
                
                info_modified_subtours <- list(modified_subtours = modified_subtours, ind_subtour_wrt_route=ind_subtour_wrt_route, subtour_in_route = subtour_in_route)
                
                new_route_ins[[t]] <- create_route_from_main_route_and_subroutes(modified_subtours[[tt]], candidate_destination_route$main_tour)
                delta_ins[[t]] <- delta_subtour[[delta_subtour_chosen_position]]
                index_ins[[t]] <- r
                route_try[[t]] <- new_route_ins[[t]]
                in_subtour[[t]] <- 1
                
                
                t <- t + 1
                tt <- tt + 1
              }
            }else if(intermediate_solution[[r]]$type == "PVR"){
              # Nuevo 06/05/2021
              # Hacer funcion para create_subtour: ojo, aqui quiero haber comprobado que se 
              # puede insertar este cliente (por cuestiones de carga y tal)
              
              #creating_subtour_with_TC <- creating_subtour_with_TC(input, intermediate_solution, r, 
              #                                                     inserting_client, problem_type, agg_clients, agg_parkings)
              
              
              candidate_destination_route <- intermediate_solution[[r]]
              
              avail <- 0
              if (problem_type == "MCTTRP") {
                res <- return_cap_and_route_permutation(candidate_destination_route, input, "MCTTRP")
                tcap <- res$tcap
                route <- res$route
                # meter penalizacion
                if (check_capacity_total_routes(inserting_client, route, input, tcap, penalty_max, "MCTTRP"))
                  avail <- boolean_available_compartments_destination_route(input, result, intermediate_solution, 
                                                                            inserting_client, candidate_destination_route, 
                                                                            initial_solution, penalty_max)$avail_truck
              }
              if (problem_type == "TTRP") { 
                res <- return_cap_and_route_permutation(candidate_destination_route, input, "TTRP")
                tcap <- res$tcap
                route <- res$route
                # meter penalizacion;
                # 07/05/2021 aqui necesito algo parecido a lo de "avail_truck" que me diga 
                # si la demanda cabe en el truck
                avail <- check_capacity_total_routes(inserting_client, route, input, tcap, penalty_max, "TTRP")
              }
              
              if(avail){ 
                route_try[[t]] <- candidate_destination_route$route
                index_ins[[t]] <- r
                
                init_time <- Sys.time()
                
                new_route_and_dist <- create_subtour_with_TC(input, candidate_destination_route,
                                                         inserting_client)
                new_route_ins[[t]] <- new_route_and_dist$route
                delta_ins[[t]] <- new_route_and_dist$dist
                
                route_try[[t]] <- new_route_ins[[t]]
                in_subtour[[t]] <- 1
                
                t <- t + 1
              }
              
            }
          }
        }else{ 
          # solo podemos insertar un cliente en su ruta de origen si se verifica que 
          # es VC y cambia del main tour a un subtour (o viceversa). Es decir, siempre
          # y cuando esta ruta sea CVR.
          # Todo esto es nuevo, 29/04/2021
          if(inserting_client <= input$n1 && intermediate_solution[[r]]$type =="CVR"){ 
            candidate_destination_route <- intermediate_solution[[r]]
            if(client_in_main_tour(inserting_client, initial_solution)){
              # solo lo podemos intentar insertar en los subtours de esta CVR
              avail_truck <- 0
              
              if (problem_type == "MCTTRP") {
                res <- return_cap_and_route_permutation(candidate_destination_route, input, "MCTTRP")
                tcap <- res$tcap
                route <- res$route
                if (check_capacity_total_routes(inserting_client, route, input, tcap, penalty_max, "MCTTRP")){
                  avail_truck <- boolean_available_compartments_destination_route(input, result, intermediate_solution, 
                                                                                  inserting_client, candidate_destination_route, 
                                                                                  initial_solution, penalty_max)$avail_truck
                }
                
              }
              if (problem_type == "TTRP") { 
                res <- return_cap_and_route_permutation(candidate_destination_route, input, "TTRP")
                tcap <- res$tcap
                route <- res$route
                avail <- check_capacity_total_routes(c(inserting_client), route, input, tcap,penalty_max, "TTRP")
                # Aqui posiblemente haya que añadir algo analogo al avail_truck del MCTTRP,
                # para permitir tambien en el TTRP que los VCs se puedan incluir en subrutas
                # a esto habria q llamarle avail_truck tb 29/04/2021
              }
              
              
              # Nuevo 28/04/2021 el siguiente if
              if(avail_truck){
                subtour_try <- list()
                index_subtour <- list()
                geni_subtour <- list()
                new_subtour_ins <- list()
                delta_subtour <- list()
                
                avail_specific_subtour <- rep(0,length(intermediate_solution[[r]]$subtours))
                kk <- 1
                for(rr in 1:length(intermediate_solution[[r]]$subtours)){
                  if (problem_type == "MCTTRP") {
                    res <- return_cap_and_route_permutation(candidate_destination_route, input, "MCTTRP")
                    tcap <- res$tcap
                    route <- res$route
                    # Fallo 27/04/2021. Meti penalty_max como input en la siguiente funcion
                    if (check_capacity_subroute_routes(inserting_client, route, intermediate_solution[[r]]$subtours, rr, input, "MCTTRP", penalty_max)){
                      avail_specific_subtour[rr] <- boolean_available_compartments_destination_route(input, result, intermediate_solution, 
                                                                                                     inserting_client, candidate_destination_route, 
                                                                                                     initial_solution, penalty_max)$avail_truck
                    }
                  }
                  if (problem_type == "TTRP") { 
                    res <- return_cap_and_route_permutation(candidate_destination_route, input, "TTRP")
                    tcap <- res$tcap
                    route <- res$route
                    # Fallo 27/04/2021. Meti penalty_max como input en la siguiente funcion
                    avail_subtour[rr] <- check_capacity_subroute_routes(inserting_client, route, intermediate_solution[[r]]$subtours, rr, input, "TTRP", penalty_max)
                  }
                  
                  if(avail_specific_subtour[rr]){
                    
                    subtour_try[[kk]] <- candidate_destination_route$subtours[[rr]]$tour
                    index_subtour[[kk]] <- rr
                    
                    geni_subtour[[kk]] <- GENI(input, subtour_try[[kk]], inserting_client)
                    #print(paste0("GENI time ", difftime(Sys.time(), init_time, units = "secs","s)")))
                    
                    new_subtour_ins[[kk]] <- geni_subtour[[kk]]$best_route
                    delta_subtour[[kk]] <- geni_subtour[[kk]]$delta_GENI
                    
                    subtour_try[[kk]] <- new_subtour_ins[[kk]]
                    
                    kk <- kk + 1
                    
                    
                  }
                }
                
                if(sum(avail_specific_subtour)!=0){
                  delta_min_subtour_pos <- which(delta_subtour == min(unlist(delta_subtour)))
                  if(length(delta_min_subtour_pos) == 1){
                    delta_subtour_chosen_position <- delta_min_subtour_pos
                  }else{
                    delta_subtour_chosen_position <- sample(delta_min_subtour_pos,1)
                  }
                  
                  index_subtour_insertion <- index_subtour[[delta_subtour_chosen_position]]
                  # La ruta de la solucion inicial cuyo indice es index_route_insertion la hay que actualizar, de modo que se le añade al cliente inserting_client
                  
                  best_subtour_ins <- new_subtour_ins[[delta_subtour_chosen_position]]
                  
                  
                  modified_subtours[[tt]] <- candidate_destination_route$subtours
                  modified_subtours[[tt]][[index_subtour_insertion]]$tour <- best_subtour_ins
                  modified_subtours[[tt]][[index_subtour_insertion]]$length <- modified_subtours[[tt]][[index_subtour_insertion]]$length + 1
                  modified_subtours[[tt]][[index_subtour_insertion]]$vc_clients <- c(modified_subtours[[tt]][[index_subtour_insertion]]$vc_clients, inserting_client)
                  ind_subtour_wrt_route[[tt]] <- t
                  subtour_in_route[[tt]]  <- r # nuevo 29/04/2021
                  
                  info_modified_subtours <- list(modified_subtours = modified_subtours, ind_subtour_wrt_route=ind_subtour_wrt_route, subtour_in_route = subtour_in_route)
                  
                  new_route_ins[[t]] <- create_route_from_main_route_and_subroutes(modified_subtours[[tt]], candidate_destination_route$main_tour)
                  delta_ins[[t]] <- delta_subtour[[delta_subtour_chosen_position]]
                  index_ins[[t]] <- r
                  route_try[[t]] <- new_route_ins[[t]]
                  in_subtour[[t]] <- 1
                  
                  t <- t + 1
                  tt <- tt + 1
                }
              }
              
              
              
              
            }else{
              # Solo lo podemos intentar insertar en el main tour
              avail <- 0
              
              if (problem_type == "MCTTRP") {
                res <- return_cap_and_route_permutation(candidate_destination_route, input, "MCTTRP")
                tcap <- res$tcap
                route <- res$route
                if (check_capacity_total_routes(inserting_client, route, input, tcap, penalty_max, "MCTTRP"))
                  avail <- boolean_available_compartments_destination_route(input, result, intermediate_solution, 
                                                                            inserting_client, candidate_destination_route, 
                                                                            initial_solution, penalty_max)$avail
                
              }
              if (problem_type == "TTRP") { 
                res <- return_cap_and_route_permutation(candidate_destination_route, input, "TTRP")
                tcap <- res$tcap
                route <- res$route
                avail <- check_capacity_total_routes(c(inserting_client), route, input, tcap,penalty_max, "TTRP")
                # Aqui posiblemente haya que añadir algo analogo al avail_subtour del MCTTRP,
                # para permitir tambien en el TTRP que los VCs se puedan incluir en subrutas
              }
              
              if(avail){ 
                route_try[[t]] <- candidate_destination_route$main_tour # porque es CVR
                index_ins[[t]] <- r
                
                init_time <- Sys.time()
                res_geni <- GENI(input, route_try[[t]], inserting_client)
                #print(paste0("GENI time ", difftime(Sys.time(), init_time, units = "secs","s)")))
                
                new_route_ins[[t]] <- res_geni$best_route
                delta_ins[[t]] <- res_geni$delta_GENI
                route_try[[t]] <- new_route_ins[[t]]
                in_subtour[[t]] <- 0
                
                t <- t + 1
                
              }
              
              
            }
            
          }
          
        }
        }
      }
      
      if(length(delta_ins) == 0){
        perturbation_not_obtained <- TRUE
        break
        
      }else{
        
        delta_min_positions <- which(delta_ins == min(unlist(delta_ins)))
        if(length(delta_min_positions) == 1){
          delta_chosen_position <- delta_min_positions
        }else{
          delta_chosen_position <- sample(delta_min_positions,1)
        }
        
        index_route_insertion <- index_ins[[delta_chosen_position]]
        # La ruta de la solucion inicial cuyo indice es index_route_insertion la hay que actualizar, de modo que se le añade al cliente inserting_client
        
        best_route_ins <- new_route_ins[[delta_chosen_position]]
        is_best_insertion_in_subtour <- in_subtour[[delta_chosen_position]] # nuevo 29/04/2021
        # Actualizamos la intermediate_solution:
        new_park_from_PVR <- -1
        
        if (intermediate_solution[[index_route_insertion]]$type == "CVR") {
          if(is_best_insertion_in_subtour==0){ 
            client_in_subtour <- 0  # nuevo 29/04/2021
            intermediate_solution[[index_route_insertion]]$main_tour <- best_route_ins
            intermediate_solution[[index_route_insertion]]$route <- create_route_from_main_route_and_subroutes(intermediate_solution[[index_route_insertion]]$subtours, best_route_ins)
          }else{
            intermediate_solution[[index_route_insertion]]$route <- best_route_ins
            client_in_subtour <- 1 # nuevo 29/04/2021
          } 
          
          # Nuevo else if 07/05/2021 que contempla el caso en el que hemos insertado un 
          # TC en una PVR de modo que pasara a ser una CVR
        }else if(intermediate_solution[[index_route_insertion]]$type == "PVR" && is_best_insertion_in_subtour){
          client_in_subtour <- 1
          intermediate_solution[[index_route_insertion]]$main_tour <-  intermediate_solution[[index_route_insertion]]$route
          intermediate_solution[[index_route_insertion]]$route <- best_route_ins
          new_park_from_PVR <- best_route_ins[duplicated(best_route_ins) & best_route_ins!=0]
          
        } else {
          client_in_subtour <- 0   # nuevo 29/04/2021
          intermediate_solution[[index_route_insertion]]$route <- best_route_ins
        }
        intermediate_solution[[index_route_insertion]]$cost <- calculateTotalDistance(input,best_route_ins)
        
        if (problem_type == "MCTTRP") {
          # Nuevo este if else (el "else" ya estaba) 07/05/2021
          if(intermediate_solution[[index_route_insertion]]$type == "PVR" && is_best_insertion_in_subtour){
            destination_route <- creating_CVR_from_PVR(input, initial_solution, intermediate_solution, index_route_insertion, 
                                                       inserting_client, new_park_from_PVR, "MCTTRP")
            aggregated_list_info_after_removal$aggregated_parking[[k]][[i]] <- 1
            intermediate_solution[[index_route_insertion]] <- destination_route
            
          }else{
          # Fallo 27/04/2021. Meti penalty_max como input en la siguiente funcion
          # 29/04/2021. Meto el client_in_subtour
          
          
          destination_route <- check_available_compartments(input, result, intermediate_solution, inserting_client, 
                                                            intermediate_solution[[index_route_insertion]], initial_solution,
                                                            penalty_max, client_in_subtour)$destination_route
          
          intermediate_solution[[index_route_insertion]] <- destination_route
          }
          # Fallo 27/04/2021. Necesario añadir el if siguiente; 07/05/2021 añado la primera condicion
          if( new_park_from_PVR==-1 && 
              (intermediate_solution[[index_route_insertion]]$type == "CVR") && is_best_insertion_in_subtour== 1 ){
            ind_subt = which(unlist(info_modified_subtours$subtour_in_route)==index_route_insertion)
            intermediate_solution[[index_route_insertion]]$subtours <- modified_subtours[[ind_subt]]
          }
          
        }else if (problem_type == "TTRP"){
          if(intermediate_solution[[index_route_insertion]]$type == "PVR" && is_best_insertion_in_subtour){
            destination_route <- creating_CVR_from_PVR(input, initial_solution, intermediate_solution, index_route_insertion, 
                                                       inserting_client, new_park_from_PVR, "TTRP")
            aggregated_list_info_after_removal$aggregated_parking[[k]][[i]] <- 1
            intermediate_solution[[index_route_insertion]] <- destination_route
            
          }else{
          intermediate_solution[[index_route_insertion]]$total_load <- intermediate_solution[[index_route_insertion]]$total_load + input$vector.demandas[inserting_client+1]
          new_client <- list()
          new_client$id <- inserting_client
          new_client$demands <- input$vector.demandas[inserting_client+1]
          if(inserting_client <= input$n1){ # si el cliente es VC
            intermediate_solution[[index_route_insertion]]$clients_vc[[length(intermediate_solution[[index_route_insertion]]$clients_vc)+1]] <- new_client
            intermediate_solution[[index_route_insertion]]$VCs <- c(intermediate_solution[[index_route_insertion]]$VCs, inserting_client) 
          }else{    # si el cliente es TC
            intermediate_solution[[index_route_insertion]]$total_load_tc_clients <- intermediate_solution[[index_route_insertion]]$total_load_tc_clients + new_client$demands
            intermediate_solution[[index_route_insertion]]$clients_tc[[length(intermediate_solution[[index_route_insertion]]$clients_tc)+1]] <- new_client
            intermediate_solution[[index_route_insertion]]$TCs <- c(intermediate_solution[[index_route_insertion]]$TCs, inserting_client)
            if( (intermediate_solution[[index_route_insertion]]$type == "CVR") &&  is_best_insertion_in_subtour== 1){
              ind_subt = which(unlist(info_modified_subtours$subtour_in_route)==index_route_insertion)
              intermediate_solution[[index_route_insertion]]$subtours <- modified_subtours[[ind_subt]]
            }
          }
        }
        
        }
      }
    }
    
    if(perturbation_not_obtained){
      perturbation_not_obtained <- TRUE
      break
    }
  }
  
  if(perturbation_not_obtained){
    perturbed_solution <- initial_solution
    
  }else{
    if(length(no_route_left) > 0){
      intermediate_solution <- intermediate_solution[-no_route_left]
    }
    # Fallo 27/04/2021. La inclusion de este update_solution da lugar a cosas inconsistentes
    #intermediate_solution <- update_solution(intermediate_solution, input, problem_type)
    perturbed_solution <- intermediate_solution
    
  }
  
  #for(i in 1:length(perturbed_solution)){
  #  print(perturbed_solution[[i]]$route)
  #}
  
  return(list(perturbation_not_obtained = perturbation_not_obtained, perturbed_solution = perturbed_solution, phi = phi, removed_clients=removed_clients))
  
}





return_cap_and_route_permutation<-function(intermediate_solution, input, type) {
  if (intermediate_solution$type == "CVR")  {
    if (type == "MCTTRP") tcap <- input$capacidad.vehiculo[1]
    if (type == "TTRP") tcap <- input$capacidad.vehiculo
    route <- intermediate_solution$route#return_main_route(intermediate_solution$route)
  }
  if (intermediate_solution$type == "PVR")  {
    if (type == "MCTTRP") tcap <- input$capacidad.vehiculo[1]
    if (type == "TTRP") tcap <- input$capacidad.vehiculo
    route <- intermediate_solution$route
  }
  if (intermediate_solution$type == "PTR")  {
    if (type == "MCTTRP") tcap <- input$capacidad.truck[1]
    if (type == "TTRP") tcap <- input$capacidad.truck
    route <- intermediate_solution$route
  }
  
  res <- list()
  res$tcap <- tcap
  res$route <- route
  
  return(res)
}



# Creada 05/05/2021
delete_parking_solution <- function(input, parking, route_ind, intermediate_solution,
                                    problem_type, agg_clients, agg_parkings){
  
  # Si el main tour solo tiene un cliente (el parking en cuestion) y eliminamos ese
  # cliente, entonces ya no tendremos una CVR, sino que podemos pasar o bien a tener 
  # una PTR (creo que es la opcion mas facil) o a enganchar esa subruta/subrutas que
  # nos quedan en otro cliente de otra ruta. 
  # De momento vamos a tener en cuenta solo la primera opcion (pasamos de tener una 
  # CVR a tener una PTR) 
  
  change_park <- 0
  
  if(length(intermediate_solution[[route_ind]]$main_tour)==3){
    change_park <- 1
    agg_parkings[which(agg_clients==parking)] <- 0
    
    intermediate_solution[[route_ind]]$type <- "PTR"
    ind_park <- which(intermediate_solution[[route_ind]]$route==parking)
    intermediate_solution[[route_ind]]$route <- intermediate_solution[[route_ind]]$route[-ind_park]
    improved_route <- two_opt(input, intermediate_solution[[route_ind]]$route)
    improved_route <- three_opt(input, improved_route)
    improved_route <- four_opt_asterisk(input, improved_route)
    intermediate_solution[[route_ind]]$route <- improved_route
    
    intermediate_solution[[route_ind]]$main_tour <- c()
    intermediate_solution[[route_ind]]$subtours <- list()
    
    if (problem_type == "MCTTRP") {
      intermediate_solution[[route_ind]]$used_hoppers_trailer <- 0
      intermediate_solution[[route_ind]]$trailer_number <- 0
    }
    
  }else{
    # En caso de que o cliente en cuestion non sexa unico na sua ruta, o que podemos 
    # facer e que os clientes da(s) sua(s) subruta(s) pasen a estar en subrutas doutros 
    # clientes do main tour (onde mais conveña *cada subtour*)
    subtours <- intermediate_solution[[route_ind]]$subtours
    ind_sub <- numeric()
    for(jj in 1:length(subtours)){
      if(subtours[[jj]]$root == parking){
        ind_sub <- c(ind_sub,jj)
      }
    }
    subt_deleted_client <- subtours[ind_sub]
    for(kk in 1:length(subt_deleted_client)){
      f_client <- subt_deleted_client[[kk]]$tour[2] # first client de ese subtour
      l_client <- rev(subt_deleted_client[[kk]]$tour)[2] # last client de ese subtour
      dist <- numeric() #distancia
      cl <- numeric() #clientes
      for(i in 2:(length(intermediate_solution[[route_ind]]$main_tour)-1)){
        client_try <- intermediate_solution[[route_ind]]$main_tour[[i]]
        if(client_try!=parking){
          cl <- c(cl, client_try)
          dist <- c(dist,input$matriz.distancia[client_try+1,f_client+1] + input$matriz.distancia[l_client+1,client_try+1])
        }
      }
      
      ind_min_dist <- which(dist==min(dist))
      if(length(ind_min_dist)==1){
        chosen_ind <- ind_min_dist
      }else{
        chosen_ind <- sample(ind_min_dist, 1)
      }
      chosen_client <- cl[chosen_ind] # cliente donde vamos a insertar el TC (como subtour con
      # unico cliente)
      
      subt_deleted_client[[kk]]$tour[1] <- chosen_client
      subt_deleted_client[[kk]]$tour[length(subt_deleted_client[[kk]]$tour)] <- chosen_client
      subt_deleted_client[[kk]]$root <- chosen_client
      
      change_park <- 1
      
      if(sum(chosen_client == agg_clients)>0){
        new_park <- which(agg_clients == chosen_client)
        agg_parkings[new_park] <- 1
      }
      
      agg_parkings[which(agg_clients==parking)] <- 0
      
      
      
    }
    subtours[ind_sub] <- subt_deleted_client
    intermediate_solution[[route_ind]]$subtours <- subtours
    
    # Eliminamos el cliente en cuestion del main tour
    cl_ind_main_route <-  which(intermediate_solution[[route_ind]]$main_tour==parking)
    intermediate_solution[[route_ind]]$main_tour <-  intermediate_solution[[route_ind]]$main_tour[-cl_ind_main_route]
    
    # Con el nuevo main tour (habiendo eliminado el cliente) y los subtours que creamos
    # anteriormente en un nuevo parking, creamos la ruta
    intermediate_solution[[route_ind]]$route <- create_route_from_main_route_and_subroutes(subtours,intermediate_solution[[route_ind]]$main_tour)
    
    for(j in 1:length(intermediate_solution[[route_ind]]$clients_vc)){
      if(intermediate_solution[[route_ind]]$clients_vc[[j]]$id == parking){
        ind_clients_vc <- j
      }
    }
    
    if (problem_type == "MCTTRP") {
      hopptrailer_vc_client <- length(intermediate_solution[[route_ind]]$clients_vc[[ind_clients_vc]]$hoppers_trailers)
      intermediate_solution[[route_ind]]$used_hoppers_trailer <- intermediate_solution[[route_ind]]$used_hoppers_trailer - hopptrailer_vc_client
    }
    
  }
  
  for(j in 1:length(intermediate_solution[[route_ind]]$clients_vc)){
    if(intermediate_solution[[route_ind]]$clients_vc[[j]]$id == parking){
      ind_clients_vc <- j
    }
  }
  removed_load <- sum(intermediate_solution[[route_ind]]$clients_vc[[ind_clients_vc]]$demands)
  intermediate_solution[[route_ind]]$total_load <- intermediate_solution[[route_ind]]$total_load - removed_load
  if (problem_type == "MCTTRP") {
    hopptruck_vc_client <- length(intermediate_solution[[route_ind]]$clients_vc[[ind_clients_vc]]$hoppers_trucks)
    intermediate_solution[[route_ind]]$used_hoppers_truck <- intermediate_solution[[route_ind]]$used_hoppers_truck - hopptruck_vc_client
  }
  intermediate_solution[[route_ind]]$clients_vc <- intermediate_solution[[route_ind]]$clients_vc[-ind_clients_vc]
  
  VC_index <- which(intermediate_solution[[route_ind]]$VCs == parking)
  intermediate_solution[[route_ind]]$VCs <- intermediate_solution[[route_ind]]$VCs[-VC_index]
  
  
  
  return(list(intermediate_solution = intermediate_solution, agg_parkings = agg_parkings, change_park = change_park))
}


# Le pasamos lo siguiente 

#parking = aggregated_list_info_after_removal$aggregated_clients[[i]][j] 
#route_ind = aggregated_list_info_after_removal$aggregated_routes_index[[i]] 





# Creada 07/05/2021
create_subtour_with_TC <- function(input, destination_route, 
                                   tc_client){
  
  
  cl <- numeric()
  dist <- numeric()
  for(i in 2:(length(destination_route$route)-1)){
    client_try <- destination_route$route[[i]]
    cl <- c(cl, client_try)
    dist <- c(dist,input$matriz.distancia[client_try+1,tc_client+1] + input$matriz.distancia[tc_client+1,client_try+1])
  }
  ind_min_dist <- which(dist==min(dist))
  if(length(ind_min_dist)==1){
    chosen_ind <- ind_min_dist
  }else{
    chosen_ind <- sample(ind_min_dist, 1)
  }
  chosen_client <- cl[chosen_ind] # cliente donde vamos a insertar el TC (como subtour con
  # unico cliente)
  chosen_dist <- dist[chosen_ind]
  
  ind_new_park <- which(destination_route$route==chosen_client)
  destination_route$route <- c(destination_route$route[1:ind_new_park], tc_client, chosen_client,
                               destination_route$route[(ind_new_park+1):length(destination_route$route)])
  
  
  return(list(route = destination_route$route, dist = chosen_dist))
  
}

# route_ind = r
# tc_client = inserting_client





# Creada 07/05/2021

creating_CVR_from_PVR <- function(input, initial_solution, intermediate_solution, route_ind, 
                                  tc_client, new_park_from_PVR, problem_type){
  
  
  
  modified_solution <-  intermediate_solution[[route_ind]]
  modified_solution$type <- "CVR"
  
  
  ind_r <- which(names(modified_solution)=="route")
  modified_solution <- modified_solution[c(names(modified_solution)[1:ind_r], names(modified_solution)[length(modified_solution)],
                                           names(modified_solution)[(ind_r+1):(length(modified_solution)-1)])]
  
  
  
  modified_solution$TCs <- tc_client 
  
  if(problem_type=="MCTTRP"){
    include_load <- sum(input$matriz.demandas[tc_client+1,])
    
  }else if(problem_type == "TTRP"){
    include_load <- input$vector.demandas[tc_client+1]
    
  }
  
  modified_solution$total_load <- modified_solution$total_load + include_load
  modified_solution$total_load_tc_clients <- modified_solution$total_load_tc_clients + include_load
  
  client_origin_route_index <- route_of_client(tc_client, initial_solution)$index
  client_origin_route_info <- initial_solution[[client_origin_route_index]]
  
  for(ind_tc in 1:length(client_origin_route_info$clients_tc)){
    if(client_origin_route_info$clients_tc[[ind_tc]]$id == tc_client ){
      client_tc_index <- ind_tc
    }
  }
  client_tc_info <- client_origin_route_info$clients_tc[[client_tc_index]]
  
  modified_solution$clients_tc <- list()
  modified_solution$clients_tc[[1]] <- client_tc_info
  
  if(problem_type=="MCTTRP"){
    client_hoppers_trucks <- length(client_tc_info$hoppers_trucks)
    modified_solution$used_hoppers_truck <- modified_solution$used_hoppers_truck + client_hoppers_trucks
  }
  
  modified_solution$subtours <- list()
  modified_solution$subtours[[1]] <- list()
  modified_solution$subtours[[1]]$tour <- c(new_park_from_PVR, tc_client, new_park_from_PVR)
  modified_solution$subtours[[1]]$root <- new_park_from_PVR
  modified_solution$subtours[[1]]$tc_clients <- tc_client
  modified_solution$subtours[[1]]$length <- 1
  
  
  
  return(modified_solution)
  
}

# route_ind = r
# tc_client = inserting_client


# Creada 07/06/2021

create_new_route_from_insertion <- function(input, inserting_client, intermediate_solution, index_route_insertion,
                                              delta_ins, delta_chosen_position, best_route_ins, 
                                              new_pvr, new_ptr, problem_type){
  
  
  intermediate_solution[[index_route_insertion]] <- list()
  
  
  if(new_pvr){
    intermediate_solution[[index_route_insertion]]$type <- "PVR"
    intermediate_solution[[index_route_insertion]]$route <- best_route_ins
    intermediate_solution[[index_route_insertion]]$cost <- delta_ins[[delta_chosen_position]]
    
    intermediate_solution[[index_route_insertion]]$trailer_number <- index_route_insertion
    intermediate_solution[[index_route_insertion]]$truck_number <- index_route_insertion
    if(problem_type == "MCTTRP"){
      client_demands <- input$matriz.demandas[inserting_client+1,]
      intermediate_solution[[index_route_insertion]]$total_load <- sum(client_demands)
      intermediate_solution[[index_route_insertion]]$total_load_tc_clients <- 0
      
      intermediate_solution[[index_route_insertion]]$used_hoppers_trailer <-  sum(ceiling(client_demands/input$H.trailer[1,1]))
      intermediate_solution[[index_route_insertion]]$used_hoppers_truck <- 0
    }else if(problem_type == "TTRP"){
      client_demands <- input$vector.demandas[inserting_client+1]
      intermediate_solution[[index_route_insertion]]$total_load <- client_demands
      intermediate_solution[[index_route_insertion]]$total_load_tc_clients <- 0
    }
    
    intermediate_solution[[index_route_insertion]]$clients_tc <- list()
    intermediate_solution[[index_route_insertion]]$clients_vc <- list()
    
    intermediate_solution[[index_route_insertion]]$clients_vc[[1]] <- list()
    
    intermediate_solution[[index_route_insertion]]$clients_vc[[1]]$id <- inserting_client
    intermediate_solution[[index_route_insertion]]$clients_vc[[1]]$demands <- client_demands
    
    if(problem_type == "MCTTRP"){ # el "if" (solo eso) es nuevo 09/08/2021
      new_hoppers_trailer <- list()
      ttt <- 1
      client_hoppers_demands_res <- client_demands
      for (f in 1:length(client_demands)){
        if(ceiling(client_demands/input$H.trailer[1,1])[f]!=0){
          while(client_hoppers_demands_res[f]>0){
            quantity <- min(client_hoppers_demands_res[f], input$H.trailer[1,1])
            new_hoppers_trailer[[ttt]] <- data.frame(f, quantity, quantity/input$H.trailer[1,1])
            colnames(new_hoppers_trailer[[ttt]]) <- c("Pienso", "Cantidad", "Proporcion")
            client_hoppers_demands_res[f] <- client_hoppers_demands_res[f] - quantity
            ttt <- ttt + 1
          }
        }
      }
      
      intermediate_solution[[index_route_insertion]]$clients_vc[[1]]$hoppers_trailers <- new_hoppers_trailer
      intermediate_solution[[index_route_insertion]]$clients_vc[[1]]$hoppers_trucks <- list()
    }
    
    intermediate_solution[[index_route_insertion]]$VCs <- inserting_client
  }else if(new_ptr){
    intermediate_solution[[index_route_insertion]]$type <- "PTR"
    
    intermediate_solution[[index_route_insertion]]$route <- best_route_ins
    intermediate_solution[[index_route_insertion]]$cost <- delta_ins[[delta_chosen_position]]
    
    intermediate_solution[[index_route_insertion]]$trailer_number <- 0
    intermediate_solution[[index_route_insertion]]$truck_number <- index_route_insertion
    
    if(problem_type == "MCTTRP"){
      client_demands <- input$matriz.demandas[inserting_client+1,]
      intermediate_solution[[index_route_insertion]]$used_hoppers_truck <-  sum(ceiling(client_demands/input$H.camion[1,1]))
      intermediate_solution[[index_route_insertion]]$used_hoppers_trailer <- 0
      
      
    }else if(problem_type == "TTRP"){
      client_demands <- input$vector.demandas[inserting_client+1]
    }
    
    intermediate_solution[[index_route_insertion]]$total_load <- sum(client_demands)
    if(inserting_client > input$n1){ #TC
      intermediate_solution[[index_route_insertion]]$total_load_tc_clients <- sum(client_demands)
      intermediate_solution[[index_route_insertion]]$clients_vc <- list()
      intermediate_solution[[index_route_insertion]]$clients_tc <- list()
      intermediate_solution[[index_route_insertion]]$clients_tc[[1]] <- list()
      
      intermediate_solution[[index_route_insertion]]$clients_tc[[1]]$id <- inserting_client
      intermediate_solution[[index_route_insertion]]$clients_tc[[1]]$demands <- client_demands
      if(problem_type=="MCTTRP"){
        intermediate_solution[[index_route_insertion]]$clients_tc[[1]]$hoppers_trailers <- list()
        
        new_hoppers_truck <- list()
        ttt <- 1
        client_hoppers_demands_res <- client_demands
        for (f in 1:length(client_demands)){
          if(ceiling(client_demands/input$H.camion[1,1])[f]!=0){
            while(client_hoppers_demands_res[f]>0){
              quantity <- min(client_hoppers_demands_res[f], input$H.camion[1,1])
              new_hoppers_truck[[ttt]] <- data.frame(f, quantity, quantity/input$H.camion[1,1])
              colnames(new_hoppers_truck[[ttt]]) <- c("Pienso", "Cantidad", "Proporcion")
              client_hoppers_demands_res[f] <- client_hoppers_demands_res[f] - quantity
              ttt <- ttt + 1
            }
          }
        }
        
        intermediate_solution[[index_route_insertion]]$clients_tc[[1]]$hoppers_trucks <- new_hoppers_truck
        
        
      }
      intermediate_solution[[index_route_insertion]]$TCs <- inserting_client
      
      
    }else{ #VC
      intermediate_solution[[index_route_insertion]]$total_load_tc_clients <- 0
      intermediate_solution[[index_route_insertion]]$clients_tc <- list()
      
      intermediate_solution[[index_route_insertion]]$clients_vc <- list()
      intermediate_solution[[index_route_insertion]]$clients_vc[[1]] <- list()
      
      intermediate_solution[[index_route_insertion]]$clients_vc[[1]]$id <- inserting_client
      intermediate_solution[[index_route_insertion]]$clients_vc[[1]]$demands <- client_demands
      if(problem_type=="MCTTRP"){
        intermediate_solution[[index_route_insertion]]$clients_vc[[1]]$hoppers_trailers <- list()
        
        new_hoppers_truck <- list()
        ttt <- 1
        client_hoppers_demands_res <- client_demands
        for (f in 1:length(client_demands)){
          if(ceiling(client_demands/input$H.camion[1,1])[f]!=0){
            while(client_hoppers_demands_res[f]>0){
              quantity <- min(client_hoppers_demands_res[f], input$H.camion[1,1])
              new_hoppers_truck[[ttt]] <- data.frame(f, quantity, quantity/input$H.camion[1,1])
              colnames(new_hoppers_truck[[ttt]]) <- c("Pienso", "Cantidad", "Proporcion")
              client_hoppers_demands_res[f] <- client_hoppers_demands_res[f] - quantity
              ttt <- ttt + 1
            }
          }
        }
        
        intermediate_solution[[index_route_insertion]]$clients_vc[[1]]$hoppers_trucks <- new_hoppers_truck
        
        
      }
      intermediate_solution[[index_route_insertion]]$VCs <- inserting_client
      
    }
    
    
  }

return(intermediate_solution)
}