customers_insertion <- function(input, result, aggregated_list_info, aggregated_list_info_after_removal, 
                                       initial_solution, intermediate_solution, no_route_left,
                                       problem_type, penalty_max, selected_insertion){
  
  perturbation_not_obtained <- FALSE
  
  
  for(k in 1:length(aggregated_list_info$aggregated_clients)){
    for(i in 1:length(aggregated_list_info$aggregated_clients[[k]])){
      inserting_client <- aggregated_list_info$aggregated_clients[[k]][i]
      route_try <- list()
      in_subtour <- list()
      create_route <- list()
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
                create_route[[t]] <- 0
                
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
                  create_route[[t]] <- 0
                  
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
                  create_route[[t]] <- 0
                  
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
                  create_route[[t]] <- 0
                  
                  
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
                  create_route[[t]] <- 0
                  
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
                    create_route[[t]] <- 0
                    
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
                  create_route[[t]] <- 0
                  
                  t <- t + 1
                  
                }
                
                
              }
              
            }
            
          }
        }
      }
      
      # 07/06/2021
      # Xa intentei meter os clientes en todas as rutas existentes; agora vou intentar crear novas rutas, 
      # sempre e cando sexa posible (e dicir, se hai flota dispoñible)
      vehicles <- counting_vehicles(intermediate_solution)
      trucks <- vehicles$trucks
      trailers <- vehicles$trailers
      new_pvr <- 0
      new_ptr <- 0
      
      if(inserting_client <= input$n1){ #VC
        if(trailers < input$n_trailers && trucks < input$n_trucks ){ #creo PVR
          new_pvr <- 1
        }else if(trucks < input$n_trucks){ #creo PTR
          new_ptr <- 1
        }
        
      }else{ #TC
        if(trucks < input$n_trucks){ #creo PTR
          new_ptr <- 1
        }
      }
      
      if(new_pvr || new_ptr){
        delta_ins[[t]] <- input$matriz.distancia[1,inserting_client+1] + input$matriz.distancia[inserting_client+1,1]
        new_route_ins[[t]] <- c(0,inserting_client,0)
        in_subtour[[t]] <- 0
        create_route[[t]] <- 1
        index_ins[[t]] <- length(intermediate_solution) + 1
        t <- t + 1
      }
      
      
      if(length(delta_ins) == 0){
        perturbation_not_obtained <- TRUE
        break
        
      }else{
        
        
        
        delta_chosen_position <- choose_delta_insertion(selected_insertion, delta_ins)
        
        index_route_insertion <- index_ins[[delta_chosen_position]]
        # La ruta de la solucion inicial cuyo indice es index_route_insertion la hay que actualizar, de modo que se le añade al cliente inserting_client
        
        best_route_ins <- new_route_ins[[delta_chosen_position]]
        is_best_insertion_in_subtour <- in_subtour[[delta_chosen_position]] # nuevo 29/04/2021
        is_best_insertion_new <- create_route[[delta_chosen_position]] # nuevo 07/06/2021
        # Actualizamos la intermediate_solution:
        new_park_from_PVR <- -1
        
        if(is_best_insertion_new){ # nuevo 07/06/2021
          intermediate_solution <- create_new_route_from_insertion(input, inserting_client, intermediate_solution, index_route_insertion,
                                                                   delta_ins, delta_chosen_position, best_route_ins, 
                                                                   new_pvr, new_ptr, problem_type)

          
        }else{
        
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
 
  #print(perturbation_not_obtained)

  
  
  return(perturbed_solution)
}




# 07/06/2021
counting_vehicles <- function(intermediate_solution){
  trucks <- 0
  trailers <- 0
  for(ii in 1:length(intermediate_solution)){
    if(intermediate_solution[[ii]]$truck_number !=0){
      trucks <- trucks +1
      if(intermediate_solution[[ii]]$trailer_number != 0){
        trailers <- trailers + 1
      }
    }
  }
  return(list(trucks=trucks, trailers=trailers))
}


