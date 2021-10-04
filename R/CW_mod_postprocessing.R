#' Postproc ...
#'
#' @param rutas
#' @param input
#' @return A list of results ...
postproc_add_disconnected_clients_MCTTRP<-function(rutas, input, Tolvas, 
                                                   R, Rhat, CWTTRP_struct, nf){
  
  H.camion_res  <- CWTTRP_struct$H.camion_res
  H.trailer_res <- CWTTRP_struct$H.trailer_res
  H.camion  <- input$H.camion
  H.trailer  <- input$H.trailer
  demandas_res  <- CWTTRP_struct$demandas_res
  n1 <- input$n1 
  n <- input$n 
  t <- CWTTRP_struct$t
  
  num_clientes <- 0
  counter<-length(rutas)+1
  for (zz in 2:length(R[,1])){
    if ((R[zz,1]==0)&&(R[zz,3]==0)&&(Rhat[zz,1]==0)&&
        (Rhat[zz,3]==0)&&(!is.element(R[zz,2],rutas))){
      
      num_clientes <- num_clientes+1 #rev
      
      if (sum(zz==((n1+2):n))==1) {
        type<-1
        result1 = serve_customer(type,H.trailer_res,H.trailer,H.camion_res,H.camion,demandas_res,Tolvas, t, zz, nf)
        H.trailer_res<-result1$H.trailer_res
        H.camion_res<-result1$H.camion_res
        demandas_res<-result1$demandas_res
        Tolvas<-result1$Tolvas
        t<-result1$t
      } else {
        type<-2
        result1 = serve_customer(type,H.trailer_res,H.trailer,H.camion_res,H.camion,demandas_res,Tolvas, t, zz, nf)
        H.trailer_res<-result1$H.trailer_res
        H.camion_res<-result1$H.camion_res
        demandas_res<-result1$demandas_res
        Tolvas<-result1$Tolvas
        t<-result1$t
      }
      rutas[counter]<-R[zz,2]
      counter<-counter+1
      rutas[counter]<-0
      counter<-counter+1
    }
  }
  
  if(num_clientes==2){
    clientes_solos <- numeric(2)
    k <- 1
    
    for(i in 2:(length(rutas)-1)){
      if(rutas[i-1]==0 & rutas[i+1]==0){
        clientes_solos[k] <- rutas[i]
        k <- k + 1
      }
    }
    
    if(2*input$matriz.distancia[1,clientes_solos[1]+1]+
       2*input$matriz.distancia[1,clientes_solos[2]+1] >=
       input$matriz.distancia[1,clientes_solos[1]+1] +
       input$matriz.distancia[clientes_solos[1]+1,clientes_solos[2]+1] +
       input$matriz.distancia[clientes_solos[2]+1,1]) {
      
      if(Tolvas[Tolvas[,1]==clientes_solos[1],3][1] == "truck" &
         Tolvas[Tolvas[,1]==clientes_solos[2],3][1] == "truck"){
        
        trucks_usados_aux <- c(Tolvas[Tolvas[,1]==clientes_solos[1],4],
                               Tolvas[Tolvas[,1]==clientes_solos[2],4])
        
        trucks_usados <- as.numeric(c(trucks_usados_aux[1],
                                      trucks_usados_aux[length(trucks_usados_aux)]))
        
        Tolvas_usadas <- sum(H.camion_res[trucks_usados[1],]==-1) +
          sum(H.camion_res[trucks_usados[2],]==-1)
        
        if( Tolvas_usadas <= dim(H.camion_res)[2]){
          H.camion_res[trucks_usados[1],1:Tolvas_usadas] = rep(-1,Tolvas_usadas)
          H.camion_res[trucks_usados[2],] = input$H.camion[trucks_usados[2],]
          Tolvas[Tolvas[,1]==clientes_solos[2],4] = rep(trucks_usados_aux[1],length(Tolvas[Tolvas[,1]==clientes_solos[2],4]))
          R[clientes_solos[1]+1,3] = clientes_solos[2]
          R[clientes_solos[2]+1,1] = clientes_solos[1]
          for (ii in 1:length(rutas)){
            if(rutas[ii]==clientes_solos[1]){
              rutas[ii+1] = clientes_solos[2]
              rutas[(ii+2):length(rutas)] = 0
            }
            
          }
          
        }
      }
    }
  }
  
  results<-list()
  results$rutas<-rutas
  results$Tolvas<-Tolvas
  results$H.trailer_res<-H.trailer_res
  results$H.camion_res<-H.camion_res
  results$demandas_res<-demandas_res
  results$Tolvas<-Tolvas
  results$t<-t
  results$num_clientes <- num_clientes  #rev1
  
  return(results)
}
#' Postproc ...
#'
#' @param rutas
#' @param input
#' @return A list of results ...
postproc_subroutes_trailer_routes<-function(rutas, matriz.distancia,
                                            Tolvas, R, Rhat, H.trailer_res, 
                                            H.truck_res, input, opt){
  
  rutas.des <- creation_routes_post(rutas, Tolvas)
  coste<-0
  for(i in 1:(length(rutas)-1)){
    coste<-coste+matriz.distancia[rutas[i]+1,rutas[i+1]+1]
  }
  
  for(i in 1:length(rutas.des)){
    if(rutas.des[[i]]$trailer!=0 && rutas.des[[i]]$truck==0){
      new_subroute<-0
      index_client<-1
      index_route<-1
      best_cost <- Inf
      for(j in 1:length(rutas.des)){
        tam1 <- length(rutas.des[[j]]$rutas)
        mincost <- matriz.distancia[rutas.des[[j]]$rutas[1]+1,
                                    rutas.des[[j]]$rutas[2]+1] +
          matriz.distancia[rutas.des[[j]]$rutas[tam1-1]+1,
                           rutas.des[[j]]$rutas[tam1]+1]
        rescost <- coste-mincost
        if(rutas.des[[j]]$trailer==0 && rutas.des[[j]]$truck!=0){
          for(z in 2:(length(rutas.des[[i]]$rutas)-1)){
            tam1 <- length(rutas.des[[i]]$rutas)
            tam2 <- length(rutas.des[[j]]$rutas)
            new_cost <- matriz.distancia[rutas.des[[i]]$rutas[z]+1,
                                         rutas.des[[j]]$rutas[2]+1] +
              matriz.distancia[rutas.des[[j]]$rutas[tam2-1]+1,
                               rutas.des[[i]]$rutas[z]+1]
            #print(paste(new_cost," nodos:", rutas.des[[j]]$rutas[2], rutas.des[[i]]$rutas[z], rutas.des[[j]]$rutas[tam1-2], rutas.des[[i]]$rutas[z], "best_cost:", best_cost))
            flag = 0
            if ((opt==0) &&
                ((new_cost < mincost)&&(new_cost < best_cost))){
              flag = 1
            }
            if ((opt==1) &&
                #((new_cost+rescost < (mincost+rescost)*1.15)&&(new_cost < best_cost))){
                ((new_cost+rescost < (mincost+rescost)*1.15)&&(new_cost < best_cost))){
              flag = 1
            }
            if (flag == 1) {
              index_client <- z
              index_route <- j
              best_cost <- new_cost
              new_subroute <- 1
            }
          }
        }
      }
      if (new_subroute == 1){
        subroute = rutas.des[[index_route]]$rutas
        # Delete subroute in R
        for (ii in 1:length(subroute)){
          for (jj in 2:length(R[,1])){
            if (R[jj,2] == subroute[ii]){
              R[jj,1] <- 0
              R[jj,3] <- 0
            }
          }
        }
        # Add subroute in Rhat (corregido con respecto a David)
        for (ii in 1:length(subroute)){
          for (jj in 2:length(Rhat[,1])){
            if (Rhat[jj,2] == subroute[ii]){
              if(length(subroute)!=3){ #rev1
                if (ii==2){
                  Rhat[jj,1] <- rutas.des[[i]]$rutas[index_client]
                  Rhat[jj,3] <- subroute[ii+1]
                }
                else if (ii==length(subroute)-1){
                  Rhat[jj,1] <- subroute[ii-1]
                  Rhat[jj,3] <- rutas.des[[i]]$rutas[index_client]
                } else {
                  Rhat[jj,1] <- subroute[ii-1]
                  Rhat[jj,3] <- subroute[ii+1]
                }
              }else{
                if(ii==2){ #rev1
                  Rhat[jj,1] <- rutas.des[[i]]$rutas[index_client]
                  Rhat[jj,3] <- rutas.des[[i]]$rutas[index_client]
                }
              }
              
            }
          }
        }
        Rhat[rutas.des[[i]]$rutas[index_client]+1,1]<-subroute[length(subroute)-1] # estaban intercambiadas las posiciones 1 y 3
        Rhat[rutas.des[[i]]$rutas[index_client]+1,3]<-subroute[2]
        
        # Add subroute in rutas
        new_rutas<-c(rutas[1])
        counter<-2
        subroute<-subroute[2:(length(subroute)-1)]
        for (ii in 2:length(rutas)){
          if (rutas[ii]!=rutas.des[[i]]$rutas[index_client]){
            if (!is.element(rutas[ii],subroute)){
              #if (rutas[ii]!=new_rutas[counter-1]){
              new_rutas[counter]<-rutas[ii]
              counter<-counter+1
              #}
            }
          }
          else {
            #if (rutas[ii]!=new_rutas[counter-1]){
            new_rutas[counter]<-rutas[ii]
            counter<-counter+1
            #}
            for (jj in 1:length(subroute)){
              #if (rutas[ii]!=new_rutas[counter-1]){
              new_rutas[counter]<-subroute[jj]
              counter<-counter+1
              #}
            }
            #if (rutas[ii]!=new_rutas[counter-1]){
            new_rutas[counter]<-rutas[ii]
            counter<-counter+1
            #}
          }
        }
        
        rutas<-new_rutas
        
      }
    }
  }
  
  # Add subroute in rutas.des
  rutas.des <- creation_routes_post(rutas, Tolvas)
  
  rupdate <- update_Hopper_matrix(Tolvas, H.trailer_res, H.truck_res, input, rutas) 
  
  results<-list()
  results$rutas<-rutas
  results$R<-R
  results$Rhat<-Rhat
  results$rutas.des <- rutas.des
  results$Hoppers <- rupdate$Hoppers
  results$H.trailer_res <- rupdate$H.trailer_res
  results$H.truck_res <- rupdate$H.truck_res
  
  
  return(results)
}

#' Postproc ...
#'
#' @param rutas
#' @param input
#' @return A list of results ...
creation_routes_post <- function(rutas, Tolvas){
  
  n.rutas <- sum(rutas==0)-1
  rutas.depot.start <- which(rutas==0)[-length(which(rutas==0))]
  rutas.depot.end <- which(rutas==0)[-1]
  
  #for (i in n.rutas){
  #  rutas.indiv[i,] <- rutas[rutas.depot[i]:rutas.depot[i+1]]
  #}
  
  rutas.indiv <- mapply(function(x, y) t(rutas[seq(x, y)]), rutas.depot.start, rutas.depot.end)
  class(rutas.indiv)
  r.trailer <- numeric(n.rutas)
  r.truck <- numeric(n.rutas)
  
  for (i in 1:length(rutas.indiv)){
    #print(rutas.indiv)
    for (j in rutas.indiv[[i]][2:(length(rutas.indiv[[i]])-1)]){
      for(k in which(Tolvas[,1]==j)){
        if(Tolvas[k,3]=="trailer"){
          r.trailer[i] <- as.numeric(Tolvas[k,4])
        }else{
          r.truck[i] <- as.numeric(Tolvas[k,4])
        }
      }
    }
  }
  
  rutas.des=list()
  for(i in 1:length(rutas.indiv)){
    ruta.aux=list()
    ruta.aux=list(rutas=rutas.indiv[[i]],trailer=r.trailer[i],truck=r.truck[i])
    rutas.des[[i]]<-ruta.aux
  }
  
  return(rutas.des)
  
}


#' Postproc ...
#'
#' @param type
#' @param H.trailer_res
#' @return A list of results ...
serve_customer<-function(type,H.trailer_res,H.trailer,H.camion_res,H.camion,
                         demandas_res,Tolvas, t, zz, nf){
  if (type==1){
    s<-min(which(H.camion_res[,1]!=-1))
  } else {
    s<-min(which(H.trailer_res[,1]!=-1))
  }
  tc<-1
  for (i in 1:nf){
    while(demandas_res[zz,i]!=0 && sum(H.camion_res[s,])!=-dim(H.camion)[2] ){
      while (demandas_res[zz,i]>max(H.camion_res) && sum(H.camion_res[s,])!=-dim(H.camion)[2]){
        if(Tolvas[t-1,1]==0 && t>2){
          t <- min(which(Tolvas[,1]==0))
        }
        Tolvas[t,] <- c(zz-1,i,"truck",s,max(H.camion),1)
        demandas_res[zz,i] <- demandas_res[zz,i] - max(H.camion)
        H.camion_res[s,tc] <- -1
        
        if (tc < dim(H.camion)[2]){
          tc <- tc+1
        }
        else{
          tc <- 1
        }
        
        t <- t+1
      }
      
      if(sum(H.camion_res[s,])!=-dim(H.camion)[2]){
        if(Tolvas[t-1,1]==0 && t>2){
          t <- min(which(Tolvas[,1]==0))
        }
        if (type==1){
          Tolvas[t,] <- c(zz-1,i,"truck",s,demandas_res[zz,i],demandas_res[zz,i]/max(H.camion))
          demandas_res[zz,i] <- max(0,demandas_res[zz,i] - max(H.camion))
          H.camion_res[s,tc] <- -1
          if (tc < dim(H.camion)[2]){
            tc <- tc+1
          }else{
            tc <- 1
          }
        } else {
          Tolvas[t,] <- c(zz-1,i,"trailer",s,demandas_res[zz,i],demandas_res[zz,i]/max(H.trailer))
          demandas_res[zz,i] <- max(0,demandas_res[zz,i] - max(H.trailer))
          H.trailer_res[s,tc] <- -1
          if (tc < dim(H.trailer)[2]){
            tc <- tc+1
          }else{
            tc <- 1
          }
        }
        
        t <- t+1
      }
    }
  }
  
  results<-list()
  results$H.trailer_res<-H.trailer_res
  results$H.camion_res<-H.camion_res
  results$demandas_res<-demandas_res
  results$Tolvas<-Tolvas
  results$t<-t
  
  return(results)
  
}


#' Postproc ...
#'
#' @param rutas
#' @param input
#' @return A list of results ...
postproc_add_disconnected_clients_TTRP<-function(rutas_res, rutas, input, R, Rhat){
  
  for (i in 2:(length(R[,1]))) {
    if ((R[i,1]==0)&&(R[i,3]==0)&&(Rhat[i,1]==0)&&(Rhat[i,3]==0)) {
      node_to_add <- i - 1
      node_cap <- calc_load2(c(node_to_add), input$vector.demandas )
      min_value <- Inf
      index_to_insert <- -1
      subroute_selected <- -1
      for (j in 1:length(rutas_res)) {
        next_r <- 1
        if (rutas_res[[j]]$type == "PVR") {
          if (node_to_add > input$n1) {
            limit_cap <- input$capacidad.truck;
            change_type <- "PTR"
          } else {
            limit_cap <- input$capacidad.vehiculo;
            change_type <- "PVR"
          }
        }
        else if (rutas_res[[j]]$type == "PTR") {
          limit_cap <- input$capacidad.truck;
          change_type <- "PTR"
        }
        else if (rutas_res[[j]]$type == "CVR"){
          limit_cap <- input$capacidad.vehiculo;
          if (node_to_add > input$n1) next_r <- 0
          change_type <- "CVR"
        }
        
        if (((node_cap + rutas_res[[j]]$total_load) <= limit_cap) && (next_r)) {
          subroute <- append(0, node_to_add)
          subroute <- append(subroute, rutas_res[[j]]$route[2:length(rutas_res[[j]]$route)])
          cost <- calc_load2(subroute, input$vector.demandas)
          if (cost < min_value ) {
            min_value <- cost
            index_to_insert <- j
            subroute_selected <- subroute
            select_type <- change_type
          }
          subroute <- append(rutas_res[[j]]$route[1:(length(rutas_res[[j]]$route)-1)], node_to_add)
          subroute <- append(subroute, 0)
          cost <- calc_load2(subroute, input$vector.demandas)
          if (cost < min_value ) {
            min_value <- cost
            index_to_insert <- j
            subroute_selected <- subroute
            select_type <- change_type
          }
        }
      }
      
      if (index_to_insert != -1) {
        rutas_res[[index_to_insert]]$route <- subroute_selected
        rutas_res[[index_to_insert]]$type <- select_type
      } else {
        index_to_insert <- length(rutas_res)+1
        subroute <- append(0, node_to_add)
        subroute <- append(subroute, 0)
        rutas_res[[index_to_insert]] <- list()
        rutas_res[[index_to_insert]]$route <-  subroute
        if (node_to_add <= input$n1) { rutas_res[[index_to_insert]]$type <- "PVR"; }
        else {  rutas_res[[index_to_insert]]$type <- "PTR"; }
        rutas_res[[index_to_insert]]$total_load <-calc_load2(subroute, input$vector.demandas)
        rutas_res[[index_to_insert]]$total_load_tc_clients <- calc_load_only_truck(subroute, input$vector.demandas, input)
        rutas_res[[index_to_insert]]$cost <- local_cost(subroute, input$matriz.distancia)
      }
      
    }
  }
  
  return(rutas_res)
}

#' Postproc ...
#'
#' @param rutas
#' @param input
#' @return A list of results ...
postproc_TTRP<-function(rutas_res, rutas, input, R, Rhat){
  
  unfeasibility_fleet <- check_feasibility_fleet(input)
  # Add new fleet if it is unfeasibility
  if (unfeasibility_fleet) {
    
    # ADD DISCONNECTED CLIENTS
    rutas_res <- postproc_add_disconnected_clients_TTRP(rutas_res, rutas, input, R, Rhat)
    rutas_res <- postproc_add_new_subroutes_TTRP(rutas_res, rutas, input, 0)
    rutas_res <- postproc_add_new_subroutes_TTRP(rutas_res, rutas, input, 1)
    rutas_res <- result_improvement(input, rutas_res, "TTRP")
    
    # ADJUST THE FLEET
    result_fleet <- adjusts_n_trailers_and_first_transformations(rutas_res, input, "TTRP")
    rutas_res <- result_fleet$rutas_res
    n_ptr <- result_fleet$n_ptr
    rutas_res <- result_improvement(input, rutas_res, "TTRP")
    
    # SELECTED ROUTES
    selected_pvr_cvr_index <- select_PVRs_CVRs(rutas_res, input$n_trailers, input)
    non_selected_pvr_cvr_index <- return_non_selected_pvr_cvr(rutas_res, selected_pvr_cvr_index)
    selected_ptr_index <- select_PTRs(rutas_res, n_ptr, input)
    non_selected_ptr_index <- return_non_selected_ptr(rutas_res, selected_ptr_index)

    ## SPLIT AND INSERT
    if ((length(non_selected_pvr_cvr_index)!=0)||(length(non_selected_ptr_index)!=0)) {
      result_split <- split_and_insert(rutas_res, selected_pvr_cvr_index, non_selected_pvr_cvr_index, 
                                    selected_ptr_index, non_selected_ptr_index, input, "TTRP", 0)
      rutas_res <- result_split$rutas_res
      non_selected_ptr_index <- result_split$non_selected_ptr_index
      non_selected_pvr_cvr_index <- result_split$non_selected_pvr_cvr_index
    }

    ## TRANSFORM AND SPLIT AGAIN
    #if ((length(non_selected_pvr_cvr_index)!=0)||(length(non_selected_ptr_index)!=0)) {
      
    #  result_transform_space <- transform_marginal_routes(rutas_res, non_selected_ptr_index, non_selected_pvr_cvr_index, selected_ptr_index, selected_pvr_cvr_index, input, "TTRP")
    #  rutas_res <- result_transform_space$rutas_res
    #  non_selected_ptr_index <- result_transform_space$non_selected_ptr_index 
    #  non_selected_pvr_cvr_index <- result_transform_space$non_selected_pvr_cvr_index 
      
    #}

    # SWITCH RESIDUAL ROUTES
    if ((length(non_selected_pvr_cvr_index)!=0)||(length(non_selected_ptr_index)!=0)) {
  
      rutas_res <- switch_routes(rutas_res, append(selected_pvr_cvr_index, selected_ptr_index), 
                                            append(non_selected_pvr_cvr_index, non_selected_ptr_index), input, "TTRP")
        
      result_split <- split_and_insert(rutas_res, selected_pvr_cvr_index, non_selected_pvr_cvr_index, 
                                       selected_ptr_index, non_selected_ptr_index, input, "TTRP", 1)
        
      rutas_res <- result_split$rutas_res
      non_selected_ptr_index <- result_split$non_selected_ptr_index
      non_selected_pvr_cvr_index <- result_split$non_selected_pvr_cvr_index
      
    }
     
    rutas_res <- clean_rutas_res(rutas_res)
  }
  
  return(rutas_res)
}


delete_duplicities<-function(solution, input) {
  
  vector_to_delete_i <- list()
  counter_i <- 1
  vector_to_delete_j <- list()
  counter_j <- 1
  cliente_duplicado <- c()
  for (i in 1:(length(solution)-1)) {
    for (j in 2:(length(solution[[i]]$route)-1)) {
      for (ii in (i+1):length(solution)) {
        for (jj in 2:(length(solution[[ii]]$route)-1)) {
          if (solution[[i]]$route[j] == solution[[ii]]$route[jj]) {
            vector_to_delete_i[[counter_i]] <- ii
            vector_to_delete_j[[counter_j]] <- jj
            counter_i <- counter_i + 1
            counter_j <- counter_j + 1
            cliente_duplicado <- c(cliente_duplicado, solution[[i]]$route[j])
          }
        }
      }
    }
  }
  
  if (length(cliente_duplicado)) {
    print("clientes duplicados")
    print(cliente_duplicado)
  }
  
  if (length(vector_to_delete_i)) {
    for (i in 1:length(vector_to_delete_i)){
      route <- c(0)
      ii <- vector_to_delete_i[[i]]
      solution_i <- solution[[ii]]
      for (j in 2:(length(solution_i$route)-1)) {
        if (j != vector_to_delete_j[[i]]) route <- c(route, solution_i$route[j])
        else index_to_delete <- solution_i$route[j]
      }
      route <- c(route, 0)
      solution_i$route <- route
      if (index_to_delete > input$n1) {
        index_to_del <- 0
        for (z in 1:length(solution_i$clients_tc)) {
          if (solution_i$clients_tc[[z]]$id == index_to_delete) index_to_del <- z
        }
        solution_i$used_hoppers_truck <- solution_i$used_hoppers_truck - length(solution_i$clients_tc[[index_to_del]]$hoppers_trucks)
        solution_i$used_hoppers_trailer <- solution_i$used_hoppers_trailer - length(solution_i$clients_tc[[index_to_del]]$hoppers_trailers)
        solution_i$clients_tc[[index_to_del]] <- NULL
        
        TCs_1 <- c()
        for (z in 1:length(solution_i$TCs)){
          if (solution_i$TCs[z] != index_to_delete) {
            TCs_1 <- c(TCs_1, solution_i$TCs[z])
          }
        }
        solution_i$TCs <- TCs_1
      } 
      else {
        index_to_del <- 0
        for (z in 1:length(solution_i$clients_vc)) {
          if (solution_i$clients_vc[[z]]$id == index_to_delete) index_to_del <- z
        }
        solution_i$used_hoppers_truck <- solution_i$used_hoppers_truck - length(solution_i$clients_vc[[index_to_del]]$hoppers_trucks)
        solution_i$used_hoppers_trailer <- solution_i$used_hoppers_trailer - length(solution_i$clients_vc[[index_to_del]]$hoppers_trailers)
        solution_i$clients_vc[[index_to_del]] <- NULL
        
        VCs_1 <- c()
        for (z in 1:length(solution_i$VCs)){
          if (solution_i$VCs[z] != index_to_delete) {
            VCs_1 <- c(VCs_1, solution_i$VCs[z])
          }
        }
        solution_i$VCs <- VCs_1
      }
      
      solution[[ii]] <- solution_i
      
    }
  }
  
  
  for (i in 1:(length(solution))) {
    solution_i <- solution[[i]]
    counter_h_truck <- 0
    counter_h_trailer <- 0
    if (length(solution_i$clients_vc)) {
        for (j in 1:length(solution_i$clients_vc)) {
          counter_h_truck <- counter_h_truck + length(solution_i$clients_vc[[j]]$hoppers_trucks)
          counter_h_trailer <- counter_h_trailer+ length(solution_i$clients_vc[[j]]$hoppers_trailers)
        }
    }
    if (length(solution_i$clients_tc)) {
      for (j in 1:length(solution_i$clients_tc)) {
        counter_h_truck <- counter_h_truck + length(solution_i$clients_tc[[j]]$hoppers_trucks)
        counter_h_trailer <- counter_h_trailer + length(solution_i$clients_tc[[j]]$hoppers_trailers)
      }
    }
    solution_i$used_hoppers_truck <- counter_h_truck
    solution_i$used_hoppers_trailer <- counter_h_trailer
    solution[[i]] <- solution_i
  }

  return(solution)
}



#' Postproc ...
#'
#' @param rutas
#' @param input
#' @return A list of results ...
#' 
postproc_MCTTRP<-function(routes, input, R, Rhat, Hoppers, CWTTRP_struct, nf){
  
    # ADD DISCONNECTED CLIENTS
    result_postproc1 <- postproc_add_disconnected_clients_MCTTRP(routes, input, Hoppers, 
                                                                 R, Rhat, CWTTRP_struct, nf)
    routes <- delete_dupl_zeros_route(result_postproc1$rutas)
    Hoppers <- result_postproc1$Tolvas
    CWTTRP_struct <- result_postproc1$CWTTRP_struct
    num_clientes <- result_postproc1$num_clientes #rev1
    
    
    result_postproc2 <- postproc_subroutes_trailer_routes(routes, input$matriz.distancia,
                                                          Hoppers, R, Rhat, CWTTRP_struct$H.trailer_res, CWTTRP_struct$H.truck_res, input, 0)
    
    routes <- result_postproc2$rutas
    R <- result_postproc2$R
    Rhat <- result_postproc2$Rhat
    rutas_res <- result_postproc2$rutas.des
    Hoppers <- result_postproc2$Hoppers
    
    result_postproc2 <- postproc_subroutes_trailer_routes(routes, input$matriz.distancia,
                                                          Hoppers, R, Rhat, CWTTRP_struct$H.trailer_res, CWTTRP_struct$H.truck_res, input, 1)
    routes <- result_postproc2$rutas
    R <- result_postproc2$R
    Rhat <- result_postproc2$Rhat
    rutas_res <- result_postproc2$rutas.des
    Hoppers <- result_postproc2$Hoppers
    
    
    rutas_res <-  create_result_struct(routes, input, "MCTTRP")
    rutas_res <-  extract_info_from_hoppers(rutas_res, Hoppers, input)
    #rutas_res <-  delete_duplicities(rutas_res, input) # PARCHE
      
    # comentas
    n_current_trucks <- length(rutas_res)
    n_current_trailers <- length(rutas_res) - length(return_ptr_index(rutas_res))
    
    
    if (!((n_current_trucks == input$n_trailers)&&(n_current_trailers == input$n_trucks))) {

    # ADJUST THE FLEET
      result_fleet <- adjusts_n_trailers_and_first_transformations(rutas_res, input, "MCTTRP")
      rutas_res <- result_fleet$rutas_res
      n_ptr <- result_fleet$n_ptr
      
      
      # SELECTED ROUTES
      selected_pvr_cvr_index <- select_PVRs_CVRs(rutas_res, input$n_trailers, input)
      non_selected_pvr_cvr_index <- return_non_selected_pvr_cvr(rutas_res, selected_pvr_cvr_index)
      selected_ptr_index <- select_PTRs(rutas_res, n_ptr, input)
      non_selected_ptr_index <- return_non_selected_ptr(rutas_res, selected_ptr_index)
      
      ## SPLIT AND INSERT
      if ((length(non_selected_pvr_cvr_index)!=0)||(length(non_selected_ptr_index)!=0)) {
        result_split <- split_and_insert(rutas_res, selected_pvr_cvr_index, non_selected_pvr_cvr_index, 
                                         selected_ptr_index, non_selected_ptr_index, input, "MCTTRP", 1)
        rutas_res <- result_split$rutas_res
        non_selected_ptr_index <- result_split$non_selected_ptr_index
        non_selected_pvr_cvr_index <- result_split$non_selected_pvr_cvr_index
      }
      
      ## SWITCH RESIDUAL ROUTES
      if ((length(non_selected_pvr_cvr_index)!=0)||(length(non_selected_ptr_index)!=0)) {
  
         result_transform_space <- transform_marginal_routes(rutas_res, non_selected_ptr_index, 
                                                              non_selected_pvr_cvr_index, selected_ptr_index, selected_pvr_cvr_index, input, "MCTTRP")
         rutas_res <- result_transform_space$rutas_res
         non_selected_ptr_index <- result_transform_space$non_selected_ptr_index 
         non_selected_pvr_cvr_index <- result_transform_space$non_selected_pvr_cvr_index 
      
      }
       
      if ((length(non_selected_pvr_cvr_index)!=0)||(length(non_selected_ptr_index)!=0)) {
        
        rutas_res <- switch_routes(rutas_res, append(selected_pvr_cvr_index, selected_ptr_index), 
                                   append(non_selected_pvr_cvr_index, non_selected_ptr_index), input, "MCTTRP")
      
        result_split <- split_and_insert(rutas_res, selected_pvr_cvr_index, non_selected_pvr_cvr_index, 
                                         selected_ptr_index, non_selected_ptr_index, input, "MCTTRP", 1)
        
        rutas_res <- result_split$rutas_res
        non_selected_ptr_index <- result_split$non_selected_ptr_index
        non_selected_pvr_cvr_index <- result_split$non_selected_pvr_cvr_index
        
      }
      
      rutas_res <- clean_rutas_res(rutas_res)
    }

    return(rutas_res)
}

#' Control subroutine: check_feasibility_fleet
#'
#' @param input
#' @return Boolean value
check_feasibility_fleet<-function(input) {
  total_capacity_trucks <- input$capacidad.truck * input$n_trucks
  total_capacity_total  <- total_capacity_trucks + input$capacidad.trailer * input$n_trailers
  
  total_demands <- sum(input$vector.demandas)
  total_demands_trucks <- sum(input$vector.demandas[(input$n1+2):length(input$vector.demandas)])
  
  unfeasible_fleet <- 0
  if (total_capacity_total > total_demands) unfeasible_fleet <- 1
  if (total_demands_trucks > total_demands_trucks) unfeasible_fleet <- 1
  
  return(unfeasible_fleet)
}

adjusts_n_trailers_and_first_transformations<-function(rutas_res, input, type_problem) {
  
  ptr_index <- return_ptr_index(rutas_res)
  current_n_trailers <- return_VTR_CVR_routes(rutas_res)

  if (length(ptr_index)) {
    if (current_n_trailers < input$n_trailers) {
      n_convert_ptr_cvr <- input$n_trailers - current_n_trailers
      if (length(n_convert_ptr_cvr)) {
        rutas_res <- convert_PTR_CVR(rutas_res, ptr_index, n_convert_ptr_cvr, input, type_problem)
      }
    }
  }
  
  
  if (current_n_trailers > input$n_trailers) {
    n_trailers_to_delete <- current_n_trailers - input$n_trailers
    pvr_index <- return_pvr_index(rutas_res)
    pcr_index <- return_cvr_index(rutas_res)
    if (length(pcr_index)&&(length(ptr_index))) {
      rutas_res <- merge_route_CVR(rutas_res, pvr_index, pcr_index, n_trailers_to_delete, input, "PVR", type_problem)
    }
  }
  
  n_ptr <- input$n_trucks - input$n_trailers
  current_n_ptr <- return_PTR_routes(rutas_res)
  if (current_n_ptr > n_ptr) {
    n_trucks_to_delete <- current_n_ptr - n_ptr
    ptr_index <- return_ptr_index(rutas_res)
    pcr_index <- return_cvr_index(rutas_res)
    if (length(pcr_index)&&(length(ptr_index))) {
      rutas_res <- merge_route_CVR(rutas_res, ptr_index, pcr_index, n_trucks_to_delete, input, "PTR", type_problem)
    }
  }
  
  result_fleet <- list()
  result_fleet$n_ptr <- n_ptr
  result_fleet$rutas_res <- rutas_res
  
  return(result_fleet)
}

transform_marginal_routes<-function(rutas_res, non_selected_ptr_index, non_selected_pvr_cvr_index, selected_ptr_index, selected_pvr_cvr_index, input, type_problem) {
  result_transform_space <- list()
  
  route_tc_to_add <- list()
  counter_route_tc_to_add <- 1
  for (i in 1:length(rutas_res)) {
    if (length(rutas_res[[i]]$route)>3) {
      size <- 0
      if (rutas_res[[i]]$type == "CVR") {
          segment_routes <- split_vc_tc_subroute( rutas_res[[i]]$route, input)
          size <- length(segment_routes$route_vc)
      }
    
      if (rutas_res[[i]]$type == "PTR") {
          segment_routes <- split_vc_tc( rutas_res[[i]]$route, input)
          size <- length(segment_routes$route_vc)
      }
      
      if (size>2) {
        if ((length(segment_routes$route_tc)>2)) {
          old_solution <- rutas_res[[i]]
          rutas_res[[i]]$route  <- segment_routes$route_tc
          
          if (rutas_res[[i]]$type == "CVR") {
            rutas_res[[i]]$main_tour <- return_main_route(rutas_res[[i]]$route)
            rutas_res[[i]]$subtours <- return_subroutes(rutas_res[[i]]$route, input$n1)
          }
          
          if ( type_problem == "TTRP") {
              rutas_res[[i]]$total_load <- calc_load2(segment_routes$route_tc, input$vector.demandas)
              rutas_res[[i]]$total_load_tc_clients <- calc_load_only_truck(segment_routes$route_tc, input$vector.demandas, input)
          }
          if ( type_problem == "MCTTRP") {
            rutas_res[[i]]$total_load <- calc_load2_MC(segment_routes$route_tc, input$matriz.demandas)
            rutas_res[[i]]$total_load_tc_clients <- calc_load_only_truck_MC(segment_routes$route_tc, input$matriz.demandas, input)
          }
          rutas_res[[i]]$cost <- local_cost(segment_routes$route_tc, input$matriz.distancia)
          if (type_problem == "MCTTRP") {
            if  (rutas_res[[i]]$type == "PTR") res_r <- insert_hoppers_MCTTRP_PTR(rutas_res[[i]], rutas_res[[i]], input)
            if  (rutas_res[[i]]$type == "CVR") res_r <- insert_hoppers_MCTTRP_CVR_update(rutas_res[[i]]$route, old_solution, input)
            rutas_res[[i]]$clients_tc <- res_r$clients_tc
            rutas_res[[i]]$clients_vc <- res_r$clients_vc
            rutas_res[[i]]$used_hoppers_truck <- res_r$used_hoppers_truck
            rutas_res[[i]]$used_hoppers_trailer <- res_r$used_hoppers_trailer
          }
        }
          
        if ((length(segment_routes$route_vc)>2)&&(length(segment_routes$route_tc)>2)) {
          new_route <- list()
          new_route$type <- "PVR"
          new_route$route <- segment_routes$route_vc
          new_route$total_load <- calc_load2(segment_routes$route_vc, input$vector.demandas)
          new_route$total_load_tc_clients <- calc_load_only_truck(segment_routes$route_vc, input$vector.demandas, input)
          if ( type_problem == "TTRP") {
            new_route$total_load <- calc_load2(segment_routes$route_vc, input$vector.demandas)
            new_route$total_load_tc_clients <- calc_load_only_truck(segment_routes$route_vc, input$vector.demandas, input)
          }
          if ( type_problem == "MCTTRP") {
            new_route$total_load <- calc_load2_MC(segment_routes$route_vc, input$matriz.demandas)
            new_route$total_load_tc_clients <- calc_load_only_truck_MC(segment_routes$route_vc, input$matriz.demandas, input)
          }
          new_route$cost <- local_cost(segment_routes$route_vc, input$matriz.distancia)
          if (type_problem == "MCTTRP") {
            res_r <- insert_hoppers_MCTTRP_PVR_new(new_route, new_route, input)
            new_route$clients_tc <- res_r$clients_tc
            new_route$clients_vc <- res_r$clients_vc
            new_route$used_hoppers_truck <- res_r$used_hoppers_truck
            new_route$used_hoppers_trailer <- res_r$used_hoppers_trailer
          }
          route_tc_to_add[[counter_route_tc_to_add]] <- new_route
          counter_route_tc_to_add <- counter_route_tc_to_add + 1
        }
        
        if ((length(segment_routes$route_vc)>2)&&(length(segment_routes$route_tc)<=2)) {
          rutas_res[[i]]$route  <- segment_routes$route_vc
        }
      }
    }
  }
  
  if (counter_route_tc_to_add > 1) {
    for (i in 1:length(route_tc_to_add)) {
      rutas_res[[(length(rutas_res)+1)]] <- route_tc_to_add[[i]]
      non_selected_pvr_cvr_index <- append(non_selected_pvr_cvr_index, length(rutas_res))
    } 
  }
  
  if ((length(non_selected_pvr_cvr_index)!=0)||(length(non_selected_ptr_index)!=0)) {
    
    result_split <- split_and_insert(rutas_res, selected_pvr_cvr_index, non_selected_pvr_cvr_index, 
                                     selected_ptr_index, non_selected_ptr_index, input, "MCTTRP", 1)
    rutas_res <- result_split$rutas_res
    non_selected_ptr_index <- result_split$non_selected_ptr_index
    non_selected_pvr_cvr_index <- result_split$non_selected_pvr_cvr_index
  }
  
  result_transform_space <- list()
  result_transform_space$rutas_res <- rutas_res
  result_transform_space$non_selected_ptr_index <- non_selected_ptr_index
  result_transform_space$non_selected_pvr_cvr_index <- non_selected_pvr_cvr_index
  
  return(result_transform_space)
}

return_fleet<-function(routes_res) {
  fleet_stats <- list()
  fleet_stats$trucks <- 0
  fleet_stats$trailers <- 0 
  
  for (i in 1:(length(routes_res))) {
    load_total <- fleet_stats$total_load
    if (routes_res[[i]]$type == "PTR") {
      fleet_stats$trucks <- fleet_stats$trucks + 1
    } 
    else if ((routes_res[[i]]$type == "VTR")&&(load_total <= input$capacidad.truck)){
      fleet_stats$trucks <- fleet_stats$trucks + 1
    }
    else {
      fleet_stats$trailers <- fleet_stats$trailers + 1
    }
  }
  
  return(fleet_stats)
}


split_and_insert<-function(rutas_res, selected_pvr_cvr_index, non_selected_pvr_cvr_index, selected_ptr_index, non_selected_ptr_index, input, type_problem, enable) {
  
  if (length(non_selected_ptr_index)) {
    res_list <- split_all_vc_tc(rutas_res, non_selected_ptr_index, input, type_problem)
    routes_to_add_only_tc <- res_list$routes_to_add_only_tc 
    routes_to_add_only_vc <- res_list$routes_to_add_only_vc 
    rutas_res <- res_list$rutas_res
  }
  
  if (length(non_selected_pvr_cvr_index)) {
    res_list <- split_all_vc_tc(rutas_res, non_selected_pvr_cvr_index, input, type_problem)
    if (length(non_selected_ptr_index)) {
      routes_to_add_only_tc <- append(routes_to_add_only_tc, res_list$routes_to_add_only_tc )
      routes_to_add_only_vc <- append(routes_to_add_only_vc, res_list$routes_to_add_only_vc )
    }
    else {
      routes_to_add_only_tc <- res_list$routes_to_add_only_tc 
      routes_to_add_only_vc <- res_list$routes_to_add_only_vc 
    }
    rutas_res <- res_list$rutas_res
  }
  
  index_trucks <- list()
  counter1 <- 1
  for (i in 1:length(selected_pvr_cvr_index)) {
    if (rutas_res[[selected_pvr_cvr_index[[i]]]]$type == "CVR") {
      index_trucks[counter1] <- selected_pvr_cvr_index[[i]]
      counter1 <- counter1 + 1
    }
  }
  index_trucks <- append(selected_pvr_cvr_index, selected_ptr_index) #append(index_trucks, selected_ptr_index)
  index_trailers <- append(selected_pvr_cvr_index, selected_ptr_index)
  
  #print("index_trucks")
  #print(index_trucks)
  #print("index_trailers")
  #print(index_trailers)
  #print(paste0("rutas total->", length(rutas_res)))

  # "add to trucks"
  #print("ADD TO TRUCKS")
  #readline()
  
  stop_cond <- 0
  while ((length(routes_to_add_only_tc))&&(stop_cond != 1)) {
    
    result_ci <- check_insert_segment_in_route(routes_to_add_only_tc, rutas_res, index_trucks, input, "tc", type_problem, enable)
    
    routes_to_add_only_tc <- result_ci$routes_to_add
    rutas_res <- result_ci$rutas_res
    
    result_up <- update_route_to_add(routes_to_add_only_tc)
    routes_to_add_only_tc <- result_up$routes_to_add
    updated_res <- result_up$updated
    
    #print("routes_to_add_only_tc")
    #if (length(routes_to_add_only_tc)) {
    #  for ( ii in 1:length(routes_to_add_only_tc)) {
    #    print(rutas_res[[routes_to_add_only_tc[[ii]]]]$route)
    #  }
    #}
    #readline()
    
    if ((length(routes_to_add_only_tc)>0)&&(updated_res==0)) {
      res <- split_all_middle(rutas_res, routes_to_add_only_tc, input, type_problem)
      rutas_res <- res$rutas_res 
      routes_to_add_only_tc <- res$group_index
      non_split_routes <- res$non_split_routes
      if (length(routes_to_add_only_tc) == non_split_routes) stop_cond <- 1
    }
    
  }
  
  #print("ADD TO TRAILERS")
  #readline()
  ## add to trailers
  stop_cond <- 0
  while ((length(routes_to_add_only_vc))&&(stop_cond != 1)) {
    
    result_ci <- check_insert_segment_in_route(routes_to_add_only_vc, rutas_res, index_trailers, input, "vc", type_problem, enable)
    
    routes_to_add_only_vc <- result_ci$routes_to_add
    rutas_res <- result_ci$rutas_res
    
    result_up <- update_route_to_add(routes_to_add_only_vc)
    routes_to_add_only_vc <- result_up$routes_to_add
    updated_res <- result_up$updated
    
    #print(routes_to_add_only_vc)
    
    if (length(routes_to_add_only_vc>0)&&(updated_res==0)) {
      res <- split_all_middle(rutas_res, routes_to_add_only_vc, input, type_problem)
      rutas_res <- res$rutas_res 
      routes_to_add_only_vc <- res$group_index
      non_split_routes <- res$non_split_routes
      if (length(routes_to_add_only_vc) == non_split_routes) stop_cond <- 1
    }
    
  }
    
  result_split <- list()
  result_split$rutas_res <- rutas_res
  result_split$non_selected_ptr_index <- routes_to_add_only_tc
  result_split$non_selected_pvr_cvr_index <- routes_to_add_only_vc

  return(result_split)
  
}

check_insert_segment_in_route<-function(routes_to_add, rutas_res, index, input, option, type_problem, enable) {

  best_cost <- Inf
  index_to_add <- -1
  index_to_del <- -1
  for (i in 1:length(routes_to_add)) {
    route_to_insert <- rutas_res[[routes_to_add[[i]]]]
    
    #print("ROUTE TO ADD")
    #print(route_to_insert$route)
    
    for (j in 1:length(index)) {
      route_to_check <- rutas_res[[index[[j]]]]
      if (length(route_to_check$route) > 1 ) {


          if (route_to_check$type == "PTR") {
              res <- insert_PR_in_PR(route_to_insert$route[2:(length(route_to_insert$route)-1)], 
                                     route_to_check$route, input, input$capacidad.truck, type_problem, rutas_res)
              if (best_cost > res$cost) {
                best_type <- route_to_check$type
                best_cost <- res$cost
                best_route <- res$route
                if (type_problem == "TTRP") {
                    best_total_load <- calc_load2(res$route, input$vector.demandas)
                    best_total_load_tc_clients <- calc_load_only_truck(res$route, input$vector.demandas, input)
                }
                if (type_problem == "MCTTRP") {
                  best_total_load <- calc_load2_MC(res$route, input$matriz.demandas)
                  best_total_load_tc_clients <- calc_load_only_truck_MC(res$route, input$matriz.demandas, input)
                }
                index_to_add <- index[[j]]
                index_to_del <- i
              } 
          }
          if ((option == "tc")&&(route_to_check$type == "CVR")) {
            
            #print("tc_CVR**Trying insert:")
            #print(route_to_insert$route[2:(length(route_to_insert$route)-1)])
            #print("in:")
            #print(route_to_check$route)
            
                res <- insert_PTR_in_CVR(route_to_insert$route[2:(length(route_to_insert$route)-1)], 
                                         route_to_check$route, input, type_problem, rutas_res)
                if (best_cost > res$cost) {
                  best_type <- route_to_check$type
                  best_cost <- res$cost
                  best_route <- res$route
                  if (type_problem == "TTRP") {
                      best_total_load <- calc_load2(res$route, input$vector.demandas)
                      best_total_load_tc_clients <- calc_load_only_truck(res$route, input$vector.demandas, input)
                  }
                  if (type_problem == "MCTTRP") {
                    best_total_load <- calc_load2_MC(res$route, input$matriz.demandas)
                    best_total_load_tc_clients <- calc_load_only_truck_MC(res$route, input$matriz.demandas, input)
                  }
                  index_to_add <- index[[j]]
                  index_to_del <- i
                } 
          }
          if ((option == "vc")&&(route_to_check$type == "PVR")) {
            #print("vc_PVR**Trying insert:")
            #print(route_to_insert$route[2:(length(route_to_insert$route)-1)])
            #print("in:")
            #print(route_to_check$route)
            
              res <- insert_PR_in_PR(route_to_insert$route[2:(length(route_to_insert$route)-1)], 
                                     route_to_check$route, input, input$capacidad.vehiculo, type_problem, rutas_res)
              if (best_cost > res$cost) {
                best_type <- route_to_check$type
                best_cost <- res$cost
                best_route <- res$route
                if (type_problem == "TTRP") {
                    best_total_load <- calc_load2(res$route, input$vector.demandas)
                    best_total_load_tc_clients <- calc_load_only_truck(res$route, input$vector.demandas, input)
                }
                if (type_problem == "MCTTRP") {
                  best_total_load <- calc_load2_MC(res$route, input$matriz.demandas)
                  best_total_load_tc_clients <- calc_load_only_truck_MC(res$route, input$matriz.demandas, input)
                }
                index_to_add <- index[[j]]
                index_to_del <- i
              } 
          }
          if ((option == "vc")&&(route_to_check$type == "CVR")) {
            
            #print("tc_CVR**Trying insert:")
            #print(route_to_insert$route[2:(length(route_to_insert$route)-1)])
            #print("in:")
            #print(route_to_check$route)
            
            
              res <- insert_PVR_in_CVR(route_to_insert$route[2:(length(route_to_insert$route)-1)], 
                                       route_to_check$route, input, type_problem, rutas_res)
              if (best_cost > res$cost) {
                best_type <- route_to_check$type
                best_cost <- res$cost
                best_route <- res$route
                if (type_problem == "TTRP") {
                    best_total_load <- calc_load2(res$route, input$vector.demandas)
                    best_total_load_tc_clients <- calc_load_only_truck(res$route, input$vector.demandas, input)
                }
                if (type_problem == "MCTTRP") {
                  best_total_load <- calc_load2_MC(res$route, input$matriz.demandas)
                  best_total_load_tc_clients <- calc_load_only_truck_MC(res$route, input$matriz.demandas, input)
                }
                index_to_add <- index[[j]]
                index_to_del <- i
              } 
          }
          if ((option == "tc")&&(route_to_check$type == "PVR")&&(enable)) {
            
            #print("tc_PVR**Trying insert:")
            #print(route_to_insert$route[2:(length(route_to_insert$route)-1)])
            #print("in:")
            #print(route_to_check$route)
            
              res <- merge_PVR_PTR(route_to_check$route, route_to_insert$route[2:(length(route_to_insert$route)-1)], 
                                   input, type_problem, rutas_res)
              if (best_cost > res$cost) {
                best_type <- "CVR"
                best_cost <- res$cost
                best_route <- res$route
                if (type_problem == "TTRP") {
                    best_total_load <- calc_load2(res$route, input$vector.demandas)
                    best_total_load_tc_clients <- calc_load_only_truck(res$route, input$vector.demandas, input)
                }
                if (type_problem == "MCTTRP") {
                  best_total_load <- calc_load2_MC(res$route, input$matriz.demandas)
                  best_total_load_tc_clients <- calc_load_only_truck_MC(res$route, input$matriz.demandas, input)
                }
                index_to_add <- index[[j]]
                index_to_del <- i
              } 
          }          
      }
    }
    
    #print("END ITERATION")
  
  }
  
  

  if (best_cost != Inf) {
    solution_old <- rutas_res[[index_to_add]]
    
    rutas_res[[routes_to_add[[index_to_del]]]]$route <- c(0)
    rutas_res[[routes_to_add[[index_to_del]]]]$total_load <- 0
    rutas_res[[routes_to_add[[index_to_del]]]]$total_load_tc_clients <- 0
    rutas_res[[routes_to_add[[index_to_del]]]]$cost <- 0

    
    new_r <- list()
    new_r$type <- best_type
    new_r$route <- best_route
    if (new_r$type == "CVR") {
      new_r$main_tour <- return_main_route(new_r$route)
      new_r$subtours <- return_subroutes(new_r$route, input$n1)
    }
    new_r$total_load <- best_total_load
    new_r$total_load_tc_clients <- best_total_load_tc_clients
    new_r$cost <- best_cost
    if (type_problem == "MCTTRP") {
      if (new_r$type == "PTR") res_r <- insert_hoppers_MCTTRP_PTR       (new_r, solution_old, input)
      if (new_r$type == "PVR") res_r <- insert_hoppers_MCTTRP_PVR_update(new_r$route, solution_old, input)
      if (new_r$type == "CVR") res_r <- insert_hoppers_MCTTRP_CVR_update(new_r$route, solution_old, input)
      
      new_r$clients_tc <- res_r$clients_tc
      new_r$clients_vc <- res_r$clients_vc
      new_r$used_hoppers_truck <- res_r$used_hoppers_truck
      new_r$used_hoppers_trailer <- res_r$used_hoppers_trailer
    }
    
    rutas_res[[index_to_add]] <- new_r
    
    routes_to_add[[index_to_del]] <- -1 
    
  }

  result_ci <- list()
  result_ci$routes_to_add <- routes_to_add
  result_ci$rutas_res <- rutas_res
  
  return(result_ci)
  
}

switch_routes<-function(rutas_res, routes_selected, routes_to_add, input, type_problem) {

  
  for (j in 1:length(routes_to_add)) {
    flag_exit <- 0
    prev_value <- Inf
    
    res_list <- list()
    res_list$index_i_route <- -1
    res_list$index_j_route <- -1
    res_list$index_z_route <- -1
    res_list$load <- Inf
    while(!flag_exit) {
      
      res_list$value <- Inf
      
      node_to_check <- rutas_res[[routes_to_add[[j]]]]$route[2] 
      
      res_list <- switch_r(res_list, routes_selected, rutas_res, node_to_check, routes_to_add[[j]], input, type_problem)
      
      if (res_list$load<prev_value) {
        prev_value <- res_list$load
        node_selected1 <- res_list$node_selected
     
        old_solution <- rutas_res[[res_list$index_i_route]]
        rutas_res[[res_list$index_i_route]]$route <- res_list$route
        subtours <- return_subroutes2(res_list$route, input$n1)
        
        
        if (length(subtours)&&(rutas_res[[res_list$index_i_route]]$type == "PVR"))  rutas_res[[res_list$index_i_route]]$type <- "CVR"
        
        if (type_problem == "TTRP") {
          rutas_res[[res_list$index_i_route]]$total_load <- calc_load2(res_list$route, input$vector.demandas)
          rutas_res[[res_list$index_i_route]]$total_load_tc_clients <- calc_load_only_truck(res_list$route, input$vector.demandas, input)
        }
        
        if (type_problem == "MCTTRP") {
          rutas_res[[res_list$index_i_route]]$total_load <- calc_load2_MC(res_list$route, input$matriz.demandas)
          rutas_res[[res_list$index_i_route]]$total_load_tc_clients <- calc_load_only_truck_MC(res_list$route,  input$matriz.demandas, input)
        }
        
        if (rutas_res[[res_list$index_i_route]]$type == "CVR") {
          rutas_res[[res_list$index_i_route]]$main_tour <- return_main_route(rutas_res[[res_list$index_i_route]]$route )
          rutas_res[[res_list$index_i_route]]$subtours <- return_subroutes(rutas_res[[res_list$index_i_route]]$route , input$n1)
        }
        
        rutas_res[[res_list$index_i_route]]$cost <- local_cost(res_list$route, input$matriz.distancia)
        
        if (type_problem == "MCTTRP") {
          if (rutas_res[[res_list$index_i_route]]$type == "PTR") res_r <- insert_hoppers_MCTTRP_PTR       (rutas_res[[res_list$index_i_route]], old_solution, input)
          if (rutas_res[[res_list$index_i_route]]$type == "PVR") res_r <- insert_hoppers_MCTTRP_PVR_update(rutas_res[[res_list$index_i_route]]$route, old_solution, input)
          if (rutas_res[[res_list$index_i_route]]$type == "CVR") res_r <- insert_hoppers_MCTTRP_CVR_update(rutas_res[[res_list$index_i_route]]$route, old_solution, input)
          rutas_res[[res_list$index_i_route]]$clients_tc <- res_r$clients_tc
          rutas_res[[res_list$index_i_route]]$clients_vc <- res_r$clients_vc
          rutas_res[[res_list$index_i_route]]$used_hoppers_truck <- res_r$used_hoppers_truck
          rutas_res[[res_list$index_i_route]]$used_hoppers_trailer <- res_r$used_hoppers_trailer
        }
        
        #########
        
        rutas_res[[res_list$index_j_route]]$route[2] <- node_selected1
        n_route2 <- rutas_res[[res_list$index_j_route]]$route
        
        if (type_problem == "TTRP") {
          rutas_res[[res_list$index_j_route]]$total_load <- calc_load2(n_route2, input$vector.demandas)
          rutas_res[[res_list$index_j_route]]$total_load_tc_clients <- calc_load_only_truck(n_route2, input$vector.demandas, input)
        }
        
        if (type_problem == "MCTTRP") {
          rutas_res[[res_list$index_j_route]]$total_load <- calc_load2_MC(n_route2, input$matriz.demandas)
          rutas_res[[res_list$index_j_route]]$total_load_tc_clients <- calc_load_only_truck_MC(n_route2, input$matriz.demandas, input)          
        }
        
        rutas_res[[res_list$index_j_route]]$cost <- local_cost(n_route2, input$matriz.distancia) 
        if (type_problem == "MCTTRP") {
          res_r <- insert_hoppers_MCTTRP_PTR (rutas_res[[res_list$index_j_route]], rutas_res[[res_list$index_j_route]], input)
          rutas_res[[res_list$index_j_route]]$clients_tc <- res_r$clients_tc
          rutas_res[[res_list$index_j_route]]$clients_vc <- res_r$clients_vc
          rutas_res[[res_list$index_j_route]]$used_hoppers_truck <- res_r$used_hoppers_truck
          rutas_res[[res_list$index_j_route]]$used_hoppers_trailer <- res_r$used_hoppers_trailer
        }
        
      }
      else flag_exit <- 1
    }
  }
  
  
  return(rutas_res)
  
}

switch_r<-function(res_list, routes_selected, rutas_res, node_j, routes_to_add, input, typeproblem) {
  
  for (i in 1:length(routes_selected)){
    route_i <- rutas_res[[routes_selected[[i]]]]
    if (typeproblem == "TTRP")   cap_node_j <- calc_load2(node_j, input$vector.demandas)
    if (typeproblem == "MCTTRP") cap_node_j <- calc_load2_MC(node_j, input$matriz.demandas)

    # for PTR
    if (route_i$type == "PTR") {
      for (z in 2:(length(route_i$route)-1)) {
        res_list <- create_switch_route(res_list, route_i$route, route_i, node_j, input,  routes_selected[[i]], routes_to_add, z, input$capacidad.truck, 1, 0, typeproblem)
      }
    }
    # for VTR
    if (route_i$type == "PVR") {
      if ((node_j<=input$n1)) {
        for (z in 2:(length(route_i$route)-1)) {
          res_list <- create_switch_route(res_list, route_i$route, route_i, node_j, input,  routes_selected[[i]], routes_to_add, z, input$capacidad.vehiculo, 0, 0, typeproblem)
        }
      } else {
        if (length(route_i$route)>3) {
          for (z in 3:(length(route_i$route)-1)) {
            res_list <- create_switch_route(res_list, route_i$route, route_i, node_j, input,  routes_selected[[i]], routes_to_add, z, input$capacidad.vehiculo, 1, 1, typeproblem)
          }
        }
      }
    }
    # CVR
    if (route_i$type == "CVR") {
      subroutes <- return_subroutes2(route_i$route, input$n1)
      for (s in 1:length(subroutes)) {
        subroute_i <-  subroutes[[s]]
        for (ss in 2:(length(subroute_i$tour)-1)) {
          return_index <- which(subroute_i$tour[ss] == route_i$route)
          if ((node_j<=input$n1)) {
            res_list <- create_switch_route(res_list, route_i$route, route_i, node_j, input, routes_selected[[i]], routes_to_add, return_index, input$capacidad.truck, 1, 0, typeproblem)
          }
          else if ((node_j>input$n1)){
            res_list <- create_switch_route(res_list, route_i$route, route_i, node_j, input, routes_selected[[i]], routes_to_add, return_index, input$capacidad.truck, 1, 0, typeproblem)
          }
        }
      }
      main_route <- return_main_route(route_i$route)
      for (m in 2:(length(main_route)-1)) {
        if (sum(main_route[m]==route_i$route)==1) {
          return_index<- which(main_route[m] == route_i$route)
          if ((node_j<=input$n1)) {
            res_list <- create_switch_route(res_list, route_i$route, route_i, node_j, input, routes_selected[[i]], routes_to_add, return_index, input$capacidad.vehiculo, 0, 0, typeproblem)
          }
          else if ((node_j>input$n1)&&(ss > 2)){
            res_list <- create_switch_route(res_list, route_i$route, route_i, node_j, input, routes_selected[[i]], routes_to_add, return_index, input$capacidad.vehiculo, 1, 1, typeproblem)
          }
        }
      }
    }
  }
  
  return(res_list)
}
  
create_switch_route<-function(res_list, n_route, solution, selected_node, input, i, j, z, capacity, allow_different_type, insert_as_subroute, typeproblem){
  if (selected_node <= input$n1) type_selected_node <- "vc"
  else type_selected_node <- "tc"
  if (n_route[z] <= input$n1) type_node <- "vc"
  else type_node <- "tc"
  
  if (allow_different_type) {
    check <- 1
  }
  else {
    check <- 0
    if (type_selected_node == type_node) {
      check <- 1
    }
  }
  
  if (check) {
    
    if (!insert_as_subroute){
      prev_node <- n_route[z]
      logic_n_route <- (n_route == n_route[z])
      for (ii in 2:length(logic_n_route)) {
        if (logic_n_route[ii]) {
          n_route[ii] <- selected_node
        }
      }
    } else {
      prev_node <- n_route[z]
      old_route <- n_route
      n_route <- c(0)
      for (ii in 2:length(old_route)) {
        if (old_route[ii]==prev_node) {
          n_route <- c(n_route, selected_node, old_route[ii-1])
        } else {
          n_route <- c(n_route, old_route[ii])
        }
      }
    }
    
    if (typeproblem == "TTRP")   {
      new_capacity <-  calc_load2(n_route, input$vector.demandas)
      cap <- capacity
    }
    
    if (typeproblem == "MCTTRP")  {
      new_capacity <-  calc_load2_MC(n_route, input$matriz.demandas)
      cap <- capacity[1]
    }
    
    if (new_capacity <= cap) {
      # check subroutes
      subroute_index <- 0
      subtours <- return_subroutes2(n_route, input$n1)
      if (length(subtours)) subroute_index <- return_subroute_index(n_route, z, subtours)
      
      feasible <- 1
      if ((subroute_index!=0)&&(!insert_as_subroute)) {
        feasible <- 0
        if (typeproblem == "TTRP") { 
          new_truck_cap <- calc_load2(subtours[[subroute_index]]$tour[2:(length(subtours[[subroute_index]]$tour)-1)], input$vector.demandas)
          if (new_truck_cap <= input$capacidad.truck) {
            feasible <- 1
          }
        }
        if (typeproblem == "MCTTRP") {
          new_truck_cap <- calc_load2_MC(subtours[[subroute_index]]$tour[2:(length(subtours[[subroute_index]]$tour)-1)], input$matriz.demandas)
          if (new_truck_cap <= input$capacidad.truck[1]) {
            feasible <- 1
          } 
        }

      }
      
##### HOPPERS
      if ((typeproblem == "MCTTRP")&&(feasible)) {
          if (solution$type == "PTR") feasible <- check_capacity_hoppers_MCTTRP_PR(n_route, solution, input, cap)
          if (solution$type == "PVR") feasible <- check_capacity_hoppers_MCTTRP_PVR_update(n_route, solution, input)
          if (solution$type == "CVR") feasible <- check_capacity_hoppers_MCTTRP_CVR_update(n_route, solution, input)
      }
      
      if (feasible){
        if (typeproblem == "TTRP") {
            old_node_capacity <-  calc_load2(prev_node, input$vector.demandas)
            node_capacity <- calc_load2(selected_node, input$vector.demandas)
        }
        if (typeproblem == "MCTTRP")  {
          old_node_capacity <-  calc_load2_MC(prev_node, input$matriz.demandas)
          node_capacity <- calc_load2_MC(selected_node, input$matriz.demandas)
        }
        
        total_cost <- local_cost(n_route, input$matriz.distancia)
        
        if ((old_node_capacity < node_capacity ) && (total_cost <= res_list$value)) {
          res_list$load <-  node_capacity
          res_list$value <- total_cost
          res_list$index_i_route <- i
          res_list$index_j_route <- j
          res_list$index_z_route <- z
          res_list$insert_as_subroute <- insert_as_subroute
          res_list$route <- n_route
          res_list$node_selected <- prev_node
        }
      }
    }
  }
  
  return(res_list)
}

update_route_to_add<-function(routes_to_add) {
    new_routes_to_add <- list()
    counter_n <- 1
    update <- 0
    for (i in 1:length(routes_to_add)) {
      if (routes_to_add[[i]] != -1) {
        new_routes_to_add[counter_n] <- routes_to_add[[i]]
        counter_n <- counter_n + 1
      }else {
        update <- 1
      }
    }

    result_u <- list()
    result_u$routes_to_add <- new_routes_to_add
    result_u$updated <- update
    
    return(result_u)
}

split_all_vc_tc<-function(rutas_res, non_selected_ptr_index, input, type_problem) {
  routes_to_add_only_tc <- list()
  routes_to_add_only_vc <- list()
  
  counter_tc <- 1
  counter_vc <- 1
  for (i in 1:length(non_selected_ptr_index)) {

    if (( length(rutas_res[[non_selected_ptr_index[[i]]]]$route)>3) && (rutas_res[[non_selected_ptr_index[[i]]]]$type != "PVR")) {
      segment_routes <- split_vc_tc(rutas_res[[non_selected_ptr_index[[i]]]]$route, input)
      
      if (length(segment_routes$route_tc) > 2 ) {
        pos1 <- non_selected_ptr_index[[i]]
        
        rutas_res[[pos1]]$type <-"PTR"
        rutas_res[[pos1]]$route <- segment_routes$route_tc

        if (type_problem == "TTRP") {
          rutas_res[[pos1]]$total_load <- calc_load2(segment_routes$route_tc, input$vector.demandas)
          rutas_res[[pos1]]$total_load_tc_clients <- calc_load_only_truck(segment_routes$route_tc, input$vector.demandas, input)
        }
        if (type_problem == "MCTTRP") {
          rutas_res[[pos1]]$total_load <- calc_load2_MC(segment_routes$route_tc, input$matriz.demandas)
          rutas_res[[pos1]]$total_load_tc_clients <- calc_load_only_truck_MC(segment_routes$route_tc, input$matriz.demandas, input)
        }
        rutas_res[[pos1]]$cost <- local_cost(segment_routes$route_tc, input$matriz.distancia)

        if (type_problem == "MCTTRP") {
          res_r <- insert_hoppers_MCTTRP_PTR(rutas_res[[pos1]], rutas_res, input)
          rutas_res[[pos1]]$clients_tc <- res_r$clients_tc
          rutas_res[[pos1]]$clients_vc <- res_r$clients_vc
          rutas_res[[pos1]]$used_hoppers_truck <- res_r$used_hoppers_truck
          rutas_res[[pos1]]$used_hoppers_trailer <- res_r$used_hoppers_trailer
        }
        
        routes_to_add_only_tc[[counter_tc]] <- non_selected_ptr_index[[i]]
        counter_tc <- counter_tc + 1
      }
      
      if ((length(segment_routes$route_vc) > 2 )&&(length(segment_routes$route_tc) > 2 )) {

        pos2 <- length(rutas_res) + 1
        routes_to_add_only_vc[[counter_vc]] <- pos2
        counter_vc <- counter_vc + 1
        
        new_vc <- list()
        new_vc$type <-"PVR"
        new_vc$route <- segment_routes$route_vc
        if (type_problem == "TTRP") {
          new_vc$total_load <- calc_load2(segment_routes$route_vc, input$vector.demandas)
          new_vc$total_load_tc_clients <- calc_load_only_truck(segment_routes$route_vc, input$vector.demandas, input)
        }
        if (type_problem == "MCTTRP") {
          new_vc$total_load <- calc_load2_MC(segment_routes$route_vc, input$matriz.demandas)
          new_vc$total_load_tc_clients <- calc_load_only_truck_MC(segment_routes$route_vc, input$matriz.demandas, input)
        }
        new_vc$cost <- local_cost(segment_routes$route_vc, input$matriz.distancia)
        if (type_problem == "MCTTRP") {
          res_r <- insert_hoppers_MCTTRP_PVR_new(new_vc, rutas_res, input)
          new_vc$clients_tc <- res_r$clients_tc
          new_vc$clients_vc <- res_r$clients_vc
          new_vc$used_hoppers_truck <- res_r$used_hoppers_truck
          new_vc$used_hoppers_trailer <- res_r$used_hoppers_trailer
        }
        
        rutas_res[[pos2]] <- new_vc
        
      }
    }
    else  if ( length(rutas_res[[non_selected_ptr_index[[i]]]]$route)==3) {
      if (rutas_res[[non_selected_ptr_index[[i]]]]$route[2] > input$n1) {
        routes_to_add_only_tc[[counter_tc]] <- non_selected_ptr_index[[i]]
        counter_tc <- counter_tc + 1
      } else {
        routes_to_add_only_vc[[counter_vc]] <- non_selected_ptr_index[[i]]
        counter_vc <- counter_vc + 1
      }
    }
    else  if (rutas_res[[non_selected_ptr_index[[i]]]]$type == "PVR") {
        routes_to_add_only_vc[[counter_vc]] <- non_selected_ptr_index[[i]]
        counter_vc <- counter_vc + 1
    }
    
  }
  
  res_list <- list()
  res_list$routes_to_add_only_tc <- routes_to_add_only_tc
  res_list$routes_to_add_only_vc <- routes_to_add_only_vc
  res_list$rutas_res <- rutas_res
  
  return(res_list)
}

split_all_middle<-function(rutas_res, group_index, input, type_problem) {
  
  new_index <- list()
  counter <- 1
  non_split_routes <- 0
  for (i in 1:length(group_index)) {
    select_route <- rutas_res[[group_index[[i]]]]$route
    
    if (length(select_route)>3) {
      pos_center <- ceiling(length(select_route)/2)
      
      route1 <- c(select_route[1:pos_center], 0)
      route2 <- c(0, select_route[(pos_center+1):length(select_route)])
      
      pos1 <- group_index[[i]]
      pos2 <- length(rutas_res) + 1
      
      rutas_res[[pos1]]$route <- route1
      if (rutas_res[[pos1]]$type == "CVR") {
        rutas_res[[pos1]]$main_tour <- return_main_route(rutas_res[[pos1]]$route)
        rutas_res[[pos1]]$subtours <- return_subroutes(rutas_res[[pos1]]$route, input$n1)
      }
      if (type_problem == "TTRP") {
          rutas_res[[pos1]]$total_load <- calc_load2(route1, input$vector.demandas)
          rutas_res[[pos1]]$total_load_tc_clients <- calc_load_only_truck(route1, input$vector.demandas, input)
      }
      if (type_problem == "MCTTRP") {
          rutas_res[[pos1]]$total_load <- calc_load2_MC(route1, input$matriz.demandas)
          rutas_res[[pos1]]$total_load_tc_clients <- calc_load_only_truck_MC(route1, input$matriz.demandas, input)        
      }
      rutas_res[[pos1]]$cost <- local_cost(route1, input$matriz.distancia)
      if (type_problem == "MCTTRP") {
        res_r <- insert_hoppers_MCTTRP_PTR(rutas_res[[pos1]], rutas_res, input)
        rutas_res[[pos1]]$clients_tc <- res_r$clients_tc
        rutas_res[[pos1]]$clients_vc <- res_r$clients_vc
        rutas_res[[pos1]]$used_hoppers_truck <- res_r$used_hoppers_truck
        rutas_res[[pos1]]$used_hoppers_trailer <- res_r$used_hoppers_trailer
      }
      
      new_vc <- list()
      new_vc$type <-rutas_res[[pos1]]$type 
      new_vc$route <- route2
      if (type_problem == "TTRP") {
          new_vc$total_load <- calc_load2(route2, input$vector.demandas)
          new_vc$total_load_tc_clients <- calc_load_only_truck(route2, input$vector.demandas, input)
      }
      if (type_problem == "MCTTRP") {
          new_vc$total_load <- calc_load2_MC(route2, input$matriz.demandas)
          new_vc$total_load_tc_clients <- calc_load_only_truck_MC(route2, input$matriz.demandas, input) 
      }
      new_vc$cost <- local_cost(route2, input$matriz.distancia)
      if (type_problem == "MCTTRP") {
        res_r <- insert_hoppers_MCTTRP_PTR(new_vc, rutas_res, input)
        new_vc$clients_tc <- res_r$clients_tc
        new_vc$clients_vc <- res_r$clients_vc
        new_vc$used_hoppers_truck <- res_r$used_hoppers_truck
        new_vc$used_hoppers_trailer <- res_r$used_hoppers_trailer
      }
      rutas_res[[pos2]] <- new_vc
      
      new_index[[counter]] <- pos2
      counter <- counter + 1
    } else {
      non_split_routes <- non_split_routes + 1
    }
  }
  
  group_index <- append(group_index, new_index)
  
  res <- list()
  res$rutas_res <- rutas_res
  res$group_index <- group_index
  res$non_split_routes <- non_split_routes
  return(res)
}

split_vc_tc<-function(route, input) {
  
  route_tc <- c(0)
  route_vc <- c(0)
  for (i in 2:(length(route)-1)) {
    if (route[i] > input$n1) {
      if (sum(route_tc==route[i])==0) {
        route_tc <- c(route_tc, route[i])
      }
    } else {
      if (sum(route_vc==route[i])==0) {
        route_vc <- c(route_vc, route[i])
      }
    }
  }
  route_tc <- c(route_tc, 0)
  route_vc <- c(route_vc, 0)
  
  res <- list()
  res$route_vc <- route_vc
  res$route_tc <- route_tc
  return(res)
}



split_vc_tc_subroute<-function(route, input) {
  
  route_tc <- c(0)
  route_vc <- c(0)
  
  main_tour <- return_main_route(route)

  for (i in 2:(length(route)-1)) {
    is_parking_1 <- 0
    if (sum(main_tour[2:(length(main_tour)-1)]==route[i])) is_parking_1 <- 1
    
    if ((route[i] > input$n1)||(is_parking_1)) {
      #if (sum(route_tc==route[i])==0) {
        route_tc <- c(route_tc, route[i])
      #}
    } else {
      if (sum(route_vc==route[i])==0) {
        route_vc <- c(route_vc, route[i])
      }
    }
  }
  route_tc <- c(route_tc, 0)
  route_vc <- c(route_vc, 0)
  
  res <- list()
  res$route_vc <- route_vc
  res$route_tc <- route_tc
  return(res)
}

select_PTRs<-function(rutas_res, n_ptr, input) {
  
  list_index <- list()
  decrease_list <- NULL
  
  counter <- 1
  for (i in 1:length(rutas_res)) {
    if (rutas_res[[i]]$type == "PTR") {
      decrease_value <- rutas_res[[i]]$total_load - rutas_res[[i]]$total_load_tc_clients
      #if (rutas_res[[i]]$total_load < (input$capacidad.truck*3)/4) decrease_value <- Inf
      #else decrease_value <- rutas_res[[i]]$cost 
      
      if (counter == 1) {
        decrease_list <- c(decrease_value)
        decrease_index <- c(i)
      }
      else {
        decrease_list <- c(decrease_list, decrease_value)
        decrease_index <- c(decrease_index, i)
      }
      counter <- counter + 1
    }
  }
  
  counter <- 1
  if (!is.null(decrease_list)) {
    order_list <- order(decrease_list, decreasing = FALSE)
    counter <- 1
    for (i in 1:length(order_list)) {
      if (counter > n_ptr) break
      position <- decrease_index[order_list[i]]
      list_index[[counter]] <- position
      counter <- counter + 1
    }
  }
  
  return(list_index)
}

select_PVRs_CVRs<-function(rutas_res, n_pvr_cvr, input) {
  
  list_index <- list()
  decrease_list <- NULL
  counter <- 1
  
  
  for (i in 1:length(rutas_res)) {
    
    if ((rutas_res[[i]]$type == "PVR")||(rutas_res[[i]]$type == "CVR")) {
      
      decrease_value <- rutas_res[[i]]$total_load 
      #if (rutas_res[[i]]$total_load < (input$capacidad.vehiculo*3)/4) decrease_value <- Inf
      #else decrease_value <- rutas_res[[i]]$cost 
      
      if (counter == 1) {
        decrease_list <- c(decrease_value)
        decrease_index <- c(i)
      }
      else {
        decrease_list <- c(decrease_list, decrease_value)
        decrease_index <- c(decrease_index, i)
      }
      counter <- counter + 1
    }
  }
  
  counter <- 1
  if (!is.null(decrease_list)) {
    order_list <- order(decrease_list, decreasing = FALSE)
    counter <- 1
    for (i in 1:n_pvr_cvr) {
      if (length(decrease_index)<i) break
      position <- decrease_index[order_list[i]]
      list_index[[counter]] <- position
      counter <- counter + 1
    }
  }
  
  return(list_index)
}

return_VTR_CVR_routes<-function(rutas_res) {
  
  counter <- 0
  
  for (elem in rutas_res) {
    if ((elem$type == "PVR")||(elem$type == "CVR")) {
      counter <- counter + 1
    }
  }
  
  return(counter)
}

return_PTR_routes<-function(rutas_res) {
  
  counter <- 0
  
  for (elem in rutas_res) {
    if ((elem$type == "PTR")) {
      counter <- counter + 1
    }
  }
  
  return(counter)
}

return_non_selected_ptr<-function(rutas_res, index_selected) {
  
  list_index <- list()
  if (length(index_selected)) {
    counter <- 1
    for (i in 1:length(rutas_res)) {
      if (rutas_res[[i]]$type == "PTR") {
        is_selected <- sum(index_selected == i)
        if (!is_selected) {
          list_index[counter] <- i
          counter <- counter + 1
        }
      }
    }
  }
  
  return(list_index)
}

return_non_selected_pvr_cvr <-function(rutas_res, index_selected) {
  list_index <- list()
  
  counter <- 1
  for (i in 1:length(rutas_res)) {
    if ((rutas_res[[i]]$type == "PVR")||(rutas_res[[i]]$type == "CVR")) {
      is_selected <- sum(index_selected == i)
      if (!is_selected) {
        list_index[counter] <- i
        counter <- counter + 1
      }
    }
  }
  
  return(list_index)
}

return_ptr_index<-function(rutas_res) {
  
  list_index <- list()
  
  counter <- 1
  for (i in 1:length(rutas_res)) {
    if (rutas_res[[i]]$type == "PTR") {
      list_index[counter] <- i
      counter <- counter + 1
    }
  }
  
  return(list_index)
}

return_pvr_index<-function(rutas_res) {
  
  list_index <- list()
  
  counter <- 1
  for (i in 1:length(rutas_res)) {
    if (rutas_res[[i]]$type == "PVR") {
      list_index[counter] <- i
      counter <- counter + 1
    }
  }
  
  return(list_index)
}

return_cvr_index<-function(rutas_res) {
  
  list_index <- list()
  
  counter <- 1
  for (i in 1:length(rutas_res)) {
    if (rutas_res[[i]]$type == "CVR") {
      list_index[counter] <- i
      counter <- counter + 1
    }
  }
  
  return(list_index)
}

return_cvr_index2<-function(rutas_res, selected_pvr_cvr_index) {
  
  list_index <- list()
  
  counter <- 1
  for (i in 1:length(selected_pvr_cvr_index)) {
    position <- selected_pvr_cvr_index[[i]]
    if (rutas_res[[position]]$type == "CVR") {
      list_index[counter] <- position
      counter <- counter + 1
    }
  }
  
  return(list_index)
}

return_ptr_index<-function(rutas_res) {
  
  list_index <- list()
  
  counter <- 1
  for (i in 1:length(rutas_res)) {
    if (rutas_res[[i]]$type == "PTR") {
      list_index[counter] <- i
      counter <- counter + 1
    }
  }
  
  return(list_index)
}

return_intex_vc_clients<-function(route, n1) {
  vc_list <- list()
  counter <- 1 
  
  for (i in 1:length(route)) {
    if ((route[i] <= n1) && (i!=1) && (i != length(route))) {
      vc_list[counter] <- as.numeric(i)
      counter <- counter + 1
    }
  }
  
  return(vc_list)
}

convert_PTR_CVR<-function(rutas_res, non_selected_ptr_index, n_convert_ptr_cvr, input, type_problem) {
  
  index_convert_ptr_cvr <- list() 
  counter <- 1
  
  if ((length(non_selected_ptr_index) > 0)&&(n_convert_ptr_cvr > 0)) {
    for (i in 1:length(non_selected_ptr_index)) {
      position <- as.numeric(non_selected_ptr_index[i])
      vc_list <- return_intex_vc_clients(rutas_res[[position]]$route, input$n1)
      result_list <- return_best_subroute_in_position(rutas_res[[position]]$route, vc_list, position,input, type_problem, rutas_res)
      
      #print(result_list$best_cost)
      
      if (result_list$best_cost != Inf) {
        decrease_value <- result_list$best_cost - local_cost(rutas_res[[position]]$route, input$matriz.distancia)
        if (counter == 1) decrease_list <- c(decrease_value)
        else decrease_list <- c(decrease_list, decrease_value)
        index_convert_ptr_cvr[[counter]] <- result_list
        counter <- counter + 1
      }
      
    }
  }
  
  if ((n_convert_ptr_cvr > 0)&&(length(index_convert_ptr_cvr))) {
    decrease_list_order <- order(decrease_list)
    
    for (i in 1:n_convert_ptr_cvr) {
          if (length(decrease_list_order)<i) break
          pos1 <- decrease_list_order[[i]] 
          pos2 <- index_convert_ptr_cvr[[pos1]]$position
          rutas_res[[pos2]]$type <- "CVR"
          old_route <- rutas_res[[pos2]]$route
          rutas_res[[pos2]]$route <- index_convert_ptr_cvr[[pos1]]$route
          rutas_res[[pos2]]$main_tour <- return_main_route(rutas_res[[pos2]]$route)
          rutas_res[[pos2]]$subtours <- return_subroutes(rutas_res[[pos2]]$route, input$n1)
          if (type_problem == "TTRP") {
            rutas_res[[pos2]]$total_load <- calc_load2(rutas_res[[pos2]]$route, input$vector.demandas)
            rutas_res[[pos2]]$total_load_tc_clients <- calc_load_only_truck(rutas_res[[pos2]]$route, input$vector.demandas, input)
          } 
          if (type_problem == "MCTTRP") {
            rutas_res[[pos2]]$total_load <- calc_load2_MC(rutas_res[[pos2]]$route, input$matriz.demandas)
            rutas_res[[pos2]]$total_load_tc_clients <- calc_load_only_truck_MC(rutas_res[[pos2]]$route, input$matriz.demandas, input)
          }
          rutas_res[[pos2]]$cost <- index_convert_ptr_cvr[[pos1]]$best_cost
          if (type_problem == "MCTTRP") {
            res_r <- insert_hoppers_MCTTRP_CVR_new(rutas_res[[pos2]], old_route, input)
            rutas_res[[pos2]]$clients_tc <- res_r$clients_tc
            rutas_res[[pos2]]$clients_vc <- res_r$clients_vc
            rutas_res[[pos2]]$used_hoppers_truck <- res_r$used_hoppers_truck
            rutas_res[[pos2]]$used_hoppers_trailer <- res_r$used_hoppers_trailer
          }
    }
    
  }
  
  return(rutas_res)
}

delete_vc_subroute<-function(route, input) {
  
  counter <- 2
  new_route<- c(route[1])
  pos_insert <- list()
  pos_insert[[1]] <- 1
  
  pending_vc_flag <- 0
  state <- 0
  for (i in 2:(length(route)-1)) {
    if (sum(route==route[i])>=2) {
      if (state==1) state <- 0
      else state <- 1
    }
    if (state == 1) {
      if ((route[i]>input$n1)||(sum(route==route[i])>=2)) {
        new_route <- c(new_route, route[i])
        pos_insert[[counter]] <- 0
        counter <- counter + 1
      } else {
        if (pending_vc_flag == 0) pending_vc <- c(route[i])
        else pending_vc <- c(pending_vc, route[i])
        pending_vc_flag <- 1
      }
    } else {
      new_route <- c(new_route, route[i])
      pos_insert[[counter]] <- 1
      counter <- counter + 1
    }
  } 
  pos_insert[[counter]] <- 0
  new_route <- c(new_route, 0)
  
  if (pending_vc_flag) {
    min_cost <- Inf
    end_route <- c(0)
    for (i in 1:length(pos_insert)) {
      if (pos_insert[[i]]) {
        new_route_2 <- c(new_route[1:i])
        new_route_2 <- c(new_route_2, pending_vc)
        new_route_2 <- c(new_route_2, new_route[(i+1):length(new_route)])
        cost <- local_cost(new_route_2, input$matriz.distancia)
        if (min_cost > cost) {
          end_route <- new_route_2
          min_cost <- cost
        }
      }
    }
  } else end_route <- route
  
  return(end_route)
}

merge_route_CVR<-function(rutas_res, ptr_pvr_index, pcr_index, n_v_to_delete, input, option, type_problem){
  
  continue_merg <- 1
  counter_merg <- 0
  while (continue_merg == 1) {
    result_list <- list()
    counter <- 1
    for (i in 1:length(ptr_pvr_index)) {
      i_pos <- ptr_pvr_index[[i]]
      if (length(rutas_res[[i_pos]]$route)>1) {
        i_route <- c(rutas_res[[i_pos]]$route[2:(length(rutas_res[[i_pos]]$route)-1)])
        MAX_value <- Inf
        for (j in 1:length(pcr_index)) {
          j_pos <- pcr_index[[j]]

          if (option == "PVR") res <- insert_PVR_in_CVR(i_route, rutas_res[[j_pos]]$route, input, type_problem, rutas_res)
          if (option == "PTR") res <- insert_PTR_in_CVR(i_route, rutas_res[[j_pos]]$route, input, type_problem, rutas_res)
            
          if (res$cost < MAX_value) {
              MAX_value <- res$cost
              if (option == "PVR") res$pvr_index <- i_pos
              if (option == "PTR") res$ptr_index <- i_pos
              res$cvr_index <- j_pos
              candidate <- res
          }
        }
        
        if (MAX_value != Inf) {
          result_list[[counter]] <- candidate
          
          original_cost <- local_cost(rutas_res[[i_pos]]$route, input$matriz.distancia) + 
                           local_cost(rutas_res[[j_pos]]$route, input$matriz.distancia)
          
          decrease_value <- result_list[[counter]]$cost - original_cost
          
          if (counter == 1) decrease_list <- c(decrease_value)
          else decrease_list <- c(decrease_list, decrease_value)
          counter <- counter + 1
        }
      }
    }
    
    if (counter > 1) {
      
      decrease_list_order <- order(decrease_list)
      pos_select <- decrease_list_order[[1]] 
      if (option == "PVR") pos1 <- result_list[[pos_select]]$pvr_index
      if (option == "PTR") pos1 <- result_list[[pos_select]]$ptr_index
        
      rutas_res[[pos1]]$route <- c(0)
      rutas_res[[pos1]]$total_load <- 0
      rutas_res[[pos1]]$total_load_tc_clients <- 0
      rutas_res[[pos1]]$cost <- 0
        
      pos2 <- result_list[[pos_select]]$cvr_index
        
      old_route <- rutas_res[[pos2]]
      rutas_res[[pos2]]$route <- result_list[[pos_select]]$route
      if (rutas_res[[pos2]]$type == "CVR") {
        rutas_res[[pos2]]$main_tour <- return_main_route(rutas_res[[pos2]]$route)
        rutas_res[[pos2]]$subtours <- return_subroutes(rutas_res[[pos2]]$route, input$n1)
      }
      if (type_problem == "TTRP") {
        rutas_res[[pos2]]$total_load <- calc_load2(rutas_res[[pos2]]$route, input$vector.demandas)
        rutas_res[[pos2]]$total_load_tc_clients <- calc_load_only_truck(rutas_res[[pos2]]$route, input$vector.demandas, input)
      }
      if (type_problem == "MCTTRP") {
        rutas_res[[pos2]]$total_load <- calc_load2_MC(rutas_res[[pos2]]$route, input$matriz.demandas)
        rutas_res[[pos2]]$total_load_tc_clients <- calc_load_only_truck_MC(rutas_res[[pos2]]$route, input$matriz.demandas, input)
      }
      rutas_res[[pos2]]$cost <- result_list[[pos_select]]$cost

      if (type_problem == "MCTTRP") {
        
        res_r <- insert_hoppers_MCTTRP_CVR_update(rutas_res[[pos2]]$route, old_route, input)
        rutas_res[[pos2]]$clients_tc <- res_r$clients_tc
        rutas_res[[pos2]]$clients_vc <- res_r$clients_vc
        rutas_res[[pos2]]$used_hoppers_truck <- res_r$used_hoppers_truck
        rutas_res[[pos2]]$used_hoppers_trailer <- res_r$used_hoppers_trailer
      }
      
      counter_merg <- counter_merg + 1
      
      if (counter_merg == n_v_to_delete) continue_merg <- 0
    } 
    else continue_merg <- 0
  }

  rutas_res <- clean_rutas_res(rutas_res)
  
  return(rutas_res)
}

clean_rutas_res<-function(rutas_res) {
  new_rutas_res <- list()
  counter <- 1
  for (i in 1:length(rutas_res)) {
    if (length(rutas_res[[i]]$route)>1) {
      new_rutas_res[[counter]] <- rutas_res[[i]]
      counter <- counter + 1
    }
  }
  
  return(new_rutas_res)
}

insert_PVR_in_CVR<-function(pvr_route, cvr_route, input, type_problem, routes_res) {
  result_list <- list()
  result_list$cost <- Inf
  result_list$route <- Inf
  
  subtours <- return_subroutes2(cvr_route, input$n1)
  
  for (i in 1:(length(cvr_route)-1)) {
    subroute_index <- return_subroute_index(cvr_route, i, subtours)
    main_route <- 0
    if (subroute_index == 0)  main_route <- 1
    # vc client --> add main tour
    if (main_route) {
      if (is_last_parking(cvr_route, i)) {
        check <- check_capacity_total_routes(pvr_route, cvr_route, input, input$capacidad.vehiculo, 0, type_problem)
        # check it it is parking
        if ((check)&&(!is_parking_i(cvr_route, i))) check <- 0
        # check if it is in subroute
        subroute_index <- return_subroute_index(cvr_route, i, subtours)
        if (subroute_index != 0)  check <- 0
        # add to main root
        if (check) {
          nroute <- c(cvr_route[1:i])
          nroute <- c(nroute, pvr_route)
          nroute <- c(nroute, cvr_route[(i+1):length(cvr_route)])
          
          cost <- Inf
          id <- return_id_route(cvr_route, routes_res)
            
          if ((type_problem == "MCTTRP")
              &&((check_capacity_hoppers_MCTTRP_CVR_update(nroute, routes_res[[id]], input)))) cost <- local_cost(nroute, input$matriz.distancia) 
          if (type_problem == "TTRP")  cost <- local_cost(nroute, input$matriz.distancia) 
          
          if (cost < result_list$cost ) {
            result_list$cost <- cost
            result_list$route <- nroute
          } 
        }
      }
    } 
    # tc client -> add to subroute
    else {
      check <- check_capacity_subroute_routes(pvr_route, cvr_route, subtours, subroute_index, input, type_problem, penalty_max=0) 
      if (check){
        nroute <- c(cvr_route[1:i])
        nroute <- c(nroute, pvr_route)
        nroute <- c(nroute, cvr_route[(i+1):length(cvr_route)])
        
        cost <- Inf
        id <- return_id_route(cvr_route, routes_res)
        
        if ((type_problem == "MCTTRP")
            &&((check_capacity_hoppers_MCTTRP_CVR_update(nroute, routes_res[[id]], input)))) cost <- local_cost(nroute, input$matriz.distancia) 
        if (type_problem == "TTRP") cost <- local_cost(nroute, input$matriz.distancia) 
        
        if (cost < result_list$cost ) {
          result_list$cost <- cost
          result_list$route <- nroute
        }
      }
    }
    
  }
  
  
  return(result_list)
}

merge_PVR_PTR<-function(pvr_route, ptr_route, input, type_problem, routes_res) {
  result_list <- list()
  result_list$cost <- Inf
  result_list$route <- Inf
  
  for (i in 2:(length(pvr_route)-1)) {
    check <- check_capacity_merge_subroute_routes(pvr_route, ptr_route, input, type_problem)
    if (check) {
      nroute <- c(pvr_route[1:i])
      nroute <- c(nroute, ptr_route)
      nroute <- c(nroute, pvr_route[i:length(pvr_route)])
      
      cost <- Inf
      id <- return_id_route(pvr_route, routes_res)
      
      if (type_problem == "TTRP") cost <- local_cost(nroute, input$matriz.distancia) 
      if ((type_problem == "MCTTRP") && 
         ((check_capacity_hoppers_MCTTRP_CVR_update(nroute, routes_res[[id]], input)))) cost <- local_cost(nroute, input$matriz.distancia) 
      
      if (cost < result_list$cost ) {
        result_list$cost <- cost
        result_list$route <- nroute
      } 
    }
  }
  
  
  return(result_list)
}

insert_PR_in_PR<-function(route_to_insert, route_to_check, input, capacity, type_problem, routes_res) {
  
  result_list <- list()
  result_list$cost <- Inf
  result_list$route <- Inf
  
  state <- 0
  
  for (i in 1:(length(route_to_check)-1)) {
    
    cap_condition <- check_capacity_total_routes(route_to_insert, route_to_check, input, capacity, 0, type_problem)
    if (cap_condition) {
        nroute <- c(route_to_check[1:i])
        nroute <- c(nroute, route_to_insert)
        nroute <- c(nroute, route_to_check[(i+1):length(route_to_check)])
        
        cost <- Inf
        id <- return_id_route(route_to_check, routes_res)
        
        if ((type_problem == "MCTTRP")
            &&((check_capacity_hoppers_MCTTRP_PR(nroute, routes_res[[id]], input, capacity)))) cost <- local_cost(nroute, input$matriz.distancia) 
        if (type_problem == "TTRP") cost <- local_cost(nroute, input$matriz.distancia) 
        
        if (cost < result_list$cost ) {
          result_list$cost <- cost
          result_list$route <- nroute
        } 
    }
  }
  
  return(result_list)
}

insert_PTR_in_CVR<-function(ptr_route, cvr_route, input, type_problem, routes_res) {
  result_list <- list()
  result_list$cost <- Inf
  result_list$route <- Inf

  subtours <- return_subroutes2(cvr_route, input$n1)

  for (i in 1:(length(cvr_route)-1)) {
    create_subroute <- 0
    subroute_index <- return_subroute_index(cvr_route, i, subtours)
    if (subroute_index == 0)  create_subroute <- 1
    
    # vc client not in subroute--> create subroute
    if (create_subroute) {
      check <- check_capacity_total_routes(ptr_route, cvr_route, input, input$capacidad.vehiculo, 0, type_problem)
      if (check) {
        if (is_last_parking(cvr_route, i)) {
          
          nroute <- c(cvr_route[1:i])
          nroute <- c(nroute, ptr_route)
          nroute <- c(nroute, cvr_route[i:length(cvr_route)])
          
          cost <- Inf
          id <- return_id_route(cvr_route, routes_res)
          if ((type_problem == "MCTTRP")&&((check_capacity_hoppers_MCTTRP_CVR_update(nroute, routes_res[[id]], input)))) cost <- local_cost(nroute, input$matriz.distancia) 
          if (type_problem == "TTRP") cost <- local_cost(nroute, input$matriz.distancia) 
          
          if (cost < result_list$cost ) {
            result_list$cost <- cost
            result_list$route <- nroute
          } 
          
        }
      }
    } 
    # tc client -> add to subroute
    else {
      check <- check_capacity_subroute_routes(ptr_route, cvr_route, subtours, subroute_index, input, type_problem, penalty_max=0) 
      if (check){
        
        nroute <- c(cvr_route[1:i])
        nroute <- c(nroute, ptr_route)
        nroute <- c(nroute, cvr_route[(i+1):length(cvr_route)])
        
        cost <- Inf
        id <- return_id_route(cvr_route, routes_res)
        if ((type_problem == "MCTTRP")&&((check_capacity_hoppers_MCTTRP_CVR_update(nroute, routes_res[[id]], input)))) cost <- local_cost(nroute, input$matriz.distancia) 
        if (type_problem == "TTRP") cost <- local_cost(nroute, input$matriz.distancia) 
        
        if (cost < result_list$cost ) {
          result_list$cost <- cost
          result_list$route <- nroute
        } 
        
      }
    }
    
  }
  
  return(result_list)
}

locate_parkings<-function(route) {
  index_park <- list()
  counter_i <- 1
  for (i in 2:(length(route)-1)) {
    if (sum(route==route[i])>=2) {
      index_park[counter_i] <- i
      counter_i <- counter_i + 1
    }
  }
  
  
  return (index_park)
}

return_best_subroute_in_position<-function(route, vc_list, position, input, type_problem, routes_res) {
  
  result_list <- list()
  result_list$best_cost <- Inf
  result_list$best_posinit_vc <- Inf
  result_list$best_posinit_other <- Inf
  result_list$position <- position
  
  
  if (length(vc_list)) {
    for (i in 1:length(vc_list)) {
      cost1 <- Inf
      cost2 <- Inf
      cost3 <- Inf
      
      # check left
      if ((route[2]!=route[vc_list[[i]]]) && ( ! sum(route[vc_list[[i]]:(length(route))] > input$n1) )) {
        new_route_left <- c(0, route[vc_list[[i]]]) 
        new_route_left <- c(new_route_left, route[2:(length(route))])
        if ((type_problem == "MCTTRP")&&(check_capacity_hoppers_MCTTRP_CVR_new(new_route_left, routes_res, input))) cost1 <- local_cost(new_route_left, input$matriz.distancia)
        if (type_problem == "TTRP") cost1 <- local_cost(new_route_left, input$matriz.distancia)
      }
      # right
      if ((route[length(route)-1]!=route[vc_list[[i]]])  && ( ! sum(route[1:vc_list[[i]]] > input$n1) )) {
        new_route_right <- route[1:(length(route)-1)]
        new_route_right <- c(new_route_right, route[vc_list[[i]]])
        new_route_right <- c(new_route_right, 0)
        if ((type_problem == "MCTTRP")&&(check_capacity_hoppers_MCTTRP_CVR_new(new_route_right, routes_res, input))) cost2 <- local_cost(new_route_right, input$matriz.distancia) 
        else cost2 <- local_cost(new_route_right, input$matriz.distancia) 
      }
      # both
      if ((route[2]!=route[vc_list[[i]]]) && (route[length(route)-1]!=route[vc_list[[i]]])) {
        
        new_route_both <- c(0, route[vc_list[[i]]])
        new_route_both <- c(new_route_both, route[2:(vc_list[[i]]-1)])
        new_route_both <- c(new_route_both, route[(vc_list[[i]]+1):(length(route)-1)])
        new_route_both <- c(new_route_both, route[vc_list[[i]]])
        new_route_both <- c(new_route_both, 0)
        if ((type_problem == "MCTTRP")&&(check_capacity_hoppers_MCTTRP_CVR_new(new_route_both, routes_res, input))) cost3 <- local_cost(new_route_both, input$matriz.distancia) 
        if (type_problem == "TTRP")  cost3 <- local_cost(new_route_both, input$matriz.distancia) 
      }
      
      
      if (cost1 < result_list$best_cost ) {
        result_list$best_posinit_vc <- vc_list[i]
        result_list$best_posinit_other <- 1
        result_list$best_cost <- cost1
        result_list$route <- new_route_left
      }
      
      if (cost2 < result_list$best_cost ) {
        result_list$best_posinit_vc <- vc_list[i]
        result_list$best_posinit_other <- length(route)  
        result_list$best_cost <- cost2
        result_list$route <- new_route_right
      }   
      
      if (cost3 < result_list$best_cost ) {
        result_list$best_posinit_vc <- vc_list[i]
        result_list$best_posinit_other <- length(route)  
        result_list$best_cost <- cost3
        result_list$route <- new_route_both
      }   
      
    }
  }
  
  
  return(result_list)
}

count_vc_clients<-function(route, n1) {
  counter <- 0
  for (i in 1:length(route)) {
    if (route[i] > n1) {
      counter <- counter + 1
    }
  }
  
  return(counter)
}

#' Postproc ...
#'
#' @param rutas
#' @param input
#' @return A list of results ...
postproc_add_new_subroutes_TTRP<-function(rutas_res, rutas, input, opt){
  
  new_rutas_res <- rutas_res
  correc <- 1
  if (opt) {
    correc <- 1.15
  }
  
  for (i in 1:length(rutas_res)) {
    if (rutas_res[[i]]$type == "PVR" ) {
      success <- 0
      index_j <- -1
      selected_subroute <- 0
      min_cost <- local_cost(rutas, input$matriz.distancia) * correc
      for (j in 1:length(new_rutas_res)) {
        if (new_rutas_res[[j]]$type == "PTR" ) {
          if (((rutas_res[[i]]$total_load + new_rutas_res[[j]]$total_load) <= input$capacidad.vehiculo)){
            
            subroute_pvr <- rutas_res[[i]]$route
            subroute_ptr <- new_rutas_res[[j]]$route
            
            for (z in 2:(length(subroute_pvr)-1)) {
              
              new_route <- new_rutas_res
              aux <- append(subroute_pvr[1:z], subroute_ptr[2:(length(subroute_ptr)-1)])
              aux <- append(aux, subroute_pvr[z:length(subroute_pvr)])
              new_route[[j]]$route <- aux
              new_route[[j]]$type <- "CVR"
              new_route[[j]]$total_load <- calc_load2(aux, input$vector.demandas)
              new_route[[j]]$total_load_tc_clients <- calc_load_only_truck(aux, input$vector.demandas, input)
              new_route_eval <- new_route[-i]
              cost <- local_cost(convert_in_route(new_route_eval), input$matriz.distancia)
              feasible <- 0
              if ((new_route[[j]]$total_load <= input$capacidad.vehiculo)) feasible <- 1
              if ((cost < min_cost) && (feasible == 1)) {
                success <- 1
                index_j <- j
                selected_subroute <- new_route
                min_cost <- cost
              }
              
            }
            
          }
        }
      }
      
      if (success == 1) {
        new_rutas_res <- selected_subroute[-i]
        
      }
    }
  }
  
  
  return(new_rutas_res)
}

convert_in_route<-function(route_res) {
  route <- list()
  route <- 0
  for (i in 1:length(route_res)) {
    route <- c(route, route_res[[i]]$route[2:length(route_res[[i]]$route)])
    route <- c(route, 0)
  }
  
  return (route)
}

select_route_TTRP<-function(counter_routes,routes,type_routes,input, dis_client){
  min_value = Inf
  min_index = -1
  index_selected_route = -1
  for ( ii in 1:counter_routes) {
    route_ii <- routes[[ii]]
    type_ii <- type_routes[[ii]]
    if (type_ii == 0) {
      capacity <- input$capacidad.truck[1]
    }
    else {
      capacity <- input$capacidad.vehiculo[1]
    }
    for ( jj in 1:length(route_ii)) {
      local_route <- list()
      local_route <- 0
      
      if (jj == 1) {
        local_route <- append(local_route, dis_client)
        local_route <- append(local_route, route_ii)
      }
      else if (jj == length(routes[[ii]])) {
        local_route <- append(local_route, route_ii)
        local_route <- append(local_route, dis_client)
      }
      else {
        local_route <- append(local_route, route_ii[1:jj])
        local_route <- append(local_route, dis_client)
        local_route <- append(local_route, route_ii[jj:length(route_ii)])
      }
      local_route <- append(local_route, 0)
      if (calc_load(local_route, input$vector.demandas, capacity) > 0 ) {
        cost <- local_cost(local_route, input$matriz.distancia)
        if (cost < min_value) {
          min_index <- jj
          min_value <- cost
          selected_route <- local_route
          index_selected_route <- ii
        }
      }
    }
  }
  
  result <- list()
  result$selected_route<-selected_route
  result$cost<-cost
  result$index_selected_route<-index_selected_route
  result$min_index<-min_index
  
  return(result)
}

select_subroute_to_add_TTRP<-function(counter_routes,routes,type_routes,input, dis_client){
  min_value = Inf
  min_index = -1
  index_selected_route = -1
  for ( ii in 1:counter_routes) {
    route_ii <- routes[[ii]]
    type_ii <- type_routes[[ii]]
    if (type_ii == 2) {
      duplicate1 = -1
      duplicate2 = -1
      for ( jj in 1:(length(route_ii)-1)) {
        for ( zz in (jj+1):length(route_ii)) {
          if (jj == zz) {
            duplicate1 = jj
            duplicate2 = zz
            break
          }
        }
        if (duplicate1!=-1) {
          break
        }
      }
      capacity <- input$capacidad.truck[1]
      for ( jj in duplicate1:duplicate2) {
        local_route <- list()
        local_route <- route_ii[1:duplicate1]
        if (jj == duplicate1) {
          local_route <- append(local_route, dis_client)
          local_route <- append(local_route, route_ii[(duplicate1+1):length(route_ii)])
        }
        else if (jj == duplicate2) {
          local_route <- append(local_route, route_ii[(duplicate1+1):(duplicate2-1)])
          local_route <- append(local_route, dis_client)
          local_route <- append(local_route, route_ii[duplicate2:duplicate2])
        }
        else {
          local_route <- append(local_route, route_ii[(duplicate1+1):(duplicate1+jj)])
          local_route <- append(local_route, dis_client)
          local_route <- append(local_route, route_ii[(duplicate1+1+jj):duplicate2])
        }
        local_route <- append(local_route, route_ii[(duplicate2+1):length(route_ii)])
        if (calc_load(local_route, input$vector.demandas, capacity) > 0 ) {
          cost <- local_cost(local_route, input$matriz.distancia)
          if (cost < min_value) {
            min_index <- jj
            min_value <- cost
            selected_route <- local_route
            index_selected_route <- ii
          }
        }
      }
    }
    
  }
  
  result <- list()
  result$selected_route<-selected_route
  result$cost<-cost
  result$index_selected_route<-index_selected_route
  result$min_index<-min_index
  
  return(result)
}

add_to_route_TTRP<-function(R, rutas, ruta_origin, position, newclient) {
  
  for (i in 1:length(rutas)){
    
    for (j in 1:length(ruta_origin)){
      
      if (rutas[i+j]!=ruta_origin[j]) {
        noEq <- FALSE
        break
      }
      else {
        noEq <- TRUE
      }
      
    }
    
    if (noEq == TRUE) {
      new_rutas <- 0
      new_rutas <- append(new_rutas, rutas[1:(i+position)])
      new_rutas <- append(new_rutas, newclient)
      new_rutas <- append(new_rutas, rutas[(i+position+1):length(rutas)])
      break
    }
  }
  
  init <- return_index_route(rutas, ruta_origin[1])+1
  for (zz in init:(init+length(ruta_origin))) {
    position <- new_rutas[zz]
    # add new values
    R[position+1,1] <- new_rutas[zz-1]
    R[position+1,3] <- new_rutas[zz+1]
  }
  
  
  results <- list()
  results$rutas <- new_rutas
  results$R <- R
  
  return(results)
  
}


descent_search <- function(input, current_solution, type_problem){

  #current_solution <- update_solution(current_solution, input, type_problem)
  print("init descent_search")
  # Partimos de una solucion perturbada
  counter <- 1
  changed_list <- 1:length(current_solution)
  # improvement
  no_more_imp <- 0
  # create mov list in C++
  .Call('createMovsList', PACKAGE = "mcttrpalns")
  
  current_solution <- result_improvement(input, current_solution, type_problem)
  current_cost <- calculateTotalDistanceTS(input, 0, current_solution, type_problem)
  
  while (!no_more_imp) {
    
    movements_imp_new(input, current_solution, type_problem, input$vecinity, 1, 0, changed_list, 1)
    size_mov_list <- .Call('return_size_mov_list', PACKAGE = "mcttrpalns")

    evaluate_cost_mov_list_new(input, changed_list, current_solution, 0, size_mov_list, type_problem)

    #concert list to vector
    if (size_mov_list) {
      
      order_movs_new("cost")
      movid <- return_mov_struct(1)
      
      print(paste0("it ", movid$mov_list_cost, " < ", current_cost, " mov ", movid$string1, 
                   " indexr1 ", movid$indexr1, " indexr2 ", movid$indexr2,
                   " mov_list_cost ",  movid$mov_list_cost,
                   " mov_list_cost_feas ", movid$mov_list_cost_feas,
                   " mov_list$opt_reconf ", movid$opt_reconf
                   ))

#print(movid$route1)
#print(movid$route2)
#readline()
#print(current_solution[[movid$indexr1]]$route)
#if (movid$indexr2 != -1 ) print(current_solution[[movid$indexr2]]$route)
      
#      print(movid$mov_list_cost - current_cost)

      if (((movid$mov_list_cost - current_cost) < -0.01)) {
          changed_routes <- modified_changed_list(changed_routes, movid$indexr1,  movid$indexr2 )
          current_solution <- insert_selected_mov_new(input, movid, current_solution, type_problem)
          modified_mov_list_using_changed_list_new(changed_routes)

      } else no_more_imp <- 1
        
    } else no_more_imp <- 1
    
    counter <- counter + 1
    
    current_solution <- result_improvement(input, current_solution, type_problem)
    current_cost <- calculateTotalDistanceTS(input, 0, current_solution, type_problem)
  }
  
  return(current_solution)
}
