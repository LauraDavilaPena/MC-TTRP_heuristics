#' selecting initial parking
#'
#' @param R
#' @param Rhat
#' @return A list of results ...
return_route_MCTTRP<-function(CWTTRP_struct, Tolvas, R, Rhat, n, n1, verbose){

  # Vector de rutas FINAL
  rutas<<-numeric()

  # Inicializamos asi, luego ya cambiaremos a 0 (pues hay que empezar en el deposito)
  rutas[1] = -1

  # Nos movemos por el vector de rutas (en R)
  indicador<-2

  for(i in 2:n){
    if(R[i,1]==0){
      # si el cliente es v.c.
      if (sum((i-1)==1:n1)==1){
        if (R[i,3]!=0){
          rutas[indicador] <- (i-1)
          while(rutas[indicador]!=0){
            if(Rhat[rutas[indicador]+1,3]!=0){
              ind_root <- rutas[indicador]

              while(sum(rutas==ind_root)==1){
                rutas[indicador+1]<-Rhat[rutas[indicador]+1,3]
                indicador<-indicador+1
                ind_root <- rutas[indicador]
              }
            }

            rutas[indicador+1]<-R[rutas[indicador]+1,3]
            indicador<-indicador+1
          }
          indicador <- indicador + 1
        }

        else if(R[i,3]==0 &&  dim(Tolvas[Tolvas[,1]==(i-1),][3])[1]>0){
          # dado que esta en el MT, comenzamos por incluirlo. Y ahora tambien tenemos
          # que ir recorriendo su correspondiente ruta
          if(sum(Tolvas[Tolvas[,1]==(i-1),][3]=="trailer")>0 ){

            rutas[indicador] <- (i-1)

            while(rutas[indicador]!=0){
              # Ahora debemos chequear si en ese cliente se inicia una subruta
              # (y en tal caso, contemplarla en el vector rutas)
              if(Rhat[rutas[indicador]+1,3]!=0){
                ind_root <- rutas[indicador]

                while(sum(rutas==ind_root)==1){
                  rutas[indicador+1]<-Rhat[rutas[indicador]+1,3]
                  indicador<-indicador+1
                  ind_root <- rutas[indicador]
                }
              }

              rutas[indicador+1]<-R[rutas[indicador]+1,3]
              indicador<-indicador+1
            }
            indicador <- indicador + 1
          }
        }
      }
      else{
        if (R[i,3]!=0){
          rutas[indicador] <- (i-1)
          while(rutas[indicador]!=0){
            rutas[indicador+1]<-R[rutas[indicador]+1,3]
            indicador<-indicador+1
          }
          indicador <- indicador + 1
        }

      }
    }
  }
  rutas[1]=0


  return(rutas)
}


#' returnFinalRouteVector
#'
#' @param matriz.distancia
#' @param n
#' @param n1
#' @return S
return_route_TTRP<-function(CWTTRP_struct, R, Rhat, n, n1, verbose){

  # Vector de rutas FINAL
  rutas<<-numeric()

  # Inicializamos asi, luego ya cambiaremos a 0 (pues hay que empezar en el deposito)
  rutas[1] = -1

  # Nos movemos por el vector de rutas (en R)
  indicador<-2

  for(i in 2:n){
    if(R[i,1]==0){
      # si el cliente es v.c.
      if (sum((i-1)==1:n1)==1){
        if (R[i,3]!=0){
          rutas[indicador] <- (i-1)
          while(rutas[indicador]!=0){
            if(Rhat[rutas[indicador]+1,3]!=0){
              ind_root <- rutas[indicador]

              while(sum(rutas==ind_root)==1){
                rutas[indicador+1]<-Rhat[rutas[indicador]+1,3]
                indicador<-indicador+1
                ind_root <- rutas[indicador]
              }
            }

            rutas[indicador+1]<-R[rutas[indicador]+1,3]
            indicador<-indicador+1
          }
          indicador <- indicador + 1
        }

        else if ((R[i,3]==0) && (sum(CWTTRP_struct$parking_list==i)==1)){
          # dado que esta en el MT, comenzamos por incluirlo. Y ahora tambien tenemos
          # que ir recorriendo su correspondiente ruta
#          print(R[i,])

            rutas[indicador] <- (i-1)

            while(rutas[indicador]!=0){
              # Ahora debemos chequear si en ese cliente se inicia una subruta
              # (y en tal caso, contemplarla en el vector rutas)
              if(Rhat[rutas[indicador]+1,3]!=0){
                ind_root <- rutas[indicador]

                while(sum(rutas==ind_root)==1){
                  rutas[indicador+1]<-Rhat[rutas[indicador]+1,3]
                  indicador<-indicador+1
                  ind_root <- rutas[indicador]
                }
              }

              rutas[indicador+1]<-R[rutas[indicador]+1,3]
              indicador<-indicador+1
            }
            indicador <- indicador + 1
        }
      }
      else{
        if (R[i,3]!=0){
          rutas[indicador] <- (i-1)
          while(rutas[indicador]!=0){
            rutas[indicador+1]<-R[rutas[indicador]+1,3]
            indicador<-indicador+1
          }
          indicador <- indicador + 1
        }

      }
    }
  }
  rutas[1]=0

  return(rutas)
}


#' selecting initial parking
#'
#' @param R
#' @param Rhat
#' @return A list of results ...
selecting_initial_parking_TTRP<-function(CWTTRP_struct, Tolvas, R, Rhat, n, n1, verbose){

  # Vector de rutas FINAL
  rutas<<-numeric()

  # Inicializamos asi, luego ya cambiaremos a 0 (pues hay que empezar en el deposito)
  rutas[1] = -1

  # Nos movemos por el vector de rutas (en R)
  indicador<-2

  for(i in 2:n){
    if(R[i,1]==0){

      # ahora vamos a distinguir segun tipo de clientes rutas[indicador] <- (i-1)
      # ahora el cliente puede ser de dos tipos

      # si el cliente es v.c.
      if (sum((i-1)==1:n1)==1){

        if (R[i,3]!=0){

          rutas[indicador] <- (i-1)

          while(rutas[indicador]!=0){

            if(Rhat[rutas[indicador]+1,3]!=0){
              ind_root <- rutas[indicador]

              while(sum(rutas==ind_root)==1){
                rutas[indicador+1]<-Rhat[rutas[indicador]+1,3]
                indicador<-indicador+1
                ind_root <- rutas[indicador]
              }
            }

            rutas[indicador+1]<-R[rutas[indicador]+1,3]
            indicador<-indicador+1
          }
          indicador <- indicador + 1
        }

        else if(R[i,3]==0 &&  dim(Tolvas[Tolvas[,1]==(i-1),][3])[1]>0){
          # dado que esta en el MT, comenzamos por incluirlo. Y ahora tambien tenemos
          # que ir recorriendo su correspondiente ruta
          if(sum(Tolvas[Tolvas[,1]==(i-1),][3]=="trailer")>0 ){

            rutas[indicador] <- (i-1)

            while(rutas[indicador]!=0){
              # Ahora debemos chequear si en ese cliente se inicia una subruta
              # (y en tal caso, contemplarla en el vector rutas)
              if(Rhat[rutas[indicador]+1,3]!=0){
                ind_root <- rutas[indicador]

                while(sum(rutas==ind_root)==1){
                  rutas[indicador+1]<-Rhat[rutas[indicador]+1,3]
                  indicador<-indicador+1
                  ind_root <- rutas[indicador]
                }
              }

              rutas[indicador+1]<-R[rutas[indicador]+1,3]
              indicador<-indicador+1
            }
            indicador <- indicador + 1
          }
        }
      }
      else{
        if (R[i,3]!=0){
          rutas[indicador] <- (i-1)
          while(rutas[indicador]!=0){
            rutas[indicador+1]<-R[rutas[indicador]+1,3]
            indicador<-indicador+1
          }
          indicador <- indicador + 1
        }

      }
    }
  }
  rutas[1]=0


  return(rutas)
}

#' Calcula la posicion de Sm en S
#'
#' @param R
#' @param Rhat
#' @return A list of results ...
positionSm<-function(S, Sm, n){
  if(order(S,decreasing=TRUE)[1]%%n==0){    #order(S,decreasing=T)[1] dame o indice do elemento de S mais grande
    #order(S,decreasing=T)[1]%%n da o modulo: resto de dividir o anterior entre n
    Positionfilas<-n
    Positioncolumnas<-order(S,decreasing=TRUE)[1]%/%n     # #order(S,decreasing=T)[1]%/%n da a parte enteira
  }
  else{
    Positionfilas<-order(S,decreasing=TRUE)[1]%%n
    Positioncolumnas<-order(S,decreasing=TRUE)[1]%/%n + 1
  }

  pos=list()
  pos$Positionfilas = Positionfilas
  pos$Positioncolumnas = Positioncolumnas

  return(pos)
}

return_index_route<-function(route, index) {
  for (i in 1:length(route)) {
    if (route[i] == index) {
      break
    }
  }
  return (i)
}

type_route_TTRP<-function(rutas,ii) {
  route_local <- rutas[ii]
  counter_local <- 1
  with_tc <- 0
  while (rutas[ii+counter_local] != 0) {
    route_local <- c( route_local, rutas[ii+counter_local] )
    if (rutas[ii+counter_local] > n1){
      with_tc <- 1
    }
    counter_local <- counter_local + 1
  }
  if (length(unique(duplicated(route_local))) > 1) {
    type_route <- 3
  }
  else {
    if (with_tc == 1) {
      type_route <- 2
    }
    else {
      type_route <- 1
    }
  }
  result <- list()
  result$route <- route_local
  result$type <- type_route
  return (result)
}


local_cost<-function(local_route, matriz.distancia) {
  cost <- 0.0
  for(i in 1:(length(local_route)-1)){
    cost<-cost + matriz.distancia[local_route[i]+1,local_route[i+1]+1]
  }
  return(cost)
}


calc_load<-function(local_route, vector.demandas, capacity) {
  load <- 0.0
  repeated_index <- list() 
  counter_rep <- 1  
  for(i in 1:(length(local_route))){
    if (sum(local_route == local_route[i])>= 2) {
      repeated_index[counter_rep] <- local_route[i]
    }    
    if (!sum(repeated_index==local_route[i])) {
      load<-load + vector.demandas[local_route[i]+1]
    }
  }
  is_posible <- 1
  if (load > capacity) {
    is_posible <- 0
  }
  return(is_posible)
}

calc_load2<-function(local_route, vector.demandas) {
  load <- 0.0
  repeated_index <- list() 
  counter_rep <- 1  
  route <- unique(local_route)
  for(i in 1:(length(route))){
      load<-load + vector.demandas[route[i]+1]
  }
  return(load)
}

calc_load2_subroute<-function(local_route, vector.demandas) {
  load <- 0.0
  
  sr <- return_subroutes2(local_route)
  
  route_to_calc <- c(0)
  for (i in 1:length(sr)) {
    route_to_calc <- c(route_to_calc, sr$tour[2:(length(sr$tour)-1)])
  }

  
  return(calc_load2(route_to_calc, vector.demandas))
}



calc_load_only_truck<-function(local_route, vector.demandas, input) {
  load <- 0.0
  repeated_index <- list() 
  counter_rep <- 1
  for(i in 1:(length(local_route))){
    if (sum(local_route == local_route[i])>= 2) {
      repeated_index[counter_rep] <- local_route[i]
    }
    if ((local_route[i] > input$n1)&&(!sum(repeated_index==local_route[i]))) {
      load<-load + vector.demandas[local_route[i]+1]
    }
  }
  return(load)
}

calc_load2_MC<-function(local_route, matrix.demands) {
  load <- 0.0
  repeated_index <- list() 
  repeated_indices <- numeric() 
  counter_rep <- 1
  for(i in 1:(length(local_route))){
    if (sum(local_route == local_route[i])>= 2) {
      repeated_index <- local_route[i]
      repeated_indices[counter_rep] <- local_route[i]
      counter_rep <- counter_rep + 1
    }
    if ((!sum(repeated_index==local_route[i]))) {
      for (j in 1:length(matrix.demands[local_route[i]+1,])) {
         load<-load + matrix.demands[local_route[i]+1,j]
      }
    }
  }
  # 05/05/2021 Nuevo (porque no se estaba sumando la demanda de los parkings)
  parkings <- repeated_indices[repeated_indices!=0 & !duplicated(repeated_indices)]
  load <- load + sum(matrix.demands[parkings+1,])
  
  return(load)
}

calc_load_only_truck_MC<-function(local_route, matrix.demands, input) {
  load <- 0.0
  repeated_index <- list() 
  counter_rep <- 1
  for(i in 1:(length(local_route))){
    if (sum(local_route == local_route[i])>= 2) {
      repeated_index[counter_rep] <- local_route[i]
    }
    if ((local_route[i] > input$n1)&&((!sum(repeated_index==local_route[i])))) {
      for (j in 1:length(matrix.demands[local_route[i]+1,])) {
          load<-load + matrix.demands[local_route[i]+1,j]
      }
    }
  }
  return(load)
}

analyse<-function(rutas, input, rutas_res, option) {
  cvr <- 0
  ptr <- 0
  pvr <- 0
  cap_truck <- input$capacidad.truck[1]
  cap_total <- input$capacidad.vehiculo[1]
  
  counter_route <- 1
  for (i in 2:(length(rutas))) {

     if ((rutas[i-1] == 0) && (rutas[i] != 0)) {
       subroute <- list
       subroute <- c(0, rutas[i])
     }
     else if ((rutas[i-1] != 0) && (rutas[i] == 0)) {
       exist_subroute <- 0
       if (sum(duplicated(subroute))) exist_subroute <- 1
       subroute <- c(subroute, 0)
       exist_truck <- 0
       counter_route <- counter_route + 1

       if (sum(subroute>input$n1)) exist_truck <- 1
       if (exist_subroute == 1) {
          if (option == "TTRP") c <- calc_load2(subroute, input$vector.demandas)
          if (option == "MCTTRP") c <- calc_load2_MC(subroute, input$matriz.demandas)
          cc <- local_cost(subroute, input$matriz.distancia) 
          sr <- return_subroutes(subroute, input$n1)
          for (z in 1:length(sr)) {
            sr1 <- sr[[z]]$tour
            sr1 <- sr1[2:(length(sr1)-1)]
            if (option == "TTRP") ccc <- calc_load2(sr1, input$vector.demandas)
            if (option == "MCTTRP") ccc <- calc_load2_MC(sr1, input$matriz.demandas)
          }
          cvr <- cvr + 1
       }
       else if ((exist_subroute == 0) && (exist_truck == 1)) {
         if (option == "TTRP") c <- calc_load2(subroute, input$vector.demandas)
         if (option == "MCTTRP") c <- calc_load2_MC(subroute, input$matriz.demandas)
         cc <- local_cost(subroute, input$matriz.distancia) 
         ptr <- ptr + 1
         
       }
       else {
         if (option == "TTRP") c <- calc_load2(subroute, input$vector.demandas)
         if (option == "MCTTRP") c <- calc_load2_MC(subroute, input$matriz.demandas)
         cc <- local_cost(subroute, input$matriz.distancia) 
         pvr <- pvr + 1
       }
       
     } else if ((rutas[i-1] != 0) && (rutas[i] != 0)) {
       subroute <- c(subroute, rutas[i])
     }

  }
  counter_errors <- 0

  for (i in 1:length(rutas_res)) {
    if (option == "TTRP") tload <- calc_load2(rutas_res[[i]]$route, input$vector.demandas)
    if (option == "MCTTRP") tload <- calc_load2_MC(rutas_res[[i]]$route, input$matriz.demandas)
    
    tcost <- local_cost(rutas_res[[i]]$route, input$matriz.distancia) 
    
    if (rutas_res[[i]]$type == "CVR") {
      sr <- return_subroutes(rutas_res[[i]]$route, input$n1)
      main_route <- return_main_route(rutas_res[[i]]$route)
      
      if ((tload != rutas_res[[i]]$total_load)){
        print(paste0("   ERROR total load in CVR is not updated, route number ", i, " load in struct ", rutas_res[[i]]$total_load, " real load ", tload))
        counter_errors <- counter_errors +  1
      }
      
      if (tload > cap_total) {
        print(paste0("   ERROR total capacity in CVR, route number ", i, " current load ", tload, " max load ", cap_total))
        counter_errors <- counter_errors +  1
      }
      
      for (j in 1:length(sr)) {
        sr1 <- sr[[j]]$tour
        sr1 <- sr1[2:(length(sr1)-1)]
        if (option == "TTRP") sload <- calc_load2(sr1, input$vector.demandas)
        if (option == "MCTTRP") sload <- calc_load2_MC(sr1, input$matriz.demandas)
        if (sload > cap_truck) {
          print(paste0("   ERROR subroute capacity in CVR, route number ", i, " subroute ", j))
          counter_errors <- counter_errors +  1
        }
      }
      
      for (j in 1:length(main_route)) {
          if (main_route[j] > input$n1){
            print(paste0("   ERROR client TC in main root, route number ", i))
            counter_errors <- counter_errors +  1
          }
      }
    }
    else if (rutas_res[[i]]$type == "PTR") {
      if ((tload != rutas_res[[i]]$total_load)) {
        print(paste0("   ERROR total load in PTR is not updated, route number ", i, " load in struct ", rutas_res[[i]]$total_load, " real load ", tload))
        counter_errors <- counter_errors +  1
      }
      if ((tload > cap_truck)) {
        print(paste0("   ERROR total capacity in PTR, route number ", i, " current load ", tload, " max load ", cap_truck))
        counter_errors <- counter_errors +  1
      }
    }
    else if (rutas_res[[i]]$type == "PVR") {
      if ((tload != rutas_res[[i]]$total_load)){
        print(paste0("   ERROR total load in PVR is not updated, route number ", i, " load in struct ", rutas_res[[i]]$total_load, " real load ", tload))
        counter_errors <- counter_errors +  1
      }
      if(tload > cap_total) {
        print(paste0("   ERROR total capacity in PVR, route number ", i, " current load ", tload, " max load ", cap_total))
        counter_errors <- counter_errors +  1
      }
    }
  }
  
  # CHECK HOPPERS
  if (option == "MCTTRP") {
    for (i in 1:length(rutas_res)) {
      if (rutas_res[[i]]$type == "CVR") {
        if (!check_capacity_hoppers_MCTTRP_analysis(rutas_res[[i]], input, i)) {
          print(paste0("   ERROR in hoppers (CVR), route number ", i))
          counter_errors <- counter_errors +  1
        } 
      }
      else if (rutas_res[[i]]$type == "PTR") {
        if (!check_capacity_hoppers_MCTTRP_analysis(rutas_res[[i]], input, i)) {
          print(paste0("   ERROR in hoppers (PTR), route number ", i))
          counter_errors <- counter_errors +  1
        } 
      }
      else if (rutas_res[[i]]$type == "PVR") {
        if (!check_capacity_hoppers_MCTTRP_analysis(rutas_res[[i]], input, i)) {
          print(paste0("   ERROR in hoppers (PVR), route number ", i))
          counter_errors <- counter_errors +  1
        } 
      }
      
      for (j in 2:(length(rutas_res[[i]]$route)-1)) {
        if (rutas_res[[i]]$route[j] > input$n1) {
          found <- 0
          is_null <- 0
          if (length(rutas_res[[i]]$clients_tc)) {
              for (z in 1:length(rutas_res[[i]]$clients_tc)) {
                if (is.null(rutas_res[[i]]$clients_tc[[z]])) {
                  is_null <- 1
                  break
                }
                if (rutas_res[[i]]$route[j] == rutas_res[[i]]$clients_tc[[z]]$id) {
                  found <- 1
                  break
                }
              }
          }
        }
        else {
          found <- 0
          is_null <- 0
          if (length(rutas_res[[i]]$clients_vc)) {
            for (z in 1:length(rutas_res[[i]]$clients_vc)) {
              if (is.null(rutas_res[[i]]$clients_vc[[z]])) {
                is_null <- 1
                break
              }
              if (rutas_res[[i]]$route[j] == rutas_res[[i]]$clients_vc[[z]]$id) {
                found <- 1
                break
              }
            }
          }
        }
        if (is_null) print(paste0("null value in route ", i))
        else {
          if (found == 0) print(paste0("client not found ", rutas_res[[i]]$route[j]))
        }
      }
      
    }
  }
  
  print(paste0("   total nodes in final route  ---> ", length(unique(rutas))))
  print(paste0("   n  ---> ", input$n))
  print(paste0("   n1 ---> ", input$n1))
  print(paste0("   NUMBER OF ERRORS -> ", counter_errors))

}



update_Tolvas<-function(Hoppers, rutas) {
  rutas_num <- rutas

  for (i in 1:(length(rutas))) {
    if (rutas[i]==0) {
      rutas_num[i] = 0
    } else {
      for (j in 1:length(Hoppers[,1])) {
        if (Hoppers[j, 1] == rutas[i]) {
          rutas_num[i] = Hoppers[j, 4]
          break;
        } 
      }
    }
  }
  
  return(rutas_num)
}

check_Hoppers_route<-function(rutas_id) {
  error <- "NO ERROR IN HOPPERS"
  current_id <- 0
  
  for (i in 2:(length(rutas_id))) {
    if ((rutas_id[i-1]==0) && ((rutas_id[i]!=0))) {
      current_id = rutas_id[i]
    }
    else if ((rutas_id[i-1]!=0) && ((rutas_id[i]!=0))) {
      if (current_id != rutas_id[i]) {
        error <- "ERROR IN HOPPERS"
        break
      }
    }
  }
  
  return(error)
}

update_Hopper_matrix<-function(Hoppers, H.trailer_res, H.truck_res, input, rutas) {
  rutas <- delete_dupl_zeros_route(rutas)
  counter_routes <- 1
  for (i in 2:(length(rutas))) {
    if (rutas[i]==0) {
      counter_routes = counter_routes + 1
    } else {
      for (j in 1:length(Hoppers[,1])) {
        if (Hoppers[j, 1] == rutas[i]) {
          Hoppers[j, 4] = counter_routes
        } 
      }
    }
  }
  
  H.truck_res  <-  input$H.camion
  H.trailer_res <- input$H.trailer
  counter_routes <- counter_routes - 1

  for (i in 1:counter_routes) {
    counter_hoppers_trailer <- 0
    counter_hoppers_truck <- 0
    for (j in 1:length(Hoppers[,1])) {
      if ((Hoppers[j, 4] == i)&&(Hoppers[j, 3] == "trailer")) {
        counter_hoppers_trailer <- counter_hoppers_trailer + 1
      }
      if ((Hoppers[j, 4] == i)&&(Hoppers[j, 3] == "truck")) {
        counter_hoppers_truck <- counter_hoppers_truck + 1
      }
    }
    
    if (counter_hoppers_truck) {
      for (z in 1:counter_hoppers_truck) {
          H.truck_res[i,z] = -1
      }
    }
    
    if (counter_hoppers_trailer) {
      for (z in 1:counter_hoppers_trailer) {
          H.trailer_res[i,z] = -1
      }
    }
    
  }
  
  result <- list()
  result$Hoppers <- Hoppers
  result$H.trailer_res <- H.trailer_res
  result$H.truck_res <- H.truck_res
  
  return(result)
}

create_result_struct<-function(rutas, input, option) {
  
  rutas_res <- list()
  counter <- 1
  for (i in 2:(length(rutas))) {

    if ((rutas[i-1] == 0) && (rutas[i] != 0)) {
      route <- list()
      route <- c(0, rutas[i])
    }
    else if ((rutas[i-1] != 0) && (rutas[i] == 0)) {
      exist_route <- 0
      if (sum(duplicated(route))) exist_route <- 1
      route <- c(route, 0)

      exist_truck <- 0

      if (sum(route>input$n1)) exist_truck <- 1

      if (option == "TTRP") load_check <- calc_load2(route, input$vector.demandas)
      if (option == "MCTTRP") load_check <- calc_load2_MC(route, input$matriz.demandas)
      
      if (exist_route == 1) {
        rutas_res[[counter]] <- list()
        rutas_res[[counter]]$type <-  "CVR"
      }
      else if ((exist_route == 0) && (exist_truck == 1) && (load_check <= input$capacidad.truck[1])) {
        rutas_res[[counter]] <- list()
        rutas_res[[counter]]$type <-  "PTR"
      }
      else {
        rutas_res[[counter]] <- list()
        rutas_res[[counter]]$type <-  "PVR"
      }
      rutas_res[[counter]]$route <-  route
      if (option == "TTRP") {
          rutas_res[[counter]]$total_load <-  calc_load2(route, input$vector.demandas)
          rutas_res[[counter]]$total_load_tc_clients <- calc_load_only_truck(route, input$vector.demandas, input)
      } 
      else if (option == "MCTTRP")  {
        rutas_res[[counter]]$total_load <-  calc_load2_MC(route, input$matriz.demandas)
        rutas_res[[counter]]$total_load_tc_clients <- calc_load_only_truck_MC(route, input$matriz.demandas, input)        
      }
      rutas_res[[counter]]$cost <- local_cost(route, input$matriz.distancia)
      counter <- counter + 1
    } else if ((rutas[i-1] != 0) && (rutas[i] != 0)) {
      route <- c(route, rutas[i])
    }

  }
  
  return(rutas_res)
}

add_vehicles_result_res<-function(routes_res, Hoppers ) {

  for (i in 1:length(routes_res)) {
    for (k in 1:length(Hoppers[,1])) {

      if((sum(Hoppers[k,1]==routes_res[[i]]$route[2])>0) && (routes_res[[i]]$type == "PTR")){ 
        routes_res[[i]]$truck <- Hoppers[k,4]
        break
      }
      else if((sum(Hoppers[k,1]==routes_res[[i]]$route[2])>0) && (routes_res[[i]]$type == "PVR")){ 
        routes_res[[i]]$trailer <- Hoppers[k,4]
        routes_res[[i]]$truck <- Hoppers[k,4]
        break
      }
      else if((sum(Hoppers[k,1]==routes_res[[i]]$route[2])>0) && (routes_res[[i]]$type == "CVR")){ 
        routes_res[[i]]$trailer <- Hoppers[k,4]
        routes_res[[i]]$truck <- Hoppers[k,4]
        break
      }
    }
  }
  
  return(routes_res)
}



calc_load_hoppers<-function(Hoppers, num_veh) {
  num_hoppers_truck <- 0
  num_hoppers_trailer <- 0
  for (i in 1:length(Hoppers[,1])) {
    if (Hoppers[i, 4] == num_veh) {
      if (Hoppers[i, 3] == "trailer") {
        num_hoppers_trailer = num_hoppers_trailer + 1
      }
      else if (Hoppers[i, 3] == "truck") {
        num_hoppers_truck = num_hoppers_truck + 1
      }
    }
  }
  
  result_load <- list()
  
  result_load$used_hoppers_truck <- num_hoppers_truck 
  result_load$used_hoppers_trailer <- num_hoppers_trailer 
  return(result_load)
}

check_truck_hopper<-function(Hoppers, client) {
  index_h <- -1
  for (i in 1:length(Hoppers[,1])) {
    if (Hoppers[i, 1] == client) {
      index_h <- Hoppers[i, 4]
    }
  }
  
  return(index_h)
}


check_pvr<-function(position, R, input, option) {
  if (option == "left") dir <-1
  else if (option == "right") dir <-3
  threshold <- (input$capacidad.truck)/2
  load <- input$vector.demandas[position]
  condition <- 1
  sub <- c(0)
  while ((R[position,dir]!=0) && (condition == 1)) {
    sub <- c(sub, position-1)
    load <- load + input$vector.demandas[R[position,dir]+1]
    if (R[position,dir] > input$n1) condition <- 0
    position<-R[position,dir]+1
  }

  result <- 1
  if ((condition == 1) && (load > threshold)) {
    result <- 0
  }

  return(result)
}

delete_dupl_zeros_route<-function(route) {
  new_route <- c(0)
  for (i in 2:length(route)) {
    if (!(route[i-1] == 0 && route[i] == 0)) {
      new_route <- c(new_route, route[i])
    }
  } 
    return (new_route)
}


delete_zeros<-function(route) {
  bool_route <- (route != 0)
  new_route <- c(-1)
  for (i in 1:length(bool_route)) {
    if (bool_route[i]) {
      new_route <- c(new_route, route[i])
    }
  } 
  new_route <- new_route[2:length(new_route)]
  return (new_route)
}






