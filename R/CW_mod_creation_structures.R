#' Create and initialize the main structure for MC-TTRP problem
#'
#' @param input Input structure list
#' @return The CWTTRPstruct list, where the global variables of the solver are
#' managed.
createCWMCTTRPStruct<-function(input){
  CWTTRP_struct = list()
  CWTTRP_struct$newPositionfilas <- 0
  CWTTRP_struct$newPositioncolumnas <- 0
  CWTTRP_struct$newPositionfilas2 <- 0
  CWTTRP_struct$newPositioncolumnas2 <- 0
  CWTTRP_struct$newPositionfilas3 <- 0
  CWTTRP_struct$newPositioncolumnas3 <- 0
  CWTTRP_struct$CargaT <- 0
  CWTTRP_struct$x <- 0
  CWTTRP_struct$y <- 0
  CWTTRP_struct$z <- 0
  CWTTRP_struct$a <- 0
  CWTTRP_struct$b <- 0
  CWTTRP_struct$aux <- 0
  CWTTRP_struct$aux1 <- 0
  CWTTRP_struct$aux2 <- 0
  CWTTRP_struct$iter <- 0
  CWTTRP_struct$primerclienteMT <- 0
  CWTTRP_struct$ultimoclienteMT <- 0

  CWTTRP_struct$demandas_res  <- input$matriz.demandas
  CWTTRP_struct$H.camion_res  <- reprow(input$H.camion, 10)
  CWTTRP_struct$H.trailer_res <- reprow(input$H.trailer, 10)
  CWTTRP_struct$parking <- numeric(input$n1)
  CWTTRP_struct$park_index <- 1
  CWTTRP_struct$parking_list <- list()
  CWTTRP_struct$parking_list <- 0  

  CWTTRP_struct$t <- 1
  CWTTRP_struct$s <- 1
  CWTTRP_struct$tc <- 1
  CWTTRP_struct$tt <- 1
  CWTTRP_struct$ss <- 1


  return(CWTTRP_struct)
}

#' Create final output data in TTRP
#'
#' @param input Input structure list
#' @return A list with all information about the route.
createCWTTRPStruct<-function(){
  new = list()
  new$Positionfilas <- 0
  new$Positioncolumnas <- 0
  new$Positionfilas2 <- 0
  new$Positioncolumnas2 <- 0
  
  pos = list()
  pos$Positionfilas <- 0
  pos$Positioncolumnas <- 0
  
  CWTTRP_struct = list()
  CWTTRP_struct$new <- new
  CWTTRP_struct$pos <- pos
  CWTTRP_struct$CargaT <- 0
  CWTTRP_struct$current_type <- -1
  
  CWTTRP_struct$iter <- 0
  CWTTRP_struct$parking_list <- list()
  CWTTRP_struct$parking_list <- 0
  
  return(CWTTRP_struct)
}

#' Create input structure for MC-TTRP problem
#'
#' @param matriz.demandas
#' @param matriz.distancia
#' @param capacidad.truck
#' @param capacidad.trailer
#' @param capacidad.vehiculo
#' @param H.camion
#' @param H.trailer
#' @return A input list with all input data.
createInputStruct_MCTTRP<-function(matriz.demandas, matriz.distancia, capacidad.truck,
                                   capacidad.trailer, capacidad.vehiculo, H.camion, H.trailer,
                                   n1, n_trucks, n_trailers){
  input <- list()
  input$matriz.demandas    <- matriz.demandas
  input$matriz.distancia   <- matriz.distancia
  input$capacidad.truck    <- capacidad.truck
  input$capacidad.trailer  <- capacidad.trailer
  input$capacidad.vehiculo <- capacidad.vehiculo
  input$H.camion  <- H.camion
  input$H.trailer <- H.trailer
  input$n1 <- n1
  input$n <-  dim(input$matriz.distancia)[1]
  input$n_trucks <- n_trucks
  input$n_trailers <- n_trailers
  input$vecinity <- calc_vecinity(input)
  
  return(input)
}

#' Create input structure for TTRP problem
#'
#' @param vector.demandas
#' @param matriz.distancia
#' @param capacidad.truck
#' @param capacidad.trailer
#' @param n1
#' @return A input list with all input data.
createInputStruct_TTRP<-function(vector.demandas, matriz.distancia, capacidad.truck,
                                 capacidad.trailer, n1, n_trucks, n_trailers){
  input <- list()
  input$vector.demandas <- rep(vector.demandas)
  input$matriz.distancia <- matrix(rep(matriz.distancia), nrow = nrow(matriz.distancia))
  input$capacidad.truck <- capacidad.truck
  input$capacidad.trailer <- capacidad.trailer
  input$capacidad.vehiculo <- input$capacidad.truck+input$capacidad.trailer
  input$n1 <- n1
  input$n <- dim(input$matriz.distancia)[1]
  input$n_trucks <- n_trucks
  input$n_trailers <- n_trailers
  input$vecinity <- calc_vecinity(input)
  
  return(input)
}

#' Create and initialize the main structure for MC-TTRP problem
#'
#' @param input Input structure list
#' @return The CWTTRPstruct list, where the global variables of the solver are
#' managed.
createResultStruct_MCTTRP<-function(rutas, coste.total, H.camion_res,
                                    H.trailer_res, demandas_res, rutas_res, input){
  
  result<-list()
  result$input <- input
  result$routes <- rutas
  result$cost <- coste.total
  result$H.camion_res <- H.camion_res
  result$H.trailer_res <- H.trailer_res
  result$demandas_res <- demandas_res
  
  res_r <- extract_info_from_result_res(rutas_res, input, "MCTTRP")
    
  result$n_trucks <- res_r$n_trucks
  result$n_trailers <- res_r$n_trailers
  result$PTR <- res_r$PTR
  result$PVR <- res_r$PVR
  result$CVR <- res_r$CVR
  result$result_res <- res_r$result_res
  
  
  return(result)
}


extract_info_from_hoppers<-function(result_res, Hoppers, input) {
  
  for (i in 1:length(result_res)) {
    if (result_res[[i]]$type == "CVR") {
      result_res[[i]]$main_tour <- return_main_route(result_res[[i]]$route)
      result_res[[i]]$subtours <- return_subroutes(result_res[[i]]$route, input$n1)
    }
    id_truck <- check_truck_hopper(Hoppers, result_res[[i]]$route[2])
    r_h <- calc_load_hoppers(Hoppers, id_truck)

    result_res[[i]]$truck_number <- i
    if (result_res[[i]]$type == "PTR") {
      result_res[[i]]$trailer_number <- 0
    } 
    else {
      result_res[[i]]$trailer_number <- i
    }
    
    result_res[[i]]$used_hoppers_truck <- r_h$used_hoppers_truck
    result_res[[i]]$used_hoppers_trailer <- r_h$used_hoppers_trailer
    
    result_res[[i]]$clients_tc <- list() 
    result_res[[i]]$clients_vc <- list() 
    
    num_clients <- delete_zeros(unique(result_res[[i]]$route))
    counter <- 1
    
    for (j in num_clients) {
      if (j <= input$n1) {
        client_vc <- list()
        client_vc$id <- j
        client_vc$demands <-  input$matriz.demandas[j+1,]
        client_vc$hoppers_trailers <- list()
        client_vc$hoppers_trucks <- list()
        counter2 <- 1
        counter3 <- 1
        for (z in 1:length(Hoppers[,1])) {
          if ((Hoppers[z,1] == j) && (Hoppers[z,3] == "trailer")) {
            client_vc$hoppers_trailers[[counter2]] <- Hoppers[z,c(2,5,6)]
            counter2 <- counter2 + 1
          }
          if ((Hoppers[z,1] == j) && (Hoppers[z,3] == "truck")) {
            client_vc$hoppers_trucks[[counter3]] <- Hoppers[z,c(2,5,6)]
            counter3 <- counter3 + 1
          }
        }
        result_res[[i]]$clients_vc[[counter]] <- client_vc
        counter <- counter + 1
      }
    }
    
    counter <- 1
    for (j in num_clients) {
      if (j > input$n1) {
        client_tc <- list()
        client_tc$id <- j
        client_tc$demands <-input$matriz.demandas[j+1,]
        client_tc$hoppers_trucks <- list()
        counter2 <- 1
        for (z in 1:length(Hoppers[,1])) {
          if (Hoppers[z,1] == j) {
            client_tc$hoppers_trucks[[counter2]] <- Hoppers[z,c(2,5,6)]
            counter2 <- counter2 + 1
          }
        }
        result_res[[i]]$clients_tc[[counter]] <- client_tc
        counter <- counter + 1
      }
    }

    result_tc_vc <- return_tc_vc(result_res[[i]]$route, input)
    result_res[[i]]$TCs <-result_tc_vc$TCs
    result_res[[i]]$VCs <-result_tc_vc$VCs
  }

  return(result_res)
}


extract_info_from_result_res<-function(result_res, input, option) {
  result <- list()
  n_trucks <- 0
  n_trailers <- 0
  ptr <- 0
  pvr <- 0
  cvr <- 0

  for (i in 1:length(result_res)) {
    
    if (option == "TTRP") {
      result_res[[i]]$total_load <-  calc_load2(result_res[[i]]$route, input$vector.demandas)
      result_res[[i]]$total_load_tc_clients <- calc_load_only_truck(result_res[[i]]$route, input$vector.demandas, input)
    } 
    else if (option == "MCTTRP")  {
      result_res[[i]]$total_load <-  calc_load2_MC(result_res[[i]]$route, input$matriz.demandas)
      result_res[[i]]$total_load_tc_clients <- calc_load_only_truck_MC(result_res[[i]]$route, input$matriz.demandas, input)        
    }
    result_res[[i]]$cost <- local_cost(result_res[[i]]$route, input$matriz.distancia)
    
    if (result_res[[i]]$type == "CVR") {
      result_res[[i]]$main_tour <- return_main_route(result_res[[i]]$route)
      result_res[[i]]$subtours <- return_subroutes(result_res[[i]]$route, input$n1)
    }
    result_res[[i]]$truck_number <- i
    if (result_res[[i]]$type == "PTR") {
      result_res[[i]]$trailer_number <- 0
    } 
    else {
      result_res[[i]]$trailer_number <- i
    }
    
    if (result_res[[i]]$type == "PTR") {
      n_trucks <- n_trucks + 1
      ptr <- ptr + 1
    } else {
      n_trucks <- n_trucks + 1
      n_trailers <- n_trailers + 1
      if (result_res[[i]]$type == "PVR") pvr <- pvr + 1
      if (result_res[[i]]$type == "CVR") cvr <- cvr + 1
    }
    
    num_clients <- delete_zeros(unique(result_res[[i]]$route))

    result_tc_vc <- return_tc_vc(result_res[[i]]$route, input)
    result_res[[i]]$TCs <-result_tc_vc$TCs
    result_res[[i]]$VCs <-result_tc_vc$VCs
  }
  
  
  result <- list()
  result$n_trucks <- n_trucks
  result$n_trailers <- n_trailers
  result$PTR <- ptr
  result$PVR <- pvr
  result$CVR <- cvr
  result$result_res <- result_res
  
  return(result)
}


#' Create final output data in MC-TTRP
#'
#' @param input Input structure list
#' @return A list with all information about the route.
createFinalResult_TTRP<-function(rutas, coste.total, matriz.distancia, result_res, vector.demandas, input){

  # rutas<-rutas-1
  # Output
  n_trucks <- 0
  n_trailers <- 0
  ptr <- 0
  pvr <- 0
  cvr <- 0
  
  for (i in 1:length(result_res)) {
    result_res[[i]]$total_load <-  calc_load2(result_res[[i]]$route, input$vector.demandas)
    result_res[[i]]$total_load_tc_clients <- calc_load_only_truck(result_res[[i]]$route, input$vector.demandas, input)
    
    if (result_res[[i]]$type == "CVR") {
      result_res[[i]]$main_tour <- return_main_route(result_res[[i]]$route)
      result_res[[i]]$subtours <- return_subroutes(result_res[[i]]$route, input$n1)
    }
    result_res[[i]]$truck_number <- i
    if (result_res[[i]]$type == "PTR") {
      result_res[[i]]$trailer_number <- 0
    } else {
      result_res[[i]]$trailer_number <- i
    }
    
    if (result_res[[i]]$type == "PTR") {
      n_trucks <- n_trucks + 1
      ptr <- ptr + 1
    } else {
      n_trucks <- n_trucks + 1
      n_trailers <- n_trailers + 1
      if (result_res[[i]]$type == "PVR") pvr <- pvr + 1
      if (result_res[[i]]$type == "CVR") cvr <- cvr + 1
    }
    
    result_res[[i]]$clients_tc <- list() 
    result_res[[i]]$clients_vc <- list() 
    
    num_clients <- delete_zeros(unique(result_res[[i]]$route))
    counter <- 1
    for (j in num_clients) {
      if (j <= input$n1) {
        client_vc <- list()
        client_vc$id <- j
        client_vc$demands <- vector.demandas[j+1]
        result_res[[i]]$clients_vc[[counter]] <- client_vc
        counter <- counter + 1
      }
    }    
    counter <- 1
    for (j in num_clients) {
      if (j > input$n1) {
        client_tc <- list()
        client_tc$id <- j
        client_tc$demands <- vector.demandas[j+1]
        result_res[[i]]$clients_tc[[counter]] <- client_tc
        counter <- counter + 1
      }
    }  
    
    result_tc_vc <- return_tc_vc(result_res[[i]]$route, input)
    result_res[[i]]$TCs <-result_tc_vc$TCs
    result_res[[i]]$VCs <-result_tc_vc$VCs
  }
  
  result = list()
  result$routes = rutas
  result$cost = coste.total
  result$n_trucks = n_trucks
  result$n_trailers = n_trailers
  result$PVR <- pvr
  result$PTR <- ptr
  result$CVR <- cvr
  result$result_res <- result_res
  result$input <- input
  
  return(result)
}


#' Create S matrix
#'
#' @param matriz.distancia
#' @param n
#' @return Savings matrix
matrixS<-function(matriz.distancia,n, n1){
  #matrixS: Calcular la matriz de ahorros "usual", donde S_ij=c_0i+c_j0-c_ij

  S<-matrix(0,nrow=n,ncol=n) #Matriz ahorros
  rownames(S)<-0:(n-1)
  colnames(S)<-0:(n-1)

  for(i in 2:n){
    for(j in 2:n){
      {
        if(i!=j){
          S[i,j]<-(matriz.distancia[1,i]+matriz.distancia[1,j]-matriz.distancia[i,j])
        }
      }
    }
  }
  return(S)
}

matrixS_2<-function(matriz.distancia,n, n1){
  #matrixS: Calcular la matriz de ahorros "usual", donde S_ij=c_0i+c_j0-c_ij
  
  S<-matrix(0,nrow=n,ncol=n) #Matriz ahorros
  rownames(S)<-0:(n-1)
  colnames(S)<-0:(n-1)
  
  for(i in 2:n){
    for(j in 2:n){
      {
        if(i!=j){
          if ((i<n1&&j<n1)&&(i>n1&&j>n1)) weight = 10
          else weight = 1
          S[i,j]<-(matriz.distancia[1,i]+matriz.distancia[1,j]-matriz.distancia[i,j])*weight
        }
      }
    }
  }
  return(S)
}


#' Create Shat matrix
#'
#' @param matriz.distancia
#' @param n
#' @param n1
#' @return  Savings matrix in surtours
matrixShat<-function(matriz.distancia,n, n1){
  #matrixShat: Calcular la matriz de ahorros hat, modificada: cuando los dos clientes
  # son de tipo v.c. o de tipo t.c., los ahorros se calculan de la forma usual. Cuando
  # uno de los clientes es de tipo v.c. y el otro de tipo t.c., entonces la cosa cambia:
  # calculamos S_ij=2c_0i-2c_j0 (pues asumimos que se puede formar una ruta del tipo 0-i-j-i-0)

  Shat<-matrix(0,nrow=n,ncol=n) #Matriz ahorros hat
  colnames(Shat) <- 0:(n-1)
  rownames(Shat) <- 0:(n-1)

  for(i in 2:(n1+1)){
    for(j in 2:(n1+1)){
      {
        if(i!=j){
          Shat[i,j]<-matriz.distancia[1,i]+matriz.distancia[1,j]-matriz.distancia[i,j]
        }
      }
    }
  }
  for(i in (n1+2):n){
    for(j in (n1+2):n){
      if (i!=j){
        Shat[i,j]<-matriz.distancia[1,i]+matriz.distancia[1,j]-matriz.distancia[i,j]
      }
    }
  }
  for(i in 2:(n1+1)){
    for(j in (n1+2):n){
      Shat[i,j]<-2*matriz.distancia[j,1]-2*matriz.distancia[i,j]
    }
  }
  for(i in (n1+2):n){
    for(j in 2:(n1+1)){
      Shat[i,j]<-2*matriz.distancia[i,1]-2*matriz.distancia[i,j]
    }
  }
  return(Shat)
}

return_main_route<-function(route) {
  main_route <- c(0)
  state <- 1
  counter_list <- 0
  counter_parking <- 0
  current_parking <- 0
  
  
  for (i in 2:(length(route)-1)) {
    if (state == 1) {
      main_route <- c(main_route, route[i])
    }
    if (sum(route==route[i])>=2) {
      counter_parking <- counter_parking + 1
      
      if ((state==0)&&(counter_parking == sum(route==route[i]))) {
        state <- 1
        counter_parking <- 0
      }
      else {
        state <- 0
      }
    }
  }
  
  main_route <- c(main_route, 0)
  return(main_route)
}


return_subroutes<-function(route, n1) {
  subtours <- list()
  counter_list <- 1
  for (current_pointer in 2:(length(route)-1)) {
    if (sum(route==route[current_pointer])>1) {
      par1 <- route[current_pointer]
      tour <- c(par1)
      for (i in (current_pointer+1):length(route)) {
        tour <- c(tour, route[i])
        if (sum(route==route[i])>1) {
          par2 <-  route[i]
          break
        }
      }
      if (par1 == par2) {
        subtours[[counter_list]] <- list()
        subtours[[counter_list]]$tour <-  tour
        subtours[[counter_list]]$root <- par1
        counter_list <- counter_list + 1
      }
    }
  }
  
  for (i in 1:length(subtours)) {
    f_vc <- 0
    f_tc <- 0
    counter_length_tour <- 0
    for (j in 2:(length(subtours[[i]]$tour)-1)){

      if (subtours[[i]]$tour[j] <= n1) {
        if (f_vc == 0) {
          subtours[[i]]$vc_clients <- c(subtours[[i]]$tour[j])
          f_vc <- 1
        } else {
          subtours[[i]]$vc_clients <- c(subtours[[i]]$vc_clients, subtours[[i]]$tour[j])
        }
      } else {
        if (f_tc == 0) {
          subtours[[i]]$tc_clients <- c(subtours[[i]]$tour[j])
          f_tc <- 1
        } else {
          subtours[[i]]$tc_clients <- c(subtours[[i]]$tc_clients, subtours[[i]]$tour[j])
        }
      }
      subtours[[i]]$length  <- counter_length_tour <- counter_length_tour + 1
    }
    counter_length_tour
  }
  
  return(subtours)
}

return_subroutes2<-function(route, n1) {
  subtours <- list()
  counter_list <- 1
  
  for (current_pointer in 2:(length(route)-1)) {
    if (sum(route==route[current_pointer])>1) {
      par1 <- route[current_pointer]
      tour <- c(par1)
      for (i in (current_pointer+1):length(route)) {
        tour <- c(tour, route[i])
        if (sum(route==route[i])>1) {
          par2 <-  route[i]
          break
        }
      }
      if (par1 == par2) {
        subtours[[counter_list]] <- list()
        subtours[[counter_list]]$tour <-  tour
        subtours[[counter_list]]$root <- par1
        counter_list <- counter_list + 1
      }
    }
  }
  
  return(subtours)
}

is_parking_i<-function(cvr_route, i) {
  
  check <- 0
  if (sum(cvr_route==cvr_route[i])>1) {
    check <- 1
  }
  
  return(check)
}

return_subroute_index<-function(cvr_route, i, subtours) {
  
  subroute_index <- 0
  for (j in 1:length(subtours)) {
    if (sum(subtours[[j]]$tour[2:(length(subtours[[j]]$tour)-1)] == cvr_route[i]) > 0) {
      subroute_index = j
      break
    }
  }
  
  return(subroute_index)
}

is_last_parking<-function(cvr_route, i) {
  check <- 1
  if (sum(cvr_route==cvr_route[i])>1) {
    ocurrences <- which(cvr_route==cvr_route[i])
    if (i != ocurrences[length(ocurrences)]) {
      check <- 0
    }
  }
  return(check)
}

return_tc_vc<-function(routes, input) {
  routes_unique <- unique(routes)
  
  result_tc_vc <- list()
  counter_tcs <- 0
  counter_vcs <- 0
  for (i in 2:length(routes_unique)) {
      if (routes_unique[i] <= input$n1) {
        if (counter_vcs == 0) {
          result_tc_vc$VCs <- c(routes_unique[i])
          counter_vcs <- counter_vcs + 1
        } else {
          result_tc_vc$VCs <- c(result_tc_vc$VCs, routes_unique[i])
        }
      }
    
    if (routes_unique[i] > input$n1) {
      if (counter_tcs == 0) {
        result_tc_vc$TCs <- c(routes_unique[i])
        counter_tcs <- counter_tcs + 1
      } else {
        result_tc_vc$TCs <- c(result_tc_vc$TCs, routes_unique[i])
      }
    }
  }
  
  return(result_tc_vc)
}

create_route_from_main_route_and_subroutes<-function(subtours, main_tour) {
 new_route <- c(0)
 
 for (i in 2:(length(main_tour))) {
   new_route <- c(new_route, main_tour[i])
   for (j in 1:length(subtours)){
     if (subtours[[j]]$root == main_tour[[i]]) {
       new_route <- c(new_route, subtours[[j]]$tour[2:(length(subtours[[j]]$tour))])
     }
   }
 }
 
 return(new_route)
}

update_solution<-function(initial_solution, input, problem_type) {
    iroute <- all_routes(initial_solution)
    icost <- calculateTotalDistance(input, iroute)
    if (problem_type == "TTRP")   {
      result <- createFinalResult_TTRP(iroute, icost, input$matriz.distancia, initial_solution, input$vector.demandas, input)
      solution <- result$result_res
    }
    if (problem_type == "MCTTRP") {
      
      result <- createResultStruct_MCTTRP(iroute, icost, input$H.camion_res, input$H.trailer_res, input$matriz.demandas, initial_solution, input)
      solution <- result$result_res
    }

  return(solution)
}

