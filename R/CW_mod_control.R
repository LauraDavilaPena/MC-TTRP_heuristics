control_PTR<-function(CWTTRP_struct, R, Rhat, input, option, verbose){
  
  rutas <- return_route_TTRP(CWTTRP_struct, R, Rhat, input$n, input$n1, verbose)
  if (length(rutas) < 2) return(0)
  
  rutas_res <- create_result_struct(rutas, input, option)  
  
  counter_limit_PTR <- 0
  limit_capacity <- (input$capacidad.truck)*3/4
  limit_trucks_only <- input$n_trucks - input$n_trailers
  
  for (elem in rutas_res) {
    if (elem$type == "PTR") {
      if (elem$total_load >=  limit_capacity) {
        counter_limit_PTR <- counter_limit_PTR + 1
      }
    }
  }
  
  
  if (counter_limit_PTR >= limit_trucks_only) {
    return(1)
  }
  else {
    return(0)
  }
}

control_PVR<-function(CWTTRP_struct, R, Rhat, input, option, verbose){
  
  rutas <- return_route_TTRP(CWTTRP_struct, R, Rhat, input$n, input$n1, verbose)
  if (length(rutas) < 2) return(0)
  
  rutas_res <- create_result_struct(rutas, input, option)  
  
  counter_limit_PVR <- 0
  limit_capacity <- (input$capacidad.trailer)*3/4
  limit_trailer_only <- input$n_trailers
  
  for (elem in rutas_res) {
    if (elem$type == "PVR") {
      if (elem$total_load >=  limit_capacity) {
        counter_limit_PVR <- counter_limit_PVR + 1
      }
    }
  }
  
  
  if (counter_limit_PVR >= limit_trailer_only) {
    return(1)
  }
  else {
    return(0)
  }
}

check_limit_PTR<-function(CWTTRP_struct, R, Rhat, input, option){

        if (CWTTRP_struct$limit_ptr) {
          limit_capacity <- (input$capacidad.truck)*3/4
          result <- 0
          rutas <- return_route_TTRP(CWTTRP_struct, R, Rhat, input$n, input$n1, verbose)
          rutas_res <- create_result_struct(rutas, input, option)
          for (elem in rutas_res) {
            if ((elem$type == "PTR")&&(elem$total_load >=  limit_capacity)) {
              cond <- sum(elem$route==(CWTTRP_struct$pos$Positionfilas-1)) + 
                      sum(elem$route==(CWTTRP_struct$pos$Positioncolumnas-1))
              if (cond) result <- 1
            }
          }
        } else {
          result  <-1  
        }
        print(result)
  return(result)
}

check_limit_PTR_init<-function(CWTTRP_struct, R, Rhat, input, option){

  if (CWTTRP_struct$limit_ptr == 0) {
    print(CWTTRP_struct$pos$Positionfilas-1)
    print(CWTTRP_struct$pos$Positioncolumnas-1)
    result <- 0
  } else {
    result  <-1  
  }
  return(result)
}

check_limit_PVR<-function(CWTTRP_struct, R, Rhat, input, option){
  
  if (CWTTRP_struct$limit_pvr) {
    result <- 0
    rutas <- return_route_TTRP(CWTTRP_struct, R, Rhat, input$n, input$n1, verbose)
    rutas_res <- create_result_struct(rutas, input, option)  
    for (elem in rutas_res) {
      if (elem$type == "PVR") {
        cond <- sum(elem$route==(CWTTRP_struct$pos$Positionfilas-1)) + 
                sum(elem$route==(CWTTRP_struct$pos$Positioncolumnas-1))
        if (cond) result <- 1
      }
    }
  } else {
    result  <-1  
  }
  
  return(result)
}

check_limit_cVR<-function(CWTTRP_struct, R, Rhat, input, option){
  
  if ((CWTTRP_struct$limit_pvr)) {
    result <- 0
    rutas <- return_route_TTRP(CWTTRP_struct, R, Rhat, input$n, input$n1, verbose)
    condition1 <- ((sum(rutas==(CWTTRP_struct$pos$Positionfilas-1))==0   && 
                      sum(rutas==(CWTTRP_struct$pos$Positioncolumnas-1))>0) ||
                     (sum(rutas==(CWTTRP_struct$pos$Positionfilas-1))>0   && 
                        sum(rutas==(CWTTRP_struct$pos$Positioncolumnas-1))==0))
    if (!condition1) {
      result <- 1
    } 
    print(result)
  } else {
    result  <-1  
  }
  
  return(result)
}

control_CVR<-function(CWTTRP_struct, R, Rhat, input, n, verbose){
  
  rutas <- return_route_TTRP(CWTTRP_struct, R, Rhat, n, input$n1, verbose)
  # <- create_result_struct(rutas, input)
  
  print()
  counter_limit_PTR = 0
  limit_capacity = (input$capacidad.truck)/2
  limit_trucks_only = input$n_trucks - input$n_trailers
  for (elem in rutas_res) {
    if (elem$type == "PTR") {
      if (elem$capacity >=  limit_capacity) {
        counter_limit_PTR = counter_limit_PTR + 1
      }
    }
  }
  
  #print(rutas_res)
  readline(prompt = "")
  
  if (counter_limit_PTR >= limit_trucks_only) {
    #print(paste("NON",counter_limit_PTR))
    return(0)
  }
  else {
    #print(paste("SI",counter_limit_PTR))
    return(1)
  }
}

