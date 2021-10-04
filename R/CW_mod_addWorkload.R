#' Add workload to a truck
#'
#' @param R
#' @param Tolvas
#' @param input
#' @param pos1
#' @param pos2
#' @param CargaT
#' @param H.trailer
#' @param n.truck
#' @param n.trailer
#' @param a1 flag
#' @param a2 flag
#' @param option "row" or "col"
#' @return A list of results, with CargaT, pos1, n.truck, n.trailer, a1, a2
addWorkload_R<-function(R, Tolvas, input, pos1, pos2, CargaT, n.truck,
                        n.trailer, a1, a2, option){

  if (option=="row") {
    rr <- 1
  } else {
    rr <- 3
  }

  while(R[pos1,rr]!=0){ #Sumamos la carga de los clientes anteriores a i
    a1 <- a1 + 1
    result_sub <- check_tolvas(pos1, Tolvas, n.truck, n.trailer)
    n.truck <- result_sub$n.truck
    n.trailer <- result_sub$n.trailer

    CargaT <- CargaT+sum(input$matriz.demandas[R[pos1,rr]+1,])
    pos1<-R[pos1,rr]+1
    if(pos1==pos2) a2<-a2+1
  }


  result <- list()
  result$CargaT <- CargaT
  result$pos1 <- pos1
  result$n.truck <- n.truck
  result$n.trailer <- n.trailer
  result$a1 <- a1
  result$a2 <- a2

  return(result)

}

#' Add workload to a subtour ...
#'
#' @param R
#' @param Rhat
#' @return A list of results ...
addWorkload_R_check_subtour<-function(R, Rhat, Tolvas, input, CWTTRP_struct,
                                      pos1, pos2, pos3, pos4,
                                      n.truck, n.trailer,
                                      a1, a2, a3, n, n1, parking, option){

  if (option=="row") {
    rr1 <- 1
    rr2 <- 3
  } else {
    rr1 <- 3
    rr2 <- 1
  }

  while(R[pos1,rr1]!=0){

    a1 <- a1 + 1

    # Vamos comprobando si en cada cliente v.c. se inicia un subtour
    if (Rhat[pos1,rr2]!=0) {
      if (parking == 1) {
        CWTTRP_struct$parking[CWTTRP_struct$park_index] <- pos1
        CWTTRP_struct$park_index <- CWTTRP_struct$park_index + 1
      }
      pos2 <- pos1 - 1

      # Voy sumando la carga en las sub-rutas
      result_sub <- addWorkload_Rhat(Rhat, Tolvas, input, pos1, pos2,
                                     CWTTRP_struct$CargaT, n.truck,
                                     n.trailer, "col")
      CWTTRP_struct$CargaT <- result_sub$CargaT
      pos1 <- result_sub$pos1
      n.truck <- result_sub$n.truck
      n.trailer <- result_sub$n.trailer
    }

    result_sub <- check_tolvas(pos1, Tolvas, n.truck, n.trailer)
    n.truck <- result_sub$n.truck
    n.trailer <- result_sub$n.trailer
    CWTTRP_struct$CargaT<-CWTTRP_struct$CargaT+sum(input$matriz.demandas[R[pos1,rr1]+1,])
    pos1<- R[pos1,rr1] + 1
    if(pos1==pos4) a2 <- a2 + 1
    #  aqui estamos viendo si hay algun t.c.
    if(sum(pos1==((n1+2):n))==1) a3 <- a3 + 1
    pos3 <- pos1
  }

  result <- list()
  result$CWTTRP_struct <- CWTTRP_struct
  result$pos1 <- pos1
  result$pos2 <- pos2
  result$pos3 <- pos3
  result$n.truck <- n.truck
  result$n.trailer <- n.trailer
  result$a1 <- a1
  result$a2 <- a2
  result$a3 <- a3

  return(result)

}

#' Add workload to ...
#'
#' @param R
#' @param Rhat
#' @return A list of results ...
addWorkload_Rhat<-function(Rhat, Tolvas, input, pos1, pos2, CargaT, n.truck,
                           n.trailer, option){

  if (option=="row") {
    rr <- 1
  } else {
    rr <- 3
  }

  while(Rhat[pos1,rr] != pos2){

    result_sub <- check_tolvas(pos1, Tolvas, n.truck, n.trailer)
    n.truck <- result_sub$n.truck
    n.trailer <- result_sub$n.trailer

    CargaT <- CargaT + sum(input$matriz.demandas[Rhat[pos1,rr]+1,])
    pos1 <- Rhat[pos1,rr]+1

  }
  pos1 <- pos2 + 1

  result <- list()
  result$CargaT <- CargaT
  result$pos1 <- pos1
  result$n.truck <- n.truck
  result$n.trailer <- n.trailer

  return(result)

}

#' Add workload to ...
#'
#' @param R
#' @param CWTTRP_struct
#' @return A list of results ...
addWorkload<-function(input, R, CWTTRP_struct, pos1, pos2, flag, option){

  if (option=="row") {
    des <- 1
  }else {
    des <- 3
  }

  tc_load_route <- 0
  vc_load_route <- 0

  index_parking <- c(0)
  is_parking <- is_in_parking_list(CWTTRP_struct, pos1)
  if (is_parking) index_parking <- c(index_parking, pos1)

  while(R[pos1,des]!=0){
    if (R[pos1,des] <= input$n1) {
      vc_load_route<-vc_load_route+input$vector.demandas[R[pos1,des]+1]
    } else {
      tc_load_route<-tc_load_route+input$vector.demandas[R[pos1,des]+1]
    }
    pos1<-R[pos1,des]+1
    if(pos1==pos2) {
      flag<-flag+1
    }

    is_parking <- is_in_parking_list(CWTTRP_struct, pos1)
    if (is_parking) index_parking <- c(index_parking, pos1)

  }

  if (length(index_parking) > 1) {
    is_parking <- 1
    index_parking <- index_parking[2:length(index_parking)]
  } else {
    is_parking <- 0
    index_parking <- -1
  }

  result <- list()
  result$flag <- flag
  result$is_parking <- is_parking
  result$index_parking <- index_parking
  result$tc_load_route <- tc_load_route
  result$vc_load_route <- vc_load_route
  result$pos1 <- pos1

  return(result)

}


#' Add workload to ...
#'
#' @param R
#' @param CWTTRP_struct
#' @return A list of results ...
addWorkload_parking<-function(input, Rhat, position_parking){

  des <- 1
  vc_load_subroute <- 0
  tc_load_subroute <- 0

  for ( ii in 1:length(position_parking)) {
    position <- position_parking[ii]
    exit <- 0

    vc_load_subroute <-vc_load_subroute+input$vector.demandas[Rhat[position_parking[ii],des]+1]

    while(exit==0){
        if (Rhat[position,des] <= input$n1) {
          #print(paste0(Rhat[position,des], " ", input$vector.demandas[Rhat[position,des]+1]))
          vc_load_subroute <-vc_load_subroute+input$vector.demandas[Rhat[position,des]+1]
        } else {
          #print(paste0(Rhat[position,des], " ", input$vector.demandas[Rhat[position,des]+1]))
          tc_load_subroute <-tc_load_subroute+input$vector.demandas[Rhat[position,des]+1]
        }

        position<-Rhat[position,des]+1

        if(position==position_parking[ii]) {
          exit <- 1
        }
    }

  }

  result <- list()
  result$vc_load_subroute <- vc_load_subroute
  result$tc_load_subroute <- tc_load_subroute

  return(result)

}


