SmInS_VcVc_with_hoppers<-function(CWTTRP_struct, Tolvas, R, Rhat, S, Shat,
                                  input, pos, n, nf, n1, verbose){

  t <- CWTTRP_struct$t
  s <- CWTTRP_struct$s
  tc <- CWTTRP_struct$tc
  tt <- CWTTRP_struct$tt
  ss <- CWTTRP_struct$ss
  merge <- 0

  
  if (is_in_parking_list(CWTTRP_struct, pos$Positionfilas)) {
    condition1 <- (R[pos$Positionfilas,3]==0)
  } else {
    condition1 <- (R[pos$Positionfilas,3]==0) &&
      (Rhat[pos$Positionfilas,3]==0)
  }
  
  if (is_in_parking_list(CWTTRP_struct, pos$Positioncolumnas)) {
    condition2 <- (R[pos$Positioncolumnas,1]==0)
  } else {
    condition2 <- (R[pos$Positioncolumnas,1]==0) &&
      (Rhat[pos$Positioncolumnas,1]==0)
  }
  
  
  # if(R[pos$Positionfilas,3]==0 && R[pos$Positioncolumnas,1]==0 &&
  #    CWTTRP_struct$CargaT<=input$capacidad.vehiculo){ # no hay clientes posteriores a i ni anteriores a j
  #   
    if(condition1 && condition2 && CWTTRP_struct$CargaT<=input$capacidad.vehiculo){ 
    CWTTRP_struct$newPositionfilas<-pos$Positionfilas
    CWTTRP_struct$newPositioncolumnas<-pos$Positioncolumnas
    # avoid cycles
    x<-0
    # control var to check t.c. clients
    z<-0
    # si hay clientes anteriores a i, esto sera distinto de 0 porque entraremos
    # en el primer while
    a <- 0
    # si hay clientes posteriores a j, esto sera distinto de 0, porque entraremos
    # en el segundo while
    b <- 0

    n.truck_i <- 0
    n.trailer_i <- 0
    n.truck_j <- 0
    n.trailer_j <- 0

    result_sub <- addWorkload_R_check_subtour(R, Rhat, Tolvas, input, CWTTRP_struct,
                                              CWTTRP_struct$newPositionfilas,
                                              CWTTRP_struct$newPositionfilas3,
                                              CWTTRP_struct$primerclienteMT,
                                              pos$Positionfilas,
                                              n.truck_i, n.trailer_i, a, x, z,
                                              n, n1, 1, "row")

    CWTTRP_struct <- result_sub$CWTTRP_struct
    CWTTRP_struct$newPositionfilas <- result_sub$pos1
    CWTTRP_struct$newPositionfilas3 <- result_sub$pos2
    CWTTRP_struct$primerclienteMT <- result_sub$pos3
    n.truck_i <- result_sub$n.truck
    n.trailer_i <- result_sub$n.trailer
    a <- result_sub$a1
    x <- result_sub$a2
    z <- result_sub$a3
    
    result_sub <- addWorkload_R_check_subtour(R, Rhat, Tolvas, input, CWTTRP_struct,
                                              CWTTRP_struct$newPositioncolumnas,
                                              CWTTRP_struct$newPositioncolumnas3,
                                              CWTTRP_struct$ultimoclienteMT,
                                              pos$Positionfilas,
                                              n.truck_j, n.trailer_j, b, x, z,
                                              n, n1, 1, "col")

    CWTTRP_struct <- result_sub$CWTTRP_struct
    CWTTRP_struct$newPositioncolumnas <- result_sub$pos1
    CWTTRP_struct$newPositioncolumnas3 <- result_sub$pos2
    CWTTRP_struct$ultimoclienteMT <- result_sub$pos3
    n.truck_j <- result_sub$n.truck
    n.trailer_j <- result_sub$n.trailer
    b <- result_sub$a1
    x <- result_sub$a2
    z <- result_sub$a3

    result_sub <- SmInS_VcVc_manage_parking(CWTTRP_struct, Tolvas, R, Rhat, input, a, b,
                                        n.truck_i, n.trailer_i, n.truck_j, n.trailer_j,
                                        verbose)

    CWTTRP_struct <- result_sub$CWTTRP_struct
    R <- result_sub$R
    Rhat <- result_sub$Rhat
    n.truck_i <- result_sub$n.truck_i
    n.trailer_i <- result_sub$n.trailer_i
    n.truck_j <- result_sub$n.truck_j
    n.trailer_j <- result_sub$n.trailer_j

    
    parada <- 0


    result_sub <- SmInS_VcVc_manage_hoppers_trailer(CWTTRP_struct, Tolvas, R,
                                                    input, s, t,
                                                    n.trailer_i, n.truck_i,
                                                    n.trailer_j, n.truck_j,
                                                    parada, pos, verbose)
    CWTTRP_struct <- result_sub$CWTTRP_struct
    Tolvas <- result_sub$Tolvas
    s <- result_sub$s
    t <- result_sub$t
    parada <- result_sub$parada


    result_sub <- SmInS_VcVc_control_fusion(CWTTRP_struct, Tolvas, R, input,
                                                      n.trailer_i, n.truck_i,
                                                      n.trailer_j, n.truck_j,
                                                      z, parada, pos, verbose)
    CWTTRP_struct <- result_sub$CWTTRP_struct
    Tolvas <- result_sub$Tolvas
    parada <- result_sub$flag_stop

    
    result_sub <- SmInS_VcVc_apply_fusion_with_hoppers(CWTTRP_struct, Tolvas, R, Rhat, S,
                                                   Shat, input, n.truck_i, n.trailer_i,
                                                   n.truck_j, n.trailer_j, pos, n, z, x,
                                                   a, b, t, s, tc, tt, ss, nf, parada, verbose)

    CWTTRP_struct <- result_sub$CWTTRP_struct
    t  <- CWTTRP_struct$t
    s  <- CWTTRP_struct$s
    tc <- CWTTRP_struct$tc
    tt <- CWTTRP_struct$tt
    ss <- CWTTRP_struct$ss
    
    Tolvas <- result_sub$Tolvas
    R <- result_sub$R
    Rhat <- result_sub$Rhat
    S <- result_sub$S
    Shat <- result_sub$Shat
    merge <- result_sub$merge

    #Si no es factible tambien lo borramos
    S[pos$Positionfilas,pos$Positioncolumnas]<-0
    Shat[pos$Positionfilas,pos$Positioncolumnas]<-0
  }
  S[pos$Positionfilas,pos$Positioncolumnas]<-0 #Si no es factible tambien lo borramos
  Shat[pos$Positionfilas,pos$Positioncolumnas]<-0

  CWTTRP_struct$t <- t
  CWTTRP_struct$s <- s
  CWTTRP_struct$tc <- tc
  CWTTRP_struct$tt <- tt
  CWTTRP_struct$ss <- ss

  result <- list()
  result$CWTTRP_struct <- CWTTRP_struct
  result$R <- R
  result$Rhat <- Rhat
  result$S <- S
  result$Shat <- Shat
  result$Tolvas <- Tolvas
  result$n  <- n
  result$merge <- merge

  return(result)
}

SmInS_VcVc_manage_parking<-function(CWTTRP_struct, Tolvas, R, Rhat, input, a, b,
                                    n.truck_i, n.trailer_i, n.truck_j, n.trailer_j,
                                    verbose){


  # Check if last MT client is serving as CWTTRP_struct$parking with the goal to
  # init a sub-route.
  if ( b > 0 ){

    result_sub <- is_parking(Rhat, Tolvas, input, CWTTRP_struct,
                             CWTTRP_struct$ultimoclienteMT,
                             CWTTRP_struct$newPositioncolumnas3,
                             n.truck_j, n.trailer_j, 1, "col")

    CWTTRP_struct <- result_sub$CWTTRP_struct
    CWTTRP_struct$ultimoclienteMT <- result_sub$pos1
    CWTTRP_struct$newPositioncolumnas3 <- result_sub$pos2
    n.truck_j <- result_sub$n.truck
    n.trailer_j <- result_sub$n.trailer

    result_sub <- check_tolvas(CWTTRP_struct$ultimoclienteMT, Tolvas, n.truck_j, n.trailer_j)
    n.truck_j <- result_sub$n.truck
    n.trailer_j <- result_sub$n.trailer

  }
  else{
    # Check if j client is serving as CWTTRP_struct$parking with the goal to
    # init a sub-route.
    result_sub <- is_parking(Rhat, Tolvas, input, CWTTRP_struct,
                             CWTTRP_struct$newPositioncolumnas,
                             CWTTRP_struct$newPositioncolumnas3,
                             n.truck_j, n.trailer_j, 1, "col")

    CWTTRP_struct <- result_sub$CWTTRP_struct
    CWTTRP_struct$newPositioncolumnas <- result_sub$pos1
    CWTTRP_struct$newPositioncolumnas3 <- result_sub$pos2
    n.truck_j <- result_sub$n.truck
    n.trailer_j <- result_sub$n.trailer

  }


  # Check if first client serves as CWTTRP_struct$parking
  if ( a > 0 ){

    result_sub <- check_tolvas(CWTTRP_struct$primerclienteMT, Tolvas, n.truck_i, n.trailer_i)
    n.truck_i <- result_sub$n.truck
    n.trailer_i <- result_sub$n.trailer

    result_sub <- is_parking(Rhat, Tolvas, input, CWTTRP_struct,
                             CWTTRP_struct$primerclienteMT,
                             CWTTRP_struct$newPositionfilas3,
                             n.truck_i, n.trailer_i, 1, "col")

    CWTTRP_struct <- result_sub$CWTTRP_struct
    CWTTRP_struct$primerclienteMT <- result_sub$pos1
    CWTTRP_struct$newPositionfilas3 <- result_sub$pos2
    n.truck_i <- result_sub$n.truck
    n.trailer_i <- result_sub$n.trailer

  }
  else{
    # Check if i client is serving as CWTTRP_struct$parking with the goal to
    # init a sub-route.
    result_sub <- is_parking(Rhat, Tolvas, input, CWTTRP_struct,
                             CWTTRP_struct$newPositionfilas,
                             CWTTRP_struct$newPositionfilas3,
                             n.truck_i, n.trailer_i, 1, "col")

    CWTTRP_struct <- result_sub$CWTTRP_struct
    CWTTRP_struct$newPositionfilas <- result_sub$pos1
    CWTTRP_struct$newPositionfilas3 <- result_sub$pos2
    n.truck_i <- result_sub$n.truck
    n.trailer_i <- result_sub$n.trailer

  }


  result <- list()
  result$CWTTRP_struct <- CWTTRP_struct
  result$R <- R
  result$Rhat <- Rhat
  result$n.truck_i <- n.truck_i
  result$n.trailer_i <- n.trailer_i
  result$n.truck_j <- n.truck_j
  result$n.trailer_j <- n.trailer_j

  return(result)
}




SmInS_VcVc_manage_hoppers_trailer<-function(CWTTRP_struct, Tolvas, R,
                                            input, s, t,
                                            n.trailer_i, n.truck_i,
                                            n.trailer_j, n.truck_j,
                                            parada, pos, verbose){


  
  if(n.trailer_i!=0 && n.trailer_j!=0 && n.trailer_i!=n.trailer_j &&
     sum(CWTTRP_struct$H.trailer_res[n.trailer_i,]==-1)+
     sum(CWTTRP_struct$H.trailer_res[n.trailer_j,]==-1)>dim(input$H.trailer)[2]){
    
    
    parada <- 1
    
    if(n.truck_i==0 && n.truck_j==0){
      # primer truck vacio
      # s <- min(which(CWTTRP_struct$H.camion_res[,1]!=-1))
      # numero de Tolvas libres en n.trailer_i
      tol.lib_i <- sum(CWTTRP_struct$H.trailer_res[n.trailer_i,]!=-1)
      # numero de Tolvas libres en n.trailer_j
      tol.lib_j <- sum(CWTTRP_struct$H.trailer_res[n.trailer_j,]!=-1)

      if(sum(CWTTRP_struct$H.trailer_res[n.trailer_i,]==-1)<
         sum(CWTTRP_struct$H.trailer_res[n.trailer_j,]==-1)){
        # trailer mas ocupado
        trailer_ocup <- n.trailer_j
        # trailer mas liberado
        trailer_lib <- n.trailer_i
      }else{
        trailer_ocup <- n.trailer_i
        trailer_lib <- n.trailer_j
      }
      
      s <- trailer_ocup
      
      tol.lib_ocup <- sum(CWTTRP_struct$H.trailer_res[trailer_ocup,]!=-1) # estas son las tolvas libres en el trailer ocupado
      if(sum(CWTTRP_struct$H.trailer_res[trailer_ocup,]!=-1)>0){
        ntol.lib_ocup <- min(which(CWTTRP_struct$H.trailer_res[trailer_ocup,]!=-1)) # la primera tolva libre del trailer ocupado
      }else{
        ntol.lib_ocup <- 0
      }
      tol.ocup_lib <- sum(CWTTRP_struct$H.trailer_res[trailer_lib,]==-1) #tolvas ocupadas en el trailer libre
      ntol.ocup_lib <- max(which(CWTTRP_struct$H.trailer_res[trailer_lib,]==-1)) #ultima tolva ocupada en el trailer libre (coincide con lo anterior)
      
      
      min.TolvasLib <- min(which(Tolvas[,4]==trailer_lib & Tolvas[,3]=="trailer")) #primera tolva en la matriz Tolvas q estaba siendo usada por el trailer_lib
      tot.TolvasLib <- which(Tolvas[,4]==trailer_lib & Tolvas[,3]=="trailer")
      #if(tol.lib_ocup!=0){
      #  for (k in tot.TolvasLib[1:tol.lib_ocup]){
      #    Tolvas[k,4] <- trailer_ocup
      #  }
      #}
      #for (k in ((min.TolvasLib+tol.lib_ocup):(min.TolvasLib+tol.ocup_lib-1))){
      lista_aux <- tot.TolvasLib
      
      availability <- PVRx2_available_compartments(input, tol.lib_ocup, lista_aux, 
                                                   tol.ocup_lib, Tolvas)
      
      if(availability$available_compartments){
        needed_trailer_hoppers_per_product <- availability$trailer_hoppers
        needed_truck_hoppers_per_product <- availability$truck_hoppers
        
        Tolvas_new <- PVRx2_union(input, tol.lib_ocup, lista_aux, Tolvas, trailer_ocup, s, needed_trailer_hoppers_per_product,
                                  needed_truck_hoppers_per_product)
        
        Tolvas <- Tolvas_new$Tolvas_aux
        t <- Tolvas_new$T_index
        
        t <- min(which(Tolvas[,1]==0))
        
        CWTTRP_struct$H.trailer_res[trailer_lib,] <- rep(input$H.trailer[trailer_lib,1],dim(input$H.trailer)[2]) # "vaciamos" el trailer libre
        if(sum(CWTTRP_struct$H.trailer_res[trailer_ocup,]!=-1)>0){
          CWTTRP_struct$H.trailer_res[trailer_ocup,ntol.lib_ocup:dim(input$H.trailer)[2]] <- rep(-1,tol.lib_ocup) # "llenamos" el trailer ocupado (si no esta lleno)
        }
        
        load_truck_hoppers <- Tolvas_new$load_truck_hoppers
        
        if (load_truck_hoppers > 0) CWTTRP_struct$H.camion_res[s,1:load_truck_hoppers] <- rep(-1,load_truck_hoppers)
        
        
      }else{
        parada <- 2
      }
      # Si las Tolvas vacias entre el camion (libre) y el trailer (mas ocupado)
      # son mayores que las Tolvas ocupadas del trailer mas liberado, quiere
      # decir que podemos cambiar la mercancia de este trailer mas liberado
      # (y vaciarlo) al trailer mas ocupado+camion
      # if(sum(CWTTRP_struct$H.trailer_res[trailer_lib,]==-1)<=
      #    sum(CWTTRP_struct$H.trailer_res[trailer_ocup,]!=-1)+
      #    sum(CWTTRP_struct$H.camion_res[s,]!=-1)){
      # 
      #   ss.prueba <- trailer_ocup
      #   # estas son las Tolvas libres en el trailer ocupado
      #   tol.lib_ocup <- sum(CWTTRP_struct$H.trailer_res[ss.prueba,]!=-1)
      #   if(sum(CWTTRP_struct$H.trailer_res[ss.prueba,]!=-1)>0){
      #     # la primera tolva libre del trailer ocupado
      #     ntol.lib_ocup <- min(which(CWTTRP_struct$H.trailer_res[ss.prueba,]!=-1))
      #   }else{
      #     ntol.lib_ocup <- 0
      #   }
      #   #Tolvas ocupadas en el trailer libre
      #   tol.ocup_lib <- sum(CWTTRP_struct$H.trailer_res[trailer_lib,]==-1)
      #   #ultima tolva ocupada en el trailer libre (coincide con lo anterior)
      #   ntol.ocup_lib <- max(which(CWTTRP_struct$H.trailer_res[trailer_lib,]==-1))
      # 
      #   # Time to complete: TOlvas, H.camion_res and H.trailer_res
      # 
      #   CWTTRP_struct$H.trailer_res[trailer_lib,] <- rep(input$H.trailer[trailer_lib,1],dim(input$H.trailer)[2]) # "vaciamos" el trailer libre
      #   if(sum(CWTTRP_struct$H.trailer_res[ss.prueba,]!=-1)>0){
      #     CWTTRP_struct$H.trailer_res[ss.prueba,ntol.lib_ocup:dim(input$H.trailer)[2]] <- rep(-1,tol.lib_ocup) # "llenamos" el trailer ocupado (si no est? lleno)
      #   }
      #   # Y ahora lo que hay que hacer es ir llenando el cami?n
      #   # Sabemos que en el trailer libre habia "tol.ocup_lib" Tolvas ocupadas,
      #   # de las cuales "tol.lib_ocup" han sido trasladadas
      #   # al trailer ocupado, quedando este lleno. Entonces nos quedan todav?a
      #   # por trasladar tol.ocup_lib-tol.lib_ocup Tolvas, que
      #   # hay que pasar al camion. ?Que ocurre? Que igual en el camion no hace
      #   # falta usar este numero de Tolvas, y con una
      #   # menor cantidad es suficiente (pues las Tolvas de los camiones no tienen
      #   # la misma capacidad que la de los traileres)
      #   # Entonces para saber eso debemos explorar la matriz Tolvas (ver cual es
      #   # la cantidad que se le habia asignado a cada
      #   # cliente por tipo de pienso en el trailer "libre" y ver si se puede
      #   # agrupar para "fusionar" Tolvas)
      # 
      # 
      #   min.TolvasLib <- min(which(Tolvas[,4]==trailer_lib & Tolvas[,3]=="trailer")) #primera tolva en la matriz Tolvas q estaba siendo usada por el trailer_lib
      #   tot.TolvasLib <- which(Tolvas[,4]==trailer_lib & Tolvas[,3]=="trailer")
      #   if(tol.lib_ocup!=0){
      #     for (k in tot.TolvasLib[1:tol.lib_ocup]){
      #       Tolvas[k,4] <- trailer_ocup
      #     }
      #   }
      #   #for (k in ((min.TolvasLib+tol.lib_ocup):(min.TolvasLib+tol.ocup_lib-1))){
      #   lista_aux <- tot.TolvasLib[(tol.lib_ocup+1):tol.ocup_lib]
      #   if (verbose == 1) {
      #     print(tot.TolvasLib)
      #     print(tol.lib_ocup)
      #     print(tol.ocup_lib)
      #     print(lista_aux)
      #     print(head(Tolvas,400))
      #     print(CWTTRP_struct$H.trailer_res)
      #     print(n.trailer_i)
      #     print(n.trailer_j)
      #     print(R)
      #   }
      # 
      #   for (k in 1:(length(lista_aux)-1)){
      #     if(Tolvas[lista_aux[k],1]==Tolvas[lista_aux[k+1],1] && Tolvas[lista_aux[k],2]==Tolvas[lista_aux[k+1],2] && Tolvas[lista_aux[k],1]!=0){
      #       feed <- Tolvas[lista_aux[k],2]
      #       if(as.numeric(Tolvas[lista_aux[k],5])+as.numeric(Tolvas[lista_aux[k+1],5])<=input$H.camion[s,1]){
      #         k2 <- k
      #         if(k>1){
      #           if(Tolvas[lista_aux[k2-1],1]==0){
      #             Tolvas[lista_aux[k2-1],1] <- Tolvas[lista_aux[k],1]
      #             Tolvas[lista_aux[k2-1],2] <- feed
      #             Tolvas[lista_aux[k2-1],3] <- "truck"
      #             Tolvas[lista_aux[k2-1],4] <- s
      #             Tolvas[lista_aux[k2-1],5] <- as.numeric(Tolvas[lista_aux[k],5])+as.numeric(Tolvas[lista_aux[k+1],5])
      #             Tolvas[lista_aux[k2-1],6] <- as.numeric(Tolvas[lista_aux[k2-1],5])/(input$H.camion[s,1])
      #             Tolvas[lista_aux[k2],] <- rep(0,dim(Tolvas)[2])
      #             Tolvas[lista_aux[k2+1],] <- rep(0,dim(Tolvas)[2])
      #           }
      #           else{
      #             Tolvas[lista_aux[k2],2] <- feed
      #             Tolvas[lista_aux[k2],3] <- "truck"
      #             Tolvas[lista_aux[k2],4] <- s
      #             Tolvas[lista_aux[k2],5] <- as.numeric(Tolvas[lista_aux[k],5])+as.numeric(Tolvas[lista_aux[k+1],5])
      #             Tolvas[lista_aux[k2],6] <- as.numeric(Tolvas[lista_aux[k2],5])/(input$H.camion[s,1])
      #             Tolvas[lista_aux[k2+1],] <- rep(0,dim(Tolvas)[2])
      #           }
      #         }else{
      #           Tolvas[lista_aux[k2],2] <- feed
      #           Tolvas[lista_aux[k2],3] <- "truck"
      #           Tolvas[lista_aux[k2],4] <- s
      #           Tolvas[lista_aux[k2],5] <- as.numeric(Tolvas[lista_aux[k],5])+as.numeric(Tolvas[lista_aux[k+1],5])
      #           Tolvas[lista_aux[k2],6] <- as.numeric(Tolvas[lista_aux[k2],5])/(input$H.camion[s,1])
      #           Tolvas[lista_aux[k2+1],] <- rep(0,dim(Tolvas)[2])
      #         }
      #       }
      # 
      #     }else if(Tolvas[lista_aux[k],1]==Tolvas[lista_aux[k+1],1] && Tolvas[lista_aux[k],2]!=Tolvas[lista_aux[k+1],2] && Tolvas[lista_aux[k],1]!=0){
      #       k2 <- k
      #       Tolvas[lista_aux[k2],3] <- "truck"
      #       Tolvas[lista_aux[k2],4] <- s
      #       Tolvas[lista_aux[k2],6] <- as.numeric(Tolvas[lista_aux[k2],5])/(input$H.camion[s,1])
      #       # actualizacion: 19-12-2019
      #       # si yo entro aqui es porque le estoy sirviendo al mismo cliente sus dos tipos de pienso diferentes. Entonces modifico la fila lista_aux[k] y
      #       # lista_aux[k+1] (creo q podr?a ser as?, a?adiendo las siguientes tres lineas)
      #       Tolvas[lista_aux[k2+1],3] <- "truck"
      #       Tolvas[lista_aux[k2+1],4] <- s
      #       Tolvas[lista_aux[k2+1],6] <- as.numeric(Tolvas[lista_aux[k2+1],5])/(input$H.camion[s,1])
      # 
      #     }else if(Tolvas[lista_aux[k-1],1]==0 && Tolvas[lista_aux[k],1]!=0){
      #       k2 <- k
      #       Tolvas[lista_aux[k2],3] <- "truck"
      #       Tolvas[lista_aux[k2],4] <- s
      #       Tolvas[lista_aux[k2],6] <- as.numeric(Tolvas[lista_aux[k2],5])/(input$H.camion[s,1])
      #     }
      # 
      #   }
      # 
      #   # Para ultimo elemento:
      #   k3 <- length(lista_aux)
      #   if(Tolvas[lista_aux[k3],1]!=0){
      #     Tolvas[lista_aux[k3],3] <- "truck"
      #     Tolvas[lista_aux[k3],4] <- s
      #     Tolvas[lista_aux[k3],6] <- as.numeric(Tolvas[lista_aux[k3],5])/(input$H.camion[s,1])
      #   }
      # 
      # 
      # 
      # 
      #   tol.usadas <- 0
      #   # Veamos cuantas Tolvas de cami?n us?
      #   lista_aux2 <- tot.TolvasLib[(tol.lib_ocup+1):tol.ocup_lib]
      #   #for(k in ((min.TolvasLib+tol.lib_ocup):(min.TolvasLib+tol.ocup_lib-1))){
      #   for(k in 1:length(lista_aux2)){
      #     if(Tolvas[lista_aux2[k],3]!=0){ #creo q aqui no hay q poner !=0
      #       tol.usadas <- tol.usadas + 1
      #     }
      #   }
      #   CWTTRP_struct$H.camion_res[s,1:tol.usadas] <- rep(-1,tol.usadas)
      # 
      # 
      #   # Para eliminar ocos intermedios:
      #   res_tol = delete_zeros_tolvas(Tolvas,t)
      #   Tolvas = res_tol$Tolvas
      #   t = res_tol$t
      # 
      #   # Para eliminar ocos intermedios:
      #   res_tol = delete_zeros_tolvas(Tolvas,t)
      #   Tolvas = res_tol$Tolvas
      #   t = res_tol$t
      # 
      #   t <- min(which(Tolvas[,1]==0))
      # 
      # 
      # }
      # else{
      # 
      #   parada <- 2
      # }

    }
    else{
      parada <- 2
    }

  }

  else if(n.trailer_i!=0 && n.trailer_j!=0 && n.trailer_i!=n.trailer_j &&
          sum(CWTTRP_struct$H.trailer_res[n.trailer_i,]==-1)+
          sum(CWTTRP_struct$H.trailer_res[n.trailer_j,]==-1)<=dim(input$H.trailer)[2]){
    
    # aqui lo que tendria que hacer es meter la mercancia de los clientes q estaban 
    # utilizando el trailer "mas libre" en el trailer "mas ocupado" 
    parada <- 1

    #numero de Tolvas libres en n.trailer_i ?necesario?
    tol.lib_i <- sum(CWTTRP_struct$H.trailer_res[n.trailer_i,]!=-1)
    #numero de Tolvas libres en n.trailer_j ?necesario?
    tol.lib_j <- sum(CWTTRP_struct$H.trailer_res[n.trailer_j,]!=-1)

    # es menor que el num de Tolvas ocupadas en n.trailer_j
    if(tol.lib_i < tol.lib_j){
      trailer_ocup <- n.trailer_j #trailer_ocup <- trailer mas ocupado
      trailer_lib <- n.trailer_i #trailer_lib <- trailer mas liberado
    }
    else{
      trailer_ocup <- n.trailer_i
      trailer_lib <- n.trailer_j
    }

    ss.prueba <- trailer_ocup
    
    # estas son las Tolvas libres en el trailer ocupado
    tol.lib_ocup <- sum(CWTTRP_struct$H.trailer_res[ss.prueba,]!=-1)
    # la primera tolva libre del trailer ocupado
    if(tol.lib_ocup>0){
      # la primera tolva libre del trailer ocupado
      ntol.lib_ocup <- min(which(CWTTRP_struct$H.trailer_res[ss.prueba,]!=-1))
    }else{
      ntol.lib_ocup <- 0
    }
    #Tolvas ocupadas en el trailer libre
    tol.ocup_lib <- sum(CWTTRP_struct$H.trailer_res[trailer_lib,]==-1)
    #ultima tolva ocupada en el trailer libre (coincide con lo anterior)
    ntol.ocup_lib <- max(which(CWTTRP_struct$H.trailer_res[trailer_lib,]==-1))

    # "vaciamos" el trailer libre
    CWTTRP_struct$H.trailer_res[trailer_lib,] <-
      rep(input$H.trailer[trailer_lib,1],dim(input$H.trailer)[2])

    # "llenamos" el trailer ocupado
    CWTTRP_struct$H.trailer_res[ss.prueba,ntol.lib_ocup:(ntol.lib_ocup+tol.ocup_lib-1)] <-
      rep(-1,tol.ocup_lib)
    
    for (k in which(Tolvas[,4]==trailer_lib & Tolvas[,3]=="trailer")){
      Tolvas[k,4] <- trailer_ocup
    }
    
    

  }
  result <- list()
  result$CWTTRP_struct <- CWTTRP_struct
  result$Tolvas <- Tolvas
  result$s <- s
  result$t <- t
  result$parada <- parada

  return(result)
}


PVRx2_available_compartments <- function(input, tol.lib_ocup, lista_aux, 
                                         tol.ocup_lib, Tolvas){
  
  available_compartments <- FALSE
  
  available_trailer_hoppers <- tol.lib_ocup
  
  #lista_aux <- tot.TolvasLib[(tol.lib_ocup+1):tol.ocup_lib]
  
  clients <- numeric()
  for(k in 1:length(lista_aux)){
    clients <- c(clients, Tolvas[lista_aux[k],1])
  }
  clients <- as.numeric(clients[!duplicated(clients)])
  
  clients_demands <- list()
  for(kk in 1:length(clients)){
    clients_demands[[kk]] <- input$matriz.demandas[clients[kk]+1,]
  }
  
  needed_trailer_hoppers_per_product <- list()
  for(kkk in 1:length(clients_demands)){
    needed_trailer_hoppers_per_product[[kkk]] <- ceiling(clients_demands[[kkk]]/input$H.trailer[1,1])
    
  }
  
  needed_trailer_hoppers_per_product_res <- needed_trailer_hoppers_per_product
  needed_trailer_hoppers_per_product_res2 <- needed_trailer_hoppers_per_product_res
  
  load_res <- clients_demands
  
  
  for (i in 1:length(needed_trailer_hoppers_per_product)){
    for(j in 1:length(needed_trailer_hoppers_per_product[[i]])){
      if(needed_trailer_hoppers_per_product[[i]][j]!=0){
        while(available_trailer_hoppers>0 & needed_trailer_hoppers_per_product_res2[[i]][j]>0){
          
          needed_trailer_hoppers_per_product_res2[[i]][j] <- max(needed_trailer_hoppers_per_product_res[[i]][j]-available_trailer_hoppers,0 )
          load_res[[i]][j] <- max(0, load_res[[i]][j] - (needed_trailer_hoppers_per_product_res[[i]][j]-needed_trailer_hoppers_per_product_res2[[i]][j])*input$H.trailer[1,1])
          available_trailer_hoppers <- available_trailer_hoppers - (needed_trailer_hoppers_per_product_res[[i]][j] - needed_trailer_hoppers_per_product_res2[[i]][j])
          needed_trailer_hoppers_per_product_res <- needed_trailer_hoppers_per_product_res2
          
        }
      }
    }
  }
  
  
  # Ahora he "consumido" todas las tolvas de trailer, y todavia no he acoplado load_res de mercancia. Tengo que ver si esto me cabe en las tolvas de truck que hay libres
  
  available_truck_hoppers <- dim(input$H.camion)[2]
  
  
  needed_truck_hoppers_per_product <- list()
  for(kkk in 1:length(load_res)){
    needed_truck_hoppers_per_product[[kkk]] <- ceiling(load_res[[kkk]]/input$H.camion[1,1])
    
  }
  
  needed_truck_hoppers_total <- sum(unlist(needed_truck_hoppers_per_product))
  
  
  
  if(needed_truck_hoppers_total <=  available_truck_hoppers){
    #print(paste0( available_trailer_hoppers , " " ,(dim(input$H.trailer)[2])))
    available_compartments <- TRUE
  }
  
  
  
  return(list(available_compartments=available_compartments, trailer_hoppers=needed_trailer_hoppers_per_product, truck_hoppers=needed_truck_hoppers_per_product)) 
}


PVRx2_union <- function(input, tol.lib_ocup, lista_aux, Tolvas, trailer_ocup, s, needed_trailer_hoppers_per_product,
                        needed_truck_hoppers_per_product){
  
  
  available_trailer_hoppers <- tol.lib_ocup
  
  
  clients <- numeric()
  for(k in 1:length(lista_aux)){
    clients <- c(clients, Tolvas[lista_aux[k],1])
  }
  clients <- as.numeric(clients[!duplicated(clients)])
  
  
  clients_demands <- list()
  for(kk in 1:length(clients)){
    clients_demands[[kk]] <- input$matriz.demandas[clients[kk]+1,]
  }  
  
  clients_demands_res <- clients_demands
  
  capacity_trailer_hoppers <- input$H.trailer[1,1]
  new_hoppers_trailers <- list()
  t <- 1
  
  for(i in 1:length(clients_demands)){
    for (f in 1:length(clients_demands[[i]])){
      if(needed_trailer_hoppers_per_product[[i]][f]!=0){
        while(clients_demands_res[[i]][f]>0 & available_trailer_hoppers > 0){
          quantity <- min(clients_demands_res[[i]][f], capacity_trailer_hoppers)
          new_hoppers_trailers[[t]] <- data.frame(clients[i], f, "trailer", trailer_ocup, quantity, quantity/capacity_trailer_hoppers)
          colnames(new_hoppers_trailers[[t]]) <- c("Cliente", "Pienso", "Tipo_vehiculo", "numero vehiculo", "Cantidad", "Proporcion")
          clients_demands_res[[i]][f] <- clients_demands_res[[i]][f] - quantity
          available_trailer_hoppers <- available_trailer_hoppers - 1
          t <- t + 1
        }
      }
    }
  }
  
  # Ya he llenado las tolvas de trailer. Ahora vamos con las de truck
  available_truck_hoppers <- dim(input$H.camion)[2]
  capacity_truck_hoppers <-  input$H.camion[1,1]
  new_hoppers_trucks <- list()
  tt <- 1
  
  for (i in 1:length(clients_demands)){
    for (f in 1:length(clients_demands[[i]])){
      if(needed_truck_hoppers_per_product[[i]][f]!=0){
        while(clients_demands_res[[i]][f]>0 & available_truck_hoppers > 0){
          quantity <- min(clients_demands_res[[i]][f], capacity_truck_hoppers)
          new_hoppers_trucks[[tt]] <-  data.frame(clients[i], f, "truck", s, quantity, quantity/capacity_truck_hoppers) 
          colnames(new_hoppers_trucks[[tt]]) <- c("Cliente", "Pienso", "Tipo_vehiculo", "numero vehiculo", "Cantidad", "Proporcion")
          clients_demands_res[[i]][f] <- clients_demands_res[[i]][f] - quantity
          available_truck_hoppers <- available_truck_hoppers - 1
          tt <- tt + 1
        }
      }
    }
  }
  
  
  Tolvas_aux <- Tolvas
  
  T_index <- min(which(Tolvas_aux[,1]==0))
  if(length(new_hoppers_trailers)!=0){
    for(k in 1:length(new_hoppers_trailers)){
      Tolvas_aux[T_index,] <- new_hoppers_trailers[[k]]
      Tolvas_aux[T_index,3] <- "trailer"
      T_index <- T_index + 1
    }
  }
  
  if(length(new_hoppers_trucks)!=0){
    for(kk in 1:length(new_hoppers_trucks)){
      Tolvas_aux[T_index,] <- new_hoppers_trucks[[kk]]
      Tolvas_aux[T_index,3] <- "truck"
      T_index <- T_index + 1
    }
  }
  
  Tolvas_aux[lista_aux,] <- matrix(0,nrow=length(lista_aux), ncol=dim(Tolvas)[2])
  
  res_tol = delete_zeros_tolvas(Tolvas_aux,T_index)
  Tolvas_aux = res_tol$Tolvas
  T_index = res_tol$t
  
  
  
  
  return(list(Tolvas_aux = Tolvas_aux, T_index = T_index, load_trailer_hoppers = length(new_hoppers_trailers), load_truck_hoppers = length(new_hoppers_trucks)))
}




SmInS_VcVc_control_fusion<-function(CWTTRP_struct, Tolvas, R,
                                    input, n.trailer_i, n.truck_i,
                                    n.trailer_j, n.truck_j, z,
                                    flag_stop, pos, verbose){

  
  # If we have a PTR and trailer route, the fusion is not possible

  if( (n.truck_i!=0 || n.truck_j!=0) && n.truck_i*n.truck_j==0 &&
      (n.trailer_i!=0 || n.trailer_j!=0) && n.trailer_i*n.trailer_j==0 && z>0){

    flag_stop <- 2

  }


  # If client i is in PTR, and client j is in CTR, they can not merge.

  if( n.truck_i!=0 && n.trailer_i==0 && z>0 && n.truck_j!=0 &&
      n.trailer_j!=0 && n.truck_i!=n.truck_j){

    flag_stop <- 2

  }

  # If client j is in PTR, and client i is in CTR, they can not merge.

  if( n.truck_j!=0 && n.trailer_j==0 && z>0 &&
      n.truck_i!=0 && n.trailer_i!=0 && n.truck_i!=n.truck_j){

    flag_stop <- 2

  }

  # The 2 clients are in PTR.

  if(n.truck_i!=0 && n.truck_j!=0 && n.truck_i!=n.truck_j &&
     n.trailer_i==0 && n.trailer_j==0 &&
     sum(CWTTRP_struct$H.camion_res[n.truck_i,]==-1)+
     sum(CWTTRP_struct$H.camion_res[n.truck_j,]==-1)>dim(input$H.camion)[2]){

    # there is not space in hoppers
    flag_stop <- 2

  }

  else if(n.truck_i!=0 && n.truck_j!=0 && n.truck_i!=n.truck_j &&
          n.trailer_i==0 && n.trailer_j==0 &&
          sum(CWTTRP_struct$H.camion_res[n.truck_i,]==-1)+
          sum(CWTTRP_struct$H.camion_res[n.truck_j,]==-1)<=dim(input$H.camion)[2]){

    flag_stop <- 1

    # the algorithm puts the elements of less ocuppied truck, in the most occupied.

    tol.lib_i <- sum(CWTTRP_struct$H.camion_res[n.truck_i,]!=-1)
    tol.lib_j <- sum(CWTTRP_struct$H.camion_res[n.truck_j,]!=-1)

    # si el num de Tolvas ocupadas en n.truck_i
    if(sum(CWTTRP_struct$H.camion_res[n.truck_i,]==-1)<
       sum(CWTTRP_struct$H.camion_res[n.truck_j,]==-1)){
      # es menor que el num de Tolvas ocupadas en n.truck_j
      truck_ocup <- n.truck_j #truck_ocup <- truck mas ocupado
      truck_lib <- n.truck_i #truck_lib <- truck mas liberado

    }else{
      truck_ocup <- n.truck_i
      truck_lib <- n.truck_j}

    s.prueba <- truck_ocup
    # estas son las Tolvas libres en el truck ocupado
    tol.lib_ocup <- sum(CWTTRP_struct$H.camion_res[s.prueba,]!=-1)
    # la primera tolva libre del truck ocupado
    # ntol.lib_ocup <- min(which(CWTTRP_struct$H.camion_res[s.prueba,]!=-1))
    if(sum(CWTTRP_struct$H.camion_res[s.prueba,]!=-1)>0){
      # la primera tolva libre del trailer ocupado
      ntol.lib_ocup <- min(which(CWTTRP_struct$H.camion_res[s.prueba,]!=-1))
    }else{
      ntol.lib_ocup <- 0
    }
    #Tolvas ocupadas en el truck libre
    tol.ocup_lib <- sum(CWTTRP_struct$H.camion_res[truck_lib,]==-1)
    #ultima tolva ocupada en el truck libre (coincide con lo anterior)
    ntol.ocup_lib <- max(which(CWTTRP_struct$H.camion_res[truck_lib,]==-1))

    # let's rewrite Tolvas matrix and H.camion_res

    CWTTRP_struct$H.camion_res[truck_lib,] <- rep(input$H.camion[truck_lib,1],dim(input$H.camion)[2])
    #CWTTRP_struct$H.camion_res[s.prueba,ntol.lib_ocup:dim(input$H.camion)[2]] <- rep(-1,tol.lib_ocup)
    CWTTRP_struct$H.camion_res[s.prueba,ntol.lib_ocup:(ntol.lib_ocup+tol.ocup_lib-1)] <- rep(-1,tol.ocup_lib)

    #primera tolva en la matriz Tolvas q estaba siendo usada por el truck_lib
    min.TolvasLib <- min(which(Tolvas[,4]==truck_lib))
    # as Tolvas q usa o truck_lib
    tot.TolvasLib <- which(Tolvas[,4]==truck_lib & Tolvas[,3]=="truck")

    # for (k in min.TolvasLib:(min.TolvasLib+tol.ocup_libre-1)){
    #   Tolvas[k,4] <- truck_ocup
    # }

    for (kk in 1:length(tot.TolvasLib)){
      Tolvas[tot.TolvasLib[kk],4]  <- truck_ocup
    }


  }

  # the clients are in the same route: stop the merge.

  if(n.truck_i==n.truck_j && n.trailer_i==n.trailer_j && n.truck_i!=0 && n.trailer_i!=0){
    flag_stop <- 2
  }

  result <- list()
  result$CWTTRP_struct <- CWTTRP_struct
  result$Tolvas <- Tolvas
  result$flag_stop <- flag_stop

  return(result)
}

SmInS_VcVc_apply_fusion_with_hoppers<-function(CWTTRP_struct, Tolvas, R, Rhat, S,
                                               Shat, input, n.truck_i, n.trailer_i,
                                               n.truck_j, n.trailer_j, pos, n,
                                               z, x, a, b, t,
                                               s, tc, tt, ss, nf, flag_stop, verbose) {
  
  merge <- 0
  if( flag_stop == 0 ){
    
    if( z == 0 ){

      if(CWTTRP_struct$CargaT<=input$capacidad.vehiculo && x==0){

        if( a > 0 || b > 0){

          if(n.trailer_i!=0){ss <- n.trailer_i; ss <- as.numeric(ss)}
          if(n.trailer_j!=0){ss <- n.trailer_j; ss <- as.numeric(ss)}
          if(n.truck_i!=0){s <- n.truck_i; s <- as.numeric(s)}
          if(n.truck_j!=0){s <- n.truck_j; s <- as.numeric(s)}
          
          if( (n.trailer_i!=0 || n.trailer_j!=0) &&
              n.trailer_i*n.trailer_j==0 &&
              n.truck_i==0 && n.truck_j==0) {
            s <- ss##min(which(CWTTRP_struct$H.camion_res[,1]!=-1))
            s <- as.numeric(s)
          }

        }
        else {
          
          s.aux <- s
          s.aux <- as.numeric(s.aux)
          ss.aux <- ss
          ss.aux <- as.numeric(ss.aux)
          
          if((sum(Tolvas[,1]==(pos$Positionfilas-1))==0)&&
             (sum(Tolvas[,1]==(pos$Positioncolumnas-1))==0)){
            
            for (i in 1:length(CWTTRP_struct$H.trailer_res[,1])) {
              if ((sum(CWTTRP_struct$H.trailer_res[i,] == -1)  == 0) &&
                  (sum(CWTTRP_struct$H.camion_res[i,] == -1)  == 0)) {
                ss <- i
                s <- i
                break;
              }
            }
          }
          
          if(sum(Tolvas[,1]==(pos$Positionfilas-1))>0){
            
            CWTTRP_struct$aux <- max(which(Tolvas[,1]==(pos$Positionfilas-1)))
            if (Tolvas[CWTTRP_struct$aux,3]=="trailer"){
              ss <- Tolvas[CWTTRP_struct$aux,4]
              ss <- as.numeric(ss)
            }else{
              s <- Tolvas[CWTTRP_struct$aux,4]
              s <- as.numeric(s)
            }
          }

          if(sum(Tolvas[,1]==(pos$Positioncolumnas-1))>0){
            
            CWTTRP_struct$aux1 <- min(which(Tolvas[,1]==(pos$Positioncolumnas-1)))
            CWTTRP_struct$aux2 <- max(which(Tolvas[,1]==(pos$Positioncolumnas-1)))
            if(Tolvas[CWTTRP_struct$aux2,3]=="trailer"){
              ss <- Tolvas[CWTTRP_struct$aux2,4]
              ss <- as.numeric(ss)
            }
            else{
              s <- Tolvas[CWTTRP_struct$aux2,4]
              s <- as.numeric(s)
            }
            if(Tolvas[CWTTRP_struct$aux1,3]=="trailer"){
              ss <- Tolvas[CWTTRP_struct$aux1,4]
              ss <- as.numeric(ss)
            }
            else{
              s <- Tolvas[CWTTRP_struct$aux1,4]
              s <- as.numeric(s)
            }
          }

          if(sum(Tolvas[,1]==(pos$Positionfilas-1))==0 &&
             sum(Tolvas[,1]==(pos$Positioncolumnas-1))==0 &&
             sum(CWTTRP_struct$H.trailer_res[ss.aux,])!=sum(input$H.trailer[ss.aux,]) &&
             ss == ss.aux){

            ss <- min(which(CWTTRP_struct$H.trailer_res[,1]!=-1))
            ss <- as.numeric(ss)

          }

          if(sum(Tolvas[,1]==(pos$Positionfilas-1))==0 &&
             sum(Tolvas[,1]==(pos$Positioncolumnas-1))==0  &&
             sum(CWTTRP_struct$H.camion_res[s.aux,])!=sum(input$H.camion[s.aux,]) &&
             s == s.aux){

            s <- min(which(CWTTRP_struct$H.camion_res[,1]!=-1))
            s <- as.numeric(s)
          }

        }

        # En este momento ya tengo decidido cual es el camion y cual es el
        # trailer q voy a utilizar. Entonces aqui deberia tambien "actualizar"
        # los valores tt y tc.
        if(sum(CWTTRP_struct$H.trailer_res[ss,])!=-dim(input$H.trailer)[2] && ss!=0){
          tt <- min(which(CWTTRP_struct$H.trailer_res[ss,]!=-1))
        }
        
        if(sum(CWTTRP_struct$H.camion_res[s,])!=-dim(input$H.camion)[2] && s!=0){
          tc <- min(which(CWTTRP_struct$H.camion_res[s,]!=-1))
        }

        #recorremos todos los tipos de pienso
        for (i in 1:nf){
          while(CWTTRP_struct$demandas_res[pos$Positionfilas,i]!=0 &&
                sum(CWTTRP_struct$H.trailer_res[ss,])!=-dim(input$H.trailer)[2] ){
            # para el primer cliente
            #if(sum(CWTTRP_struct$H.trailer_res[ss,])!=-dim(input$H.trailer)[2]){ # comenzamos dandole preferencia al llenado del trailer, al ser v.c.
            while (CWTTRP_struct$demandas_res[pos$Positionfilas,i]>max(CWTTRP_struct$H.trailer_res) && sum(CWTTRP_struct$H.trailer_res[ss,])!=-dim(input$H.trailer)[2]){   #(al ser homogeneo da igual q ponga el max o no)
              # Si esto se cumple, entonces la demanda de pienso i para este cliente no cabe en ninguna tolva, luego relleno una de ellas y el resto
              # lo meto en otra (aqui al tener todas las Tolvas la misma capacidad, el problema se simplifica)
              if(t>2 && Tolvas[t-1,1]==0){
                t <- min(which(Tolvas[,1]==0))
              }
              Tolvas[t,] <- c(pos$Positionfilas-1,i,"trailer",ss,max(input$H.trailer),1)
              CWTTRP_struct$demandas_res[pos$Positionfilas,i] <- CWTTRP_struct$demandas_res[pos$Positionfilas,i] - max(input$H.trailer)

              CWTTRP_struct$H.trailer_res[ss,tt] <- -1 # este s lo voy a tener que cambiar en algun momento

              if (tt < dim(input$H.trailer)[2]){
                tt <- tt+1
              }else{tt <- 1}

              t <- t+1
            }

            if(sum(CWTTRP_struct$H.trailer_res[ss,])!= -dim(CWTTRP_struct$H.trailer_res)[2]){
              if(t>2 && Tolvas[t-1,1]==0){
                t <- min(which(Tolvas[,1]==0))
              }
              Tolvas[t,] <- c(pos$Positionfilas-1,i,"trailer",ss,CWTTRP_struct$demandas_res[pos$Positionfilas,i],CWTTRP_struct$demandas_res[pos$Positionfilas,i]/max(input$H.trailer))
              CWTTRP_struct$demandas_res[pos$Positionfilas,i] <- max(0,CWTTRP_struct$demandas_res[pos$Positionfilas,i] - max(input$H.trailer))
              CWTTRP_struct$H.trailer_res[ss,tt] <- -1 # este ss lo voy a tener que cambiar en algun momento

              if (tt < dim(input$H.trailer)[2]){
                tt <- tt+1
              }else{tt <- 1}
              t <- t+1
            }
          }
          while(CWTTRP_struct$demandas_res[pos$Positionfilas,i]!=0 && sum(CWTTRP_struct$H.camion_res[s,])!=-dim(input$H.camion)[2]){
            while(CWTTRP_struct$demandas_res[pos$Positionfilas,i]>max(CWTTRP_struct$H.camion_res) && sum(CWTTRP_struct$H.camion_res[s,])!=-dim(input$H.camion)[2]){
              # es decir, si nos encontramos en la situacion de que el cliente en cuestion todavia tiene demanda sin repartir, y el
              # trailer que hemos estado considerando ya esta lleno, miramos si existe algun camion q le podamos acoplar para seguir
              # metiendo en el la carga de los clientes q hay en esta ruta
              if(Tolvas[t-1,1]==0){
                t <- min(which(Tolvas[,1]==0))
              }

              Tolvas[t,] <- c(pos$Positionfilas-1,i,"truck",s,max(input$H.camion),1)
              CWTTRP_struct$demandas_res[pos$Positionfilas,i] <- CWTTRP_struct$demandas_res[pos$Positionfilas,i] - max(input$H.camion)
              CWTTRP_struct$H.camion_res[s,tc] <- -1

              if (tc < dim(input$H.camion)[2]){
                tc <- tc+1
              }else{tc <- 1}

              t <- t+1

            }

            if(sum(CWTTRP_struct$H.camion_res[s,])!=-dim(input$H.camion)[2] ){

              if(Tolvas[t-1,1]==0){
                t <- min(which(Tolvas[,1]==0))
              }


              Tolvas[t,] <- c(pos$Positionfilas-1,i,"truck",s,
                              CWTTRP_struct$demandas_res[pos$Positionfilas,i],
                              CWTTRP_struct$demandas_res[pos$Positionfilas,i]/max(input$H.camion))
              CWTTRP_struct$demandas_res[pos$Positionfilas,i] <- max(0,
                                                                     CWTTRP_struct$demandas_res[pos$Positionfilas,i] -
                                                                       max(input$H.camion))
              CWTTRP_struct$H.camion_res[s,tc] <- -1

              if (tc < dim(input$H.camion)[2]){
                tc <- tc+1
              }
              else {
                tc <- 1
              }

              t <- t+1

            }

          }

        }# aqui acaba el bucle "for (i in 1:nf)" para el cliente t.c.


        for (i in 1:nf){    #recorremos todos los tipos de pienso
          while(CWTTRP_struct$demandas_res[pos$Positioncolumnas,i]!=0 && sum(CWTTRP_struct$H.trailer_res[ss,])!=-dim(input$H.trailer)[2] ){
            while (CWTTRP_struct$demandas_res[pos$Positioncolumnas,i]>max(CWTTRP_struct$H.trailer_res) && sum(CWTTRP_struct$H.trailer_res[ss,])!=-dim(input$H.trailer)[2]){   #(al ser homogeneo da igual q ponga el max o no)
              # Si esto se cumple, entonces la demanda de pienso i para este cliente no cabe en ninguna tolva, luego relleno una de ellas y el resto
              # lo meto en otra (aqui al tener todas las Tolvas la misma capacidad, el problema se simplifica)

              Tolvas[t,] <- c(pos$Positioncolumnas-1,i,"trailer",ss,max(input$H.trailer),1)
              CWTTRP_struct$demandas_res[pos$Positioncolumnas,i] <- CWTTRP_struct$demandas_res[pos$Positioncolumnas,i] - max(input$H.trailer)
              CWTTRP_struct$H.trailer_res[ss,tt] <- -1 # este s lo voy a tener que cambiar en algun momento

              if (tt < dim(input$H.trailer)[2]){
                tt <- tt+1
              }else{tt <- 1}

              t <- t+1
            }

            if(sum(CWTTRP_struct$H.trailer_res[ss,])!= -dim(CWTTRP_struct$H.trailer_res)[2]){
              Tolvas[t,] <- c(pos$Positioncolumnas-1,i,"trailer",ss,CWTTRP_struct$demandas_res[pos$Positioncolumnas,i],CWTTRP_struct$demandas_res[pos$Positioncolumnas,i]/max(input$H.trailer))
              CWTTRP_struct$demandas_res[pos$Positioncolumnas,i] <- max(0,CWTTRP_struct$demandas_res[pos$Positioncolumnas,i] - max(input$H.trailer))
              CWTTRP_struct$H.trailer_res[ss,tt] <- -1

              if (tt < dim(input$H.trailer)[2]){
                tt <- tt+1
              }else{tt <- 1}
              t <- t+1
            }


          }
          while(sum(CWTTRP_struct$H.camion_res[s,])!=-dim(input$H.camion)[2] && CWTTRP_struct$demandas_res[pos$Positioncolumnas,i]!=0){
            while(CWTTRP_struct$demandas_res[pos$Positioncolumnas,i]>max(CWTTRP_struct$H.camion_res) && sum(CWTTRP_struct$H.camion_res[s,])!=-dim(input$H.camion)[2]){
              # es decir, si nos encontramos en la situacion de que el cliente en cuestion todavia tiene demanda sin repartir, y el
              # trailer que hemos estado considerando ya esta lleno, miramos si existe algun camion q le podamos acoplar para seguir
              # metiendo en el la carga de los clientes q hay en esta ruta
              if(Tolvas[t-1,1]==0){
                t <- min(which(Tolvas[,1]==0))
              }
              Tolvas[t,] <- c(pos$Positioncolumnas-1,i,"truck",s,max(input$H.camion),1)
              CWTTRP_struct$demandas_res[pos$Positioncolumnas,i] <- CWTTRP_struct$demandas_res[pos$Positioncolumnas,i] - max(input$H.camion)
              CWTTRP_struct$H.camion_res[s,tc] <- -1

              if (tc < dim(input$H.camion)[2]){
                tc <- tc+1
              }else{tc <- 1}

              t <- t+1

            }

            if(sum(CWTTRP_struct$H.camion_res[s,])!=-dim(input$H.camion)[2] ){

              if(Tolvas[t-1,1]==0){
                t <- min(which(Tolvas[,1]==0))
              }
              Tolvas[t,] <- c(pos$Positioncolumnas-1,i,"truck",s,CWTTRP_struct$demandas_res[pos$Positioncolumnas,i],CWTTRP_struct$demandas_res[pos$Positioncolumnas,i]/max(input$H.camion))
              CWTTRP_struct$demandas_res[pos$Positioncolumnas,i] <- max(0,CWTTRP_struct$demandas_res[pos$Positioncolumnas,i] - max(input$H.camion))
              CWTTRP_struct$H.camion_res[s,tc] <- -1

              if (tc < dim(input$H.camion)[2]){
                tc <- tc+1
              }else{tc <- 1}

              t <- t + 1

            }
          }

        }# aqui acaba el bucle for


        # [Martes 12/11/2019, 18:12h, acabo de sacar esto para fuera del bucle for(i in 1:nf), espero no liarla]:

        if (sum(CWTTRP_struct$H.trailer_res[ss,])==-dim(input$H.trailer)[2] && ss <= dim(input$H.trailer)[1]){
          ss <- ss+1; ss <- as.numeric(ss)
        }else if(ss > dim(input$H.trailer)[1]){
          ss <- 1; ss <- as.numeric(ss)
        }

        if (sum(CWTTRP_struct$H.camion_res[s,])==-dim(input$H.camion)[2] && s <= dim(input$H.camion)[1]){
          s <- s+1; s <- as.numeric(s)
        }else if(s > dim(input$H.camion)[1]){
          s <- 1; s <- as.numeric(s)
        }



      }
      else{ # si no se verifica que CargaT<=... y x==0
        S[pos$Positionfilas,pos$Positioncolumnas]<-0 #Si no es factible tambien lo borramos
        Shat[pos$Positionfilas,pos$Positioncolumnas]<-0
      }

    }
    else{ # este else hace referencia a que z!=0, y por tanto en la ruta principal hay algun cliente de tipo
      # t.c. En tal caso, debemos construir una ruta de camion
      if(CWTTRP_struct$CargaT<=input$capacidad.truck && x==0){

        # Primero, elegimos el camion, que dependera si los clientes q estoy considerando han sido ya incluidos en una
        # ruta o no
        if(sum(Tolvas[,1]==(pos$Positionfilas-1))>0){
          CWTTRP_struct$aux <- max(which(Tolvas[,1]==(pos$Positionfilas-1)))
          if (Tolvas[CWTTRP_struct$aux,3]=="trailer"){
            ss <- Tolvas[CWTTRP_struct$aux,4]
            ss <- as.numeric(ss)
          }else{
            s <- Tolvas[CWTTRP_struct$aux,4]
            s <- as.numeric(s)
          }
        }

        # Lo mismo para el cliente j. Vemos si ya habia sido incluido en alguna ruta con anterioridad. Y en tal
        # caso, miramos de que

        if(sum(Tolvas[,1]==(pos$Positioncolumnas-1))>0){
          CWTTRP_struct$aux2 <- max(which(Tolvas[,1]==(pos$Positioncolumnas-1)))
          if(Tolvas[CWTTRP_struct$aux2,3]=="trailer"){
            ss <- Tolvas[CWTTRP_struct$aux2,4]
            ss <- as.numeric(ss)
          }else{
            s <- Tolvas[CWTTRP_struct$aux2,4]
            s <- as.numeric(s)
          }
        }



        # En este momento ya tengo decidido cual es el camion y cual es el trailer q voy a utilizar. Entonces aqui
        # deberia tambien "actualizar" los valores tt y tc.
        if(sum(CWTTRP_struct$H.trailer_res[ss,])!=-dim(input$H.trailer)[2] && ss!=0){
          tt <- min(which(CWTTRP_struct$H.trailer_res[ss,]!=-1))
        }

        if(sum(CWTTRP_struct$H.camion_res[s,])!=-dim(input$H.camion)[2] && s!=0){
          tc <- min(which(CWTTRP_struct$H.camion_res[s,]!=-1))
        }

        #recorremos todos los tipos de pienso
        for (i in 1:nf){
          while(CWTTRP_struct$demandas_res[pos$Positionfilas,i]!=0 && sum(CWTTRP_struct$H.camion_res[s,])!=-dim(input$H.camion)[2] ){
            while (CWTTRP_struct$demandas_res[pos$Positionfilas,i]>max(CWTTRP_struct$H.camion_res) && sum(CWTTRP_struct$H.camion_res[s,])!=-dim(input$H.camion)[2]){   #(al ser homogeneo da igual q ponga el max o no)
              # Si esto se cumple, entonces la demanda de pienso i para este cliente no cabe en ninguna tolva, luego relleno una de ellas y el resto
              # lo meto en otra (aqui al tener todas las Tolvas la misma capacidad, el problema se simplifica)
              if(Tolvas[t-1,1]==0 && t>2){
                t <- min(which(Tolvas[,1]==0))
              }
              Tolvas[t,] <- c(pos$Positionfilas-1,i,"truck",s,max(input$H.camion),1)
              CWTTRP_struct$demandas_res[pos$Positionfilas,i] <- CWTTRP_struct$demandas_res[pos$Positionfilas,i] - max(input$H.camion)
              CWTTRP_struct$H.camion_res[s,tc] <- -1 # este s lo voy a tener que cambiar en algun momento

              if (tc < dim(input$H.camion)[2]){
                tc <- tc+1
              }else{tc <- 1}

              t <- t+1
            }
            # al llegar aqui sabemos que lo que falta de mercancia para este cliente
            # no cabe en una unica tolva; debo dejar claro que tc es como maximo 3
            # (ya que es el numero de Tolvas de mis camiones)

            if(sum(CWTTRP_struct$H.camion_res[s,])!=-dim(input$H.camion)[2]){
              if(Tolvas[t-1,1]==0 && t>2){
                t <- min(which(Tolvas[,1]==0))
              }
              Tolvas[t,] <- c(pos$Positionfilas-1,i,"truck",s,CWTTRP_struct$demandas_res[pos$Positionfilas,i],CWTTRP_struct$demandas_res[pos$Positionfilas,i]/max(input$H.camion))
              # pongo max(input$H.camion) porque al ser homogenea me da igual

              CWTTRP_struct$demandas_res[pos$Positionfilas,i] <- max(0,CWTTRP_struct$demandas_res[pos$Positionfilas,i] - max(input$H.camion))
              # a lo anterior le puedo poner 0 directamente creo

              CWTTRP_struct$H.camion_res[s,tc] <- -1 # este s lo voy a tener que cambiar en algun momento

              if (tc < dim(input$H.camion)[2]){
                tc <- tc+1
              }else{tc <- 1}
              t <- t+1
            }
          }

          # tambien tengo que indicar en algun momento que, aunque no haya rellenado este camion, tambien tengo q
          # pasar a otro cuando cambio de ruta

        }# aqui acaba el bucle "for (i in 1:nf)" para el cliente t.c.

        # Para el otro cliente:
        for (i in 1:nf){    #recorremos todos los tipos de pienso
          while(CWTTRP_struct$demandas_res[pos$Positioncolumnas,i]!=0 && sum(CWTTRP_struct$H.camion_res[s,])!=-dim(input$H.camion)[2]){
            while (CWTTRP_struct$demandas_res[pos$Positioncolumnas,i]>max(CWTTRP_struct$H.camion_res) && sum(CWTTRP_struct$H.camion_res[s,])!=-dim(input$H.camion)[2]){
              # Si esto se cumple, entonces la demanda de pienso i para este cliente no cabe en ninguna tolva, luego relleno una de ellas y el resto
              # lo meto en otra (aqui al tener todas las Tolvas la misma capacidad, el problema se simplifica)
              if(Tolvas[t-1,1]==0 && t>2){
                t <- min(which(Tolvas[,1]==0))
              }
              Tolvas[t,] <- c(pos$Positioncolumnas-1,i,"truck",s,max(input$H.camion),1)
              CWTTRP_struct$demandas_res[pos$Positioncolumnas,i] <- CWTTRP_struct$demandas_res[pos$Positioncolumnas,i] - max(input$H.camion)
              CWTTRP_struct$H.camion_res[s,tc] <- -1 # este s lo voy a tener que cambiar en algun momento

              if (tc < dim(input$H.camion)[2]){
                tc <- tc+1
              }else{tc <- 1}

              t <- t+1
            }

            if(sum(CWTTRP_struct$H.camion_res[s,])!=-dim(input$H.camion)[2]){
              if(Tolvas[t-1,1]==0 && t>2){
                t <- min(which(Tolvas[,1]==0))
              }
              Tolvas[t,] <- c(pos$Positioncolumnas-1,i,"truck",s,CWTTRP_struct$demandas_res[pos$Positioncolumnas,i],CWTTRP_struct$demandas_res[pos$Positioncolumnas,i]/max(input$H.camion))
              CWTTRP_struct$demandas_res[pos$Positioncolumnas,i] <- max(0,CWTTRP_struct$demandas_res[pos$Positioncolumnas,i] - max(input$H.camion))
              CWTTRP_struct$H.camion_res[s,tc] <- -1

              if (tc < dim(input$H.camion)[2]){
                tc <- tc+1
              }else{tc <- 1}
              t <- t+1

            }
          }
        }# aqui acaba el bucle "for (i in 1:nf)"


        if (sum(CWTTRP_struct$H.camion_res[s,])==-dim(input$H.camion)[2] && s!=0){
          s <- s+1; s <- as.numeric(s)
        } # Si ya he rellenado este camion, paso a otro


      }else{
        S[pos$Positionfilas,pos$Positioncolumnas]<-0 #Si no es factible tambien lo borramos
        Shat[pos$Positionfilas,pos$Positioncolumnas]<-0
      }
    }
  }

  # Llegados a este punto tengo las Tolvas rellenas siempre que las limitaciones de
  # capacidad me hayan permitido llegar .
  # Ahora vamos a ver si se puede efectuar la fusion y, en tal caso, llevarla a cabo.
  # Si no, "deshacer" lo hecho


  if(flag_stop == 0 || flag_stop == 1){
    if (z==0){
      
      if(CWTTRP_struct$CargaT<=input$capacidad.vehiculo && x==0){ #Anhadimos la ruta si es factible
      
        if(sum(CWTTRP_struct$demandas_res[pos$Positionfilas,])+
          (sum(CWTTRP_struct$demandas_res[pos$Positioncolumnas,]))==0){ # efectuamos la fusion
          merge <- 1
          R[pos$Positionfilas,3]<-(pos$Positioncolumnas-1)
          R[pos$Positioncolumnas,1]<-(pos$Positionfilas-1)

          # Debug Output
          if (verbose == 1) {
            print(paste("Iteration ", CWTTRP_struct$iter))
            print(paste("Seleccionamos ahorros ", pos$Positionfilas, "-", pos$Positioncolumnas))
            print(R)
            print(paste("Carga ", CWTTRP_struct$CargaT, "capacidad.vehiculo", input$capacidad.vehiculo[1]))
            print("Matriz de Tolvas")
            print(Tolvas)
          }

          #Borramos ahorros utilizados para evitar ciclos
          S[pos$Positionfilas,pos$Positioncolumnas]<-0
          S[pos$Positioncolumnas,pos$Positionfilas]<-0
          Shat[pos$Positionfilas,pos$Positioncolumnas]<-0
          #Borramos ahorros analogos en la matriz Shat,
          # porque obviamente no se puede implementar esta ruta
          Shat[pos$Positioncolumnas,pos$Positionfilas]<-0


        }
        else{
          
          #si no se han encontrado Tolvas para servir a los clientes, "deshago"
          flag_stop_i <- 0
          flag_stop_j <- 0
          if (sum(CWTTRP_struct$demandas_res[pos$Positionfilas,])!=0){
            flag_stop_i <- 2
          }
          if (sum(CWTTRP_struct$demandas_res[pos$Positioncolumnas,])!=0){
            flag_stop_j <- 2
          }


          # Inicializamos a 0 el numero de Tolvas que vamos a borrar
          delete.tol <- 0

          # Si el cliente i no habia sido introducido en ninguna ruta anteriormente
          # (es decir, en alguna iteraci?n anterior), entonces n.trailer_i=0 y n.truck_i=0,
          # de modo que deber?amos deshacer lo que se haya hecho en esta iteracion
          # con respecto a este cliente (pues no hemos sido capaces de servir a los
          # dos clientes)
          if(n.trailer_i==0 && n.truck_i==0 && flag_stop_i == 2){
            CWTTRP_struct$demandas_res[pos$Positionfilas,] <- input$matriz.demandas[pos$Positionfilas,]
            if (sum(Tolvas[,1]==pos$Positionfilas-1)>0){
              for (k in which(Tolvas[,1]==pos$Positionfilas-1)){
                delete.tol <- delete.tol + 1
                Tolvas[k,] <- numeric(dim(Tolvas)[2])}
            }
          }

          # Si el cliente j no habia sido introducido en ninguna ruta anteriormente
          # (es decir, en alguna iteraci?n anterior), entonces n.trailer_j=0 y n.truck_j=0,
          # de modo que deber?amos deshacer lo que se haya hecho en esta iteracion con
          # respecto a este cliente (pues no hemos sido capaces de servir a los dos clientes)
          if(n.trailer_j==0 && n.truck_j==0 && flag_stop_j == 2){
            CWTTRP_struct$demandas_res[pos$Positioncolumnas,] <- input$matriz.demandas[pos$Positioncolumnas,]
            if (sum(Tolvas[,1]==pos$Positioncolumnas-1)>0){
              for (k in which(Tolvas[,1]==pos$Positioncolumnas-1)){
                delete.tol <- delete.tol + 1
                Tolvas[k,] <- numeric(dim(Tolvas)[2])}
            }
          }

          # Para eliminar ocos intermedios:
          res_tol = delete_zeros_tolvas(Tolvas,t)
          Tolvas = res_tol$Tolvas
          t = res_tol$t

          # Para eliminar ocos intermedios:
          res_tol = delete_zeros_tolvas(Tolvas,t)
          Tolvas = res_tol$Tolvas
          t = res_tol$t

          t <- min(which(Tolvas[,1]==0))

          # Pongo s-1 porq si hemos llegado hasta aqu? seguramente hemos hecho en algun momento s<-s+1
          if(delete.tol>0){
            CWTTRP_struct$H.camion_res[s-1,] <-
              c(CWTTRP_struct$H.camion_res[s-1,1:(dim(input$H.camion)[2]-delete.tol)],
                input$H.camion[s-1,(dim(input$H.camion)[2]-delete.tol+1):dim(input$H.camion)[2]])
          }

          #Si no es factible tambien lo borramos
          S[pos$Positionfilas,pos$Positioncolumnas]<-0
          Shat[pos$Positionfilas,pos$Positioncolumnas]<-0

        }
      }

    }
    else{
      #Anhadimos la ruta si es factible
      if(CWTTRP_struct$CargaT<=input$capacidad.truck && x==0){
        if((sum(CWTTRP_struct$demandas_res[pos$Positionfilas,])+sum(CWTTRP_struct$demandas_res[pos$Positioncolumnas,]))==0){
          R[pos$Positionfilas,3]<-(pos$Positioncolumnas-1)
          R[pos$Positioncolumnas,1]<-(pos$Positionfilas-1)


          # Debug Output
          if (verbose == 1) {
            print(paste("Iteration ", CWTTRP_struct$iter))
            print(paste("Seleccionamos ahorros ", pos$Positionfilas, "-", pos$Positioncolumnas))
            print(R)
            print(paste("Carga ", CWTTRP_struct$CargaT, "capacidad.vehiculo", input$capacidad.vehiculo[1]))
            print("Matriz de Tolvas")
            print(Tolvas)
          }

          S[pos$Positionfilas,pos$Positioncolumnas]<-0
          #Borramos ahorros utilizados para evitar ciclos
          S[pos$Positioncolumnas,pos$Positionfilas]<-0
          Shat[pos$Positionfilas,pos$Positioncolumnas]<-0
          # Borramos ahorros analogos en la matriz Shat, porque obviamente no
          # se puede implementar esta ruta
          Shat[pos$Positioncolumnas,pos$Positionfilas]<-0
        }
        #cuando no hay compartimentos, "deshacemos"
        else{

          flag_stop_i <- 0
          flag_stop_j <- 0
          if (sum(CWTTRP_struct$demandas_res[pos$Positionfilas,])!=0){flag_stop_i <- 2}
          if (sum(CWTTRP_struct$demandas_res[pos$Positioncolumnas,])!=0){flag_stop_j <- 2}


          # Inicializamos a 0 el numero de Tolvas que vamos a borrar
          delete.tol <- 0

          # Si el cliente i no habia sido introducido en ninguna ruta anteriormente (es decir, en alguna iteraci?n anterior), entonces n.trailer_i=0 y n.truck_i=0,
          # de modo que deber?amos deshacer lo que se haya hecho en esta iteraci?n con respecto a este cliente (pues no hemos sido capaces de servir a los dos clientes)
          if(n.trailer_i==0 && n.truck_i==0 && flag_stop_i == 2){
            CWTTRP_struct$demandas_res[pos$Positionfilas,] <- input$matriz.demandas[pos$Positionfilas,]
            if (sum(Tolvas[,1]==pos$Positionfilas-1)>0){
              for (k in which(Tolvas[,1]==pos$Positionfilas-1)){
                delete.tol <- delete.tol + 1
                Tolvas[k,] <- numeric(dim(Tolvas)[2])}
            }
          }

          # Si el cliente j no habia sido introducido en ninguna ruta anteriormente (es decir, en alguna iteraci?n anterior), entonces n.trailer_j=0 y n.truck_j=0,
          # de modo que deber?amos deshacer lo que se haya hecho en esta iteraci?n con respecto a este cliente (pues no hemos sido capaces de servir a los dos clientes)
          if(n.trailer_j==0 && n.truck_j==0 && flag_stop_j == 2){
            CWTTRP_struct$demandas_res[pos$Positioncolumnas,] <- input$matriz.demandas[pos$Positioncolumnas,]
            if (sum(Tolvas[,1]==pos$Positioncolumnas-1)>0){
              for (k in which(Tolvas[,1]==pos$Positioncolumnas-1)){
                delete.tol <- delete.tol + 1
                Tolvas[k,] <- numeric(dim(Tolvas)[2])}
            }
          }

          # Para eliminar ocos intermedios:
          res_tol = delete_zeros_tolvas(Tolvas,t)
          Tolvas = res_tol$Tolvas
          t = res_tol$t

          # Para eliminar ocos intermedios:
          res_tol = delete_zeros_tolvas(Tolvas,t)
          Tolvas = res_tol$Tolvas
          t = res_tol$t

          t <- min(which(Tolvas[,1]==0))

          # Pongo s-1 porq si hemos llegado hasta aqu? seguramente hemos hecho en algun momento s<-s+1
          if(delete.tol>0){
            CWTTRP_struct$H.camion_res[s-1,] <- c(CWTTRP_struct$H.camion_res[s-1,1:(dim(input$H.camion)[2]-delete.tol)],input$H.camion[s-1,(dim(input$H.camion)[2]-delete.tol+1):dim(input$H.camion)[2]])
          }

          S[pos$Positionfilas,pos$Positioncolumnas]<-0 #Si no es factible tambien lo borramos
          Shat[pos$Positionfilas,pos$Positioncolumnas]<-0
        }
      }
    }
  }

  else{
    S[pos$Positionfilas,pos$Positioncolumnas]<-0 #Si no es factible tambien lo borramos
    Shat[pos$Positionfilas,pos$Positioncolumnas]<-0
  }
  
  CWTTRP_struct$t <- t
  CWTTRP_struct$s <- s
  CWTTRP_struct$tc <- tc
  CWTTRP_struct$tt <- tt
  CWTTRP_struct$ss <- ss
  
  result <- list()
  result$CWTTRP_struct <- CWTTRP_struct
  result$Tolvas <- Tolvas
  result$R <- R
  result$Rhat <- Rhat
  result$S <- S
  result$Shat <- Shat
  result$merge <- merge


  return(result)
}

SmInS_VcVc<-function(CWTTRP_struct, R, Rhat, S, Shat, input, debug){

  merge <- 0
  if (is_in_parking_list(CWTTRP_struct, CWTTRP_struct$pos$Positionfilas)) {
    condition1 <- (R[CWTTRP_struct$pos$Positionfilas,3]==0)
  } else {
    condition1 <- (R[CWTTRP_struct$pos$Positionfilas,3]==0) &&
                  (Rhat[CWTTRP_struct$pos$Positionfilas,3]==0)
  }

  if (is_in_parking_list(CWTTRP_struct, CWTTRP_struct$pos$Positioncolumnas)) {
    condition2 <- (R[CWTTRP_struct$pos$Positioncolumnas,1]==0)
  } else {
    condition2 <- (R[CWTTRP_struct$pos$Positioncolumnas,1]==0) &&
      (Rhat[CWTTRP_struct$pos$Positioncolumnas,1]==0)
  }

  condition3 <- check_in_parking_list_and_tc(CWTTRP_struct,
                                             CWTTRP_struct$pos$Positionfilas,
                                             CWTTRP_struct$pos$Positioncolumnas,
                                             R, input)

  if( condition1 &&  condition2 && condition3 && CWTTRP_struct$CargaT<=input$capacidad.vehiculo){

    CWTTRP_struct$new$Positionfilas<-CWTTRP_struct$pos$Positionfilas
    CWTTRP_struct$new$Positioncolumnas<-CWTTRP_struct$pos$Positioncolumnas
    x<-0 #Evitamos ciclos

    #Sumamos la carga de los clientes posteriores a i

    tc_load_route1 <- 0
    vc_load_route1 <- input$vector.demandas[CWTTRP_struct$pos$Positionfilas]
    tc_load_route2 <- 0
    vc_load_route2 <- input$vector.demandas[CWTTRP_struct$pos$Positioncolumnas]

    #print(paste0("carga inicial 1 ->", CWTTRP_struct$CargaT))

    result<-addWorkload(input, R, CWTTRP_struct, CWTTRP_struct$new$Positionfilas,
                               CWTTRP_struct$pos$Positioncolumnas, x, option="row")

    x <- result$flag
    is_parking <- result$is_parking
    index_parking <- result$index_parking
    tc_load_route1 <- tc_load_route1 + result$tc_load_route
    vc_load_route1 <- vc_load_route1 + result$vc_load_route
    CWTTRP_struct$new$Positionfilas <- result$pos1

    ##print(paste0("carga first workload ->", CWTTRP_struct$CargaT))

    if (is_parking == 1) {
      #print("is_parking part 1")
      #print(CWTTRP_struct$parking_list)
      result <- addWorkload_parking(input, Rhat, index_parking)
      vc_load_subroute1 <- result$vc_load_subroute
      tc_load_subroute1 <- result$tc_load_subroute
    } else {
      vc_load_subroute1 <- 0
      tc_load_subroute1 <- 0
    }


    #print(paste0("carga parking ->", CWTTRP_struct$CargaT))

    #Sumamos la carga de los clientes posteriores a j

    result<-addWorkload(input, R, CWTTRP_struct, CWTTRP_struct$new$Positioncolumnas,
                        CWTTRP_struct$pos$Positionfilas, x, option="col")

    x <- x + result$flag
    is_parking <- result$is_parking
    index_parking <- result$index_parking
    tc_load_route2 <- tc_load_route2 + result$tc_load_route
    vc_load_route2 <- vc_load_route2 + result$vc_load_route
    CWTTRP_struct$new$Positioncolumnas <- result$pos1

    if (is_parking == 1) {
      ##print("is_parking part 2")
      ##print(CWTTRP_struct$parking_list)
      result <- addWorkload_parking(input, Rhat, index_parking)
      vc_load_subroute2 <- result$vc_load_subroute
      tc_load_subroute2 <- result$tc_load_subroute
    } else {
      vc_load_subroute2 <- 0
      tc_load_subroute2 <- 0
    }

    # Calc the load and the maximum capacity of vehicles

    result <- load_manager(CWTTRP_struct, input, vc_load_route1 , vc_load_route2 ,
                           tc_load_route1 , tc_load_route2 , vc_load_subroute1,
                           vc_load_subroute2, tc_load_subroute1, tc_load_subroute2)
    unfeasibility <- result$unfeasibility
    capacity <- result$capacity
    load_truck <- result$load_truck
    CWTTRP_struct <- result$CWTTRP_struct



    if(unfeasibility<=0 && x==0 ){
      merge <- 1
      #Anhadimos la ruta si es factible
      R[CWTTRP_struct$pos$Positionfilas,3]<-(CWTTRP_struct$pos$Positioncolumnas-1)
      R[CWTTRP_struct$pos$Positioncolumnas,1]<-(CWTTRP_struct$pos$Positionfilas-1)
      string <- paste0("add R, carga total ", CWTTRP_struct$CargaT, " / capacidadtotal ",
                       capacity, "capacidadtruck ",  load_truck, "/ flag ", x)

    } else  {
      string <- paste0("not add R, carga total ", CWTTRP_struct$CargaT, " / capacidadtotal ",
                       capacity, "capacidadtruck ",  load_truck, "/ flag ", x)
    }
    #print(string)
    # Debug Output
    if (debug==1){
      print(paste("Iteration ", CWTTRP_struct$iter))
      print(paste("Seleccionamos ahorros ", CWTTRP_struct$pos$Positionfilas, "-", CWTTRP_struct$pos$Positioncolumnas))
      print(R)
      print(paste("Carga ", CWTTRP_struct$CargaT, "capacity", capacity))
    }
    S[CWTTRP_struct$pos$Positionfilas,CWTTRP_struct$pos$Positioncolumnas]<-0
    S[CWTTRP_struct$pos$Positioncolumnas,CWTTRP_struct$pos$Positionfilas]<-0 #Borramos ahorros utilizados para evitar ciclos
    Shat[CWTTRP_struct$pos$Positionfilas,CWTTRP_struct$pos$Positioncolumnas]<-0
    Shat[CWTTRP_struct$pos$Positioncolumnas,CWTTRP_struct$pos$Positionfilas]<-0   #Borramos ahorros analogos en la matriz Shat, porque obviamente no se puede implementar esta ruta

  }
  S[CWTTRP_struct$pos$Positionfilas,CWTTRP_struct$pos$Positioncolumnas]<-0 #Si no es factible tambien lo borramos
  Shat[CWTTRP_struct$pos$Positionfilas,CWTTRP_struct$pos$Positioncolumnas]<-0

  result <- list()
  result$CWTTRP_struct <- CWTTRP_struct
  result$R <- R
  result$S <- S
  result$Shat <- Shat
  result$merge <-merge

  return(result)
}
