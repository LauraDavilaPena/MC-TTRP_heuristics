SmInShat_TcVc_with_hoppers<-function(CWTTRP_struct, Tolvas, R, Rhat, S, Shat,
                                     input, pos,  n, n1, nf, verbose){

  t <- CWTTRP_struct$t
  s <- CWTTRP_struct$s
  tc <- CWTTRP_struct$tc
  tt <- CWTTRP_struct$tt
  ss <- CWTTRP_struct$ss
  merge <- 0

  if( CWTTRP_struct$CargaT <= input$capacidad.vehiculo &&
      sum(input$matriz.demandas[pos$Positionfilas,])   <=
      input$capacidad.truck[1:length(input$capacidad.trailer)] &&
      R[pos$Positionfilas,3]==0 && sum(Rhat==(pos$Positionfilas-1))==1){

    # este seria el t.c.
    CWTTRP_struct$newPositionfilas     <- pos$Positionfilas
    # este seria el v.c.
    CWTTRP_struct$newPositioncolumnas  <- pos$Positioncolumnas
    CWTTRP_struct$newPositioncolumnas2 <- pos$Positioncolumnas

    # local variables
    x <- 0
    z <- 0
    a <- 0
    b <- 0
    n.truck_i <- 0
    n.trailer_i <- 0
    n.truck_j <- 0
    n.trailer_j <- 0

    result_sub <- addWorkload_R(R, Tolvas, input, CWTTRP_struct$newPositionfilas,
                                pos$Positioncolumnas, CWTTRP_struct$CargaT, n.truck_i,
                                n.trailer_i, 0, x, "row")
    CWTTRP_struct$CargaT <- result_sub$CargaT
    CWTTRP_struct$newPositionfilas <- result_sub$pos1
    n.truck_i <- result_sub$n.truck
    n.trailer_i <- result_sub$n.trailer
    x <- result_sub$a2

    result_sub <- addWorkload_R_check_subtour(R, Rhat, Tolvas, input, CWTTRP_struct,
                                              CWTTRP_struct$newPositioncolumnas2,
                                              CWTTRP_struct$newPositioncolumnas3,
                                              CWTTRP_struct$primerclienteMT,
                                              pos$Positionfilas,
                                              n.truck_j, n.trailer_j, a, x, z,
                                              n, n1, 0, "row")

    CWTTRP_struct <- result_sub$CWTTRP_struct
    CWTTRP_struct$newPositioncolumnas2 <- result_sub$pos1
    CWTTRP_struct$newPositioncolumnas3 <- result_sub$pos2
    CWTTRP_struct$primerclienteMT <- result_sub$pos3
    n.truck_j <- result_sub$n.truck
    n.trailer_j <- result_sub$n.trailer
    a <- result_sub$a1
    x <- result_sub$a2
    z <- result_sub$a3

    if (a>0){

      result_sub <- check_tolvas(CWTTRP_struct$primerclienteMT, Tolvas, n.truck_j, n.trailer_j)
      n.truck_j <- result_sub$n.truck
      n.trailer_j <- result_sub$n.trailer

      if (Rhat[CWTTRP_struct$primerclienteMT,3]!=0) {
        CWTTRP_struct$newPositioncolumnas3 <- CWTTRP_struct$primerclienteMT-1

        result_sub <- addWorkload_Rhat(Rhat, Tolvas, input,   CWTTRP_struct$primerclienteMT,
                                       CWTTRP_struct$newPositioncolumnas3, CWTTRP_struct$CargaT,
                                       n.truck_j, n.trailer_j, "col")
        CWTTRP_struct$CargaT <- result_sub$CargaT
        CWTTRP_struct$primerclienteMT <- result_sub$pos1
        n.truck_j <- result_sub$n.truck
        n.trailer_j <- result_sub$n.trailer

      }
    }

    result_sub <- addWorkload_R_check_subtour(R, Rhat, Tolvas, input, CWTTRP_struct,
                                              CWTTRP_struct$newPositioncolumnas,
                                              CWTTRP_struct$newPositioncolumnas3,
                                              CWTTRP_struct$ultimoclienteMT,
                                              pos$Positionfilas,
                                              n.truck_j, n.trailer_j, b, x, z, n, n1, 0, "col")

    CWTTRP_struct <- result_sub$CWTTRP_struct
    CWTTRP_struct$newPositioncolumnas <- result_sub$pos1
    CWTTRP_struct$newPositioncolumnas3 <- result_sub$pos2
    CWTTRP_struct$ultimoclienteMT <- result_sub$pos3
    n.truck_j <- result_sub$n.truck
    n.trailer_j <- result_sub$n.trailer
    b <- result_sub$a1
    x <- result_sub$a2
    z <- result_sub$a3

    if (b>0){

      if (Rhat[CWTTRP_struct$ultimoclienteMT,3]!=0) {
        CWTTRP_struct$newPositioncolumnas3 <- CWTTRP_struct$ultimoclienteMT-1

        result_sub <- addWorkload_Rhat(Rhat, Tolvas, input,   CWTTRP_struct$ultimoclienteMT,
                                       CWTTRP_struct$newPositioncolumnas3, CWTTRP_struct$CargaT,
                                       n.truck_j, n.trailer_j, "col")
        CWTTRP_struct$CargaT <- result_sub$CargaT
        CWTTRP_struct$ultimoclienteMT <- result_sub$pos1
        n.truck_j <- result_sub$n.truck
        n.trailer_j <- result_sub$n.trailer

      }

      result_sub <- check_tolvas(CWTTRP_struct$ultimoclienteMT, Tolvas, n.truck_j, n.trailer_j)
      n.truck_j <- result_sub$n.truck
      n.trailer_j <- result_sub$n.trailer

    }

    # if there are previous or later clients
    if (a>1 && b>1){
        if (Rhat[pos$Positioncolumnas,3]!=0){
            cliente_subruta <- (Rhat[pos$Positioncolumnas,3]+1)
            Carga_subruta <- sum(input$matriz.demandas[Rhat[cliente_subruta,2]+1,])
    
            while(Rhat[cliente_subruta,3]!=(pos$Positioncolumnas-1)){
              Carga_subruta <- Carga_subruta + sum(input$matriz.demandas[Rhat[cliente_subruta,3]+1,])
              cliente_subruta <- Rhat[cliente_subruta,3]+1
            }
    
            CWTTRP_struct$CargaT <- CWTTRP_struct$CargaT - Carga_subruta
        }
    }

    CWTTRP_struct$aux <- 0

    # if there are not previous or later clients
    if (a==0 && b==0){
      if (Rhat[pos$Positioncolumnas,3]!=0){
        if (verbose == 1) {
          print ("Solo podemos tener una subruta por aparcamiento")
        }
        CWTTRP_struct$aux <- CWTTRP_struct$aux+1
      }
    }

    if (n.trailer_j!=0 && n.truck_j!=0){
      CWTTRP_struct$aux <- CWTTRP_struct$aux + 1
    }

    if(CWTTRP_struct$CargaT <= input$capacidad.vehiculo &&
       sum(input$matriz.demandas[pos$Positionfilas,])<=
       input$capacidad.truck[1:length(input$capacidad.trailer)] &&
       x==0 && CWTTRP_struct$aux==0){

      if( (input$matriz.distancia[1,CWTTRP_struct$newPositionfilas]+
           input$matriz.distancia[pos$Positionfilas,1]-input$matriz.distancia[pos$Positioncolumnas,CWTTRP_struct$newPositionfilas]-
           input$matriz.distancia[pos$Positionfilas,pos$Positioncolumnas] >= 0) ){#Anhadimos la ruta si es factible

        if(n.truck_j==0 && n.trailer_j==0){
          ss <- max(n.truck_i, n.trailer_i)
          #min(which(CWTTRP_struct$H.trailer_res[,1]!=-1))
          #ss <- as.numeric(ss)

          tt <- 1 # comenzamos llenando la primera tolva

          for (i in 1:nf){
            while(CWTTRP_struct$demandas_res[pos$Positioncolumnas,i]!=0 &&
                  sum(CWTTRP_struct$H.trailer_res[ss,])!=-dim(input$H.trailer)[2] ){

              while (CWTTRP_struct$demandas_res[pos$Positioncolumnas,i]>
                     max(CWTTRP_struct$H.trailer_res) &&
                     sum(CWTTRP_struct$H.trailer_res[ss,])!=-dim(input$H.trailer)[2]){

                if(t>2 && Tolvas[t-1,1]==0){
                  t <- min(which(Tolvas[,1]==0))
                }

                Tolvas[t,] <- c(pos$Positioncolumnas-1,i,"trailer",ss,max(input$H.trailer),1)
                CWTTRP_struct$demandas_res[pos$Positioncolumnas,i] <- CWTTRP_struct$demandas_res[pos$Positioncolumnas,i] - max(input$H.trailer)
                CWTTRP_struct$H.trailer_res[ss,tt] <- -1

                if (tt < dim(input$H.trailer)[2]){
                  tt <- tt+1
                }else{tt <- 1}

                t <- t+1
              }

              if(sum(CWTTRP_struct$H.trailer_res[ss,])!= -dim(CWTTRP_struct$H.trailer_res)[2]){
                if(t>2 && Tolvas[t-1,1]==0){
                  t <- min(which(Tolvas[,1]==0))
                }
                Tolvas[t,] <- c(pos$Positioncolumnas-1,i,"trailer",ss,CWTTRP_struct$demandas_res[pos$Positioncolumnas,i],CWTTRP_struct$demandas_res[pos$Positioncolumnas,i]/max(input$H.trailer))
                CWTTRP_struct$demandas_res[pos$Positioncolumnas,i] <- max(0,CWTTRP_struct$demandas_res[pos$Positioncolumnas,i] - max(input$H.trailer))
                CWTTRP_struct$H.trailer_res[ss,tt] <- -1

                if (tt < dim(input$H.trailer)[2]){
                  tt <- tt+1
                }else{tt <- 1}
                t <- t+1
              }
            }

          }# aqui acaba el bucle "for (i in 1:nf)" para el cliente vc.
        }
        
        if(n.truck_i==0 && n.trailer_i==0){
          s <- max(n.truck_j, n.trailer_j)

          # s <- min(which(CWTTRP_struct$H.camion_res[,1]!=-1))
          #s <- as.numeric(s)
          tc <- 1

          for (i in 1:nf){
            while(sum(CWTTRP_struct$H.camion_res[s,])!=-dim(input$H.camion)[2] &&
                  CWTTRP_struct$demandas_res[pos$Positionfilas,i]!=0){
              while(CWTTRP_struct$demandas_res[pos$Positionfilas,i]>
                    max(CWTTRP_struct$H.camion_res) &&
                    sum(CWTTRP_struct$H.camion_res[s,])!=-dim(input$H.camion)[2]){

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
                Tolvas[t,] <- c(pos$Positionfilas-1,i,"truck",s,CWTTRP_struct$demandas_res[pos$Positionfilas,i],CWTTRP_struct$demandas_res[pos$Positionfilas,i]/max(input$H.camion))
                CWTTRP_struct$demandas_res[pos$Positionfilas,i] <- max(0,CWTTRP_struct$demandas_res[pos$Positionfilas,i] - max(input$H.camion))
                CWTTRP_struct$H.camion_res[s,tc] <- -1

                if (tc < dim(input$H.camion)[2]){
                  tc <- tc+1
                }else{tc <- 1}

                t <- t + 1
              }
            }
          }

        }


        # en el caso de que la fusion se pueda efectuar (pues la combinacion de
        # Tolvas me lo permite), hago lo siguiente:

        if (sum(CWTTRP_struct$demandas_res[pos$Positionfilas,])+
            sum(CWTTRP_struct$demandas_res[pos$Positioncolumnas,])==0){

          merge <- 1
          Rhat[pos$Positioncolumnas,1]<-(pos$Positionfilas-1)
          Rhat[pos$Positionfilas,3]<-(pos$Positioncolumnas-1)
          CWTTRP_struct$newPositionfilas2 <- pos$Positionfilas
          y <- 0
          while(CWTTRP_struct$newPositionfilas2!=1){
            Rhat[CWTTRP_struct$newPositionfilas2,1] = R[CWTTRP_struct$newPositionfilas2,1]
            R[CWTTRP_struct$newPositionfilas2,1] <- 0
            CWTTRP_struct$newPositionfilas2 <- (Rhat[CWTTRP_struct$newPositionfilas2,1]+1)
            if(CWTTRP_struct$newPositionfilas2==1) y <- y+1
            if(y==0){
              Rhat[CWTTRP_struct$newPositionfilas2,3] = R[CWTTRP_struct$newPositionfilas2,3]
              R[CWTTRP_struct$newPositionfilas2,3] <- 0
            }
            #CWTTRP_struct$newPositionfilas2 <- (Rhat[CWTTRP_struct$newPositionfilas2,1]+1)
          }
          
          CWTTRP_struct$parking_list <- c(CWTTRP_struct$parking_list, pos$Positioncolumnas-1)
          
          Rhat[CWTTRP_struct$newPositionfilas,1] <- (pos$Positioncolumnas-1)
          Rhat[pos$Positioncolumnas,3] <- (CWTTRP_struct$newPositionfilas-1)


          # Debug Output
          if (verbose == 1) {
            print(paste("Iteration ", CWTTRP_struct$iter))
            print(paste("Seleccionamos ahorros ", pos$Positionfilas, "-", pos$Positioncolumnas))
            print("La matriz de rutas es")
            print(R)
            print("La matriz de subrutas es ")
            print(Rhat)
            print(paste("Carga ", CWTTRP_struct$CargaT, "capacidad.truck", input$capacidad.truck[1], "capacidad.trailer", input$capacidad.trailer[1]))
            print("La matriz que contiene informacion de Tolvas e")
            print(Tolvas)
          }
          Shat[pos$Positionfilas,pos$Positioncolumnas]<-0
          Shat[pos$Positioncolumnas,pos$Positionfilas]<-0 #Borramos ahorros utilizados para evitar ciclos
          S[pos$Positionfilas,pos$Positioncolumnas]<-0
          S[pos$Positioncolumnas,pos$Positionfilas]<-0 #Borramos ahorros analogos en la matriz S, porque obviamente no se puede implementar esta ruta

        }
        else{# si no se han encontrado Tolvas para servir a los clientes, "deshago" lo hecho

          parada_i <- 0
          parada_j <- 0
          if (sum(CWTTRP_struct$demandas_res[pos$Positionfilas,])!=0){parada_i <- 2}
          if (sum(CWTTRP_struct$demandas_res[pos$Positioncolumnas,])!=0){parada_j <- 2}


          # Inicializamos a 0 el numero de Tolvas que vamos a borrar
          delete.tol <- 0

          if(n.trailer_i==0 && n.truck_i==0 && parada_i == 2){
            CWTTRP_struct$demandas_res[pos$Positionfilas,] <- input$matriz.demandas[pos$Positionfilas,]
            if (sum(Tolvas[,1]==pos$Positionfilas-1)>0){
              for (k in which(Tolvas[,1]==pos$Positionfilas-1)){
                delete.tol <- delete.tol + 1
                Tolvas[k,] <- numeric(dim(Tolvas)[2])}
            }
          }

          if(n.trailer_j==0 && n.truck_j==0 && parada_j == 2){
            CWTTRP_struct$demandas_res[pos$Positioncolumnas,] <- input$matriz.demandas[pos$Positioncolumnas,]
            if (sum(Tolvas[,1]==pos$Positioncolumnas-1)>0){
              for (k in which(Tolvas[,1]==pos$Positioncolumnas-1)){
                delete.tol <- delete.tol + 1
                Tolvas[k,] <- numeric(dim(Tolvas)[2])}
            }
          }

          # Para eliminar ocos intermedios:
          res_tol = delete_zeros_Tolvas(Tolvas,t)
          Tolvas = res_tol$Tolvas
          t = res_tol$t

          # Para eliminar ocos intermedios:
          res_tol = delete_zeros_Tolvas(Tolvas,t)
          Tolvas = res_tol$Tolvas
          t = res_tol$t

          t <- min(which(Tolvas[,1]==0))


          if(delete.tol>0){
            CWTTRP_struct$H.camion_res[s,] <-
              c(CWTTRP_struct$H.camion_res[s,1:(dim(input$H.camion)[2]-delete.tol)],
                input$H.camion[s,(dim(input$H.camion)[2]-delete.tol+1):dim(input$H.camion)[2]])
          }
        }

      }
      else{
        Shat[pos$Positionfilas,pos$Positioncolumnas]<-0
      }
    }else{
      Shat[pos$Positionfilas,pos$Positioncolumnas]<-0
    }
  }
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


SmInShat_TcVc<-function(CWTTRP_struct, R, Rhat, S, Shat, input, debug){

  merge <- 0
  if(CWTTRP_struct$CargaT<=input$capacidad.vehiculo &&
     input$vector.demandas[CWTTRP_struct$pos$Positionfilas]<=input$capacidad.truck &&
     R[CWTTRP_struct$pos$Positionfilas,3]==0 &&
     sum(Rhat==(CWTTRP_struct$pos$Positionfilas-1))==1 &&
     sum(Rhat==(CWTTRP_struct$pos$Positioncolumnas-1))==1){

    #readline()
    # Si la demanda de ambos no excede Qvehiculo y la del t.c. no excede Qtruck;
    # y ademas el t.c. es el ultimo de su ruta (y no esta incluido en ningun otro subtour ya)

    CWTTRP_struct$new$Positionfilas <- CWTTRP_struct$pos$Positionfilas         # este seria el t.c.
    CWTTRP_struct$new$Positioncolumnas <- CWTTRP_struct$pos$Positioncolumnas   # este seria el v.c.
    CWTTRP_struct$new$Positioncolumnas2 <- CWTTRP_struct$pos$Positioncolumnas
    x<-0 #Evitamos ciclos


    tc_load_route1 <- input$vector.demandas[CWTTRP_struct$pos$Positionfilas]
    vc_load_route1 <- 0
    tc_load_route2 <- 0
    vc_load_route2 <- input$vector.demandas[CWTTRP_struct$pos$Positioncolumnas]
    #print("addWorkload")

    result<-addWorkload(input, R, CWTTRP_struct, CWTTRP_struct$new$Positionfilas,
                        CWTTRP_struct$pos$Positioncolumnas, x, option="row")

    x <- result$flag
    is_parking <- result$is_parking
    index_parking <- result$index_parking
    tc_load_route1 <- tc_load_route1 + result$tc_load_route
    vc_load_route1 <- vc_load_route1 + result$vc_load_route +
                  input$vector.demandas[R[CWTTRP_struct$pos$Positionfilas,2]+1]
    CWTTRP_struct$new$Positionfilas <- result$pos1

    result<-addWorkload(input, R, CWTTRP_struct, CWTTRP_struct$new$Positioncolumnas,
                        CWTTRP_struct$pos$Positionfilas, x, option="row")
    x <- x + result$flag
    is_parking <- result$is_parking
    index_parking <- result$index_parking
    tc_load_route2 <- tc_load_route2 + result$tc_load_route
    vc_load_route2 <- vc_load_route2 + result$vc_load_route
    CWTTRP_struct$new$Positioncolumnas <- result$pos1
    vc_load_subroute2 <- 0
    tc_load_subroute2 <- 0

    if (is_parking == 1) {
      result <- addWorkload_parking(input, Rhat, index_parking)
      vc_load_route2 <- vc_load_route2 + result$vc_load_subroute
      tc_load_route2 <- tc_load_route2 + result$tc_load_subroute
    }

    result<-addWorkload(input, R, CWTTRP_struct, CWTTRP_struct$new$Positioncolumnas2,
                        CWTTRP_struct$pos$Positionfilas, x, option="col")
    x <- x + result$flag
    is_parking <- result$is_parking
    index_parking <- result$index_parking
    tc_load_route2 <- tc_load_route2 + result$tc_load_route
    vc_load_route2 <- vc_load_route2 + result$vc_load_route
    CWTTRP_struct$new$Positioncolumnas2 <- result$pos1

    if (is_parking == 1) {
      result <- addWorkload_parking(input, Rhat, index_parking)
      vc_load_route2 <- vc_load_route2 + result$vc_load_subroute
      tc_load_route2 <- tc_load_route2 + result$tc_load_subroute
    }

    if (tc_load_route2 == 0) {
      #print("tc_load_route2")
      # only add subroutes in PVR  routes
      vc_load_route2 <- vc_load_route2 + +input$vector.demandas[R[CWTTRP_struct$new$Positioncolumnas,2]+1]
      result <- load_manager_create_subroute(CWTTRP_struct, input, vc_load_route1 ,
                                           vc_load_route2 , tc_load_route1 , tc_load_route2,
                                           option="tcvc")
      unfeasibility <- result$unfeasibility
      capacity <- result$capacity
      load_truck <- result$load_truck
      CWTTRP_struct <- result$CWTTRP_struct
    } else {
      unfeasibility <- 1
      capacity <- 0
      load_truck <- 0
    }

    #print(paste0(vc_load_route1, " ", vc_load_route2, " ", tc_load_route1, " ", tc_load_route2 ))
    #print(unfeasibility)
    #print(paste0("xxxload truck ", load_truck, " x ", x))

    if(unfeasibility<=0 && load_truck <= input$capacidad.truck && x==0 ){


      feasible_condition1 <- input$matriz.distancia[1,CWTTRP_struct$new$Positionfilas]+
                            input$matriz.distancia[CWTTRP_struct$pos$Positionfilas,1]-
                            input$matriz.distancia[CWTTRP_struct$pos$Positioncolumnas,CWTTRP_struct$new$Positionfilas]-
                            input$matriz.distancia[CWTTRP_struct$pos$Positionfilas,CWTTRP_struct$pos$Positioncolumnas]

      if( (feasible_condition1>=0) ){

        #print("feasible")
        merge <- 1
        Rhat[CWTTRP_struct$pos$Positioncolumnas,1]<-(CWTTRP_struct$pos$Positionfilas-1)
        Rhat[CWTTRP_struct$pos$Positionfilas,3]<-(CWTTRP_struct$pos$Positioncolumnas-1)
        CWTTRP_struct$new$Positionfilas2 <- CWTTRP_struct$pos$Positionfilas
        y <- 0
        while(CWTTRP_struct$new$Positionfilas2!=1){
          Rhat[CWTTRP_struct$new$Positionfilas2,1] = R[CWTTRP_struct$new$Positionfilas2,1]
          R[CWTTRP_struct$new$Positionfilas2,1] <- 0
          CWTTRP_struct$new$Positionfilas2 <- (Rhat[CWTTRP_struct$new$Positionfilas2,1]+1)
          if(CWTTRP_struct$new$Positionfilas2==1) y <- y+1
          if(y==0){
            Rhat[CWTTRP_struct$new$Positionfilas2,3] = R[CWTTRP_struct$new$Positionfilas2, 3]
            R[CWTTRP_struct$new$Positionfilas2,3] <- 0
          }
        }
        Rhat[CWTTRP_struct$new$Positionfilas,1] <- (CWTTRP_struct$pos$Positioncolumnas-1)
        Rhat[CWTTRP_struct$pos$Positioncolumnas,3] <- (CWTTRP_struct$new$Positionfilas-1)

        CWTTRP_struct$parking_list <- c(CWTTRP_struct$parking_list, CWTTRP_struct$pos$Positioncolumnas)

      }
    }


    # Debug Output
    if (debug==1){
      print(paste("Iteration ", CWTTRP_struct$iter))
      print(paste("Seleccionamos ahorros ", CWTTRP_struct$pos$Positionfilas, "-",
                  CWTTRP_struct$pos$Positioncolumnas))
      print("La matriz de rutas es")
      print(R)
      print("La matriz de subrutas es ")
      print(Rhat)

      print(paste("Carga ", CWTTRP_struct$CargaT, "capacidad.truck", input$capacidad.truck,
                  "capacidad.trailer", input$capacidad.trailer))
    }


    Shat[CWTTRP_struct$pos$Positionfilas,CWTTRP_struct$pos$Positioncolumnas]<-0
    Shat[CWTTRP_struct$pos$Positioncolumnas,CWTTRP_struct$pos$Positionfilas]<-0
    #Borramos ahorros utilizados para evitar ciclos
    S[CWTTRP_struct$pos$Positionfilas,CWTTRP_struct$pos$Positioncolumnas]<-0
    S[CWTTRP_struct$pos$Positioncolumnas,CWTTRP_struct$pos$Positionfilas]<-0
    #Borramos ahorros analogos en la matriz S, porque obviamente no se puede implementar esta ruta


  }
  Shat[CWTTRP_struct$pos$Positionfilas,CWTTRP_struct$pos$Positioncolumnas]<-0
  #Si no es factible tambien lo borramos

  result = list()
  result$CWTTRP_struct<-CWTTRP_struct
  result$R<-R
  result$Rhat<-Rhat
  result$S<-S
  result$Shat<-Shat
  result$merge <-merge

  return(result)
}
