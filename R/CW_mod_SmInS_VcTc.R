SmInS_VcTc_with_hoppers<-function(CWTTRP_struct, Tolvas, R, Rhat, S, Shat, input,
                                  pos, n, nf, verbose){

  t <- CWTTRP_struct$t
  s <- CWTTRP_struct$s
  tc <- CWTTRP_struct$tc
  tt <- CWTTRP_struct$tt
  ss <- CWTTRP_struct$ss
  merge <- 0

  if(R[pos$Positionfilas,3]==0 && R[pos$Positioncolumnas,1]==0 &&
     Rhat[pos$Positioncolumnas,1]==0 && Rhat[pos$Positionfilas,1]==0 &&
     CWTTRP_struct$CargaT<=input$capacidad.truck){
    CWTTRP_struct$newPositionfilas<-pos$Positionfilas
    CWTTRP_struct$newPositioncolumnas<-pos$Positioncolumnas
    x<-0 #Evitamos ciclos
    n.trailer_i <- 0
    n.truck_i <- 0
    n.trailer_j <- 0
    n.truck_j <- 0

    result_sub <- addWorkload_R(R, Tolvas, input, CWTTRP_struct$newPositionfilas,
                                pos$Positioncolumnas, CWTTRP_struct$CargaT, n.truck_i,
                                n.trailer_i, 0, x, "row")
    CWTTRP_struct$CargaT <- result_sub$CargaT
    CWTTRP_struct$newPositionfilas <- result_sub$pos1
    n.truck_i <- result_sub$n.truck
    n.trailer_i <- result_sub$n.trailer
    x <- result_sub$a2

    result_sub <- addWorkload_R(R, Tolvas, input, CWTTRP_struct$newPositioncolumnas,
                                pos$Positionfilas, CWTTRP_struct$CargaT, n.truck_j,
                                n.trailer_j, 0, x, "col")
    CWTTRP_struct$CargaT <- result_sub$CargaT
    CWTTRP_struct$newPositioncolumnas <- result_sub$pos1
    n.truck_j <- result_sub$n.truck
    n.trailer_j <- result_sub$n.trailer
    x <- result_sub$a2

    parada <- 0

    result_sub <- SmInS_VcTc_control_fusion(CWTTRP_struct, Tolvas, R,
                                            input, n.trailer_i, n.truck_i,
                                            n.trailer_j, n.truck_j, s,
                                            parada, verbose)
    CWTTRP_struct <- result_sub$CWTTRP_struct
    Tolvas <- result_sub$Tolvas
    s <- result_sub$s
    parada <- result_sub$flag_stop

#########

    result_sub <- SmInS_VcTc_apply_fusion_with_hoppers(CWTTRP_struct, Tolvas, R,
                                                       Rhat, S, Shat, input,
                                                       n.truck_i, n.trailer_i,
                                                       n.truck_j, n.trailer_j,
                                                       pos, n, x, t, s, tc, tt,
                                                       ss, parada, nf, verbose)
    CWTTRP_struct <- result_sub$CWTTRP_struct
    Tolvas <- result_sub$Tolvas
    R <- result_sub$R
    Rhat <- result_sub$Rhat
    S <- result_sub$S
    Shat <- result_sub$Shat
    t <- result_sub$t
    s <- result_sub$s
    tc <- result_sub$tc
    tt <- result_sub$tt
    ss <- result_sub$ss
    merge <- result_sub$merge
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


# SUBROUTINE: SmInS_VcTc_control_fusion
#
# It controls the different cases of fusion, setting to value two the variable
# flag_stop, when the merge is not possible.
#
SmInS_VcTc_control_fusion<-function(CWTTRP_struct, Tolvas, R,
                                    input, n.trailer_i, n.truck_i,
                                    n.trailer_j, n.truck_j, s,
                                    flag_stop, verbose){

  # Check if one of the trucks is completed.

  if( (n.truck_i!=0 || n.truck_j!=0) && n.truck_i*n.truck_j==0){
    if(n.truck_i!=0){s <- n.truck_i; s <- as.numeric(s)}
    if(n.truck_j!=0){s <- n.truck_j; s <- as.numeric(s)}

    if(sum(CWTTRP_struct$H.camion_res[s,])==-dim(input$H.camion)[2]){
      flag_stop <- 2
    }

  }

  if(n.truck_i!=0 && n.truck_j!=0 && n.truck_i!=n.truck_j &&
     n.trailer_i==0 && n.trailer_j==0 &&
     sum(CWTTRP_struct$H.camion_res[n.truck_i,]==-1)+
     sum(CWTTRP_struct$H.camion_res[n.truck_j,]==-1)>dim(input$H.camion)[2]){

    flag_stop <- 2

  }

  else if(n.truck_i!=0 && n.truck_j!=0 &&
          n.truck_i!=n.truck_j && n.trailer_i==0 && n.trailer_j==0 &&
          sum(CWTTRP_struct$H.camion_res[n.truck_i,]==-1)+
          sum(CWTTRP_struct$H.camion_res[n.truck_j,]==-1)<=dim(input$H.camion)[2]){

    flag_stop <- 1


    tol.lib_i <- sum(CWTTRP_struct$H.camion_res[n.truck_i,]!=-1)
    tol.lib_j <- sum(CWTTRP_struct$H.camion_res[n.truck_j,]!=-1)

    if(sum(CWTTRP_struct$H.camion_res[n.truck_i,]==-1)<
       sum(CWTTRP_struct$H.camion_res[n.truck_j,]==-1)){

      truck_ocup <- n.truck_j
      truck_lib <- n.truck_i

    }else{
      truck_ocup <- n.truck_i
      truck_lib <- n.truck_j}

    s.prueba <- truck_ocup

    tol.lib_ocup <- sum(CWTTRP_struct$H.camion_res[s.prueba,]!=-1)

    if(sum(CWTTRP_struct$H.camion_res[s.prueba,]!=-1)>0){
      ntol.lib_ocup <- min(which(CWTTRP_struct$H.camion_res[s.prueba,]!=-1))
    }else{
      ntol.lib_ocup <- 0
    }

    tol.ocup_lib <- sum(CWTTRP_struct$H.camion_res[truck_lib,]==-1)
    ntol.ocup_lib <- max(which(CWTTRP_struct$H.camion_res[truck_lib,]==-1))

    CWTTRP_struct$H.camion_res[truck_lib,] <- rep(input$H.camion[truck_lib,1],dim(input$H.camion)[2])
    CWTTRP_struct$H.camion_res[s.prueba,ntol.lib_ocup:(ntol.lib_ocup+tol.ocup_lib-1)] <- rep(-1,tol.ocup_lib)

    min.TolvasLib <- min(which(Tolvas[,4]==truck_lib))
    tot.TolvasLib <- which(Tolvas[,4]==truck_lib & Tolvas[,3]=="truck")


    for (kk in 1:length(tot.TolvasLib)){
      Tolvas[tot.TolvasLib[kk],4]  <- truck_ocup
    }


  }

  if(n.truck_i!=0 && n.trailer_i!=0){
    flag_stop <- 2
  }

  if( (n.truck_i==0 && n.trailer_i!=0) && (n.truck_j!=0 && n.trailer_j==0) ){
    flag_stop <- 2
  }

  if( (n.truck_i==0 && n.trailer_i!=0) && (n.truck_j==0 && n.trailer_j==0) ){
    flag_stop <- 2
  }


  result <- list()
  result$CWTTRP_struct <- CWTTRP_struct
  result$Tolvas <- Tolvas
  result$s <- s
  result$flag_stop <- flag_stop

  return(result)
}

# SUBROUTINE: SmInS_VcTc_apply_fusion_with_hoppers
#
# It tries to apply a merge of routes, considering the hoppers' configuration in
# the affirmative case.
#
SmInS_VcTc_apply_fusion_with_hoppers<-function(CWTTRP_struct, Tolvas, R, Rhat, S,
                                               Shat, input, n.truck_i, n.trailer_i,
                                               n.truck_j, n.trailer_j, pos, n,
                                               x, t, s, tc, tt, ss, flag_stop, nf,
                                               verbose) {
  merge <- 0

  if(flag_stop == 0){

    if(CWTTRP_struct$CargaT<=input$capacidad.truck && x==0){

      s.aux <- s
      s.aux <- as.numeric(s.aux)
      ss.aux <- ss
      ss.aux <- as.numeric(ss.aux)

      if (sum(Tolvas[,1]==(pos$Positionfilas-1))>0){
        CWTTRP_struct$aux <- max(which(Tolvas[,1]==(pos$Positionfilas-1)))
        if (Tolvas[CWTTRP_struct$aux,3]=="trailer"){
          ss <- Tolvas[CWTTRP_struct$aux,4]
          ss <- as.numeric(ss)
        }
        else{
          s <- Tolvas[CWTTRP_struct$aux,4]
          s <- as.numeric(s)
        }
      }
      
      if (sum(Tolvas[,1]==(pos$Positionfilas-1))==0 && 
         sum(Tolvas[,1]==(pos$Positioncolumnas-1))==0 && 
         ss == ss.aux){
         #sum(CWTTRP_struct$H.trailer_res[ss.aux,])!=sum(input$H.trailer[ss.aux,]) && ss == ss.aux){
          for (i in 1:length(CWTTRP_struct$H.trailer_res[,1])) {
            if ((sum(CWTTRP_struct$H.trailer_res[i,] == -1)  == 0) &&
                (sum(CWTTRP_struct$H.camion_res[i,] == -1)  == 0)) {
              ss <- i
              s <- i
              break;
            }
          }
      }
      
      #if(sum(Tolvas[,1]==(pos$Positionfilas-1))==0 && 
      #   sum(Tolvas[,1]==(pos$Positioncolumnas-1))==0  && 
      #   sum(CWTTRP_struct$H.camion_res[s.aux,])!=sum(input$H.camion[s.aux,]) && s == s.aux){
      #  #s <- s + 1
      #  #s <- as.numeric(s)
      #  auxv = which(CWTTRP_struct$H.camion_res[,1]!=-1)
      #  for (cc in 1:length(auxv)) {
      #    if (CWTTRP_struct$H.trailer_res[cc,1]!=-1) { s <- as.numeric(cc); break; }
      #  }
      #}

      if(sum(Tolvas[,1]==(pos$Positioncolumnas-1))>0){
        CWTTRP_struct$aux1 <- min(which(Tolvas[,1]==(pos$Positioncolumnas-1)))
        CWTTRP_struct$aux2 <- max(which(Tolvas[,1]==(pos$Positioncolumnas-1)))
        if(Tolvas[CWTTRP_struct$aux2,3]=="trailer"){
          ss <- Tolvas[CWTTRP_struct$aux2,4]
          ss <- as.numeric(ss)
        }else{
          s <- Tolvas[CWTTRP_struct$aux2,4]
          s <- as.numeric(s)
        }
        if(Tolvas[CWTTRP_struct$aux1,3]=="trailer"){
          ss <- Tolvas[CWTTRP_struct$aux1,4]
          ss <- as.numeric(ss)
        }else{
          s <- Tolvas[CWTTRP_struct$aux1,4]
          s <- as.numeric(s)
        }
      }

      # En este momento ya tengo decidido cual es el camion y cual es el trailer
      # q voy a utilizar. Entonces aqui deberia tambien "actualizar" los valores
      # tt y tc.
      if(sum(CWTTRP_struct$H.trailer_res[ss,])!=-dim(input$H.trailer)[2]){
        tt <- min(which(CWTTRP_struct$H.trailer_res[ss,]!=-1))
      }
      if(sum(CWTTRP_struct$H.camion_res[s,])!=-dim(input$H.camion)[2]){
        tc <- min(which(CWTTRP_struct$H.camion_res[s,]!=-1))
      }

      # COMPARTIMENTOS
      # Veamos si es posible que exista una configuracion de compartimentos adecuada
      for (i in 1:nf){    #recorremos todos los tipos de pienso
        while(CWTTRP_struct$demandas_res[pos$Positionfilas,i]!=0 &&
              sum(CWTTRP_struct$H.camion_res[s,])!=-dim(input$H.camion)[2]){
          while (CWTTRP_struct$demandas_res[pos$Positionfilas,i]>max(input$H.camion) &&
                 sum(CWTTRP_struct$H.camion_res[s,])!=-dim(input$H.camion)[2]){   #(al ser homogeneo da igual q ponga el max o no)

            if(Tolvas[t-1,1]==0 && t>2){
              t <- min(which(Tolvas[,1]==0))
            }
            Tolvas[t,] <- c(pos$Positionfilas-1,i,"truck",s,max(input$H.camion),1)
            CWTTRP_struct$demandas_res[pos$Positionfilas,i] <- CWTTRP_struct$demandas_res[pos$Positionfilas,i] - max(input$H.camion)

            # este s lo voy a tener que cambiar en algun momento
            CWTTRP_struct$H.camion_res[s,tc] <- -1

            if (tc < dim(input$H.camion)[2]){
              tc <- tc+1
            }else{
              tc <- 1
            }

            t <- t+1
          }

          if(sum(CWTTRP_struct$H.camion_res[s,])!=-dim(input$H.camion)[2]){

            if(Tolvas[t-1,1]==0 && t>2){
              t <- min(which(Tolvas[,1]==0))
            }

            # pongo max(input$H.camion) porque al ser homogenea me da igual
            Tolvas[t,] <- c(pos$Positionfilas-1,i,"truck",s,CWTTRP_struct$demandas_res[pos$Positionfilas,i],CWTTRP_struct$demandas_res[pos$Positionfilas,i]/max(input$H.camion))

            # a lo anterior le puedo poner 0 directamente creo
            CWTTRP_struct$demandas_res[pos$Positionfilas,i] <-
              max(0,CWTTRP_struct$demandas_res[pos$Positionfilas,i] - max(input$H.camion))

            # este s lo voy a tener que cambiar en algun momento
            CWTTRP_struct$H.camion_res[s,tc] <- -1

            if (tc < dim(input$H.camion)[2]){
              tc <- tc+1
            }else{
              tc <- 1
            }
            t <- t+1
          }

        }

      }
      # aqui acaba el bucle "for (i in 1:nf)" para el cliente t.c.


      for (i in 1:nf){
        while(CWTTRP_struct$demandas_res[pos$Positioncolumnas,i]!=0 && sum(CWTTRP_struct$H.camion_res[s,])!=-dim(input$H.camion)[2]){
          while (CWTTRP_struct$demandas_res[pos$Positioncolumnas,i] > max(input$H.camion) && sum(CWTTRP_struct$H.camion_res[s,])!=-dim(input$H.camion)[2]){
            if(Tolvas[t-1,1]==0 && t>2){
              t <- min(which(Tolvas[,1]==0))
            }
            Tolvas[t,] <- c(pos$Positioncolumnas-1,i,"truck",s,max(input$H.camion),1)
            CWTTRP_struct$demandas_res[pos$Positioncolumnas,i] <- CWTTRP_struct$demandas_res[pos$Positioncolumnas,i]-max(input$H.camion)

            CWTTRP_struct$H.camion_res[s,tc] <- -1

            if (tc < dim(input$H.camion)[2]){
              tc <- tc+1
            }else{
              tc <- 1
            }
            t <- t+1

          }

          if(sum(CWTTRP_struct$H.camion_res[s,])!=-dim(input$H.camion)[2]){
            if(Tolvas[t-1,1]==0 && t>2){
              t <- min(which(Tolvas[,1]==0))
            }
            Tolvas[t,] <- c(pos$Positioncolumnas-1,i,"truck",s,CWTTRP_struct$demandas_res[pos$Positioncolumnas,i],CWTTRP_struct$demandas_res[pos$Positioncolumnas,i]/max(input$H.camion))
            CWTTRP_struct$demandas_res[pos$Positioncolumnas,i] <- max(0,CWTTRP_struct$demandas_res[pos$Positioncolumnas,i]-max(input$H.camion))
            CWTTRP_struct$H.camion_res[s,tc] <- -1
            if (tc <= dim(input$H.camion)[2]){
              tc <- tc+1
            }else{
              tc <- 1
            }
            t <- t+1
          }
        }

      }

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

    }else{
      S[pos$Positionfilas,pos$Positioncolumnas]<-0 #Si no es factible tambien lo borramos
      Shat[pos$Positionfilas,pos$Positioncolumnas]<-0
    }
  }

  if (flag_stop == 0 || flag_stop == 1){
    if(CWTTRP_struct$CargaT<=input$capacidad.truck && x==0){ #Anhadimos la ruta si es factible en cuanto a capacidad
      if (sum(CWTTRP_struct$demandas_res[pos$Positionfilas,])+
          (sum(CWTTRP_struct$demandas_res[pos$Positioncolumnas,]))==0){ # Anhadimos la ruta si es factible en cuanto a compartimentos

        merge <- 1
        R[pos$Positionfilas,3]<-(pos$Positioncolumnas-1)
        R[pos$Positioncolumnas,1]<-(pos$Positionfilas-1)


        # Debug Output
        if (verbose == 1) {
          print(paste("Iteration ", CWTTRP_struct$iter))
          print(paste("Seleccionamos ahorros ", pos$Positionfilas, "-", pos$Positioncolumnas))
          print(R)
          print(paste("Carga ", CWTTRP_struct$CargaT, "capacidad.truck", input$capacidad.truck[length(input$capacidad.truck)]))
          print("Matriz de Tolvas")
          print(Tolvas)
        }

        S[pos$Positionfilas,pos$Positioncolumnas]<-0
        #Borramos ahorros utilizados para evitar ciclos
        S[pos$Positioncolumnas,pos$Positionfilas]<-0
        Shat[pos$Positionfilas,pos$Positioncolumnas]<-0
        #Borramos ahorros analogos en la matriz Shat, porque obviamente no se
        # puede implementar esta ruta
        Shat[pos$Positioncolumnas,pos$Positionfilas]<-0

      }
      #si no se han encontrado Tolvas para servir a los clientes, "deshago"
      else{

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

        if(n.trailer_i==0 && n.truck_i==0 && flag_stop_i == 2){
          CWTTRP_struct$demandas_res[pos$Positionfilas,] <- input$matriz.demandas[pos$Positionfilas,]
          if (sum(Tolvas[,1]==pos$Positionfilas-1)>0){
            for (k in which(Tolvas[,1]==pos$Positionfilas-1)){
              delete.tol <- delete.tol + 1
              Tolvas[k,] <- numeric(dim(Tolvas)[2])}
          }
        }

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

        # Pongo s-1 porq si hemos llegado hasta aqu? seguramente hemos hecho
        # en algun momento s<-s+1
        if(delete.tol>0){
          CWTTRP_struct$H.camion_res[s-1,] <- c(CWTTRP_struct$H.camion_res[s-1,1:(dim(input$H.camion)[2]-delete.tol)],input$H.camion[s-1,(dim(input$H.camion)[2]-delete.tol+1):dim(input$H.camion)[2]])
        }

        #Si no es factible tambien lo borramos
        S[pos$Positionfilas,pos$Positioncolumnas]<-0
        Shat[pos$Positionfilas,pos$Positioncolumnas]<-0

      }

    }
  }
  else{
    S[pos$Positionfilas,pos$Positioncolumnas]<-0 #Si no es factible tambien lo borramos
    Shat[pos$Positionfilas,pos$Positioncolumnas]<-0
  }

  result <- list()
  result$CWTTRP_struct <- CWTTRP_struct
  result$Tolvas <- Tolvas
  result$R <- R
  result$Rhat <- Rhat
  result$S <- S
  result$Shat <- Shat
  result$t <- t
  result$s <- s
  result$tc <- tc
  result$tt <- tt
  result$ss <- ss
  result$merge <- merge


  return(result)
}

# SmInS_VcTc
SmInS_VcTc<-function(CWTTRP_struct, R, Rhat, S, Shat, input, debug){

  merge <- 0
  condition1 <- !check_in_parking_list(CWTTRP_struct, CWTTRP_struct$pos$Positionfilas,
                                      R, input, "left")

  condition2 <- check_pvr(CWTTRP_struct$pos$Positionfilas, R, input, "left")

  if(R[CWTTRP_struct$pos$Positionfilas,3]==0 &&
     R[CWTTRP_struct$pos$Positioncolumnas,1]==0 &&
     Rhat[CWTTRP_struct$pos$Positionfilas,3]==0 &&
     Rhat[CWTTRP_struct$pos$Positioncolumnas,1]==0 &&
     CWTTRP_struct$CargaT<=input$capacidad.truck && condition1 && condition2){

    CWTTRP_struct$new$Positionfilas<-CWTTRP_struct$pos$Positionfilas
    CWTTRP_struct$new$Positioncolumnas<-CWTTRP_struct$pos$Positioncolumnas
    x<-0 #Evitamos ciclos

    tc_load_route1 <- 0
    vc_load_route1 <- input$vector.demandas[CWTTRP_struct$pos$Positionfilas]
    tc_load_route2 <- input$vector.demandas[CWTTRP_struct$pos$Positioncolumnas]
    vc_load_route2 <- 0

    is_parking <- 0

    result<-addWorkload(input, R, CWTTRP_struct, CWTTRP_struct$new$Positionfilas,
                        CWTTRP_struct$pos$Positioncolumnas, x, option="row")

    x <- result$flag
    #is_parking <- is_parking + result$is_parking
    is_parking <- result$is_parking
    index_parking <- result$index_parking
    tc_load_route1 <- tc_load_route1 + result$tc_load_route
    vc_load_route1 <- vc_load_route1 + result$vc_load_route
    CWTTRP_struct$new$Positionfilas <- result$pos1

    if (is_parking == 1) {
      result <- addWorkload_parking(input, Rhat, index_parking)
      vc_load_route1 <- vc_load_route1 + result$vc_load_subroute
      tc_load_route1 <- tc_load_route1 + result$tc_load_subroute
    }

    result<-addWorkload(input, R, CWTTRP_struct, CWTTRP_struct$new$Positioncolumnas,
                        CWTTRP_struct$pos$Positionfilas, x, option="col")

    x <- x + result$flag
    #is_parking <- is_parking + result$is_parking
    is_parking <- result$is_parking
    index_parking <- result$index_parking
    tc_load_route2 <- tc_load_route2 + result$tc_load_route
    vc_load_route2 <- vc_load_route2 + result$vc_load_route
    CWTTRP_struct$new$Positioncolumnas <- result$pos1

    if (is_parking == 1) {
      result <- addWorkload_parking(input, Rhat, index_parking)
      vc_load_route2 <- vc_load_route2 + result$vc_load_subroute
      tc_load_route2 <- tc_load_route2 + result$tc_load_subroute
    }

        result <- load_manager(CWTTRP_struct, input, vc_load_route1 , vc_load_route2 ,
                               tc_load_route1 , tc_load_route2 , 0, 0, 0, 0)

        unfeasibility <- result$unfeasibility
        capacity <- result$capacity
        load_truck <- result$load_truck
        CWTTRP_struct <- result$CWTTRP_struct

    if(unfeasibility<=0 && x==0 ){

          merge <- 1
      R[CWTTRP_struct$pos$Positionfilas,3]<-(CWTTRP_struct$pos$Positioncolumnas-1)
      R[CWTTRP_struct$pos$Positioncolumnas,1]<-(CWTTRP_struct$pos$Positionfilas-1)
      string <- paste0("add R")

    }
    else  {
      string <- paste0("not add R / ", CWTTRP_struct$CargaT, " / ",
                       input$capacidad.truck, " / ", x)    }

    # Debug Output
    if (debug==1){
      print(paste("Iteration ", CWTTRP_struct$iter))
      print(paste("Seleccionamos ahorros ", CWTTRP_struct$pos$Positionfilas, "-", CWTTRP_struct$pos$Positioncolumnas))
      print(R)
      print(paste("Carga ", CWTTRP_struct$CargaT, "capacidad.truck", input$capacidad.truck))
    }
    S[CWTTRP_struct$pos$Positionfilas,CWTTRP_struct$pos$Positioncolumnas]<-0
    S[CWTTRP_struct$pos$Positioncolumnas,CWTTRP_struct$pos$Positionfilas]<-0 #Borramos ahorros utilizados para evitar ciclos
    Shat[CWTTRP_struct$pos$Positionfilas,CWTTRP_struct$pos$Positioncolumnas]<-0
    Shat[CWTTRP_struct$pos$Positioncolumnas,CWTTRP_struct$pos$Positionfilas]<-0   #Borramos ahorros analogos en la matriz Shat, porque obviamente no se puede implementar esta ruta

  }
  S[CWTTRP_struct$pos$Positionfilas,CWTTRP_struct$pos$Positioncolumnas]<-0 #Si no es factible tambien lo borramos

  result <- list()
  result$CWTTRP_struct <- CWTTRP_struct
  result$R <- R
  result$S <- S
  result$Shat <- Shat
  result$merge <- merge

  return(result)
}
