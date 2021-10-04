SmInS_TcTc_with_hoppers<-function(CWTTRP_struct, Tolvas, R, Rhat, S, Shat, input,
                                  pos, n, nf, verbose){

  t <- CWTTRP_struct$t
  s <- CWTTRP_struct$s
  tc <- CWTTRP_struct$tc
  tt <- CWTTRP_struct$tt
  ss <- CWTTRP_struct$ss
  merge <- 0

  if(R[pos$Positionfilas,3]==0 && Rhat[pos$Positionfilas,3]==0 &&
     R[pos$Positioncolumnas,1]==0 && Rhat[pos$Positioncolumnas,1]==0 &&
     CWTTRP_struct$CargaT<=input$capacidad.truck){

    CWTTRP_struct$newPositionfilas<-pos$Positionfilas
    CWTTRP_struct$newPositioncolumnas<-pos$Positioncolumnas
    # si hay clientes anteriores a i, esto sera distinto de 0 porque entraremos en el primer while
    a <- 0
    # si hay clientes posteriores a j, esto sera distinto de 0, porque entraremos en el segundo while
    b <- 0
    n.truck_i <- 0
    n.truck_j <- 0
    n.trailer_i <- 0
    n.trailer_j <- 0
    #Evitamos ciclos
    x<-0

    result_sub <- addWorkload_R(R, Tolvas, input, CWTTRP_struct$newPositionfilas,
                  pos$Positioncolumnas, CWTTRP_struct$CargaT, n.truck_i,
                  n.trailer_i, a, x, "row")

    CWTTRP_struct$CargaT <- result_sub$CargaT
    CWTTRP_struct$newPositionfilas <- result_sub$pos1
    n.truck_i <- result_sub$n.truck
    n.trailer_i <- result_sub$n.trailer
    a <- result_sub$a1
    x <- result_sub$a2

    result_sub <- addWorkload_R(R, Tolvas, input, CWTTRP_struct$newPositioncolumnas,
                                pos$Positionfilas, CWTTRP_struct$CargaT, n.truck_j,
                                n.trailer_j, b, x, "col")

    CWTTRP_struct$CargaT <- result_sub$CargaT
    CWTTRP_struct$newPositioncolumnas <- result_sub$pos1
    n.truck_j <- result_sub$n.truck
    n.trailer_j <- result_sub$n.trailer
    b <- result_sub$a1
    x <- result_sub$a2

    flag_stop <- 0

    result_sub <- SmInS_TcTc_control_fusion(CWTTRP_struct, Tolvas, R,
                                            input, n.trailer_i, n.truck_i,
                                            n.trailer_j, n.truck_j, s,
                                            flag_stop, verbose)
    CWTTRP_struct <- result_sub$CWTTRP_struct
    Tolvas <- result_sub$Tolvas
    s <- result_sub$s
    flag_stop <- result_sub$flag_stop

    result_sub <- SmInS_TcTc_apply_fusion_with_hoppers(CWTTRP_struct, Tolvas, R, Rhat, S,
                                                   Shat, input, n.truck_i, n.trailer_i,
                                                   n.truck_j, n.trailer_j, pos, n,
                                                   x, a, b, t, s, tc, tt, ss,
                                                   flag_stop, nf, verbose)
    CWTTRP_struct <- result_sub$CWTTRP_struct
    t <- CWTTRP_struct$t
    s <- CWTTRP_struct$s
    tc <- CWTTRP_struct$tc
    tt <- CWTTRP_struct$tt
    ss <- CWTTRP_struct$ss
    
    Tolvas <- result_sub$Tolvas
    R <- result_sub$R
    Rhat <- result_sub$Rhat
    S <- result_sub$S
    Shat <- result_sub$Shat
    merge <- result_sub$merge

  }
  #Si no es factible tambien lo borramos
  S[pos$Positionfilas,pos$Positioncolumnas]<-0
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

SmInS_TcTc_control_fusion<-function(CWTTRP_struct, Tolvas, R,
                                    input, n.trailer_i, n.truck_i,
                                    n.trailer_j, n.truck_j, s,
                                    flag_stop, verbose){

  # Si uno de los camiones ya esta lleno, pues no podemos hacer nada:
  # Con este if lo que digo es que al menos se esta utilizando un truck,
  # y no los dos a la vez
  if( (n.truck_i!=0 || n.truck_j!=0) && n.truck_i*n.truck_j==0){
    if(n.truck_i!=0){
      s <- n.truck_i;
      s <- as.numeric(s)
    }
    if(n.truck_j!=0){
      s <- n.truck_j;
      s <- as.numeric(s)
    }
    # Tomamos como "s" el truck que se esta usando en alguna de las rutas.
    # Si est? lleno, pasamos
    if(sum(CWTTRP_struct$H.camion_res[s,])==-dim(input$H.camion)[2]){
      flag_stop <- 2
    }
  }

  # Si ambos clientes est?n en PTR's, podemos ver si se pueden fusionar
  if(n.truck_i!=0 && n.truck_j!=0 && n.truck_i!=n.truck_j &&
     n.trailer_i==0 && n.trailer_j==0 &&
     sum(CWTTRP_struct$H.camion_res[n.truck_i,]==-1)+
     sum(CWTTRP_struct$H.camion_res[n.truck_j,]==-1)>dim(input$H.camion)[2]){

    # no hago nada, esto no se puede dar
    flag_stop <- 2

  }

  # aqui lo que tendria que hacer es meter la mercancia de los clientes q estaban utilizando el truck "mas libre" en
  # el truck "mas ocupado"
  else if(n.truck_i!=0 && n.truck_j!=0 &&
          n.truck_i!=n.truck_j && n.trailer_i==0 && n.trailer_j==0 &&
          sum(CWTTRP_struct$H.camion_res[n.truck_i,]==-1)+
          sum(CWTTRP_struct$H.camion_res[n.truck_j,]==-1)<=dim(input$H.camion)[2]){

    flag_stop <- 1

    #numero de Tolvas libres en n.camion_i ?necesario?
    tol.lib_i <- sum(CWTTRP_struct$H.camion_res[n.truck_i,]!=-1)
    #numero de Tolvas libres en n.camion_j ?necesario?
    tol.lib_j <- sum(CWTTRP_struct$H.camion_res[n.truck_j,]!=-1)

    if(sum(CWTTRP_struct$H.camion_res[n.truck_i,]==-1)<
       sum(CWTTRP_struct$H.camion_res[n.truck_j,]==-1)){

      # si el num de Tolvas ocupadas en n.truck_i
      # es menor que el num de Tolvas ocupadas en n.truck_j

      #truck_ocup <- truck mas ocupado
      truck_ocup <- n.truck_j

      #truck_lib <- truck mas liberado
      truck_lib <- n.truck_i

    }else{
      truck_ocup <- n.truck_i
      truck_lib <- n.truck_j
    }

    s.prueba <- truck_ocup
    # estas son las Tolvas libres en el truck ocupado
    tol.lib_ocup <- sum(CWTTRP_struct$H.camion_res[s.prueba,]!=-1)
    # la primera tolva libre del truck ocupado
    #ntol.lib_ocup <- min(which(CWTTRP_struct$H.camion_res[s.prueba,]!=-1))

    if(sum(CWTTRP_struct$H.camion_res[s.prueba,]!=-1)>0){
      # la primera tolva libre del trailer ocupado
      ntol.lib_ocup <- min(which(CWTTRP_struct$H.camion_res[s.prueba,]!=-1)) #
    }else{
      ntol.lib_ocup <- 0
    }
    #Tolvas ocupadas en el truck libre
    tol.ocup_lib <- sum(CWTTRP_struct$H.camion_res[truck_lib,]==-1)
    #ultima tolva ocupada en el truck libre (coincide con lo anterior)
    ntol.ocup_lib <- max(which(CWTTRP_struct$H.camion_res[truck_lib,]==-1))

    # Una vez llegados a esta situacion, yo se que tengo dos rutas, y las quiero fusionar. No necesito hacer
    # la movida de los compartimentos porque se supone que ya estan los clientes servidos. Solo necesito "reescribir"
    # lo que vendrian siendo las matrices de Tolvas, CWTTRP_struct$H.camion_res

    # ahora tendria que coger las Tolvas ocupadas del truck_lib y meterlas en este s.prueba (truck)

    # "vaciamos" el camion libre
    CWTTRP_struct$H.camion_res[truck_lib,] <-
      rep(input$H.camion[truck_lib,1],dim(input$H.camion)[2])
    # "llenamos" el camion ocupado
    CWTTRP_struct$H.camion_res[s.prueba,ntol.lib_ocup:(ntol.lib_ocup+tol.ocup_lib-1)] <-
      rep(-1,tol.ocup_lib)

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

  result <- list()
  result$CWTTRP_struct <- CWTTRP_struct
  result$Tolvas <- Tolvas
  result$s <- s
  result$flag_stop <- flag_stop

  return(result)
}

SmInS_TcTc_apply_fusion_with_hoppers<-function(CWTTRP_struct, Tolvas, R, Rhat, S,
                                               Shat, input, n.truck_i, n.trailer_i,
                                               n.truck_j, n.trailer_j, pos, n,
                                               x, a, b, t, s, tc, tt, ss,
                                               flag_stop, nf, verbose) {

  merge <- 0

  # Ahora entramos en el apartado de compartimentos solo si es posible
  if (flag_stop == 0){
    if (CWTTRP_struct$CargaT<=input$capacidad.truck && x==0){

      s.aux <- s
      ss.aux <- ss

      if(sum(Tolvas[,1]==(pos$Positionfilas-1))>0){
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

      # Lo mismo para el cliente j. Vemos si ya habia sido incluido en alguna ruta
      # con anterioridad. Y en tal caso, miramos de qu?
      if(sum(Tolvas[,1]==(pos$Positioncolumnas-1))>0){
        CWTTRP_struct$aux2 <- max(which(Tolvas[,1]==(pos$Positioncolumnas-1)))
        if(Tolvas[CWTTRP_struct$aux2,3]=="trailer"){
          ss <- Tolvas[CWTTRP_struct$aux2,4]
          ss <- as.numeric(ss)
        }
        else{
          s <- Tolvas[CWTTRP_struct$aux2,4]
          s <- as.numeric(s)
        }
      }
      

      # new in hoppers in truck
#      if(sum(Tolvas[,1]==(pos$Positionfilas-1))==0 &&
#         sum(Tolvas[,1]==(pos$Positioncolumnas-1))==0 &&
#         sum(CWTTRP_struct$H.trailer_res[ss.aux,])!=sum(input$H.trailer[ss.aux,]) &&
#          ss == ss.aux){
#          ss <- min(which(CWTTRP_struct$H.trailer_res!=-1))
#          ss <- as.numeric(ss)
#      }
      
      # new in hoppers in truck
      if(sum(Tolvas[,1]==(pos$Positionfilas-1))==0 &&
         sum(Tolvas[,1]==(pos$Positioncolumnas-1))==0){
          for (i in 1:length(CWTTRP_struct$H.trailer_res[,1])) {
            if ((sum(CWTTRP_struct$H.trailer_res[i,] == -1)  == 0) &&
              (sum(CWTTRP_struct$H.camion_res[i,] == -1)  == 0)) {
              ss <- i
              s <- i
              break;
            }
          }
      }

      # En este momento ya tengo decidido cual es el camion y cual es el trailer
      # q voy a utilizar. Entonces aqui deberia tambien "actualizar" los valores tt y tc.
      if(sum(CWTTRP_struct$H.trailer_res[ss,])!=-dim(input$H.trailer)[2] &&
         ss!=0){
        tt <- min(which(CWTTRP_struct$H.trailer_res[ss,]!=-1))
      }

      if(sum(CWTTRP_struct$H.camion_res[s,])!=-dim(input$H.camion)[2] &&
         s!=0){
        tc <- min(which(CWTTRP_struct$H.camion_res[s,]!=-1))
      }

      # Veamos si es posible que exista una configuracion de compartimentos adecuada
      for (i in 1:nf){    #recorremos todos los tipos de pienso
        while(CWTTRP_struct$demandas_res[pos$Positionfilas,i]!=0 &&
              sum(CWTTRP_struct$H.camion_res[s,])!=-dim(input$H.camion)[2]){
          while (CWTTRP_struct$demandas_res[pos$Positionfilas,i]>
                 max(input$H.camion) && sum(CWTTRP_struct$H.camion_res[s,])!=-
                 dim(input$H.camion)[2]){

            #(al ser homogeneo da igual q ponga el max o no) si esto se cumple,
            # entonces la demanda de pienso i para este cliente no cabe en ninguna
            # tolva, luego relleno una de ellas y el resto lo meto en otra (aqui
            # al tener todas las Tolvas la misma capacidad, el problema se simplifica)
            if(Tolvas[t-1,1]==0 && t>2){
              t <- min(which(Tolvas[,1]==0))
            }

            Tolvas[t,] <- c(pos$Positionfilas-1,i,"truck",s,max(input$H.camion),1)
            CWTTRP_struct$demandas_res[pos$Positionfilas,i] <-
              CWTTRP_struct$demandas_res[pos$Positionfilas,i] - max(input$H.camion)
            # este s lo voy a tener que cambiar en algun momento
            CWTTRP_struct$H.camion_res[s,tc] <- -1

            if (tc < dim(input$H.camion)[2]){
              tc <- tc+1
            }
            else{
              tc <- 1
            }

            t <- t+1
          }
          # al llegar aqui sabemos que lo que falta de mercancia para este cliente
          # no cabe en una unica tolva; debo dejar claro que tc es como maximo 3
          # (ya que es el numero de Tolvas de mis camiones)
          if(Tolvas[t-1,1]==0 && t>2){
            t <- min(which(Tolvas[,1]==0))
          }

          if (sum(CWTTRP_struct$H.camion_res[s,])!=-dim(input$H.camion)[2]){
            Tolvas[t,] <- c(pos$Positionfilas-1,i,"truck",s,CWTTRP_struct$demandas_res[pos$Positionfilas,i],CWTTRP_struct$demandas_res[pos$Positionfilas,i]/max(input$H.camion))
            # pongo max(input$H.camion) porque al ser homogenea me da igual
            CWTTRP_struct$demandas_res[pos$Positionfilas,i] <-
              max(0,CWTTRP_struct$demandas_res[pos$Positionfilas,i] - max(input$H.camion))
            # a lo anterior le puedo poner 0 directamente creo
            # este s lo voy a tener que cambiar en algun momento
            CWTTRP_struct$H.camion_res[s,tc] <- -1

            if (tc < dim(input$H.camion)[2]){
              tc <- tc+1
            }
            else{
              tc <- 1
            }

            t <- t+1

          }
        }

        # if (sum(CWTTRP_struct$H.camion_res[s,])==-dim(input$H.camion)[2]){s <- s+1}
        # Si ya he rellenado este camion, paso a otro tambien tengo que indicar en
        # algun momento que, aunque no haya rellenado este camion, tambien tengo q
        # pasar a otro cuando cambio de ruta

      } # aqui acaba el bucle "for (i in 1:nf)" para el cliente t.c.

      for (i in 1:nf){
        while(CWTTRP_struct$demandas_res[pos$Positioncolumnas,i]!=0 &&
              sum(CWTTRP_struct$H.camion_res[s,])!=-dim(input$H.camion)[2] ){
          while (CWTTRP_struct$demandas_res[pos$Positioncolumnas,i] >
                 max(input$H.camion) && sum(CWTTRP_struct$H.camion_res[s,])!=
                 -dim(input$H.camion)[2]){
            if(Tolvas[t-1,1]==0 && t>2){
              t <- min(which(Tolvas[,1]==0))
            }

            Tolvas[t,] <- c(pos$Positioncolumnas-1,i,"truck",s,max(input$H.camion),1)
            CWTTRP_struct$demandas_res[pos$Positioncolumnas,i] <-
              CWTTRP_struct$demandas_res[pos$Positioncolumnas,i]-max(input$H.camion)

            CWTTRP_struct$H.camion_res[s,tc] <- -1

            if (tc < dim(input$H.camion)[2]){
              tc <- tc+1
            }
            else{
              tc <- 1
            }

            t <- t+1

          }

          if(sum(CWTTRP_struct$H.camion_res[s,])!=-dim(input$H.camion)[2]){

            if(Tolvas[t-1,1]==0 && t>2){
              t <- min(which(Tolvas[,1]==0))
            }

            Tolvas[t,] <- c(pos$Positioncolumnas-1,i,"truck",s,
                            CWTTRP_struct$demandas_res[pos$Positioncolumnas,i],
                            CWTTRP_struct$demandas_res[pos$Positioncolumnas,i]
                            /max(input$H.camion))

            CWTTRP_struct$demandas_res[pos$Positioncolumnas,i] <-
              max(0,CWTTRP_struct$demandas_res[pos$Positioncolumnas,i]-max(input$H.camion))

            CWTTRP_struct$H.camion_res[s,tc] <- -1

            if (tc <= dim(input$H.camion)[2]){
              tc <- tc+1
            }
            else{
              tc <- 1
            }
            t <- t+1
          }

        }
        # if (sum(CWTTRP_struct$H.camion_res[s,])==-dim(input$H.camion)[2]){s <- s+1} # Si ya he rellenado este camion, paso a otro
        # tambien tengo que indicar en algun momento que, aunque no haya rellenado este camion, tambien tengo q
        # pasar a otro cuando cambio de ruta

      }


      if (sum(CWTTRP_struct$H.trailer_res[ss,])==-dim(input$H.trailer)[2] &&
          ss <= dim(input$H.trailer)[1]){
        ss <- ss+1
        ss <- as.numeric(ss)
      }
      else if(ss > dim(input$H.trailer)[1]){
        ss <- 1
        ss <- as.numeric(ss)
      }

      if (sum(CWTTRP_struct$H.camion_res[s,])==-dim(input$H.camion)[2] && s <= dim(input$H.camion)[1]){
        s <- s+1
        s <- as.numeric(s)
      }else if(s > dim(input$H.camion)[1]){
        s <- 1
        s <- as.numeric(s)
      }

    }
    else{
      #Si no es factible tambien lo borramos
      S[pos$Positionfilas,pos$Positioncolumnas]<-0
      Shat[pos$Positionfilas,pos$Positioncolumnas]<-0
    }
  }


  if(flag_stop == 0 || flag_stop == 1){
    # Anhadimos la ruta si es factible en cuanto a capacidad
    if(CWTTRP_struct$CargaT<=input$capacidad.truck && x==0){
      # Anhadimos la ruta si es factible en cuanto a compartimentos
      if (sum(CWTTRP_struct$demandas_res[pos$Positionfilas,])+
          (sum(CWTTRP_struct$demandas_res[pos$Positioncolumnas,]))==0){
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
      else{
        # si no se han encontrado Tolvas para servir a los clientes, "deshago"
        # lo que haya hecho "mal"

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
        # (es decir, en alguna iteraci?n anterior), entonces n.truck_i=0,
        # de modo que deber?amos deshacer lo que se haya hecho en esta iteraci?n
        # con respecto a este cliente (pues no hemos sido capaces de servir a los
        # dos clientes)
        if(n.truck_i==0 && flag_stop_i == 2){
          CWTTRP_struct$demandas_res[pos$Positionfilas,] <- input$matriz.demandas[pos$Positionfilas,]
          if (sum(Tolvas[,1]==pos$Positionfilas-1)>0){
            for (k in which(Tolvas[,1]==pos$Positionfilas-1)){
              delete.tol <- delete.tol + 1
              Tolvas[k,] <- numeric(dim(Tolvas)[2])}
          }
        }

        # Si el cliente j no habia sido introducido en ninguna ruta anteriormente
        # (es decir, en alguna iteraci?n anterior), entonces n.truck_j=0,
        # de modo que deber?amos deshacer lo que se haya hecho en esta iteraci?n
        # con respecto a este cliente (pues no hemos sido capaces de servir a los
        # dos clientes)
        if(n.truck_j==0 && flag_stop_j == 2){
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

        # Pongo s-1 porq si hemos llegado hasta aqu? seguramente hemos hecho en
        # algun momento s<-s+1
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
    #Si no es factible tambien lo borramos
    S[pos$Positionfilas,pos$Positioncolumnas]<-0
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

SmInS_TcTc<-function(CWTTRP_struct, R, Rhat, S, Shat, input, debug){
  merge <- 0
  if(R[CWTTRP_struct$pos$Positionfilas,3]==0 &&
     R[CWTTRP_struct$pos$Positioncolumnas,1]==0 &&
     Rhat[CWTTRP_struct$pos$Positionfilas,3]==0 &&
     Rhat[CWTTRP_struct$pos$Positioncolumnas,1]==0 &&
     CWTTRP_struct$CargaT<=input$capacidad.truck){

    CWTTRP_struct$new$Positionfilas<-CWTTRP_struct$pos$Positionfilas
    CWTTRP_struct$new$Positioncolumnas<-CWTTRP_struct$pos$Positioncolumnas
    x<-0 #Evitamos ciclos


    tc_load_route1 <- input$vector.demandas[CWTTRP_struct$pos$Positionfilas]
    vc_load_route1 <- 0
    tc_load_route2 <- input$vector.demandas[CWTTRP_struct$pos$Positioncolumnas]
    vc_load_route2 <- 0

    result<-addWorkload(input, R, CWTTRP_struct, CWTTRP_struct$new$Positionfilas,
                        CWTTRP_struct$pos$Positioncolumnas, x, option="row")

    x <- result$flag
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

    #print(paste0(vc_load_route1, " ", vc_load_route2, " ", tc_load_route1, " ", tc_load_route2 ))
    #print(paste0("unfeasibility ", unfeasibility, " capacity ",  capacity, "load_truck ", load_truck ))

    if(unfeasibility<=0 && x==0 ){

      merge <- 1
      R[CWTTRP_struct$pos$Positionfilas,3]<-(CWTTRP_struct$pos$Positioncolumnas-1)
      R[CWTTRP_struct$pos$Positioncolumnas,1]<-(CWTTRP_struct$pos$Positionfilas-1)
      string <-  paste0("add R")

    }
    else  {
      string <- paste0("not add R / ", CWTTRP_struct$CargaT, " / ",
                       capacity, " / ", x)    }
    #print(string)

    # Debug Output
    if (debug==1){
      print(paste("Iteration ", CWTTRP_struct$iter))
      print(paste("Seleccionamos ahorros ", CWTTRP_struct$pos$Positionfilas, "-", CWTTRP_struct$pos$Positioncolumnas))
      print(R)
      print(paste("Carga ", CWTTRP_struct$CargaT, "capacidad.truck",
                  input$capacidad.truck))
    }
    S[CWTTRP_struct$pos$Positionfilas,CWTTRP_struct$pos$Positioncolumnas]<-0
    S[CWTTRP_struct$pos$Positioncolumnas,CWTTRP_struct$pos$Positionfilas]<-0
    #Borramos ahorros utilizados para evitar ciclos
    Shat[CWTTRP_struct$pos$Positionfilas,CWTTRP_struct$pos$Positioncolumnas]<-0
    Shat[CWTTRP_struct$pos$Positioncolumnas,CWTTRP_struct$pos$Positionfilas]<-0
    #Borramos ahorros analogos en la matriz Shat, porque obviamente no se puede implementar esta ruta

  }
  S[CWTTRP_struct$pos$Positionfilas,CWTTRP_struct$pos$Positioncolumnas]<-0 #Si no es factible tambien lo borramos
  Shat[CWTTRP_struct$pos$Positionfilas,CWTTRP_struct$pos$Positioncolumnas]<-0 #Si no es factible tambien lo borramos

  result <- list()
  result$CWTTRP_struct <- CWTTRP_struct
  result$R <- R
  result$S <- S
  result$Shat <- Shat
  result$merge <-merge

  return(result)
}

