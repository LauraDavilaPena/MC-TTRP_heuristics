#' The core algorithm of Clarke-and-Wright, to deal Multicompartimental
#' Truck and Trailer problem.
#'
#' @param matriz.demandas Demand matrix.
#' @param matriz.distancia Distance matrix.
#' @param capacidad.truck Array list.
#' @param capacidad.trailer Array list.
#' @param capacidad.vehiculo Array list.
#' @param H.camion Matrix with truck's hoppers.
#' @param H.trailer Matrix with trailer's hoppers.
#' @param n1 The position where the clients v.c. are started.
#' @param nf Number of hoppers.
#' @param verbose Verbosity variable.
#' @return A list of results, with the route, cost, hoppers state, truck and trailers used.
CW_MCTTRPcore<-function(matriz.demandas, matriz.distancia, capacidad.truck, capacidad.trailer,
                        capacidad.vehiculo, H.camion, H.trailer, n1, nf, n_trucks, n_trailers, verbose, descent){

  input<-createInputStruct_MCTTRP(matriz.demandas, matriz.distancia, capacidad.truck,
                           capacidad.trailer, capacidad.vehiculo, H.camion, H.trailer, 
                           n1, n_trucks, n_trailers)

  #numero de clientes mas deposito
  n<-dim(input$matriz.distancia)[1]
  #vector costes de rutas
  c<-numeric(n)
  #matriz de rutas
  R<-matrix(0,nrow=n,ncol=3)
  #matriz de subtours
  Rhat<-matrix(0,nrow=n,ncol=3)

  ##########Paso 1: calcular los ciclos iniciales (rutas ir y volver).

  #Coste ir desde el deposito al cliente i y volver
  c<-input$matriz.distancia[1,]*2
  #Coste total de ir desde cada deposito al cliente
  ctotal<-sum(c)
  #Generamos las rutas (0,i,0) donde 0 es deposito
  R[2:n,2]<-1:(n-1)
  #Generamos la matriz donde vamos a meter los subtours
  Rhat[2:n,2]<-1:(n-1)

  Tolvas <- matrix(0,nrow=dim(input$H.camion)[1]*dim(input$H.camion)[2]+
                     dim(input$H.trailer)[1]*dim(input$H.trailer)[2],ncol=6)
  Tolvas <- as.data.frame(Tolvas,stringsAsFactors=FALSE)
  colnames(Tolvas) <- c("Cliente","Pienso","Tipo_vehiculo","numero vehiculo",
                        "Cantidad","Proporcion")

  
  
  ############Paso 2: calcular las matrices de ahorros
  S<-matrixS(input$matriz.distancia,n)
  Shat<-matrixShat(input$matriz.distancia,n,n1)

  CWTTRP_struct <- createCWMCTTRPStruct(input)

  ###########Paso 3: optimizar rutas
  indicar<-1
  #Valores de entrada del primer while
  Sm<-1

  #Mientras existan ahorros mayores que cero buscamos rutas factibles
  while(Sm>0){

    Sm<-max(max(Shat),max(S)) #Escogemos el valor maximo de la matriz de ahorros
    merge <- 0

    if (verbose == 1) {
      print(Sm)
    }

    if(Sm>0){

      if (verbose == 1) {
        print(CWTTRP_struct$iter)
      }

      #Coordenadas de Sm en S o Shat que Sm estea en Shat
      if (sum(S==Sm)==0) {
        pos = positionSm(Shat,Sm,n)
        if (verbose == 1) {
          print(paste("El maximo esta en la matriz Shat, en la posicion",
                      "(",pos$Positionfilas,",",pos$Positioncolumnas,")"))
        }
        #Demandas de los clientes i y j de Sm
        CWTTRP_struct$CargaT<-
                sum(input$matriz.demandas[pos$Positionfilas,])+
                sum(input$matriz.demandas[pos$Positioncolumnas,])

        #Indicamos a que cliente visitamos antes de ir a i y despues de ir a j:
        # Asumimos que i es t.c. y j es v.c.

        if (sum(pos$Positionfilas==((n1+2):n))==1 &&
            sum(pos$Positioncolumnas==(2:(n1+1)))==1) {

          case <- "SmInShat_TcVc"
          subroutine_result <- SmInShat_TcVc_with_hoppers(CWTTRP_struct, Tolvas,
                                                          R, Rhat, S, Shat, input,
                                                          pos, n, n1, nf, verbose)

          CWTTRP_struct <- subroutine_result$CWTTRP_struct
          R <- subroutine_result$R
          Rhat <- subroutine_result$Rhat
          S <- subroutine_result$S
          Shat <- subroutine_result$Shat
          Tolvas <- subroutine_result$Tolvas
          n  <- subroutine_result$n
          if (subroutine_result$merge == 1) {merge <- "success";} else {merge <- "fail"}
        } #fin del caso en el q i es t.c. y j es v.c.



        # Asumimos que i es v.c. y j es t.c.
        if (sum(pos$Positionfilas==(2:(n1+1)))==1 &&
            sum(pos$Positioncolumnas==((n1+2):n))==1) {

          case <- "SmInShat_VcTc"
          subroutine_result <- SmInShat_VcTc_with_hoppers(CWTTRP_struct, Tolvas, R,
                                                          Rhat, S, Shat, input, pos,
                                                          n, n1, nf, verbose)

          CWTTRP_struct <- subroutine_result$CWTTRP_struct
          R <- subroutine_result$R
          Rhat <- subroutine_result$Rhat
          S <- subroutine_result$S
          Shat <- subroutine_result$Shat
          Tolvas <- subroutine_result$Tolvas
          n  <- subroutine_result$n
          if (subroutine_result$merge == 1) {merge <- "success";} else {merge <- "fail"}
        }

      }
      else {

        pos = positionSm(S,Sm,n)

        if (verbose == 1) {
          print(paste("El maximo esta en la matriz S, en la posicion", "(",
                      pos$Positionfilas,",",pos$Positioncolumnas,")"))
        }
        #Demandas de los clientes i y j de Sm
        CWTTRP_struct$CargaT<-sum(input$matriz.demandas[pos$Positionfilas,]) +
                              sum(input$matriz.demandas[pos$Positioncolumnas,])


        # si ambos clientes son de tipo v.c.
        if (sum(pos$Positionfilas==(2:(n1+1)))==1 &&
            sum(pos$Positioncolumnas==(2:(n1+1)))==1) {

            case <- "SmInS_VcVc"
            subroutine_result <- SmInS_VcVc_with_hoppers(CWTTRP_struct, Tolvas,
                                                         R, Rhat, S, Shat, input,
                                                         pos, n, nf, n1, verbose)

            CWTTRP_struct <- subroutine_result$CWTTRP_struct
            R <- subroutine_result$R
            Rhat <- subroutine_result$Rhat
            S <- subroutine_result$S
            Shat <- subroutine_result$Shat
            Tolvas <- subroutine_result$Tolvas
            n  <- subroutine_result$n
            if (subroutine_result$merge == 1) {merge <- "success";} else {merge <- "fail"}

        }

        # Si los dos clientes son de tipo t.c.
        if (sum(pos$Positionfilas==((n1+2):n))==1 &&
            sum(pos$Positioncolumnas==((n1+2):n))==1) {

          case <- "SmInS_TcTc"
          subroutine_result <- SmInS_TcTc_with_hoppers(CWTTRP_struct, Tolvas, R,
                                                       Rhat, S, Shat, input, pos,
                                                       n, nf, verbose)

          CWTTRP_struct <- subroutine_result$CWTTRP_struct
          R <- subroutine_result$R
          Rhat <- subroutine_result$Rhat
          S <- subroutine_result$S
          Shat <- subroutine_result$Shat
          Tolvas <- subroutine_result$Tolvas
          n  <- subroutine_result$n
          if (subroutine_result$merge == 1) {merge <- "success";} else {merge <- "fail"}
          
        }



        # Si uno de los clientes es de tipo v.c. y el otro de tipo t.c.
        # Asumimos que i es t.c. y j es v.c.
        if (sum(pos$Positionfilas==((n1+2):n))==1 && sum(pos$Positioncolumnas==(2:(n1+1)))==1) {

          case <- "SmInS_TcVc"
          subroutine_result <- SmInS_TcVc_with_hoppers(CWTTRP_struct, Tolvas, R,
                                                       Rhat, S, Shat, input, pos,
                                                       n, nf, verbose)

          CWTTRP_struct <- subroutine_result$CWTTRP_struct
          R <- subroutine_result$R
          Rhat <- subroutine_result$Rhat
          S <- subroutine_result$S
          Shat <- subroutine_result$Shat
          Tolvas <- subroutine_result$Tolvas
          n  <- subroutine_result$n
          if (subroutine_result$merge == 1) {merge <- "success";} else {merge <- "fail"}
        }

        # Asumimos que i es v.c. y j es t.c.
        if (sum(pos$Positionfilas==(2:(n1+1)))==1 && sum(pos$Positioncolumnas==((n1+2):n))==1) {

          case <- "SmInS_VcTc"
          subroutine_result <- SmInS_VcTc_with_hoppers(CWTTRP_struct, Tolvas, R,
                                                       Rhat, S, Shat, input, pos,
                                                       n, nf, verbose)

          CWTTRP_struct <- subroutine_result$CWTTRP_struct
          R <- subroutine_result$R
          Rhat <- subroutine_result$Rhat
          S <- subroutine_result$S
          Shat <- subroutine_result$Shat
          Tolvas <- subroutine_result$Tolvas
          n  <- subroutine_result$n
          if (subroutine_result$merge == 1) {merge <- "success";} else {merge <- "fail"}
        }


      }
      if (verbose > 1) {
        print(paste0("Iter: ", CWTTRP_struct$iter, " row: ", CWTTRP_struct$pos$Positionfilas,
                     " col: " ,CWTTRP_struct$pos$Positioncolumnas,
                     " Sm: "  , Sm, " Case: ", case, " Merge? ", merge))
      }
      CWTTRP_struct$iter=CWTTRP_struct$iter+1
    }

  }  #Fin del while

  rutas <- return_route_MCTTRP(CWTTRP_struct, Tolvas, R, Rhat, n, n1, verbose)
  rutas <- delete_dupl_zeros_route(rutas)
  
  ##################################################################################
  # POSTPROCESSING
  rutas_res <- postproc_MCTTRP(rutas, input, R, Rhat, Tolvas, CWTTRP_struct, nf)
  rutas <- all_routes(rutas_res)

  ##################################################################################
  # IMPROVING
   coste.total<-0
   for(i in 1:(length(rutas)-1)){ coste.total<-coste.total+input$matriz.distancia[rutas[i]+1,rutas[i+1]+1]}
   result <- createResultStruct_MCTTRP(rutas, coste.total, CWTTRP_struct$H.camion_res, CWTTRP_struct$H.trailer_res,
                                       input$matriz.demandas, rutas_res, input)
   rutas_res <- result$result_res
   if(descent) rutas_res <- descent_search(input, rutas_res, "MCTTRP")
   
   rutas <- all_routes(rutas_res)
   rutas <- convert_in_route(rutas_res)
   rutas <- delete_dupl_zeros_route(rutas)
  
  # extract data
  coste.total<-0
  for(i in 1:(length(rutas)-1)){
    coste.total<-coste.total+input$matriz.distancia[rutas[i]+1,rutas[i+1]+1]
  }
  result <- createResultStruct_MCTTRP(rutas, coste.total, CWTTRP_struct$H.camion_res, CWTTRP_struct$H.trailer_res,
                                      input$matriz.demandas, rutas_res, input)

  #rutas <- convert_in_route(rutas_res)
  
  analyse(rutas, input, result$result_res, "MCTTRP")
  print(paste0("l  ---> ", length(unique(rutas))))
  print(paste0("n  ---> ", input$n))
  print(paste0("n1 ---> ", input$n1))
  
  return(result)
} #Fin de la funcion



