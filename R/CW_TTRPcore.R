#' The core algorithm of Clarke-and-Wright, to deal Truck and Trailer problem.
#'
#' @param vector.demandas Demand clients vectors
#' @param matriz.distancia Distance matrix.
#' @param capacidad.truck Integer value with the capacity of the trucks
#' @param capacidad.trailer Integer value with the capacity of the trailers
#' @param capacidad.vehiculo Integer value with the capacity of the trucks+trailers
#' @param n1 The position where the clients t.c. are started.
#' @param verbose Verbosity variable
#' @return A list of results, with the route, cost, truck and trailers used.
CWTTRPcore<-function(vector.demandas,matriz.distancia,capacidad.truck,capacidad.trailer,
                     capacidad.vehiculo,n1,n_trucks, n_trailers, verbose, descent){


if (missing(verbose)){
  verbose = 0
}

input <- createInputStruct_TTRP(vector.demandas, matriz.distancia, capacidad.truck,
                                  capacidad.trailer, n1,n_trucks, n_trailers)

 #numero de clientes mas deposito
c<-numeric(input$n) #vector costes de rutas
R<-matrix(0,nrow=input$n,ncol=3) #matriz de rutas
Rhat<-matrix(0,nrow=input$n,ncol=3) #matriz de subtours

CWTTRP_struct<-createCWTTRPStruct()

##########Paso 1: calcular los ciclos iniciales (rutas ir y volver).
c<-matriz.distancia[1,]*2 #Coste ir desde el deposito al cliente i y volver
ctotal<-sum(c) #Coste total de ir desde cada deposito al cliente
R[2:input$n,2]<-1:(input$n-1) #Generamos las rutas (0,i,0) donde 0 es deposito
Rhat[2:input$n,2]<-1:(input$n-1) #Generamos la matriz donde vamos a meter los subtours

############Paso 2: calcular las matrices de ahorros
S<-matrixS(matriz.distancia,input$n, input$n1)
Shat<-matrixShat(matriz.distancia,input$n,input$n1)

Sm<-1 #Valores de entrada del primer while

while(Sm>0){ #Mientras existan ahorros mayores que cero buscamos rutas factibles

  Sm<-max(max(Shat),max(S)) #Escogemos el valor maximo de la matriz de ahorros

  merge <- 0

  if(Sm>0){
    #Coordenadas de Sm en S o Shat
    if (sum(S==Sm)==0) {

      CWTTRP_struct$pos <- positionSm(Shat,Sm,input$n)

      if (verbose==1){
        print(paste("El maximo esta en la matriz Shat, en la posicion", "(",CWTTRP_struct$pos$Positionfilas,",",CWTTRP_struct$pos$Positioncolumnas,")"))
      }

      #Demandas de los clientes i y j de Sm
      CWTTRP_struct$CargaT<-vector.demandas[CWTTRP_struct$pos$Positionfilas]+
        vector.demandas[CWTTRP_struct$pos$Positioncolumnas]

      # Indicamos a que cliente visitamos antes de ir a i y despues de ir a j:

      # Si uno de los clientes es de tipo v.c. y el otro de tipo t.c.
      # CASE 3 --> Asumimos que i es t.c. y j es v.c.
      if (sum(CWTTRP_struct$pos$Positionfilas==((input$n1+2):input$n))==1 &&
          sum(CWTTRP_struct$pos$Positioncolumnas==(2:(input$n1+1)))==1) {

        case <- "SmInShat_TcVc"
        result <- SmInShat_TcVc(CWTTRP_struct, R, Rhat, S, Shat, input, verbose)

        CWTTRP_struct <- result$CWTTRP_struct
        R <- result$R
        S <- result$S
        Shat <- result$Shat
        Rhat <- result$Rhat
        if (result$merge == 1) {merge <- "success";} else {merge <- "fail"}
      }

      # CASE 2 --> Asumimos que i es v.c. y j es t.c.
      else if (sum(CWTTRP_struct$pos$Positionfilas==(2:(input$n1+1)))==1 &&
               sum(CWTTRP_struct$pos$Positioncolumnas==((input$n1+2):input$n))==1) {

        case <- "SmInShat_VcTc"
        result <- SmInShat_VcTc(CWTTRP_struct, R, Rhat, S, Shat, input, verbose)

        CWTTRP_struct <- result$CWTTRP_struct
        R <- result$R
        S <- result$S
        Shat <- result$Shat
        Rhat <- result$Rhat
        if (result$merge == 1) {merge <- "success";} else {merge <- "fail"}

      }
      else {
        Shat[CWTTRP_struct$pos$Positionfilas,CWTTRP_struct$pos$Positioncolumnas]<-0
      }

    } # fin del else, donde digo que el maximo ahorro encontrado, Sm, esta en Shat
    else if (sum(S==Sm)>0){
      CWTTRP_struct$pos <- positionSm(S,Sm,input$n)

      if (verbose==1){
        print(paste("El maximo esta en la matriz S, en la posicion", "(",CWTTRP_struct$pos, ")"))
      }

      #Demandas de los clientes i y j de Sm
      CWTTRP_struct$CargaT<-vector.demandas[CWTTRP_struct$pos$Positionfilas]+
        vector.demandas[CWTTRP_struct$pos$Positioncolumnas]


      # CASE 1 --> si ambos clientes son de tipo v.c. CASE 1
      if (sum(CWTTRP_struct$pos$Positionfilas==(2:(input$n1+1)))==1 &&
          sum(CWTTRP_struct$pos$Positioncolumnas==(2:(input$n1+1)))==1) {
        case <-"SmInS_VcVc"


        result <- SmInS_VcVc(CWTTRP_struct, R, Rhat, S, Shat, input, verbose)

        CWTTRP_struct <- result$CWTTRP_struct
        R <- result$R
        S <- result$S
        Shat <- result$Shat
        if (result$merge == 1) {merge <- "success";} else {merge <- "fail"}

      }

      # CASE 2 --> Si los dos clientes son de tipo t.c.
      else if (sum(CWTTRP_struct$pos$Positionfilas==((input$n1+2):input$n))==1 &&
               sum(CWTTRP_struct$pos$Positioncolumnas==((input$n1+2):input$n))==1) {

        case <- "SmInS_TcTc"
        result <- SmInS_TcTc(CWTTRP_struct, R, Rhat, S, Shat, input, verbose)

        CWTTRP_struct <- result$CWTTRP_struct
        R <- result$R
        S <- result$S
        Shat <- result$Shat
        if (result$merge == 1) {merge <- "success";} else {merge <- "fail"}
      }

      # CASE 3 --> Si uno de los clientes es de tipo v.c. y el otro de tipo t.c.
      # Asumimos que i es t.c. y j es v.c.
      else if (sum(CWTTRP_struct$pos$Positionfilas==((input$n1+2):input$n))==1 &&
               sum(CWTTRP_struct$pos$Positioncolumnas==(2:(input$n1+1)))==1) {

        case <- "SmInS_TcVc"
        result <- SmInS_TcVc(CWTTRP_struct, R, Rhat, S, Shat, input, verbose)

        CWTTRP_struct <- result$CWTTRP_struct
        R <- result$R
        S <- result$S
        Shat <- result$Shat
        if (result$merge == 1) {merge <- "success";} else {merge <- "fail"}

      }

      # CASE 4 --> Asumimos que i es v.c. y j es t.c.
      else if (sum(CWTTRP_struct$pos$Positionfilas==(2:(input$n1+1)))==1 &&
               sum(CWTTRP_struct$pos$Positioncolumnas==((input$n1+2):input$n))==1) {

        case <- "SmInS_VcTc"
        result <- SmInS_VcTc(CWTTRP_struct, R, Rhat, S, Shat, input, verbose)

        CWTTRP_struct <- result$CWTTRP_struct
        R <- result$R
        S <- result$S
        Shat <- result$Shat
        if (result$merge == 1) {merge <- "success";} else {merge <- "fail"}

      }
      # a lo mejor, en el caso en el que considero los ahorros de la matriz S,
      # es suficiente con distinguir dos casos: uno en el que ambos clientes
      # son de tipo v.c.; y otro caso para el resto de posibilidades, ya que
      # entonces voy a crear una PTR.

    } # fin del else, donde digo que el maximo ahorro encontrado, Sm, esta en S
    else {
      S[CWTTRP_struct$pos$Positionfilas,CWTTRP_struct$pos$Positioncolumnas]<-0
      Shat[CWTTRP_struct$pos$Positionfilas,CWTTRP_struct$pos$Positioncolumnas]<-0
    }
    if (verbose > 1) {
    print(paste0("Iter: ", CWTTRP_struct$iter, " row: ", CWTTRP_struct$pos$Positionfilas,
                 " col: " ,CWTTRP_struct$pos$Positioncolumnas,
                 " Sm: "  , Sm, " Case: ", case, " Merge? ", merge))
    }

    CWTTRP_struct$iter=CWTTRP_struct$iter+1
  }

}  #Fin del while
rutas <- return_route_TTRP(CWTTRP_struct, R, Rhat, input$n, input$n1, verbose)
rutas <- delete_dupl_zeros_route(rutas)

rutas_res <- create_result_struct(rutas, input, "TTRP")

##################################################################################
# POSTPROCESSING
rutas_res <- postproc_TTRP(rutas_res, rutas, input, R, Rhat)
rutas <- all_routes(rutas_res)

##################################################################################
# IMPROVING
coste.total<-0
for(i in 1:(length(rutas)-1)){ coste.total<-coste.total+matriz.distancia[rutas[i]+1,rutas[i+1]+1] }
result <- createFinalResult_TTRP(rutas,  coste.total, matriz.distancia, rutas_res, vector.demandas, input)

if (descent) rutas_res <- descent_search(input, result$result_res, "TTRP")
rutas <- all_routes(rutas_res)

coste.total<-0
for(i in 1:(length(rutas)-1)){ coste.total<-coste.total+matriz.distancia[rutas[i]+1,rutas[i+1]+1] }
rutas <- convert_in_route(rutas_res)
rutas <- delete_dupl_zeros_route(rutas)

result <- createFinalResult_TTRP(rutas,  coste.total, matriz.distancia, rutas_res, vector.demandas, input)

analyse(rutas, input, result$result_res, "TTRP")
print(paste0("l  ---> ", length(unique(rutas))))
print(paste0("n  ---> ", input$n))
print(paste0("n1 ---> ", input$n1))

  return(result)
} #Fin de la funcion

