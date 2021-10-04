#' The main R command to call the algorithm
#'
#' @param string 'route of the input data"
#' @param option 'TTRP' or 'MCTTRP'
#' @param verbose Verbosity option
#' @return A list of results, with the route, cost, truck and trailers used.
#' @example
#' result <- CW_algorithm('instances/CHAO_MCTTRP_01.txt', 'MCTTRP', 0)
#' result <- CW_algorithm('instances/CHAO_TTRP_01.txt', 'TTRP', 0)
#' result <- CW_algorithm('instances/P1.csv', 'MCTTRP', 0)
CW_algorithm<-function(string, option, n_trucks, n_trailers, verbose, descent){
  
  if (verbose) {
    print("Initilize CW_algorithm")
  }
  options(show.error.locations = TRUE)
  options(error=traceback)
  options(max.print=2000)

  result <- list()
  if (option == 'TTRP') {
    if (verbose) {
      print("Reading file ...")
    }
    input_result <- input_TTRP( string )

    capacidad.truck <- input_result$capacidad.truck
    capacidad.trailer <- input_result$capacidad.trailer
    capacidad.vehiculo <- input_result$capacidad.vehiculo
    N1 <- input_result$N1
    n1 <- input_result$n1
    matriz.distancia <- input_result$matriz.distancia
    demand_vector_list <- input_result$matriz.demandas

    start_time <- Sys.time()
    if (verbose) {
      print("Call to CW_TTRP")
    }

    result <- CWTTRPcore(demand_vector_list,matriz.distancia,capacidad.truck,capacidad.trailer,
                         capacidad.vehiculo,n1,n_trucks, n_trailers,0,descent)
    if (verbose) {
      print("Solver finished with success")
    }

    totaltime <- difftime(Sys.time(), start_time, units = "secs")

    print("END CW Algorithm")
    print(paste0("Result: ", string))
    print(paste0("    Cost ", result$cost))
    print(paste0("    Time ", totaltime," seconds"))
    print(paste0("    N trucks ", result$n_trucks))
    print(paste0("    N trailers ", result$n_trailers))
    print(paste0("    PTR ", result$PTR))
    print(paste0("    PVR ", result$PVR))
    print(paste0("    CVR ", result$CVR))
    string_route <- "    Route: "
    for (i in 1:length(result$routes)) {
      string_route <- paste0(string_route, " ", result$routes[i])
    }
    print(string_route)
    print("")

  }
  else if (option == 'MCTTRP') {
    if (verbose) {
      print("Type of problem: MCTTRP")
      print("Reading file ...")
    }
    input_result <- input_MCTTRP( string )

    capacidad.truck <- input_result$capacidad.truck
    capacidad.trailer <- input_result$capacidad.trailer
    capacidad.vehiculo <- input_result$capacidad.vehiculo
    N1 <- input_result$N1
    n1 <- input_result$n1
    h.camion <- input_result$h.camion
    H.camion <- input_result$H.camion
    h.trailer <- input_result$h.trailer
    H.trailer <- input_result$H.trailer
    matriz.distancia <- input_result$matriz.distancia
    matriz.demandas <- input_result$matriz.demandas
    nf <- input_result$nf

    start_time <- Sys.time()
    if (verbose) {
      print("Call to CW_MCTTRP")
    }
    result<- CW_MCTTRPcore(matriz.demandas,matriz.distancia,capacidad.truck,capacidad.trailer,
                           capacidad.vehiculo,H.camion,H.trailer,n1,nf, n_trucks, n_trailers, verbose,descent)
    if (verbose) {
      print("Solver finished with success")
    }

    totaltime <- difftime(Sys.time(), start_time, units = "secs")
    print(paste0("CW Result: ", string))
    print(paste0("    Cost ", result$cost))
    print(paste0("    Time ", totaltime," seconds"))
    print(paste0("    N trucks ", result$n_trucks))
    print(paste0("    N trailers ", result$n_trailers))
    print(paste0("    PTR ", result$PTR))
    print(paste0("    PVR ", result$PVR))
    print(paste0("    CVR ", result$CVR))
    string_route <- "    Route: "
    for (i in 1:length(result$routes)) {
      string_route <- paste0(string_route, " ", result$routes[i])
    }
    print(string_route)
    print("")
  }
  else {
    print("ERROR[1]: Incorrect type of problem (TTRP or MCTTRP)")

  }


  return(result)

}
