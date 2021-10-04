MCTTRP_opt_solver<-function(string, option, n_trucks, n_trailers, total_time=3600, total_iterations=1000, vtr=-Inf, verbose=1, seed=100){
  
###################################
    dyn.load("src/mcttrpalns.so")
    library(Rcpp)
###################################
    
    set.seed(seed)

    init_time <- Sys.time()
  
    print("INIT CONSTRUCTIVE ALGORITHM")
    results_CW <- CW_algorithm(string, option, n_trucks, n_trailers, verbose)
    print("END CONSTRUCTIVE ALGORITHM")
    
    initial_solution  <- results_CW$result_res
    input <- results_CW$input
    input$max_time <- total_time
    input$max_iter <- total_iterations
    input$vtr <- vtr

    
    if (option == "MCTTRP") {
      input$H.camion_res <- results_CW$H.camion_res
      input$H.trailer_res <- results_CW$H.trailer_res
    }
    
    print(paste0("START ITERATIVE LOCAL SEARCH (time ", difftime(Sys.time(), init_time, units = "secs"),"s)"))
    result_MCTTRP_opt <- MCTTRP_opt_method(results_CW, initial_solution, input, init_time, option, seed)
    print("END ITERATIVE LOCAL SEARCH")
    
    # Calc final results
    end_route <- convert_in_route(result_MCTTRP_opt)
    end_route <- delete_dupl_zeros_route(end_route)
    if (option == "MCTTRP") {
      total_cost<-0
      for(tt in 1:(length(end_route)-1)){
        total_cost <- total_cost + input$matriz.distancia[end_route[tt]+1,end_route[tt+1]+1]
      }
      result <- createResultStruct_MCTTRP(end_route, total_cost,
                                          results_CW$H.camion_res,
                                          results_CW$H.trailer_res,
                                          input$matriz.demandas, result_MCTTRP_opt, input)
    }
    if (option == "TTRP") {
      total_cost<-0
      for(tt in 1:(length(end_route)-1)){
        total_cost <- total_cost + input$matriz.distancia[end_route[tt]+1,end_route[tt+1]+1]
      }
      result <- createFinalResult_TTRP(end_route, total_cost, input$matriz.distancia, result_MCTTRP_opt, 
                                       input$vector.demandas, input)
    }
    
    totaltime <- difftime(Sys.time(), init_time, units = "secs")
    print(paste0("ANALYZING ERRORS IN FINAL ROUTE (time ", difftime(Sys.time(), init_time, units = "secs"),"s)"))
    result$time <- totaltime
    result$init_cost <- results_CW$cost
    analyse(result$route, input, result$result_res,  option)

    print("FINAL RESULTS")
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
    return(result)
}
  
