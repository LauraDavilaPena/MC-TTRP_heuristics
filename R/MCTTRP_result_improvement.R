#string <- "instances/CHAO_MCTTRP_01.txt"
#matriz.distancia <- input_MCTTRP(string)$matriz.distancia
#result <- CW_algorithm(string, "MCTTRP", 5, 3, 0)

#solution <- result$result_res

result_improvement <- function(input, solution, problem_type){
  
  solution <- update_solution(solution, input, problem_type)
  
  improved_solution <-list()

  for(r in 1:length(solution)){
    improved_solution[[r]] <- solution[[r]]

    continue <- 0
    if(solution[[r]]$type == "CVR"){
      if (length(solution[[r]]$main_tour)> 1) {
        route_to_improve <- solution[[r]]$main_tour
        continue <- 1
      } 
    }else{
      if (length(solution[[r]]$route)) {
        route_to_improve <- solution[[r]]$route
        continue <- 1
      } 
    }
    
    if (continue) {
      improved_method <- list()
      improved_route <- list()
      delta_improvements <- list()
  
      improved_route[[1]] <- two_opt(input, route_to_improve)
      improved_route[[2]] <- three_opt(input, route_to_improve)
      improved_route[[3]] <- four_opt_asterisk(input, route_to_improve)
      
      delta_improvements[[1]] <- calculateTotalDistance(input, improved_route[[1]]) - calculateTotalDistance(input, route_to_improve)
      delta_improvements[[2]] <- calculateTotalDistance(input, improved_route[[2]]) - calculateTotalDistance(input, route_to_improve)
      delta_improvements[[3]] <- calculateTotalDistance(input, improved_route[[3]]) - calculateTotalDistance(input, route_to_improve)
      
      if(any(delta_improvements!=0)){
        
          improvement_index <- which(delta_improvements==min(unlist(delta_improvements)))
          
          
          if(length(improvement_index)>1){
            chosen_improvement_index <- sample(improvement_index, 1)
          }else{
            chosen_improvement_index <- improvement_index
          }
          
          if(solution[[r]]$type != "CVR") {
            improved_solution[[r]]$route <- improved_route[[chosen_improvement_index]]
            improved_solution[[r]]$cost <- calculateTotalDistance(input, improved_solution[[r]]$route)
          }
          else {
            improved_solution[[r]]$main_tour <- improved_route[[chosen_improvement_index]]
          }
      }
    
    }
    
    if(solution[[r]]$type == "CVR"){

      subtours <- solution[[r]]$subtours
      kk <- 1
      
      for(k in 1:length(subtours)){
        if (length(subtours[[k]]$tour) > 3) { 
            improved_subtour <- list()
            delta_impr_subtour <- list()
            
            route_to_improve <- subtours[[k]]$tour[2:(length(subtours[[k]]$tour)-1)]
            
            improved_subtour[[1]] <- two_opt(input, route_to_improve)
            improved_subtour[[2]] <- three_opt(input, route_to_improve)
            improved_subtour[[3]] <- four_opt_asterisk(input, route_to_improve)
            
            
            delta_impr_subtour[[1]] <- calculateTotalDistance(input,improved_subtour[[1]]) - calculateTotalDistance(input, route_to_improve)
            delta_impr_subtour[[2]] <- calculateTotalDistance(input,improved_subtour[[2]]) - calculateTotalDistance(input, route_to_improve)
            delta_impr_subtour[[3]] <- calculateTotalDistance(input,improved_subtour[[3]]) - calculateTotalDistance(input, route_to_improve)
            
            if(any(delta_impr_subtour!=0)){
              
              impr_subtour_index <- which(delta_impr_subtour==min(unlist(delta_impr_subtour)))
              
              
              if(length(impr_subtour_index)>1){
                chosen_impr_subtour_index <- sample(impr_subtour_index, 1)
              }else{
                chosen_impr_subtour_index <- impr_subtour_index
              }
              
              chosen_impr_subtour <- c( subtours[[k]]$root, improved_subtour[[chosen_impr_subtour_index]], subtours[[k]]$root)
              improved_solution[[r]]$subtours[[k]]$tour <- chosen_impr_subtour
    
            } 
            
            
            #if(length(which(chosen_impr_subtour==subtours[[k]]$root)) > 1){
            #  kk <- sum(chosen_improved_route  == subtours[[k]]$root)
              
            #}
            #chosen_improved_route <- c(chosen_improved_route[1:which(chosen_improved_route==subtours[[k]]$root)[kk] ], subtours[[k]]$tour[2:(length(subtours[[k]]$tour))], 
            #                           chosen_improved_route[(which(chosen_improved_route==subtours[[k]]$root)[kk]+1): length(chosen_improved_route)] ) 
        }
      }
      
      improved_solution[[r]]$route <- create_route_from_main_route_and_subroutes(improved_solution[[r]]$subtours, improved_solution[[r]]$main_tour)
      improved_solution[[r]]$cost <- calculateTotalDistance(input, improved_solution[[r]]$route)
    } 
    
  }
  
  return(improved_solution)
  
  
}

