calculateTotalDistance <- function(input, route){
  cost <- 0
  for (i in 1:(length(route)-1)){
    cost <- cost + input$matriz.distancia[route[i]+1, route[i+1]+1]
  }
  return(cost)
}





two_opt_Swap <- function(input, route, i, k) {
  new_route <- c(route[1:i-1], rev(route[i:k]), route[(k+1):length(route)])
  delta <- calculateTotalDistance(input, new_route)- calculateTotalDistance(input, route)
  return(list(new_route=new_route, delta=delta))
}





two_opt_all_segments <- function(route){
  segment = list()
  t <- 1
  i_cand <- seq(2,length(route)-2)
  for (i in i_cand[i_cand >= 2 & i_cand <= (length(route)-2)]){
    k_cand <- seq(i+1,length(route)-1)
    for (k in k_cand[k_cand>=(i+1) & k_cand<=(length(route)-1)]){
      segment[[t]] = c(i,k)
      t <- t + 1 
    }
  }
  return(segment)
}


two_opt <- function(input, route){
  delta = list()
  segm = list()
  new_route = list()
  
  res_s <- two_opt_all_segments(route)
  if(length(res_s)>=1){
    for (kk in 1:length(res_s)){
      segm[[kk]] = res_s[[kk]]
      res_t <- two_opt_Swap(input, route, segm[[kk]][1], segm[[kk]][2])
      delta[[kk]] = res_t$delta
      new_route[[kk]] = res_t$new_route
    }
    if (all(delta >=0)){
      best_route <- route
    }else{
      
      delta_min_positions <- which(delta == min(unlist(delta)))
      if(length(delta_min_positions) == 1){
        delta_chosen_position <- delta_min_positions
      }else{
        delta_chosen_position <- sample(delta_min_positions,1)
      }
      best_route <- new_route[[delta_chosen_position]]
    }
  }
  else{
    best_route <- route
  }
  return(best_route)
}


#two_opt <- function(input, route){
#  delta = list()
#  segm = list()
#  new_route = list()
#  res_s <- two_opt_all_segments(route)
#  if(length(res_s)>=1){
#    for (kk in 1:length(two_opt_all_segments(route))){
#      segm[[kk]] = two_opt_all_segments(route)[[kk]]
#      delta[[kk]] = two_opt_Swap(input, route, segm[[kk]][1], segm[[kk]][2])$delta
#      new_route[[kk]] = two_opt_Swap(input, route, segm[[kk]][1], segm[[kk]][2])$new_route
#    }
#    if (all(delta >=0)){
#      best_route <- route
#    }else{
#      delta_min_positions <- which(delta == min(unlist(delta)))
#      if(length(delta_min_positions) == 1){
#        delta_chosen_position <- delta_min_positions
#      }else{
#        delta_chosen_position <- sample(delta_min_positions,1)
#      }
#      best_route <- new_route[[delta_chosen_position]]
#    }
#  }else{
#    best_route <- route
#  }
#  return(best_route)
#}





three_opt_reverse_segment_if_better <- function(input, route, i, j, k) {
  d0 = input$matriz.distancia[route[i-1]+1,route[i]+1] + input$matriz.distancia[route[j-1]+1,route[j]+1] + input$matriz.distancia[route[k-1]+1,route[k]+1]
  d1 = input$matriz.distancia[route[i-1]+1,route[j-1]+1] + input$matriz.distancia[route[i]+1,route[j]+1] + input$matriz.distancia[route[k-1]+1,route[k]+1] 
  d2 = input$matriz.distancia[route[i-1]+1,route[i]+1] + input$matriz.distancia[route[j-1]+1,route[k-1]+1] + input$matriz.distancia[route[j]+1,route[k]+1]
  d3 = input$matriz.distancia[route[i-1]+1,route[j-1]+1] + input$matriz.distancia[route[i]+1,route[k-1]+1] + input$matriz.distancia[route[j]+1,route[k]+1]
  d4 = input$matriz.distancia[route[i-1]+1,route[j]+1] + input$matriz.distancia[route[k-1]+1,route[i]+1] + input$matriz.distancia[route[j-1]+1,route[k]+1]
  d5 = input$matriz.distancia[route[i-1]+1,route[j]+1] + input$matriz.distancia[route[k-1]+1,route[j-1]+1] + input$matriz.distancia[route[i]+1,route[k]+1]
  d6 = input$matriz.distancia[route[i-1]+1,route[k-1]+1] + input$matriz.distancia[route[j]+1,route[j-1]+1] + input$matriz.distancia[route[i]+1,route[k]+1]
  d7 = input$matriz.distancia[route[i-1]+1,route[k-1]+1] + input$matriz.distancia[route[j]+1,route[i]+1] + input$matriz.distancia[route[j-1]+1,route[k]+1]
  
  new_route = route
  d.min = min(d1,d2,d3,d4,d5,d6)
  
  if(d0 > d1 && d.min == d1){
    new_route[i:(j-1)] = rev(route[i:(j-1)])
    new_cost = calculateTotalDistance(input, new_route)
    delta = d1 - d0
  }else if(d0 > d2 && d.min == d2){
    new_route[j:(k-1)] = rev(route[j:(k-1)])
    new_cost = calculateTotalDistance(input, new_route)
    delta = d2 - d0
  }else if(d0 > d3 && d.min == d3){
    new_route[i:(j-1)] = rev(route[i:(j-1)])
    new_route[j:(k-1)] = rev(route[j:(k-1)])
    new_cost = calculateTotalDistance(input, new_route)
    delta = d3 - d0
  }else if(d0 > d4 && d.min == d4){
    new_route = c(route[1:(i-1)], route[j:(k-1)], route[i:(j-1)], route[k:length(route)])
    new_cost = calculateTotalDistance(input, new_route)
    delta = d4 - d0
  }else if(d0 > d5 && d.min == d5){
    new_route = c(route[1:(i-1)], route[j:(k-1)], rev(route[i:(j-1)]), route[k:length(route)])
    new_cost = calculateTotalDistance(input, new_route)
    delta = d5 - d0
  }else if(d0 > d6 && d.min == d6){
    new_route = c(route[1:(i-1)],rev(route[j:(k-1)]),rev(route[i:(j-1)]),route[k:length(route)])
    new_cost = calculateTotalDistance(input, new_route)
    delta = d6 - d0
  }else if(d0 > d7 && d.min == d7){
    new_route = c(route[1:(i-1)], rev(route[j:(k-1)]), route[i:(j-1)], route[k:length(route)])
    new_cost = calculateTotalDistance(input, new_route)
    delta = d7 - d0
    
  }else{
    new_route = route
    new_cost = calculateTotalDistance(input, new_route)
    delta = 0
  }
  
  return(list(new_route=new_route, new_cost=new_cost, delta=delta))
}






three_opt_all_segments <- function(route){
  segment = list()
  t <- 1
  i_cand <- seq(3,length(route)-5)
  for(i in i_cand[i_cand >=3 & i_cand <= (length(route)-5)]){
    j_cand <- seq(i+2,length(route)-3)
    for (j in j_cand[j_cand >= (i+2) & j_cand<=(length(route)-3)]){
      k_cand <-  seq(j+2,length(route)-1)
      for (k in k_cand[k_cand>=(j+2) & k_cand<=(length(route)-1)]){
        segment[[t]] = c(i,j,k)
        t <- t + 1 
      }
    }
  }
  return(segment)
}





three_opt <- function(input, route){
  delta = list()
  segm = list()
  new_route = list()
  
  res_s <- three_opt_all_segments(route)
  if(length(res_s)>=1){
    for (kk in 1:length(res_s)){
      segm[[kk]] = res_s[[kk]]
      res_t = three_opt_reverse_segment_if_better(input, route, segm[[kk]][1], segm[[kk]][2], segm[[kk]][3])
      delta[[kk]] = res_t$delta
      new_route[[kk]] = res_t$new_route
      
    }
    if (all(delta >=0)){
      best_route <- route
    }else{
      
      delta_min_positions <- which(delta == min(unlist(delta)))
      if(length(delta_min_positions) == 1){
        delta_chosen_position <- delta_min_positions
      }else{
        delta_chosen_position <- sample(delta_min_positions,1)
      }
      best_route <- new_route[[delta_chosen_position]]
    }
  }else{
    best_route <- route
  }
  return(best_route)
}





four_opt_asterisk_swap <- function(input, route,i,j,k, printing = FALSE){
  
  delLen_1 = input$matriz.distancia[route[i]+1,route[i+1]+1] 
  delLen_2 = input$matriz.distancia[route[j]+1,route[j+1]+1] 
  delLen_3 = input$matriz.distancia[route[k]+1,route[k+1]+1]
  delLen_4 = input$matriz.distancia[route[k+1]+1,route[k+2]+1]
  
  delLen = delLen_1 + delLen_2 + delLen_3 + delLen_4
  
  delLenMax = max(delLen_1, delLen_2, delLen_3, delLen_4)
  
  addLen_A = input$matriz.distancia[route[i+1]+1,route[k+1]+1]
  addLen_B = input$matriz.distancia[route[j]+1,route[k+1]+1]
  
  addLenMin = min(addLen_A, addLen_B)
  
  delta <- 0
  
  if (addLenMin < delLenMax){
    if (addLenMin == addLen_A){ # Clase A
      dA2opt  = addLen_A + input$matriz.distancia[route[i]+1,route[k]+1] + input$matriz.distancia[route[j]+1,route[j+1]+1] + input$matriz.distancia[route[k+1]+1,route[k+2]+1]
      dA3opt1 = addLen_A + input$matriz.distancia[route[i]+1,route[j]+1] + input$matriz.distancia[route[j+1]+1,route[k+2]+1] + input$matriz.distancia[route[k]+1,route[k+1]+1]
      dA3opt2 = addLen_A + input$matriz.distancia[route[i]+1,route[j+1]+1] + input$matriz.distancia[route[j]+1,route[k]+1] + input$matriz.distancia[route[k+1]+1,route[k+2]+1]
      dA3opt3 = addLen_A + input$matriz.distancia[route[i]+1,route[j+1]+1] + input$matriz.distancia[route[j]+1,route[k+2]+1] + input$matriz.distancia[route[k]+1,route[k+1]+1]
      dA3opt4 = addLen_A + input$matriz.distancia[route[j]+1,route[j+1]+1] + input$matriz.distancia[route[i]+1,route[k+1]+1] + input$matriz.distancia[route[k]+1,route[k+2]+1]
      dA4opt1 = addLen_A + input$matriz.distancia[route[i]+1,route[k+1]+1] + input$matriz.distancia[route[j]+1,route[k]+1] + input$matriz.distancia[route[j+1]+1,route[k+2]+1]
      dA4opt2 = addLen_A + input$matriz.distancia[route[i]+1,route[j]+1] + input$matriz.distancia[route[j+1]+1,route[k+1]+1] + input$matriz.distancia[route[k]+1,route[k+2]+1]
      dA4opt3 = addLen_A + input$matriz.distancia[route[i]+1,route[k]+1] + input$matriz.distancia[route[j+1]+1,route[k+1]+1] + input$matriz.distancia[route[j]+1,route[k+2]+1]
      
      dAmin = min(dA2opt, dA3opt1, dA3opt2, dA3opt3, dA3opt4, dA4opt1, dA4opt2, dA4opt3)
      
      if (dA2opt < delLen && dAmin == dA2opt){
        new_route = c(route[1:i], rev(route[(i+1):k]), route[(k+1):length(route)])
        new_cost = calculateTotalDistance(input, new_route)
        delta = dA2opt - delLen
        if (printing) cat("Movimiento clase A, 2opt\n\n")
      }else if(dA3opt1 < delLen && dAmin == dA3opt1){
        new_route = c(route[1:i], rev(route[(i+1):j]), rev(route[(j+1):(k+1)]), route[(k+2):length(route)])
        new_cost = calculateTotalDistance(input, new_route)
        delta = dA3opt1 - delLen
        if (printing)cat("Movimiento clase A, 3opt1\n\n")
      }else if(dA3opt2 < delLen && dAmin == dA3opt2){
        new_route = c(route[1:i], route[(j+1):k], rev(route[(i+1):j]), route[(k+1):length(route)])
        new_cost = calculateTotalDistance(input, new_route)
        delta = dA3opt2 - delLen
        if (printing) cat("Movimiento clase A, 3opt2\n\n")
      }else if(dA3opt3 < delLen && dAmin == dA3opt3){
        new_route = c(route[1:i], route[(j+1):(k+1)], route[(i+1):j], route[(k+2):length(route)])
        new_cost = calculateTotalDistance(input, new_route)
        delta = dA3opt3 - delLen
        if (printing) cat("Movimiento clase A, 3opt3\n\n")
      }else if(dA3opt4 < delLen && dAmin == dA3opt4){
        new_route = c(route[1:i], route[k+1], route[(i+1):k], route[(k+2):length(route)])
        new_cost = calculateTotalDistance(input, new_route)
        delta = dA3opt4 - delLen
        if (printing) cat("Movimiento clase A, 3opt4\n\n")
      }else if(dA4opt1 < delLen && dAmin == dA4opt1){
        new_route = c(route[1:i], route[k+1], route[(i+1):j], rev(route[(j+1):k]), route[(k+2):length(route)])
        new_cost = calculateTotalDistance(input, new_route)
        delta = dA4opt1 - delLen
        if (printing) cat("Movimiento clase A, 4opt1\n\n")
      }else if(dA4opt2 < delLen && dAmin == dA4opt2){
        new_route = c(route[1:i], rev(route[(i+1):j]), route[k+1], route[(j+1):k], route[(k+2):length(route)])
        new_cost = calculateTotalDistance(input, new_route)
        delta = dA4opt2 - delLen
        if (printing) cat("Movimiento clase A, 4opt2\n\n")
      }else if(dA4opt3 < delLen && dAmin == dA4opt3){
        new_route = c(route[1:i], rev(route[(j+1):k]), route[k+1], route[(i+1):j], route[(k+2):length(route)])
        new_cost = calculateTotalDistance(input, new_route)
        delta = dA4opt3 - delLen
        if (printing) cat("Movimiento clase A, 4opt3\n\n")
      }else{
        if (printing)  cat("No improvement found \n\n")
        new_route = route
        new_cost = calculateTotalDistance(input, route)
      }
      
      
    }else if(addLenMin == addLen_B){ # Clase B
      dB2opt  = addLen_B + input$matriz.distancia[route[i]+1,route[i+1]+1] + input$matriz.distancia[route[k]+1,route[k+1]+1] + input$matriz.distancia[route[j+1]+1,route[k+2]+1]
      dB3opt1 = addLen_B + input$matriz.distancia[route[i]+1,route[k]+1] + input$matriz.distancia[route[i+1]+1,route[j+1]+1] + input$matriz.distancia[route[k+1]+1,route[k+2]+1]
      dB3opt2 = addLen_B + input$matriz.distancia[route[i]+1,route[j+1]+1] + input$matriz.distancia[route[k]+1,route[k+1]+1] + input$matriz.distancia[route[i+1]+1,route[k+2]+1]
      dB3opt3 = addLen_B + input$matriz.distancia[route[i]+1,route[j+1]+1] + input$matriz.distancia[route[i+1]+1,route[k]+1] + input$matriz.distancia[route[k+1]+1,route[k+2]+1]
      dB3opt4 = addLen_B + input$matriz.distancia[route[i]+1,route[i+1]+1] + input$matriz.distancia[route[j+1]+1,route[k+1]+1] + input$matriz.distancia[route[k]+1,route[k+2]+1]
      dB4opt1 = addLen_B + input$matriz.distancia[route[i]+1,route[k]+1] + input$matriz.distancia[route[j+1]+1,route[k+1]+1] + input$matriz.distancia[route[i+1]+1,route[k+2]+1]
      dB4opt2 = addLen_B + input$matriz.distancia[route[i]+1,route[k+1]+1] + input$matriz.distancia[route[i+1]+1,route[j+1]+1] + input$matriz.distancia[route[k]+1,route[k+2]+1]
      dB4opt3 = addLen_B + input$matriz.distancia[route[i]+1,route[k+1]+1] + input$matriz.distancia[route[i+1]+1,route[k]+1] + input$matriz.distancia[route[j+1]+1,route[k+2]+1]
      
      dBmin = min(dB2opt, dB3opt1, dB3opt2, dB3opt3, dB3opt4, dB4opt1, dB4opt2, dB4opt3)
      
      if (dB2opt < delLen && dBmin == dB2opt){
        new_route = c(route[1:j], rev(route[(j+1):(k+1)]), route[(k+2):length(route)])
        new_cost = calculateTotalDistance(input, new_route)
        delta = dB2opt - delLen
        if (printing) cat("Movimiento clase B, 2opt\n\n")
      }else if(dB3opt1 < delLen && dBmin == dB3opt1){
        new_route = c(route[1:i], rev(route[(j+1):k]), route[(i+1):j], route[(k+1):length(route)])
        new_cost = calculateTotalDistance(input, new_route)
        delta = dB3opt1 - delLen
        if (printing) cat("Movimiento clase B, 3opt1\n\n")
      }else if(dB3opt2 < delLen && dBmin == dB3opt2){
        new_route = c(route[1:i], route[(j+1):(k+1)], rev(route[(i+1):j]), route[(k+2):length(route)])
        new_cost = calculateTotalDistance(input, new_route)
        delta = dB3opt2 - delLen
        if (printing) cat("Movimiento clase B, 3opt2\n\n")
      }else if(dB3opt3 < delLen && dBmin == dB3opt3){
        new_route = c(route[1:i], route[(j+1):k], route[(i+1):j], route[(k+1):length(route)])
        new_cost = calculateTotalDistance(input, new_route)
        delta = dB3opt3 - delLen
        if (printing) cat("Movimiento clase B, 3opt3\n\n")
      }else if(dB3opt4 < delLen && dBmin == dB3opt4){
        new_route = c(route[1:j], route[k+1], route[(j+1):k], route[(k+2):length(route)])
        new_cost = calculateTotalDistance(input, new_route)
        delta = dB3opt4 - delLen
        if (printing) cat("Movimiento clase B, 3opt4\n\n")
      }else if(dB4opt1 < delLen && dBmin == dB4opt1){
        new_route = c(route[1:i], rev(route[(j+1):k]), route[k+1], rev(route[(i+1):j]), route[(k+2):length(route)])
        new_cost = calculateTotalDistance(input, new_route)
        delta = dB4opt1 - delLen
        if (printing) cat("Movimiento clase B, 4opt1\n\n")
      }else if(dB4opt2 < delLen && dBmin == dB4opt2){
        new_route = c(route[1:i], route[k+1], rev(route[(i+1):j]), route[(j+1):k], route[(k+2):length(route)])
        new_cost = calculateTotalDistance(input, new_route)
        delta = dB4opt2 - delLen
        if (printing) cat("Movimiento clase B, 4opt2\n\n")
      }else if(dB4opt3 < delLen && dBmin == dB4opt3){
        new_route = c(route[1:i], route[k+1], rev(route[(i+1):j]), rev(route[(j+1):k]), route[(k+2):length(route)])
        new_cost = calculateTotalDistance(input, new_route)
        delta = dB4opt3 - delLen
        if (printing) cat("Movimiento clase B, 4opt3\n\n")
      }else{
        if (printing) cat("No improvement found \n\n")
        new_route = route
        new_cost = calculateTotalDistance(input, route)
      }
      
    } 
  }else{
    if (printing) cat("No improvement found \n\n")
    new_route = route
    new_cost = calculateTotalDistance(input, route)
  }
  
  return(list(best_route4opt=new_route, best_cost4opt = new_cost, delta = delta))
}




four_opt_asterisk_all_segments <- function(route){
  w <- ceiling(0.2*(length(route)-2))
  u <- 1
  segment = list()
  t <- 1
  while (u <= w){
    i_cand <-  seq(2,length(route)-(5+u))
    for(i in i_cand[i_cand >=2 & i_cand <=(length(route)-(5+u))]){
      j <- i + u
      k_cand <- seq(j+2, length(route)-3)
      for (k in k_cand[k_cand >= (j+2) & k_cand <=(length(route)-3)]){
        segment[[t]] = c(i,j,k)
        t <- t + 1
      }
    }
    u <- u + 1 
  }
  return(segment)
}




four_opt_asterisk <- function(input, route){
  delta = list()
  segm = list()
  new_route = list()
  
  res_f <- four_opt_asterisk_all_segments(route)
  if(length(res_f)>=1){
    for (kk in 1:length(res_f)){
      segm[[kk]] = res_f[[kk]]
      res = four_opt_asterisk_swap(input, route, segm[[kk]][1], segm[[kk]][2], segm[[kk]][3])
      delta[[kk]] = res$delta
      new_route[[kk]] = res$best_route4opt
    }
    
    if (all(delta >=0)){
      best_route <- route
    }else{
      
      delta_min_positions <- which(delta == min(unlist(delta)))
      if(length(delta_min_positions) == 1){
        delta_chosen_position <- delta_min_positions
      }else{
        delta_chosen_position <- sample(delta_min_positions,1)
      }
      
      best_route <- new_route[[delta_chosen_position]]
      
    }
    
  }else{
    best_route <- route
  }
  
  return(best_route)
}




# route: ruta en la que vamos a insertar v
# q: numero de vecinos m?s pr?ximos
# v: v?rtice a insertar
# v_i, v_j, v_k: clientes de esta ruta

insertion_typeI_swap <- function(input, route, v, v_i, v_j, v_k){ 
  
  i <- which(route==v_i) #posici?n del cliente v_i en la ruta
  j <- which(route==v_j)
  k <- which(route==v_k)
  
  new_route = c(route[1:i], v, rev(route[(i+1):j]), rev(route[(j+1):k]), 
                route[(k+1):length(route)])
  
  new_cost = calculateTotalDistance(input, new_route)
  
  delta = new_cost - calculateTotalDistance(input, route)
  
  
  return(list(new_route = new_route, delta = delta))
}





# Esto ser?a sin considerar vecindad:
insertion_typeI_all_segments <- function(route){
  segment = list()
  t <- 1
  i_cand <- seq(2,length(route)-3)
  for(i in i_cand[i_cand>=2 & i_cand<=(length(route)-3)]){
    j_cand <- seq(i+1,length(route)-2)
    for (j in j_cand[j_cand>=(i+1) & j_cand<=(length(route)-2)]){
      k_cand <- seq(j+1,length(route)-1)
      for (k in k_cand[k_cand>=(j+1) & k_cand <= (length(route)-1)]){
        segment[[t]] = c(i,j,k)
        t <- t + 1 
      }
    }
  }
  return(segment)
}





insertion_typeI <- function(input, route, v){
  delta = list()
  segm = list()
  new_route = list()
  
  res_insertions <- insertion_typeI_all_segments(route)
  if(length(res_insertions)>=1){
    for (kk in 1:length(res_insertions)){
      segm[[kk]] = res_insertions[[kk]]
      res <- insertion_typeI_swap(input, route, v, route[segm[[kk]][1]], route[segm[[kk]][2]], route[segm[[kk]][3]])
      delta[[kk]] <- res$delta
      new_route[[kk]] <- res$new_route
    }
    
    
    delta_min_positions <- which(delta == min(unlist(delta)))
    if(length(delta_min_positions) == 1){
      delta_chosen_position <- delta_min_positions
    }else{
      delta_chosen_position <- sample(delta_min_positions,1)
    }
    best_route <- new_route[[delta_chosen_position]]
    delta <- delta[[delta_chosen_position]]
  }else{
    new_route_compulsory <- c(route[1:ceiling(length(route)/2)], v, route[(ceiling(length(route)/2)+1): length(route)])
    best_route = three_opt(input, new_route_compulsory)
    delta <- calculateTotalDistance(input, best_route) - calculateTotalDistance(input, route)
  }
  return(list(best_route_I = best_route, delta_I = delta))
}




# route: ruta en la que vamos a insertar v
# q: numero de vecinos m?s pr?ximos
# v: v?rtice a insertar
# v_i, v_j, v_k: clientes de esta ruta


insertion_typeII_swap <- function(input, route, v, v_i, v_l, v_j, v_k){ 
  
  i <- which(route==v_i) #posici?n del cliente v_i en la ruta
  j <- which(route==v_j)
  k <- which(route==v_k)
  l <- which(route==v_l)
  
  new_route = c(route[1:i], v, rev(route[l:j]), route[(j+1):(k-1)], 
                rev(route[(i+1):(l-1)]), route[k:length(route)])
  
  new_cost = calculateTotalDistance(input, new_route)
  
  delta = new_cost - calculateTotalDistance(input, route)
  
  
  return(list(new_route = new_route, delta = delta))
}







# Esto ser?a sin considerar vecindad;
insertion_typeII_all_segments <- function(route){
  segment = list()
  t <- 1
  i_cand <- seq(2,length(route)-5)
  for(i in i_cand[i_cand>=2 & i_cand<=(length(route)-5)]){
    l_cand <- seq(i+2,length(route)-3)
    for (l in l_cand[l_cand>=(i+2) & l_cand<=(length(route)-3)]){
      j_cand <- seq(l,length(route)-3)
      for (j in j_cand[j_cand>=l & j_cand<=(length(route)-3)]){
        k_cand <- seq(j+2,length(route)-1)
        for (k in k_cand[k_cand>=(j+2) & k_cand<=(length(route)-1)]){
          segment[[t]] = c(i,l,j,k)
          t <- t + 1 
        }
      }
    }
  }
  return(segment)
}





insertion_typeII <- function(input, route, v){
  delta = list()
  segm = list()
  new_route = list()
  
  res_insert <- insertion_typeII_all_segments(route)
  if(length(res_insert)>=1){
    for (kk in 1:length(res_insert)){
      segm[[kk]] <- res_insert[[kk]]
      res <- insertion_typeII_swap(input, route, v, route[segm[[kk]][1]], route[segm[[kk]][2]], route[segm[[kk]][3]], route[segm[[kk]][4]])
      delta[[kk]] <- res$delta
      new_route[[kk]] <- res$new_route
    }
    
    
    delta_min_positions <- which(delta == min(unlist(delta)))
    if(length(delta_min_positions) == 1){
      delta_chosen_position <- delta_min_positions
    }else{
      delta_chosen_position <- sample(delta_min_positions,1)
    }
    best_route <- new_route[[delta_chosen_position]]
    delta <- delta[[delta_chosen_position]]
    
  }else{
    new_route_compulsory <- c(route[1:ceiling(length(route)/2)], v, route[(ceiling(length(route)/2)+1): length(route)])
    best_route <- four_opt_asterisk(input, new_route_compulsory)
    delta <- calculateTotalDistance(input, best_route) - calculateTotalDistance(input, route)
  }
  
  return(list(best_route_II = best_route, delta_II = delta))
}





GENI <- function(input, route, v){
  
  #Elegimos el mejor orden para insercion tipo I
  res_route_I = insertion_typeI(input, route, v)
  res_route_I_rev = insertion_typeI(input, rev(route), v)
  
  # best_route_I = best_route, delta_I = delta
  if(res_route_I$delta_I < res_route_I_rev$delta_I){
    delta_I <- res_route_I$delta_I
    new_route_I =  res_route_I$best_route_I
  }else if(res_route_I$delta_I > res_route_I_rev$delta_I){
    delta_I <- res_route_I_rev$delta_I
    new_route_I =  res_route_I_rev$best_route_I
  }else{
    rand = sample(c("I","I_rev"),1)
    if(rand == "I"){
      delta_I <- res_route_I$delta_I
      new_route_I =  res_route_I$best_route_I
    }else{
      delta_I <- res_route_I_rev$delta_I
      new_route_I =  res_route_I_rev$best_route_I
    }
  }
  
  best_route_I = three_opt(input, new_route_I)
  
  #Elegimos el mejor orden para insercion tipo II
  res_route_II = insertion_typeII(input, route, v)
  res_route_II_rev = insertion_typeII(input,rev(route), v)
  # best_route_I = best_route, delta_I = delta
  
  if(res_route_II$delta_II < res_route_II_rev$delta_II){
    delta_II <- res_route_II$delta_II
    new_route_II =  res_route_II$best_route_II
  }else if(res_route_II$delta_II > res_route_II_rev$delta_II){
    delta_II <- res_route_II_rev$delta_II
    new_route_II =  res_route_II_rev$best_route_II
  }else{
    rand = sample(c("II","II_rev"),1)
    if(rand == "II"){
      delta_II <- res_route_II$delta_II
      new_route_II =  res_route_II$best_route_II
    }else{
      delta_II <- res_route_II_rev$delta_II
      new_route_II =  res_route_II_rev$best_route_II
    }
  }


  best_route_II = four_opt_asterisk(input, new_route_II)
  
  # Ahora escogemos la mejor insercion: tipo I vs tipo II
  if(delta_I < delta_II){
    best_route = best_route_I
    delta = delta_I
  }else if(delta_I > delta_II){
    best_route = best_route_II
    delta = delta_II
  }else{
    rand = sample(c("I","II"),1)
    if(rand == "I"){
      best_route = best_route_I
      delta = delta_I
    }else{
      best_route = best_route_II
      delta = delta_II
    }
  }
  
  return(list(best_route = best_route, delta_GENI = delta))
}




# route: ruta de la que vamos a eliminar v
# v: v?rtice a eliminar
# v_i, v_j, v_k: clientes de esta ruta

removal_typeI_swap <- function(input, route, v_i, v_k, v_j){ 
  
  i <- which(route==v_i) #posici?n del cliente v_i en la ruta
  j <- which(route==v_j)
  k <- which(route==v_k)
  
  new_route = c(route[1:(i-1)], rev(route[(i+1):k]), rev(route[(k+1):j]), 
                route[(j+1):length(route)])
  
  new_cost = calculateTotalDistance(input, new_route)
  
  delta = new_cost - calculateTotalDistance(input, route)
  
  
  return(list(new_route = new_route, delta = delta))
}






# Ahora no voy a pensar en una vecindad, sino que asumo que cualquier "intercambio" se puede hacer
removal_typeI_all_segments <- function(route, v_i){
  i <- which(route == v_i)
  segment = list()
  t <- 1
  k_cand <-seq(i+1,length(route)-2)
  for (k in k_cand[k_cand>=(i+1) & k_cand<=(length(route)-2)]){
    j_cand <- seq(k+1,length(route)-1)
    for (j in j_cand[j_cand>=(k+1) & j_cand<=(length(route)-1)]){
      segment[[t]] = c(i,k,j)
      t <- t + 1 
    }
  }
  return(segment)
}





removal_typeI <- function(input, route, v_i){
  delta = list()
  segm = list()
  new_route = list()
  
  res_r <- removal_typeI_all_segments(route, v_i)
  if(length(res_r)>=1){
    for (kk in 1:length(res_r)){
      segm[[kk]] <- res_r[[kk]]
      res <- removal_typeI_swap(input, route, v_i, route[segm[[kk]][2]], route[segm[[kk]][3]])
      delta[[kk]] <- res$delta
      new_route[[kk]] <- res$new_route
    }
    
    
    delta_min_positions <- which(delta == min(unlist(delta)))
    if(length(delta_min_positions) == 1){
      delta_chosen_position <- delta_min_positions
    }else{
      delta_chosen_position <- sample(delta_min_positions,1)
    }
    best_route <- new_route[[delta_chosen_position]]
    delta <- delta[[delta_chosen_position]]
    
  }else{
    best_route <- two_opt( input,route[-which(route==v_i)])
    delta <- calculateTotalDistance(input, best_route) - calculateTotalDistance(input, route)
  }
  return(list(best_route_remI = best_route, delta_remI = delta))
}




# route: ruta en la que vamos a insertar v
# q: numero de vecinos m?s pr?ximos
# v: v?rtice a insertar
# v_i, v_j, v_k: clientes de esta ruta


removal_typeII_swap <- function(input, route, v_i, v_j, v_l, v_k){ 
  
  i <- which(route==v_i) #posici?n del cliente v_i en la ruta
  j <- which(route==v_j)
  k <- which(route==v_k)
  l <- which(route==v_l)
  
  new_route = c(route[1:(i-1)], rev(route[(l+1):k]), rev(route[(i+1):(j-1)]), 
                route[j:l], route[(k+1):length(route)])
  
  new_cost = calculateTotalDistance(input, new_route)
  
  delta = new_cost - calculateTotalDistance(input, route)
  
  
  return(list(new_route = new_route, delta = delta))
}






# Ahora no voy a pensar en una vecindad, sino que asumo que cualquier "intercambio" se puede hacer
removal_typeII_all_segments <- function(route, v_i){
  i <- which(route == v_i)
  segment = list()
  t <- 1
  j_cand <- seq(i+2,length(route)-2)
  for (j in j_cand[j_cand>=(i+2) & j_cand<=(length(route)-2)]){
    l_cand <- seq(j,length(route)-2)
    for (l in l_cand[l_cand>=j & l_cand<=(length(route)-2)]){
      k_cand <- seq(l+1,length(route)-1)
      for (k in k_cand[k_cand>=(l+1) & k_cand <= (length(route)-1)]){
        segment[[t]] = c(i,j,l,k)
        t <- t + 1 
      }
    }
  }
  return(segment)
}





removal_typeII <- function(input, route, v_i){
  delta = list()
  segm = list()
  new_route = list()
  
  res_r <- removal_typeII_all_segments(route, v_i)
  if(length(res_r)>=1){
    for (kk in 1:length(res_r)){
      segm[[kk]] <- res_r[[kk]]
      res <- removal_typeII_swap(input, route, v_i, route[segm[[kk]][2]], route[segm[[kk]][3]], route[segm[[kk]][4]])
      delta[[kk]] <- res$delta
      new_route[[kk]] <- res$new_route
    }
    
    delta_min_positions <- which(delta == min(unlist(delta)))
    if(length(delta_min_positions) == 1){
      delta_chosen_position <- delta_min_positions
    }else{
      delta_chosen_position <- sample(delta_min_positions,1)
    }
    best_route <- new_route[[delta_chosen_position]]
    delta <- delta[[delta_chosen_position]]
    
  }else{
    best_route <- two_opt( input,route[-which(route==v_i)])
    delta <- calculateTotalDistance(input, best_route) - calculateTotalDistance(input, route)
  }
  return(list(best_route_remII = best_route, delta_remII = delta))
  
}



GENI_US <- function(input, route, v_i){
  
  #Elegimos el mejor orden para removal tipo I
  res_rem_I <- removal_typeI(input, route, v_i)
  res_rem_I_rev <- removal_typeI(input, rev(route), v_i)
  if(res_rem_I$delta_remI < res_rem_I_rev$delta_remI){
    delta_I <- res_rem_I$delta_remI
    new_route_I <-  res_rem_I$best_route_remI
  }else if(res_rem_I$delta_remI > res_rem_I_rev$delta_remI){
    delta_I <- res_rem_I_rev$delta_remI
    new_route_I <-  res_rem_I_rev$best_route_remI
  }else{
    rand = sample(c("I","I_rev"),1)
    if(rand == "I"){
      delta_I <- res_rem_I$delta_remI
      new_route_I =  res_rem_I$best_route_remI
    }else{
      delta_I <- res_rem_I_rev$delta_remI
      new_route_I =  res_rem_I_rev$best_route_remI
    }
  }
  
  best_route_I = three_opt(input, new_route_I)
  
  
  #Elegimos el mejor orden para removal tipo II
  delta_rem_II = removal_typeII( input,route, v_i)$delta_remII
  delta_rem_II_rev = removal_typeII( input,rev(route), v_i)$delta_remII
  
  if(delta_rem_II < delta_rem_II_rev){
    delta_II <- delta_rem_II
    new_route_II =  removal_typeII( input,route, v_i)$best_route_remII
  }else if(delta_rem_II > delta_rem_II_rev){
    delta_II <- delta_rem_II_rev
    new_route_II =  removal_typeII( input,rev(route), v_i)$best_route_remII
  }else{
    rand = sample(c("II","II_rev"),1)
    if(rand == "II"){
      delta_II <- delta_rem_II
      new_route_II =  removal_typeII( input,route, v_i)$best_route_remII
    }else{
      delta_II <- delta_rem_II_rev
      new_route_II =  removal_typeII( input,rev(route), v_i)$best_route_remII
    }
  }
  
  best_route_II = four_opt_asterisk(input, new_route_II)
  
  # Ahora escogemos la mejor removal: tipo I vs tipo II
  if(delta_I < delta_II){
    best_route = best_route_I
  }else if(delta_I > delta_II){
    best_route = best_route_II
  }else{
    rand = sample(c("I","II"),1)
    if(rand == "I"){
      best_route = best_route_I
    }else{
      best_route = best_route_II
    }
  }
  
  return(best_route)
}




