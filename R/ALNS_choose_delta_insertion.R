choose_delta_insertion <- function(selected_insertion, delta_ins){
  
  if(selected_insertion == 1){ # greedy
    delta_min_positions <- which(delta_ins == min(unlist(delta_ins)))
    if(length(delta_min_positions) == 1){
      delta_chosen_position <- delta_min_positions
    }else{
      delta_chosen_position <- sample(delta_min_positions,1)
    }
    
  }else if(selected_insertion == 2){ # random
    delta_chosen_position <- sample(length(delta_ins),1) 
    
  }else if(selected_insertion == 3){ # pbest
    if(length(delta_ins) == 1){
      p <- 1
    }else if(length(delta_ins) <= 10){
      p <- 2
    }else{
      p <- ceiling(0.20*length(delta_ins))
    }
    
    delta_chosen_position <- sample(order(unlist(delta_ins))[1:p],1)
    
  }
  
  return(delta_chosen_position)
}

