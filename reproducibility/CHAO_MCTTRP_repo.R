library(mcttrpcw)

options(show.error.locations = TRUE)
options(error=function()traceback(2))
options(max.print=3000)
#N <- 1
#init <- 1

#for (i in init:N) {
#  if (i < 10) {
#    string <- paste0("instances\\CHAO_MCTTRP_0",i,".txt")
#    result <- CW_algorithm(string, "MCTTRP", 0)
#  } else {
#    string <- paste0("instances\\CHAO_MCTTRP_",i,".txt")
#    result <- CW_algorithm(string, "MCTTRP", 0)
#  }
#}

result_CW <- list()
result_CW[[1]] <- CW_algorithm("instances/CHAO_MCTTRP_01.txt", "MCTTRP", 8, 5, 0)
result_CW[[2]] <- CW_algorithm("instances/CHAO_MCTTRP_02.txt", "MCTTRP", 8, 5, 0)
result_CW[[3]] <- CW_algorithm("instances/CHAO_MCTTRP_03.txt", "MCTTRP", 8, 5, 0) 

result_CW[[4]] <- CW_algorithm("instances/CHAO_MCTTRP_04.txt", "MCTTRP", 13, 8, 0)
result_CW[[5]] <- CW_algorithm("instances/CHAO_MCTTRP_05.txt", "MCTTRP", 14, 8, 0) 
result_CW[[6]] <- CW_algorithm("instances/CHAO_MCTTRP_06.txt", "MCTTRP", 14, 8, 0) 

result_CW[[7]] <- CW_algorithm("instances/CHAO_MCTTRP_07.txt", "MCTTRP", 11, 6, 0)
result_CW[[8]] <- CW_algorithm("instances/CHAO_MCTTRP_08.txt", "MCTTRP", 12, 6, 0)
result_CW[[9]] <- CW_algorithm("instances/CHAO_MCTTRP_09.txt", "MCTTRP", 11, 6, 0)  ## XXX

result_CW[[10]] <- CW_algorithm("instances/CHAO_MCTTRP_10.txt", "MCTTRP", 18, 9, 0) 
result_CW[[11]] <- CW_algorithm("instances/CHAO_MCTTRP_11.txt", "MCTTRP", 18, 9, 0) 
result_CW[[12]] <- CW_algorithm("instances/CHAO_MCTTRP_12.txt", "MCTTRP", 18, 9, 0) 

result_CW[[13]] <- CW_algorithm("instances/CHAO_MCTTRP_13.txt", "MCTTRP", 23, 14, 0)
result_CW[[14]] <- CW_algorithm("instances/CHAO_MCTTRP_14.txt", "MCTTRP", 26, 14, 0)
result_CW[[15]] <- CW_algorithm("instances/CHAO_MCTTRP_15.txt", "MCTTRP", 25, 13, 0) 

result_CW[[16]] <- CW_algorithm("instances/CHAO_MCTTRP_16.txt", "MCTTRP", 11, 6, 0)
result_CW[[17]] <- CW_algorithm("instances/CHAO_MCTTRP_17.txt", "MCTTRP", 11, 6, 0)
result_CW[[18]] <- CW_algorithm("instances/CHAO_MCTTRP_18.txt", "MCTTRP", 11, 6, 0) # x

result_CW[[19]] <- CW_algorithm("instances/CHAO_MCTTRP_19.txt", "MCTTRP", 12, 8, 0)
result_CW[[20]] <- CW_algorithm("instances/CHAO_MCTTRP_20.txt", "MCTTRP", 13, 8, 0)
result_CW[[21]] <- CW_algorithm("instances/CHAO_MCTTRP_21.txt", "MCTTRP", 13, 8, 0)

for (i in 1:length(result_CW)) {
  print(paste0(i,"  cost: ", result_CW[[i]]$cost))
}



result_ITS <- list()
result_ITS[[1]] <- CW_algorithm("instances/CHAO_MCTTRP_01.txt", "MCTTRP", 8, 5, 0)
result_ITS[[2]] <- CW_algorithm("instances/CHAO_MCTTRP_02.txt", "MCTTRP", 8, 5, 0)
result_ITS[[3]] <- CW_algorithm("instances/CHAO_MCTTRP_03.txt", "MCTTRP", 8, 5, 0) 

result_ITS[[4]] <- CW_algorithm("instances/CHAO_MCTTRP_04.txt", "MCTTRP", 13, 8, 0)
result_ITS[[5]] <- CW_algorithm("instances/CHAO_MCTTRP_05.txt", "MCTTRP", 14, 8, 0) 
result_ITS[[6]] <- CW_algorithm("instances/CHAO_MCTTRP_06.txt", "MCTTRP", 14, 8, 0) 

result_ITS[[7]] <- CW_algorithm("instances/CHAO_MCTTRP_07.txt", "MCTTRP", 11, 6, 0)
result_ITS[[8]] <- CW_algorithm("instances/CHAO_MCTTRP_08.txt", "MCTTRP", 12, 6, 0)
result_ITS[[9]] <- CW_algorithm("instances/CHAO_MCTTRP_09.txt", "MCTTRP", 11, 6, 0)  ## XXX

result_ITS[[10]] <- CW_algorithm("instances/CHAO_MCTTRP_10.txt", "MCTTRP", 18, 9, 0) 
result_ITS[[11]] <- CW_algorithm("instances/CHAO_MCTTRP_11.txt", "MCTTRP", 18, 9, 0) 
result_ITS[[12]] <- CW_algorithm("instances/CHAO_MCTTRP_12.txt", "MCTTRP", 18, 9, 0) 

result_ITS[[13]] <- CW_algorithm("instances/CHAO_MCTTRP_13.txt", "MCTTRP", 23, 14, 0)
result_ITS[[14]] <- CW_algorithm("instances/CHAO_MCTTRP_14.txt", "MCTTRP", 26, 14, 0)
result_ITS[[15]] <- CW_algorithm("instances/CHAO_MCTTRP_15.txt", "MCTTRP", 25, 13, 0) 

result_ITS[[16]] <- CW_algorithm("instances/CHAO_MCTTRP_16.txt", "MCTTRP", 11, 6, 0)
result_ITS[[17]] <- CW_algorithm("instances/CHAO_MCTTRP_17.txt", "MCTTRP", 11, 6, 0)
result_ITS[[18]] <- CW_algorithm("instances/CHAO_MCTTRP_18.txt", "MCTTRP", 11, 6, 0) # x

result_ITS[[19]] <- CW_algorithm("instances/CHAO_MCTTRP_19.txt", "MCTTRP", 12, 8, 0)
result_ITS[[20]] <- CW_algorithm("instances/CHAO_MCTTRP_20.txt", "MCTTRP", 13, 8, 0)
result_ITS[[21]] <- CW_algorithm("instances/CHAO_MCTTRP_21.txt", "MCTTRP", 13, 8, 0)

for (i in 1:length(result_ITS)) {
  print(paste0(i,"  cost: ", result_ITS[[i]]$cost))
}

result_ITS <- list()
result_ITS[[1]] <- MCTTRP_opt_solver("instances/CHAO_MCTTRP_01.txt", "MCTTRP", 8, 5, total_time=60000, total_iterations=100, verbose=0)
result_ITS[[2]] <- MCTTRP_opt_solver("instances/CHAO_MCTTRP_02.txt", "MCTTRP", 8, 5, total_time=60000, total_iterations=100, verbose=0)
result_ITS[[3]] <- MCTTRP_opt_solver("instances/CHAO_MCTTRP_03.txt", "MCTTRP", 8, 5, total_time=60000, total_iterations=100, verbose=0) # x

result_ITS[[4]] <- MCTTRP_opt_solver("instances/CHAO_MCTTRP_04.txt", "MCTTRP", 13, 8, total_time=60000, total_iterations=100, verbose=0)
result_ITS[[5]] <- MCTTRP_opt_solver("instances/CHAO_MCTTRP_05.txt", "MCTTRP", 14, 8, total_time=60000, total_iterations=100, verbose=0)
result_ITS[[6]] <- MCTTRP_opt_solver("instances/CHAO_MCTTRP_06.txt", "MCTTRP", 14, 8, total_time=60000, total_iterations=100, verbose=0) 

result_ITS[[7]] <- MCTTRP_opt_solver("instances/CHAO_MCTTRP_07.txt", "MCTTRP", 11, 6, total_time=60000, total_iterations=100, verbose=0)
result_ITS[[8]] <- MCTTRP_opt_solver("instances/CHAO_MCTTRP_08.txt", "MCTTRP", 12, 6, total_time=60000, total_iterations=100, verbose=0)
result_ITS[[9]] <- MCTTRP_opt_solver("instances/CHAO_MCTTRP_09.txt", "MCTTRP", 11, 6, total_time=60000, total_iterations=100, verbose=0)

result_ITS[[10]] <- MCTTRP_opt_solver("instances/CHAO_MCTTRP_10.txt", "MCTTRP", 18, 9, total_time=60000, total_iterations=100, verbose=0)
result_ITS[[11]] <- MCTTRP_opt_solver("instances/CHAO_MCTTRP_11.txt", "MCTTRP", 18, 9, total_time=60000, total_iterations=100, verbose=0)
result_ITS[[12]] <- MCTTRP_opt_solver("instances/CHAO_MCTTRP_12.txt", "MCTTRP", 18, 9, total_time=60000, total_iterations=100, verbose=0)

result_ITS[[13]] <- MCTTRP_opt_solver("instances/CHAO_MCTTRP_13.txt", "MCTTRP", 23, 14, total_time=60000, total_iterations=2, verbose=0)
result_ITS[[14]] <- MCTTRP_opt_solver("instances/CHAO_MCTTRP_14.txt", "MCTTRP", 26, 14, total_time=60000, total_iterations=2, verbose=0)
result_ITS[[15]] <- MCTTRP_opt_solver("instances/CHAO_MCTTRP_15.txt", "MCTTRP", 25, 13, total_time=60000, total_iterations=2, verbose=0)

result_ITS[[16]] <- MCTTRP_opt_solver("instances/CHAO_MCTTRP_16.txt", "MCTTRP", 11, 6, total_time=60000, total_iterations=2, verbose=0)
result_ITS[[17]] <- MCTTRP_opt_solver("instances/CHAO_MCTTRP_17.txt", "MCTTRP", 11, 6, total_time=60000, total_iterations=2, verbose=0)
result_ITS[[18]] <- MCTTRP_opt_solver("instances/CHAO_MCTTRP_18.txt", "MCTTRP", 11, 6, total_time=60000, total_iterations=2, verbose=0)

result_ITS[[19]] <- MCTTRP_opt_solver("instances/CHAO_MCTTRP_19.txt", "MCTTRP", 13, 8, total_time=60000, total_iterations=2, verbose=0)
result_ITS[[20]] <- MCTTRP_opt_solver("instances/CHAO_MCTTRP_20.txt", "MCTTRP", 13, 8, total_time=60000, total_iterations=2, verbose=0)
result_ITS[[21]] <- MCTTRP_opt_solver("instances/CHAO_MCTTRP_21.txt", "MCTTRP", 13, 8, total_time=60000, total_iterations=2, verbose=0)

for (i in 1:length(result_ITS)) {
  print(paste0(i,"  cost: ", result_ITS[[i]]$cost))
}



cont <- 18
for (i in 1:length(result_ITS[[cont]]$result_res )) {
  res <- result_ITS[[cont]]$result_res[[i]]
  print(paste0(i,"  $type: ", res$type))
  print(paste0(i,"  $total_load: ", res$total_load))
  print(paste0(i,"  $used_hoppers_trailer: ", res$used_hoppers_trailer))
  print(paste0(i,"  $used_hoppers_truck: ", res$used_hoppers_truck))
}

cont <- 18
for (i in 1:length(result[[cont]]$result_res )) {
  res <- result[[cont]]$result_res[[i]]
  print(paste0(i,"  $route: "))
  print(res$route)
}

cont <- 3
for (i in 1:length(result[[cont]]$result_res )) {
  res <- result[[cont]]$result_res[[i]]
  print(paste0(i,"  $type: ", res$type))
}


for (i in 1:length(result)) {
  print(paste0(i,"  cost: ", result[[i]]$cost))
}
