# SCRIPT 
################################################################################
library(mcttrpalns)
#################################################################################
options(show.error.locations = TRUE)
options(error=function()traceback(2))
options(max.print=3000)

args<-commandArgs(TRUE)

# input paramenters:
n_prob <- as.numeric(args[1])
num_truck <- as.numeric(args[2])
num_trailer <- as.numeric(args[3])
descent_s <- as.numeric(args[4])
time_exec <- as.numeric(args[5])
type <- args[6]
init_instance <- as.numeric(args[7])
end_instance <- as.numeric(args[8])

# 1 5 3 1 300 "TTRP"

if (type == "TTRP") {
	if (n_prob < 10) {
	  str_file <- paste0("instances/CHAO_TTRP_0", n_prob,".txt")
	} else {
	  str_file <- paste0("instances/CHAO_TTRP_", n_prob,".txt")
	}
} else {
        if (n_prob < 10) {
          str_file <- paste0("instances/CHAO_MCTTRP_0", n_prob,".txt")
        } else {
          str_file <- paste0("instances/CHAO_MCTTRP_", n_prob,".txt")
        }
}

problemaP1<- CW_algorithm(str_file, type, num_truck, num_trailer, 0, descent_s)
input <- problemaP1$input
initial_solution <- problemaP1$result_res
init_time <- Sys.time()
problem_type=type
type_problem=type


print(paste0(calculateTotalDistance(input, all_routes(initial_solution))))

input$max_iter = Inf
input$max_time = time_exec
input$vtr = -Inf

prob1_seeds <- seq(init_instance, end_instance, by = 100)
problems_list <- list()
for(s in 1:length(prob1_seeds)){
  problems_list[[s]] <- ALNS_core(input, problemaP1, initial_solution, init_time, type_problem, prob1_seeds[s])
  iter_str <- paste0("Problem ", n_prob, "| type_problem ", type_problem, "| descent ", descent_s, "| init cost (CW): " , calculateTotalDistance(input, all_routes(initial_solution)), 
                     "| cost ALNS: ", problems_list[[s]]$bestcost)
  problems_list[[s]]$n_prob <- n_prob
  problems_list[[s]]$initial_solution <- initial_solution
  print(iter_str)
}

str_save <- paste0("problem", n_prob, "_", type, "_descent", descent_s)
save(problems_list, file=paste0(str_save,".RData"))


#for(s in 1:length(problems_list)){
#  iter_str <- paste0("Problem ", problems_list[[s]]$n_prob, "| init cost (CW): " , calculateTotalDistance(input, all_routes(problems_list[[s]]$initial_solution)), 
#                     "| cost ALNS: ", problems_list[[s]]$bestcost)
#  print(iter_str)
#}
