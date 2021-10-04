# SCRIPT 
################################################################################
library(mcttrpalns)
#################################################################################
options(show.error.locations = TRUE)
options(error=function()traceback(2))
options(max.print=3000)

# input paramenters:
n_prob <- 1
num_truck <- 8
num_trailer <- 5 
descent_s <- 1
time_exec <- 300 
type <- "MCTTRP"

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

print("ALNS_core")
result <- ALNS_core(input, problemaP1, initial_solution, init_time, type_problem, 100)

iter_str <- paste0("Problem ", n_prob, "| type_problem ", type_problem, "| descent ", descent_s, "| init cost (CW): " , calculateTotalDistance(input, all_routes(initial_solution)), 
                     "| cost ALNS: ", result$bestcost)

