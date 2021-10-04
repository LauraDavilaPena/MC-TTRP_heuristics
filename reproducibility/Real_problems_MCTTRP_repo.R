library(mcttrpcw)

options(show.error.locations = TRUE)
options(error=function()traceback(2))
options(max.print=3000)
problems <- c("P1", "P2","P3","P4","P5","P6","P7","P8")

for (i in problems) {
    string <- paste0("instances\\", i ,".csv")
    result <- CW_algorithm(string, "MCTTRP", 2, 1, 0)
}


result1 <- MCTTRP_opt_solver("instances/P1.csv", "MCTTRP", 2, 1, total_time=60, total_iterations=1, verbose=0)



result2 <- CW_algorithm("instances/P1.csv", "MCTTRP", 2, 1, 0)
result2 <- CW_algorithm("instances/P2.csv", "MCTTRP", 2, 1, 0)
result2 <- CW_algorithm("instances/P3.csv", "MCTTRP", 2, 1, 0)
result2 <- CW_algorithm("instances/P4.csv", "MCTTRP", 2, 1, 0)
result2 <- CW_algorithm("instances/P5.csv", "MCTTRP", 2, 1, 0)
result2 <- CW_algorithm("instances/P6.csv", "MCTTRP", 2, 1, 0)
result2 <- CW_algorithm("instances/P7.csv", "MCTTRP", 2, 1, 0)
result2 <- CW_algorithm("instances/P8.csv", "MCTTRP", 2, 1, 0)


Piensos <- list()
Piensos[[1]] <- MCTTRP_opt_solver("instances/P1.csv", "MCTTRP", 2, 1, total_time=60, total_iterations=100, verbose=0)
Piensos[[2]] <- MCTTRP_opt_solver("instances/P2.csv", "MCTTRP", 2, 1, total_time=60, total_iterations=100, verbose=0)
Piensos[[3]] <- MCTTRP_opt_solver("instances/P3.csv", "MCTTRP", 2, 1, total_time=60, total_iterations=100, verbose=0)
Piensos[[4]] <- MCTTRP_opt_solver("instances/P4.csv", "MCTTRP", 2, 1, total_time=60, total_iterations=100, verbose=0)
Piensos[[5]] <- MCTTRP_opt_solver("instances/P5.csv", "MCTTRP", 2, 1, total_time=60, total_iterations=100, verbose=0)
Piensos[[6]] <- MCTTRP_opt_solver("instances/P6.csv", "MCTTRP", 2, 1, total_time=60, total_iterations=100, verbose=0)
Piensos[[7]] <- MCTTRP_opt_solver("instances/P7.csv", "MCTTRP", 2, 1, total_time=60, total_iterations=100, verbose=0)
Piensos[[8]] <- MCTTRP_opt_solver("instances/P8.csv", "MCTTRP", 2, 1, total_time=60, total_iterations=100, verbose=0)

save(Piensos, file = "Piensos.Rdata")

