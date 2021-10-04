#' Parse the input file, and generate all information to solve a MC-TTRP problem.
#'
#' @param string The path of the file to parse.
#' @return A list with all the information to call the solver.
input_MCTTRP<-function(string) {

  if (grepl(".csv", string, fixed = TRUE)) {
    table <- read.csv(string, nrows=1, skip =0 )
    Truck_CAP <- as.numeric(table[1,1])
    Trailer_CAP <- as.numeric(table[1,2])
    N_Customers_CAP <- as.numeric(table[1,3])
    n_trucks <- as.numeric(table[1,4])
    n_trailers <- as.numeric(table[1,5])
    nf <- as.numeric(table[1,6])
    Hoppers_Truck_size <- as.numeric(table[1,7])
    Hoppers_Trailer_size <- as.numeric(table[1,8])

    table <- read.csv(string, nrows=(1+N_Customers_CAP), skip =2 )
    distance_matrix_1 <- data.matrix(table[2:(2+N_Customers_CAP)])
    type_nr <- data.matrix(table[(3+N_Customers_CAP)])
    demand_vector_list_1 <- data.matrix(table[(4+N_Customers_CAP):(4+N_Customers_CAP+nf-1)])

    # extract the order
    clients_index <- c(1)
    for (i in 2:length(type_nr)) {
      if (type_nr[i] == 0) {
        clients_index <- c(clients_index, i)
      }
    }
    size_n1 = length(clients_index) - 1
    for (i in 2:length(type_nr)) {
      if (type_nr[i] == 1) {
        clients_index <- c(clients_index, i)
      }
    }

    # reorder demand_vector, and coordinate vectors
    matriz.demandas <- demand_vector_list_1[clients_index,]

    # reorder demand_vector, and coordinate vectors
    distance_matrix <- matrix(0, nrow = N_Customers_CAP+1, ncol = N_Customers_CAP+1)
    for (i in 1:(N_Customers_CAP+1)) {
      for (j in 1:(N_Customers_CAP+1)) {
          distance_matrix[i,j] <- distance_matrix_1[i,j]
      }
    }
    matriz.distancia <- distance_matrix

  }
  else if (grepl(".txt", string, fixed = TRUE)){

      result <- try({
        lines <- readLines(string)
      }, silent = TRUE)

      if ( "try-error" %in% class(result) ) {
        err_msg <- geterrmessage()
        stop()
      }

      counter_lines = 0
      for (i in 1:length(lines)) {
        if (grepl("#", lines[i], fixed = TRUE) == FALSE) {
          if (counter_lines == 0) {
            data_line1 <- unlist(strsplit(lines[i], split = "\\s"))
            Truck_CAP <- as.numeric(data_line1[1])
            Trailer_CAP <- as.numeric(data_line1[2])
            N_Customers_CAP <- as.numeric(data_line1[3])
            n_trucks <- as.numeric(data_line1[4])
            n_trailers <- as.numeric(data_line1[5])
            counter_lines = counter_lines + 1
          }
          else if (counter_lines == 1) {
            data_line1 <- unlist(strsplit(lines[i], split = "\\s"))
            nf <- as.numeric(data_line1[1])
            Hoppers_Truck_size <- as.numeric(data_line1[2])
            Hoppers_Trailer_size <- as.numeric(data_line1[3])
            counter_lines = counter_lines + 1
          }
          else if (counter_lines == 2) {
            data_line1 <- unlist(strsplit(lines[i], split = "\\s"))
            coord_x_nr <- c(as.numeric(data_line1[2]))
            coord_y_nr <- c(as.numeric(data_line1[3]))
            type_nr <- c(0)
            demand_vector_nr_list <- list()
            for (j in 1:nf) {
              demand_vector_nr_list[j] <- c(0)
            }
            counter_lines = counter_lines + 1
          }
          else if (counter_lines > 2) {
            data_line1 <- unlist(strsplit(lines[i], split = "\\s"))
            coord_x_nr <- c(coord_x_nr, as.numeric(data_line1[2]))
            coord_y_nr <- c(coord_y_nr, as.numeric(data_line1[3]))
            type_nr <- c(type_nr, as.numeric(data_line1[4]))
            for (j in 1:nf) {
              demand_vector_nr_list[[j]] <- c(demand_vector_nr_list[[j]], as.numeric(data_line1[4+j]))
            }
            counter_lines = counter_lines + 1
          }
        }
    }

    # extract the order
    clients_index <- c(1)
    for (i in 2:length(type_nr)) {
      if (type_nr[i] == 0) {
        clients_index <- c(clients_index, i)
      }
    }
    size_n1 = length(clients_index) - 1
    for (i in 2:length(type_nr)) {
      if (type_nr[i] == 1) {
        clients_index <- c(clients_index, i)
      }
    }

    # reorder demand_vector, and coordinate vectors
    coord_x <- c(coord_x_nr[clients_index[1]])
    for (i in 2:length(clients_index)) {
      coord_x <- c(coord_x, coord_x_nr[clients_index[i]])
    }

    coord_y <- c(coord_y_nr[clients_index[1]])
    for (i in 2:length(clients_index)) {
      coord_y <- c(coord_y, coord_y_nr[clients_index[i]])
    }

    demand_vector_list = list()
    for (j in 1:nf) {
      auxl <- unlist(demand_vector_nr_list[[j]])
      demand_vector_list[[j]] <- c(auxl[clients_index[1]])
      for (i in 2:length(clients_index)) {
        demand_vector_list[[j]] <-c(demand_vector_list[[j]],auxl[clients_index[i]])
      }
    }

    # calc distance matrix
    distance_matrix <- matrix(0, nrow = N_Customers_CAP+1, ncol = N_Customers_CAP+1)
    for (i in 1:(N_Customers_CAP+1)) {
      for (j in 1:(N_Customers_CAP+1)) {
        if (i != j) {
          distance_matrix[i,j] <- sqrt((coord_x[j]-coord_x[i])^2+(coord_y[j]-coord_y[i])^2)
        } else {
          distance_matrix[i,j] <- 0
        }
      }
    }

    # DEMANDAS, por exemplo 2 tipos de pienso,
    # polo momento teÃ±en as mesmas demandas de 1 tipo de pienso que do outro:
    matriz.demandas <- matrix(unlist(demand_vector_list[1]),ncol=1,byrow=T)
    for ( i in 2:nf) {
      matriz.demandas <- cbind(matriz.demandas, unlist(demand_vector_list[i]))
    }
  } else {
    print("ERROR[2]: Incorrect extension of input file (.txt or .csv)")
  }

  # DATOS FINAIS
  # capacity matrices

  capacidad.truck <- Truck_CAP
  capacidad.trailer <- Trailer_CAP
  for (i in 2:n_trucks) {
    capacidad.truck<-c(capacidad.truck, Truck_CAP)
  }
  for (i in 2:n_trailers) {
    capacidad.trailer<-c(capacidad.trailer, Trailer_CAP)
  }
  capacidad.vehiculo <- capacidad.truck[1:length(capacidad.trailer)]+
                        capacidad.trailer

  # Number of clients
  N1 <- 1:size_n1
  n1 <- length(N1)

  # Tolvas de los camiones
  n_hopper <- Truck_CAP / Hoppers_Truck_size
  h.camion <- Hoppers_Truck_size
  for (i in 2:n_hopper) {
    h.camion <- c(h.camion,Hoppers_Truck_size)
  }
  H.camion <- matrix(rep(h.camion, n_trucks),ncol=n_hopper)

  # TOlvas de los  traileres
  n_hopper <- Trailer_CAP / Hoppers_Trailer_size
  h.trailer <- Hoppers_Trailer_size
  for (i in 2:n_hopper) {
    h.trailer <- c(h.trailer,Hoppers_Trailer_size)
  }
  H.trailer <- matrix(rep(h.trailer, n_trailers),ncol=n_hopper)

  # Matriz de distancias
  matriz.distancia<-distance_matrix

  colnames(matriz.distancia)=0:(dim(matriz.distancia)[2]-1)
  rownames(matriz.distancia)=0:(dim(matriz.distancia)[1]-1)

  result <- list()
  result$capacidad.truck <- capacidad.truck
  result$capacidad.trailer <- capacidad.trailer
  result$capacidad.vehiculo <- capacidad.vehiculo
  result$N1 <- N1
  result$n1 <- n1
  result$h.camion <- h.camion
  result$H.camion <- H.camion
  result$h.trailer <- h.trailer
  result$H.trailer <- H.trailer
  result$matriz.distancia <- matriz.distancia
  result$matriz.demandas <- matriz.demandas
  result$nf <- nf
  verbose <- 0

  return(result)
}


#' Parse the input file, and generate all information to solve a TTRP problem.
#'
#' @param string The path of the file to parse.
#' @return A list with all the information to call the solver.
input_TTRP<-function(string) {

  if (grepl(".txt", string, fixed = TRUE)) {
    lines <- readLines(string)

    counter_lines = 0
    for (i in 1:length(lines)) {
      if (grepl("#", lines[i], fixed = TRUE) == FALSE) {
        if (counter_lines == 0) {
          data_line1 <- unlist(strsplit(lines[i], split = "\\s"))
          Truck_CAP <- as.numeric(data_line1[1])
          Trailer_CAP <- as.numeric(data_line1[2])
          N_Customers_CAP <- as.numeric(data_line1[3])
          n_trucks <- as.numeric(data_line1[4])
          n_trailers <- as.numeric(data_line1[5])
          counter_lines = counter_lines + 1
        }
        else if (counter_lines == 1) {
          data_line1 <- unlist(strsplit(lines[i], split = "\\s"))
          coord_x_nr <- as.numeric(data_line1[2])
          coord_y_nr <- as.numeric(data_line1[3])
          demand_vector_list_nr <- as.numeric(data_line1[4])
          type_nr <- 0
          counter_lines = counter_lines + 1
        }
        else if (counter_lines > 1) {
          data_line1 <- unlist(strsplit(lines[i], split = "\\s"))
          coord_x_nr <- c(coord_x_nr, as.numeric(data_line1[2]))
          coord_y_nr <- c(coord_y_nr, as.numeric(data_line1[3]))
          demand_vector_list_nr <- c(demand_vector_list_nr, as.numeric(data_line1[4]))
          type_nr <- c(type_nr, as.numeric(data_line1[5]))
          counter_lines = counter_lines + 1
        }
      }
    }

    # extract the order
    clients_index <- c(1)
    for (i in 2:length(type_nr)) {
      if (type_nr[i] == 0) {
        clients_index <- c(clients_index, i)
      }
    }
    size_n1 = length(clients_index) - 1
    for (i in 2:length(type_nr)) {
      if (type_nr[i] == 1) {
        clients_index <- c(clients_index, i)
      }
    }

    # reorder demand_vector, and coordinate vectors
    coord_x <- c(coord_x_nr[clients_index[1]])
    for (i in 2:length(clients_index)) {
      coord_x <- c(coord_x, coord_x_nr[clients_index[i]])
    }

    coord_y <- c(coord_y_nr[clients_index[1]])
    for (i in 2:length(clients_index)) {
      coord_y <- c(coord_y, coord_y_nr[clients_index[i]])
    }

    demand_vector_list <- c(demand_vector_list_nr[clients_index[1]])
    for (i in 2:length(clients_index)) {
      demand_vector_list <- c(demand_vector_list, demand_vector_list_nr[clients_index[i]])
    }

    # calc distance matrix
    distance_matrix <- matrix(0, nrow = N_Customers_CAP+1, ncol = N_Customers_CAP+1)
    for (i in 1:(N_Customers_CAP+1)) {
      for (j in 1:(N_Customers_CAP+1)) {
        if (i != j) {
          distance_matrix[i,j] <- sqrt((coord_x[j]-coord_x[i])^2+(coord_y[j]-coord_y[i])^2)
        } else {
          distance_matrix[i,j] <- 0
        }
      }
    }
  }
  else {
    print("ERROR[2]: Incorrect extension of input file (.txt)")
  }

  capacidad.truck <- Truck_CAP
  capacidad.trailer <- Trailer_CAP
  capacidad.vehiculo <- capacidad.truck+capacidad.trailer

  N1 <- 1:size_n1
  n1 <- length(N1)

  matriz.distancia<-distance_matrix

  colnames(matriz.distancia)=0:(dim(matriz.distancia)[2]-1)
  rownames(matriz.distancia)=0:(dim(matriz.distancia)[1]-1)

  # Create result list
  result <- list()
  result$capacidad.truck <- capacidad.truck
  result$capacidad.trailer <- capacidad.trailer
  result$capacidad.vehiculo <- capacidad.vehiculo
  result$N1 <- N1
  result$n1 <- n1
  result$matriz.distancia <- matriz.distancia
  result$matriz.demandas <- demand_vector_list


  return(result)
}

