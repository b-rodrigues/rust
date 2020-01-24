library(tidyverse)

data_file_path <- Sys.glob("datasets/*.asc")

data_files <- map(data_file_path, read_lines)

process_bus_data <- function(data_file){
  data_file <- as.numeric(data_file)
  first_bus <- data_file[1]
  second_bus <- first_bus + 1
  second_bus_index <- which(data_file == second_bus)

  nb_data_points <- second_bus_index - 1

  nb_buses <- length(data_file) / nb_data_points

  indices <- nb_data_points * seq(1, nb_buses)

  indices <- c(0, indices)
  
  map(indices, ~`[`(data_file, (. + 1):(. + nb_data_points) ))

}


map(data_files, process_bus_data)
