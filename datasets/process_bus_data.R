library(tidyverse)
library(lubridate)

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

  sep_data_sets <- map(indices, ~`[`(data_file, (. + 1):(. + nb_data_points) ))

  headers_list <- map(sep_data_sets, ~`[`(., 1:11))

  header_elements <- c("bus number", "month purchased", "year purchased",
                       "month of 1st engine replacement", "year of 1st engine replacement",
                       "odometer at replacement", "month of 2nd replacement",
                       "year of 2nd replacement", "odometer at replacement",
                       "month odometer data begins", "year odometer data begins")

  create_start_date <- function(one_dataset){
      one_dataset <- pull(one_dataset)
      month <- one_dataset[10]
      year <- paste0("19", one_dataset[11])

      month <- ifelse(nchar(month) == 1, paste0("0", month), month)

      ymd(paste0(year, "-", month, "-01"))
  }

  create_first_replacement <- function(one_dataset){
      one_dataset <- pull(one_dataset, odometer_reading)
      month <- one_dataset[4]
      year <- paste0("19", one_dataset[5])

      month <- ifelse(nchar(month) == 1, paste0("0", month), month)

      ymd(paste0(year, "-", month, "-01"))
  }

  create_second_replacement <- function(one_dataset){
      one_dataset <- pull(one_dataset, odometer_reading)
      month <- one_dataset[7]
      year <- paste0("19", one_dataset[8])

      month <- ifelse(nchar(month) == 1, paste0("0", month), month)

      ymd(paste0(year, "-", month, "-01"))
  }

  get_bus_id <- function(one_dataset){
      one_dataset <- pull(one_dataset, odometer_reading)
      one_dataset[1]
  }

  named_headers <- map(headers_list, ~set_names(., header_elements))


  raw_data <- map(sep_data_sets, ~tibble("odometer_reading" = .))
  raw_data <- map(raw_data, ~mutate(., "date" = create_start_date(.)))
  raw_data <- map(raw_data, ~mutate(., "first_replacement_date" = create_first_replacement(.)))
  raw_data <- map(raw_data, ~mutate(., "second_replacement_date" = create_second_replacement(.)))
  raw_data <- map(raw_data, ~mutate(., "bus_id" = get_bus_id(.)))
  raw_data <- map(raw_data, ~slice(., -c(1:11)))

  fill_dates <- function(vector){
      for(i in 2:length(vector)){
          vector[i] <- add_with_rollback(vector[i-1], months(1))
          #vector[i] <- if_else(vector[i] == ymd("1980-07-01"), add_with_rollback(vector[i], months(2)),
          #                    vector[i])
      }
      vector
  }

  raw_data <- raw_data %>%
      map(~mutate(., date = fill_dates(date)))

  raw_data <- map(raw_data, ~mutate(., "replacement_1" = if_else(date == first_replacement_date, 1, 0)))
  raw_data <- map(raw_data, ~mutate(., "replacement_2" = if_else(date == second_replacement_date, 1, 0, 0)))
  raw_data <- map(raw_data, ~mutate(., replacement = replacement_1 + replacement_2))
  raw_data <- map(raw_data, ~select(., bus_id, date, odometer_reading, replacement,
                                    -replacement_1, -replacement_2, -first_replacement_date, -second_replacement_date))

  return(raw_data)
}

processed_data <- map(data_files, process_bus_data)

processed_data <- processed_data %>%
    map(bind_rows)

dataset_names <- str_remove_all(data_file_path, "datasets/|\\.asc")

processed_data <- set_names(processed_data, dataset_names)

processed_data <- processed_data %>%
    map(., ~filter(., !is.na(bus_id)))

walk2(.x = processed_data,
      .y = dataset_names,
      ~write_csv(.x, path = paste0("datasets/processed_data/", .y, ".csv")))
