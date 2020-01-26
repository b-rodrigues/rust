library(tidyverse)
library(brotools)

all_bus_data <- read_csv("datasets/processed_data/all_buses.csv")

ggplot(all_bus_data, aes(y = odometer_reading, x = date, col = bus_family)) +
    #geom_point() +
    geom_smooth() +
    theme_blog()


