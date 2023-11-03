library(tidyverse) 

traffic_data <- read_csv(here::here("dataset-ignore", "Traffic_Violations_20231025.csv")) 

## CLEAN the data
traffic_data_clean <- traffic_data |>   
  select("Date Of Stop", 
         "Time Of Stop", 
         Description, 
         Location, 
         Accident, 
         Fatal, 
         Alcohol, 
         State, 
         Year, 
         Make, 
         Model, 
         "Violation Type", 
         Race, 
         Gender, 
         "Arrest Type") 

write_csv(traffic_data_clean, file = here::here("dataset-ignore", "traffic_data_clean.csv")) 

save(traffic_data_clean, file = here::here("dataset/traffic_violations.RData"))