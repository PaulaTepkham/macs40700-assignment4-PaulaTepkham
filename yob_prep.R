
library(dplyr)
library(readr)
library(tidyr)

years <- seq(1880, 2023, by = 5)

read_yearly_data <- function(year) {
  file_name <- paste0("names/yob", year, ".txt")
  
  if (file.exists(file_name)) {
    data <- read_csv(file_name, col_names = c("name", "gender", "births"))
  
    total_births <- data %>%
      group_by(gender) %>%
      summarize(total_births = sum(births), .groups = "drop")
    
    data <- data %>%
      group_by(gender) %>%
      mutate(
        rank = dense_rank(desc(births))
      ) %>%
      ungroup()
    
    data <- left_join(data, total_births, by = "gender") %>%
      mutate(births_percent = (births / total_births) * 100) %>%
      select(-total_births) 
    
    data$year <- year
    
    data <- data %>% select(year, name, gender, births, births_percent, rank)
    
    return(data)
  } else {
    return(NULL)
  }
}

long_format_data <- bind_rows(lapply(years, read_yearly_data))

print(head(long_format_data))

write_csv(long_format_data, "long_format_yob_1880_2023_10years.csv")

