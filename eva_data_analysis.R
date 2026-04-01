#Emma P Wilson
#3-31-26
#This code's purpose: To take in a data file regarding EVA times and provides a graph showing the total time spend in space from 1965 to present date


# https://data.nasa.gov/resource/eva.json (with modifications) original location of data
library(tidyverse) #tidyverse "contains" ggplot2
library(jsonlite)
library(lubridate)

# Files
input_file  <- "./eva-data.json"
output_file <- "./eva-data.csv"
graph_file  <- "./cumulative_eva_graph.png"

# 1) Reads JSON array into a tibble
eva_tbl <- jsonlite::fromJSON(input_file) |>
  as_tibble()

# 2) Convert types + drop missing duration/date
eva_tbl <- eva_tbl |>
  mutate(
    eva  = as.numeric(eva),
    date = ymd_hms(date, quiet = TRUE) ) |>
  filter(!is.na(duration), duration != "", !is.na(date))

# 3) Creates a CSV file
readr::write_csv(eva_tbl, output_file)

# 4) Sorts data by date
eva_tbl <- eva_tbl |>
  arrange(date)

# 5) duration_hours summed after ever EVA to solve for cumulative_time
eva_tbl <- eva_tbl |>
  mutate(
    duration_hours = {
      parts <- str_split(duration, ":", n = 2, simplify = TRUE)
      as.numeric(parts[, 1]) + as.numeric(parts[, 2]) / 60
    },
    cumulative_time = cumsum(duration_hours)
  )

# 6) Plots Year on X-axis and Total time spent in space (hours) on Y axis
#Saves Plot
cumulative_spacetime_plot <- ggplot(eva_tbl, aes(x = date, y = cumulative_time)) +
  geom_point() +
  geom_line() +
  labs(
    x = "Year",
    y = "Total time spent in space to date (hours)"
  ) +
  theme_minimal()

ggsave(graph_file, plot = cumulative_spacetime_plot, width = 9, height = 5, dpi = 300)
print(cumulative_spacetime_plot)
