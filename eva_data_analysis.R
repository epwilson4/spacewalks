#Emma P Wilson
#3-31-26
#This code's purpose: To take in a data file regarding EVA times and provides a graph showing the total time spend in space from 1965 to present date



# https://data.nasa.gov/resource/eva.json (with modifications)

#Files
input_file = 'eva-data.json'#gives input data file a variable name"
output_file = 'eva-data.csv'#gives output data file a variable name"
graph_file = 'cumulative_eva_graph.png'#gives graph file a varible name"

library(jsonlite)
library(lubridate)
library(tidyverse)

#This reads data in the data and arranges
eva_tbl <- jsonlite::fromJSON(input_file) |>
  as_tibble()

j_l <- read_json(input_file)
data=as.data.frame(j_l[[1]])

for( i in 2:374){
  r = j_l[[i]]
  print(r)
  data =merge(data, as.data.frame(r),  all=TRUE)
}
#data.pop(0)

#Writes a CVS
write.csv(output_file)



time <- c()
date = Date()

j=1
for (i in rownames(data)){
  print(data[j, ])
  # and this bit
  # w.writerow(data[j].values())
  if (!is.na(data[j,]$duration)){
    duration_str=data[j,]$duration
    if(duration_str == ''){
      #do nothing
    }else{
      duration_dt=as.POSIXlt(duration_str,format='%H:%M')
      duration_hours <- as.numeric(as.difftime(hour(duration_dt), units = 'hours')+as.difftime(minute(duration_dt), units='mins')+as.difftime(second(duration_dt), units='secs'))/(60*60)
      print(duration_dt,duration_hours)
      time <- c(time, duration_hours)
      if(!is.na(data[j,]$date)){
        date= c(date, as.Date(substr(data[j,'date'], 1, 10), format = '%Y-%m-%d'))
        #date.append(data[j]['date'][0:10])
        
      }else{
        time <- time[1:length(time) -1]
      }
    }
  }
  j = j+1
}

duration_dt=0
for(i in time)
  duration_dt <- c(duration_dt, duration_dt[length(duration_dt)]+i)


df <- data.frame(
  date, time
)[order(date, time), ]

date <- df$date
time <- df$time
cumulative_time <- duration_dt[2:length(duration_dt)]

#plots the data
cumulative_spacetime_plot <- ggplot(df, aes(x = date, y = cumulative_time)) +  
  geom_point() +
  geom_line() +  
  labs(x = "Year", y = "Total time spent in space to date (hours)") +
  theme_minimal()

#saves the graph produced as a file
ggsave(graph_file, plot = cumulative_spacetime_plot, width = 9, height = 5, dpi = 300)

#prints graph 
print(cumulative_spacetime_plot)