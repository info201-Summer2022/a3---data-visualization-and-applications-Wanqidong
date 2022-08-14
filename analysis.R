#libraries
library(dplyr)
library(ggplot2)
library(tidyverse)
library(tidyr)
library(stringr)
library(usmap)
library(plotly)

#read the incarceration trends file
incarceration_df <- read.csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv")

#pick out the data to form a new dataset
incarceration_race_df <- select(incarceration_df, year, state, total_pop, 
                                total_prison_pop, aapi_prison_pop, 
                                black_prison_pop, latinx_prison_pop, 
                                native_prison_pop, white_prison_pop, 
                                total_prison_pop_rate, aapi_prison_pop_rate, 
                                black_prison_pop_rate, latinx_prison_pop_rate, 
                                native_prison_pop_rate,  white_prison_pop_rate)

#The first value: average value of different race prison pop across all the counties
average_appi <- mean(incarceration_race_df$aapi_prison_pop, na.rm = TRUE)
average_black <- mean(incarceration_race_df$black_prison_pop, na.rm = TRUE)
average_latinx <- mean(incarceration_race_df$latinx_prison_pop, na.rm = TRUE)
average_native <- mean(incarceration_race_df$native_prison_pop, na.rm = TRUE)
average_white <- mean(incarceration_race_df$white_prison_pop, na.rm = TRUE)
average_race_df <- rbind(average_appi, average_black, average_latinx, 
                         average_native, average_white)

#The second value: the highest value of different race prison pop
high_appi <- max(incarceration_race_df$aapi_prison_pop, na.rm = TRUE)
high_black <- max(incarceration_race_df$black_prison_pop, na.rm = TRUE)
high_latinx <- max(incarceration_race_df$latinx_prison_pop, na.rm = TRUE)
high_native <- max(incarceration_race_df$native_prison_pop, na.rm = TRUE)
high_white <- max(incarceration_race_df$white_prison_pop, na.rm = TRUE)
high_race_df <- rbind(average_appi, average_black, average_latinx, 
                         average_native, average_white)

#The third value: different race prison pop's change over year(1983-2016)
min_year_df <- filter(incarceration_race_df, year == "1983", na.rm = TRUE)
min_year_aapi <- sum(min_year_df$aapi_prison_pop, na.rm = TRUE)
min_year_black <- sum(min_year_df$black_prison_pop, na.rm = TRUE)
min_year_latinx <- sum(min_year_df$latinx_prison_pop, na.rm = TRUE)
min_year_native <- sum(min_year_df$native_prison_pop, na.rm = TRUE)
min_year_white <- sum(min_year_df$white_prison_pop, na.rm = TRUE)
min_year_df_1983 <- c(min_year_aapi, min_year_black, min_year_latinx, 
                     min_year_native, min_year_white)

max_year_df <- filter(incarceration_race_df, year == "2016", na.rm = TRUE)
max_year_aapi <- sum(max_year_df$aapi_prison_pop, na.rm = TRUE)
max_year_black <- sum(max_year_df$black_prison_pop, na.rm = TRUE)
max_year_latinx <- sum(max_year_df$latinx_prison_pop, na.rm = TRUE)
max_year_native <- sum(max_year_df$native_prison_pop, na.rm = TRUE)
max_year_white <- sum(max_year_df$white_prison_pop, na.rm = TRUE)
max_year_df_2016 <- c(max_year_aapi, max_year_black, max_year_latinx, 
                     max_year_native, max_year_white)

difference_aapi <- max_year_aapi - min_year_aapi
difference_black <- max_year_black - min_year_black
difference_latinx <- max_year_latinx - min_year_latinx
difference_native <- max_year_native - min_year_native
difference_white <- max_year_white - min_year_white
difference_df <- c(difference_aapi, difference_black, difference_latinx, 
                       difference_native, difference_white)

change_year_df <- data.frame(name = c("aapi", "black", "latinx", "native", "white"), 
                             min_year_df_1983, max_year_df_2016, difference_df)

#The fourth value: different race prison pop about different state from 1970 to 2018
state_df <- select(incarceration_df, state, aapi_prison_pop, 
                   black_prison_pop, latinx_prison_pop, 
                   native_prison_pop, white_prison_pop)
new_state_df <- state_df %>%
  group_by(state) %>%
  summarise(aapi_prison_pop = sum(aapi_prison_pop, na.rm = TRUE),  
            black_prison_pop = sum(black_prison_pop, na.rm = TRUE), 
            latinx_prison_pop = sum(latinx_prison_pop, na.rm = TRUE), 
            native_prison_pop = sum(native_prison_pop, na.rm = TRUE), 
            white_prison_pop = sum(white_prison_pop, na.rm = TRUE))

##The fifth value: average value of different race prison pop rate across all the counties
average_appi_rate <- mean(incarceration_race_df$aapi_prison_pop_rate, 
                          na.rm = TRUE)
average_black_rate <- mean(incarceration_race_df$black_prison_pop_rate, 
                           na.rm = TRUE)
average_latinx_rate <- mean(incarceration_race_df$latinx_prison_pop_rate, 
                            na.rm = TRUE)
average_native_rate <- mean(incarceration_race_df$native_prison_pop_rate, 
                            na.rm = TRUE)
average_white_rate <- mean(incarceration_race_df$white_prison_pop_rate, 
                           na.rm = TRUE)
average_race_rate_df <- rbind(average_appi_rate, average_black_rate, 
                              average_latinx_rate, average_native_rate, 
                              average_white_rate)
#summary information
summary_info <- list()
summary_info$average_race_df <- average_race_df
summary_info$high_race_df <- high_race_df
summary_info$change_year_df <- change_year_df
summary_info$new_state_df <- new_state_df
summary_info$average_race_rate_df <- average_race_rate_df

#Trends over time chart
#the chart of average value of different race prison pop over time
#create the new dataset based on years
year_df <- select(incarceration_df, year, aapi_prison_pop, 
                   black_prison_pop, latinx_prison_pop, 
                   native_prison_pop, white_prison_pop)
new_year_df <- year_df %>%
  group_by(year) %>%
  summarise(aapi_prison_pop = sum(aapi_prison_pop, na.rm = TRUE),  
            black_prison_pop = sum(black_prison_pop, na.rm = TRUE), 
            latinx_prison_pop = sum(latinx_prison_pop, na.rm = TRUE), 
            native_prison_pop = sum(native_prison_pop, na.rm = TRUE), 
            white_prison_pop = sum(white_prison_pop, na.rm = TRUE))

#line chart
chart_1 <- ggplot(data = new_year_df, aes(x = year, y = race)) +
  geom_line(aes(x = year, y = aapi_prison_pop, color = "black")) +
  geom_line(aes(x = year, y = black_prison_pop, color = "blue"))+
  geom_line(aes(x = year, y = latinx_prison_pop, color = "green")) +
  geom_line(aes(x = year, y = native_prison_pop, color = "pink")) +
  geom_line(aes(x = year, y = white_prison_pop, color = "yellow"))+
  scale_color_manual(name = "race", 
                     values = c("black", "blue", "green", "pink", "yellow"), 
                     labels = c("aapi", "black", "latinx", "native", "white")) +
  scale_fill_manual(name = "race", 
                    values = c("black", "blue", "green", "pink", "yellow"), 
                    labels = c("aapi", "black", "latinx", "native", "white")) +
  ggtitle("average value of different race prison pop over time") +
  labs(x = "year", y = "race")

#Variable comparison chart
#the chart of the comparison of prison pop in GA between black and white over years
#create new dataset
prison_pop_GA <- filter(incarceration_race_df, state == "GA")
prison_pop_ga <- select(prison_pop_GA, year, black_prison_pop, white_prison_pop)
prison_pop_GA_new <- prison_pop_ga %>%
  group_by(year) %>%
  summarise(black_prison_pop = sum(black_prison_pop, na.rm = TRUE), 
            white_prison_pop = sum(white_prison_pop, na.rm = TRUE))

#scatter plot
chart_2 <- plot_ly(prison_pop_GA_new, x = ~year)
chart_2 <- chart_2 %>% add_trace(y = ~black_prison_pop, 
                                 name = 'black_prison_pop',mode = 'points')
chart_2 <- chart_2 %>% add_trace(y = ~white_prison_pop, 
                                 name = 'white_prison_pop',mode = 'points') %>%
  layout(title = "the comparison of prison pop in GA between black and white over years", 
         xaxis = list(title = "year"), 
         yaxis = list(title = "prison_pop"))

#map: the black prison pop of different states over years
black_new_state_df <- select(new_state_df, state, black_prison_pop)
map <- plot_usmap(data = black_new_state_df, values = "black_prison_pop", 
                  color = "orange", labels = FALSE) + 
  scale_fill_continuous(low = "white", high = "orange", 
                        name = "black_prison_pop", label = scales::comma) +
  theme(legend.position = "right") + 
  theme(panel.background = element_rect(color = "black")) + 
  labs(title = "black prison pop of different states over years") 

