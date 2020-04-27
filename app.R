# PORES COVID Hackathon
# Cameron Cabo
# GAFL 531

# ================================================================================ #
# Packages
# ================================================================================ #

library(ggplot2)
library(zoo)
library(magrittr)
library(dplyr)
library(gridExtra)
library(haven)

# ================================================================================ #
# Constants
# ================================================================================ #

RED <- "#FF5A5F"
GREEN <- "#00A699"
ORANGE <- "#FC642D"

# ================================================================================ #
# Read in data
# ================================================================================ #

data_agg <- read.csv("https://covidtracking.com/api/v1/states/current.csv")
data_daily <- read.csv("https://covidtracking.com/api/v1/states/daily.csv")
data_census <- read.csv("./state-census-data.csv", stringsAsFactors = FALSE)

# ================================================================================ #
# Understanding the data
# ================================================================================ #

colnames(data_agg)

data_agg[order(data_agg$totalTestResults, decreasing=TRUE),
         c('state', 'totalTestResults', 'score', 'grade', 'hospitalized', 'death')]

# NOTE 'hospitalized' is not a well-reported column (NA for many states), 'death' is

cor.test(data_agg$totalTestResults, data_agg$death)
# 95 percent confidence interval:
#   0.7785091 0.9194056
# sample estimates:
#   cor 
# 0.8651263
# -> States with more deaths have more tests

cor.test(data_agg$score, data_agg$death)
# 95 percent confidence interval:
#   -0.2259889  0.3185489
# sample estimates:
#   cor 
# 0.04999469 
# -> States with better testing scores do not really have more deaths

cor.test(data_agg$score, data_agg$totalTestResults)
# 95 percent confidence interval:
#   -0.2928155  0.2527485
# sample estimates:
#   cor 
# -0.0216448 
# -> States with better testing scores do not necessarily have more tests

colnames(data_daily)

# Format date column to use R Date type
data_daily$dateformatted <- as.Date(as.character(data_daily$date), format = "%Y%m%d")

# ================================================================================ #
# Taking a deeper dive into NYC data
# ================================================================================ #

ny_data_daily <- data_daily[data_daily$state == 'NY', ]
ny_data_daily[order(ny_data_daily$date, decreasing=TRUE),
              c('date', 'totalTestResults', 'totalTestResultsIncrease', 'death', 'deathIncrease')]

ggplot(data=ny_data_daily) +
  geom_point(aes(x=dateformatted, y=deathIncrease), colour=RED) +
  geom_line(aes(x=dateformatted, y=rollmean(deathIncrease, 3, na.pad=TRUE)), colour=RED, alpha=0.25, size=1) +
  geom_line(aes(x=dateformatted, y=rollmean(deathIncrease, 14, na.pad=TRUE)), colour=RED, alpha=0.5, size=1) +
  
  ggtitle('New Deaths per Day in New York State', subtitle = 'With 3 and 14 day rolling average') +
  ylab('Number of New Deaths') +
  xlab('Date') +
  theme_minimal()
# -> We see a flattening curve for NY!

ggplot(data=ny_data_daily) +
  geom_point(aes(x=dateformatted, y=totalTestResultsIncrease), colour=GREEN) +
  geom_line(aes(x=dateformatted, y=rollmean(totalTestResultsIncrease, 3, na.pad=TRUE)), colour=GREEN, alpha=0.25, size=1) +
  geom_line(aes(x=dateformatted, y=rollmean(totalTestResultsIncrease, 14, na.pad=TRUE)), colour=GREEN, alpha=0.5, size=1) +
  
  ggtitle('New Tests per Day in New York State', subtitle = 'With 3 and 14 day rolling average') +
  ylab('Number of New Tests') +
  xlab('Date') +
  theme_minimal()
# -> But we might be seeing a peak in test capacity (reaching some new bottleneck)

ny_data_daily$pending
# Number of pending tests are not well reported -> unclear what the bottleneck is

# ================================================================================ #
# Considering rates of change in testing and deaths across states
# ================================================================================ #

grouped <- data_daily[, c('state', 'deathIncrease', 'totalTestResultsIncrease')] %>% group_by(state)
grouped$deathIncrease

rollmean(grouped$deathIncrease, 14)

data_daily <- data_daily[order(data_daily$dateformatted, decreasing=FALSE), ]

deathgrouped <- tapply(data_daily$deathIncrease, data_daily$state, function(x) rollmean(x, 3))
testgrouped <- tapply(data_daily$totalTestResultsIncrease, data_daily$state, function(x) rollmean(x, 3))
deathgrouped$NY
testgrouped$NY

rate_of_change <- function (data) {
  data[is.na(data)] <- 0
  lagged_data <- head((append(0, data)), -1)
  lagged_data[is.na(lagged_data)] <- 0
  diff <- data - lagged_data
  return(diff)
}

get_second_derivative_curves <- function(state) {
  # More analysis considering the 2nd derivative
  # This is a little noisey but can be telling
  dates <- unique(data_daily$dateformatted)
  death_increases <- deathgrouped[state][[1]]
  test_increases <- testgrouped[state][[1]]
  # return(death_increases)
  death_rate_of_change <- rate_of_change(death_increases)
  test_rate_of_change <- rate_of_change(test_increases)
  rate_of_change_dates <- tail(dates, length(death_rate_of_change))
  
  plot1 <- ggplot() +
    geom_point(aes(x=rate_of_change_dates, y=death_rate_of_change), colour=RED, alpha=1, size=1) +
    geom_smooth(aes(x=rate_of_change_dates, y=death_rate_of_change), colour=RED, alpha=0.1, size=1, method = "loess") +
    
    ggtitle(paste(state, "Change in Increases in Deaths", sep=" ")) +
    ylab('Change in Increase') +
    xlab('Date') +
    theme_minimal()
  
  plot2 <- ggplot() +
    geom_point(aes(x=rate_of_change_dates, y=test_rate_of_change), colour=GREEN, alpha=1, size=1) +
    geom_smooth(aes(x=rate_of_change_dates, y=test_rate_of_change), colour=GREEN, alpha=0.1, size=1, method = "loess") +
    
    ggtitle(paste(state, "Change in Increases in Tests", sep=" ")) +
    ylab('Change in Increase') +
    xlab('Date') +
    theme_minimal()
  
  return(list(plot1, plot2))
}

plot_second_derivatives <- function (state) {
  plots <- get_second_derivative_curves(state)
  grid.arrange(plots[[1]], plots[[2]], ncol=2)
}

plot_second_derivatives('NY')
plot_second_derivatives('CA')
plot_second_derivatives('WA')

#
#
#

get_first_derivative_curves <- function (state) {
  state_data_daily <- data_daily[data_daily$state == state, ]
  state_data_daily[order(state_data_daily$date, decreasing=TRUE),
                   c('date', 'totalTestResults', 'totalTestResultsIncrease', 'death', 'deathIncrease')]
  
  plot1 <- ggplot(data=state_data_daily) +
    geom_point(aes(x=dateformatted, y=deathIncrease), colour=RED) +
    geom_line(aes(x=dateformatted, y=rollmean(deathIncrease, 3, na.pad=TRUE)), colour=RED, alpha=0.25, size=1) +
    geom_line(aes(x=dateformatted, y=rollmean(deathIncrease, 14, na.pad=TRUE)), colour=RED, alpha=0.5, size=1) +
    
    ggtitle(paste(state, 'New Deaths per Day', sep=' '), subtitle = 'With 3 and 14 day rolling average') +
    ylab('Number of New Deaths') +
    xlab('Date') +
    theme_minimal()
  
  plot2 <- ggplot(data=state_data_daily) +
    geom_point(aes(x=dateformatted, y=totalTestResultsIncrease), colour=GREEN) +
    geom_line(aes(x=dateformatted, y=rollmean(totalTestResultsIncrease, 3, na.pad=TRUE)), colour=GREEN, alpha=0.25, size=1) +
    geom_line(aes(x=dateformatted, y=rollmean(totalTestResultsIncrease, 14, na.pad=TRUE)), colour=GREEN, alpha=0.5, size=1) +
    
    ggtitle(paste(state, 'New Tests per Day', sep=' '), subtitle = 'With 3 and 14 day rolling average') +
    ylab('Number of New Tests') +
    xlab('Date') +
    theme_minimal()

  return(list(plot1, plot2))
}

plot_first_derivatives <- function (state) {
  plots <- get_first_derivative_curves(state)
  grid.arrange(plots[[1]], plots[[2]], ncol=2)
}

plot_first_derivatives('NY')
plot_first_derivatives('CA')
plot_first_derivatives('WA')

get_curves <- function (state) {
  state_data_daily <- data_daily[data_daily$state == state, ]
  state_data_daily[order(state_data_daily$date, decreasing=TRUE),
                   c('date', 'totalTestResults', 'death')]
  
  plot1 <- ggplot(data=state_data_daily) +
    geom_point(aes(x=dateformatted, y=death), colour=RED) +
    stat_smooth(aes(x=dateformatted, y=death), colour=RED, alpha=0.5, size=1, geom = "line", se=FALSE) +
    
    ggtitle(paste(state, 'Total Deaths', sep=' ')) +
    ylab('Number of Deaths') +
    xlab('Date') +
    theme_minimal()
  
  plot2 <- ggplot(data=state_data_daily) +
    geom_point(aes(x=dateformatted, y=totalTestResults), colour=GREEN) +
    stat_smooth(aes(x=dateformatted, y=totalTestResults), colour=GREEN, alpha=0.5, size=1, geom = "line", se=FALSE) +
    
    ggtitle(paste(state, 'Total Tests', sep=' ')) +
    ylab('Number of Tests') +
    xlab('Date') +
    theme_minimal()
  
  return(list(print(plot1), print(plot2)))
}

plot_curves <- function (state) {
  plots <- get_curves(state)
  grid.arrange(plots[[1]], plots[[2]], ncol=2)
}

plot_curves('NY')
plot_curves('CA')
plot_curves('WA')

plot_all_curves <- function (state) {
  plots <- get_curves(state)
  fd_plots <- get_first_derivative_curves(state)
  sd_plots <- get_second_derivative_curves(state)
  print(plots)
  grid.arrange(
    plots[[1]], plots[[2]], fd_plots[[1]], fd_plots[[2]], sd_plots[[1]], sd_plots[[2]], ncol=2)
}

# Honing in on states with the most buzz of late
plot_all_curves('NY')
plot_all_curves('CA')
plot_all_curves('WA')
plot_all_curves('FL')
plot_all_curves('MI')
plot_all_curves('NJ')
plot_all_curves('CT')
plot_all_curves('TX')

# TODO plot test count today vs population with label for each state

states <- tail(colnames(data_census), -1)
# "NJ" "MI" "FL" "WA" "NY" "CA" "TX" "CT"

populations <- data_census[data_census$Fact == "Population estimates, July 1, 2019,  (V2019)", states]

# populations <- t(populations)
# colnames(populations) <- c('population')
# populations

# populations <- data.frame(populations)
# typeof(populations)

data_agg_states <- data_agg[data_agg$state %in% states, ]
data_agg_states$population <- NA
i <- 0
for (state in states) {
  i <- i + 1
  print(state)
  data_agg_states[data_agg_states$state == state, c('population')] <- populations[i]
}
data_agg_states$population <- gsub(",", "", data_agg_states$population)
data_agg_states$population <- as.numeric(as.character(data_agg_states$population))
data_agg_states$population

typeof(data_agg_states$death)
typeof(data_agg_states$population)

ggplot(data=data_agg_states, aes(x=population, y=death)) +
  geom_point(colour=RED) +
  geom_text(aes(label=state),hjust=-0.5, vjust=0.5, colour=RED) +
  ggtitle('Deaths vs. Population') +
  ylab('Number of Deaths') +
  xlab('Population') +
  theme_minimal()

ggplot(data=data_agg_states, aes(x=population, y=totalTestResults)) +
  geom_point(colour=GREEN) +
  geom_text(aes(label=state),hjust=-0.5, vjust=0.5, colour=GREEN) +
  ggtitle('Tests vs. Population') +
  ylab('Number of Tests') +
  xlab('Population') +
  theme_minimal()
