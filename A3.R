library(dplyr)
library(corrplot)
library(ggplot2)
library(lubridate)
library(zoo)
library(imputeTS)
library("depmixS4")
set.seed(1)

### Functions ###
# add a specified number of seconds to a POSIXlt time object
addTimeToPOSIXlt <- function(time, numSecs) {
  return(as.POSIXlt(as.POSIXct(time)+numSecs))
}

# Convert POSIXlt to POSIXct
POSIXlt_to_ct <- function(datetime, format_str) {
  return(as.POSIXct(strftime(datetime, format = format_str), format = format_str, tz = timezone))
}


# Calculate the rolling mean for the 'Global_intensity' column with a defined window size
rollMeanGlobInt <- function(df, window) {
  rollmean(df[['Global_intensity']], window)
}


timezone <- "UTC"
window_size <- 10 # moving time window

# Loading data 
data <- read.table("Group_Assignment_Dataset.txt", header = TRUE, sep = ",")

# see how many na values for each feature
colSums(is.na(data))

# go through all the data, if it is numeric then interpolate the na values (From Assignment1)
data <- as.data.frame(lapply(data, function(col) {
  if (is.numeric(col)) {
    return(na.approx(col, na.rm = FALSE)) # Apply interpolation
  } 
  else {
    return(col) # Leave non-numeric columns as is
  }
}))

# check that there are no na values left
colSums(is.na(data))

columns <- c("Global_active_power", "Global_reactive_power", "Voltage", 
             "Global_intensity", "Sub_metering_1", "Sub_metering_2", "Sub_metering_3")

# Create a new dataframe to store z-scores
z_scores <- data.frame(matrix(nrow = nrow(data), ncol = length(columns)))
colnames(z_scores) <- columns

# Calculate z-scores for the selected columns
for (col in columns) {
  z_scores[[col]] <- (data[[col]] - mean(data[[col]], na.rm = TRUE)) / sd(data[[col]], na.rm = TRUE)
}

# Create a matrix to identify anomalies where z-scores are greater than 3 (or less than -3)
anomaly_matrix <- abs(z_scores) > 3

# Remove rows that have at least one anomaly
data_cleaned <- data[!apply(anomaly_matrix, 1, any), ]

# Creates a new Datetime column
data_cleaned$Datetime <- as.POSIXlt(paste(data_cleaned$Date, data_cleaned$Time), format = "%d/%m/%Y %H:%M:%S", tz = timezone)

# Converting Date column to actual Date data
data_cleaned$Date <- as.Date(data_cleaned$Date, format = "%d/%m/%Y")


# columns for the weekday and week number
data_cleaned$Weekday <- wday(data_cleaned$Date, label = TRUE, abbr = TRUE)
data_cleaned$Week <- isoweek(data_cleaned$Date) # ISO week number

# Filter between 5 AM and 10:59 AM
tuesday_morning_data <- filter(data_cleaned, 
                              Weekday == "Tue" & 
                                format(Datetime, "%H:%M:%S") >= "05:00:00" & 
                                format(Datetime, "%H:%M:%S") <= "10:59:00")

weekly_counts <- group_by(tuesday_morning_data, Week)
weekly_counts <- summarise(weekly_counts, Data_Points = n())

# Vector to be used for ntimes in the model
count_vector <- weekly_counts$Data_Points

# states from 4-16
state_range <- 4:16

# vectors to store log-likelihood and BIC for each model
log_likelihoods <- numeric(length(state_range))
BIC_values <- numeric(length(state_range))

# Loop for each nstate value and store BIC and Log-likelihood
for (i in state_range) {
  model <- depmix(response = Global_active_power ~ 1, 
                  data = tuesday_morning_data, 
                  nstates = i, 
                  ntimes = count_vector)
  
  fitModel <- fit(model, verbose = FALSE)
  log_likelihoods[i - 3] <- logLik(fitModel)  # Store log-likelihood
  BIC_values[i - 3] <- BIC(fitModel)  # Store BIC
}

# Create a data frame for plotting
results_df <- data.frame(
  States = state_range,
  LogLikelihood = log_likelihoods,
  BIC = BIC_values
)

# Plot log-likelihood for each model
ggplot(results_df, aes(x = States, y = LogLikelihood)) +
  geom_line(color = "blue") +
  geom_point(color = "blue") +
  labs(title = "Log-Likelihood for Different Model States", 
       x = "Number of States", 
       y = "Log-Likelihood") +
  theme_minimal()

# Plot BIC for each model
ggplot(results_df, aes(x = States, y = BIC)) +
  geom_line(color = "red") +
  geom_point(color = "red") +
  labs(title = "BIC for Different Model States", 
       x = "Number of States", 
       y = "BIC") +
  theme_minimal()


