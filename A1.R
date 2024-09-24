
# installed zoo to have access to the approx function to do linear interpolation
install.packages("zoo")
library(zoo)

install.packages("dplyr")
library(dplyr)


# install corrplot packages 
install.packages("corrplot")

install.packages("ggplot2")
library(ggplot2)


# Open the data as a dataframe, keeping the first row as header
DataDf <- read.table("Group_Assignment_Dataset.txt", header = T, sep = ",")

# see how many na values for each feature
colSums(is.na(DataDf))

# go through all the data, if it is numeric then interpolate the na values
DataDf <- as.data.frame(lapply(DataDf, function(col) {
  if (is.numeric(col)) {
    return(na.approx(col, na.rm = FALSE)) # Apply interpolation
  } 
  else {
    return(col) # Leave non-numeric columns as is
  }
}))

# check that there are no na values left
colSums(is.na(DataDf))

# create new dataframe that will store the z-scores of the features
columns = c("Global_active_power","Global_reactive_power","Voltage","Global_intensity","Sub_metering_1", "Sub_metering_2", "Sub_metering_3" ) 
z_scores = data.frame(matrix(nrow = 525600, ncol = length(columns))) 
colnames(z_scores) = columns

# calculate the z-scores
for (col in columns){
  z_scores[[col]] <- (DataDf[[col]] - mean(DataDf[[col]])) / sd(DataDf[[col]])
}


# create vector to store the anomalies
anomalies <- numeric(length(columns))
names(anomalies) <- columns

# calculate the percentage of anomalies in the data
for (col in columns){
  anomalies[col] <- ((sum(abs(z_scores[[col]]) > 3, na.rm = TRUE)) / nrow(z_scores)) * 100
}
print(anomalies)

anomaly_matrix <- abs(z_scores) > 3

# Count the number of rows with at least one anomaly
total_anomalies <- sum(rowSums(anomaly_matrix, na.rm = TRUE) > 0)

print(paste("percentage of data with anonamlies", 100*(total_anomalies/nrow(z_scores))))

########## WEEK SELECTION ############

# Change the data to POSIXlt form
DataDf <- DataDf %>%
  mutate(Date = as.POSIXlt(Date, format = "%d/%m/%Y"))

# Set the start date correctly
start_date <- as.POSIXlt("2007-01-01") + 17 * 7 * 24 * 60 * 60  
end_date <- start_date + 7 * 24 * 60 * 60


# Filter the data for the specified week
filtered_data <- DataDf %>%
  filter(Date >= start_date & Date <= end_date)



library(corrplot)

columns <- c("Global_active_power", "Global_reactive_power", "Voltage", "Global_intensity", 
             "Sub_metering_1", "Sub_metering_2", "Sub_metering_3")

#loop to calculate correlations
for (i in 1:(length(columns)-1)) {
  for (j in (i):length(columns)) {
    var1 <- filtered_data[[columns[i]]]
    var2 <- filtered_data[[columns[j]]]
    
    correlation <- cor(var1, var2, method = "pearson")
    
    print(sprintf("Correlation between %s and %s: %0.2f", columns[i], columns[j], correlation))
  }
}


# Create an empty correlation matrix
correlation_matrix <- matrix(NA, nrow = length(columns), ncol = length(columns))

# Set row and column names for the correlation matrix
rownames(correlation_matrix) <- columns
colnames(correlation_matrix) <- columns

# Loop to calculate correlations
for (i in 1:(length(columns))) {
  for (j in (i):length(columns)) {
    var1 <- filtered_data[[columns[i]]]
    var2 <- filtered_data[[columns[j]]]
    
    correlation <- cor(var1, var2, method = "pearson")
    
    # Store the correlation in the matrix
    correlation_matrix[i, j] <- correlation
    correlation_matrix[j, i] <- correlation  # To ensure the matrix is symmetric
    
    print(sprintf("Correlation between %s and %s: %0.2f", columns[i], columns[j], correlation))
  }
}

# View the correlation matrix
print(correlation_matrix)


#http://www.sthda.com/english/wiki/visualize-correlation-matrix-using-correlogram
#used "color" and "upper" styles to color code and display upper triangular of correlation matrix 
corrplot(correlation_matrix, method = "color", type = "upper", 
         tl.col = "black", tl.srt = 45, tl.cex = 0.6, addCoef.col = "black", 
         number.cex = 0.6, col = colorRampPalette(c("blue", "white", "red"))(200))
####### Task 3 #######

# Create a vector to store combined date and time from filtered_data
date_time <- as.POSIXct(paste(filtered_data$Date, filtered_data$Time))

# Create a Daytime window 
daytime <- filtered_data %>%
  filter(format(date_time, "%H:%M:%S") >= "07:30:00" & format(date_time, "%H:%M:%S") <= "17:00:00")

# Create a Nighttime window
nighttime <- filtered_data %>%
  filter(format(date_time, "%H:%M:%S") > "17:00:00" |format(date_time, "%H:%M:%S") < "07:30:00")

# Calculate average Global_intensity for the daytime during the week days 
# Each value corresponds to an average Global_intensity for each minute across 5 Week Days
day_weekday_average <- daytime %>%
  filter(weekdays(Date) != "Saturday" & weekdays(Date) != "Sunday") %>%
  group_by(Time) %>%
  summarize(average = mean(Global_intensity, na.rm = TRUE))

# Calculate average Global_intensity for the nightime during the week days 
# Each value corresponds to an average Global_intensity for each time minute across 5 Week Days

night_weekday_average <- nighttime %>%
  filter(weekdays(Date) != "Saturday" & weekdays(Date) != "Sunday") %>%
  group_by(Time) %>%
  summarize(average = mean(Global_intensity, na.rm = TRUE))

# Calculate average Global_intensity for the daytime during the weekend days 
# Each value corresponds to an average Global_intensity for each minute across 5 Week Days
day_weekend_average <- daytime %>%
  filter(weekdays(Date) == "Saturday" | weekdays(Date) == "Sunday") %>%
  group_by(Time) %>%
  summarize(average = mean(Global_intensity, na.rm = TRUE))

# Calculate average Global_intensity for the nighttime during the weekend days 
# Each value corresponds to an average Global_intensity for each minute across 5 Week Days
night_weekend_average <- nighttime %>%
  filter(weekdays(Date) == "Saturday" | weekdays(Date) == "Sunday") %>%
  group_by(Time) %>%
  summarize(average = mean(Global_intensity, na.rm = TRUE))





# Task 3
# Load the dataset
data <- read.csv("Group_Assignment_Dataset.txt", sep = ",", header = TRUE)

# Combine the Date and Time columns to create a proper datetime column
data <- data %>%
  mutate(Datetime = as.POSIXlt(paste(Date, Time), format = "%d/%m/%Y %H:%M:%S"))

# Extract the day of the week
data <- data %>%
  mutate(Weekday = weekdays(Datetime),
         is_weekend = ifelse(Weekday %in% c("Saturday", "Sunday"), "Weekend", "Weekday"))

# Define the daytime (7:30 AM to 5 PM) and nighttime windows
daytime_start <- "07:30:00"
daytime_end <- "17:00:00"

# Filter the dataset for daytime
daytime_data <- data %>%
  filter(format(Datetime, "%H:%M:%S") >= daytime_start & format(Datetime, "%H:%M:%S") <= daytime_end)

# Calculate average Global_intensity for each time point for weekdays and weekends
average_daytime_weekdays <- daytime_data %>%
  filter(is_weekend == "Weekday") %>%
  group_by(Time) %>%
  summarise(avg_intensity_weekday = mean(Global_intensity, na.rm = TRUE))

average_daytime_weekends <- daytime_data %>%
  filter(is_weekend == "Weekend") %>%
  group_by(Time) %>%
  summarise(avg_intensity_weekend = mean(Global_intensity, na.rm = TRUE))

# Combine the data for weekdays and weekends
average_daytime <- merge(average_daytime_weekdays, average_daytime_weekends, by = "Time")

# Convert time to numeric to facilitate regression analysis
average_daytime <- average_daytime %>%
  mutate(Time_numeric = as.numeric(strptime(Time, "%H:%M:%S")))

# Linear regression for weekdays and weekends
fit_linear_weekday <- lm(avg_intensity_weekday ~ Time_numeric, data = average_daytime)
fit_linear_weekend <- lm(avg_intensity_weekend ~ Time_numeric, data = average_daytime)

# Polynomial regression for weekdays and weekends (degree 2)
fit_polynomial_weekday <- lm(avg_intensity_weekday ~ poly(Time_numeric, 2, raw = TRUE), data = average_daytime)
fit_polynomial_weekend <- lm(avg_intensity_weekend ~ poly(Time_numeric, 2, raw = TRUE), data = average_daytime)

# Create plot for linear regression with a legend
plot_linear <- ggplot(average_daytime, aes(x = Time_numeric)) +
  geom_point(aes(y = avg_intensity_weekday, color = "Weekday"), alpha = 0.5) +
  geom_point(aes(y = avg_intensity_weekend, color = "Weekend"), alpha = 0.5) +
  geom_smooth(aes(y = avg_intensity_weekday, color = "Weekday"), method = "lm", se = FALSE) +
  geom_smooth(aes(y = avg_intensity_weekend, color = "Weekend"), method = "lm", se = FALSE) +
  labs(title = "Linear Regression of Global_intensity (Weekday vs Weekend)",
       x = "Time (Numeric)", y = "Average Global_intensity") +
  scale_color_manual(values = c("Weekday" = "blue", "Weekend" = "red")) +
  theme_minimal() +
  theme(legend.title = element_blank())

# Create plot for polynomial regression with a legend
plot_polynomial <- ggplot(average_daytime, aes(x = Time_numeric)) +
  geom_point(aes(y = avg_intensity_weekday, color = "Weekday"), alpha = 0.5) +
  geom_point(aes(y = avg_intensity_weekend, color = "Weekend"), alpha = 0.5) +
  geom_smooth(aes(y = avg_intensity_weekday, color = "Weekday"), method = "lm", formula = y ~ poly(x, 2), linetype = "dashed", se = FALSE) +
  geom_smooth(aes(y = avg_intensity_weekend, color = "Weekend"), method = "lm", formula = y ~ poly(x, 2), linetype = "dashed", se = FALSE) +
  labs(title = "Polynomial Regression of Global_intensity (Weekday vs Weekend)",
       x = "Time (Numeric)", y = "Average Global_intensity") +
  scale_color_manual(values = c("Weekday" = "blue", "Weekend" = "red")) +
  theme_minimal() +
  theme(legend.title = element_blank())

# Print the plots
print(plot_linear)
print(plot_polynomial)
