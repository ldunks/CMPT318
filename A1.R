
# installed zoo to have access to the approx function to do linear interpolation
install.packages("zoo")
library(zoo)

# installed dplyr to allow filtering of data
install.packages("dplyr")
library(dplyr)

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





