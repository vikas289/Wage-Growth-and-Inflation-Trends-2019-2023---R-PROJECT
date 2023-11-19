# Load libraries
library(dplyr)
library(readr)
library(ggplot2)
library(lubridate)

# Load the CSV file
my_data_wages <- read_csv("average-wage-canada.csv")

my_data_wages<-t(my_data_wages)

# Set the column names to the values in the first row
colnames(my_data_wages) <- my_data_wages[1,]

# Remove the first row
my_data_wages <- my_data_wages[-1,]


# Change the reference number columns to the length of rows
rownames(my_data_wages) <- 1:nrow(my_data_wages)


wages_canada_new <- my_data_wages[, c(1, 2)]



# Change the column names
colnames(wages_canada_new) <- c("Date", "Wage_Canada")

# convert matrix to data.frame
wages_canada_new <- as.data.frame(wages_canada_new)

#Convert wages date into proper format
wages_canada_new$Date <- as.Date(wages_canada_new$Date)

# Convert for non-numeric values in Wage_Canada
wages_canada_new$Wage_Canada = as.numeric(wages_canada_new$Wage_Canada)


# filter the data to only include dates from March 2019 to March 2023
wages_canada_new <- subset(wages_canada_new, Date >= as.Date("2019-03-01") & Date <= as.Date("2023-03-01"))


#Cleaning the consumer price index data

# Load the CSV file
my_data_inflation <- read_csv("consumer-price-canada.csv")

my_data_inflation<-t(my_data_inflation)

# Set the column names to the values in the first row
colnames(my_data_inflation) <- my_data_inflation[1,]

# Remove the first row
my_data_inflation <- my_data_inflation[-1,]

# Change the reference number columns to the length of rows
rownames(my_data_inflation) <- 1:nrow(my_data_inflation)

inflation_canada_new <- my_data_inflation[, c(1, 2)]

# Change the column names
colnames(inflation_canada_new) <- c("Date", "CPI")

# convert matrix to data.frame
inflation_canada_new <- as.data.frame(inflation_canada_new)

# convert the 'Date' column to a date format
inflation_canada_new$Date <- as.Date(inflation_canada_new$Date)

inflation_canada_new$CPI = as.numeric(inflation_canada_new$CPI)

# filter the data to only include dates from March 2019 to March 2023
inflation_canada_new <- subset(inflation_canada_new, Date >= as.Date("2019-03-01") & Date <= as.Date("2023-03-01"))

# Combine the two data frames using a left join
combined_df <- left_join(wages_canada_new, inflation_canada_new, by = "Date")

combined_df <- na.omit(combined_df)

combined_df <- combined_df %>% 
  mutate(
    AWR_YoY = (Wage_Canada-lag(Wage_Canada, 12))/lag(Wage_Canada,12)*100,
    CPI_YoY = (CPI-lag(CPI, 12))/lag(CPI,12)*100
  )


# create a dataframe that includes only the relevant columns
AWR_vs_CPI <- data.frame(Date = combined_df$Date, AWR_YoY = combined_df$AWR_YoY, CPI_YoY = combined_df$CPI_YoY)



ggplot(AWR_vs_CPI, aes(x = Date)) +
  geom_line(aes(y = AWR_YoY, color = "AWR")) +
  geom_line(aes(y = CPI_YoY, color = "CPI")) +
  labs(title = "Year-over-Year Growth of AWR and CPI (2019-2023)", x = "Date", y = "Growth Rate (%)") +
  scale_color_manual(name = "Variables", values = c("AWR" = "blue", "CPI" = "red"))


#Part b
# Load the CSV file
my_data_wages_ontario <- read_csv("average-wage-ontario.csv")

my_data_wages_ontario<-t(my_data_wages_ontario)

# Set the column names to the values in the first row
colnames(my_data_wages_ontario) <- my_data_wages_ontario[1,]

# Remove the first row
my_data_wages_ontario <- my_data_wages_ontario[-1,]


# Change the reference number columns to the length of rows
rownames(my_data_wages_ontario) <- 1:nrow(my_data_wages_ontario)


wages_ontario_new <- my_data_wages_ontario[, c(1, 2)]



# Change the column names
colnames(wages_ontario_new) <- c("Date", "Wage_ontario")

# convert matrix to data.frame
wages_ontario_new <- as.data.frame(wages_ontario_new)

#Convert wages date into proper format
wages_ontario_new$Date <- as.Date(wages_ontario_new$Date)

# Convert for non-numeric values in Wage_Canada
wages_ontario_new$Wage_ontario = as.numeric(wages_ontario_new$Wage_ontario)


# filter the data to only include dates from March 2019 to March 2023
wages_ontario_new <- subset(wages_ontario_new, Date >= as.Date("2019-03-01") & Date <= as.Date("2023-03-01"))


#Cleaning the consumer price index data

# Load the CSV file
my_data_inflation_ontario <- read_csv("consumer-price-ontario.csv")

my_data_inflation_ontario<-t(my_data_inflation_ontario)

# Set the column names to the values in the first row
colnames(my_data_inflation_ontario) <- my_data_inflation_ontario[1,]

# Remove the first row
my_data_inflation_ontario <- my_data_inflation_ontario[-1,]

# Change the reference number columns to the length of rows
rownames(my_data_inflation_ontario) <- 1:nrow(my_data_inflation_ontario)

inflation_ontario_new <- my_data_inflation_ontario[, c(1, 2)]

# Change the column names
colnames(inflation_ontario_new) <- c("Date", "CPI")

# convert matrix to data.frame
inflation_ontario_new <- as.data.frame(inflation_ontario_new)

# convert the 'Date' column to a date format
inflation_ontario_new$Date <- as.Date(inflation_ontario_new$Date)

inflation_ontario_new$CPI = as.numeric(inflation_ontario_new$CPI)

# filter the data to only include dates from March 2019 to March 2023
inflation_ontario_new <- subset(inflation_ontario_new, Date >= as.Date("2019-03-01") & Date <= as.Date("2023-03-01"))

# Combine the two data frames using a left join
combined_df_ontario <- left_join(wages_ontario_new, inflation_ontario_new, by = "Date")

combined_df_ontario <- na.omit(combined_df_ontario)

combined_df_ontario <- combined_df_ontario %>% 
  mutate(
    AWR_YoY = (Wage_ontario-lag(Wage_ontario, 12))/lag(Wage_ontario,12)*100,
    CPI_YoY = (CPI-lag(CPI, 12))/lag(CPI,12)*100
  )


# create a dataframe that includes only the relevant columns
AWR_vs_CPI_ontario <- data.frame(Date = combined_df_ontario$Date, AWR_YoY = combined_df_ontario$AWR_YoY, CPI_YoY = combined_df_ontario$CPI_YoY)



ggplot(AWR_vs_CPI_ontario, aes(x = Date)) +
  geom_line(aes(y = AWR_YoY, color = "AWR")) +
  geom_line(aes(y = CPI_YoY, color = "CPI")) +
  labs(title = "Year-over-Year Growth of AWR and CPI (2019-2023) Ontario", x = "Date", y = "Growth Rate (%)") +
  scale_color_manual(name = "Variables", values = c("AWR" = "blue", "CPI" = "red"))



#Question 2


# Load the iris dataset
data("iris")

# Create a subset of the data for petal length
petal_data <- iris %>%
  select(Species, Petal.Length)

# Create the plot
petal_plot <- ggplot(petal_data, aes(x = Species, y = Petal.Length)) +
  geom_boxplot(outlier.shape = NA) +
  geom_point(size = 2, color = "black") +
  labs(title = "Petal Length of Iris Species", x = "Species", y = "Petal Length") +
  theme_bw() +
  scale_color_manual(values = c("darkblue", "darkgreen", "purple"))

# Save the plot as a JPEG file
jpeg("petal_plot.jpeg")
print(petal_plot)
dev.off()