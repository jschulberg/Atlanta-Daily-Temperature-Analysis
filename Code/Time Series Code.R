#######################################################################
#######################################################################

#####             Atlanta Daily Temeperature Analysis             #####

####################################################################### 
#######################################################################
# In this script, I will analyze the Atlanta daily temperature dataset 
# included in the data folder of this repository. I will use a variety 
# of time series change detection techniques to do so, including, but 
# not limited to:

#   - Cumulative Sum (CuSum)  
#   - Holts-Winters
#   - ARIMA    


#######################################################################
# Set Up --------------------------------------------------------------
#######################################################################
# Bring in packages
suppressMessages(library("dplyr")) # Used for data cleaning
suppressMessages(library("tidyr")) # Used for data cleaning
suppressMessages(library("ggplot2")) # Used for visualizations
suppressMessages(library("stringr")) # Used for string manipulations
suppressMessages(library("reshape2")) # Used for reshaping data

# bring in the data, delimited by a tab ("\t")
data_temp <- read.delim(here::here("Data/temps.txt"), header = T, sep = "\t")

# convert to a tibble
data_temp <- as_tibble(data_temp)
head(data_temp)


########################################################################
# Data Prep ------------------------------------------------------------
######################################################################## 
# The first thing we'll note is how strange the data set-up is, with
# each column being a different year. This *screams* for a pivoted 
# approach where the data is in a tidier format so it's easier to
# analyze. We'll do this first and then see what we're working with.

# Let's first convert our first column a bit so we can use it as a date for different graphs
data_temp_fixed <- data_temp

# separate the first column into day and month
data_temp_fixed <- data_temp_fixed %>% 
  separate(1, into = c("day", "month"), sep = "-")

# create a full date out of the year from the header, and the day/month we created above
data_temp_fixed$date <- as.Date(paste(substr(names(data_temp_fixed)[3], 2, 5), 
                                      match(data_temp_fixed$month, month.abb), 
                                      data_temp_fixed$day, sep = "-"))

# let's get rid of the unnecessary day and month columns
data_temp_fixed <- data_temp_fixed %>%
  select(-1, -2)

# fix up our column headers to be full years
names(data_temp_fixed)[1:20] <- substr(names(data_temp_fixed)[1:20], 2, 5)

# Recreate our data frame by melting together all the years into one column
molten <- melt(data_temp_fixed, id = "date")


# Save our visualization to the correct working directory
setwd("C:/Users/jschulberg/Documents/Data Analytics Blog/Blog 5 - Atlanta Daily Temperature/Atlanta-Daily-Temperature-Analysis/Viz/")
jpeg(file = "Atlanta Temperatures 2nd Half of the Year.jpeg") # Name of the file for the viz we'll save

ggplot(molten, aes(x = date, y = value)) +
  # Use the facet wrap function to create a separate graph, one for each year
  facet_wrap(~ variable, nrow = 4) +
  geom_point(color = "slateblue", lwd = .75) +
  theme_classic() +
  # Let's change the names of the axes and title
  # for the title, let's pull the header of the column and only look at the last four digits
  labs(title = "Temperatures in the Second Half of the Year",
       subtitle = "*Temperature measured in Farenheit") +
  ylab("Temperature*") +
  xlab("") +
  # Center the title and format the subtitle/caption
  theme(plot.title = element_text(hjust = 0, color = "black"),
        plot.subtitle = element_text(color = "dark gray", size = 10))

dev.off()

########################################################################
# CUSUM ----------------------------------------------------------------
######################################################################## 
# In this section, I'll use a CUSUM approach to identify when unofficial 
# summer ends (i.e., when the weather starts cooling off) each year. 

# To figure out when the weather starts getting colder, we'll
# compute the cumulative sum. First let's calculate the means across each row.
# This will signify the mean temperature for each year.
(means <- apply(data_temp_fixed[,1:20], 2, mean))


# We'll initiailize an empty data frame that we can fill with values for 3 columns:
#   1. Cumulative Sum
#   2. Day of the year
#   3. Year
cumsum_df <- as.data.frame(matrix(ncol=3, nrow=0))
cumsum_vector <- rep(0, nrow(data_temp_fixed))
cumsum <- 0L


# On the inner for loop, we'll calculate the cumulative sum for a specific year
for (i in 1:nrow(data_temp_fixed)) {
  # As the first part of this formula, we'll subtract each row i in the year column 1
  # from the mean temperature for that year, 1
  val <- data_temp_fixed[i, 1] - means[1]
  
  # if our value plus the previous cumsum is greater than 0, we'll retain it. Otherwise, we'll impute the value 0
  cumsum <- ifelse(cumsum + val > 0, cumsum + val, 0)
  
  # store the cumulative sum into a vector we can use later
  cumsum_vector[[i]] <- as.numeric(cumsum)
  
}

cumsum_df <- data.frame(cumsum = c(cumsum_vector),
                        date = c(data_temp_fixed$date),
                        year = c(substr(data_temp_fixed$date, 1, 4)))

# reset our cumulative sum back to 0
cumsum <- 0L

for (j in 2:ncol(data_temp_fixed)-1) {
  
  cumsum_vector <- rep(0, nrow(data_temp_fixed))
  
  # We'll set a variable called cumsum to 0. Each time our loop runs on a new column,
  # it'll reset the cumsum variable to 0.
  cumsum <- 0L
  
  # We'll also initialize an empty vector each time that will hold the cumulative sum values for each column
  # cumsum_vector <- rep(0, nrow(data_temp_fixed))
  
  # On the inner for loop, we'll calculate the cumulative sum for a specific year
  for (i in 1:nrow(data_temp_fixed)) {
    # As the first part of this formula, we'll subtract each row i in the year column j
    # from the mean temperature for that year, j
    val <- data_temp_fixed[i,j] - means[j]
    
    # if our value plus the previous cumsum is greater than 0, we'll retain it. Otherwise, we'll impute the value 0
    cumsum <- ifelse(cumsum + val > 0, cumsum + val, 0)
    
    # store the cumulative sum into a vector we can use later
    cumsum_vector[[i]] <- as.numeric(cumsum)
    
  }
  
  # Bind our results for column j into our master data frame with all the
  # cumulative sum values
  cumsum_df <- cumsum_df %>%
    rbind(data.frame(cumsum = c(cumsum_vector),
                     date = c(data_temp_fixed$date),
                     year = c(names(data_temp_fixed)[j])))
  
}


jpeg(file = "Atlanta Temperatures Cumulative Sums YoY.jpeg") # Name of the file for the viz we'll save

# Let's now visualize what we have
ggplot(cumsum_df, aes(x = date, y = cumsum)) +
  geom_point(color = "slateblue", lwd = .5) +
  # Use the facet wrap function to create a separate graph, one for each year
  facet_wrap(~ year, nrow = 4) +
  theme_classic() +
  # Let's change the names of the axes and title
  # for the title, let's pull the header of the column and only look at the last four digits
  labs(title = "Cumulative Temperature",
       subtitle = "This viz shows the cumulative sum in the summer months by year.") +
  ylab("Cumulative Sum") +
  xlab("") +
  # Center the title and format the subtitle/caption
  theme(plot.title = element_text(hjust = 0, color = "black"),
        plot.subtitle = element_text(color = "dark gray", size = 10))

dev.off()

# Now that we have a visual sense of what the cumulative sum temperatures look like, let's
# spread out our data so we can figure out when the cumulative sum starts to tail off. We'll
# use the "maximum cumsum" as our sense of when the weather starts to noticeably decrease,
# thus marking the end of summer.
max_cumsum <- cumsum_df %>%
  # group by year so we can look at one year at a time
  group_by(year) %>%
  # calculate the maximum cumulative sum per year using the summarise function
  summarise(cumsum = max(cumsum)) %>%
  # we still need the date associated with each one, so bring this back in
  left_join(cumsum_df, by = "cumsum") %>%
  # fix up our date column
  mutate(full_date = paste(year.x, substr(date, 6, 10), sep = "-"))  %>%
  # since we have two year columns, get rid of one
  select(-year.y) %>%
  # get rid of duplicate rows
  distinct() %>%
  # rename our year column
  rename("year" = "year.x")

# print the dates at which summer ended for each year
end_of_summer_dates <- max_cumsum %>%
  select(-cumsum, -date) %>%
  print()

# Generally, summer ends around mid-September, based on the values outputted above.

jpeg(file = "Atlanta Temperatures Maximum Cumulative Sums YoY.jpeg") # Name of the file for the viz we'll save

# Let's see if we can add each max date as a red point on our previous visualization
ggplot(cumsum_df, aes(x = date, y = cumsum)) +
  # our base layer will be all of the cumulative sums over time, similar to before. We'll
  # make the color of each point light gray so as to make it look like a background
  geom_point(color = "light gray", size = .5) +
  # our "top" layer will be the maximum cumulative sums from our max_cumsum data frame
  # we'll make the size and color of these points more distinct so they stand out
  geom_point(data = max_cumsum, aes(x = date, y = cumsum), size = 3, color = "slateblue4") +
  # Use the facet wrap function to create a separate graph, one for each year. Since both
  # layers of our viz have year as a variable, facet wrap will work on both layers
  facet_wrap(~ year, nrow = 4) +
  # change the theme so it's more simple
  theme_classic() +
  # Let's change the names of the axes and title
  # for the title, let's pull the header of the column and only look at the last four digits
  labs(title = "Cumulative Temperature",
       subtitle = "This viz shows the cumulative sum in the summer months by year. The\npoint at which unofficial summer is ending is highlighted in slate blue.",
       caption =  "Viz inspired by https://drsimonj.svbtle.com/plotting-background-data-for-groups-with-ggplot2") +
  ylab("Cumulative Sum") +
  xlab("") +
  # Center the title and format the subtitle/caption
  theme(plot.title = element_text(hjust = 0, color = "black"),
        plot.subtitle = element_text(color = "dark gray", size = 10),
        plot.caption = element_text(color = "dark gray", hjust = 1, face = "italic"))

dev.off()

# Based on the above analysis, it does not seem that the climate has gotten warmer
# over time.



########################################################################
# Exponential Smoothing ------------------------------------------------
######################################################################## 
# Next, I will build and use an exponential smoothing model to help make a judgment of whether
# the unofficial end of summer has gotten later over the 20 years.

# Bring in packages
suppressMessages(library("gridExtra"))
suppressMessages(library("grid"))
suppressMessages(library("ggfortify"))


# Ensure randomness/reproducibility
set.seed(123)

# Create a time series object from the temperature values. We'll make the frequency of
# our data be 123 days, which is the number of rows in our original data frame. We'll also
# start our time series object from the first year (column) in our data frame.
ts_vector <- ts(data = molten$value, # what are the actual data values?
                frequency = nrow(data_temp_fixed), # how often is our data re-occuring?
                start = as.integer(names(data_temp_fixed)[1])) # when does it start?
summary(ts_vector)

jpeg(file = "Atlanta Temperatures Over Time.jpeg") # Name of the file for the viz we'll save

# A quick plot of our time series data shows, as we expected, seasonality in the temperature data.
autoplot(ts_vector, # our time series object
         ts.colour = "slateblue4", # color of the line,
         main = "Atlanta Temperature over Time", # chart title
         ylab = "Temperature (degrees F)" # name of y-axis
         )
dev.off()

# Use Holt Winters to smooth the data. Set beta to false so we use exponential smoothing.
# Gamma is set to true so we include a seasonal component.
hw1 <- HoltWinters(ts_vector, beta = F, gamma = T) 
summary(hw1)

# Let's check out the fitted values
# A multiple time series with one column for the filtered series as well as for 
# the level, trend and seasonal components, estimated contemporaneously (that is 
# at time t and not at the end of the series).
head(hw1$fitted)
head(hw1$coefficients)

# Re-create our original plot, with the Holts-Winters estimated smoothing.
plot_hw1 <- autoplot(hw1)
autoplot(hw1, # our time series object
         ts.colour = "slateblue4", # color of the line,
         main = "Exponential Smoothing of Atlanta Temperature over Time", # chart title
         ylab = "Temperature (degrees F)" # name of y-axis
)
# We can see the effects of the smoothing on the graph, namely in red


# Create a matrix of all the seasonal factors for each data point.
sf_matrix <- matrix(hw1$fitted[, 3], nrow = 123)
sf_data <- as_tibble(sf_matrix)

colnames(sf_data) <- colnames(data_temp_fixed[, 2:20])
head(sf_data)


# Create a matrix of all the estimated values (xhat) for each data point.
xhat_matrix <- matrix(hw1$fitted[,1], nrow = 123)
xhat_data <- as_tibble(xhat_matrix)

# Change the column names
colnames(xhat_data) <- colnames(data_temp_fixed[, 2:20])
# Bring our dates back in
xhat_data <- cbind(xhat_data, date = data_temp_fixed$date)
head(xhat_data)

# Let's melt our xhat data so we can graph it with our normal temperature data
xhat_molten <- melt(xhat_data, id = "date")
head(xhat_molten)
# rename the column name for xhat
colnames(xhat_molten)[3] <- "xhat_value"

# Before we bring this data into our original melted data, we need to impute our values for 1996.
data_1996 <- matrix(nrow = 123, ncol = 3)
data_1996 <- as_tibble(data_1996)

data_1996[, 1] <- as.Date(data_temp_fixed$date)
colnames(data_1996)[1] <- colnames(xhat_molten)[1]

data_1996[, 2] <- as.factor(1996)
colnames(data_1996)[2] <- colnames(xhat_molten)[2]

data_1996[, 3] <- data_temp_fixed$`1996`
colnames(data_1996)[3] <- colnames(xhat_molten)[3]


# Now let's bring this back into our original melted data frame so we can plot everything.
combined_data <- xhat_molten %>%
  rbind(data_1996) %>%
  cbind(value = molten$value)

jpeg(file = "Exponential Smoothing of Atlanta Temperatures (Holts-Winters).jpeg") # Name of the file for the viz we'll save

# Let's look at the predicted and normal temperatures year-by-year.
ggplot() +
  # bring in our normal temperatures as points
  geom_point(data = combined_data, 
             aes(x = date, y = value), 
             color = "gray", 
             lwd = .75) +
  # bring in our xhat values as a line with slight transparency so we can still see the points
  geom_line(data = combined_data, 
            aes(x = date, y = xhat_value), 
            color = "slateblue", 
            lwd = .75,
            alpha = .5) +  
  # Use the facet wrap function to create a separate graph, one for each year
  facet_wrap(~ variable, nrow = 4) +
  theme_classic() +
  # Let's change the names of the axes and title
  # for the title, let's pull the header of the column and only look at the last four digits
  labs(title = "Temperatures in the Second Half of the Year",
       subtitle = "Gray dots represent our original data.\nSlate blue lines represent exponential smoothing.") +
  ylab("Temperature (degrees F") +
  xlab("") +
  # Center the title and format the subtitle/caption
  theme(plot.title = element_text(hjust = 0, color = "black"),
        plot.subtitle = element_text(color = "dark gray", size = 10))

dev.off()
# In general, we do not see a significant uptick in temperatures through the years,
# even as predicted by Holts-Winter.Thus, we can not definitely say that the "end of
# summer" has gone later in the season as time has progressed.
