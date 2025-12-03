library(readr)
fars <- read_csv("fars_clean.csv", show_col_types = FALSE)

# columns  to numeric
persons <- as.numeric(fars$persons)
fatals  <- as.numeric(fars$fatals)
hour    <- as.numeric(fars$hour)
weather <- fars$weather

# Removing any rows withmissing key values.
keep_complete <- !is.na(persons) & !is.na(fatals) & !is.na(hour)
persons <- persons[keep_complete]
fatals  <- fatals[keep_complete]
hour    <- hour[keep_complete]
weather <- weather[keep_complete]

start_n <- nrow(fars)
missing_removed <- start_n - length(persons)

# 1.5*IQR rule for upper outliers
fatals_iqr <- IQR(fatals)
fatals_q3  <- quantile(fatals, 0.75)
fatals_upper <- fatals_q3 + 1.5 * fatals_iqr

persons_iqr <- IQR(persons)
persons_q3  <- quantile(persons, 0.75)
persons_upper <- persons_q3 + 1.5 * persons_iqr

# keping new IQR range for rows.
keep_range <- (fatals <= fatals_upper) & (persons <= persons_upper)
clean_persons <- persons[keep_range]
clean_fatals  <- fatals[keep_range]
clean_hour    <- hour[keep_range]
clean_weather <- weather[keep_range]

outliers_removed <- length(persons) - length(clean_persons)

# Put cleaned columns into a data frame .
clean_data <- data.frame(
  weather = clean_weather,
  hour    = clean_hour,
  persons = clean_persons,
  fatals  = clean_fatals
)


