library(dplyr)
library(tidyr)
library(skimr)
library(janitor)
library(lubridate)

str(X2021_Jan)
str(X2021_Jan)
str(X2021_Feb)
str(X2021_Mar)
str(X2021_Apr)
str(X2021_May)
str(X2021_Jun)
str(X2021_Jul)
str(X2021_Aug)
str(X2021_Sep)
str(X2021_Oct)
str(X2021_Nov)
str(X2021_Dec)

compare_df_cols(X2021_Jan,X2021_Feb,X2021_Mar,X2021_Apr,X2021_May,X2021_Jun,X2021_Jul,X2021_Aug,X2021_Sep,X2021_Oct,X2021_Nov,X2021_Dec)


X2021_Jan$started_at = strptime(X2021_Jan$started_at, format = "%d-%m-%Y %H:%M")
X2021_Jan$ended_at = strptime(X2021_Jan$ended_at, format = "%d-%m-%Y %H:%M")

X2021_Feb$started_at = strptime(X2021_Feb$started_at, format = "%d-%m-%Y %H:%M")
X2021_Feb$ended_at = strptime(X2021_Feb$ended_at, format = "%d-%m-%Y %H:%M")

X2021_Mar$started_at = strptime(X2021_Mar$started_at, format = "%d-%m-%Y %H:%M")
X2021_Mar$ended_at = strptime(X2021_Mar$ended_at, format = "%d-%m-%Y %H:%M")

X2021_Apr$started_at = strptime(X2021_Apr$started_at, format = "%d-%m-%Y %H:%M")
X2021_Apr$ended_at = strptime(X2021_Apr$ended_at, format = "%d-%m-%Y %H:%M")

X2021_May$started_at = strptime(X2021_May$started_at, format = "%d-%m-%Y %H:%M")
X2021_May$ended_at = strptime(X2021_May$ended_at, format = "%d-%m-%Y %H:%M")

X2021_Jun$started_at = strptime(X2021_Jun$started_at, format = "%d-%m-%Y %H:%M")
X2021_Jun$ended_at = strptime(X2021_Jun$ended_at, format = "%d-%m-%Y %H:%M")

X2021_Jul$started_at = strptime(X2021_Jul$started_at, format = "%d-%m-%Y %H:%M")
X2021_Jul$ended_at = strptime(X2021_Jul$ended_at, format = "%d-%m-%Y %H:%M")

X2021_Aug$started_at = strptime(X2021_Aug$started_at, format = "%d-%m-%Y %H:%M")
X2021_Aug$ended_at = strptime(X2021_Aug$ended_at, format = "%d-%m-%Y %H:%M")

X2021_Sep$started_at = strptime(X2021_Sep$started_at, format = "%d-%m-%Y %H:%M")
X2021_Sep$ended_at = strptime(X2021_Sep$ended_at, format = "%d-%m-%Y %H:%M")

X2021_Oct$started_at = strptime(X2021_Oct$started_at, format = "%d-%m-%Y %H:%M")
X2021_Oct$ended_at = strptime(X2021_Oct$ended_at, format = "%d-%m-%Y %H:%M")

X2021_Nov$started_at = strptime(X2021_Nov$started_at, format = "%d-%m-%Y %H:%M")
X2021_Nov$ended_at = strptime(X2021_Nov$ended_at, format = "%d-%m-%Y %H:%M")

X2021_Dec$started_at = strptime(X2021_Dec$started_at, format = "%d-%m-%Y %H:%M")
X2021_Dec$ended_at = strptime(X2021_Dec$ended_at, format = "%d-%m-%Y %H:%M")


all_trips_2021 <- bind_rows(X2021_Jan,X2021_Feb,X2021_Mar,X2021_Apr,X2021_May,X2021_Jun,X2021_Jul,X2021_Aug,X2021_Sep,X2021_Oct,X2021_Nov,X2021_Dec)

str(all_trips_2021)

all_trips_2021 <-mutate(all_trips_2021,tripduration=difftime(ended_at,started_at,units = "secs"))
filter(all_trips_2021,tripduration < 0)

all_trips_2021 <- all_trips_2021[!(all_trips_2021$tripduration<0),]

all_trips_2021<-mutate(all_trips_2021,weekday=weekdays(started_at))

#na_if("")  %>%  # We are replacing blanks with NA
  na.omit(all_trips_2021)     # Now we are removing rows with NA values in any column
  
  
all_trips_2021 <-arrange(all_trips_2021,started_at)

View(all_trips_2021)

# Remove start_station_name and end_station_name blank results 
all_trips_2021 <- all_trips_2021 %>%
  filter(!(is.na(start_station_name) | start_station_name == "") ) %>% 
  
  filter(!(is.na(end_station_name) | end_station_name == ""))

# Create a data frame to check that there are no duplicates 
ride_id_check <- all_trips_2021 %>%
  count(ride_id) %>%
  filter(n > 1)

# Create a data frame to check that there are no duplicates 
ride_id_check <- all_trips_cleaned %>%
  count(ride_id) %>%
  filter(n > 1)

View(all_trips_2021)

write.csv(all_trips_2021,"C:\\Users\\Aditi\\OneDrive\\Documents\\Case Study\\all_trips_2021.csv")

View(all_trips_2021)

str(all_trips_2021)

str(train)

train$noChar <- nchar(train$case_id)

train$II <- nchar(train$Hospital_code)
train$III <- nchar(train$Hospital_type_code)
train$IV <- nchar(train$City_Code_Hospital)
train$V <- nchar(train$Hospital_region_code)
train$VI <- nchar(train$`Available Extra Rooms in Hospital`)
train$VII <- nchar(train$Department)
train$VIII <- nchar(train$Ward_Type)
train$IX <- nchar(train$Ward_Facility_Code)
train$X <- nchar(train$`Bed Grade`)
train$XI <- nchar(train$patientid)
train$XII <- nchar(train$City_Code_Patient)
train$XII <- nchar(train$`Type of Admission`)
train$XIV <- nchar(train$`Severity of Illness`)
train$XV <- nchar(train$`Visitors with Patient`)
train$XVI <- nchar(train$Age)
train$XVII <- nchar(train$Admission_Deposit)
train$XVIII <- nchar(train$Stay)

