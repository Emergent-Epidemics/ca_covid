#SV Scarpino
#July 2020
#CA COVID county-level Hospitalizations

###########
#libraries#
###########
library(zoo)

#########
#Globals#
#########
roll_mean_days <- 7
time_stamp <- format(Sys.time(), format = "%d_%m_%Y")
save_new <- FALSE

######
#Data#
######
dat_ca <- read.csv("https://data.ca.gov/dataset/529ac907-6ba1-4cb7-9aae-8966fc96aeef/resource/42d33765-20fd-44b8-a978-b083b7542225/download/hospitals_by_county.csv")
dat_ca$date <- as.POSIXct(strptime(dat_ca$todays_date, format = "%Y-%m-%d"))

#########
#Process#
#########
by_day_ca_county <- by(data = dat_ca$hospitalized_covid_confirmed_patients, INDICES = dat_ca[,c("todays_date", "county")], FUN = sum, na.rm = TRUE)

mat_by_day_ca_county <- matrix(by_day_ca_county, nrow = nrow(by_day_ca_county), ncol = ncol(by_day_ca_county))
colnames(mat_by_day_ca_county) <- colnames(by_day_ca_county)
rownames(mat_by_day_ca_county) <- rownames(by_day_ca_county)

######
#Save#
######
if(save_new == TRUE){
  write.csv(mat_by_day_ca_county, file = paste0("../Data/CA_county_hosp_COVID_", time_stamp, ".csv"), quote = FALSE)
}