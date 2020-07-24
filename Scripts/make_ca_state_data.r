#SV Scarpino
#July 2020
#CA COVID state-level confirmed cases

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
dat_ca <- read.csv("https://data.ca.gov/dataset/590188d5-8545-4c93-a9a0-e230f0db7290/resource/926fd08f-cc91-4828-af38-bd45de97f8c3/download/statewide_cases.csv")
dat_ca$date <- as.POSIXct(strptime(dat_ca$date, format = "%Y-%m-%d"))

#need to download latestdata.csv from https://github.com/beoutbreakprepared/nCoV2019
dat_gh <- read.csv("../Data/latestdata.csv")
dat_gh$date_confirmation <- as.POSIXct(strptime(dat_gh$date_confirmation, format = "%d.%m.%Y"))

##########
#Citation#
##########
#Please cite  Xu, B., Gutierrez, B., Mekaru, S. et al. Epidemiological data from the COVID-19 outbreak, real-time case information. Sci Data 7, 106 (2020). https://doi.org/10.1038/s41597-020-0448-0

cat("Please cite: ", "\n", "\n",  "Xu, B., Gutierrez, B., Mekaru, S. et al. Epidemiological data from the COVID-19 outbreak, real-time case information. Sci Data 7, 106 (2020). https://doi.org/10.1038/s41597-020-0448-0", "\n", "\n")

#########
#Process#
#########
use_ca <- which(dat_gh$province == "California")

by_day_ca_gh <- by(data = rep(1, length(use_ca)), INDICES = dat_gh$date_confirmation[use_ca], FUN = sum, na.rm = TRUE)

by_day_ca_ca <- by(data = dat_ca$newcountconfirmed, INDICES = dat_ca$date, FUN = sum, na.rm = TRUE)

date <- c(as.character(names(by_day_ca_gh)), as.character(names(by_day_ca_ca)))
date <- as.POSIXct(strptime(date, format = "%Y-%m-%d"))
count <- c(as.character(by_day_ca_gh), as.character(by_day_ca_ca))

dates <- format(seq(min(date, na.rm = TRUE), max(date, na.rm = TRUE) + 2*60*60*24, by = 60*60*24), format = "%Y-%m-%d")
count <- rep(NA, length(dates))
data_source <- rep(NA, length(dates))
for(i in 1:length(dates)){
  use.i.ca <- which(as.character(names(by_day_ca_ca)) == as.character(dates)[i])
  if(length(use.i.ca) > 1){
    stop()
  }
  use.i.ca.gh <- which(as.character(names(by_day_ca_gh)) == as.character(dates)[i])
  if(length(use.i.ca.gh) > 1){
    stop()
  }
  if(length(use.i.ca) == 1 & length(use.i.ca.gh) == 0){
    count[i] <- as.numeric(as.character(by_day_ca_ca[use.i.ca]))
    data_source[i] <- "ca.state"
  }
  if(length(use.i.ca) == 0 & length(use.i.ca.gh) == 1){
    count[i] <- as.numeric(as.character(by_day_ca_gh[use.i.ca.gh]))
    data_source[i] <- "open.covid.data"
  }
  if(length(use.i.ca) == 1 & length(use.i.ca.gh) == 1){
    count.i <- c(as.numeric(as.character(by_day_ca_ca[use.i.ca])), as.numeric(as.character(by_day_ca_gh[use.i.ca.gh])))
    use.i <<- which.max(count.i)
    count[i] <- max(count.i[use.i], na.rm = TRUE)
    data_source[i] <- "ca.state + open.covid.data"
  }
}
count[which(is.na(count) == TRUE)] <- 0
rollmean_cout <- rep(NA, length(count))
rollmean_cout[-c(1:(roll_mean_days-1))] <- rollmean(x = count, k = roll_mean_days)

dat.out <- data.frame(dates, count, rollmean_cout, data_source)
colnames(dat.out) <- c("Date", "Daily_new", paste0("Rolling_mean_", roll_mean_days, "_days"), "Source")

######
#Save#
######
if(save_new == TRUE){
  write.csv(dat.out, file = paste0("../Data/CA_state_COVID_OpenCovid_", time_stamp, ".csv"), row.names = FALSE, quote = FALSE)
}