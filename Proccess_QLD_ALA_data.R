dat = read.csv('QLD_covid_dat.csv')
dat = dat[ (dat$year==2020  & dat$month==3 & dat$day>=29) | 
                       (dat$year==2020  & dat$month>3) | 
                       (dat$year==2021  & dat$month<3) | 
                       (dat$year==2021 & dat$month==3 & dat$day<=28), ]

dates = paste(dat$year, dat$month, dat$day, sep='-')

library(lubridate)
dat$eventDate = as.numeric( as.POSIXct(strptime(dates, "%Y-%m-%d") ) )
dat.sorted = dat[order(dat$eventDate),]

write.csv(dat.sorted, 'ALA_QLD_records.csv')