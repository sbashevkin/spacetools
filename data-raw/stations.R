## code to prepare `stations` dataset goes here
library(dplyr)
library(tidyr)
stations <- zooper::stations%>%
  mutate(Station=paste(Source, Station))%>%
  filter(Source!="YBFMP")%>%
  drop_na(Latitude, Longitude)%>%
  select(-Source)

usethis::use_data(stations, overwrite=T)
