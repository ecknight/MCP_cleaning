#rm(list=ls())  #Clear lists

options(scipen = 999)

library(tidyverse)
library(stringr)
library(readxl)
library(rgdal)
library(sp)
library(lubridate)
library(segclust2d)
library(geosphere)

#Metadata

setwd("/Users/ellyknight/Documents/UoA/Projects/Projects/MCP2/Analysis/Data")
tag <- read_excel("tbl_tag.xlsx", sheet=1)
band <- read_excel("tbl_Band.xlsx", sheet=1) %>% 
  dplyr::rename(BandDate = Date,
                BandTime = Time,
                BandLat = Lat,
                BandLong = Long)
pop <- read_excel("tbl_population.xlsx", sheet=1)

#Read in transmissions----

setwd("/Users/ellyknight/Documents/UoA/Projects/Projects/MCP2/Data/Transmissions/Raw")#Set working directory

files <- list.files(pattern=".csv", recursive=TRUE)

trans.all <- data.frame()
trans.clean <- data.frame()

for (file in files) {
  data1 <- read.csv(file) %>% 
    dplyr::select(CRC, Date, Time, Latitude, Longitude, Fix) %>% 
    mutate(file=file) %>% 
    mutate(ArgosID=as.integer(str_sub(file, -23, -18))) %>% 
    mutate(retrieval="transmission")
  trans.all <- rbind(trans.all, data1)
}

trans.clean1 <- trans.all %>% 
  dplyr::select(-file) %>% 
  left_join(band, by="ArgosID") %>% 
  left_join(pop, by="Population") %>% 
  left_join(tag, by=c("ArgosID", "PinpointID")) %>% 
  mutate(Date=as.POSIXct(strptime(Date, format="%d/%m/%Y")),
         Type="Transmission") %>% 
  dplyr::select(PinpointID, ArgosID, Type, TagOwner, Population, CRC, Fix, Date, Time, Latitude, Longitude)

trans.all1 <- trans.all %>% 
  left_join(tag, by="ArgosID") %>% 
  mutate(Date=dmy(Date),
         Date = paste(month(Date),day(Date),year(Date), sep="/")) %>% 
  mutate(Altitude=-99,
         Duration=-99,
         DOP=-99,
         Satellites=-99,
         Type="Transmission") %>% 
  dplyr::select(PinpointID, ArgosID, Type, CRC, Date, Time, Latitude, Longitude, Altitude, Fix, Duration, DOP, Satellites)

#Read in retrievals----

setwd("/Users/ellyknight/Documents/UoA/Projects/Projects/MCP2/Data/Transmissions/RetrievedArgos")#Set working directory

files <- list.files(pattern=".csv", recursive=TRUE)

ret.all <- data.frame()

for (file in files) {
  data1 <- read.csv(file) %>% 
    mutate(file=file) %>% 
    mutate(PinpointID=as.integer(str_sub(file, 10, 12))) 
  #    mutate(downloaddate=str_sub(file, 6, 15)))
  ret.all <- rbind(ret.all, data1)
}

ret.clean1 <- ret.all %>% 
  left_join(band, by="PinpointID") %>% 
  left_join(pop, by="Population") %>% 
  left_join(tag, by=c("ArgosID", "PinpointID")) %>% 
  mutate(ID=paste0(Population, "-", PinpointID)) %>% 
  separate(GMT.Time, into=c("Date", "Time", "AMPM"), sep=" ", remove=FALSE) %>% 
  separate(Time, into=c("hour", "min", "sec"), sep=":") %>% 
  mutate(Date=as.POSIXct(strptime(Date, format="%m/%d/%Y")),
         hour=ifelse(AMPM=="PM", as.numeric(hour)+12, as.numeric(hour)),
         Time=paste0(hour, ":", min, ":", sec),
         Type="Retrieval",
         CRC='NA',
         Fix='NA') %>% 
  dplyr::select(PinpointID, ArgosID, Type, TagOwner, Population, CRC, Fix, Date, Time, Latitude, Longitude)

ret.all1 <- ret.all %>% 
  left_join(tag, by="PinpointID") %>% 
  separate(GMT.Time, into=c("Date", "Time", "AMPM"), sep=" ", remove=FALSE) %>% 
  separate(Time, into=c("hour", "min", "sec"), sep=":") %>% 
  mutate(hour=ifelse(AMPM=="PM", as.numeric(hour)+12, as.numeric(hour)),
         Time=paste0(hour, ":", min, ":", sec),
         CRC='NA',
         Fix='NA',
         Type="Retrieval") %>% 
  dplyr::select(PinpointID, ArgosID, Type, CRC, Date, Time, Latitude, Longitude, Altitude, Fix, Duration, DOP, Satellites)

#Put together----

all <- rbind(ret.all1, trans.all1) %>% 
  unique()

clean <- rbind(ret.clean1, trans.clean1) %>% 
  unique() %>% 
  dplyr::rename(Lat = Latitude, Long = Longitude) %>% 
  arrange(Population, PinpointID, Date, Time) %>% 
  group_by(PinpointID) %>% 
  mutate(Order=row_number()) %>% 
  dplyr::filter(CRC!="Fail",
                Lat!=0) %>% 
  ungroup() %>% 
  arrange(PinpointID, Date, Time)

#Write out----
date <- Sys.Date()
write.csv(all, paste0("/Users/ellyknight/Documents/UoA/Projects/Projects/MCP2/Data/Transmissions/CONIMCP_AllData_", date, ".csv"), row.names=FALSE)
write.csv(clean, paste0("/Users/ellyknight/Documents/UoA/Projects/Projects/MCP2/Data/Transmissions/CONIMCP_CleanData_", date, ".csv"), row.names=FALSE)

#Clean out inaccurate points & dead birds----

removals <- read.csv("/Users/ellyknight/Documents/UoA/Projects/Projects/MCP2/Data/Cleaning/RemovedPoints.csv") %>% 
  mutate(Date=as.POSIXct(strptime(Date, format="%Y-%m-%d"))) %>% 
  dplyr::select(PinpointID, Lat, Long)

clean2 <- clean %>%
  anti_join(removals) %>% 
  filter(Long < 0) %>% 
  filter(!PinpointID %in% c(466, 818, 829)) %>% #remove birds that never left
  filter(!PinpointID %in% c(135, 440, 446, 461, 817, 823)) %>%  #remove birds that likely died during fall migration
  filter(!PinpointID %in% c(447)) %>% #remove birds that seem to be wintering very far north
  filter()

#Remove duplicates from retrievals & argos points----

clean3 <- clean2 %>% 
  mutate(LatR =round(Lat, 3),
         LongR=round(Long, 3),
         Month=month(Date),
         Day=day(Date),
         Hour=str_sub(Time, 1, 2),
         Min=str_sub(Time, 4, 5)) %>% 
  arrange(PinpointID, Month, Day, Fix, CRC) %>% 
  ungroup()

clean4 <- clean3 %>% 
  dplyr::select(PinpointID, Month, Day) %>% 
  duplicated() %>% 
  data.frame()
colnames(clean4) <- "dup"

clean5 <- clean3 %>% 
  cbind(clean4) %>% 
  arrange(PinpointID, Order) %>% 
  dplyr::mutate(dup2 = ifelse(!Month%in%c(6,7), dup, "FALSE"))
summary(as.factor(clean5$dup2))
table(clean5$dup2, clean5$Fix, clean5$Type, clean5$CRC)

clean6 <- clean5 %>% 
  arrange(PinpointID, Order) %>% 
  dplyr::filter(dup2!="TRUE") %>% 
  dplyr::select(PinpointID, ArgosID, Type, TagOwner, Population, CRC, Fix, Date, Time, Lat, Long, Order) %>% dplyr::filter(!Fix%in%c("AZ", "AB", "AA", "A0", "A1", "A2", "A3"))

#Add banding locations----
band.tags <- band %>% 
  semi_join(clean6, by="PinpointID") %>% 
  inner_join(tag, by=c("PinpointID", "ArgosID")) %>% 
  mutate(Type="Band",
         CRC="NA",
         Fix="NA",
         Order=0,
         BandTime="00:00:00") %>% 
  dplyr::rename(Date=BandDate, Time=BandTime, Lat=BandLat, Long=BandLong) %>% 
  dplyr::select(PinpointID, ArgosID, Type, TagOwner, Population, CRC, Fix, Date, Time, Lat, Long, Order) 
  
clean7 <- clean6 %>% 
  rbind(band.tags) %>% 
  mutate(doy = yday(Date),
         doy.order = case_when(yday(Date) > 200 ~ yday(Date)-222,
                               yday(Date) < 200 ~ yday(Date) + 143),
         week = week(Date),
         DateTime=as.POSIXct(strptime(paste(Date, Time), format="%Y-%m-%d %H:%M:%S"))) %>% 
  group_by(PinpointID) %>% 
  arrange(DateTime) %>% 
  mutate(Order=row_number()) %>% 
  arrange(PinpointID, Order)

#Add legs----
clean8 <- clean7 %>% 
  filter(year(DateTime) <= 2020,
         year(DateTime) >= 2015,
         Lat!=0,
         Lat > -30) %>% 
  group_by(PinpointID) %>% 
  arrange(PinpointID, DateTime) %>% 
  mutate(Duration = as.numeric(difftime(lead(DateTime), DateTime, units="days")),
         MeanLat = (Lat + lead(Lat))/2,
         MeanLong = (Long + lead(Long))/2) %>% 
#  filter((!is.na(Duration)) %>% 
  mutate(Order=row_number()) %>% 
  ungroup()

latlong1 <- clean8 %>% 
  dplyr::select(Long, Lat)

Dist <- distHaversine(latlong1)/1000
Dist <- c(Dist, NA)

clean9 <- clean8 %>% 
  cbind(Dist) %>% 
  mutate(Dist = ifelse(Order < lead(Order), Dist, NA),
         Duration = ifelse(Order < lead(Order), Duration, NA),
         MeanLat = ifelse(Order < lead(Order), MeanLat, NA),
         MeanLong = ifelse(Order < lead(Order), MeanLong, NA)) %>% 
  mutate(Speed = Dist/Duration) %>% 
  rename(TravDist = Dist)

clean10 <- band %>% 
  dplyr::select(PinpointID, BandLat, BandLong, BandDate, Mass, Wing, Sex) %>% 
  right_join(clean9, by="PinpointID") %>% 
  arrange(PinpointID, DateTime)

latlong1 <- clean10 %>% 
  dplyr::select(Long, Lat)

bandlatlong <- clean10 %>% 
  dplyr::select(BandLong, BandLat)

BandDist <- data.frame(BandDist = distHaversine(latlong1, bandlatlong)/1000,
                       PinpointID = clean10$PinpointID,
                       Date = clean10$Date,
                       Time = clean10$Time,
                       Order = clean10$Order)

#Identify birds with wintering grounds----
clean11 <- clean10 %>% 
  left_join(BandDist) %>% 
  group_by(PinpointID) %>% 
  mutate(MaxDist=max(BandDist),
         WinterDist=MaxDist-BandDist,
         MinLat=min(Lat)) %>% 
  filter(WinterDist < 100) %>% 
  dplyr::summarize(WintLat=mean(Lat),
            WintLong=mean(Long),
            WintN=n()) %>% 
  ungroup() %>% 
  right_join(clean10, by="PinpointID") %>% 
  arrange(PinpointID, DateTime)

wintlatlong <- clean11 %>% 
  dplyr::select(WintLong, WintLat)

latlong1 <- clean11 %>% 
  dplyr::select(Long, Lat)

WintDist <- data.frame(WintDist = distHaversine(latlong1, wintlatlong)/1000,
                       PinpointID = clean11$PinpointID,
                       Date = clean11$Date,
                       Time = clean11$Time,
                       Order = clean11$Order)

#Calculate GCD
GCD <- distHaversine(wintlatlong, bandlatlong)/1000

Distances <- left_join(BandDist, WintDist)

#Classify seasons----
clean12 <-  clean11 %>% 
  left_join(Distances) %>% 
  cbind(GCD) %>% 
  mutate(Winter=ifelse(WintN > 1, 1, 0)) %>% 
  mutate(Season = case_when(year(BandDate)==year(Date) & BandDist < 100 ~ "Breed1",
                             year(BandDate)<year(Date) & BandDist < 100 ~ "Breed2",
                             Winter==1 & WintDist < 100 ~ "Winter",
                             BandDist >=100 & Winter==1 & WintDist >= 100 & year(BandDate)==year(Date) ~ "FallMig",
                             BandDist >=100 & Winter==0 & year(BandDate)==year(Date) ~ "FallMig",
                             BandDist >=100 & Winter==1 & WintDist >= 100 & year(BandDate)<year(Date) ~ "SpringMig",
                             BandDist >=100 & Winter==0 & year(BandDate)<year(Date) ~ "SpringMig")) %>% 
  mutate(Season=ifelse((PinpointID==828 & Lat > 58.2 & year(Date)==2019), "Breed2", Season)) %>% 
  mutate(Season=ifelse(lag(Season)=="Winter" & Season=="FallMig", "SpringMig", Season))

#Classify second wintering grounds
clean13 <- clean12 %>% 
  dplyr::mutate(WintDistRound = round(WintDist, -2)) %>% 
  dplyr::mutate(Season2 = case_when(PinpointID==81 & WintDistRound==1000 ~ "Winter2",
                                    PinpointID==439 & WintDistRound==1600 ~ "Winter2",
                                    PinpointID==439 & WintDistRound==1700 ~ "WinterMig",
                                    PinpointID==443 & WintDistRound==1900 ~ "Winter2",
                                    PinpointID==443 & WintDistRound==1200 ~ "WinterMig",
                                    PinpointID==490 & WintDistRound==1200 ~ "Winter2",
                                    PinpointID==825 & WintDistRound %in% c(300, 1200) ~ "WinterMig",
                                    PinpointID==825 & WintDistRound==1300 & year(DateTime)==2019 ~ "Winter2",
                                    PinpointID==826 & WintDistRound==1500 ~ "WinterMig",
                                    PinpointID==826 & WintDistRound==1600 & year(DateTime)==2019 ~ "Winter2",
                                    PinpointID==828 & WintDistRound==800 ~ "Winter2",
                                    !is.na(Season) ~ Season))

clean14 <- clean13 %>% 
  dplyr::group_by(PinpointID, Season) %>% 
  summarize(n=n()) %>% 
  spread(key=Season, value=n, fill=0) %>% 
  rename(nbreed1=Breed1, nbreed2=Breed2, nfall=FallMig, nspring=SpringMig, nwinter=Winter) %>% 
  right_join(clean13, by=c("PinpointID")) %>% 
  dplyr::select(-WintN) %>% 
  mutate(Winter=ifelse(nwinter > 0, 1, 0)) %>% 
  ungroup()


#Classify continents----
clean15 <- clean14 %>% 
  mutate(MeanLat = as.numeric(lead(Lat)+Lat)/2,
         DiffLat = abs(lead(Lat)-Lat)) %>% 
  mutate(ContRate=case_when(TravDist > 4000 ~ "NA",
                        MeanLat > 27 ~ "North",
                        MeanLat < 27 & MeanLat > 7 ~ "Central",
                        MeanLat < 7 ~ "South"),
         ContPoint=case_when(Lat > 27 ~ "North",
                             Lat < 27 & Lat > 7 ~ "Central",
                             Lat < 7 ~ "South"))

clean15.fall <- clean14 %>% 
  dplyr::filter(Season=="FallMig") %>% 
  arrange(Population, PinpointID, Date)

clean15.spring <- clean14 %>% 
  dplyr::filter(Season=="SpringMig") %>% 
  arrange(Population, PinpointID, Date)


#Calculate days since migration----
dat.fall.dep <- clean15 %>%
  dplyr::filter(Type!="Band") %>% 
  mutate(Diff = case_when(Season!=dplyr::lead(Season) ~ 1,
                          Season!=dplyr::lag(Season) ~ 2)) %>% 
  dplyr::filter((Diff==1 & Season=="Breed1") |
                (Diff==2 & Season=="FallMig")) %>% 
  group_by(PinpointID) %>% 
  summarize(Date1=yday(min(Date)),
            Date2=yday(max(Date)),
            Date = yday(mean(Date))) %>% 
  mutate(Duration = Date2 - Date1) %>% 
  dplyr::filter(Duration > 0 ) %>% 
  ungroup() %>% 
  dplyr::select(PinpointID, Date, Duration) %>% 
  dplyr::rename(DoyFallDep=Date,
                DurFallDep=Duration)

dat.fall.arr <- clean15 %>%
  dplyr::filter(Type!="Band") %>% 
  mutate(Diff = case_when(Season!=dplyr::lead(Season) ~ 1,
                          Season!=dplyr::lag(Season) ~ 2)) %>% 
  dplyr::filter((Diff==1 & Season=="FallMig") |
                  (Diff==2 & Season=="Winter")) %>% 
  group_by(PinpointID) %>% 
  summarize(Date1=yday(min(Date)),
            Date2=yday(max(Date)),
            Date = yday(mean(Date))) %>% 
  mutate(Duration = Date2 - Date1) %>% 
  dplyr::filter(Duration > 0 ) %>% 
  ungroup() %>% 
  dplyr::select(PinpointID, Date, Duration) %>% 
  dplyr::rename(DoyFallArr=Date,
                DurFallArr=Duration)

dat.spring.dep <- clean15 %>%
  dplyr::filter(Type!="Band") %>% 
  mutate(Diff = case_when(Season!=dplyr::lead(Season) ~ 1,
                          Season!=dplyr::lag(Season) ~ 2)) %>% 
  dplyr::filter((Diff==1 & Season=="Winter") |
                  (Diff==2 & Season=="SpringMig")) %>% 
  group_by(PinpointID) %>% 
  summarize(Date1=yday(min(Date)),
            Date2=yday(max(Date)),
            Date = yday(mean(Date))) %>% 
  mutate(Duration = Date2 - Date1) %>% 
  dplyr::filter(Duration > 0 ) %>% 
  ungroup() %>% 
  dplyr::select(PinpointID, Date, Duration) %>% 
  dplyr::rename(DoySpringDep=Date,
                DurSpringDep=Duration)

dat.spring.arr <- clean15 %>%
  dplyr::filter(Type!="Band") %>% 
  mutate(Diff = case_when(Season!=dplyr::lead(Season) ~ 1,
                          Season!=dplyr::lag(Season) ~ 2)) %>% 
  dplyr::filter((Diff==1 & Season=="SpringMig") |
                  (Diff==2 & Season=="Breed2")) %>% 
  group_by(PinpointID) %>% 
  summarize(Date1=yday(min(DateTime)),
            Date2=yday(max(DateTime)),
            Date = yday(mean(DateTime))) %>% 
  mutate(Duration = Date2 - Date1) %>% 
  dplyr::filter(Duration > 0 ) %>% 
  ungroup() %>% 
  dplyr::select(PinpointID, Date, Duration) %>% 
  dplyr::rename(DoySpringArr=Date,
                DurSpringArr=Duration)

clean16 <- clean15 %>% 
  left_join(dat.fall.dep, by=c("PinpointID")) %>% 
  left_join(dat.fall.arr, by=c("PinpointID")) %>% 
  left_join(dat.spring.dep, by=c("PinpointID")) %>% 
  left_join(dat.spring.arr, by=c("PinpointID"))

#Add Gulf route----
dat.tags <- read.csv("/Users/ellyknight/Documents/UoA/Projects/Projects/MCP2/Analysis/Data/GulfRoute.csv") %>% 
  dplyr::select(PinpointID, fall, spring) %>% 
  rename(routefall=fall, routespring=spring)

clean17 <- clean16 %>% 
  left_join(dat.tags, by="PinpointID") %>% 
  mutate(Year=year(BandDate)) %>% 
  arrange(PinpointID, DateTime) %>% 
  data.frame()

setwd("/Users/ellyknight/Documents/UoA/Projects/Projects/MCP2/Analysis/Data")
write.csv(clean17, "CONIMCP_CleanDataAll.csv", row.names = FALSE)