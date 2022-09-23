#Written by Melodie Lao (github.com/data-lao) & Leigh Creilley 
#feel free to email data-lao/VandenBoer group if you come across any errors or require help in troubleshooting your tNr instrument 
#packages used 
library(xts)
library(zoo)
library(data.table)
library(lubridate)
library(tidyverse)
library(padr)

rm(list=ls(all=TRUE)) #clear all previous variables in environment 

# 1. Data Processing 
# Import Data ------------------------------------------------------------
setwd("C:/Users/your_drivename_here")

tnrdata <- read.csv(file.choose(), sep = "\t") #load csv file 

## Modify Dataset ------------------------------------------------------------
tnrdata <- tnrdata[ -c(5:6)] #removes v4 and MFC2 flow which are column 5 and 6 

tnrdata$date <- dmy_hms(tnrdata$datetime) 

colnames(tnrdata) <- c("datetime", 
                       "V1", 
                       "V2", 
                       "V3", 
                       "NO", 
                       "NO2", 
                       "NOx", 
                       "date") 

tnrdata$Vt <- rowSums(tnrdata[2:4])

tnrdata <- tnrdata %>%            
  mutate(tnr_cycle = case_when(
    Vt == 0 ~ "tNr",
    Vt == 2 ~ "NH3",
    Vt == 1 ~ "HONO",
    Vt == 3 ~ "NOx")) 

#add a cycle count up column.
#cycle defined as a measurement mode, i.e. NOx, HONO, tNr or NH3
tnrdata$Vt<- rowSums(tnrdata[2:4])

foo<-rle(tnrdata$Vt) #creates cycle based on total valve change 
foo$values <- 1:length(foo$values) 
tnrdata$cycle <- inverse.rle(foo)

ncyc = max(tnrdata$cycle)

TD <- subset(tnrdata, select = c(8,9,5,6,7,10,11)) #re-arrange columns to a new dataframe 

TD <- data.table(TD)

# double check if cycle numbers are uneven
TD %>% 
  select(tnr_cycle, cycle) %>%
  table()
options(max.print=999999)

#applied voltage corrections to instrument
TD2 <- TD %>% 
  mutate(TD, NOcorr = (1.0018*TD$NO) - 0.8569) %>%
  mutate(TD, NO2corr = (1.0018*TD$NO2) - 0.8469)
  
TD2 <- mutate(TD2, NOxcorr = TD2$NOcorr + TD2$NO2corr)

TD3 <- subset(TD2, select = c("date", "Vt", "NOcorr", "NO2corr", "NOxcorr", "tnr_cycle", "cycle"))

#rearrange where number of cycles become columns 
pivot_wider(TD3, names_from = cycle, values_from = NOcorr ) -> totcycl_TD

#create new df per each cycle
for(i in unique(TD3$cycle)) {
  nam <- paste("TD", i, sep = "_")
  assign(nam, TD3[TD3$cycle == i, ])
}

# CYCLE 1 ------------------------------------------------------------
  #the length of cycle 1 is not divisible by 60, ruining the averaged start time
  #Remove the first rows by nth to make it divisible by 60 for time averaging to a min 

rn <- nrow(TD_1) #rn = row number
rm <- rn %% 60  #rm = remainder of row number when divided by 60 
TD_1 <- tail(TD_1, -rm)
                
# CLEAN/AVG ------------------------------------------------------------
## Avg 1 min ------------------------------------------------------------

list_TD <- mget(ls(pattern = 'TD_'))  #makes a list of all df with cycles 

#apply averaging of every 60 rows to list 
AVG_list <- map(list_TD, ~ .x %>%
                group_by(date = cut(date, breaks = "60 sec")) %>% 
                summarize(NO = mean(NOcorr), 
                          NO2 = mean(NO2corr), 
                          NOx = mean(NOxcorr), 
                          Vt = mean(Vt), 
                          cycle = mean(cycle))
                )
#NOTE: averaged times are different by seconds in cycle 1 to other cycles due to uneven lengths 

## Remove Rows ------------------------------------------------------------

AVG_list2 <- lapply(AVG_list, tail, -1) #removes first 60 rows in this list 
       #maybe better to replace c(5,6,7) with replace_na by tail, -1 

## Combine Data ------------------------------------------------------------

CL <- bind_rows(AVG_list2, .id = "column_label")  #combines list in one df, sort by date
CL <- arrange(CL, cycle) #rearrange by date

CLtot <- CL %>%            
  mutate(tnr_cycle = case_when(
    Vt == 0 ~ "tNr",
    Vt == 2 ~ "NH3",
    Vt == 1 ~ "HONO",
    Vt == 3 ~ "NOx"))

#create new df of each tnr_cycle
for(i in unique(CLtot$tnr_cycle)) {
  nam1 <- paste("CL", i, sep = "_")
  assign(nam1, CLtot[CLtot$tnr_cycle == i, ])
}

# CALCULATIONS ------------------------------------------------------------
## Linear Interp ------------------------------------------------------------
### HONO ------------------------------------------------------------
CL_HONO[['date']] <- as.POSIXct(CL_HONO[['date']],
                                format = "%Y-%m-%d %H:%M:%S")

#aggregate if the time series don't match up (seconds skipped,etc) 
#datetime variable contains missing values, they are left in place in the df with thicken 
CL_HONO2 <- thicken(CL_HONO, '1 min')
CL_HONO2 <- CL_HONO2[ -c(2)]

CL_HONO2[['date_min']] <- as.POSIXct(CL_HONO2[['date_min']],
                                format = "%Y-%m-%d %H:%M:%S")

LI_HONO <- CL_HONO2 %>%
  pad(group = 'tnr_cycle', interval = 'min') %>%   # Explicitly fill by 1 min
  fill_by_value(0)

LI_HONO_2 <- LI_HONO 
LI_HONO_2$NO <- na.approx(LI_HONO$NO)
LI_HONO_2$NO2 <- na.approx(LI_HONO$NO2)
LI_HONO_2$NOx <- na.approx(LI_HONO$NOx)

### NOx ------------------------------------------------------------
CL_NOx[['date']] <- as.POSIXct(CL_NOx[['date']],
                               format = "%Y-%m-%d %H:%M:%S")

CL_NOx2 <- thicken(CL_NOx, '1 min')
CL_NOx2 <- CL_NOx2[ -c(2)]

CL_NOx2[['date_min']] <- as.POSIXct(CL_NOx2[['date_min']],
                                     format = "%Y-%m-%d %H:%M:%S")

LI_NOX <- CL_NOx2 %>%
  pad(group = 'tnr_cycle', interval = 'min') %>%   # Explicitly fill by 1 min
  fill_by_value(0)

LI_NOX_2 <- LI_NOX 
LI_NOX_2$NO <- na.approx(LI_NOX$NO)
LI_NOX_2$NO2 <- na.approx(LI_NOX$NO2)
LI_NOX_2$NOx <- na.approx(LI_NOX$NOx)

### NH3 ------------------------------------------------------------
CL_NH3[['date']] <- as.POSIXct(CL_NH3[['date']],
                               format = "%Y-%m-%d %H:%M:%S")

CL_NH3_2 <- thicken(CL_NH3, '1 min')
CL_NH3_2 <- CL_NH3_2[ -c(2)]

CL_NH3_2[['date_min']] <- as.POSIXct(CL_NH3_2[['date_min']],
                                    format = "%Y-%m-%d %H:%M:%S")

LI_NH3 <- CL_NH3_2 %>%
  pad(group = 'tnr_cycle', interval = 'min') %>%   # Explicitly fill by 1 min
  fill_by_value(0)

LI_NH3_2 <- LI_NH3 
LI_NH3_2$NO <- na.approx(LI_NH3$NO)
LI_NH3_2$NO2 <- na.approx(LI_NH3$NO2)
LI_NH3_2$NOx <- na.approx(LI_NH3$NOx)

### tNr ------------------------------------------------------------
CL_tNr[['date']] <- as.POSIXct(CL_tNr[['date']],
                               format = "%Y-%m-%d %H:%M:%S")

CL_tNr2 <- thicken(CL_tNr, '1 min')
CL_tNr2 <- CL_tNr2[ -c(2)]

CL_tNr2[['date_min']] <- as.POSIXct(CL_tNr2[['date_min']],
                                     format = "%Y-%m-%d %H:%M:%S")

LI_TNR <- CL_tNr2 %>%
  pad(group = 'tnr_cycle', interval = 'min') %>%   # Explicitly fill by 1 min
  fill_by_value(0)

LI_TNR_2 <- LI_TNR 
LI_TNR_2$NO <- na.approx(LI_TNR$NO)
LI_TNR_2$NO2 <- na.approx(LI_TNR$NO2)
LI_TNR_2$NOx <- na.approx(LI_TNR$NOx)

## No Interpolation ------------------------------------------------------------
NL_HONO <- merge(LI_HONO, LI_NOX, by = "date_min", all = TRUE)
NL_NH3 <- merge(LI_TNR, LI_NH3, by = "date_min", all = TRUE)
NL_Nr <- merge(NL_HONO, NL_NH3, by = "date_min", all = TRUE)

#merge columns again for vt and cycles 
NL_Nr2 <- NL_Nr
NL_Nr2$vt = NL_Nr2$Vt.x.x  
NL_Nr2$vt[!is.na(NL_Nr2$Vt.y.x)] = NL_Nr$Vt.y.x[!is.na(NL_Nr$Vt.y.x)]  
NL_Nr2$vt[!is.na(NL_Nr2$Vt.x.y)] = NL_Nr$Vt.x.y[!is.na(NL_Nr$Vt.x.y)]  
NL_Nr2$vt[!is.na(NL_Nr2$Vt.y.y)] = NL_Nr$Vt.y.y[!is.na(NL_Nr$Vt.y.y)]

NL_Nr2$cycle = NL_Nr2$cycle.x.x  
NL_Nr2$cycle[!is.na(NL_Nr2$cycle.x.y)] = NL_Nr$cycle.x.y[!is.na(NL_Nr$cycle.x.y)] 
NL_Nr2$cycle[!is.na(NL_Nr2$cycle.y.x)] = NL_Nr$cycle.y.x[!is.na(NL_Nr$cycle.y.x)]  
NL_Nr2$cycle[!is.na(NL_Nr2$cycle.y.y)] = NL_Nr$cycle.y.y[!is.na(NL_Nr$cycle.y.y)]

NL_Nr2 <- NL_Nr2[ -c(2,6:9,13:16,20:23, 27:29)]

colnames(NL_Nr2) <- c("date_min", 
                     "HONO.NO", 
                     "HONO.NO2", 
                     "HONO.NOx", 
                     "NOx.NO",
                     "NOx.NO2",
                     "NOx.NOx",
                     "tnr.NO",
                     "tnr.NO2",
                     "tnr.NOx",
                     "nh3.NO",
                     "nh3.NO2",
                     "nh3.NOx",
                     "vt",
                     'cycle')

#compile original data with accurate datetime averaged to 1 min
O_HONO <- merge(CL_HONO, CL_NOx, by = "date", all = TRUE)
O_NH3 <- merge(CL_tNr, CL_NH3, by = "date", all = TRUE)
O_Nr <- merge(O_HONO, O_NH3, by = "date", all = TRUE)

O_Nr2 <- O_Nr
O_Nr2$vt = O_Nr2$Vt.x.x
O_Nr2$vt[!is.na(O_Nr2$Vt.y.x)] = O_Nr$Vt.y.x[!is.na(O_Nr$Vt.y.x)]  
O_Nr2$vt[!is.na(O_Nr2$Vt.x.y)] = O_Nr$Vt.x.y[!is.na(O_Nr$Vt.x.y)]  
O_Nr2$vt[!is.na(O_Nr2$Vt.y.y)] = O_Nr$Vt.y.y[!is.na(O_Nr$Vt.y.y)]

O_Nr2$cycle = O_Nr2$cycle.x.x  
O_Nr2$cycle[!is.na(O_Nr2$cycle.x.y)] = O_Nr$cycle.x.y[!is.na(O_Nr$cycle.x.y)] 
O_Nr2$cycle[!is.na(O_Nr2$cycle.y.x)] = O_Nr$cycle.y.x[!is.na(O_Nr$cycle.y.x)]  
O_Nr2$cycle[!is.na(O_Nr2$cycle.y.y)] = O_Nr$cycle.y.y[!is.na(O_Nr$cycle.y.y)]

O_Nr2 <- O_Nr2[ -c(2,6:9,13:16,20:23, 27:29)]

colnames(O_Nr2) <- c("date_min", 
                      "HONO.NO", 
                      "HONO.NO2", 
                      "HONO.NOx", 
                      "NOx.NO",
                      "NOx.NO2",
                      "NOx.NOx",
                      "tnr.NO",
                      "tnr.NO2",
                      "tnr.NOx",
                      "nh3.NO",
                      "nh3.NO2",
                      "nh3.NOx",
                      "vt",
                      'cycle')



## HONO-NO2 ------------------------------------------------------------
#here we take the avg data and calc HONO and NH3 by differnce using the nearest points

#1st HONO calculation from NOx
#merge files based on base
F_HONO <- merge(LI_HONO_2, LI_NOX_2, by = "date_min", all = TRUE) 
# .x columns are HONO pathway, .y columns are true NOx pathway 

F_HONO$cycle = F_HONO$cycle.x  
F_HONO$cycle[!is.na(F_HONO$cycle.y)] = F_HONO$cycle.y[!is.na(F_HONO$cycle.y)] 

F_HONO <- F_HONO[ -c(2,6,7,9,13,14)] 

#new df, subtract differences to form new column, HONO 
G_HONO <- mutate(F_HONO, HONO = NO2.x-NO2.y) 

hono <- subset(G_HONO, select = c("date_min", "HONO" , "NOx.x", "NO2.x", "cycle"))
nox <- subset(F_HONO, select = c("date_min", "NOx.y", "NO2.y", "NO.y"))
colnames(hono) <- c("date_min", "HONO", "NOx.star", "NO2.star", "cycle")
colnames(nox) <- c("date_min", "NOx.true", "NO2.true", "NO")

tp1 <- full_join(nox, hono, by = "date_min")

## tNr-NH3 ------------------------------------------------------------

F_NH3 <- merge(LI_TNR_2, LI_NH3_2, by = "date_min", all = TRUE)
# .x columns are TNr pathway, .y columns are NH3 pathway 

F_NH3$cycle = F_NH3$cycle.x  
F_NH3$cycle[!is.na(F_NH3$cycle.y)] = F_NH3$cycle.y[!is.na(F_NH3$cycle.y)] 

F_NH3 <- F_NH3[ -c(2,6,7,9,13,14)]

G_NH3 <- mutate(F_NH3, NH3 = NOx.x-NOx.y) 

#NH3 calculated using NOx

nh3 <- subset(G_NH3, select = c("date_min", "NH3", "cycle"))
tnr <- subset(F_NH3, select = c("date_min", "NOx.x"))
colnames(tnr) <- c("date_min", "tnr") 

tp2 <- full_join(tnr, nh3, by = "date_min") 

#if we want to offload specific data frames 
# write.csv(TD2,file = file.choose(new = T))

#recombine four datasets(NOx, HONO, tNr, NH3)
all <- full_join(tp1, tp2, by = "date_min")
all <- arrange(all, date_min)

all$cycle = all$cycle.x  
all$cycle[!is.na(all$cycle.y)] = all$cycle.y[!is.na(all$cycle.y)]  
all <- all[-c(8,11)]


# NO2 as true NO2, NO2.star is NO2 + Nr...etc 
all2 <- all %>%
  group_by(date = cut (date_min, breaks = "5 min")) %>%
  summarize(NOx.true = mean(NOx.true, na.rm = TRUE),
            NOx.star = mean(NOx.star, na.rm = TRUE),
            HONO = mean(HONO, na.rm = TRUE),
            NO2.true = mean(NO2.true, na.rm = TRUE),
            NO2.star = mean(NO2.star, na.rm = TRUE),
            NO = mean(NO, na.rm = TRUE),
            NH3 = mean(NH3, na.rm = TRUE),
            tnr = mean(tnr, na.rm = TRUE))

# GRAPHS ------------------------------------------------------------
summary(all)

# quick check with a scatter plot 
all %>%
  gather("key", "value", -date_min) %>%
  ggplot(aes (x = date_min,
              y = value,
              color = key)) + 
  # geom_line()  
  geom_point()

all.f %>%
  gather("key", "value", -date_min) %>%
  ggplot(aes (x = date_min,
              y = value,
              color = key)) + 
  # geom_line()  
  geom_point()

all2 %>%
  gather("key", "value", -date) %>%
  ggplot(aes (x = date,
              y = value,
              color = key)) + 
  geom_point()

NL_Nr2 %>%
  gather("key", "value", -date_min) %>%
  ggplot(aes (x = date_min,
              y = value,
              color = key)) + 
  geom_point()

#offload selective files 
# write.csv(NL_NH3, file = file.choose(new = T))

# FINAL DATA ------------------------------------------------------------
#saves the final processed data csv file in your directory 

#measured and interpolated data averaged to 1 min
outfile_final=(paste(sub('.csv', '', infile),"_1min_processed_final_data.csv",sep=""))
print(outfile_final)
write.csv(all, outfile_final, row.names=F)

#the measured and interpolated data is averaged to 5 min 
outfile_final2=(paste(sub('.csv', '', infile),"_5min_processed_final_data.csv",sep=""))
print(outfile_final)
write.csv(all2, outfile_final2, row.names=F)

#measured data with no interpolation, with rounded datetimes per min 
outfile_final3=(paste(sub('.csv', '', infile),"_1min_measurements_no_lin_interp.csv",sep=""))
print(outfile_final)
write.csv(NL_Nr2, outfile_final3, row.names=F)

#measured data with no interpolation, with the exact averaged datetime in 1 min 
outfile_final4=(paste(sub('.csv', '', infile),"_1min_ORIGINAL_datetime_measurements.csv",sep=""))
print(outfile_final4)
write.csv(O_Nr2, outfile_final4, row.names=F)


# Compile All ------------------------------------------------------------
# use this code if you want to compile all daily datasets into weekly 
library(readr)
library(gridExtra)
library(ggpubr)
library(lmodel2)

rm(list=ls(all=TRUE)) #clear all previous variables

setwd("C:/Users/folder_name_here")

# 1 MIN BIND ------------------------------------------------------------
#first 1 min interpolated and measured data 
data.path.1min="C:/Users/your_drive_name_here"

# load files into a list  
filenames.1min=list.files(path=data.path.1min, pattern = "Total*",full.names=TRUE)
d.1min <- rbindlist(lapply(filenames.1min, fread, header=T,blank.lines.skip=TRUE))

d.1min$date.m <- ymd_hms(d.1min$date_min)

## Measured only 1 min data ------------------------------------------------------------
data.path.1min.m="C:/Users/drive_name_here"

## Combine All ------------------------------------------------------------
filenames.1min.m=list.files(path=data.path.1min.m, pattern = "Total*",full.names=TRUE)
d.1min.m <- rbindlist(lapply(filenames.1min.m, fread, header=T,blank.lines.skip=TRUE))

d.1min.m$date <- ymd_hms(d.1min.m$date_min)

#select true NOx, NO2 star and tnr measurements

d.1min.meas <- select(d.1min.m, date, NOx.NO, NOx.NO2, NOx.NOx, HONO.NO2, tnr.NOx, nh3.NOx, vt)
colnames(d.1min.meas) <- c("date", "NO.true.meas", "NO2.true.meas", "NOx.true.meas", "NO2.star.meas", "tnr.meas", "tnr.nh3.meas", "Vt")

#combine 1 min files

tp1 <- cbind(d.1min.meas, d.1min)

all.1min <- select(tp1, -c("cycle"))

## quick graph check 
all.1min %>%
  gather("key", "value", -date) %>%
  ggplot(aes (x = date,
              y = value,
              color = key)) + 
  geom_point()

ggplot() +
  geom_point(data = all.1min, aes(x=date, y=tnr, colour = group), colour = "green")+
  geom_point(data = all.1min, aes(x=date, y=tnr.meas, colour = group), colour = "gold")+
  geom_point(data = all.1min, aes(x=date, y=NH3, colour = group), colour = "red")+
  geom_point(data = all.1min, aes(x=date, y=tnr.nh3.meas, colour = group), colour = "purple")+
  theme(legend.position = "right")+
  theme_light()

#write csv file

write.csv(all.1min, "tNr_processed_1_min_data.csv", row.names = F)

#5 MIN WEEKLY ------------------------------------------------------------
#5 min data interpolated and measured data 
#note no measurements only file for 5 min data
data.path.5min="drive_name_here"

# FINAL CSV ------------------------------------------------------------

filenames.5min=list.files(path=data.path.5min, pattern = "Total*",full.names=TRUE)
d.5min <- rbindlist(lapply(filenames.5min, fread, header=T,blank.lines.skip=TRUE))

d.5min$date <- ymd_hms(d.5min$date)

d.5min %>%
  gather("key", "value", -date) %>%
  ggplot(aes (x = date,
              y = value,
              color = key)) + 
  geom_point()

write.csv(d.5min, "5_min_averaged_data", row.names = F)
