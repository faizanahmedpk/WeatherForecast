rm(list = ls())

library(tidyverse)
#library(dplyr)
library(Rglimclim)

#help("Rglimclim-package", help_type = 'html')

load('BullooModInp.RData')
data(GLCdemo)



names(PRECIPITATION)
#year_max <- 2018
#year_min <- 1989 

Site_4Char <- c("T133", "T137", "T040", "T120","T000", "T001", "T002", "T003", "T004", "T007")
Station_10 <- STATION_NAMES[1:10]

# Rain Dataframe

PRECIPITATION_New <- PRECIPITATION %>% 
  rename(Year = year, Month = month, Day = day, 
         T133 = T36133, T137 = T36137, T040 = T44040, T120 = T44120, T000 = T45000, T001 = T45001,
         T002=T45002,   T003 = T45003, T004 = T45004, T007 = T45007) %>%
  gather(Site, Rain, T133:T007, factor_key = TRUE) %>% 
  select(Year, Month, Day , Site, Rain) #%>%
    #filter(PRECIPITATION$year == 1989)
  #filter(Year %in% c(1989:2018)) %>%
  #arrange(Month, Day)

PRECIPITATION_New <- PRECIPITATION_New %>% filter(Year == 1989)


PRECIPITATION_New$Year <- as.numeric(PRECIPITATION_New$Year)
PRECIPITATION_New$Month <- as.numeric(PRECIPITATION_New$Month)
PRECIPITATION_New$Day <- as.numeric(PRECIPITATION_New$Day)
head(PRECIPITATION_New)

str(PRECIPITATION_New)

# Site Dataframe
Precipitation_site <- CatStations %>% select(2:4,6) %>% slice(1:10) %>% 
  mutate(Region = ifelse(State == "QLD", 1, 2),
         Name = factor(Name)) 
Precipitation_site$State <- NULL   
row.names(Precipitation_site) <- Site_4Char

str(Precipitation_site)


Precipiation_region <- data.frame(Region = c(1,2), Name = factor(unique(CatStations$State)))
str(Precipiation_region)

# Writing data to correct formate

write.GLCdata(PRECIPITATION_New,file = 'PRECIPITATION.dat')


# Site information

Precipitation_site_info <- make.siteinfo(Precipitation_site, site.names = 1, region.col = 4,
                                         attr.names = c("Longitude", "Latitude") #,
                                         #regions = Precipiation_region
                                         )
Precipitation_site_info


ConstantModel


write.modeldef(ConstantModel,file = "Model0_Init.def")


Model0.Init <- read.modeldef("Model0_Init.def", model.type =  "logistic", siteinfo = Precipitation_site_info)
Model0.Init


Model0.fitted <- GLCfit("logistic", siteinfo = Precipitation_site_info, model.def = Model0.Init,
                        data.file = "PRECIPITATION.dat", diagnostics = 1, nprev.required = 0)
    