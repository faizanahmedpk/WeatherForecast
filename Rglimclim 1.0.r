rm(list = ls())

library(tidyverse)
library(Rglimclim)
#install.packages("Rglimclim")
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

PRECIPITATION_New <- PRECIPITATION_New %>% filter(Year %in% c(1989:2018)) %>% arrange(Month, Day)


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
                                         attr.names = c("Longitude", "Latitude") ,
                                         regions = Precipiation_region
)
Precipitation_site_info


ConstantModel


write.modeldef(ConstantModel,file = "Model0_Init.def")


Model0.Init <- read.modeldef("Model0_Init.def", model.type =  "logistic", siteinfo = Precipitation_site_info)
Model0.Init


Model0.fitted <- GLCfit("logistic", siteinfo = Precipitation_site_info, model.def = Model0.Init,
                        data.file = "PRECIPITATION.dat", diagnostics = 1, nprev.required = 0)


Model0.fitted
summary(Model0.fitted)


if(dev.cur() == 1) x11(width=8,height=6)
par(mfrow = c(2,2))
plot(Model0.fitted)
par(mfrow = c(1,1))


# Occerence model with seasonality

write.modeldef(Model0.fitted, file = "Model1_Init.def")



Model1.Init <- read.modeldef("Model1_Init.def",model.type="logistic", siteinfo=Precipitation_site_info)
Model1.Init

Model1.fitted <- GLCfit("logistics", siteinfo = Precipitation_site_info, model.def = Model1.Init, data.file = "PRECIPITATION.dat",
                        diagnostics = 1, nprev.required = 0)
Model1.fitted

#anova(Model0.fitted, Model1.fitted)

summary(Model1.fitted, tables = NULL)
par(mfrow = c(2,2))
plot(Model1.fitted, which.plots = 1:2)

#Rainfall occurrence — accounting for autocorrelation
write.modeldef(Model1.fitted,file="Model2_Init.def")

Model2.Init <- read.modeldef("Model2_Init.def",model.type="logistic", siteinfo=Precipitation_site_info ,var.names="Rainfall")
Model2.Init

Model2.fitted <- GLCfit("logistic", siteinfo = Precipitation_site_info , model.def = Model2.Init,data.file="PRECIPITATION.dat",
                        diagnostics=1,nprev.required=0)
Model2.fitted

#anova(Model2.fitted, Model1.fitted)

Model2a.fitted <- GLCfit("logistic", siteinfo = Precipitation_site_info , model.def = Model2.Init,data.file="PRECIPITATION.dat",
                         diagnostics=1,nprev.required=1)

# Running again and this time changing the value of nprev.required = 4

Model2a.fitted <- GLCfit("logistic", siteinfo = Precipitation_site_info , model.def = Model2.Init,data.file="PRECIPITATION.dat",
                         diagnostics=1,nprev.required=4)

write.modeldef(Model2a.fitted,file="Model2b_Init.def")

Model2b.Init <- read.modeldef("Model2b_Init.def",model.type="logistic", siteinfo=Precipitation_site_info ,var.names="Rainfall")
Model2b.Init

Model2b.fitted <- GLCfit("logistic",siteinfo=Precipitation_site_info, model.def=Model2b.Init,data.file="PRECIPITATION.dat",
                         diagnostics=1,nprev.required=4)
Model2b.fitted

#anova(Model2a.fitted, Model2b.fitted)

write.modeldef(Model2b.fitted,file="Model2c_Init.def")

Model2c.Init <- read.modeldef("Model2c_Init.def",model.type="logistic", siteinfo= Precipitation_site_info,var.names="Rainfall",
                              oldGlimClim.warning=FALSE)


Model2c.Init
#---
Model2c.fitted <- GLCfit("logistic",siteinfo=Precipitation_site_info,model.def=Model2c.Init,data.file="PRECIPITATION.dat",
                         diagnostics=1,nprev.required=4)

write.modeldef(Model2c.fitted,file="Model2d_Init.def")

Model2d.Init <- read.modeldef("Model2d_Init.def",model.type="logistic",
                              siteinfo=Precipitation_site_info,var.names="Rainfall",
                              oldGlimClim.warning=FALSE)
Model2d.Init

Model2d.fitted <- GLCfit("logistic",siteinfo=Precipitation_site_info,
                         model.def=Model2d.Init,data.file="PRECIPITATION.dat",
                         diagnostics=1,nprev.required=4)


#anova(Model2a.fitted,Model2b.fitted,Model2c.fitted,Model2d.fitted)


Model3.fitted <- GLCfit("logistic",siteinfo=Precipitation_site_info,
                        model.def=Model2c.Init,data.file="PRECIPITATION.dat",
                        diagnostics=1,nprev.required=3)

summary(Model3.fitted,tables=NULL)

plot(Model3.fitted, which.plots = 1:2)


# Rainfall Occerence - Interection

write.modeldef(Model3.fitted,"Model4_Init.def")


Model4.Init <- read.modeldef("Model4_Init.def",model.type="logistic",siteinfo=Precipitation_site_info,var.names="Rainfall",
                             oldGlimClim.warning=FALSE)

Model4.Init

Model4.fitted <- GLCfit("logistic",siteinfo=Precipitation_site_info,model.def=Model4.Init,data.file="PRECIPITATION.dat",
                        diagnostics=1,nprev.required=3)
Model4.fitted

anova(Model3.fitted,Model4.fitted)

Model4.fitted


# RainFall Occerence --- Site Effects

par(mfrow = c(1,1))
plot(Model4.fitted, which.plots = 3)


write.modeldef(Model4.fitted, file = "Model5_Init.def")


min(Precipitation_site$lat)
max(Precipitation_site$lat)
min(Precipitation_site$lon)
max(Precipitation_site$lon)


Model5.Init <- read.modeldef("Model5_Init.def",model.type="logistic",
                             siteinfo=Precipitation_site_info,var.names="Rainfall",
                             oldGlimClim.warning=FALSE)


Model5.Init


Model5.fitted <- GLCfit("logistic",siteinfo=Precipitation_site_info,
                        model.def=Model5.Init,data.file="PRECIPITATION.dat",
                        diagnostics=1,nprev.required=3)


#anova(Model5.fitted,Model4.fitted)

# Rainfall Occerence - Intersite Dependence

write.modeldef(Model5.fitted,file="Model6_Init.def")


Model6.Init <- read.modeldef("Model6_Init.def",model.type="logistic",
                             siteinfo=Precipitation_site_info,var.names="Rainfall",
                             oldGlimClim.warning=FALSE)
Model6.Init

Model6.fitted <- GLCfit("logistic",siteinfo=Precipitation_site_info,
                        model.def=Model6.Init,data.file="PRECIPITATION.dat",
                        diagnostics=1,nprev.required=3)



# Modeling Rainfal INtensity
Ashdown.IntensityModel.Initial


Intensity.fitted <- GLCfit("gamma",siteinfo=Precipitation_site_info,
                           model.def=Ashdown.IntensityModel.Initial,
                           data.file="PRECIPITATION.dat",nprev.required=0,
                           diagnostics=2,
                           cor.file="IntensityCorrelations.dat",
                           resid.file="IntensityResids.dat")
















