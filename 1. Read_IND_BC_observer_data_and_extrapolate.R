# Load the necessary libraries
library(ggplot2)
library(dplyr)
library(gridExtra)
library(grid)
library(tidyr)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(rgdal)
library(rgeos)
library(maps)
library(sf)

rm(list=ls())

setwd("C:/Use/OneDrive - AZTI/1. Tesis/5. Data/7. Bycatch documents & data")

# Load the data
load("C:/Use/OneDrive - AZTI/1. Tesis/5. Data/7. Bycatch documents & data/obs.data_IO_AO_2003_2023_enviadoROGER18062024.RData")

# Change the name
datos <- azti

summary(datos)

length(unique(datos$set_id))

names(datos)
dim(datos)

######################################################
# SELECT ONLY INDIAN OCEAN DATA
head(datos)
unique(datos$ocean)

datos_IO<-datos %>% filter(ocean=="Indian")
summary(datos_IO)

######################################################
# SELECT Spain and Seychelles (discard Belize and Curacao observations)
head(datos_IO)
unique(datos_IO$flag_country)

datos_IO_ES<-datos_IO %>% filter(flag_country=="Spain" | flag_country=="Seychelles")
summary(datos_IO_ES)

######################################################

##### We have to filter the observations that occur inside our model area #####

### Changing the names of the variables

##################################################
#read shapefile
##################################################

ecoregions<-readOGR("Final_area_for_calculations.shp", layer="Final_area_for_calculations")
class(ecoregions) # see how it is a "SpatialPolygonsDataFrame"

### plot ecoregions and points
world <- ne_countries(scale = "medium", returnclass = "sf")
class(world)

##################################################
# Visualizing which points fall over land
##################################################

ggplot(data = world) + geom_polygon(data = ecoregions, aes(x = long, y = lat, group = group), colour = "red", 
                                    fill = NA) +geom_point(data=datos_IO_ieo,aes(x=longitude, y=latitude)) 
# notice how some points are OUTSIDE the ecoregions

##################################################
# Transforming and merging rised cath and the ecorregions
##################################################

### To merge the Rised Catch and Ecoregion_they need to be the same class, and have the same Coordinates System.

proj4string(ecoregions) #"+proj=longlat +datum=WGS84 +no_defs
proj4string(datos_IO) # this is a dataframe, and below we convert it to a "SpatialPointsDataFrame"

datos_IO$longitude2<-datos_IO$longitude #just making a copy
datos_IO$latitude2<-datos_IO$latitude #just making a copy

coordinates(datos_IO) <- ~longitude2 + latitude2 # converts DATOS into a "SpatialPointsDataFrame"
class(datos_IO) # see how now it is "SpatialPointsDataFrame"
proj4string(datos_IO) #still not coordiantes systems 

proj4string(datos_IO)<-CRS("+proj=longlat +datum=WGS84 +no_defs") #this assigs the coordinate systems
proj4string(datos_IO) 

datos_IO<-spTransform(datos_IO,CRS(proj4string(ecoregions)))

identical(proj4string(datos_IO),proj4string(ecoregions)) # We want to see here TRUE

##################################################
# Find overlaping and adding extra column to the database
##################################################

# Now we find the overlap between the points and poligons of the shapefiles so we can add the extra columns with
# ecoregion information into the database 

# Here we do the spatial overlap between points and polygons
points_with_ecoregions <- over(datos_IO, ecoregions)

class(points_with_ecoregions)
head(points_with_ecoregions) #we still need to add the data attributes from the ecoregion shapefiles to the datapoint file. 
datos_IO$Ecoregion_name <- points_with_ecoregions$Ecoregion #important line
datos_IO$Ecoregion_ID <- points_with_ecoregions$Region_ID #important line


datos_IO$Ecoregion_name<-as.factor(datos_IO$Ecoregion_name)
datos_IO$Ecoregion_ID<-as.factor(datos_IO$Ecoregion_ID)
class(datos_IO)
summary(datos_IO)
levels(datos_IO$Ecoregion_name)

class(datos_IO) # still it is a "SpatialPointsDataFrame"
dim(datos_IO)

datos_IO<-as.data.frame(datos_IO) #convert back to data.frame before saving it to csv
head(datos_IO)

### making sure the asigments have been done correct!
ggplot(data = world) +
  geom_polygon(data = ecoregions, aes(x = long, y = lat, group = group), colour = "grey10", fill = NA)+
  geom_sf(color = "darkgrey", fill = "lightgrey")+
  coord_sf(xlim = c(10,160), ylim = c(40,-60), expand = FALSE)+ 
  geom_point(data=datos_IO_ieo, size=3, aes(longitude,latitude, col=Ecoregion_name)) + 
  ggtitle("Point assignation by ecorregion")

### Filter only the points of the TIO model area

datos_IO<-datos_IO %>% filter(Ecoregion_name=="Model_area")

######
# save the dataset with the ecoregion info added at the end. Once you do this once, 
# you dont´t need to do it anymore and just use this file for analysis
######
write.csv(datos_IO_ieo,file="Bycatch_AZTI_database_ESP_SYC_TIO_area.csv") # SAVE FILE


###################################################### 
############################ 
datos_IO_ES <- read.csv("Bycatch_AZTI_database_ESP_SYC_TIO_area.csv", sep=",")

datos_bony_fishes <- datos_IO_ES %>% filter(species_group=="Other bony fishes")

unique(datos_bony_fishes$scientific_name)
unique(datos_bony_fishes$fao_code)


### Filtramos solo las columnas que nos interesan 

datos_IO_ES <- datos_IO_ES[,c(2:7,9,15,16,17,23,33,37,39)]

######################################################
unique(datos_IO_ES$species_group)

### Differentiate the Rays species group into Rays and Mobulas
datos_IO_ES$species_group <- ifelse(datos_IO_ES$species_group == "Rays" & 
                                      datos_IO_ES$scientific_name %in% c("Rajiformes", "Pteroplatytrygon violacea", "Myliobatis aquila",
                                                                         "Dasyatidae", "Aetobatus narinari", "Rhinopteridae", "Rhina ancylostoma"), 
                                    "Rays", datos_IO_ES$species_group)

datos_IO_ES$species_group <- ifelse(datos_IO_ES$species_group == "Rays" & 
                                      datos_IO_ES$scientific_name %in% c("Mobula mobular", "Mobula tarapacana", "Mobulidae",
                                                                         "Mobula spp", "Manta spp", "Mobula japanica", "Mobula birostris"), 
                                    "Mobulas", datos_IO_ES$species_group)

#MAKE EXPLORATORY PLOT - to detect outliers

boxplot(data=datos_IO_ES[datos_IO_ES$species_group=="Billfishes"& !is.na (datos_IO_ES$weight_tons),], weight_tons~factor(fao_code), main="Billfishes total weight by set",las=2 )

boxplot(data=datos_IO_ES[datos_IO_ES$species_group=="Cetaceans"& !is.na (datos_IO_ES$weight_tons),], weight_tons~factor(fao_code), main="Cetaceans total weight by set" ,las=2 )

boxplot(data=datos_IO_ES[datos_IO_ES$species_group=="Other bony fishes"& !is.na (datos_IO_ES$weight_tons),], weight_tons~factor(fao_code), main="Bony fishes total weight by set" ,las=2 )

boxplot(data=datos_IO_ES[datos_IO_ES$species_group=="Sharks"& !is.na (datos_IO_ES$weight_tons),], weight_tons~factor(fao_code), main="Sharks total weight by set",las=2  )

boxplot(data=datos_IO_ES[datos_IO_ES$species_group=="Tunas nei"& !is.na (datos_IO_ES$weight_tons),], weight_tons~factor(fao_code), main="Tunas total weight by set",las=2  )

boxplot(data=datos_IO_ES[datos_IO_ES$species_group=="Turtles"& !is.na (datos_IO_ES$weight_tons),], weight_tons~factor(fao_code), main="Turtles total weight by set" ,las=2 )

boxplot(data=datos_IO_ES[datos_IO_ES$species_group=="Rays"& !is.na (datos_IO_ES$weight_tons),], weight_tons~factor(fao_code), main="Rays total weight by set" ,las=2 )

boxplot(data=datos_IO_ES[datos_IO_ES$species_group=="Mobulas"& !is.na (datos_IO_ES$weight_tons),], weight_tons~factor(fao_code), main="Mobulas total weight by set" ,las=2 )

boxplot(data=datos_IO_ES[datos_IO_ES$species_group=="Whales shark"& !is.na (datos_IO_ES$weight_tons),], weight_tons~factor(fao_code), main="Whale shark total weight by set" ,las=2 )


#######Boxplot con numeros totales por lance
###########################################
boxplot(data=datos_IO_ES[datos_IO_ES$species_group=="Billfishes"& !is.na (datos_IO_ES$number),], number~factor(fao_code), main="Billfishes total individuals by set",las=2 )
boxplot(data=datos_IO_ES[datos_IO_ES$species_group=="Cetaceans"& !is.na (datos_IO_ES$number),], number~factor(fao_code), main="Cetaceans total individuals by set" ,las=2 )
boxplot(data=datos_IO_ES[datos_IO_ES$species_group=="Other bony fishes"& !is.na (datos_IO_ES$number),], number~factor(fao_code), main="Bony fishes total individuals by set" ,las=2 )
boxplot(data=datos_IO_ES[datos_IO_ES$species_group=="Sharks"& !is.na (datos_IO_ES$number),], number~factor(fao_code), main="Sharks total individuals by set",las=2  )
boxplot(data=datos_IO_ES[datos_IO_ES$species_group=="Tunas nei"& !is.na (datos_IO_ES$number),], number~factor(fao_code), main="Tunas total individuals by set",las=2  )
boxplot(data=datos_IO_ES[datos_IO_ES$species_group=="Turtles"& !is.na (datos_IO_ES$number),], number~factor(fao_code), main="Turtles total individuals by set" ,las=2 )
boxplot(data=datos_IO_ES[datos_IO_ES$species_group=="Rays"& !is.na (datos_IO_ES$number),], number~factor(fao_code), main="Rays total individuals by set" ,las=2 )
boxplot(data=datos_IO_ES[datos_IO_ES$species_group=="Mobulas"& !is.na (datos_IO_ES$number),], number~factor(fao_code), main="Mobulas total individuals by set" ,las=2 )
boxplot(data=datos_IO_ES[datos_IO_ES$species_group=="Whales shark"& !is.na (datos_IO_ES$number),], number~factor(fao_code), main="Whale shark total individuals by set" ,las=2 )
######################################################

######################################################
# REMOVE whale shark and cetaceans
# Here we remove Cetaceans, NA, and Unknown, as they do not kill any of this species when setting the net

datos_IO_ES_noCE<-datos_IO_ES %>% filter(species_group!="Cetaceans" & species_group!="NA" & species_group!="Unknown")

unique(datos_IO_ES_noCE$species_group)

length(unique(datos_IO_ES_noCE$set_id))

######################################################
# REMOVE outliers
# For bony fishes if wt is >35t
# For tunas if nº is >300000

# Create two data frames with the limits of the species
species_limits_nº <- data.frame(
  species_group = c("Tunas nei"),
  nºlimit = c(300000)
)

species_limits_wt <- data.frame(
  species_group = c("Other bony fishes"),
  wt_limit = c(35)
)

# Create two columns at the end of the data frame that add the limit to the species group according to the limits
# that we set before
clean_datos_IO_ES <- datos_IO_ES_noCE %>%
  left_join(species_limits_nº, by = "species_group") %>%
  left_join(species_limits_wt, by = "species_group")

# Create two columns that will tell by TRUE or FALSE if the register (line) must be deleted. 
# The NA that other groups have in this columns are treted as FALSE, so they aren't deleted. 
clean_datos_IO_ES <- clean_datos_IO_ES %>%
  mutate(delete_nº = coalesce(ifelse(is.na(nºlimit) | number <= nºlimit, FALSE, TRUE), FALSE),
         delete_wt = coalesce(ifelse(is.na(wt_limit) | weight_tons <= wt_limit, FALSE, TRUE), FALSE))

# Filtrar los registros
clean_datos_IO_ES <- clean_datos_IO_ES %>%
  filter(!delete_nº & !delete_wt) %>%
  select(-nºlimit, -wt_limit, -delete_nº, -delete_wt)

#MAKE AGAIN EXPLORATORY PLOT - without outliers
boxplot(data=clean_datos_IO_ES[clean_datos_IO_ES$species_group=="Billfishes"& !is.na (clean_datos_IO_ES$weight_tons),], weight_tons~factor(fao_code), main="Billfishes total weight by set",las=2 )

boxplot(data=clean_datos_IO_ES[clean_datos_IO_ES$species_group=="Other bony fishes"& !is.na (clean_datos_IO_ES$weight_tons),], weight_tons~factor(fao_code), main="Bony fishes total weight by set" ,las=2 )

boxplot(data=clean_datos_IO_ES[clean_datos_IO_ES$species_group=="Sharks"& !is.na (clean_datos_IO_ES$weight_tons),], weight_tons~factor(fao_code), main="Sharks total weight by set",las=2  )

boxplot(data=clean_datos_IO_ES[clean_datos_IO_ES$species_group=="Tunas nei"& !is.na (clean_datos_IO_ES$weight_tons),], weight_tons~factor(fao_code), main="Tunas total weight by set",las=2  )

boxplot(data=clean_datos_IO_ES[clean_datos_IO_ES$species_group=="Turtles"& !is.na (clean_datos_IO_ES$weight_tons),], weight_tons~factor(fao_code), main="Turtles total weight by set" ,las=2 )

boxplot(data=clean_datos_IO_ES[clean_datos_IO_ES$species_group=="Rays"& !is.na (clean_datos_IO_ES$weight_tons),], weight_tons~factor(fao_code), main="Rays total weight by set" ,las=2 )

boxplot(data=clean_datos_IO_ES[clean_datos_IO_ES$species_group=="Mobulas"& !is.na (clean_datos_IO_ES$weight_tons),], weight_tons~factor(fao_code), main="Mobulas total weight by set" ,las=2 )

boxplot(data=clean_datos_IO_ES[clean_datos_IO_ES$species_group=="Whales shark"& !is.na (clean_datos_IO_ES$weight_tons),], weight_tons~factor(fao_code), main="Whale shark total weight by set" ,las=2 )


#######Boxplot con numeros totales por lance
###########################################
boxplot(data=clean_datos_IO_ES[clean_datos_IO_ES$species_group=="Billfishes"& !is.na (clean_datos_IO_ES$number),], number~factor(fao_code), main="Billfishes total individuals by set",las=2 )
boxplot(data=clean_datos_IO_ES[clean_datos_IO_ES$species_group=="Other bony fishes"& !is.na (clean_datos_IO_ES$number),], number~factor(fao_code), main="Bony fishes total individuals by set" ,las=2 )
boxplot(data=clean_datos_IO_ES[clean_datos_IO_ES$species_group=="Sharks"& !is.na (clean_datos_IO_ES$number),], number~factor(fao_code), main="Sharks total individuals by set",las=2  )
boxplot(data=clean_datos_IO_ES[clean_datos_IO_ES$species_group=="Tunas nei"& !is.na (clean_datos_IO_ES$number),], number~factor(fao_code), main="Tunas total individuals by set",las=2  )
boxplot(data=clean_datos_IO_ES[clean_datos_IO_ES$species_group=="Turtles"& !is.na (clean_datos_IO_ES$number),], number~factor(fao_code), main="Turtles total individuals by set" ,las=2 )
boxplot(data=clean_datos_IO_ES[clean_datos_IO_ES$species_group=="Rays"& !is.na (clean_datos_IO_ES$number),], number~factor(fao_code), main="Rays total individuals by set" ,las=2 )
boxplot(data=clean_datos_IO_ES[clean_datos_IO_ES$species_group=="Mobulas"& !is.na (clean_datos_IO_ES$number),], number~factor(fao_code), main="Mobulas total individuals by set" ,las=2 )
boxplot(data=clean_datos_IO_ES[clean_datos_IO_ES$species_group=="Whales shark"& !is.na (clean_datos_IO_ES$number),], number~factor(fao_code), main="Whale shark total individuals by set" ,las=2 )
######################################################


### plot number of sets over time by school type
length(unique(clean_datos_IO_ES$set_id))
length(unique(clean_datos_IO_ES$trip_id))

mydf2<-clean_datos_IO_ES %>% group_by(year) %>% summarize (ntrips=n_distinct(trip_id),nºsets=n_distinct(set_id),nobservations=n())

mydf<-clean_datos_IO_ES %>% group_by(year,school_type) %>% summarize (ntrips=n_distinct(trip_id),nºsets=n_distinct(set_id),nobservations=n())

grid.table(mydf)

ggplot(mydf, aes(x=year, y=nºsets, fill=school_type)) +
  geom_bar(stat='identity', position='dodge')+
labs(title = "Nº of sets by type and year by the Spanish PS fleet", x = "Year", y = "Nº of sets") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 18)
  )
summary(clean_datos_IO_ES)
summary(clean_datos_IO_ES$species_group)
summary(clean_datos_IO_ES$weight_tons)
summary(clean_datos_IO_ES$destiny)
summary(as.factor(clean_datos_IO_ES$destiny_code)) ## it has many NaNs, but not destiny

length(unique(clean_datos_IO_ES$scientific_name))
length(unique(clean_datos_IO_ES$fao_code))

# Delete the registers where destiny is Others, as we don't know if they are discarded or retained
# They are less than 1% (~0.81%) of the registers
others<-clean_datos_IO_ES %>% filter(destiny_code==9 | destiny_code=="NA" | weight_tons=="NA")
clean_datos_IO_ES<-clean_datos_IO_ES %>% filter(destiny_code!=9 & destiny_code!="NA" & weight_tons!="NA")

######################################################
#EXTRAPOLATION ANALYSIS
######################################################

#STEPS for EXTRAPOLATION
#1) Select what they call bycatch. Separate the 3 tropicals (SKJ, BET and YFT) from the destiny ="retained" from the rest. So we can calculate TOTAL Production

#2) Prepare the data by strata: quarter and fishing mode

#3) Get the total production from LOGBOOKS (SKJ, BET and YFT)

#4) make extrapolation

#############-----------------------
#1) Select what they call bycatch. Separate the 3 tropicals (SKJ, BET and YFT) from the destiny ="retained"
# from the rest so can calculate TOTAL Production by the SPANISH fleet

#target catch
target<-subset(clean_datos_IO_ES, (clean_datos_IO_ES$fao_code=="SKJ" |clean_datos_IO_ES$fao_code=="YFT"|clean_datos_IO_ES$fao_code=="BET") & (clean_datos_IO_ES$destiny_code==("15")|clean_datos_IO_ES$destiny_code==("6")|clean_datos_IO_ES$destiny_code==("8")) & clean_datos_IO_ES$flag_country==("Spain"))

# check it worked properly
unique(target$fao_code)
unique(target$destiny_code)
unique(target$destiny)

#bycatch
bycatch<-subset(clean_datos_IO_ES, (clean_datos_IO_ES$fao_code=="SKJ" |clean_datos_IO_ES$fao_code=="YFT"|clean_datos_IO_ES$fao_code=="BET") & (clean_datos_IO_ES$destiny_code==("4")|clean_datos_IO_ES$destiny_code==("5")|clean_datos_IO_ES$destiny_code==("14")|clean_datos_IO_ES$destiny_code==("7")) & clean_datos_IO_ES$flag_country==("Spain"))

# check it worked properly
unique(bycatch$fao_code)
unique(bycatch$destiny_code)
unique(bycatch$destiny)

# We add a column to the data frame defining if the register is TARGET or BYCATCH 
clean_datos_IO_ES["Catch"] <- ifelse((clean_datos_IO_ES$destiny_code==("15")
                                    |clean_datos_IO_ES$destiny_code==("6")|
                                      clean_datos_IO_ES$destiny_code==("8")), "retained", "discarded")

# We also add a column to the data frame defining if the register is discarded ALIVE or DEAD 
clean_datos_IO_ES["End"] <- ifelse(clean_datos_IO_ES$Catch=="discarded" & (clean_datos_IO_ES$destiny_code==("4")
                                      |clean_datos_IO_ES$destiny_code==("2")|
                                        clean_datos_IO_ES$destiny_code==("1")|
                                        clean_datos_IO_ES$destiny_code==("14")), "alive", "dead")

write.csv(clean_datos_IO_ES, file="Data_bycatch_ESP_SYC_TIO_area.csv")

#MAKE AGAIN EXPLORATORY PLOTS (only for the Spanish fleet)

########### Destiny of the catch of targeted species
TT_catch <- subset(clean_datos_IO_ES, (clean_datos_IO_ES$fao_code=="SKJ" |clean_datos_IO_ES$fao_code=="YFT"|clean_datos_IO_ES$fao_code=="BET") & clean_datos_IO_ES$flag_country=="Spain")

ggplot(TT_catch, aes(x = year, y = weight_tons, fill = Catch)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(x = "Year", y = "Weight (tons)", fill = "Destiny", title = "Destiny of the catch of tunas by the Spanish fleet" ) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 18)
  )

ggplot(TT_catch, aes(x = factor(year), y = weight_tons, fill = Catch)) +
  geom_bar(stat = "identity", position = "fill") +
  labs(title = "Destiny of the catch of tunas by the Spanish fleet", x = "Year", y = "Percentage", fill = "Destiny") +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 20)
  )


TT_discards <- subset(clean_datos_IO_ES, (clean_datos_IO_ES$fao_code=="SKJ" |clean_datos_IO_ES$fao_code=="YFT"|clean_datos_IO_ES$fao_code=="BET") & clean_datos_IO_ES$Catch=="discarded" & clean_datos_IO_ES$flag_country=="Spain")

ggplot(TT_discards, aes(x = year, y = weight_tons, fill = End)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(x = "Year", y = "Weight (tons)", fill = "State", title = "State of the discarded tunas by the Spanish fleet" ) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 18)
  )


TT_set <- subset(clean_datos_IO_ES, (clean_datos_IO_ES$fao_code=="SKJ" |clean_datos_IO_ES$fao_code=="YFT"|clean_datos_IO_ES$fao_code=="BET")& clean_datos_IO_ES$flag_country=="Spain")

ggplot(TT_set, aes(x = year, y = weight_tons, fill = school_type)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(x = "Year", y = "Weight (tons)", fill = "School type", title = "Catch of tunas by school type by the Spanish fleet" ) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 18)
  )


########### Bony fishes
bony_fishes_catch <- subset(clean_datos_IO_ES, (clean_datos_IO_ES$species_group=="Other bony fishes") & clean_datos_IO_ES$flag_country=="Spain") 

########### Retained catches of non-targeted species
ggplot(bony_fishes_catch, aes(x = year, y = weight_tons, fill = Catch)) +
  geom_bar(stat = "identity", position = "stack") +
  #scale_fill_manual(values = c("BET" = "red", "SKJ" = "orange", "YFT" = "yellow"))+
  labs(x = "Year", y = "Weight (tons)", fill = "Destiny", title = "Destiny of the catch of non-targeted bony fishes by the Spanish fleet") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 18)
  )

ggplot(bony_fishes_catch, aes(x = factor(year), y = weight_tons, fill = Catch)) +
  geom_bar(stat = "identity", position = "fill") +
  labs(title = "Destiny of the catch of non-targeted bony fishes by the Spanish fleet", x = "Year", y = "Percentage", fill = "Destiny") +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 20)
  )

bony_fishes_discards <- subset(clean_datos_IO_ES, clean_datos_IO_ES$species_group=="Other bony fishes" & clean_datos_IO_ES$Catch=="discarded" & clean_datos_IO_ES$flag_country=="Spain")

ggplot(bony_fishes_discards, aes(x = year, y = weight_tons, fill = End)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(x = "Year", y = "Weight (tons)", fill = "State", title = "State of the discarded bony fishes by the Spanish fleet" ) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 18)
  )

ggplot(bony_fishes_catch, aes(x = year, y = weight_tons, fill = school_type)) +
  geom_bar(stat = "identity", position = "stack") +
  #scale_fill_manual(values = c("BET" = "red", "SKJ" = "orange", "YFT" = "yellow"))+
  labs(x = "Year", y = "Weight (tons)", fill = "School type", title = "Catch of bony fishes by school type by the Spanish fleet") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 18)
  )

########### Sharks
sharks <- subset(clean_datos_IO_ES, (clean_datos_IO_ES$species_group=="Sharks") & clean_datos_IO_ES$flag_country=="Spain") 

########### Retained catches of non-targeted species
ggplot(sharks, aes(x = year, y = weight_tons, fill = Catch)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(x = "Year", y = "Weight (tons)", fill = "Destiny", title = "Destiny of the catch of sharks by the Spanish fleet") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 18)
  )

ggplot(sharks, aes(x = factor(year), y = weight_tons, fill = Catch)) +
  geom_bar(stat = "identity", position = "fill") +
  labs(title = "Destiny of the catch of sharks by the Spanish fleet", x = "Year", y = "Percentage", fill = "Destiny") +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 20)
  )


sharks_discards <- subset(clean_datos_IO_ES, clean_datos_IO_ES$species_group=="Sharks" & clean_datos_IO_ES$Catch=="discarded"& clean_datos_IO_ES$flag_country=="Spain")

ggplot(sharks_discards, aes(x = year, y = weight_tons, fill = End)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(x = "Year", y = "Weight (tons)", fill = "State", title = "State of the discarded sharks by the Spanish fleet" ) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 18)
  )

ggplot(sharks, aes(x = year, y = weight_tons, fill = school_type)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(x = "Year", y = "Weight (tons)", fill = "School_type", title = "Catch of sharks by school type by the Spanish fleet") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 18)
  )

########### Rays
rays <- subset(clean_datos_IO_ES, (clean_datos_IO_ES$species_group=="Rays")& clean_datos_IO_ES$flag_country=="Spain") 

########### Retained catches of non-targeted species
ggplot(rays, aes(x = year, y = weight_tons, fill = Catch)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(x = "Year", y = "Weight (tons)", fill = "Destiny", title = "Destiny of the catch of rays by the Spanish fleet") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 18)
  )

ggplot(rays, aes(x = factor(year), y = weight_tons, fill = Catch)) +
  geom_bar(stat = "identity", position = "fill") +
  labs(title = "Destiny of the catch of rays by the Spanish fleet", x = "Year", y = "Percentage", fill = "Destiny") +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 20)
  )

rays_discards <- subset(clean_datos_IO_ES, clean_datos_IO_ES$species_group=="Rays" & clean_datos_IO_ES$Catch=="discarded" & clean_datos_IO_ES$flag_country=="Spain")

ggplot(rays_discards, aes(x = year, y = weight_tons, fill = End)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(x = "Year", y = "Weight (tons)", fill = "State", title = "State of the discarded rays by the Spanish fleet" ) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 18)
  )

ggplot(rays, aes(x = year, y = weight_tons, fill = school_type)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(x = "Year", y = "Weight (tons)", fill = "School_type", title = "Catch of rays by school type by the Spanish fleet") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 18)
  )

########### Mobulas
mobulas <- subset(clean_datos_IO_ES, (clean_datos_IO_ES$species_group=="Mobulas")& clean_datos_IO_ES$flag_country=="Spain") 

########### Retained catches of non-targeted species
ggplot(mobulas, aes(x = year, y = weight_tons, fill = Catch)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(x = "Year", y = "Weight (tons)", fill = "Destiny", title = "Destiny of the catch of mobulas by the Spanish fleet") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 18)
  )

ggplot(mobulas, aes(x = factor(year), y = weight_tons, fill = Catch)) +
  geom_bar(stat = "identity", position = "fill") +
  labs(title = "Destiny of the catch of mobulas by the Spanish fleet", x = "Year", y = "Percentage", fill = "Destiny") +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 20)
  )

mobulas_discards <- subset(clean_datos_IO_ES, clean_datos_IO_ES$species_group=="Mobulas" & clean_datos_IO_ES$Catch=="discarded" & clean_datos_IO_ES$flag_country=="Spain")

ggplot(mobulas_discards, aes(x = year, y = weight_tons, fill = End)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(x = "Year", y = "Weight (tons)", fill = "State", title = "State of the discarded mobulas by the Spanish fleet" ) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 18)
  )

ggplot(mobulas, aes(x = year, y = weight_tons, fill = school_type)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(x = "Year", y = "Weight (tons)", fill = "School_type", title = "Catch of mobulas by school type by the Spanish fleet") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 18)
  )

########### Billfishes
billfishes <- subset(clean_datos_IO_ES, (clean_datos_IO_ES$species_group=="Billfishes")& clean_datos_IO_ES$flag_country=="Spain") 

########### Retained catches of non-targeted species
ggplot(billfishes, aes(x = year, y = weight_tons, fill = Catch)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(x = "Year", y = "Weight (tons)", fill = "Destiny", title = "Destiny of the catch of billfishes by the Spanish fleet") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 18)
  )

ggplot(billfishes, aes(x = factor(year), y = weight_tons, fill = Catch)) +
  geom_bar(stat = "identity", position = "fill") +
  labs(title = "Destiny of the catch of billfishes by the Spanish fleet", x = "Year", y = "Percentage", fill = "Destiny") +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 20)
  )

billfishes_discards <- subset(clean_datos_IO_ES, clean_datos_IO_ES$species_group=="Billfishes" & clean_datos_IO_ES$Catch=="discarded" & clean_datos_IO_ES$flag_country=="Spain")

ggplot(billfishes_discards, aes(x = year, y = weight_tons, fill = End)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(x = "Year", y = "Weight (tons)", fill = "State", title = "State of the discarded billfishes by the Spanish fleet" ) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 18)
  )

ggplot(billfishes, aes(x = year, y = weight_tons, fill = school_type)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(x = "Year", y = "Weight (tons)", fill = "School_type", title = "Catch of billfishes by school type by the Spanish fleet") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 18)
  )

########### Turtles
turtles <- subset(clean_datos_IO_ES, (clean_datos_IO_ES$species_group=="Turtles") & clean_datos_IO_ES$flag_country=="Spain") 

########### Retained catches of non-targeted species
ggplot(turtles, aes(x = year, y = weight_tons, fill = Catch)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(x = "Year", y = "Weight (tons)", fill = "Destiny", title = "Destiny of the catch of turtles by the Spanish fleet") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 18)
  )

ggplot(turtles, aes(x = factor(year), y = weight_tons, fill = Catch)) +
  geom_bar(stat = "identity", position = "fill") +
  labs(title = "Destiny of the catch of turtles by the Spanish fleet", x = "Year", y = "Percentage", fill = "Destiny") +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 20)
  )

turtles_discards <- subset(clean_datos_IO_ES, clean_datos_IO_ES$species_group=="Turtles" & clean_datos_IO_ES$Catch=="discarded" & clean_datos_IO_ES$flag_country=="Spain")

ggplot(turtles_discards, aes(x = year, y = weight_tons, fill = End)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(x = "Year", y = "Weight (tons)", fill = "State", title = "State of the discarded turtles by the Spanish fleet" ) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 18)
  )

ggplot(turtles, aes(x = year, y = weight_tons, fill = school_type)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(x = "Year", y = "Weight (tons)", fill = "School_type", title = "Catch of turtles by school type by the Spanish fleet") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 18)
  )
########### Whale shark
ws <- subset(clean_datos_IO_ES, (clean_datos_IO_ES$species_group=="Whales shark") & clean_datos_IO_ES$flag_country=="Spain") 

########### Retained catches of non-targeted species
ggplot(ws, aes(x = year, y = weight_tons, fill = Catch)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(x = "Year", y = "Weight (tons)", fill = "Destiny", title = "Destiny of the catch of whale sharks by the spanish fleet") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 18)
  )

ggplot(ws, aes(x = factor(year), y = weight_tons, fill = Catch)) +
  geom_bar(stat = "identity", position = "fill") +
  labs(title = "Destiny of the catch of whale shark by the Spanish fleet", x = "Year", y = "Percentage", fill = "Destiny") +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 20)
  )

ws_discards <- subset(clean_datos_IO_ES, clean_datos_IO_ES$species_group=="Whales shark" & clean_datos_IO_ES$Catch=="discarded" & clean_datos_IO_ES$flag_country=="Spain")

ggplot(ws_discards, aes(x = year, y = weight_tons, fill = End)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(x = "Year", y = "Weight (tons)", fill = "State", title = "State of the discarded whale sharks by the Spanish fleet" ) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 18)
  )

ggplot(ws, aes(x = year, y = weight_tons, fill = school_type)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(x = "Year", y = "Weight (tons)", fill = "School_type", title = "Catch of whale sharks by school type by the Spanish fleet") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 18)
  )


#############-----------------------
#2) Prepare the data by year and school type

#total weight of target by year and school type
production_obs<-target %>% group_by(year,school_type) %>% summarize (total_target_weigth=sum(weight_tons,na.rm=TRUE))

#add column 
production_obs$strata<-paste(production_obs$year, production_obs$school_type, sep="")
head(production_obs)

#total weight of bycatch by year and school type
bycatch_sp<-bycatch %>% group_by(species_group,scientific_name,school_type,year) %>% summarize (total_bycatch_weigth=sum(weight_tons,na.rm=TRUE))

#add column
bycatch_sp$strata<-paste(bycatch_sp$year,bycatch_sp$school_type, sep = "")
head(bycatch_sp)


dim(production_obs)
dim(bycatch_sp)
head(bycatch_sp)

##MERGE both data sets by the columns strata
unique(production_obs$strata) #34 strata
unique(bycatch_sp$strata) #30 strata

bycatch_sp_1 <- bycatch_sp[,c(3,4,5,6)] %>% aggregate(.~year+school_type+strata, FUN=sum)

Tproduction_Tbycatch_observed<-merge(bycatch_sp_1, production_obs, by=c("strata","school_type","year"), all=TRUE) #these BC data is ready to be extrapolated. 

#Tproduction_Tbc_observed<-merge(BC_sp, production_obs, by=c("strata"), all=TRUE)
dim(Tproduction_Tbycatch_observed)
unique(Tproduction_Tbycatch_observed$strata)
head(Tproduction_Tbycatch_observed)
Tproduction_Tbycatch_observed<-droplevels(Tproduction_Tbycatch_observed)

# For some quarters there is no bycatch, so we replace NA for 0
Tproduction_Tbycatch_observed$total_bycatch_weigth[is.na(Tproduction_Tbycatch_observed$total_bycatch_weigth)] <- 0

#############-----------------------
#3) Get the total production (skj, bet and yft)

total_targ_catch <- read.csv("Total_target_catch_spanish_fleet.csv", sep=";")

head(total_targ_catch)
summary(total_targ_catch)
str(total_targ_catch)
library(reshape2)
total_targ_catch$FSC<-(total_targ_catch$BET.FSC+total_targ_catch$YFT.FSC+total_targ_catch$SKJ.FSC)
total_targ_catch$FOB<-(total_targ_catch$BET.FOB+total_targ_catch$YFT.FOB+total_targ_catch$SKJ.FOB)

###########################
##############
#Join observed Bycatch & production and total production datasets (by year, set_type)
###########################
##############

pro_by_obs <- Tproduction_Tbycatch_observed[,c(2,3,4,5)]

pro_by_obs <- pro_by_obs %>% aggregate(.~year+school_type, FUN=sum) 

total_targ <- total_targ_catch[,c(1,8,9)]

total_targ <- total_targ %>%
  pivot_longer(cols = c(FOB, FSC), names_to = "school_type", values_to = "reported_total_catch_(t)")

total_targ <- total_targ %>% rename(year = Year)

table<-merge(pro_by_obs, total_targ, by=c("school_type", "year"), all=TRUE)

head(table)
summary(table)

##--------------
# explore the coverage in terms of production
#Prepare total EU production by year and school type
#prepare total observed production by year and school type

table$coverage.prod<-(table$total_target_weigth/table$`reported_total_catch_(t)`)*100



ggplot(table, aes(x=year,y=coverage.prod,fill=school_type))+
  geom_bar(stat="identity",position='dodge')+ ylim(0,100) +
  labs(x = "Year", y = "Coverage (%)", fill = "School_type", title = "Percentage of the total catches of targeted species observed") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 18)
  )

######################--------------------------------------------------------

table$total_bycatch_estimated<-((table$total_bycatch_weigth*100)/table$coverage.prod)

table$coverage.prod[is.na(table$coverage.prod)] <- 0
table$total_bycatch_weigth[is.na(table$total_bycatch_weigth)] <- "ND"
table$total_target_weigth[is.na(table$total_target_weigth)] <- "ND"
table$total_bycatch_estimated[is.na(table$total_bycatch_estimated)] <- "ND"
table$`reported_total_catch_(t)`[is.na(table$`reported_total_catch_(t)`)] <- "ND"

write.csv(table, file="Catch_bycatch_and_coverage_targeted_species_ESP_TIO.csv")















