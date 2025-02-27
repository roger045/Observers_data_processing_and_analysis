library(ggplot2)
library(dplyr)
library(gridExtra)
library(grid)
library(tidyr)
library(reshape2)

rm(list=ls())

setwd("C:/Use/OneDrive - AZTI/1. Tesis/5. Data/7. Bycatch documents & data")

data <- read.csv("Data_bycatch_ESP_SYC_TIO_area.csv", sep=",")

# only select the columns with data that interests us
data <- data[,c(4:7,12,13,15:17)]

# Select only the data for the SPANISH fleet
esp <- data %>% filter(flag_country=="Spain")

# Agregate the data by fao_code, scientific_name, school_type, year, Catch, End
esp1 <- esp %>% aggregate(weight_tons~., FUN=sum)

# read the data of the total reported catch by the spanish fleet and the proportions of observed and estimated
catch_data <- read.csv("Catch_bycatch_and_coverage_targeted_species_ESP_TIO.csv", sep=",")
catch_data <-catch_data[,c(2:8)]

# Merge the data by school_type and year 
bycatch_spain<-merge(esp1, catch_data, by=c("school_type","year"), all=TRUE) #these BC data is ready to be extrapolated. 

# delete the columns that don't interests us
bycatch_spain <- bycatch_spain[,-c(10,11,14)]

# Crear una nueva columna combinando Catch y End
bycatch_spain <- bycatch_spain %>%
  mutate(Catch_End = paste(Catch, End, sep = "_"))

# Reorganize the data frame using pivot_wider to have 3 columns of observed retained, observed discarded dead,
# and observed discarded alive
bycatch_spain <- bycatch_spain %>%
  pivot_wider(names_from = Catch_End, values_from = weight_tons, values_fill = list(weight_tons = 0))

bycatch_spain <- bycatch_spain[,-c(6:8,14)]

# rename some columns
bycatch_spain <- bycatch_spain %>% rename(reported_total_catch_tons = reported_total_catch_.t.)
bycatch_spain <- bycatch_spain %>% rename(perc.cov.obs = coverage.prod)
bycatch_spain <- bycatch_spain %>% rename(obs_discarded_dead = discarded_dead)
bycatch_spain <- bycatch_spain %>% rename(obs_retained = retained_dead)
bycatch_spain <- bycatch_spain %>% rename(obs_discarded_alive = discarded_alive)

# Change the format from scientific to numeric
options(scipen = 999)

# calculate the estimate amount of bycatch species using the proportion of observer coverage (perc.cov.obs)
bycatch_spain$est_retained <- ifelse(bycatch_spain$obs_retained==0,0,(bycatch_spain$obs_retained*100)/bycatch_spain$perc.cov.obs)
bycatch_spain$est_discarded_dead <- ifelse(bycatch_spain$obs_discarded_dead==0,0,(bycatch_spain$obs_discarded_dead*100)/bycatch_spain$perc.cov.obs)
bycatch_spain$est_discarded_alive <- ifelse(bycatch_spain$obs_discarded_alive==0,0,(bycatch_spain$obs_discarded_alive*100)/bycatch_spain$perc.cov.obs)

# summarise the data so for each species, each year and each school type, we only have line
bycatch_spain <- 
  bycatch_spain %>% 
  group_by(school_type, year, species_group, fao_code, scientific_name, reported_total_catch_tons, perc.cov.obs)%>% 
  summarise (obs_discarded_dead=sum(obs_discarded_dead),obs_retaineds=sum(obs_retained),obs_discarded_alive=sum(obs_discarded_alive),
             est_retained= sum(est_retained), est_discarded_dead=sum(est_discarded_dead), est_discarded_alive = sum(est_discarded_alive))

# Delate the data from 2023
bycatch_spain <- bycatch_spain %>% filter(year!=2023)

# convert to numeric the total catch reported column
bycatch_spain$reported_total_catch_tons <- as.numeric(bycatch_spain$reported_total_catch_tons)

# calculate the proportions for 1000t of targeted species caught
bycatch_spain$prop_disc_dead <- ifelse(bycatch_spain$est_discarded_dead==0, 0, ((bycatch_spain$est_discarded_dead*1000)/bycatch_spain$reported_total_catch_tons))
bycatch_spain$prop_reteined <- ifelse(bycatch_spain$est_retained==0, 0, ((bycatch_spain$est_retained*1000)/bycatch_spain$reported_total_catch_tons))
bycatch_spain$prop_disc_alive <- ifelse(bycatch_spain$est_discarded_alive==0, 0, ((bycatch_spain$est_discarded_alive*1000)/bycatch_spain$reported_total_catch_tons))

# Calculate the percentages of retained, discarded dead and discarded alive
bycatch_spain$perc_disc_dead <- ifelse(bycatch_spain$est_discarded_dead==0, 0, (bycatch_spain$est_discarded_dead*100/(bycatch_spain$est_discarded_dead+bycatch_spain$est_discarded_alive+bycatch_spain$est_retained)))
bycatch_spain$perc_disc_alive <- ifelse(bycatch_spain$est_discarded_alive==0, 0, (bycatch_spain$est_discarded_alive*100/(bycatch_spain$est_discarded_dead+bycatch_spain$est_discarded_alive+bycatch_spain$est_retained)))
bycatch_spain$perc_retained <- ifelse(bycatch_spain$est_retained==0, 0, (bycatch_spain$est_retained*100/(bycatch_spain$est_discarded_dead+bycatch_spain$est_discarded_alive+bycatch_spain$est_retained)))


################## MAKE SOME EXPLORATORY PLOTS

########### BET ###########
bet <- bycatch_spain %>% filter(fao_code=="BET")

# select only the columns that we want to use in this plot

bet <- bet[,c(1,2,7,17,18,19)]

bet <- bet %>% rename("Discarded dead" = "perc_disc_dead")
bet <- bet %>% rename("Discarded alive" = "perc_disc_alive")
bet <- bet %>% rename("Retained" = "perc_retained")
bet <- bet %>% rename("observed" = "perc.cov.obs")
bet$school_type <- ifelse(bet$school_type == "FOB", "FAD", bet$school_type)

# Convert the dataframe into long format
bet_long <- melt(bet, id.vars = c('year', 'school_type', 'observed'), variable.name = 'Destiny', value.name = 'percentage')

# Add fake data with NA
years_to_add <- data.frame(
  year = 2010:2011,
  school_type = rep(unique(bet_long$school_type), each = 2),
  Destiny = "Retained",
  percentage = 0,
  observed = 0
)

# combine with the original data frame
bet_long <- rbind(bet_long, years_to_add)

# Create the barplot
ggplot(bet_long, aes(x = factor(year), y = percentage, fill = Destiny)) +
  geom_bar(stat = 'identity', position = 'stack', width = 0.85) +
  facet_wrap(~ school_type) +
  labs(x = 'Year', y = '%', title = "Destiny of Bigeye catches by the Spanish purse seine fleet") +
  scale_x_discrete(drop = FALSE, breaks = function(x) x[seq(1, length(x), by = 3)]) +
  theme(plot.title = element_text(hjust = 0.5, size = 18), strip.text = element_text(size = 12),
  legend.title = element_text(size = 14),  
    legend.text = element_text(size = 12),  
    axis.text.x = element_text(size = 12),  
    axis.text.y = element_text(size = 12),  
    axis.title.x = element_text(size = 14),  
    axis.title.y = element_text(size = 14)  
  ) +
  geom_text(data = bet_long %>% filter(observed != 0) %>% group_by(year, school_type, Destiny) %>% summarise(observed = first(observed), .groups = 'drop'), 
            aes(x = factor(year), y = 106, label = sprintf("%.1f", observed)), 
            angle = 60, size = 3, na.rm = TRUE)

###########################

########### YFT ###########
yft <- bycatch_spain %>% filter(fao_code=="YFT")

# select only the columns that we want to use in this plot

yft <- yft[,c(1,2,7,17,18,19)]

yft <- yft %>% rename("Discarded dead" = "perc_disc_dead")
yft <- yft %>% rename("Discarded alive" = "perc_disc_alive")
yft <- yft %>% rename("Retained" = "perc_retained")
yft <- yft %>% rename("observed" = "perc.cov.obs")
yft$school_type <- ifelse(yft$school_type == "FOB", "FAD", yft$school_type)

# Convert the dataframe into long format
yft_long <- melt(yft, id.vars = c('year', 'school_type', 'observed'), variable.name = 'Destiny', value.name = 'percentage')

# Add fake data with NA
years_to_add <- data.frame(
  year = 2010:2011,
  school_type = rep(unique(yft_long$school_type), each = 2),
  Destiny = "Retained",
  percentage = 0,
  observed = 0
)

# combine with the original data frame
yft_long <- rbind(yft_long, years_to_add)

# Create the barplot
ggplot(yft_long, aes(x = factor(year), y = percentage, fill = Destiny)) +
  geom_bar(stat = 'identity', position = 'stack', width = 0.85) +
  facet_wrap(~ school_type) +
  labs(x = 'Year', y = '%', title = "Destiny of Yellowfin catches by the Spanish purse seine fleet") +
  scale_x_discrete(drop = FALSE, breaks = function(x) x[seq(1, length(x), by = 3)]) +
  theme(plot.title = element_text(hjust = 0.5, size = 18), strip.text = element_text(size = 12),
        legend.title = element_text(size = 14),  
        legend.text = element_text(size = 12),  
        axis.text.x = element_text(size = 12),  
        axis.text.y = element_text(size = 12),  
        axis.title.x = element_text(size = 14), 
        axis.title.y = element_text(size = 14)  
  ) +
  geom_text(data = yft_long %>% filter(observed != 0) %>% group_by(year, school_type, Destiny) %>% summarise(observed = first(observed), .groups = 'drop'), 
            aes(x = factor(year), y = 106, label = sprintf("%.1f", observed)), 
            angle = 60, size = 3, na.rm = TRUE)

###########################

########### SKJ ###########
skj <- bycatch_spain %>% filter(fao_code=="SKJ")

# select only the columns that we want to use in this plot

skj <- skj[,c(1,2,7,17,18,19)]

skj <- skj %>% rename("Discarded dead" = "perc_disc_dead")
skj <- skj %>% rename("Discarded alive" = "perc_disc_alive")
skj <- skj %>% rename("Retained" = "perc_retained")
skj <- skj %>% rename("observed" = "perc.cov.obs")
skj$school_type <- ifelse(skj$school_type == "FOB", "FAD", skj$school_type)

# Convert the dataframe into long format
skj_long <- melt(skj, id.vars = c('year', 'school_type', 'observed'), variable.name = 'Destiny', value.name = 'percentage')

# Add fake data with NA
years_to_add <- data.frame(
  year = 2010:2011,
  school_type = rep(unique(skj_long$school_type), each = 2),
  Destiny = "Retained",
  percentage = 0,
  observed = 0
)

# combine with the original data frame
skj_long <- rbind(skj_long, years_to_add)

# Create the barplot
ggplot(skj_long, aes(x = factor(year), y = percentage, fill = Destiny)) +
  geom_bar(stat = 'identity', position = 'stack', width = 0.85) +
  facet_wrap(~ school_type) +
  labs(x = 'Year', y = '%', title = "Destiny of Skipjack catches by the Spanish purse seine fleet") +
  scale_x_discrete(drop = FALSE, breaks = function(x) x[seq(1, length(x), by = 3)]) +
  theme(plot.title = element_text(hjust = 0.5, size = 18), strip.text = element_text(size = 12),
        legend.title = element_text(size = 14),  
        legend.text = element_text(size = 12),  
        axis.text.x = element_text(size = 12),  
        axis.text.y = element_text(size = 12),  
        axis.title.x = element_text(size = 14),  
        axis.title.y = element_text(size = 14)  
  ) +
  geom_text(data = skj_long %>% filter(observed != 0) %>% group_by(year, school_type, Destiny) %>% summarise(observed = first(observed), .groups = 'drop'), 
            aes(x = factor(year), y = 106, label = sprintf("%.1f", observed)), 
            angle = 60, size = 3, na.rm = TRUE)

###########################

########### BSH ###########
bsh <- bycatch_spain %>% filter(fao_code=="BSH")

# select only the columns that we want to use in this plot

bsh <- bsh[,c(1,2,7,17,18,19)]

bsh <- bsh %>% rename("Discarded dead" = "perc_disc_dead")
bsh <- bsh %>% rename("Discarded alive" = "perc_disc_alive")
bsh <- bsh %>% rename("Retained" = "perc_retained")
bsh <- bsh %>% rename("observed" = "perc.cov.obs")
bsh$school_type <- ifelse(bsh$school_type == "FOB", "FAD", bsh$school_type)

# Convert the dataframe into long format
bsh_long <- melt(bsh, id.vars = c('year', 'school_type', 'observed'), variable.name = 'Destiny', value.name = 'percentage')

# Create the barplot
ggplot(bsh_long, aes(x = factor(year), y = percentage, fill = Destiny)) +
  geom_bar(stat = 'identity', position = 'stack', width = 0.85) +
  facet_wrap(~ school_type) +
  labs(x = 'Year', y = '%', title = "Destiny of blue shark catches by the Spanish purse seine fleet") +
  scale_x_discrete(drop = FALSE, breaks = function(x) x[seq(1, length(x), by = 1)]) +
  theme(plot.title = element_text(hjust = 0.5, size = 18), strip.text = element_text(size = 12),
        legend.title = element_text(size = 14),  
        legend.text = element_text(size = 12),  
        axis.text.x = element_text(size = 12),  
        axis.text.y = element_text(size = 12),  
        axis.title.x = element_text(size = 14),  
        axis.title.y = element_text(size = 14)  
  ) +
  geom_text(data = bsh_long %>% filter(observed != 0) %>% group_by(year, school_type, Destiny) %>% summarise(observed = first(observed), .groups = 'drop'), 
            aes(x = factor(year), y = 106, label = sprintf("%.1f", observed)), 
            angle = 60, size = 3, na.rm = TRUE)

###########################

########### FAL ###########
fal <- bycatch_spain %>% filter(fao_code=="FAL")

# select only the columns that we want to use in this plot

fal <- fal[,c(1,2,7,17,18,19)]

fal <- fal %>% rename("Discarded dead" = "perc_disc_dead")
fal <- fal %>% rename("Discarded alive" = "perc_disc_alive")
fal <- fal %>% rename("Retained" = "perc_retained")
fal <- fal %>% rename("observed" = "perc.cov.obs")
fal$school_type <- ifelse(fal$school_type == "FOB", "FAD", fal$school_type)

# Convert the dataframe into long format
fal_long <- melt(fal, id.vars = c('year', 'school_type', 'observed'), variable.name = 'Destiny', value.name = 'percentage')

# Add fake data with NA
years_to_add <- data.frame(
  year = 2010:2011,
  school_type = rep(unique(fal_long$school_type), each = 2),
  Destiny = "Retained",
  percentage = 0,
  observed = 0
)

# combine with the original data frame
fal_long <- rbind(fal_long, years_to_add)

# Create the barplot
ggplot(fal_long, aes(x = factor(year), y = percentage, fill = Destiny)) +
  geom_bar(stat = 'identity', position = 'stack', width = 0.85) +
  facet_wrap(~ school_type) +
  labs(x = 'Year', y = '%', title = "Destiny of silky shark catches by the Spanish purse seine fleet") +
  scale_x_discrete(drop = FALSE, breaks = function(x) x[seq(1, length(x), by = 3)]) +
  theme(plot.title = element_text(hjust = 0.5, size = 18), strip.text = element_text(size = 12),
        legend.title = element_text(size = 14),  
        legend.text = element_text(size = 12),  
        axis.text.x = element_text(size = 12),  
        axis.text.y = element_text(size = 12),  
        axis.title.x = element_text(size = 14),  
        axis.title.y = element_text(size = 14)  
  ) +
  geom_text(data = fal_long %>% filter(observed != 0) %>% group_by(year, school_type, Destiny) %>% summarise(observed = first(observed), .groups = 'drop'), 
            aes(x = factor(year), y = 106, label = sprintf("%.1f", observed)), 
            angle = 60, size = 3, na.rm = TRUE)

###########################

########### OCS ###########
ocs <- bycatch_spain %>% filter(fao_code=="OCS")

# select only the columns that we want to use in this plot
ocs <- ocs[,c(1,2,7,17,18,19)]

ocs <- ocs %>% rename("Discarded dead" = "perc_disc_dead")
ocs <- ocs %>% rename("Discarded alive" = "perc_disc_alive")
ocs <- ocs %>% rename("Retained" = "perc_retained")
ocs <- ocs %>% rename("observed" = "perc.cov.obs")
ocs$school_type <- ifelse(ocs$school_type == "FOB", "FAD", ocs$school_type)

# Convert the dataframe into long format
ocs_long <- melt(ocs, id.vars = c('year', 'school_type', 'observed'), variable.name = 'Destiny', value.name = 'percentage')

# Add fake data with NA
years_to_add <- data.frame(
  year = 2010:2011,
  school_type = rep(unique(ocs_long$school_type), each = 2),
  Destiny = "Retained",
  percentage = 0,
  observed = 0
)

# combine with the original data frame
ocs_long <- rbind(ocs_long, years_to_add)

# Create the barplot
ggplot(ocs_long, aes(x = factor(year), y = percentage, fill = Destiny)) +
  geom_bar(stat = 'identity', position = 'stack', width = 0.85) +
  facet_wrap(~ school_type) +
  labs(x = 'Year', y = '%', title = "Destiny of oceanic whitetip shark catches by the Spanish purse seine fleet") +
  scale_x_discrete(drop = FALSE, breaks = function(x) x[seq(1, length(x), by = 3)]) +
  theme(plot.title = element_text(hjust = 0.5, size = 18), strip.text = element_text(size = 12),
        legend.title = element_text(size = 14),  
        legend.text = element_text(size = 12),  
        axis.text.x = element_text(size = 12),  
        axis.text.y = element_text(size = 12),  
        axis.title.x = element_text(size = 14),  
        axis.title.y = element_text(size = 14)  
  ) +
  geom_text(data = ocs_long %>% filter(observed != 0) %>% group_by(year, school_type, Destiny) %>% summarise(observed = first(observed), .groups = 'drop'), 
            aes(x = factor(year), y = 106, label = sprintf("%.1f", observed)), 
            angle = 60, size = 3, na.rm = TRUE)

###########################

########### Other sharks ###########
shk <- bycatch_spain %>% filter(species_group=="Sharks" & fao_code!="FAL" & fao_code!="BSH")

# select only the columns that we want to use in this plot
shk <- shk[,c(1,2,7,11,12,13)]

# we group the data by year, school_type and coverage
shk <- shk %>% 
  group_by(year, school_type, perc.cov.obs) %>%
  summarise(est_retained=sum(est_retained), est_discarded_dead=sum(est_discarded_dead), 
            est_discarded_alive=sum(est_discarded_alive))

# transform this new estimations into percentages
shk$perc_disc_dead <- ifelse(shk$est_discarded_dead==0, 0, (shk$est_discarded_dead*100/(shk$est_discarded_dead+shk$est_discarded_alive+shk$est_retained)))
shk$perc_disc_alive <- ifelse(shk$est_discarded_alive==0, 0, (shk$est_discarded_alive*100/(shk$est_discarded_dead+shk$est_discarded_alive+shk$est_retained)))
shk$perc_retained <- ifelse(shk$est_retained==0, 0, (shk$est_retained*100/(shk$est_discarded_dead+shk$est_discarded_alive+shk$est_retained)))

# select only the columns that we want to use in this plot
shk <- shk[,c(1,2,3,7,8,9)]

shk <- shk %>% rename("Discarded dead" = "perc_disc_dead")
shk <- shk %>% rename("Discarded alive" = "perc_disc_alive")
shk <- shk %>% rename("Retained" = "perc_retained")
shk <- shk %>% rename("observed" = "perc.cov.obs")
shk$school_type <- ifelse(shk$school_type == "FOB", "FAD", shk$school_type)

# Convert the dataframe into long format
shk_long <- melt(shk, id.vars = c('year', 'school_type', 'observed'), variable.name = 'Destiny', value.name = 'percentage')

# Add fake data with NA
years_to_add <- data.frame(
  year = 2010:2011,
  school_type = rep(unique(shk_long$school_type), each = 2),
  Destiny = "Retained",
  percentage = 0,
  observed = 0
)

# combine with the original data frame
shk_long <- rbind(shk_long, years_to_add)

# Create the barplot
ggplot(shk_long, aes(x = factor(year), y = percentage, fill = Destiny)) +
  geom_bar(stat = 'identity', position = 'stack', width = 0.85) +
  facet_wrap(~ school_type) +
  labs(x = 'Year', y = '%', title = "Destiny of other sharks catches by the Spanish purse seine fleet") +
  scale_x_discrete(drop = FALSE, breaks = function(x) x[seq(1, length(x), by = 3)]) +
  theme(plot.title = element_text(hjust = 0.5, size = 18), strip.text = element_text(size = 12),
        legend.title = element_text(size = 14),  
        legend.text = element_text(size = 12),  
        axis.text.x = element_text(size = 12),  
        axis.text.y = element_text(size = 12),  
        axis.title.x = element_text(size = 14),  
        axis.title.y = element_text(size = 14)  
  ) +
  geom_text(data = shk_long %>% filter(observed != 0) %>% group_by(year, school_type, Destiny) %>% summarise(observed = first(observed), .groups = 'drop'), 
            aes(x = factor(year), y = 106, label = sprintf("%.1f", observed)), 
            angle = 60, size = 3, na.rm = TRUE)

###########################

########### RHN ###########
rhn <- bycatch_spain %>% filter(fao_code=="RHN")

# select only the columns that we want to use in this plot
rhn <- rhn[,c(1,2,7,17,18,19)]

rhn <- rhn %>% rename("Discarded dead" = "perc_disc_dead")
rhn <- rhn %>% rename("Discarded alive" = "perc_disc_alive")
rhn <- rhn %>% rename("Retained" = "perc_retained")
rhn <- rhn %>% rename("observed" = "perc.cov.obs")
rhn$school_type <- ifelse(rhn$school_type == "FOB", "FAD", rhn$school_type)

# Convert the dataframe into long format
rhn_long <- melt(rhn, id.vars = c('year', 'school_type', 'observed'), variable.name = 'Destiny', value.name = 'percentage')

# Add fake data with NA
years_to_add <- data.frame(
  year = 2010:2011,
  school_type = rep(unique(rhn_long$school_type), each = 2),
  Destiny = "Retained",
  percentage = 0,
  observed = 0
)

# combine with the original data frame
rhn_long <- rbind(rhn_long, years_to_add)

# Create the barplot
ggplot(rhn_long, aes(x = factor(year), y = percentage, fill = Destiny)) +
  geom_bar(stat = 'identity', position = 'stack', width = 0.85) +
  facet_wrap(~ school_type) +
  labs(x = 'Year', y = '%', title = "Destiny of whale shark catches by the Spanish purse seine fleet") +
  scale_x_discrete(drop = FALSE, breaks = function(x) x[seq(1, length(x), by = 3)]) +
  theme(plot.title = element_text(hjust = 0.5, size = 18), strip.text = element_text(size = 12),
        legend.title = element_text(size = 14),  
        legend.text = element_text(size = 12),  
        axis.text.x = element_text(size = 12),  
        axis.text.y = element_text(size = 12),  
        axis.title.x = element_text(size = 14),  
        axis.title.y = element_text(size = 14)  
  ) +
  geom_text(data = rhn_long %>% filter(observed != 0) %>% group_by(year, school_type, Destiny) %>% summarise(observed = first(observed), .groups = 'drop'), 
            aes(x = factor(year), y = 106, label = sprintf("%.1f", observed)), 
            angle = 60, size = 3, na.rm = TRUE)

###########################

########### Turtles ###########
turtles <- bycatch_spain %>% filter(species_group=="Turtles")

# select only the columns that we want to use in this plot
turtles <- turtles[,c(1,2,7,11,12,13)]

# we group the data by year, school_type and coverage
turtles <- turtles %>% 
  group_by(year, school_type, perc.cov.obs) %>%
  summarise(est_retained=sum(est_retained), est_discarded_dead=sum(est_discarded_dead), 
            est_discarded_alive=sum(est_discarded_alive))

# transform this new estimations into percentages
turtles$perc_disc_dead <- ifelse(turtles$est_discarded_dead==0, 0, (turtles$est_discarded_dead*100/(turtles$est_discarded_dead+turtles$est_discarded_alive+turtles$est_retained)))
turtles$perc_disc_alive <- ifelse(turtles$est_discarded_alive==0, 0, (turtles$est_discarded_alive*100/(turtles$est_discarded_dead+turtles$est_discarded_alive+turtles$est_retained)))
turtles$perc_retained <- ifelse(turtles$est_retained==0, 0, (turtles$est_retained*100/(turtles$est_discarded_dead+turtles$est_discarded_alive+turtles$est_retained)))

# select only the columns that we want to use in this plot
turtles <- turtles[,c(1,2,3,7,8,9)]

turtles <- turtles %>% rename("Discarded dead" = "perc_disc_dead")
turtles <- turtles %>% rename("Discarded alive" = "perc_disc_alive")
turtles <- turtles %>% rename("Retained" = "perc_retained")
turtles <- turtles %>% rename("observed" = "perc.cov.obs")
turtles$school_type <- ifelse(turtles$school_type == "FOB", "FAD", turtles$school_type)

# Convert the dataframe into long format
turtles_long <- melt(turtles, id.vars = c('year', 'school_type', 'observed'), variable.name = 'Destiny', value.name = 'percentage')

# Add fake data with NA
years_to_add <- data.frame(
  year = 2010:2011,
  school_type = rep(unique(turtles_long$school_type), each = 2),
  Destiny = "Retained",
  percentage = 0,
  observed = 0
)

# combine with the original data frame
turtles_long <- rbind(turtles_long, years_to_add)

# Create the barplot
ggplot(turtles_long, aes(x = factor(year), y = percentage, fill = Destiny)) +
  geom_bar(stat = 'identity', position = 'stack', width = 0.85) +
  facet_wrap(~ school_type) +
  labs(x = 'Year', y = '%', title = "Destiny of turtle catches by the Spanish purse seine fleet") +
  scale_x_discrete(drop = FALSE, breaks = function(x) x[seq(1, length(x), by = 3)]) +
  theme(plot.title = element_text(hjust = 0.5, size = 18), strip.text = element_text(size = 12),
        legend.title = element_text(size = 14),  
        legend.text = element_text(size = 12),  
        axis.text.x = element_text(size = 12),  
        axis.text.y = element_text(size = 12),  
        axis.title.x = element_text(size = 14),  
        axis.title.y = element_text(size = 14)  
  ) +
  geom_text(data = turtles_long %>% filter(observed != 0) %>% group_by(year, school_type, Destiny) %>% summarise(observed = first(observed), .groups = 'drop'), 
            aes(x = factor(year), y = 106, label = sprintf("%.1f", observed)), 
            angle = 60, size = 3, na.rm = TRUE)

###########################

########### Rays ###########
rays <- bycatch_spain %>% filter(species_group=="Rays")

# select only the columns that we want to use in this plot
rays <- rays[,c(1,2,7,11,12,13)]

# we group the data by year, school_type and coverage
rays <- rays %>% 
  group_by(year, school_type, perc.cov.obs) %>%
  summarise(est_retained=sum(est_retained), est_discarded_dead=sum(est_discarded_dead), 
            est_discarded_alive=sum(est_discarded_alive))

# transform this new estimations into percentages
rays$perc_disc_dead <- ifelse(rays$est_discarded_dead==0, 0, (rays$est_discarded_dead*100/(rays$est_discarded_dead+rays$est_discarded_alive+rays$est_retained)))
rays$perc_disc_alive <- ifelse(rays$est_discarded_alive==0, 0, (rays$est_discarded_alive*100/(rays$est_discarded_dead+rays$est_discarded_alive+rays$est_retained)))
rays$perc_retained <- ifelse(rays$est_retained==0, 0, (rays$est_retained*100/(rays$est_discarded_dead+rays$est_discarded_alive+rays$est_retained)))

# select only the columns that we want to use in this plot
rays <- rays[,c(1,2,3,7,8,9)]

rays <- rays %>% rename("Discarded dead" = "perc_disc_dead")
rays <- rays %>% rename("Discarded alive" = "perc_disc_alive")
rays <- rays %>% rename("Retained" = "perc_retained")
rays <- rays %>% rename("observed" = "perc.cov.obs")
rays$school_type <- ifelse(rays$school_type == "FOB", "FAD", rays$school_type)

# Convert the dataframe into long format
rays_long <- melt(rays, id.vars = c('year', 'school_type', 'observed'), variable.name = 'Destiny', value.name = 'percentage')

# Add fake data with NA
years_to_add <- data.frame(
  year = 2010:2011,
  school_type = rep(unique(rays_long$school_type), each = 2),
  Destiny = "Retained",
  percentage = 0,
  observed = 0
)

# combine with the original data frame
rays_long <- rbind(rays_long, years_to_add)

# Create the barplot
ggplot(rays_long, aes(x = factor(year), y = percentage, fill = Destiny)) +
  geom_bar(stat = 'identity', position = 'stack', width = 0.85) +
  facet_wrap(~ school_type) +
  labs(x = 'Year', y = '%', title = "Destiny of rays catches by the Spanish purse seine fleet") +
  scale_x_discrete(drop = FALSE, breaks = function(x) x[seq(1, length(x), by = 3)]) +
  theme(plot.title = element_text(hjust = 0.5, size = 18), strip.text = element_text(size = 12),
        legend.title = element_text(size = 14),  
        legend.text = element_text(size = 12),  
        axis.text.x = element_text(size = 12),  
        axis.text.y = element_text(size = 12),  
        axis.title.x = element_text(size = 14),  
        axis.title.y = element_text(size = 14)  
  ) +
  geom_text(data = rays_long %>% filter(observed != 0) %>% group_by(year, school_type, Destiny) %>% summarise(observed = first(observed), .groups = 'drop'), 
            aes(x = factor(year), y = 106, label = sprintf("%.1f", observed)), 
            angle = 60, size = 3, na.rm = TRUE)

###########################

########### Mobulas ###########
mobulas <- bycatch_spain %>% filter(species_group=="Mobulas")

# select only the columns that we want to use in this plot
mobulas <- mobulas[,c(1,2,7,11,12,13)]

# we group the data by year, school_type and coverage
mobulas <- mobulas %>% 
  group_by(year, school_type, perc.cov.obs) %>%
  summarise(est_retained=sum(est_retained), est_discarded_dead=sum(est_discarded_dead), 
            est_discarded_alive=sum(est_discarded_alive))

# transform this new estimations into percentages
mobulas$perc_disc_dead <- ifelse(mobulas$est_discarded_dead==0, 0, (mobulas$est_discarded_dead*100/(mobulas$est_discarded_dead+mobulas$est_discarded_alive+mobulas$est_retained)))
mobulas$perc_disc_alive <- ifelse(mobulas$est_discarded_alive==0, 0, (mobulas$est_discarded_alive*100/(mobulas$est_discarded_dead+mobulas$est_discarded_alive+mobulas$est_retained)))
mobulas$perc_retained <- ifelse(mobulas$est_retained==0, 0, (mobulas$est_retained*100/(mobulas$est_discarded_dead+mobulas$est_discarded_alive+mobulas$est_retained)))

# select only the columns that we want to use in this plot
mobulas <- mobulas[,c(1,2,3,7,8,9)]

mobulas <- mobulas %>% rename("Discarded dead" = "perc_disc_dead")
mobulas <- mobulas %>% rename("Discarded alive" = "perc_disc_alive")
mobulas <- mobulas %>% rename("Retained" = "perc_retained")
mobulas <- mobulas %>% rename("observed" = "perc.cov.obs")
mobulas$school_type <- ifelse(mobulas$school_type == "FOB", "FAD", mobulas$school_type)

# Convert the dataframe into long format
mobulas_long <- melt(mobulas, id.vars = c('year', 'school_type', 'observed'), variable.name = 'Destiny', value.name = 'percentage')

# Add fake data with NA
years_to_add <- data.frame(
  year = 2010:2011,
  school_type = rep(unique(mobulas_long$school_type), each = 2),
  Destiny = "Retained",
  percentage = 0,
  observed = 0
)

# combine with the original data frame
mobulas_long <- rbind(mobulas_long, years_to_add)

# Create the barplot
ggplot(mobulas_long, aes(x = factor(year), y = percentage, fill = Destiny)) +
  geom_bar(stat = 'identity', position = 'stack', width = 0.85) +
  facet_wrap(~ school_type) +
  labs(x = 'Year', y = '%', title = "Destiny of mobulas catches by the Spanish purse seine fleet") +
  scale_x_discrete(drop = FALSE, breaks = function(x) x[seq(1, length(x), by = 3)]) +
  theme(plot.title = element_text(hjust = 0.5, size = 18), strip.text = element_text(size = 12),
        legend.title = element_text(size = 14),  
        legend.text = element_text(size = 12),  
        axis.text.x = element_text(size = 12),  
        axis.text.y = element_text(size = 12),  
        axis.title.x = element_text(size = 14),  
        axis.title.y = element_text(size = 14)  
  ) +
  geom_text(data = mobulas_long %>% filter(observed != 0) %>% group_by(year, school_type, Destiny) %>% summarise(observed = first(observed), .groups = 'drop'), 
            aes(x = factor(year), y = 106, label = sprintf("%.1f", observed)), 
            angle = 60, size = 3, na.rm = TRUE)

###########################

########### Billfishes ###########
billfishes <- bycatch_spain %>% filter(species_group=="Billfishes" & fao_code!="SWO")

# select only the columns that we want to use in this plot
billfishes <- billfishes[,c(1,2,7,11,12,13)]

# we group the data by year, school_type and coverage
billfishes <- billfishes %>% 
  group_by(year, school_type, perc.cov.obs) %>%
  summarise(est_retained=sum(est_retained), est_discarded_dead=sum(est_discarded_dead), 
            est_discarded_alive=sum(est_discarded_alive))

# transform this new estimations into percentages
billfishes$perc_disc_dead <- ifelse(billfishes$est_discarded_dead==0, 0, (billfishes$est_discarded_dead*100/(billfishes$est_discarded_dead+billfishes$est_discarded_alive+billfishes$est_retained)))
billfishes$perc_disc_alive <- ifelse(billfishes$est_discarded_alive==0, 0, (billfishes$est_discarded_alive*100/(billfishes$est_discarded_dead+billfishes$est_discarded_alive+billfishes$est_retained)))
billfishes$perc_retained <- ifelse(billfishes$est_retained==0, 0, (billfishes$est_retained*100/(billfishes$est_discarded_dead+billfishes$est_discarded_alive+billfishes$est_retained)))

# select only the columns that we want to use in this plot
billfishes <- billfishes[,c(1,2,3,7,8,9)]

billfishes <- billfishes %>% rename("Discarded dead" = "perc_disc_dead")
billfishes <- billfishes %>% rename("Discarded alive" = "perc_disc_alive")
billfishes <- billfishes %>% rename("Retained" = "perc_retained")
billfishes <- billfishes %>% rename("observed" = "perc.cov.obs")
billfishes$school_type <- ifelse(billfishes$school_type == "FOB", "FAD", billfishes$school_type)

# Convert the dataframe into long format
billfishes_long <- melt(billfishes, id.vars = c('year', 'school_type', 'observed'), variable.name = 'Destiny', value.name = 'percentage')

# Add fake data with NA
years_to_add <- data.frame(
  year = 2010:2011,
  school_type = rep(unique(billfishes_long$school_type), each = 2),
  Destiny = "Retained",
  percentage = 0,
  observed = 0
)

# combine with the original data frame
billfishes_long <- rbind(billfishes_long, years_to_add)

# Create the barplot
ggplot(billfishes_long, aes(x = factor(year), y = percentage, fill = Destiny)) +
  geom_bar(stat = 'identity', position = 'stack', width = 0.85) +
  facet_wrap(~ school_type) +
  labs(x = 'Year', y = '%', title = "Destiny of other billfishes catches by the Spanish purse seine fleet") +
  scale_x_discrete(drop = FALSE, breaks = function(x) x[seq(1, length(x), by = 3)]) +
  theme(plot.title = element_text(hjust = 0.5, size = 18), strip.text = element_text(size = 12),
        legend.title = element_text(size = 14),  
        legend.text = element_text(size = 12),  
        axis.text.x = element_text(size = 12),  
        axis.text.y = element_text(size = 12),  
        axis.title.x = element_text(size = 14),  
        axis.title.y = element_text(size = 14)  
  ) +
  geom_text(data = billfishes_long %>% filter(observed != 0) %>% group_by(year, school_type, Destiny) %>% summarise(observed = first(observed), .groups = 'drop'), 
            aes(x = factor(year), y = 106, label = sprintf("%.1f", observed)), 
            angle = 60, size = 3, na.rm = TRUE)

###########################

########### SWO ###########
swo <- bycatch_spain %>% filter(fao_code=="SWO")

# select only the columns that we want to use in this plot
swo <- swo[,c(1,2,7,17,18,19)]

swo <- swo %>% rename("Discarded dead" = "perc_disc_dead")
swo <- swo %>% rename("Discarded alive" = "perc_disc_alive")
swo <- swo %>% rename("Retained" = "perc_retained")
swo <- swo %>% rename("observed" = "perc.cov.obs")
swo$school_type <- ifelse(swo$school_type == "FOB", "FAD", swo$school_type)

# Convert the dataframe into long format
swo_long <- melt(swo, id.vars = c('year', 'school_type', 'observed'), variable.name = 'Destiny', value.name = 'percentage')

# Add fake data with NA
years_to_add <- data.frame(
  year = 2010:2011,
  school_type = rep(unique(swo_long$school_type), each = 2),
  Destiny = "Retained",
  percentage = 0,
  observed = 0
)

# combine with the original data frame
swo_long <- rbind(swo_long, years_to_add)

# Create the barplot
ggplot(swo_long, aes(x = factor(year), y = percentage, fill = Destiny)) +
  geom_bar(stat = 'identity', position = 'stack', width = 0.85) +
  facet_wrap(~ school_type) +
  labs(x = 'Year', y = '%', title = "Destiny of swordfish catches by the Spanish purse seine fleet") +
  scale_x_discrete(drop = FALSE, breaks = function(x) x[seq(1, length(x), by = 3)]) +
  theme(plot.title = element_text(hjust = 0.5, size = 18), strip.text = element_text(size = 12),
        legend.title = element_text(size = 14),  
        legend.text = element_text(size = 12),  
        axis.text.x = element_text(size = 12),  
        axis.text.y = element_text(size = 12),  
        axis.title.x = element_text(size = 14),  
        axis.title.y = element_text(size = 14)  
  ) +
  geom_text(data = swo_long %>% filter(observed != 0) %>% group_by(year, school_type, Destiny) %>% summarise(observed = first(observed), .groups = 'drop'), 
            aes(x = factor(year), y = 106, label = sprintf("%.1f", observed)), 
            angle = 60, size = 3, na.rm = TRUE)

###########################

########### ALB ###########
alb <- bycatch_spain %>% filter(fao_code=="ALB")

# select only the columns that we want to use in this plot
alb <- alb[,c(1,2,7,17,18,19)]

alb <- alb %>% rename("Discarded dead" = "perc_disc_dead")
alb <- alb %>% rename("Discarded alive" = "perc_disc_alive")
alb <- alb %>% rename("Retained" = "perc_retained")
alb <- alb %>% rename("observed" = "perc.cov.obs")
alb$school_type <- ifelse(alb$school_type == "FOB", "FAD", alb$school_type)

# Convert the dataframe into long format
alb_long <- melt(alb, id.vars = c('year', 'school_type', 'observed'), variable.name = 'Destiny', value.name = 'percentage')

# Add fake data with NA
years_to_add <- data.frame(
  year = 2010:2011,
  school_type = rep(unique(alb_long$school_type), each = 2),
  Destiny = "Retained",
  percentage = 0,
  observed = 0
)

# combine with the original data frame
alb_long <- rbind(alb_long, years_to_add)

# Create the barplot
ggplot(alb_long, aes(x = factor(year), y = percentage, fill = Destiny)) +
  geom_bar(stat = 'identity', position = 'stack', width = 0.85) +
  facet_wrap(~ school_type) +
  labs(x = 'Year', y = '%', title = "Destiny of albacore catches by the Spanish purse seine fleet") +
  scale_x_discrete(drop = FALSE, breaks = function(x) x[seq(1, length(x), by = 3)]) +
  theme(plot.title = element_text(hjust = 0.5, size = 18), strip.text = element_text(size = 12),
        legend.title = element_text(size = 14),  
        legend.text = element_text(size = 12),  
        axis.text.x = element_text(size = 12),  
        axis.text.y = element_text(size = 12),  
        axis.title.x = element_text(size = 14),  
        axis.title.y = element_text(size = 14)  
  ) +
  geom_text(data = alb_long %>% filter(observed != 0) %>% group_by(year, school_type, Destiny) %>% summarise(observed = first(observed), .groups = 'drop'), 
            aes(x = factor(year), y = 106, label = sprintf("%.1f", observed)), 
            angle = 60, size = 3, na.rm = TRUE)

###########################

########### Small tunas and other scombrids ###########
scombri <- bycatch_spain %>% filter(fao_code=="BLT" | fao_code=="FRI" | 
                                      fao_code=="MAX" | fao_code=="KAW" | fao_code=="WAH")

# select only the columns that we want to use in this plot
scombri <- scombri[,c(1,2,7,11,12,13)]

# we group the data by year, school_type and coverage
scombri <- scombri %>% 
  group_by(year, school_type, perc.cov.obs) %>%
  summarise(est_retained=sum(est_retained), est_discarded_dead=sum(est_discarded_dead), 
            est_discarded_alive=sum(est_discarded_alive))

# transform this new estimations into percentages
scombri$perc_disc_dead <- ifelse(scombri$est_discarded_dead==0, 0, (scombri$est_discarded_dead*100/(scombri$est_discarded_dead+scombri$est_discarded_alive+scombri$est_retained)))
scombri$perc_disc_alive <- ifelse(scombri$est_discarded_alive==0, 0, (scombri$est_discarded_alive*100/(scombri$est_discarded_dead+scombri$est_discarded_alive+scombri$est_retained)))
scombri$perc_retained <- ifelse(scombri$est_retained==0, 0, (scombri$est_retained*100/(scombri$est_discarded_dead+scombri$est_discarded_alive+scombri$est_retained)))

# select only the columns that we want to use in this plot
scombri <- scombri[,c(1,2,3,7,8,9)]

scombri <- scombri %>% rename("Discarded dead" = "perc_disc_dead")
scombri <- scombri %>% rename("Discarded alive" = "perc_disc_alive")
scombri <- scombri %>% rename("Retained" = "perc_retained")
scombri <- scombri %>% rename("observed" = "perc.cov.obs")
scombri$school_type <- ifelse(scombri$school_type == "FOB", "FAD", scombri$school_type)

# Convert the dataframe into long format
scombri_long <- melt(scombri, id.vars = c('year', 'school_type', 'observed'), variable.name = 'Destiny', value.name = 'percentage')

# Add fake data with NA
years_to_add <- data.frame(
  year = 2010:2011,
  school_type = rep(unique(scombri_long$school_type), each = 2),
  Destiny = "Retained",
  percentage = 0,
  observed = 0
)

# combine with the original data frame
scombri_long <- rbind(scombri_long, years_to_add)

# Create the barplot
ggplot(scombri_long, aes(x = factor(year), y = percentage, fill = Destiny)) +
  geom_bar(stat = 'identity', position = 'stack', width = 0.85) +
  facet_wrap(~ school_type) +
  labs(x = 'Year', y = '%', title = "Destiny of small tunas catches by the Spanish purse seine fleet") +
  scale_x_discrete(drop = FALSE, breaks = function(x) x[seq(1, length(x), by = 3)]) +
  theme(plot.title = element_text(hjust = 0.5, size = 18), strip.text = element_text(size = 12),
        legend.title = element_text(size = 14),  
        legend.text = element_text(size = 12),  
        axis.text.x = element_text(size = 12),  
        axis.text.y = element_text(size = 12),  
        axis.title.x = element_text(size = 14),  
        axis.title.y = element_text(size = 14)  
  ) +
  geom_text(data = scombri_long %>% filter(observed != 0) %>% group_by(year, school_type, Destiny) %>% summarise(observed = first(observed), .groups = 'drop'), 
            aes(x = factor(year), y = 106, label = sprintf("%.1f", observed)), 
            angle = 60, size = 3, na.rm = TRUE)

###########################

########### Jacks, mackerels, runners and other carangids ###########
caran <- bycatch_spain %>% filter(fao_code=="RRU" | fao_code=="YTL" | 
                                      fao_code=="CXS" | fao_code=="CGX" | fao_code=="NAU" | fao_code=="YTC"|
                                  fao_code=="USE" | fao_code=="MSD" | fao_code=="NGT"
                                  | fao_code=="UKK" | fao_code=="URU" | fao_code=="VMA" | fao_code=="YTL")

# select only the columns that we want to use in this plot
caran <- caran[,c(1,2,7,11,12,13)]

# we group the data by year, school_type and coverage
caran <- caran %>% 
  group_by(year, school_type, perc.cov.obs) %>%
  summarise(est_retained=sum(est_retained), est_discarded_dead=sum(est_discarded_dead), 
            est_discarded_alive=sum(est_discarded_alive))

# transform this new estimations into percentages
caran$perc_disc_dead <- ifelse(caran$est_discarded_dead==0, 0, (caran$est_discarded_dead*100/(caran$est_discarded_dead+caran$est_discarded_alive+caran$est_retained)))
caran$perc_disc_alive <- ifelse(caran$est_discarded_alive==0, 0, (caran$est_discarded_alive*100/(caran$est_discarded_dead+caran$est_discarded_alive+caran$est_retained)))
caran$perc_retained <- ifelse(caran$est_retained==0, 0, (caran$est_retained*100/(caran$est_discarded_dead+caran$est_discarded_alive+caran$est_retained)))

# select only the columns that we want to use in this plot
caran <- caran[,c(1,2,3,7,8,9)]

caran <- caran %>% rename("Discarded dead" = "perc_disc_dead")
caran <- caran %>% rename("Discarded alive" = "perc_disc_alive")
caran <- caran %>% rename("Retained" = "perc_retained")
caran <- caran %>% rename("observed" = "perc.cov.obs")
caran$school_type <- ifelse(caran$school_type == "FOB", "FAD", caran$school_type)

# Convert the dataframe into long format
caran_long <- melt(caran, id.vars = c('year', 'school_type', 'observed'), variable.name = 'Destiny', value.name = 'percentage')

# Add fake data with NA
years_to_add <- data.frame(
  year = 2010:2011,
  school_type = rep(unique(caran_long$school_type), each = 2),
  Destiny = "Retained",
  percentage = 0,
  observed = 0
)

# combine with the original data frame
caran_long <- rbind(caran_long, years_to_add)

# Create the barplot
ggplot(caran_long, aes(x = factor(year), y = percentage, fill = Destiny)) +
  geom_bar(stat = 'identity', position = 'stack', width = 0.85) +
  facet_wrap(~ school_type) +
  labs(x = 'Year', y = '%', title = "Destiny of jacks, mackerels,... catches by the Spanish purse seine fleet") +
  scale_x_discrete(drop = FALSE, breaks = function(x) x[seq(1, length(x), by = 3)]) +
  theme(plot.title = element_text(hjust = 0.5, size = 18), strip.text = element_text(size = 12),
        legend.title = element_text(size = 14),  
        legend.text = element_text(size = 12),  
        axis.text.x = element_text(size = 12),  
        axis.text.y = element_text(size = 12),  
        axis.title.x = element_text(size = 14),  
        axis.title.y = element_text(size = 14)  
  ) +
  geom_text(data = caran_long %>% filter(observed != 0) %>% group_by(year, school_type, Destiny) %>% summarise(observed = first(observed), .groups = 'drop'), 
            aes(x = factor(year), y = 106, label = sprintf("%.1f", observed)), 
            angle = 60, size = 3, na.rm = TRUE)

###########################

########### EF - I ###########
efI <- bycatch_spain %>% filter(fao_code=="BAF" | fao_code=="DOX" | 
                                    fao_code=="DOL" | fao_code=="CFW" |
                                   fao_code=="BTS" | fao_code=="LOB"
                                  | fao_code=="GBA" | fao_code=="MZZ" | fao_code=="BEN" | fao_code=="BAZ")

# select only the columns that we want to use in this plot
efI <- efI[,c(1,2,7,11,12,13)]

# we group the data by year, school_type and coverage
efI <- efI %>% 
  group_by(year, school_type, perc.cov.obs) %>%
  summarise(est_retained=sum(est_retained), est_discarded_dead=sum(est_discarded_dead), 
            est_discarded_alive=sum(est_discarded_alive))

# transform this new estimations into percentages
efI$perc_disc_dead <- ifelse(efI$est_discarded_dead==0, 0, (efI$est_discarded_dead*100/(efI$est_discarded_dead+efI$est_discarded_alive+efI$est_retained)))
efI$perc_disc_alive <- ifelse(efI$est_discarded_alive==0, 0, (efI$est_discarded_alive*100/(efI$est_discarded_dead+efI$est_discarded_alive+efI$est_retained)))
efI$perc_retained <- ifelse(efI$est_retained==0, 0, (efI$est_retained*100/(efI$est_discarded_dead+efI$est_discarded_alive+efI$est_retained)))

# select only the columns that we want to use in this plot
efI <- efI[,c(1,2,3,7,8,9)]

efI <- efI %>% rename("Discarded dead" = "perc_disc_dead")
efI <- efI %>% rename("Discarded alive" = "perc_disc_alive")
efI <- efI %>% rename("Retained" = "perc_retained")
efI <- efI %>% rename("observed" = "perc.cov.obs")
efI$school_type <- ifelse(efI$school_type == "FOB", "FAD", efI$school_type)

# Convert the dataframe into long format
efI_long <- melt(efI, id.vars = c('year', 'school_type', 'observed'), variable.name = 'Destiny', value.name = 'percentage')

# Add fake data with NA
years_to_add <- data.frame(
  year = 2010:2011,
  school_type = rep(unique(efI_long$school_type), each = 2),
  Destiny = "Retained",
  percentage = 0,
  observed = 0
)

# combine with the original data frame
efI_long <- rbind(efI_long, years_to_add)

# Create the barplot
ggplot(efI_long, aes(x = factor(year), y = percentage, fill = Destiny)) +
  geom_bar(stat = 'identity', position = 'stack', width = 0.85) +
  facet_wrap(~ school_type) +
  labs(x = 'Year', y = '%', title = "Destiny of EF-I catches by the Spanish purse seine fleet") +
  scale_x_discrete(drop = FALSE, breaks = function(x) x[seq(1, length(x), by = 3)]) +
  theme(plot.title = element_text(hjust = 0.5, size = 18), strip.text = element_text(size = 12),
        legend.title = element_text(size = 14),  
        legend.text = element_text(size = 12),  
        axis.text.x = element_text(size = 12),  
        axis.text.y = element_text(size = 12),  
        axis.title.x = element_text(size = 14),  
        axis.title.y = element_text(size = 14)  
  ) +
  geom_text(data = efI_long %>% filter(observed != 0) %>% group_by(year, school_type, Destiny) %>% summarise(observed = first(observed), .groups = 'drop'), 
            aes(x = factor(year), y = 106, label = sprintf("%.1f", observed)), 
            angle = 60, size = 3, na.rm = TRUE)

###########################

########### EF - II ###########
efII <- bycatch_spain %>% filter(fao_code=="DIY" | fao_code=="DIO" | fao_code=="LGH" | 
                                  fao_code=="KYS" | fao_code=="KYP" | fao_code=="KYV" |
                                  fao_code=="BAO" | fao_code=="BAT" | fao_code=="MRW" |
                                  fao_code=="MOX"| fao_code=="ALM" | fao_code=="ALN" |
                                  fao_code=="AJS" | fao_code=="CNT" | fao_code=="UDA" |
                                  fao_code=="TRI" | fao_code=="NHR" | fao_code=="ECN" |
                                  fao_code=="EHN" | fao_code=="SPA" | fao_code=="KYC" |
                                  fao_code=="REO" | fao_code=="RRL" | fao_code=="BSX" |
                                  fao_code=="PUX" | fao_code=="USE" | fao_code=="UDD" )
                                  

# select only the columns that we want to use in this plot
efII <- efII[,c(1,2,7,11,12,13)]

# we group the data by year, school_type and coverage
efII <- efII %>% 
  group_by(year, school_type, perc.cov.obs) %>%
  summarise(est_retained=sum(est_retained), est_discarded_dead=sum(est_discarded_dead), 
            est_discarded_alive=sum(est_discarded_alive))

# transform this new estimations into percentages
efII$perc_disc_dead <- ifelse(efII$est_discarded_dead==0, 0, (efII$est_discarded_dead*100/(efII$est_discarded_dead+efII$est_discarded_alive+efII$est_retained)))
efII$perc_disc_alive <- ifelse(efII$est_discarded_alive==0, 0, (efII$est_discarded_alive*100/(efII$est_discarded_dead+efII$est_discarded_alive+efII$est_retained)))
efII$perc_retained <- ifelse(efII$est_retained==0, 0, (efII$est_retained*100/(efII$est_discarded_dead+efII$est_discarded_alive+efII$est_retained)))

# select only the columns that we want to use in this plot
efII <- efII[,c(1,2,3,7,8,9)]

efII <- efII %>% rename("Discarded dead" = "perc_disc_dead")
efII <- efII %>% rename("Discarded alive" = "perc_disc_alive")
efII <- efII %>% rename("Retained" = "perc_retained")
efII <- efII %>% rename("observed" = "perc.cov.obs")
efII$school_type <- ifelse(efII$school_type == "FOB", "FAD", efII$school_type)

# Convert the dataframe into long format
efII_long <- melt(efII, id.vars = c('year', 'school_type', 'observed'), variable.name = 'Destiny', value.name = 'percentage')

# Add fake data with NA
years_to_add <- data.frame(
  year = 2010:2011,
  school_type = rep(unique(efII_long$school_type), each = 2),
  Destiny = "Retained",
  percentage = 0,
  observed = 0
)

# combine with the original data frame
efII_long <- rbind(efII_long, years_to_add)

# Create the barplot
ggplot(efII_long, aes(x = factor(year), y = percentage, fill = Destiny)) +
  geom_bar(stat = 'identity', position = 'stack', width = 0.85) +
  facet_wrap(~ school_type) +
  labs(x = 'Year', y = '%', title = "Destiny of EF-II catches by the Spanish purse seine fleet") +
  scale_x_discrete(drop = FALSE, breaks = function(x) x[seq(1, length(x), by = 3)]) +
  theme(plot.title = element_text(hjust = 0.5, size = 18), strip.text = element_text(size = 12),
        legend.title = element_text(size = 14),  
        legend.text = element_text(size = 12),  
        axis.text.x = element_text(size = 12),  
        axis.text.y = element_text(size = 12),  
        axis.title.x = element_text(size = 14),  
        axis.title.y = element_text(size = 14)  
  ) +
  geom_text(data = efII_long %>% filter(observed != 0) %>% group_by(year, school_type, Destiny) %>% summarise(observed = first(observed), .groups = 'drop'), 
            aes(x = factor(year), y = 106, label = sprintf("%.1f", observed)), 
            angle = 60, size = 3, na.rm = TRUE)

#################################################

########### LMF ###########
lmf <- bycatch_spain %>% filter(fao_code=="POA" | fao_code=="BRZ" | fao_code=="GES" | 
                                   fao_code=="LAG" | fao_code=="OIL")


# select only the columns that we want to use in this plot
lmf <- lmf[,c(1,2,7,11,12,13)]

# we group the data by year, school_type and coverage
lmf <- lmf %>% 
  group_by(year, school_type, perc.cov.obs) %>%
  summarise(est_retained=sum(est_retained), est_discarded_dead=sum(est_discarded_dead), 
            est_discarded_alive=sum(est_discarded_alive))

# transform this new estimations into percentages
lmf$perc_disc_dead <- ifelse(lmf$est_discarded_dead==0, 0, (lmf$est_discarded_dead*100/(lmf$est_discarded_dead+lmf$est_discarded_alive+lmf$est_retained)))
lmf$perc_disc_alive <- ifelse(lmf$est_discarded_alive==0, 0, (lmf$est_discarded_alive*100/(lmf$est_discarded_dead+lmf$est_discarded_alive+lmf$est_retained)))
lmf$perc_retained <- ifelse(lmf$est_retained==0, 0, (lmf$est_retained*100/(lmf$est_discarded_dead+lmf$est_discarded_alive+lmf$est_retained)))

# select only the columns that we want to use in this plot
lmf <- lmf[,c(1,2,3,7,8,9)]

lmf <- lmf %>% rename("Discarded dead" = "perc_disc_dead")
lmf <- lmf %>% rename("Discarded alive" = "perc_disc_alive")
lmf <- lmf %>% rename("Retained" = "perc_retained")
lmf <- lmf %>% rename("observed" = "perc.cov.obs")
lmf$school_type <- ifelse(lmf$school_type == "FOB", "FAD", lmf$school_type)

# Convert the dataframe into long format
lmf_long <- melt(lmf, id.vars = c('year', 'school_type', 'observed'), variable.name = 'Destiny', value.name = 'percentage')

# Add fake data with NA
years_to_add <- data.frame(
  year = 2010:2011,
  school_type = rep(unique(lmf_long$school_type), each = 2),
  Destiny = "Retained",
  percentage = 0,
  observed = 0
)

# combine with the original data frame
lmf_long <- rbind(lmf_long, years_to_add)

# Create the barplot
ggplot(lmf_long, aes(x = factor(year), y = percentage, fill = Destiny)) +
  geom_bar(stat = 'identity', position = 'stack', width = 0.85) +
  facet_wrap(~ school_type) +
  labs(x = 'Year', y = '%', title = "Destiny of LMF catches by the Spanish purse seine fleet") +
  scale_x_discrete(drop = FALSE, breaks = function(x) x[seq(1, length(x), by = 3)]) +
  theme(plot.title = element_text(hjust = 0.5, size = 18), strip.text = element_text(size = 12),
        legend.title = element_text(size = 14),  
        legend.text = element_text(size = 12),  
        axis.text.x = element_text(size = 12),  
        axis.text.y = element_text(size = 12),  
        axis.title.x = element_text(size = 14),  
        axis.title.y = element_text(size = 14)  
  ) +
  geom_text(data = lmf_long %>% filter(observed != 0) %>% group_by(year, school_type, Destiny) %>% summarise(observed = first(observed), .groups = 'drop'), 
            aes(x = factor(year), y = 106, label = sprintf("%.1f", observed)), 
            angle = 60, size = 3, na.rm = TRUE)

#################################################

########### SEF ###########
sef <- bycatch_spain %>% filter(fao_code=="FLY")

# select only the columns that we want to use in this plot
sef <- sef[,c(1,2,7,17,18,19)]

sef <- sef %>% rename("Discarded dead" = "perc_disc_dead")
sef <- sef %>% rename("Discarded alive" = "perc_disc_alive")
sef <- sef %>% rename("Retained" = "perc_retained")
sef <- sef %>% rename("observed" = "perc.cov.obs")
sef$school_type <- ifelse(sef$school_type == "FOB", "FAD", sef$school_type)

# Convert the dataframe into long format
sef_long <- melt(sef, id.vars = c('year', 'school_type', 'observed'), variable.name = 'Destiny', value.name = 'percentage')

# Add fake data with NA
years_to_add <- data.frame(
  year = 2010:2011,
  school_type = rep(unique(sef_long$school_type), each = 2),
  Destiny = "Retained",
  percentage = 0,
  observed = 0
)

# combine with the original data frame
sef_long <- rbind(sef_long, years_to_add)

# Create the barplot
ggplot(sef_long, aes(x = factor(year), y = percentage, fill = Destiny)) +
  geom_bar(stat = 'identity', position = 'stack', width = 0.85) +
  facet_wrap(~ school_type) +
  labs(x = 'Year', y = '%', title = "Destiny of SEF catches by the Spanish purse seine fleet") +
  scale_x_discrete(drop = FALSE, breaks = function(x) x[seq(1, length(x), by = 3)]) +
  theme(plot.title = element_text(hjust = 0.5, size = 18), strip.text = element_text(size = 12),
        legend.title = element_text(size = 14),  
        legend.text = element_text(size = 12),  
        axis.text.x = element_text(size = 12),  
        axis.text.y = element_text(size = 12),  
        axis.title.x = element_text(size = 14),  
        axis.title.y = element_text(size = 14)  
  ) +
  geom_text(data = sef_long %>% filter(observed != 0) %>% group_by(year, school_type, Destiny) %>% summarise(observed = first(observed), .groups = 'drop'), 
            aes(x = factor(year), y = 106, label = sprintf("%.1f", observed)), 
            angle = 60, size = 3, na.rm = TRUE)

###########################




################### Saving the datasets x FG ####

########### BET ###########
bet <- bycatch_spain %>% filter(fao_code=="BET")

bet <- bet %>% rename("Discarded dead" = "perc_disc_dead")
bet <- bet %>% rename("Discarded alive" = "perc_disc_alive")
bet <- bet %>% rename("Retained" = "perc_retained")
bet <- bet %>% rename("observed" = "perc.cov.obs")
bet$school_type <- ifelse(bet$school_type == "FOB", "FAD", bet$school_type)

# select only the columns that we want to save
bet <- bet[,c(1,2,4,14,15,16)]

write.csv(bet, "BET_bycatch_x1000t_TT_ESP_PS_TIO.csv")

###########################

########### YFT ###########
yft <- bycatch_spain %>% filter(fao_code=="YFT")

yft <- yft %>% rename("Discarded dead" = "perc_disc_dead")
yft <- yft %>% rename("Discarded alive" = "perc_disc_alive")
yft <- yft %>% rename("Retained" = "perc_retained")
yft <- yft %>% rename("observed" = "perc.cov.obs")
yft$school_type <- ifelse(yft$school_type == "FOB", "FAD", yft$school_type)

# select only the columns that we want to save
yft <- yft[,c(1,2,4,14,15,16)]

write.csv(yft, "YFT_bycatch_x1000t_TT_ESP_PS_TIO.csv")

###########################

########### SKJ ###########
skj <- bycatch_spain %>% filter(fao_code=="SKJ")

skj <- skj %>% rename("Discarded dead" = "perc_disc_dead")
skj <- skj %>% rename("Discarded alive" = "perc_disc_alive")
skj <- skj %>% rename("Retained" = "perc_retained")
skj <- skj %>% rename("observed" = "perc.cov.obs")
skj$school_type <- ifelse(skj$school_type == "FOB", "FAD", skj$school_type)

# select only the columns that we want to save
skj <- skj[,c(1,2,4,14,15,16)]

write.csv(skj, "SKJ_bycatch_x1000t_TT_ESP_PS_TIO.csv")

###########################

########### BSH ###########
bsh <- bycatch_spain %>% filter(fao_code=="BSH")

bsh <- bsh %>% rename("Discarded dead" = "perc_disc_dead")
bsh <- bsh %>% rename("Discarded alive" = "perc_disc_alive")
bsh <- bsh %>% rename("Retained" = "perc_retained")
bsh <- bsh %>% rename("observed" = "perc.cov.obs")
bsh$school_type <- ifelse(bsh$school_type == "FOB", "FAD", bsh$school_type)

# select only the columns that we want to save
bsh <- bsh[,c(1,2,4,14,15,16)]

write.csv(bsh, "BSH_bycatch_x1000t_TT_ESP_PS_TIO.csv")

###########################

########### FAL ###########
fal <- bycatch_spain %>% filter(fao_code=="FAL")

fal <- fal %>% rename("Discarded dead" = "perc_disc_dead")
fal <- fal %>% rename("Discarded alive" = "perc_disc_alive")
fal <- fal %>% rename("Retained" = "perc_retained")
fal <- fal %>% rename("observed" = "perc.cov.obs")
fal$school_type <- ifelse(fal$school_type == "FOB", "FAD", fal$school_type)

# select only the columns that we want to save
fal <- fal[,c(1,2,4,14,15,16)]

write.csv(fal, "FAL_bycatch_x1000t_TT_ESP_PS_TIO.csv")

###########################

########### Sharks ###########
shk <- bycatch_spain %>% filter(species_group=="Sharks" & fao_code!="FAL" & fao_code!="BSH")

# select only the columns that we want to use in this plot
shk <- shk[,c(1,2,6,7,11,12,13)]

# Calculate the proportion x1000t of tropical tunas caught
shk$prop_disc_dead <- ifelse(shk$est_discarded_dead==0, 0, ((shk$est_discarded_dead*1000)/shk$reported_total_catch_tons))
shk$prop_reteined <- ifelse(shk$est_retained==0, 0, ((shk$est_retained*1000)/shk$reported_total_catch_tons))
shk$prop_disc_alive <- ifelse(shk$est_discarded_alive==0, 0, ((shk$est_discarded_alive*1000)/shk$reported_total_catch_tons))

# we group the data by year, school_type and coverage
shk <- shk %>% 
  group_by(year, school_type, perc.cov.obs) %>%
  summarise(prop_reteined=sum(prop_reteined), prop_disc_dead=sum(prop_disc_dead), 
            prop_disc_alive=sum(prop_disc_alive))

# select only the columns that we want to use in this plot
shk <- shk[,-c(3)]

shk$school_type <- ifelse(shk$school_type == "FOB", "FAD", shk$school_type)

write.csv(shk, "SHK_bycatch_x1000t_TT_ESP_PS_TIO.csv")

###########################

########### RHN ###########
rhn <- bycatch_spain %>% filter(fao_code=="RHN")

rhn <- rhn %>% rename("Discarded dead" = "perc_disc_dead")
rhn <- rhn %>% rename("Discarded alive" = "perc_disc_alive")
rhn <- rhn %>% rename("Retained" = "perc_retained")
rhn <- rhn %>% rename("observed" = "perc.cov.obs")
rhn$school_type <- ifelse(rhn$school_type == "FOB", "FAD", rhn$school_type)

# select only the columns that we want to save
rhn <- rhn[,c(1,2,4,14,15,16)]

write.csv(rhn, "RHN_bycatch_x1000t_TT_ESP_PS_TIO.csv")

###########################

########### Turtles ###########
turtles <- bycatch_spain %>% filter(species_group=="Turtles")

# select only the columns that we want to use in this plot
turtles <- turtles[,c(1,2,6,7,11,12,13)]

# Calculate the proportion x1000t of tropical tunas caught
turtles$prop_disc_dead <- ifelse(turtles$est_discarded_dead==0, 0, ((turtles$est_discarded_dead*1000)/turtles$reported_total_catch_tons))
turtles$prop_reteined <- ifelse(turtles$est_retained==0, 0, ((turtles$est_retained*1000)/turtles$reported_total_catch_tons))
turtles$prop_disc_alive <- ifelse(turtles$est_discarded_alive==0, 0, ((turtles$est_discarded_alive*1000)/turtles$reported_total_catch_tons))

# we group the data by year, school_type and coverage
turtles <- turtles %>% 
  group_by(year, school_type, perc.cov.obs) %>%
  summarise(prop_reteined=sum(prop_reteined), prop_disc_dead=sum(prop_disc_dead), 
            prop_disc_alive=sum(prop_disc_alive))

# select only the columns that we want to use in this plot
turtles <- turtles[,-c(3)]

turtles$school_type <- ifelse(turtles$school_type == "FOB", "FAD", turtles$school_type)

write.csv(turtles, "Turtles_bycatch_x1000t_TT_ESP_PS_TIO.csv")

###########################

########### Rays ###########
rays <- bycatch_spain %>% filter(species_group=="Rays")

# select only the columns that we want to use in this plot
rays <- rays[,c(1,2,6,7,11,12,13)]

# Calculate the proportion x1000t of tropical tunas caught
rays$prop_disc_dead <- ifelse(rays$est_discarded_dead==0, 0, ((rays$est_discarded_dead*1000)/rays$reported_total_catch_tons))
rays$prop_reteined <- ifelse(rays$est_retained==0, 0, ((rays$est_retained*1000)/rays$reported_total_catch_tons))
rays$prop_disc_alive <- ifelse(rays$est_discarded_alive==0, 0, ((rays$est_discarded_alive*1000)/rays$reported_total_catch_tons))

# we group the data by year, school_type and coverage
rays <- rays %>% 
  group_by(year, school_type, perc.cov.obs) %>%
  summarise(prop_reteined=sum(prop_reteined), prop_disc_dead=sum(prop_disc_dead), 
            prop_disc_alive=sum(prop_disc_alive))

# select only the columns that we want to use in this plot
rays <- rays[,-c(3)]

rays$school_type <- ifelse(rays$school_type == "FOB", "FAD", rays$school_type)

write.csv(rays, "Rays_bycatch_x1000t_TT_ESP_PS_TIO.csv")

###########################

########### Mobulas ###########
mobulas <- bycatch_spain %>% filter(species_group=="mobulas")

# select only the columns that we want to use in this plot
mobulas <- mobulas[,c(1,2,6,7,11,12,13)]

# Calculate the proportion x1000t of tropical tunas caught
mobulas$prop_disc_dead <- ifelse(mobulas$est_discarded_dead==0, 0, ((mobulas$est_discarded_dead*1000)/mobulas$reported_total_catch_tons))
mobulas$prop_reteined <- ifelse(mobulas$est_retained==0, 0, ((mobulas$est_retained*1000)/mobulas$reported_total_catch_tons))
mobulas$prop_disc_alive <- ifelse(mobulas$est_discarded_alive==0, 0, ((mobulas$est_discarded_alive*1000)/mobulas$reported_total_catch_tons))

# we group the data by year, school_type and coverage
mobulas <- mobulas %>% 
  group_by(year, school_type, perc.cov.obs) %>%
  summarise(prop_reteined=sum(prop_reteined), prop_disc_dead=sum(prop_disc_dead), 
            prop_disc_alive=sum(prop_disc_alive))

# select only the columns that we want to use in this plot
mobulas <- mobulas[,-c(3)]

mobulas$school_type <- ifelse(mobulas$school_type == "FOB", "FAD", mobulas$school_type)

write.csv(mobulas, "Mobulas_bycatch_x1000t_TT_ESP_PS_TIO.csv")

###########################

########### Billfishes ###########
billfishes <- bycatch_spain %>% filter(species_group=="Billfishes" & fao_code!="SWO")

# select only the columns that we want to use in this plot
billfishes <- billfishes[,c(1,2,6,7,11,12,13)]

# Calculate the proportion x1000t of tropical tunas caught
billfishes$prop_disc_dead <- ifelse(billfishes$est_discarded_dead==0, 0, ((billfishes$est_discarded_dead*1000)/billfishes$reported_total_catch_tons))
billfishes$prop_reteined <- ifelse(billfishes$est_retained==0, 0, ((billfishes$est_retained*1000)/billfishes$reported_total_catch_tons))
billfishes$prop_disc_alive <- ifelse(billfishes$est_discarded_alive==0, 0, ((billfishes$est_discarded_alive*1000)/billfishes$reported_total_catch_tons))

# we group the data by year, school_type and coverage
billfishes <- billfishes %>% 
  group_by(year, school_type, perc.cov.obs) %>%
  summarise(prop_reteined=sum(prop_reteined), prop_disc_dead=sum(prop_disc_dead), 
            prop_disc_alive=sum(prop_disc_alive))

# select only the columns that we want to use in this plot
billfishes <- billfishes[,-c(3)]

billfishes$school_type <- ifelse(billfishes$school_type == "FOB", "FAD", billfishes$school_type)

write.csv(billfishes, "Billfishes_bycatch_x1000t_TT_ESP_PS_TIO.csv")

###########################

########### SWO ###########
swo <- bycatch_spain %>% filter(fao_code=="SWO")

swo <- swo %>% rename("Discarded dead" = "perc_disc_dead")
swo <- swo %>% rename("Discarded alive" = "perc_disc_alive")
swo <- swo %>% rename("Retained" = "perc_retained")
swo <- swo %>% rename("observed" = "perc.cov.obs")
swo$school_type <- ifelse(swo$school_type == "FOB", "FAD", swo$school_type)

# select only the columns that we want to save
swo <- swo[,c(1,2,4,14,15,16)]

write.csv(swo, "SWO_bycatch_x1000t_TT_ESP_PS_TIO.csv")

###########################

########### ALB ###########
alb <- bycatch_spain %>% filter(fao_code=="ALB")

alb <- alb %>% rename("Discarded dead" = "perc_disc_dead")
alb <- alb %>% rename("Discarded alive" = "perc_disc_alive")
alb <- alb %>% rename("Retained" = "perc_retained")
alb <- alb %>% rename("observed" = "perc.cov.obs")
alb$school_type <- ifelse(alb$school_type == "FOB", "FAD", alb$school_type)

# select only the columns that we want to save
alb <- alb[,c(1,2,4,14,15,16)]

write.csv(alb, "ALB_bycatch_x1000t_TT_ESP_PS_TIO.csv")

###########################

########### Small tunas and other scombrids ###########
scombri <- bycatch_spain %>% filter(fao_code=="BLT" | fao_code=="FRI" | 
                                      fao_code=="MAX" | fao_code=="KAW" | fao_code=="WAH")

# select only the columns that we want to use in this plot
scombri <- scombri[,c(1,2,6,7,11,12,13)]

# Calculate the proportion x1000t of tropical tunas caught
scombri$prop_disc_dead <- ifelse(scombri$est_discarded_dead==0, 0, ((scombri$est_discarded_dead*1000)/scombri$reported_total_catch_tons))
scombri$prop_reteined <- ifelse(scombri$est_retained==0, 0, ((scombri$est_retained*1000)/scombri$reported_total_catch_tons))
scombri$prop_disc_alive <- ifelse(scombri$est_discarded_alive==0, 0, ((scombri$est_discarded_alive*1000)/scombri$reported_total_catch_tons))

# we group the data by year, school_type and coverage
scombri <- scombri %>% 
  group_by(year, school_type, perc.cov.obs) %>%
  summarise(prop_reteined=sum(prop_reteined), prop_disc_dead=sum(prop_disc_dead), 
            prop_disc_alive=sum(prop_disc_alive))

# select only the columns that we want to use in this plot
scombri <- scombri[,-c(3)]

scombri$school_type <- ifelse(scombri$school_type == "FOB", "FAD", scombri$school_type)

write.csv(scombri, "Small_tunas_bycatch_x1000t_TT_ESP_PS_TIO.csv")

###########################

########### Jacks, mackerels, runners and other carangids ###########
caran <- bycatch_spain %>% filter(fao_code=="RRU" | fao_code=="YTL" | 
                                    fao_code=="CXS" | fao_code=="CGX" | fao_code=="NAU" | fao_code=="YTC"|
                                    fao_code=="USE" | fao_code=="MSD" | fao_code=="NGT"
                                  | fao_code=="UKK" | fao_code=="URU" | fao_code=="VMA" | fao_code=="YTL")

# select only the columns that we want to use in this plot
caran <- caran[,c(1,2,6,7,11,12,13)]

# Calculate the proportion x1000t of tropical tunas caught
caran$prop_disc_dead <- ifelse(caran$est_discarded_dead==0, 0, ((caran$est_discarded_dead*1000)/caran$reported_total_catch_tons))
caran$prop_reteined <- ifelse(caran$est_retained==0, 0, ((caran$est_retained*1000)/caran$reported_total_catch_tons))
caran$prop_disc_alive <- ifelse(caran$est_discarded_alive==0, 0, ((caran$est_discarded_alive*1000)/caran$reported_total_catch_tons))

# we group the data by year, school_type and coverage
caran <- caran %>% 
  group_by(year, school_type, perc.cov.obs) %>%
  summarise(prop_reteined=sum(prop_reteined), prop_disc_dead=sum(prop_disc_dead), 
            prop_disc_alive=sum(prop_disc_alive))

# select only the columns that we want to use in this plot
caran <- caran[,-c(3)]

caran$school_type <- ifelse(caran$school_type == "FOB", "FAD", caran$school_type)

write.csv(caran, "Carangids_bycatch_x1000t_TT_ESP_PS_TIO.csv")

###########################

########### EF - I ###########
efI <- bycatch_spain %>% filter(fao_code=="BAF" | fao_code=="DOX" | 
                                  fao_code=="DOL" | fao_code=="CFW" |
                                  fao_code=="BTS" | fao_code=="LOB"
                                | fao_code=="GBA" | fao_code=="MZZ" | fao_code=="BEN" | fao_code=="BAZ")

# select only the columns that we want to use in this plot
efI <- efI[,c(1,2,6,7,11,12,13)]

# Calculate the proportion x1000t of tropical tunas caught
efI$prop_disc_dead <- ifelse(efI$est_discarded_dead==0, 0, ((efI$est_discarded_dead*1000)/efI$reported_total_catch_tons))
efI$prop_reteined <- ifelse(efI$est_retained==0, 0, ((efI$est_retained*1000)/efI$reported_total_catch_tons))
efI$prop_disc_alive <- ifelse(efI$est_discarded_alive==0, 0, ((efI$est_discarded_alive*1000)/efI$reported_total_catch_tons))

# we group the data by year, school_type and coverage
efI <- efI %>% 
  group_by(year, school_type, perc.cov.obs) %>%
  summarise(prop_reteined=sum(prop_reteined), prop_disc_dead=sum(prop_disc_dead), 
            prop_disc_alive=sum(prop_disc_alive))

# select only the columns that we want to use in this plot
efI <- efI[,-c(3)]

efI$school_type <- ifelse(efI$school_type == "FOB", "FAD", efI$school_type)

write.csv(efI, "EF-I_bycatch_x1000t_TT_ESP_PS_TIO.csv")

###########################

########### EF - II ###########
efII <- bycatch_spain %>% filter(fao_code=="DIY" | fao_code=="DIO" | fao_code=="LGH" | 
                                   fao_code=="KYS" | fao_code=="KYP" | fao_code=="KYV" |
                                   fao_code=="BAO" | fao_code=="BAT" | fao_code=="MRW" |
                                   fao_code=="MOX"| fao_code=="ALM" | fao_code=="ALN" |
                                   fao_code=="AJS" | fao_code=="CNT" | fao_code=="UDA" |
                                   fao_code=="TRI" | fao_code=="NHR" | fao_code=="ECN" |
                                   fao_code=="EHN" | fao_code=="SPA" | fao_code=="KYC" |
                                   fao_code=="REO" | fao_code=="RRL" | fao_code=="BSX" |
                                   fao_code=="PUX" | fao_code=="USE" | fao_code=="UDD" )


# select only the columns that we want to use in this plot
efII <- efII[,c(1,2,6,7,11,12,13)]

# Calculate the proportion x1000t of tropical tunas caught
efII$prop_disc_dead <- ifelse(efII$est_discarded_dead==0, 0, ((efII$est_discarded_dead*1000)/efII$reported_total_catch_tons))
efII$prop_reteined <- ifelse(efII$est_retained==0, 0, ((efII$est_retained*1000)/efII$reported_total_catch_tons))
efII$prop_disc_alive <- ifelse(efII$est_discarded_alive==0, 0, ((efII$est_discarded_alive*1000)/efII$reported_total_catch_tons))

# we group the data by year, school_type and coverage
efII <- efII %>% 
  group_by(year, school_type, perc.cov.obs) %>%
  summarise(prop_reteined=sum(prop_reteined), prop_disc_dead=sum(prop_disc_dead), 
            prop_disc_alive=sum(prop_disc_alive))

# select only the columns that we want to use in this plot
efII <- efII[,-c(3)]

efII$school_type <- ifelse(efII$school_type == "FOB", "FAD", efII$school_type)

write.csv(efII, "EF-II_bycatch_x1000t_TT_ESP_PS_TIO.csv")

###########################

########### LMF ###########
lmf <- bycatch_spain %>% filter(fao_code=="POA" | fao_code=="BRZ" | fao_code=="GES" | 
                                   fao_code=="LAG" | fao_code=="OIL")


# select only the columns that we want to use in this plot
lmf <- lmf[,c(1,2,6,7,11,12,13)]

# Calculate the proportion x1000t of tropical tunas caught
lmf$prop_disc_dead <- ifelse(lmf$est_discarded_dead==0, 0, ((lmf$est_discarded_dead*1000)/lmf$reported_total_catch_tons))
lmf$prop_reteined <- ifelse(lmf$est_retained==0, 0, ((lmf$est_retained*1000)/lmf$reported_total_catch_tons))
lmf$prop_disc_alive <- ifelse(lmf$est_discarded_alive==0, 0, ((lmf$est_discarded_alive*1000)/lmf$reported_total_catch_tons))

# we group the data by year, school_type and coverage
lmf <- lmf %>% 
  group_by(year, school_type, perc.cov.obs) %>%
  summarise(prop_reteined=sum(prop_reteined), prop_disc_dead=sum(prop_disc_dead), 
            prop_disc_alive=sum(prop_disc_alive))

# select only the columns that we want to use in this plot
lmf <- lmf[,-c(3)]

lmf$school_type <- ifelse(lmf$school_type == "FOB", "FAD", lmf$school_type)

write.csv(lmf, "LMF_bycatch_x1000t_TT_ESP_PS_TIO.csv")

###########################

########### SEF ###########
sef <- bycatch_spain %>% filter(fao_code=="FLY")

sef <- sef %>% rename("Discarded dead" = "perc_disc_dead")
sef <- sef %>% rename("Discarded alive" = "perc_disc_alive")
sef <- sef %>% rename("Retained" = "perc_retained")
sef <- sef %>% rename("observed" = "perc.cov.obs")
sef$school_type <- ifelse(sef$school_type == "FOB", "FAD", sef$school_type)

# select only the columns that we want to save
sef <- sef[,c(1,2,4,14,15,16)]

write.csv(sef, "SEF_bycatch_x1000t_TT_ESP_PS_TIO.csv")

###########################
