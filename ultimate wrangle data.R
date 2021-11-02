library(shiny)
library(tidyverse)
library(dplyr)
library(magrittr)
pesti <- read.csv("Pesticides.csv",header = T, na.strings = c("", "NA"),stringsAsFactors=FALSE)
strawbwhole <- read.csv("Strawberries.csv",stringsAsFactors=FALSE)
#Drop empty columns
strawbwhole <- strawbwhole[-c(4,8:15)]
#Separate Data.Item to 4 columns
strawbwhole <- strawbwhole %<>% separate(col=Data.Item,
                                         into = c("Strawberries", "items", "discription",  "units"),
                                         sep = ",",
                                         fill = "right")
#Separate Domian to 2 columns
strawbwhole <- strawbwhole %<>% separate(col = Domain,
                                         into = c("Domain", "Chemical"),
                                         sep = ",",
                                         fill = "right")
#Subsetting by domain
organic_sub <- subset(strawbwhole, Domain == "ORGANIC STATUS")
chemical_sub <- subset(strawbwhole, Domain == "CHEMICAL")
fert_sub <- subset(strawbwhole, Domain == "FERTILIZER")
total_sub <- subset(strawbwhole, Domain == "TOTAL")


#Clean chemical subset
#Replace NAs in units with blanks
chemical_sub$units[is.na(chemical_sub$units)] <- " "
#Join the column discription and units
chemical_sub$units <- paste(chemical_sub$discription, chemical_sub$units)
#Create a new column for measurement
chemical_sub$measurement <- chemical_sub$units

chemical_sub$Domain.Category <- gsub("[()]", "", chemical_sub$Domain.Category)
chemical_sub$Domain.Category <- gsub(".*:", "", chemical_sub$Domain.Category) 

chemical_sub<- subset(chemical_sub, Domain.Category != " TOTAL")

chemical_sub <- chemical_sub %<>% separate(col = Domain.Category,
                                           into = c("Chemicaltype", "Code"),
                                           sep = "=",
                                           fill = "right")


chemical_sub <- chemical_sub[complete.cases(chemical_sub$Code),]


chemical_sub$Chemicaltype <- gsub('\\s+', '', chemical_sub$Chemicaltype)



pesti_chemical_sub <- pesti[rowSums(is.na(pesti)) != ncol(pesti),] 
pesti_chemical_sub$Pesticide <- toupper(pesti_chemical_sub$Pesticide)

#Combine two dataset for chemical subset
full_list_chemical_sub <- left_join(chemical_sub,pesti_chemical_sub, by = c("Chemicaltype" = "Pesticide"))
full_list_chemical_sub <- full_list_chemical_sub[!(is.na(full_list_chemical_sub$Year) | full_list_chemical_sub$Year=="NA"), ]

chemical <- full_list_chemical_sub

chemical$Carcinogen[is.na(chemical$Carcinogen)] <- "unknown"
chemical$Hormone.Disruptor[is.na(chemical$Hormone.Disruptor)] <- "unknown"
chemical$Neurotoxins[is.na(chemical$Neurotoxins)] <- "unknown"
chemical$Developmental.or.Reproductive.Toxins[is.na(chemical$Developmental.or.Reproductive.Toxins)] <- "unknown"
chemical$Bee.Toxins[is.na(chemical$Bee.Toxins)] <- "unknown"

#Give the toxins variable levels
chemical$Carcinogen <- factor(chemical$Carcinogen, levels = c("known","probable","possible","unknown"))
chemical$Carcinogen <- factor(chemical$Carcinogen, levels = rev(levels(chemical$Carcinogen)))

chemical$Hormone.Disruptor <- factor(chemical$Hormone.Disruptor, levels = c("suspected","unknown"))
chemical$Hormone.Disruptor <- factor(chemical$Hormone.Disruptor, levels = rev(levels(chemical$Hormone.Disruptor)))

chemical$Neurotoxins <- factor(chemical$Neurotoxins, levels = c("present","unknown"))
chemical$Neurotoxins <- factor(chemical$Neurotoxins, levels = rev(levels(chemical$Neurotoxins)))

chemical$Developmental.or.Reproductive.Toxins <- factor(chemical$Developmental.or.Reproductive.Toxins,levels = c("present","unknown"))
chemical$Developmental.or.Reproductive.Toxins <- factor(chemical$Developmental.or.Reproductive.Toxins, levels = rev(levels(chemical$Developmental.or.Reproductive.Toxins)))

chemical$Bee.Toxins <- factor(chemical$Bee.Toxins, levels = c("high","moderate","slight","unknown"))
chemical$Bee.Toxins <- factor(chemical$Bee.Toxins, levels = rev(levels(chemical$Bee.Toxins)))


#Create toxicity-level column for bee
chemical$toxicitylevelbee <- as.numeric(chemical$Bee.Toxins)

for (i in 1:length(chemical$Year)) {
  chemical$toxicitylevelhuman[i] = sum(as.numeric(chemical$Carcinogen[i]),as.numeric(chemical$Hormone.Disruptor[i]), as.numeric(chemical$Neurotoxins[i]), as.numeric(chemical$Developmental.or.Reproductive.Toxins[i]))
}

chemical <- subset(chemical, select = c(Year,State,Chemical,Chemicaltype,Value,measurement,Carcinogen,Hormone.Disruptor,Neurotoxins,Developmental.or.Reproductive.Toxins,toxicitylevelhuman,Bee.Toxins,toxicitylevelbee))


##Data cleaning for EDA:

## filtering chemicals included in the pesti dataset
chemical_clean <- chemical %>%
  filter(grepl('ACETAMIPRID|AZOXYSTROBIN|BIFENAZATE|BIFENTHRIN|BOSCALID|CAPTAN|CARBARYL|CARBENDAZIM|CHLORPYRIFOS|CYPRODINIL|DICHLORVOS|DIMETHOATE|ENDOSULFAN|FENHEXAMID|FENPROPATHRIN|FLUDIOXONIL|HEXYTHIAZOX|IMIDACLOPRID|IPRODIONE|MALAOXON|MALATHION|METALAXYL|MEFENOXAM|METHOMYL|METHOXYFENOZIDE|MYCLOBUTANIL|OXAMYL|PIPERONYL|PROPICONAZOLE|PYRACLOSTROBIN|PYRIMETHANIL|PYRIPROXYFEN|QUINOXYFEN|SPINOSAD|SPIROMESIFEN|TEBUCONAZOLE|TETRAHYDROPHTHALIMIDE|THIABENDAZOLE|THIAMETHOXAM|TRIFLOXYSTROBIN|TRIFLUMIZOLE', Chemicaltype))
chemical_clean$Value <- as.numeric(chemical_clean$Value)


## EDA Plot 1 map data

# First creating United States map
MainStates <- map_data("state")
names(MainStates)[5] <- 'State'
MainStates$State <- toupper(MainStates$State)
MergedStates <- full_join(MainStates, chemical_clean, by = "State")

# Creating subset to plot this map
eda_subset <- MergedStates %>%
  group_by(State) %>%
  summarise(mean_toxicity = mean(toxicitylevelhuman), State = State, lat = lat, long = long, group = group, order = order)



year1 <- subset(chemical_clean, Year == "2016" & measurement == " MEASURED IN LB  ")

year2 <- subset(chemical_clean, Year == "2018" & measurement == " MEASURED IN LB  ")

year3 <- subset(chemical_clean, Year == "2019" & measurement == " MEASURED IN LB  ")


year1_chemicaltype_freq <- data.frame(table(year1$Chemicaltype))
year1_chemicaltype_freq$year <- 2016
colnames(year1_chemicaltype_freq)[1] <- c("Chemicaltype")

year2_chemicaltype_freq <- data.frame(table(year2$Chemicaltype))
year2_chemicaltype_freq$year <- 2018
colnames(year2_chemicaltype_freq)[1] <- c("Chemicaltype")


year3_chemicaltype_freq <- data.frame(table(year3$Chemicaltype))
year3_chemicaltype_freq$year <- 2019
colnames(year3_chemicaltype_freq)[1] <- c("Chemicaltype")

chemicaltype_freq <- rbind(year1_chemicaltype_freq,year2_chemicaltype_freq,year3_chemicaltype_freq)
chemicaltype_freq$year <- factor(chemicaltype_freq$year)



year1_toxicityhuman_freq <- data.frame(table(year1$toxicitylevelhuman))
year1_toxicityhuman_freq <- year1_toxicityhuman_freq[-c(1),] 
colnames(year1_toxicityhuman_freq)[1] <- c("Toxicityhumenlevel")
year1_toxicityhuman_freq <-year1_toxicityhuman_freq %>% mutate(FreqPerc = Freq/sum(Freq))
year1_toxicityhuman_freq$FreqPerc <- round(year1_toxicityhuman_freq$FreqPerc, digits = 2)

year2_toxicityhuman_freq <- data.frame(table(year2$toxicitylevelhuman))
year2_toxicityhuman_freq <- year2_toxicityhuman_freq[-c(1),]
colnames(year2_toxicityhuman_freq)[1] <- c("Toxicityhumenlevel")
year2_toxicityhuman_freq <-year2_toxicityhuman_freq %>% mutate(FreqPerc = Freq/sum(Freq))
year2_toxicityhuman_freq$FreqPerc <- round(year2_toxicityhuman_freq$FreqPerc, digits = 2)

year3_toxicityhuman_freq <- data.frame(table(year3$toxicitylevelhuman))
year3_toxicityhuman_freq <- year3_toxicityhuman_freq[-c(1),]
colnames(year3_toxicityhuman_freq)[1] <- c("Toxicityhumenlevel")
year3_toxicityhuman_freq <-year3_toxicityhuman_freq %>% mutate(FreqPerc = Freq/sum(Freq))
year3_toxicityhuman_freq$FreqPerc <- round(year3_toxicityhuman_freq$FreqPerc, digits = 2)

