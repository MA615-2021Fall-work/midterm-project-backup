source("ultimate wrangle data.R")


library(ggplot2)
library(maps)
library(treemapify)
library(tidyverse)
## 
# chemical_clean <- chemical %>%
#  filter(!grepl("(D)", Value)) %>%
#  filter(!grepl("(NA)", Value)) %>%
#  filter(!grepl("(Z)", Value))
##

## filtering chemicals included in the pesti dataset
chemical_clean <- chemical %>%
  filter(grepl('ACETAMIPRID|AZOXYSTROBIN|BIFENAZATE|BIFENTHRIN|BOSCALID|CAPTAN|CARBARYL|CARBENDAZIM|CHLORPYRIFOS|CYPRODINIL|DICHLORVOS|DIMETHOATE|ENDOSULFAN|FENHEXAMID|FENPROPATHRIN|FLUDIOXONIL|HEXYTHIAZOX|IMIDACLOPRID|IPRODIONE|MALAOXON|MALATHION|METALAXYL|MEFENOXAM|METHOMYL|METHOXYFENOZIDE|MYCLOBUTANIL|OXAMYL|PIPERONYL|PROPICONAZOLE|PYRACLOSTROBIN|PYRIMETHANIL|PYRIPROXYFEN|QUINOXYFEN|SPINOSAD|SPIROMESIFEN|TEBUCONAZOLE|TETRAHYDROPHTHALIMIDE|THIABENDAZOLE|THIAMETHOXAM|TRIFLOXYSTROBIN|TRIFLUMIZOLE', Chemicaltype))
chemical_clean$Value <- as.numeric(chemical_clean_lbperacre$Value)

# Pounds per acre

chemical_clean_lbperacre <- chemical_clean %>%
  filter(measurement == " MEASURED IN LB / ACRE / YEAR  AVG")

chemical_clean_lbperacre$Value <- as.numeric(chemical_clean_lbperacre$Value)

sum(is.na(chemical_clean_lbperacre$Value))

# Percent area bearing

chemical_clean_bearing <- chemical_clean %>%
  filter(measurement == " MEASURED IN PCT OF AREA BEARING  AVG")

chemical_clean_bearing$Value <- as.numeric(chemical_clean_bearing$Value)

sum(is.na(chemical_clean_bearing$Value))

# Measured in lbs

chemical_clean_plainlb <- chemical_clean %>%
  filter(measurement == " MEASURED IN LB  ")

chemical_clean_plainlb$Value <- as.numeric(gsub(",","",chemical_clean_plainlb$Value))

sum(is.na(chemical_clean_plainlb$Value))

# Pound per acre per application

chemical_clean_applicationlbperacre <- chemical_clean %>%
  filter(measurement == " MEASURED IN LB / ACRE / APPLICATION  AVG")

chemical_clean_applicationlbperacre$Value <- as.numeric(chemical_clean_applicationlbperacre$Value)

sum(is.na(chemical_clean_applicationlbperacre$Value))

# Measured in number

chemical_clean_number <- chemical_clean %>%
  filter(measurement == " MEASURED IN NUMBER  AVG")

chemical_clean_number$Value <- as.numeric(chemical_clean_number$Value)

sum(is.na(chemical_clean_number$Value))




#####

# str(chemical_clean_bearing)

# model = lm(Value ~ toxicitylevelhuman, data = chemical_clean_lbperacre)
# summary(model)

# plot(model)

#####



## EDA Plots

## EDA PLOT 1
# Looking at the lbs of strawberries harvested per acre in each state.
# Added color to show the toxicity level of each observation. It appears
# that the harvests with higher lbs per acre also tend to be treated with 
# chemicals that are rated higher by out 'toxicitylevelhuman' variable.

edaplot <- ggplot() +
  geom_point(aes(x = chemical_clean_number$State, y = chemical_clean_number$Value, 
  ),col = chemical_clean_number$toxicitylevelhuman)
print(edaplot)



## EDA Plot 2

# First creating United States map
MainStates <- map_data("state")
names(MainStates)[5] <- 'State'
MainStates$State <- toupper(MainStates$State)
MergedStates <- full_join(MainStates, chemical_clean, by = "State")

# Creating subset to plot this map
eda_subset <- MergedStates %>%
  group_by(State) %>%
  summarise(mean_toxicity = mean(toxicitylevelhuman), State = State, lat = lat, long = long, group = group, order = order)

# Plotting original map
edaplot2 <- ggplot() + 
  geom_polygon( data=MainStates, aes(x=long, y=lat, group=group),
                color="black", fill="lightblue" )

# Adding the mean toxicity of all observations in each state to the map. This
# plot shows that, based on this dataset, the harvests in Oregon tended
# to use chemicals that were rated higher by our toxicity scale in
# comparison to other states
edaplot2 <- ggplot() +
  geom_polygon( data=eda_subset, aes(x=long, y=lat, group=group, fill = mean_toxicity), color="white", size = 0.2)+
  ggtitle("The map for major strawberry producing area in the US")
print(edaplot2)




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

ggplot(data = chemicaltype_freq, aes(x = year, y = Freq, fill = year)) + 
  geom_bar(position="dodge", stat="identity")+
  facet_wrap(~Chemicaltype)+
  ggtitle("Comparison of Pesidicide usage: 2016 vs. 2018 vs. 2019")



ggplot(data = chemicaltype_freq)+
  geom_bar(stat="identity") + aes(x = year, y = Freq, fill = Chemicaltype)


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






human_toxicity_level_function <- function(data){
  ggplot(data)+
    geom_treemap(stat = "identity") +
    aes(area = Freq, fill= Toxicityhumenlevel, label = FreqPerc) + 
    geom_treemap_text(
      color = "white",
      place = "center",
      size = 15
    )
}
  

human_toxicity_level_function(year1_toxicityhuman_freq) + ggtitle("Pesticide Toxicitylevelhuman Frequency 2016")
human_toxicity_level_function(year2_toxicityhuman_freq) + ggtitle("Pesticide Toxicitylevelhuman Frequency 2018")
