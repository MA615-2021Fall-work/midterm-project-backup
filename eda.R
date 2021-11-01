source("ultimate data wrangle and clean.R")


library(ggplot2)
library(maps)

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
MergedStates <- inner_join(MainStates, chemical_clean, by = "State")

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
  geom_polygon( data=eda_subset, aes(x=long, y=lat, group=group, fill = mean_toxicity), color="white", size = 0.2) 
print(edaplot2)
