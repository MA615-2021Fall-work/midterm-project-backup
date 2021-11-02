library(ggplot2)
library(maps)

human_toxicity_level_function <- function(data){
  ggplot(data)+
    geom_treemap(stat = "identity")+
    aes(area = Freq, fill= Toxicityhumenlevel, label = FreqPerc)+ 
    geom_treemap_text(
      color = "white",
      place = "center",
      size = 15
    )
}


chemicaltype_freq_function <- function(data){
  ggplot(data, aes(x = year, y = Freq, fill = year)) + 
    geom_bar(position="dodge", stat="identity")+
    facet_wrap(~Chemicaltype)+
    ggtitle("Comparison of Pesidicide usage: 2016 vs. 2018 vs. 2019")
}


chemcialtype_freq_barchart_function <- function(data){
  ggplot(data)+
    geom_bar(stat="identity") + aes(x = year, y = Freq, fill = Chemicaltype)+
    ggtitle("Total Pesiticide usage: 2016 vs. 2018 vs. 2019")
}


map_function <- function(data){
  ggplot() +
    geom_polygon( data, mapping = aes(x=long, y=lat, group=group, fill = mean_toxicity), color="white", size = 0.2)+
    ggtitle("The map for major strawberry producing area in the US")
}
