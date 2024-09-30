library(shiny)
library(ggplot2)
library(dplyr)
library(scales)
library(leaflet)

#color numeric
#pal <- colorNumeric(
#  palette = "Blues",
#  domain = countries$gdp_md_est)

df = read.csv('/Users/crazycat/Documents/academic/5th year/MAS 627 R/final project/df.csv')



myPalette = colorFactor(c('#ffffb2', '#ffe990', '#ffdf4a', '#fecc5c', '#fead5e', '#fd8d3c', '#f16d38', '#f03b20', '#bd0026', '#950015'), 
                        levels=c('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j'),
                        na.color = 'grey'
                        )

myPalette(df$RateLevel)

leaflet(df) %>%
  addProviderTiles(providers$Esri.NatGeoWorldMap) %>%
  addCircleMarkers(
    radius = 3,
    opacity = 1,
    label = ~Rate,
    color = ~myPalette(RateLevel),
    clusterOptions = markerClusterOptions()
  ) %>%
    addLegend(
    position = 'topright',
    pal = myPalette,
    values = ~RateLevel
  )

  
#barplot

df %>%
  filter() %>%
  group_by(STNAM) %>%
  summarise(AvgRate = mean(Rate)) %>%
  gpplot(aes(x= STNAM, y= AvgRate))





df %>% 
  group_by(STNAM, CATEGORY) %>% 
  summarise(AvgRate = mean(Rate, na.rm = TRUE)) %>% 
  filter(CATEGORY == 'U.S. average') %>% 
  ggplot(aes(x= STNAM, y= AvgRate)) +
  geom_col(fill ='#fd8d3c') +
  coord_flip() +
  labs(title='', x = 'Student Demographics') +
  scale_y_continuous('Graduation Rate (%)') +
  theme(
    panel.background = element_rect(fill = 'white'),
    plot.title = element_text(size = 18),
    axis.ticks = element_line(size = 0.5)
 
  )





+ theme(axis.text = element_text(face='bold', size=14), 
        text = element_text(face='bold', size=18))




