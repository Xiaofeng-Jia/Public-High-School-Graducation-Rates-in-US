library(shiny)
library(leaflet)
library(dplyr)
library(ggplot2)

#question 10/4 1. size of barplot 2. circle maekers 3. the width of the sidebar 4. any suggestions


#read data
df = read.csv('/Users/crazycat/Documents/academic/5th year/MAS 627 R/final project/df.csv')

myPalette = colorFactor(c('#ffffb2', '#ffe990', '#ffdf4a', '#fecc5c', '#fead5e', '#fd8d3c', '#f16d38', '#f03b20', '#bd0026', '#950015'), 
                        levels=c('90-100', '80-90', '70-80', '60-70', '50-60', '40-50', '30-40', '20-30', '10-20', '0-10'),
                        na.color = 'grey'
)



#frontend
ui = fluidPage(
  titlePanel('Public High School Graduation Rates in United States'),
  sidebarLayout(
    
    sidebarPanel(
      conditionalPanel(condition = "input.tab_selected==1",
                       radioButtons('Categories', 'Students Demographics', choices = c('U.S. average',                                                'Children with disabilities',                                    'Economically disadvantaged students',                           'Foster Care students',
                                                                                       'Homeless Enrolled students',
                                                                                       'English Learner students',
                                                                                       'American Indian/Alaska Native students',
                                                                                       'Asian/Pacific Islander students',
                                                                                       'Black students',
                                                                                       'Hispanic/Latino students',
                                                                                       'White students',
                                                                                       'Two or More Races/Multiracial Students'))
                       
      ),
      conditionalPanel(condition = "input.tab_selected==2",
                       radioButtons('Categories2', 'Students Demographics', choices = c('U.S. average',                                                'Children with disabilities',                                    'Economically disadvantaged students',                           'Foster Care students',
                                                                                       'Homeless Enrolled students',
                                                                                       'English Learner students',
                                                                                       'American Indian/Alaska Native students',
                                                                                       'Asian/Pacific Islander students',
                                                                                       'Black students',
                                                                                       'Hispanic/Latino students',
                                                                                       'White students',
                                                                                       'Two or More Races/Multiracial Students'))
                       
      ),
      conditionalPanel(condition = "input.tab_selected==3",
                       downloadButton("downloadData", "Download")
      )
      
      
      
      
      
    ),#close side bar panel
    
    mainPanel(
      tabsetPanel(id = 'tab_selected',
        tabPanel('Map', leafletOutput('my_map'), value = 1) ,
        tabPanel('Barplot by State', plotOutput('my_barplot', height=1000), value = 2),
        tabPanel('View Source Data', dataTableOutput('my_data'), value = 3)
        )#close tabset panel
    ), #close main panel

    
  ) #closes sidebar layout
  
)#close fluid


server = function(input, output) {
  
  # map
  output$my_map = renderLeaflet(
    df %>%
      filter(CATEGORY == input$Categories) %>%
      leaflet() %>%
      addProviderTiles("CartoDB.Positron") %>%
      setView(-98.583333, 39.833333, zoom = 4) %>%
      addCircleMarkers(
        radius = 5,
        fillOpacity = 1,
        label = ~Rate,
        color = ~myPalette(RateLevel),
        clusterOptions = markerClusterOptions()
      ) %>%
      addLegend(
        position = 'bottomleft',
        pal = myPalette,
        values = ~RateLevel
      )
  )
  
  #barplot
  output$my_barplot = renderPlot(
    
    df %>% 
      group_by(STNAM, CATEGORY) %>% 
      summarise(AvgRate = mean(Rate, na.rm = TRUE)) %>% 
      filter(CATEGORY == input$Categories2) %>% 
      ggplot(aes(x= STNAM, y= AvgRate)) +
      geom_col(fill ='#fd8d3c', position = position_dodge(width = 0.8)) +
      coord_flip() +
      labs(title='', x = 'Student Demographics') +
      scale_y_continuous('Graduation Rate (%)') +
      theme(
        panel.background = element_rect(fill = 'white'),
        plot.title = element_text(size = 18),
        axis.ticks = element_line(size = 0.5),
        text = element_text(face='bold', size = 18)


      )
 )
  

  #plotOutput('my_barplot', height=5000)
  
  
  #source data
  output$my_data = renderDataTable(df)
  
  
  # Downloadable csv dataset ----
  output$downloadData <- downloadHandler(
    filename = function() {
      "graduation_rate_data.csv"
    },
    content = function(file) {
      write.csv(df, file, row.names = FALSE)
    }
  )
    
  
}


shinyApp(ui, server)