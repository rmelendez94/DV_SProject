# server.R
require("jsonlite")
require("RCurl")
require(ggplot2)
require(dplyr)
require(shiny)

shinyServer(function(input, output) {
  
  output$scatterPlot <- renderPlot({
    # Start your code here.
    # Here is the scatter plot
    
    # End your code here.
    return(plot)
  })
  
  output$barPlot <- renderPlot({
    # Start your code here.
    # Here is the bar chart
    
    # End your code here.
    return(plot)
  })

  output$crosstabPlot <- renderPlot({
# Start your code here.

# Here is the Crosstab and KPI
    
KPI_Low_Max_value = input$KPI1     
KPI_Medium_Max_value = input$KPI2
    
df %>% group_by(JOB) %>% summarize() %>% View()

dfc <- df %>% mutate(Yyes = ifelse(Y == 'yes', 1, 0), Yno = ifelse(Y == 'no', 1, 0)) %>% group_by(EDUCATION) %>% mutate(Ratio = sum(Yyes)/sum(Yno)) %>% ungroup() %>% group_by(EDUCATION, Y, HOUSING) %>% summarize(AVG_DURATION = round(mean(DURATION),1), Ratio = mean(Ratio)) %>% mutate(KPI = ifelse(Ratio <= KPI_Low_Max_value, '03 Low', ifelse(Ratio <= KPI_Medium_Max_value, '02 Medium', '01 High')))

#spread(dfc, Y, AVG_DURATION) %>% View

dfc$EDUCATION <- factor(dfc$EDUCATION, levels = c("illiterate", "basic4y", "basic6y", "basic9y", "highschool", "universitydegree", "professionalcourse", "unknown"))
    
plot <- ggplot() + 
  coord_cartesian() + 
  scale_x_discrete() +
  scale_y_discrete() +
  scale_fill_manual(values = c("green","yellow","red")) + 
  facet_grid(.~EDUCATION) + 
  labs(title='Portuguese Bank Marketing Campaign Effectiveness\nCrosstab\nAVG_DURATION') +
  labs(x=paste("EDUCATION/Y"), y=paste("HOUSING")) +
  layer(data=dfc, 
        mapping=aes(x=Y, y=HOUSING, label=AVG_DURATION), 
        stat="identity", 
        stat_params=list(), 
        geom="text",
        geom_params=list(colour="black", size=2.8), 
        position=position_identity()
  ) +
  layer(data=dfc, 
        mapping=aes(x=Y, y=HOUSING, fill=KPI), 
        stat="identity", 
        stat_params=list(), 
        geom="tile",
        geom_params=list(alpha=0.50), 
        position=position_identity()
  )

# End your code here.
      return(plot)
  })
})
