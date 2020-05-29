library(shiny)
library(dplyr)
library(tidyr)
library(httr)
library(jsonlite)

source("analysis.R")

server <- function(input, output) {
  
  output$map_plot <- renderPlot({
    
    num_attacks <- input$num_attacks
    group_name <- input$gname
    
    df <- map_terrorist_attacks_data
      
    if (num_attacks != "All") {
      df <- df %>% 
        filter(labels == num_attacks)
    }
    
    countries <- df %>% 
      filter(country_txt != "NA") %>%
      group_by(country_txt) %>%
      count(country_txt)
    
    ggplot(df) +
      geom_polygon(mapping = aes(x = long, y = lat, group = group, fill = labels, color = "white", guide = 'none')) +
      coord_quickmap() +
      scale_fill_brewer(palette = "YlOrRd") +
      labs(title = "Number of Terrorist Attacks", x = paste("Number of Countries in range:", nrow(countries)), fill = "Attacks") +
      guides(color = FALSE) +
      theme_bw() +
      theme(axis.text.x=element_blank(),
            axis.ticks.x=element_blank(),
            axis.title.y=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks.y=element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.border = element_blank(),
            panel.background = element_blank())
  })
  
  output$casVal <- renderPlot({
    yearChoice <- input$duration
    choice <- input$numArms
    
    df <- terrorism_arms_data
    df <- subset(df, Year <= yearChoice)
   
    if (choice == "Num_Casualties_Terrorism") {
      plot <- ggplot(data = df) +
        geom_bar(mapping = aes(x=as.numeric(Year),y=Num_Casualties_Terrorism),stat = "identity",fill="red")#  +
      # scale_y_continuous(sec.axis = sec_axis(~./max(terrorism_arms_data$sum_of_arms_imports_to_insurgents),name="Value of arms Sales($Millions)")) + 
      labs(title = "Number of casualities from Terrorsim",x="Year",y="Number of Casualties")
      return(plot)
    } else if (choice == "sum_of_arms_imports_to_insurgents") {
      plot <- ggplot(data = df) +
        geom_point(data = df,aes(x=as.numeric(Year),y=sum_of_arms_imports_to_insurgents*max(df$sum_of_arms_imports_to_insurgents))) +
        geom_line(data = df, aes(x=as.numeric(Year),y=sum_of_arms_imports_to_insurgents*max(df$sum_of_arms_imports_to_insurgents))) +
        # scale_y_continuous(sec.axis = sec_axis(~./max(terrorism_arms_data$sum_of_arms_imports_to_insurgents),name="Value of arms Sales($Millions)")) + 
        labs(title = "Arms Sales to non-State forces",x="Year",y="Arms Sales")
      return(plot)
    } else {
      plot <- ggplot(data = df) +
        geom_bar(mapping = aes(x=as.numeric(Year),y=Num_Casualties_Terrorism),stat = "identity",fill="red") +
        geom_point(data = df,aes(x=as.numeric(Year),y=sum_of_arms_imports_to_insurgents*max(df$sum_of_arms_imports_to_insurgents))) +
        geom_line(data =df, aes(x=as.numeric(Year),y=sum_of_arms_imports_to_insurgents*max(df$sum_of_arms_imports_to_insurgents))) +
        scale_y_continuous(sec.axis = sec_axis(~./max(df$sum_of_arms_imports_to_insurgents),name="Value of arms Sales($Millions)")) + 
        labs(title = "Arms Sales to non-State forces and Casualties from Terrorsim",x="Year",y="Number of Casualties")
      
      return(plot)
    }
   
  })
}
