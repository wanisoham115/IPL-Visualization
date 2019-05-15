server <- function(input, output) {
  
  runs = function(vk)({
    vk_season = aggregate(batsman_runs ~ season, data = vk, FUN = sum)
    
    return (renderPlotly(
      ggplot(vk_season, aes(x=season, y = batsman_runs))+
        labs(x = "Season", y = "Runs Scored")+
        ggtitle("Runs by Seasons")+
        geom_bar(stat="identity", fill="lightblue")
    ))
  })
  
  dismissal = function(vk)({
    vk_dismissal_long = subset(vk, vk$player_dismissed == input$batsman)[,c("season", "dismissal_kind")]
    
    return (renderPlotly({
      ggplot(vk_dismissal_long, aes(x = season, y = ..count.. , fill = dismissal_kind)) +
        geom_bar(stat="count") +
        ggtitle("Dismissals by Seasons") +
        labs(x = "Season", y = "Dismissal Kind")+
        scale_fill_brewer(palette = "Set2")
    }))
  })
  
  strike_rate = function(vk)({
    
    vk_over_runs = aggregate(batsman_runs ~ over, data = vk, FUN = sum)
    vk_overs_faced = as.data.frame(table(vk$over))
    colnames(vk_overs_faced) = c("over", "vk_freq")
    
    vk_over_runs = merge(vk_over_runs, vk_overs_faced)
    vk_over_runs$strike_rate = (vk_over_runs$batsman_runs/vk_over_runs$vk_freq)*100
    vk_over_runs$over = as.factor(vk_over_runs$over)
    
    vk_over_runs$batsman_runs = NULL
    return (renderPlotly({
      ggplot(vk_over_runs, aes(over, strike_rate, col = strike_rate)) +  
        geom_point(size=3)+ 
        ggtitle("Strike Rate by Over") +
        labs(x = "Over", y = "Strike Rate")
    }))
  })
  
  fav_venue = function(vk){
    vk_fav_venue1 = aggregate(batsman_runs ~ venue, data = vk, FUN = sum)
    vk_fav_venue = head(vk_fav_venue1[with(vk_fav_venue1, order(-batsman_runs)),])
    
    return (renderPlotly(
      ggplot(vk_fav_venue, aes(x= venue, y = batsman_runs))+
        labs(x = "Venue", y = "Runs Scored")+
        ggtitle("Runs by Venue")+
        geom_bar(stat="identity", fill="orange")+
        coord_flip()
    ))
  }
  
  observeEvent((input$button), {
    
    vk = subset(df, df$batsman == input$batsman)
    
    if (input$function_name == "Dismissals by Seasons"){
      output$plot = dismissal(vk)
    }
    else if (input$function_name == "Strike Rate Plots"){
      output$plot = strike_rate(vk)
    }
    else if (input$function_name == "Runs by Seasons"){
      output$plot = runs(vk)
    }else if (input$function_name == "Favorite Venue"){
      output$plot = fav_venue(vk)
    }
  })  
}

