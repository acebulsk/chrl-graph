#### monthly normals plots ####

output$header4 <- renderUI({
  req(input$monthly_site)
  str1 <- paste0("<h2>", station_meta[[input$monthly_site]][1], " (", station_meta[[input$monthly_site]][2], " m)", "</h2>")
  HTML(paste(str1))
})

# final data set

finalData <- reactive({
  req(input$monthly_site)
  file_path <- paste0('data/monthly_normals_plot_data/', input$monthly_site, '_monthly_normals_data.rds')
  df <- readRDS(file_path)

  return(df)
})

globAverage <- reactive({
  req(input$monthly_site)
  file_path <- paste0('data/glob_average/', input$monthly_site, '_glob_average.rds')
  df <- readRDS(file_path)
  
  return(df)
})

# output$table <- renderTable({
#   finalData()
# })

output$plot <- renderPlot({
  req(input$monthly_site)
  req(input$plot_type)
  monthly_air_temp_summary <- finalData()
  glob_avg <- globAverage()
  
  min_year <- min(monthly_air_temp_summary$year) |> as.numeric()
  max_year <- max(monthly_air_temp_summary$year) |> as.numeric()
  monthly_air_temp_summary$is_year <- monthly_air_temp_summary$year == max_year
  
  color_palette <- c(viridisLite::viridis(n = (max_year - min_year),
                                          option = 'D'),'black')
  
  if(input$plot_type == 'Line Graph'){
    ggplot(monthly_air_temp_summary, aes(month_num, mean_monthly_temp))  +
      geom_ribbon(
        aes(
          ymin = glob_lower_quantile,
          ymax = glob_upper_quantile,
          fill = '5th to 95th\npercentile',
          group = 1
        ),
        alpha = 0.2
      ) +
      geom_line(aes(
        colour = as.factor(year),
        group = as.factor(year),
      ),
      linewidth = ifelse(monthly_air_temp_summary$year == max_year, 1, 0.5)) +
      geom_line(data = glob_avg, aes(x = month_num, y = glob_max_monthly_temp, group = 1, linetype = 'Max')) +
      geom_line(data = glob_avg, aes(x = month_num, y = glob_mean_monthly_temp, group = 1, linetype = 'Mean')) +
      geom_line(data = glob_avg, aes(x = month_num, y = glob_min_monthly_temp, group = 1, linetype = 'Min')) +
      scale_color_manual(name = 'Year', values = color_palette) +  # Assign colors based on the year
      scale_linetype_manual(name = 'Line Type', values = c("Mean" = "dashed", "Max" = "dashed", "Min" = "dashed")) +
      scale_fill_manual(name = 'Fill', values = c('5th to 95th\npercentile' = 'grey')) +
      labs(x = "Month", y = "Mean Monthly Air Temperature (°C)") +
      theme_bw(base_size = 14) +
      theme(legend.position = 'bottom')
      
  } else {
    ggplot(monthly_air_temp_summary, aes(month_num, mean_monthly_temp)) + 
      geom_boxplot() +
      geom_point(data = subset(monthly_air_temp_summary, year == max_year), aes(shape = "Current Year"), color = 'red', size = 3) +
      scale_shape_manual(values = c("Current Year" = 2)) +
      labs(x = "Month", y = "Mean Monthly Air Temperature (°C)",
           caption = paste0('Period of Record: ', min_year, ' to ', max_year, '. Only includes months with > 90% of data.'))  +
      theme_bw(base_size = 14) +
      theme(legend.title = element_blank())
  }
  


}, res = 96)

# plot for custom graphs page
output$plot1 <- renderPlotly({
  req(input$custom_site)
  req(input$custom_year)
  req(input$custom_var)
  req(finalData())
  
  df <- finalData() %>%
    select(DateTime, input$custom_var)
  
  varNames <- names(Filter(function(x) unlist(x) %in% input$custom_var, varsDict))
  
  if(length(input$custom_var) ==  2){
    
    weatherdash::graph_two(
      data = df,
      x = "DateTime",
      y1 = 2,
      y2 = 3, 
      y1_name = varNames[1], 
      y2_name = varNames[2]
    )
    
  } else {
    
    weatherdash::graph_one(
      data = df,
      x = "DateTime",
      y1 = 2,
      y1_name = varNames[1]
    )
  }
  
})
