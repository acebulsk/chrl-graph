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
  df <- readRDS(file_path) |> 
    filter(name == input$monthly_var)

  return(df)
})

# globAverage <- reactive({
#   req(input$monthly_site)
#   file_path <- paste0('data/glob_average/', input$monthly_site, '_glob_average.rds')
#   df <- readRDS(file_path)
#   
#   return(df)
# })

# output$table <- renderTable({
#   finalData()
# })

observe({
  req(input$monthly_site)
  # need to find the year range of selected sites. finds the max of the two start years as the min.
  monthly_summary <- finalData()
  # glob_avg <- globAverage()
  min_year <- min(monthly_summary$wtr_year) |> as.numeric()
  max_year <- max(monthly_summary$wtr_year) |> as.numeric()
  year_range <- seq.int(min_year, max_year, by = 1)
  updateSelectInput(session, "monthly_year", "Select Year to Compare: ", year_range, selected = max_year)
})

output$plot <- renderPlotly({
  req(input$monthly_site)
  req(input$plot_type)
  req(input$monthly_year)
  
  select_year <- input$monthly_year
  monthly_summary <- finalData()
  
  monthly_summary$month_name <- month.abb[as.numeric(monthly_summary$month_num)]
  monthly_summary$month_name <- ordered(monthly_summary$month_name, levels = c(month.abb[10:12], month.abb[1:9]))

  
  min_year <- min(monthly_summary$wtr_year) |> as.numeric()
  max_year <- max(monthly_summary$wtr_year) |> as.numeric()
  year_range <- seq.int(min_year, max_year, by = 1)
  
  y_lab <- names(monthlyVarsDict)[monthlyVarsDict == input$monthly_var]
  
  color_palette <- c(viridisLite::viridis(n = (max_year-min_year)+1,
                                          option = 'D'))
  
  col_index <- which(year_range == select_year)
  
  color_palette[col_index] <- 'red'
  
  # glob_avg <- globAverage()
  
  # browser()
  if(input$plot_type == 'Line Graph'){
    gg_out <- ggplot(monthly_summary, aes(month_name, mean_monthly))  +
      geom_line(aes(
        colour = as.factor(wtr_year),
        group = as.factor(wtr_year),
      ),
      linetype = ifelse(monthly_summary$wtr_year == select_year, 'dashed', 'solid')
      ) +
      geom_point(aes(colour = as.factor(wtr_year)), size = 0.5)+
      scale_color_manual(name = 'Water Year', values = color_palette) +  # Assign colors based on the year
      labs(x = "Month", y = y_lab,
           caption = paste0('Only includes months with > 90% of data. Dashed red line is the selected water year.')) +
      theme_bw(base_size = 14) +
      theme(legend.position = 'bottom')
      
  } else {
    gg_out <- ggplot(monthly_summary, aes(month_name, mean_monthly)) + 
      geom_boxplot() +
      geom_point(data = subset(monthly_summary, wtr_year == select_year), 
                 aes(shape = "Select Water Year"), color = 'red', size = 3) +
      scale_shape_manual(values = c("Select Water Year" = 2)) +
      labs(x = "Month", y = y_lab,
           caption = paste0('Period of Record: ', min_year, ' to ', max_year-1, '. Does not include the latest year. \nOnly includes months with > 90% of data.'))  +
      theme_bw(base_size = 14) +
      theme(legend.title = element_blank())
  }
  plotly::ggplotly(gg_out)

})

