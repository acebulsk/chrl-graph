#### monthly normals plots ####

output$header4 <- renderUI({
  req(input$monthly_site)
  str1 <- paste0("<h2>", station_meta[[input$monthly_site]][1], " (", station_meta[[input$monthly_site]][2], " m)", "</h2>")
  HTML(paste(str1))
})

# monthly average values for each year
finalData <- reactive({
  req(input$monthly_site)
  file_path <- paste0('data/monthly_normals_plot_data/', input$monthly_site, '_monthly_normals_data.rds')
  
  validate(
    need(file.exists(file_path),
         'No data: Please select another station, data is not available for this station yet.')
    )
  
  monthly_stats_df <- readRDS(file_path) |> 
    filter(name == input$monthly_var)
  
  validate(
  need(monthly_stats_df$name %in% input$monthly_var,
       "No data: Please select another variable, this variable is not available for this station yet.")
  )
  return(monthly_stats_df)
})

# stats of monthly average values over all years  
yearlyData <- reactive({
  req(input$monthly_site)
  file_path <- paste0('data/yearly_mean_monthly_summary/', input$monthly_site, '_yearly_mean_monthly_summary.rds')
  validate(
    need(file.exists(file_path),
         'No data: Please select another station, data is not available for this station yet.')
  )
  yearly_stats_df <- readRDS(file_path) |> 
    filter(name == input$monthly_var)
  
  validate(
    need(yearly_stats_df$name %in% input$monthly_var,
         "No data: Please select another variable, this variable is not available for this station yet.")
  )
  return(yearly_stats_df)
})

observe({
  req(input$monthly_site)
  # need to find the year range of selected sites. finds the max of the two start years as the min.
  monthly_summary <- finalData()
  # glob_avg <- globAverage()
  min_year <- min(monthly_summary$wtr_year) |> as.numeric()
  max_year <- max(monthly_summary$wtr_year) |> as.numeric()
  year_range <- seq.int(min_year, max_year, by = 1)
  updateSelectInput(session, "monthly_year", "Select Water Year to Compare: ", year_range, selected = max_year)
})

output$plot <- renderPlot({
  req(input$monthly_site)
  req(input$plot_type)
  req(input$monthly_year)

  validate(
    need(nrow(finalData()) > 0, 'No Data: No data exists for this variable at this station.')
  )
  
  select_year <- input$monthly_year
  min_year <- min(finalData()$wtr_year) |> as.numeric()
  max_year <- max(finalData()$wtr_year) |> as.numeric()

  y_lab <- names(monthlyVarsDict)[monthlyVarsDict == input$monthly_var]
  
  if(input$plot_type == 'Line Graph'){
    col_names <- c('line_group', 'month_name', 'value')
    
    monthly_summary <- finalData()  |> 
      select(line_group = wtr_year, month_name, value = mean_monthly) |> 
      filter(line_group %in% select_year)
    
    yearly_summary <- yearlyData() |> 
      pivot_longer(c(Max:Min), names_to = 'line_group') |> 
      select(line_group, month_name, value)
    
    if(input$filter_month_stats){
      month_line_df <- rbind(monthly_summary, yearly_summary) |> 
        mutate(line_group = ordered(line_group, levels = c('Min', select_year, 'Max')))
    } else {
      month_line_df <- monthly_summary |> 
        mutate(line_group = as.factor(line_group))
    }

    gg_out <- ggplot(month_line_df, aes(month_name, value, colour = line_group, group = line_group))  +
      geom_line() +
      geom_point(size = 0.5)+
      scale_color_viridis_d(name = '') +  # Assign colors based on the year
      labs(x = "Month", y = y_lab,
           caption = paste0('Mean monthly values for water years: ', min_year, ' to ', max_year-1, '.\n Only includes months with > 90% of data.')) +
      theme_bw(base_size = 14) +
      theme(legend.position = 'bottom')
      
  } else if(input$plot_type == 'Boxplot'){
    
    box_data <- finalData()  
    
    box_data_means <- box_data |> 
      filter(wtr_year %in% select_year)
      
    gg_out <- ggplot(box_data, aes(month_name, mean_monthly)) + 
      geom_boxplot() +
      geom_point(data = box_data_means, aes(colour = as.factor(wtr_year)), shape = 17, size = 3) +
      scale_color_viridis_d(name = 'Monthly Mean') +  # Assign colors based on the year
      labs(x = "Month", y = y_lab,
           caption = paste0('Period of Record: ', min_year, ' to ', max_year-1, ". \nOnly includes months with > 90% of data."))  +
      theme_bw(base_size = 14) +
      theme(legend.position = 'bottom')
  }
  
  gg_out

})

# Render text output
output$text_boxplot_explain <- renderUI({
  if (input$plot_type == "Boxplot") {
    fluidRow(
      column(12, 
             tags$div(
               style = "text-align: right; font-size: x-small; font-family: 'sans-serif';",
               HTML("For boxplot documentation <a href='https://ggplot2.tidyverse.org/reference/geom_boxplot.html#summary-statistics'>click here.</a>")
             )
      )
    )
  } else {
    NULL  # If another plot type is selected, don't render the text
  }
})

# Conditional rendering of checkboxInput based on plot type
output$filter_monthly_line_stats <- renderUI({
  if (input$plot_type != "Boxplot") {
    checkboxInput("filter_month_stats", "Show Max and Min Stats", value = T)
  } else {
    NULL
  }
})

