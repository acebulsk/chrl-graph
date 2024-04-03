#### monthly normals plots ####

output$header4 <- renderUI({
  req(input$monthly_site)
  str1 <- paste0("<h2>", station_meta[[input$monthly_site]][1], " (", station_meta[[input$monthly_site]][2], " m)", "</h2>")
  HTML(paste(str1))
})

# monthly average values for each year
monthly_avg_df <- reactive({
  if(input$smenu == "monthly_normals"){
    
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
  }
})

# stats of monthly average values over all years  
yearlyData <- reactive({
  if(input$smenu == "monthly_normals"){
    
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
  }
})

observe({
  if(input$smenu == "monthly_normals"){
    
  req(input$monthly_site)
  # need to find the year range of selected sites. finds the max of the two start years as the min.
  monthly_summary <- monthly_avg_df()
  # glob_avg <- globAverage()
  year_range <- monthly_summary$WtrYr |> unique()
  updateSelectInput(session, "monthly_year", "Select Water Year to Compare: ", year_range, selected = max(year_range))
}
  })

output$plot <- renderPlot({
  req(input$monthly_site)
  req(input$plot_type)
  req(input$monthly_year)

  validate(
    need(nrow(monthly_avg_df()) > 0, 'No Data: No data exists for this variable at this station.')
  )
  
  select_year <- input$monthly_year
  min_year <- min(monthly_avg_df()$WtrYr) |> as.numeric()
  max_year <- max(monthly_avg_df()$WtrYr) |> as.numeric()

  y_lab <- names(monthlyVarsDict)[monthlyVarsDict == input$monthly_var]
  
  if(input$plot_type == 'Line Graph'){
    req(input$filter_month_stats != '')

    monthly_summary <- monthly_avg_df()  |> 
      select(line_group = WtrYr, month_name, value = mean_monthly) |> 
      filter(line_group %in% select_year) |> 
      mutate(line_group = ordered(line_group, levels = c(select_year)))

    yearly_summary <- yearlyData() |> 
      pivot_longer(c(Mean:Min), names_to = 'stat_name') |> 
      select(stat_name, month_name, stat_value = value) 
    
    if(input$filter_month_stats){
      mon_stat_df <- left_join(yearly_summary, monthly_summary, by = 'month_name')
      gg_out <- ggplot(mon_stat_df)  +
        geom_line(aes(month_name, value, colour = line_group, group = line_group)) +
        geom_point(aes(month_name, value, colour = line_group, group = line_group), size = 1) +
        geom_line(aes(x = month_name, y = stat_value, linetype = stat_name, group = stat_name), alpha = 0.8) +
        scale_linetype_manual(values = c('dashed', 'longdash', 'dashed'), name = '') +
        scale_color_viridis_d(name = '') +  # Assign colors based on the year
        labs(x = "Month", y = y_lab,
             caption = paste0('Mean monthly values for water years: ', min_year, ' to ', max_year-1, '.\n Only includes months with > 90% of data.')) +
        theme_bw(base_size = 14) +
        theme(legend.position = 'bottom')
    } else {
      gg_out <- ggplot(monthly_summary, aes(month_name, value, colour = line_group, group = line_group))  +
        geom_line() +
        geom_point(size = 0.5)+
        scale_color_viridis_d(name = '') +  # Assign colors based on the year
        labs(x = "Month", y = y_lab,
             caption = paste0('Mean monthly values for water years: ', min_year, ' to ', max_year-1, '.\n Only includes months with > 90% of data.')) +
        theme_bw(base_size = 14) +
        theme(legend.position = 'bottom')
    }
      
  } else if(input$plot_type == 'Boxplot'){
    
    box_data <- monthly_avg_df()  
    
    box_data_means <- box_data |> 
      filter(WtrYr %in% select_year)
      
    gg_out <- ggplot(box_data, aes(month_name, mean_monthly)) + 
      geom_boxplot() +
      geom_point(data = box_data_means, aes(colour = as.factor(WtrYr)), shape = 17, size = 3) +
      scale_color_viridis_d(name = 'Monthly Mean') +  # Assign colors based on the year
      labs(x = "Month", y = y_lab,
           caption = paste0('Period of Record: ', min_year, ' to ', max_year-1, ". \nOnly includes months with > 90% of data."))  +
      theme_bw(base_size = 14) +
      theme(legend.position = 'bottom')
    
  } else if(input$plot_type == 'Bar Chart'){
    req(input$filter_month_stats != '')

    monthly_summary <- monthly_avg_df()  |> 
      select(line_group = WtrYr, month_name, value = mean_monthly) |> 
      filter(line_group %in% select_year)
    
    yearly_summary <- yearlyData() |> 
      pivot_longer(c(Mean:Min), names_to = 'line_group') |> 
      select(line_group, month_name, value)
    
    if(input$filter_month_stats){
      month_line_df <- rbind(monthly_summary, yearly_summary) |> 
        mutate(line_group = ordered(line_group, levels = c('Min', 'Mean', select_year, 'Max')))
    } else {
      month_line_df <- monthly_summary |> 
        mutate(line_group = as.factor(line_group))
    }
    gg_out <- ggplot(month_line_df, aes(x = month_name, y = value, fill = line_group))  +
      geom_bar(stat = "identity", position = "dodge") +
      scale_fill_viridis_d(name = '') +  # Assign colors based on the year
      labs(x = "Month", y = y_lab,
           caption = paste0('Only includes months with > 90% of data. Red line shows the selected water year.')) +
      theme_bw(base_size = 14) +
      theme(legend.position = 'bottom')
  }
  
  gg_out

})

output$monthly_stats_text <- renderDataTable({
  monthly_summary <- monthly_avg_df()  |>
    select(water_year = WtrYr,
           Month = month_name,
           mean_month_sel_yr = mean_monthly,) |>
    filter(water_year %in% input$monthly_year)

  yearly_summary <- yearlyData() |>
    select(Month = month_name, 
           mean_month_all_yrs = Mean,
           Min_yr,
           Max_yr)

  tbl_out <-
    left_join(yearly_summary, monthly_summary, by = 'Month') |> 
    mutate(percent_of_normal = (mean_month_sel_yr/mean_month_all_yrs)*100) |> 
    select(
      `Water Year` = water_year,
      Month,
      `Monthly Mean of Select Year` = mean_month_sel_yr,
      `Monthly Mean of All Years` = mean_month_all_yrs,
      `% of Normal` = percent_of_normal,
      `Year of Min` = Min_yr,
      `Year of Max` = Max_yr
    ) |> 
    arrange(`Water Year`,
            Month)
  
  tbl_out
})

# Render text output
output$text_boxplot_explain <- renderUI({
  req(input$plot_type)
  
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
  req(input$plot_type)
  
  if (input$plot_type != "Boxplot") {
    checkboxInput("filter_month_stats", "Show Min, Mean and Max Stats", value = T)
  } else {
    NULL
  }
})

