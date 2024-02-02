#### hourly plots ####
output$header5 <- renderUI({
  req(input$hourly_site)
  str1 <- paste0("<h2>", station_meta[[input$hourly_site]][1], " (", station_meta[[input$hourly_site]][2], " m)", "</h2>")
  HTML(paste(str1))
})

# final data set

hourlyStatsData <- reactive({
  req(input$hourly_site)
  file_path <- paste0('data/hourly_stats/', input$hourly_site, '_hourly_stats.rds')
  df <- readRDS(file_path) |> 
    filter(name == input$hourly_var)
  
  return(df)
})

hourlyStatsData <- reactive({
  req(input$hourly_site)
  file_path <- paste0('data/hourly_stats/', input$hourly_site, '_hourly_stats.rds')
  df <- readRDS(file_path) |> 
    filter(name == input$hourly_var)# |> 
   # rename(Snow_Depth_qaqc = Snow_Depth_qaqc_filled)
  
  df$plot_time <- if_else(month(df$plot_time) < 10,
                          weatherdash::set_yr(df$plot_time, 1901),
                          weatherdash::set_yr(df$plot_time, 1900))
  
  return(df)
})

hourlyObsData <- reactive({
  req(input$hourly_site)
  
  file_path <- paste0('data/gap-fill/gap_fill_', input$hourly_site, '.rds')
  df <- readRDS(file_path) |> 
    pivot_longer(!datetime) |> 
    filter(name == input$hourly_var)|> 
    mutate(year = format(datetime, '%Y'),
           wtr_year = weatherdash::wtr_yr(datetime))
  
  df$plot_time <- format(df$datetime, "1900-%m-%d %H:%M:%S") # 81 so not a leap year
  df$plot_time <- as.POSIXct(df$plot_time, format = "%Y-%m-%d %H:%M:%S", tz = 'UTC')
  df$plot_time <- if_else(month(df$plot_time) < 10,
                          weatherdash::set_yr(df$plot_time, 1901),
                          weatherdash::set_yr(df$plot_time, 1900))
  
  
  return(df)
})

observe({
  req(input$hourly_site)
  # need to find the year range of selected sites. finds the max of the two start years as the min.
  hourly_obs_df <- hourlyObsData()
  # glob_avg <- globAverage()

  min_year <- min(hourly_obs_df$wtr_year) |> as.numeric()
  max_year <- max(hourly_obs_df$wtr_year) |> as.numeric()
  year_range <- seq.int(min_year, max_year, by = 1)
  updateSelectInput(session, "hourly_year", "Select Water Year to Compare: ", year_range, selected = max_year)
})

output$hourly_stats_plot <- renderPlotly({
  req(input$hourly_site)
  req(input$plot_type)
  req(input$hourly_year)
  
  select_year <- input$hourly_year
  hourly_stats_df <- hourlyStatsData()
  hourly_obs_df <- hourlyObsData() |> 
    filter(wtr_year %in% select_year)
  
  y_lab <- names(hourlyVarsDict)[hourlyVarsDict == input$hourly_var]
  
  gg_out <- ggplot(hourly_obs_df)  +
    geom_line(aes(
      plot_time, value,
      colour = as.factor(wtr_year),
      group = as.factor(wtr_year),
    ))+
    scale_color_viridis_d(name = 'Water Year') +  # Assign colors based on the year
    # scale_linetype_manual(name = 'Line Type', values = c("Mean" = "dashed", "Max" = "dashed", "Min" = "dashed")) +
    scale_x_datetime(labels = scales::date_format("%B")) +
    # scale_fill_manual(name = 'Fill', values = c('5th to 95th\npercentile' = 'grey')) +
    labs(y = y_lab, colour = 'Water Year') +
    theme_bw() +
    theme(axis.title.x = element_blank())
    
  
 # browser()
  if("5-95 Percentile \nRange (green shading)" %in% input$hourly_stats_checkbox){
    gg_out <- gg_out +
      geom_ribbon(
      data = hourly_stats_df,
      aes(
        x = plot_time,
        ymin = lower_quantile,
        ymax = upper_quantile,
        fill = '5th to 95th\npercentile'
      ),
      alpha = 0.2,
      fill = 'lightgreen',
      colour = 'green'
    )# +
      #geom_line(data = hourly_stats_df, aes(x = plot_time, y = upper_quantile, group = 1, linetype = 'Upper Quantile')) +
     # geom_line(data = hourly_stats_df, aes(x = plot_time, y = lower_quantile, group = 1, linetype = 'Lower Quantile'))
  }

  if("Max-Min Range (blue shading)" %in% input$hourly_stats_checkbox){
    gg_out <- gg_out +
      geom_ribbon(
        data = hourly_stats_df,
        aes(
          x = plot_time,
          ymin = min,
          ymax = max,
          fill = 'Max to Min Range'
        ),
        alpha = 0.2,
        fill = 'dodgerblue',
        colour = 'blue'
      ) #+
      # geom_line(data = hourly_stats_df, aes(x = plot_time, y = max, group = 1, linetype = 'Max')) +
      # geom_line(data = hourly_stats_df, aes(x = plot_time, y = min, group = 1, linetype = 'min'))
  }

  if("Mean" %in% input$hourly_stats_checkbox){
    gg_out <- gg_out +
      geom_line(data = hourly_stats_df, aes(x = plot_time, y = mean, group = 1, linetype = 'Mean'))
  }

  plotly::ggplotly(gg_out)
})

