#### custom graphs ####

output$header2 <- renderUI({
  req(input$custom_site)
  str1 <- paste0("<h2>", station_meta[[input$custom_site]][1], " (", station_meta[[input$custom_site]][2], " m)", "</h2>")
  if(input$custom_site %in% list_stn_tipping_bucket_errs){
    HTML(paste(str1, p('The tipping bucket is currently malfunctioning at this station please refer to total precipitation (stand pipe) instead.', style = "color:red")))
  }
  else{HTML(paste(str1))}
})
# construct variables based on user selection for the database query ----

# reactive element to create year list based on available years for chosen station
observe({
  # need to find the year range of selected sites. finds the max of the two start years as the min.
  start_years <- station_meta[[input$custom_site]][3]
  min_year <- unname(unlist(lapply(start_years, max)))
  max_year <- weatherdash::wtr_yr(Sys.Date(), 10) # return current water year.
  year_range <- seq.int(min_year, max_year, by = 1)
  updateSelectInput(session, "custom_year", "Select Water Year:", year_range, selected = max_year)
})

# get available variables for selected station
output$varSelection <- renderUI({
  
  # could switch to drop down select and use this which works in any order:
  # var_names <- names(sort(factor(wx_var_list[wx_var_list %in% input$wx_var],
  #                                levels = input$wx_var, ordered = T)))
  # get colnames from reactive dataset
  stnVars <- unname(unlist(station_meta[[input$custom_site]][6]))
  
  var_subset <- Filter(function(x) any(stnVars %in% x), varsDict)
  
  checkboxGroupInput(
    inputId = "custom_var",
    label = "Select Variables:",
    choices = var_subset,
    inline = FALSE,
    selected = c("Air_Temp", "RH")
  )
})

# get data from VIU Galiano database ----

# pull QAQC data from mysql db based on user station and year input
custom_qc_data_query <- reactive({
  req(input$custom_site)
  req(input$custom_year)
  
  conn <- do.call(DBI::dbConnect, args)
  on.exit(DBI::dbDisconnect(conn))
  query <-
    paste0("SELECT ", paste(qaqc_vars, collapse = ", "), " FROM qaqc_",
           input$custom_site,
           " where WatYr = ",
           input$custom_year,
           ";")

  data <- dbGetQuery(conn, query)
  
  validate(
    need(nrow(data) > 0,
         'No QAQC data: Please select try another year, data is not available for this year and station yet.')
  )
  data$qc_level <- qaqc_data_name_display
  
  return(data)
})

# pull QAQC data from mysql db based on user station and year input
qc_max_datetime <- reactive({
  req(custom_qc_data_query())
  max_dt <- max(custom_qc_data_query()$DateTime)

  return(max_dt)
})

# pull in CLEAN rawish data from mysql db based on user station and year input
custom_clean_data_query <- reactive({
  req(custom_qc_data_query())
  req(qc_max_datetime())
  req(input$custom_site)
  req(input$custom_year)
  
  clean_query_start <- qc_max_datetime() - clean_qaqc_overlap_seconds

  conn <- do.call(DBI::dbConnect, args)
  on.exit(DBI::dbDisconnect(conn))
  
  if(input$plot_all_raw){
    query <-
      paste0("SELECT * FROM clean_",
             input$custom_site,
             " where WatYr = ",
             input$custom_year,
             ";")
  } else {
    query <-
      paste0("SELECT * FROM clean_",
             input$custom_site,
             " where WatYr = ",
             input$custom_year,
             " AND DateTime >= '",
             clean_query_start,
             "';")
  }

  data <- dbGetQuery(conn, query)

  validate(
    need(nrow(data) > 0,
         'No Raw data: Please select try another year, data is not available for this year and station yet.')
  )
  data$qc_level <- clean_data_name_display
  
  return(data)

})

output$cleanSnowButton <- renderUI({
  req(input$custom_var)
  if("Snow_Depth" %in% input$custom_var){
    radioButtons("cleanSnowCstm", "Preform automated spike correction on Snow Depth?:", inline = T,
                 c("Yes" = "yes",
                   "No" = "no"),
                 selected = "no"
    )
  }

})

output$slider <- renderUI({
  req(custom_qc_data_query())
  req(custom_clean_data_query())
  
  sliderInput(inputId = "sliderTimeRange", label = "",
              min = min(custom_qc_data_query()$DateTime),
              max = max(custom_clean_data_query()$DateTime),
              value = c(min(custom_qc_data_query()$DateTime),
                        max(custom_clean_data_query()$DateTime)),
              step = 3600,
              width = '85%',
              height )
})

#filter preset data query
customDataFilter <-  reactive({
  req(input$sliderTimeRange)
  qc_raw_data <- rbind(custom_clean_data_query(), custom_qc_data_query())
  qc_raw_data$qc_level <-
    ordered(qc_raw_data$qc_level,
            c(clean_data_name_display, qaqc_data_name_display))
  
  qc_raw_data_out <- qc_raw_data %>%  dplyr::filter(DateTime >= input$sliderTimeRange[1] &
                                   DateTime <= input$sliderTimeRange[2])
  return(qc_raw_data_out)
})

# final data set

final_custom_data <- reactive({
  req(customDataFilter())
  req(input$custom_var)

  custom_data <- customDataFilter()

  if("Snow_Depth" %in% input$custom_var) {
    req(input$cleanSnowCstm)
    if(input$cleanSnowCstm == "yes"){
      flag  <- ("Snow_Depth" %in% input$custom_var)
      clean <- input$cleanSnow
      custom_data <- spike_clean(data = custom_data, 'DateTime', 'Snow_Depth', spike_th = 10, roc_hi_th = 40, roc_low_th = 75)
    }
    else{return(custom_data)}
  }
  return(custom_data)
})


# plot for custom graphs page
output$plot1 <- renderPlotly({
  req(input$custom_site)
  req(input$custom_year)
  req(input$custom_var)
  req(final_custom_data())
  req(custom_clean_data_query())
  req(custom_qc_data_query())
  
  custom_data <- final_custom_data() %>%
    select(DateTime, input$custom_var, qc_level) |> 
    pivot_longer(input$custom_var) |> 
    mutate(ylab = purrr::map_chr(name, function(short_name)
      names(varsDict)[which(unlist(varsDict) == short_name)]))
  
  ggplot(custom_data, aes(DateTime, value, colour = qc_level)) +
    geom_line() +
    facet_grid(rows = vars(ylab), switch = "y", scales = 'free_y') +
    ylab(element_blank())  +
    xlab(element_blank())  +
    scale_color_manual(values = two_cols) +
    labs(color = "Data Type") +
    theme_bw() #+
    # theme(strip.placement = "outside",
    #       strip.background = element_rect(fill="white", colour = "white"))
  plotly::ggplotly() |> 
    layout(plot_bgcolor = "#f5f5f5", paper_bgcolor = "#f5f5f5",
           margin = list(b = 0, r = 50, 
                         l = 50, t = 10))

})

output$plot1_ui <- renderUI({
  plotlyOutput('plot1', height = length(input$custom_var)*input$fig_height)
})

#### render partner logo ui ####
output$partnerLogoUI_custom <- renderUI({
  req(input$custom_site)
  cur_stn <- input$custom_site
  station_meta[[cur_stn]]['logos']
})

# create warning for down stations
observe({
  req(preset_data_query())
  req(input$custom_site)
  if(input$smenu == "cstm_graph"){
    if(input$custom_site %in% down_stations){
      showModal(modalDialog(
        title = "Warning:",
        paste("This station is currently offline."),
        easyClose = T
      ))
    }
  }
}
)


