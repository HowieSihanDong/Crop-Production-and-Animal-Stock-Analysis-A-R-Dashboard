#############
# Library() #
#############
library(shiny)
library(ggplot2)
library(ggiraph)
library(leaflet)
library(dplyr)
library(tidyr)
library(plotly)
library(terra)
library(fuzzyjoin)
library(sf)
library(rnaturalearth)
library(shinydashboard)
library(DT)
library(shinythemes)
library(RColorBrewer)
library(webr)


###################
# Data Processing #
###################
source("data-pre.R")
source("function_design.R")

##################
# USER INTERFACE #
##################

##Tab1: Map, accumulated line chart, and interactive comparison##
compare_tab <- tabPanel(
  "Global Production Overview",
  fluidRow(
    column(
      width = 8,valueBoxOutput("summary", width = 12)
    ),
    column(
      width = 4,
      div(
        style = "margin: 0 auto; width: 400px;",
        sliderInput(
          "yearRange",
          #Using HTML to put title middle
          HTML(
            "&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
               &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
               &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;Select Mapping Year"
          ),
          min = 2000,
          max = 2022,
          value = c(2000, 2022),
          ticks = FALSE,
          width = "100%"
        )
      )
    )), 
  fluidRow(
    column(
      width = 8,
      div(h4("Map of Crop Production or Animal Stock per Unit Area"), style = "text-align: center;")
    ),
    column(
      width = 4,
      div(h4("Piedonut Chart for Overall Production or Stock Distribution"), style = "text-align: center;")
    )
  ),
  fluidRow(
    # Two Map in same column
    column(
      width = 8,  
      leafletOutput("map_live", height = "350px"),
      br(),
      leafletOutput("map_crop", height = "350px")   
    ),
    
    # Two Pie Chart (interactive) in same column
    column(
      width = 4, 
      plotOutput("donut_live", height = "370px"),  
      plotOutput("donut_crop", height = "370px") 
    )
  ),
  
  # Absolute Panel 1: Livestock item selection
  absolutePanel(
    top = 315,
    left = 25,
    draggable = TRUE,
    width = "200px",
    selectInput(
      inputId = "item_type_livestock",
      label = "Select item",
      choices = c("all", unique(livestock$item)),
      selected = "all"
    ),
    style = "background-color: rgba(255, 255, 255, 0.8); padding: 10px; border-radius: 8px;"
  ),
  
  # Absolute Panel 2: Crop item selection
  absolutePanel(
    top = 700,
    left = 25,
    draggable = TRUE,
    width = "200px",
    selectInput(
      inputId = "item_type_crop",
      label = "Select item",
      choices = c("all", unique(cropstock$item)),
      selected = "all"
    ),
    style = "background-color: rgba(255, 255, 255, 0.8); padding: 10px; border-radius: 8px;"
  )
)

line_tab <- tabPanel(
  title = "Production vs Population Trends", 
  sidebarLayout(
    sidebarPanel(
      div(
        style = "display: flex; flex-direction: column; justify-content: center; height: 80vh;", 
        #Format below two input
        selectizeInput(
          inputId = "selected_area",
          label = "Select country as target: ",
          choices = c("Global Analysis", setdiff(unique(population$area), c("American Samoa", "Andorra", "Anguilla", "Aruba"))),
          selected = "Global Analysis",
          multiple = FALSE,
          options = list(
            placeholder = 'Please select countries',
            allowEmptyOption = TRUE,
            closeAfterSelect = FALSE
          )
        ),
        selectizeInput(
          inputId = "compare_area",
          label = "Select below country to compare: ",
          choices = c("Global Analysis", setdiff(unique(population$area), c("American Samoa", "Andorra", "Anguilla", "Aruba"))),
          selected = "Global Analysis",
          multiple = TRUE,
          options = list(
            placeholder = 'Please select countries',
            maxItems = NULL,  
            allowEmptyOption = TRUE,
            closeAfterSelect = FALSE 
          )
        ),
        #Select the mode of second tap
        radioButtons(
          'mode_selection',
          label = 'Visualisation is present for',
          choices = c('Global Analysis', 'Crop', 'Live Animals'),
          selected = 'Global Analysis'
        )
      ),
      width = 3
    ),
    mainPanel(uiOutput("dynamic_line_charts"), width = 9)
  )
)


ui <- navbarPage(
  theme = shinythemes::shinytheme("journal"),
  id = 'mypage',
  title = div(
    "Global Production & Population Growth Dashboard", 
    style = "font-size: 27px; font-weight: bold; color: black;"  
  ),
  tags$style(HTML("
    .navbar-nav > li > a {
      font-size: 22px; 
      font-weight: bold;
      color: black !important;  
    }
    .navbar {
      height: 70px;  
    }
    .navbar-brand {
      line-height: 70px; 
      height: 70px; 
      padding: 0 15px;
      color: black !important;
    }
  ")),
  compare_tab, 
  line_tab
)

################
# SHINY SERVER #
################

server <- function(input, output, session) {
  
  filtered_live_map <- reactive({
    data_live_filtered <- data_live_map %>%
      filter(year >= input$yearRange[1] &
               year <= input$yearRange[2])
    # Based on the type selected, generate correspounding data
    if (input$item_type_livestock == 'all') {
      data_live_aggregated <- data_live_filtered %>%
        group_by(area) %>%
        summarise(production = sum(production, na.rm = TRUE)) %>%
        ungroup()
    } else {
      data_live_aggregated <- data_live_filtered %>%
        filter(item == input$item_type_livestock) %>%
        group_by(area) %>%
        summarise(production = sum(production, na.rm = TRUE)) %>%
        ungroup()
    }
    
    # Connect with world data
    world_live_data <- world %>%
      left_join(data_live_aggregated, by = c("admin" = "area")) %>%
      mutate(cap_production = round(production/area_km2)) 
    
    return(world_live_data)
  })
  
  
  filtered_crop_map <- reactive({
    # Apply selected years to data
    data_crop_filtered <- data_crop_map %>%
      filter(year >= input$yearRange[1] &
               year <= input$yearRange[2])
    
    if (input$item_type_crop == 'all') {
      # aggregate all when select all
      data_crop_aggregated <- data_crop_filtered %>%
        group_by(area) %>%
        summarise(production = sum(production, na.rm = TRUE)) %>%
        ungroup()
    } else {
      # Otherwise, select only selected
      data_crop_aggregated <- data_crop_filtered %>%
        filter(item == input$item_type_crop) %>%
        group_by(area) %>%
        summarise(production = sum(production, na.rm = TRUE)) %>%
        ungroup()
    }
    
    world_crop_data <- world %>%
      left_join(data_crop_aggregated, by = c("admin" = "area")) %>%
      mutate(cap_production = round(production/area_km2)) 
    return(world_crop_data)
  })
  
  #Generate data ONLY For PieDonut
  filtered_live_donut <- reactive({
    data_live_map_donut <- data_live_map %>%
      filter(year >= input$yearRange[1] &
               year <= input$yearRange[2]) %>%
      mutate(area = if_else(area == "United States of America", "America", area))
    # America shows up for many times but name is too long
    return(data_live_map_donut)
  })
  
  filtered_crop_donut <- reactive({
    data_crop_map_donut <- data_crop_map %>%
      filter(year >= input$yearRange[1] &
               year <= input$yearRange[2]) %>%
      mutate(area = if_else(area == "United States of America", "America", area))
    return(data_crop_map_donut)
  })
  
  output$summary <- renderUI({
    tagList(
      tags$style(HTML("
      .small-box {
        height: 80px !important;
      }
      .small-box h3 {
        font-size: 20px !important;
      }
      .small-box p {
        font-size: 15px !important;
      }
      /* Remove top margin/padding */
      .content {
        margin-top: 0px !important;
        padding-top: 0px !important;
      }
      /* Center the valueBoxes on the same row and move them to the right */
      .center-content {
        display: flex;
        justify-content: center;
        align-items: center;
        padding-left: 200px; 
      }
    ")),
      
      fluidRow(
        class = "center-content",  # Apply center-content class here
        # Total Production
        valueBox(
          value = paste0(prettyNum(round(
            sum(filtered_live_map()$production, na.rm = TRUE) / 1e6, 2
          ), big.mark = ","), " million units"),
          subtitle = tagList(icon("cow"), " Livestock World Processes")
        ),
        
        # Total Crop Tons
        valueBox(
          value = paste0(prettyNum(round(
            sum(filtered_crop_map()$production, na.rm = TRUE) / 1e6, 2
          ), big.mark = ","), " million tons"),
          subtitle = tagList(icon("seedling"), " Total Crop Tons"), 
          icon = NULL,
          color = "blue"
        )
      )
    )
  })
  
  
  #Genrate Map
  output$map_live <- renderLeaflet({
    create_live_map(filtered_live_map())
  })
  
  output$map_crop <- renderLeaflet({
    create_crop_map(filtered_crop_map())
  })
  
  ####Tab 1: first Pie (live)
  
  #Listen to value
  selected_live <- reactiveValues(item_type_livestock = "all")
  observeEvent(input$item_type_livestock, {
    selected_live$item_type_livestock <- input$item_type_livestock  # Update reactiveValues for live type
  })
  #Update value
  data_donut_year_live <- reactive({
    data_donut_year_live <- filtered_live_donut() %>%
      filter(item == selected_live$item_type_livestock) %>%
      select(area, year, production) %>%
      group_by(area) %>%
      summarise(count = sum(production)) %>%
      slice_max(count, n = 3)
    return(data_donut_year_live)
  })
  
  output$donut_live <- renderPlot({
    if (selected_live$item_type_livestock == "all") {
      create_donut(create_donut_data(filtered_live_donut()))
    } else {
      create_donut_year(data_donut_year_live())
    }
  })
  
  #########Tab 1: second Pie (crop)
  #listen to value
  selected_crop <- reactiveValues(item_type_crop = "all")
  observeEvent(input$item_type_crop, {
    selected_crop$item_type_crop <- input$item_type_crop  # Update reactiveValues for crop type
  })
  #Update value
  data_donut_year_crop <- reactive({
    data_donut_year_crop <- filtered_crop_donut() %>%
      filter(item == selected_crop$item_type_crop) %>%
      select(area, year, production) %>%
      group_by(area) %>%
      summarise(count = sum(production)) %>%
      slice_max(count, n = 3)
    return(data_donut_year_crop)
  })
  
  output$donut_crop <- renderPlot({
    if (selected_crop$item_type_crop == "all") {
      create_donut(create_donut_data(filtered_crop_donut()))
    } else {
      create_donut_year(data_donut_year_crop())
    }
  })
  
  ######################Tab 2: Line Chart#################
  
  selected_line <- reactiveValues(selected_area = NULL,
                                  mode = 'Global Analysis',
                                  compare_area = c())
  
  ## Observer for MAP
  observeEvent(input$map_live_shape_click, {
    if (is.null(input$map_live_shape_click$id)) {
      selected_line$selected_area <- "Global Analysis"  
    } else {
      selected_line$selected_area <- input$map_live_shape_click$id  
      updateSelectInput(session, "selected_area", selected = selected_line$selected_area)
      selected_line$mode <- 'Live Animals'
      updateSelectInput(session, "mode_selection", selected = 'Live Animals')
      updateNavbarPage(session, 'mypage', selected = 'Production vs Population Trends')
    }
  })
  
  observeEvent(input$map_crop_shape_click, {
    if (is.null(input$map_crop_shape_click$id)) {
      selected_line$selected_area <- "Global Analysis"  
    } else {
      selected_line$selected_area <- input$map_crop_shape_click$id  
      updateSelectInput(session, "selected_area", selected = selected_line$selected_area)
      selected_line$mode <- 'Crop'
      updateSelectInput(session, "mode_selection", selected = 'Crop')
      updateNavbarPage(session, 'mypage', selected = 'Production vs Population Trends')
    }
  })
  
  observeEvent(input$selected_area, {
    selected_line$selected_area <- input$selected_area  
    
    if (input$mode_selection == "Global Analysis" &
        input$selected_area != "Global Analysis") {
      updateSelectInput(session, "mode_selection", selected = 'Crop')
    }
    if (input$selected_area == "Global Analysis" &
        input$mode_selection != "Global Analysis") {
      updateSelectInput(session, "mode_selection", selected = "Global Analysis")
    }
    
    if (!is.null(input$selected_area) & "Global Analysis" %in% input$compare_area) {
      updateSelectInput(session, "compare_area", selected = character(0))  
    }
  })
  
  observeEvent(input$compare_area, {
    selected_line$compare_area <- input$compare_area  # Update reactiveValues for crop type
    
    if(input$mode_selection == "Global Analysis"){
      updateSelectInput(session, "compare_area", selected = c("Global Analysis"))
    }
    
  })
  
  observeEvent(input$mode_selection, {
    selected_line$mode <- input$mode_selection
    
    if (input$mode_selection  == "Global Analysis") {
      updateSelectInput(session, "selected_area", selected = 'Global Analysis')
      updateSelectInput(session, "compare_area", selected = c('Global Analysis'))
      
    }
    
    if (input$mode_selection != "Global Analysis" &
        input$selected_area == "Global Analysis") {
      updateSelectInput(session, "selected_area", selected = 'Afghanistan')
    }
    
  })
  
  data_line_selected_area <- reactive({
    if (selected_line$mode == 'Global Analysis') {
      pop_crop_global <- pop_crop %>%
        group_by(year) %>%
        summarise(
          population = sum(population, na.rm = TRUE),
          production_crop = sum(production, na.rm = TRUE)
        )
      
      pop_live_global <- pop_live %>%
        group_by(year) %>%
        summarise(production_live = sum(production, na.rm = TRUE))
      
      pop_global <- pop_crop_global %>%
        left_join(pop_live_global, by = "year")
      
      return(pop_global)
      
    } else if (selected_line$mode == 'Live Animals') {
      pop_target <- pop_live %>%
        filter(area == selected_line$selected_area) %>%
        group_by(year) %>%
        summarise(
          population = mean(population, na.rm = TRUE),
          production = mean(production, na.rm = TRUE)
        )
      
      return(pop_target)
      
    } else if (selected_line$mode == 'Crop') {
      pop_target <- pop_crop %>%
        filter(area == selected_line$selected_area) %>%
        group_by(year) %>%
        summarise(
          population = mean(population, na.rm = TRUE),
          production = mean(production, na.rm = TRUE)
        )
      
      return(pop_target)
    }
  })
  
  create_line_chart_main <- function(data, which_production, title_name) {
    data <- data[order(data$year), ]
    
    if (which_production == 'Live Animal Stock'){
      dimensions_title = '<b>Stock</b> (unit per capita)'
      colour_which = '#ff7f0e'
    } else {
      dimensions_title = '<b>Production</b> (ton per Capita)'
      colour_which = '#34c568'
    }
    fig <- plot_ly(
      data = data,
      x = ~year,
      y = ~population,
      name = "Population",
      type = 'scatter',
      mode = 'lines+markers',
      line = list(color = '#1f77b4', width =4),
      marker = list(color = 'black')
    ) %>%
      add_trace(
        y = ~production,
        name = which_production,
        yaxis = "y2",
        mode = "lines+markers",
        type = 'scatter',
        line = list(color = colour_which,width =4),
        marker = list(color = 'black')
      )
    # Define layout with dual y-axes
    fig <- fig %>%
      layout(
        title = title_name,
        xaxis = list(title = "Year"),
        yaxis = list(
          title = "<b>Population</b>",
          titlefont = list(size = 14, color = "#1f77b4"),
          tickfont = list(color = "#1f77b4"),
          showgrid = FALSE
        ),
        yaxis2 = list(
          title = dimensions_title,
          titlefont = list(size = 14, color = colour_which),
          tickfont = list(color = colour_which),
          overlaying = "y",
          side = "right",
          showgrid = FALSE,
          standoff = 50,
          automargin = TRUE
        ),
        legend = list(x = 0, y = 0.98),
        plot_bgcolor = '#ffffff'
      )
    return(fig)
  }
  
  ###Function for data per Capita with animation
  accumulate_by <- function(dat, var) {
    var <- lazyeval::f_eval(var, dat)
    lvls <- plotly:::getLevels(var)
    dats <- lapply(seq_along(lvls), function(x) {
      cbind(dat[var %in% lvls[seq(1, x)], ], frame = lvls[[x]])
    })
    dplyr::bind_rows(dats)
  }
  
  create_compare_line_global <- function(data, crop_colour, live_colour){
    # Prepare the data with necessary calculations
    data <- data %>%
      mutate(
        average_global_crop = production_crop / population,
        average_global_live = production_live / population
      ) %>%
      accumulate_by(~year)  # Apply accumulate_by to the data
    
    #Set the upper and lower bound of dimensions
    live_min <- min(data$average_global_live, na.rm = TRUE)*0.95
    live_max <- max(data$average_global_live, na.rm = TRUE)*1.05
    crop_min <- min(data$average_global_crop, na.rm = TRUE)*0.95
    crop_max <- max(data$average_global_crop, na.rm = TRUE)*1.05
    
    # Initialize the plot
    fig <- plot_ly()
    
    # Crop Line
    fig <- fig %>%
      add_trace(
        data = data,
        x = ~year,
        y = ~average_global_crop,
        name = "Crop Per Capita",  # Ensure name is set
        mode = "lines+markers",
        type = "scatter",
        line = list(color = crop_colour, width = 4),
        marker = list(color = 'black'),
        frame = ~frame,  # Set frame here for animation
        legendgroup = "Crop",   # Group legend
        showlegend = TRUE       # Ensure it shows in the legend
      )
    
    # Live Stock Dimension
    ay <- list(
      tickfont = list(color = live_colour),
      overlaying = "y",
      side = "right",
      title = "<b>Stock</b> (unit per capita)",
      titlefont = list(size = 14, color = live_colour),  # Correct live colour here
      title.standoff = 50,
      range = c(live_min, live_max)
    )
    
    # Live Stock Line
    fig <- fig %>%
      add_trace(
        data = data,
        x = ~year,
        y = ~average_global_live,
        name = "Live Animal Per Capita",  # Ensure name is set
        yaxis = "y2",
        mode = "lines+markers",
        type = "scatter",
        line = list(color = live_colour, width = 4),
        marker = list(color = 'black'),
        frame = ~frame,  # Set frame here for animation
        legendgroup = "Live",  # Group legend
        showlegend = TRUE
      )
    
    # Crop Dimension and integration
    fig <- fig %>% layout(
      title = 'Trends in Global Per Capita Crop Yield and Livestock Stock',
      yaxis = list(
        title = "<b>Production</b> (ton per Capita)",
        titlefont = list(size = 14, color = crop_colour),  
        tickfont = list(color = crop_colour),
        zerolinecolor = 'rgba(0,0,0,0)',
        zerolinewidth = 2,
        gridcolor = 'rgba(0,0,0,0)',
        showgrid = FALSE,
        range = c(crop_min, crop_max)
      ),
      yaxis2 = ay,
      xaxis = list(
        title = "",
        zerolinecolor = 'rgba(0,0,0,0)',
        zerolinewidth = 2,
        gridcolor = 'rgba(0,0,0,0)'
      ),
      plot_bgcolor = '#ffffff'
    )
    
    # Configure animation options
    fig <- fig %>% animation_opts(
      frame = 100, 
      transition = 0, 
      redraw = FALSE,
      easing = 'linear-in'
    )
    fig <- fig %>% animation_slider(
      hide = TRUE
    )
    fig <- fig %>% animation_button(
      x = 1, xanchor = "right", y = 0, yanchor = "bottom"
    )
    
    return(fig)
  }
  
  create_compare_line_compare <- function(data, selected, compare_list, mode){
    # Prepare the data with necessary calculations
    data_compare <- data %>%
      select(year, area, production, population) %>%
      filter(area %in% unlist(compare_list) | area %in% selected) %>%
      group_by(year, area) %>%
      summarize(population = mean(population),
                production = sum(production)) %>%
      mutate(production_average = production/population)%>%
      accumulate_by(~year) %>%
      ungroup()
    
    # Initialize the plot
    fig <- data_compare %>%
      plot_ly(
        x = ~year,
        y = ~production_average,
        split = ~area,
        frame = ~frame,
        type = 'scatter',
        mode = 'lines+markers',
        colors = "Set1",
        marker = list(color = "black"),
        line = list(width = 4)       
      )
    
    if (mode == 'Live Animals'){
      name_mode = 'Comparison in Global Per Capita Livestock Stock between Selected Country'
      title_mode = '<b>Stock</b> (unit per capita)' 
    } else{
      name_mode = 'Comparison in Global Per Capita Crop Production between Selected Country'
      title_mode = "<b>Production</b> (ton per Capita)"
    }
    
    fig <- fig %>% layout(
      title = name_mode,
      yaxis = list(
        title = title_mode,
        titlefont = list(size = 14),  
        range = c(min(data_compare$production_average)*0.8, 
                  max(data_compare$production_average)*1.1)
      ),
      xaxis = list(
        title = "",
        gridcolor = 'rgba(0,0,0,0)'
      ),
      plot_bgcolor = '#ffffff'
    )
    
    # Configure animation options
    fig <- fig %>% animation_opts(
      frame = 100,
      transition = 0,
      redraw = FALSE
    )
    fig <- fig %>% animation_slider(
      hide = T
    )
    fig <- fig %>% animation_button(
      x = 1, xanchor = "right", y = 0, yanchor = "bottom"
    )
    
    return(fig)
  }
  
  ######Area-specific LINE chart#######
  
  output$line_chart_area <- renderPlotly({
    line_data <- data_line_selected_area()
    
    if (selected_line$mode == 'Global Analysis') {
      # 给 crop line first
      line_data_global_crop <- line_data %>%
        select(year, production_crop, population) %>%
        mutate(production = production_crop)
      
      return(
        create_line_chart_main(
          line_data_global_crop,
          'Crop Production',
          'Global Crop Production versus Population Across Time'
        )
      )
      
    } else {
      if (selected_line$mode == 'Crop') {
        name_legend = 'Crop Production'
      } else if (selected_line$mode == 'Live Animals') {
        name_legend = 'Live Animal Stock'
      }
      
      return(create_line_chart_main(line_data, name_legend, "Production vs Population"))
    }
  })
  
  output$line_global_live <- renderPlotly({
    if (selected_line$mode == 'Global Analysis') {
      line_data_global_live <- data_line_selected_area()
      line_data_global_live <- line_data_global_live %>%
        select(year, production_live, population) %>%
        mutate(production = production_live)
      return(
        create_line_chart_main(
          line_data_global_live,
          'Live Animal Stock',
          'Global Stock of Live Animals versus Population Across Time'
        )
      )
    }
  })
  
  output$global_average <- renderPlotly({
    if (selected_line$mode == 'Global Analysis') {
      return(create_compare_line_global(data_line_selected_area(), '#34c568', '#ff7f0e'))
    }
  })
  
  output$selected_average <- renderPlotly({
    if (selected_line$mode == 'Live Animals') {
      return(create_compare_line_compare(pop_live, 
                                         selected_line$selected_area, 
                                         selected_line$compare_area,
                                         'Live Animals'))
    } else{
      return(create_compare_line_compare(pop_crop, 
                                         selected_line$selected_area, 
                                         selected_line$compare_area,
                                         'Crop'))
    }
  })
  
  #Dynamic Layout for output (Specific to modes)
  output$dynamic_line_charts <- renderUI({
    height_px <- "450px"  
    height_px_average <- "350px"
    
    if (selected_line$mode == 'Global Analysis') {
      return(tagList(
        br(),
        fluidRow(
          column(
            width = 6,
            plotlyOutput("line_global_live", height = height_px)
          ),
          column(
            width = 6,
            plotlyOutput("line_chart_area", height = height_px)
          )
        ),
        br(),
        fluidRow(column(
          width = 12,
          plotlyOutput("global_average", height = height_px_average)
        ))))
    } else {
      return(tagList(
        tags$div(
          style = "display: flex; justify-content: center; align-items: center; gap: 20px;height: 80vh;",  # 设置 Flexbox 样式
          plotlyOutput("line_chart_area", height = height_px),
          plotlyOutput("selected_average", height = height_px)
        )
      )
      )
    }
  })
  
}

#############
# RUN SHINY #
#############

shinyApp(ui, server)

