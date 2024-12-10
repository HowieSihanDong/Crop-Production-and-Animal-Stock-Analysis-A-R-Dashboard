###################
# FUNCTION DESIGN #
###################
create_live_map <- function(data) {
  pal <- colorBin(
    palette = c(
      "#ffffcc",
      "#FFD580",
      "#FFC000",
      "#fd8d3c",
      "#cd1c18",
      "#950606"
    ),
    domain = data$cap_production,  
    bins = c(0, 1, 10, 100, 1000, 10000, Inf)  
  )
  # addProviderTiles(providers$OpenStreetMap.HOT) %>%
  map <- leaflet(data) %>%
    addProviderTiles(providers$OpenStreetMap.HOT) %>%
    setView(lng = 0, lat = 20, zoom = 2) %>%
    addPolygons(
      fillColor = ~ pal(cap_production),  
      weight = 1,
      color = "white",
      fillOpacity = 0.7,
      labelOptions = labelOptions(
        style = list("font-weight" = "normal"),
        textsize = "13px"
      ),
      highlightOptions = highlightOptions(
        weight = 5,
        color = "black"
      ),
      layerId = ~ admin,
      label = ~ paste(
        ifelse(
          is.na(cap_production) | cap_production < 0,
          paste("During the selected period, ", admin, "\nhas unknown livestock density (units per km²)"),
          paste(
            "During the selected period,\n", admin, "\nhas a livestock density of",
            ifelse(
              cap_production >= 1e5,
              paste(round(cap_production / 1e5, 2), "units per km²"),
              paste(cap_production, "units per km²")
            )
          )
        )
      )
      
      
    ) %>%
    addLegend(
      pal = pal,
      values = ~ cap_production,
      na.label = "Unknown",
      opacity = 0.7,
      position = "bottomleft",
      labFormat = function(type, cuts, p) {
        labels <- c(
          "0 units/km²",
          "1 units/km²",
          "10 units/km²",
          "100 units/km²",
          "1,000 units/km²",
          "10,000 units/km²"
        )
        return(labels)
      }
    )
  return(map)
}

create_crop_map <- function(data) {
  pal <- colorBin(
    palette = c(
      "#fafefa",
      "#cce7c9",
      "#5bb450",
      "#4b5320",
      "#1a2421"
    ),
    domain = data$cap_production, 
    bins = c(0, 1, 10, 100, 1000, 5000, Inf), 
    na.color = "gray"
  )
  
  map <- leaflet(data) %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    setView(lng = 0, lat = 20, zoom = 2) %>%
    addPolygons(
      fillColor = ~ pal(cap_production),  
      weight = 1,
      color = "white",
      fillOpacity = 0.7,
      labelOptions = labelOptions(
        style = list("font-weight" = "normal"),
        textsize = "13px"
      ),
      highlightOptions = highlightOptions(
        weight = 5,
        color = "black"
      ),
      layerId = ~ admin,
      label = ~ paste(
        ifelse(
          is.na(cap_production) | cap_production < 0,
          paste("During the selected period,", admin, "has unknown crop production per unit area"),
          paste(
            "During the selected period,", admin, "has a crop production of",
            ifelse(
              cap_production >= 1e6,
              paste(round(cap_production / 1e6, 2), "million tons per km²"),
              ifelse(
                cap_production >= 1e3,
                paste(round(cap_production / 1e3, 2), "thousand tons per km²"),
                paste(cap_production, "tons per km²")
              )
            )
          )
        )
      )
    ) %>%
    addLegend(
      na.label = "Unknown",
      title = "",
      values = ~ cap_production, 
      position = "bottomleft",
      pal = pal,
      labFormat = function(type, cuts, p) {
        labels <- c(
          "0 tons/km²",
          "1 tons/km²",
          "10 tons/km²",
          "100 tons/km²",
          "1,000 tons/km²",
          "5,000 tons/km²"
        )
        return(labels)
      },
      opacity = 0.7
    )
  return(map)
}


create_donut_data <- function(data) {
  #select top producted or pocesses items
  data_rose <- data %>%
    select(area, item, year, production)
  top_items <- data_rose %>%
    group_by(item) %>%
    summarise(total_production = sum(production, na.rm = TRUE)) %>%
    arrange(desc(total_production)) %>%
    slice_max(total_production, n = 3)
  data_rose <- data_rose %>%
    filter(item %in% top_items$item)
  
  # Generate Ranking within each item
  data_rose <- data_rose %>%
    group_by(item, area) %>%
    summarize(total_production = sum(production, na.rm = TRUE)) %>%
    arrange(item, desc(total_production)) %>%
    mutate(area_rank = row_number())
  
  # Save only top 2 country
  data_rose <- data_rose %>%
    filter(area_rank <= 2) %>%
    ungroup()
  
  data_rose_mean <- data_rose %>%
    group_by(item) %>%
    summarise(mean_item = mean(total_production),
              sd_item = sd(total_production))
  
  data_rose <- data_rose %>%
    left_join(data_rose_mean, by = "item")
  
  data_rose_final <- data_rose %>%
    mutate(count = total_production / 10000000) %>%
    select(-total_production, -mean_item, -sd_item, -area_rank)
  
  data_rose_final <- data_rose_final[!is.na(data_rose_final$item) &
                                       !is.na(data_rose_final$area), ]
  
  return(data_rose_final)
}

create_donut <- function(data) {
  PieDonut(
    data = data,
    aes(item, area, count = count),
    showPieName = FALSE,
    donutLabelSize = 5,
    pieLabelSize = 5,
    r0 = 0.1,
    color = "black",
    showRatioDonut = FALSE,
    r1 = 1,
    r2 = 1.2,
    maxx = 1.6,
    
    showRatioPie = FALSE
  )
}

create_donut_year <- function(data) {
  PieDonut(
    data = data,
    aes(area, count = count),
    showPieName = FALSE,
    pieLabelSize = 5,
    r0 = 0.2,
    color = "black",
    showRatioDonut = FALSE,
    showRatioPie = FALSE
  )
}

