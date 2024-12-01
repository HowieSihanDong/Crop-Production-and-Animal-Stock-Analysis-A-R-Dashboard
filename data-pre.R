####Data Import####

inconsistent <- c(
  # "United States of America" = "America",
  "Bahamas" = "The Bahamas",
  "Bolivia (Plurinational State of)" = "Bolivia",
  "Brunei Darussalam" = "Brunei",
  "Cabo Verde" = "Cape Verde",
  "China, Hong Kong SAR" = "Hong Kong S.A.R.",
  "China, Macao SAR" = "Macao S.A.R.",
  "China, Taiwan Province of" = "Taiwan",
  "China, mainland" = "China",
  "Côte d'Ivoire" = "Ivory Coast",
  "Congo" = "Republic of Congo",
  "Czechia" = "Czech Republic",
  "Democratic People's Republic of Korea" = "North Korea",
  "Eswatini" = "Swaziland",
  "Guinea-Bissau" = "Guinea Bissau",
  "Iran (Islamic Republic of)" = "Iran",
  "Lao People's Democratic Republic" = "Laos",
  "Micronesia (Federated States of)" = "Federated States of Micronesia",
  "Netherlands (Kingdom of the)" = "Netherlands",
  "North Macedonia" = "Macedonia",
  "Republic of Korea" = "South Korea",
  "Republic of Moldova" = "Moldova",
  "Russian Federation" = "Russia",
  "Serbia" = "Republic of Serbia",
  "Sudan (former)" = "Sudan",
  "Syrian Arab Republic" = "Syria",
  "Timor-Leste" = "East Timor",
  "Türkiye" = "Turkey",
  "United Kingdom of Great Britain and Northern Ireland" = "United Kingdom",
  "Venezuela (Bolivarian Republic of)" = "Venezuela",
  "Viet Nam" = "Vietnam"
)
### Livestock data
livestock <- read.csv('livestock.csv', header = TRUE)
colnames(livestock) <- tolower(colnames(livestock)) #lowercase
colnames(livestock)[colnames(livestock) == "value"] <- "production"
livestock$area <- recode(livestock$area, !!!inconsistent)

### Crop data
crop_2000_2009 <- read.csv('food_product_2000-.csv', header = TRUE)
crop_2010 <- read.csv('food_product_2010-.csv', header = TRUE)
cropstock <- rbind(crop_2000_2009, crop_2010)
colnames(cropstock) <- tolower(colnames(cropstock)) #lowercase
colnames(cropstock)[colnames(cropstock) == "value"] <- "production"
cropstock$area <- recode(cropstock$area, !!!inconsistent)

### Population data
population <- read.csv('population.csv', header = TRUE)
colnames(population) <- tolower(colnames(population)) #lowercase
colnames(population)[colnames(population) == "value"] <- "population"
population$area <- recode(population$area, !!!inconsistent)

item_mapping <- c(
  "Almonds, in shell" = "Almonds",
  "Anise, badian, coriander, cumin, caraway, fennel and juniper berries, raw" = "Mixed Spices",
  "Cantaloupes and other melons" = "Melons",
  "Cotton lint, ginned" = "Cotton",
  "Maize (corn)" = "Corn",
  "Onions and shallots, dry (excluding dehydrated)" = "Dry Onions",
  "Other berries and fruits of the genus vaccinium n.e.c." = "Other Berries",
  "Other citrus fruit, n.e.c." = "Other Citrus Fruits",
  "Other fruits, n.e.c." = "Other Fruits",
  "Other nuts (excluding wild edible nuts and groundnuts), in shell, n.e.c." = "Other Nuts",
  "Other pulses n.e.c." = "Other Pulses",
  "Other stone fruits" = "Other Stone Fruits",
  "Other vegetables, fresh n.e.c." = "Other Vegetables",
  "Pistachios, in shell" = "Pistachios",
  "Plums and sloes" = "Plums",
  "Raw cane or beet sugar (centrifugal only)" = "Raw Sugar",
  "Seed cotton, unginned" = "Seed Cotton",
  "Sunflower-seed oil, crude" = "Sunflower Oil",
  "Chillies and peppers, green (Capsicum spp. and Pimenta spp.)" = "Green Chillies and Peppers",
  "Chillies and peppers, dry (Capsicum spp., Pimenta spp.), raw" = "Dry Chillies and Peppers",
  "Groundnuts, excluding shelled" = "Groundnuts",
  "Rape or colza seed" = "Rapeseed",
  "Rapeseed or canola oil, crude" = "Rapeseed Oil",
  "Safflower-seed oil, crude" = "Safflower Oil",
  "Kenaf, and other textile bast fibres, raw or retted" = "Kenaf and Bast Fibres",
  "Oil of palm kernel" = "Palm Kernel Oil",
  "Palm kernels" = "Palm Kernels",
  "Oil palm fruit" = "Oil Palm Fruit",
  "Sisal, raw" = "Sisal",
  "Mangoes, guavas and mangosteens" = "Mangoes and Guavas",
  "Other tropical fruits, n.e.c." = "Other Tropical Fruits",
  "Flax, raw or retted" = "Flax",
  "Other fibre crops, raw, n.e.c." = "Other Fibre Crops",
  "Other oil seeds, n.e.c." = "Other Oil Seeds",
  "Peppermint, spearmint" = "Peppermint and Spearmint",
  "Hazelnuts, in shell" = "Hazelnuts",
  "Green corn (maize)" = "Green Corn",
  "Mixed grain" = "Mixed Grain",
  "True hemp, raw or retted" = "Hemp",
  "Coconuts, in shell" = "Coconuts",
  "Plantains and cooking bananas" = "Plantains",
  "Coir, raw" = "Raw Coir",
  "Ginger, raw" = "Raw Ginger",
  "Jute, raw or retted" = "Jute",
  "Natural rubber in primary forms" = "Natural Rubber",
  "Other sugar crops n.e.c." = "Other Sugar Crops",
  "Other pome fruits" = "Other Pome Fruits",
  "Chicory roots" = "Chicory Roots",
  "Vanilla, raw" = "Raw Vanilla",
  "Edible roots and tubers with high starch or inulin content, n.e.c., fresh" = "Edible Roots and Tubers",
  "Nutmeg, mace, cardamoms, raw" = "Nutmeg and Cardamoms",
  "Pepper (Piper spp.), raw" = "Pepper",
  "Cinnamon and cinnamon-tree flowers, raw" = "Cinnamon",
  "Cloves (whole stems), raw" = "Cloves",
  "Beer of barley, malted" = "Malt beer",
  "Beans, dry"    = "Dry Beans",
  "Broad beans and horse beans, dry" = "Dry Broad Beans",
  "Broad beans and horse beans, green" = "Green Broad Beans",
  "Chestnuts, in shell" = "Chestnuts",
  "Eggplants (aubergines)" = "Eggplants",
  "Leeks and other alliaceous vegetables" = "Leeks",
  "Onions and shallots, green" = "Green Onions",
  "Other beans, green" = "Other Green Beans",
  "Other stimulant, spice and aromatic crops, n.e.c." = "Other Spices",
  "Peas, green" = "Green Peas",
  "Pumpkins, squash and gourds" = "Pumpkins and Squash",
  "Tangerines, mandarins, clementines" = "Tangerines",
  "Chick peas, dry" = "Dry Chick Peas",
  "Dry Chillies and Peppers" = "Dry Chillies and Peppers",
  "Locust beans (carobs)" = "Carobs",
  "Peas, dry" = "Dry Peas",
  "Cashew nuts, in shell" = "Cashew Nuts",
  "Cassava, fresh" = "Cassava",
  "Cereals n.e.c." = "Other Cereals",
  "Edible Roots and Tubers" = "Roots and Tubers",
  "Karite nuts (sheanuts)" = "Shea Nuts",
  "Brazil nuts, in shell" = "Brazil Nuts",
  "Pyrethrum, dried flowers" = "Pyrethrum",
  "Ramie, raw or retted" = "Ramie",
  "Bambara beans, dry" = "Dry Bambara Beans",
  "Agave fibres, raw, n.e.c." = "Agave Fibres",
  "Abaca, manila hemp, raw" = "Abaca",
  "Kapok fibre, raw" = "Kapok Fibre",
  "Kapokseed in shell" = "Kapok Seeds",
  "Green tea (not fermented), black tea (fermented) and partly fermented tea, in immediate packings of a content not exceeding 3 kg" = "Tea"
  
)

# Replace item names with concise names
cropstock$item <- recode(cropstock$item, !!!item_mapping)

## world data loding ##
world <- ne_countries(scale = "medium", returnclass = "sf") %>%
  mutate(admin = trimws(admin)) 


# 将地理坐标系转换为平面坐标系 EPSG:3857（用于面积计算）
world <- st_transform(world, crs = 4326)

# 计算地面积（单位：平方米），并转换为平方公里
world <- world %>%
  mutate(area_km2 = as.numeric(st_area(geometry)) / 1e6)

#Delete NA data
livestock <- livestock %>% filter(!is.na(area), !is.na(item), !is.na(year))
cropstock <- cropstock %>% filter(!is.na(area), !is.na(item), !is.na(year))


data_live_map <- livestock %>%
  group_by(area, item, year) %>%
  summarise(production = round(sum(production, na.rm = TRUE), 2)) %>%
  ungroup()



data_crop_map <- cropstock %>%
  group_by(area, item, year) %>%
  summarise(production = round(sum(production, na.rm = TRUE), 2)) %>%
  ungroup()


#### Tab 2 #####
data_population_line <- population %>%
  group_by(area, year)  %>%
  summarise(population = mean(population)) %>%
  ungroup()

pop_crop <- data_population_line %>%
  left_join(data_crop_map, by = c("year", "area"))

pop_live <- data_population_line %>%
  left_join(data_live_map, by = c("year", "area"))