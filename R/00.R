pacman::p_load(
  rio,             # data import/export     
  here,            # locate files
  tidyverse,       # data management and visualization
  flexdashboard,   # dashboard versions of R Markdown reports
  shiny,           # interactive figures
  plotly,           # interactive figures
  sf,            # to manage spatial data using a Simple Feature format
  tmap,          # to produce simple maps, works for both interactive and static maps
  janitor,       # to clean column names
  OpenStreetMap, # to add OSM basemap in ggplot map
  spdep,          # spatial statistics
  DT
)

# import the linelist
linelist <- import("https://github.com/appliedepi/epirhandbook_eng/raw/master/data/case_linelists/linelist_cleaned.rds")
dim(linelist)
view(linelist)

# generate 1000 random row numbers, from the number of rows in linelist
sample_rows <- sample(nrow(linelist), 1000)

# subset linelist to keep only the sample rows, and all columns
linelist <- linelist[sample_rows,]
linelist
dim(linelist)
view(linelist)

# Create sf object
linelist_sf <- linelist %>%
  sf::st_as_sf(coords = c("lon", "lat"), crs = 4326)

DT::datatable(head(linelist_sf, 10), rownames = FALSE, options = list(pageLength = 5, scrollX=T), class = 'white-space: nowrap' )


# ADM3 level clean
sle_adm3 <- sle_adm3_raw %>%
  janitor::clean_names() %>% # standardize column names
  filter(admin2name %in% c("Western Area Urban", "Western Area Rural")) # filter to keep certain areas
