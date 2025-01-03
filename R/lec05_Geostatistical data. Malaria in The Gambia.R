pacman::p_load(
  dplyr,
  ggplot2,
  leaflet,
  geoR,
  rgdal,
  raster,
  sp,
  spdep,
  SpatialEpi,
  SpatialEpiApp,
  tgda
)

install.packages("sf")
install.packages("terra")
install.packages("rgdal")
library(terra)
library(sf)
library(rgdal)
library(sp)
library(geoR)
library(raster)
data(gambia)
gb <- gambia

#Explanation of the gambia variables
# x: x-coordinate of the village (UTM),
# y: y-coordinate of the village (UTM),
# pos: presence (1) or absence (0) of malaria in a blood sample taken from the child,
# age: age of the child in days,
# netuse: indicator variable denoting whether (1) or not (0) the child regularly sleeps under a bed-net,
# treated: indicator variable denoting whether (1) or not (0) the bed-net is treated (coded 0 if netuse = 0),
# green: satellite-derived measure of the greenness of vegetation in the immediate vicinity of the village (arbitrary units),
# phc: indicator variable denoting the presence (1) or absence (0) of a health center in the village.

dim(gb)

head(gb)

dim(unique(gambia[, c("x", "y")]))


d <- group_by(gambia, x, y) %>%
  summarize(total = n(),
            positive  = sum(pos),
            prev = positive/total
            )
head(d)

# Transform coordinates
#Tranforming the UTM coordinates to
# geographical coordinates bese leafrates requires so

sps  <- SpatialPoints(d[, c("x", "y")], proj4string = CRS("+proj=utm +zone=28"))
spst <- spTransform(sps, CRS("+proj=longlat +datum=WGS84"))

d[, c("long", "lat")] <- coordinates(spst)

head(d)

# Map prevalence

library(leaflet)
library(geodata)

pal <- colorBin("viridis", bins = c(0, 0.25, 0.5, 0.75, 1))
leaflet(d) %>%  addProviderTiles(providers$CartoDB.Positron) %>%
  addCircles(lng = ~long, lat = ~lat, color = ~pal(prev)) %>%
  addLegend("bottomright", pal = pal, values = ~prev, title = "Prevalence") %>%
  addScaleBar(position = c("bottomleft"))

# Environmental covariates

install.packages("geodata")
library(sp)
library(raster)
library(geodata)
#r <- getData(name = 'alt', country = 'GMB', mask = TRUE)


# Get elevation data for The Gambia
r <- elevation_30s(country = "GMB", mask = TRUE, path = "C:/Users/HP/AppData/Local/R/win-library/4.4/spdep/data")

class(r)
# Plot the elevation data (optional)
plot(r)

# Convert raster object to SpatRaster (for terra)

class(r)

install.packages("leaflet")
library(leaflet)
library(sp)
library(raster)
library(terra)
library(spdep)

# Find the path to the data directory within the package
package_path <- system.file("data", package = "spdep")
print(package_path)
data_files <- list.files(package_path)
print(data_files)

# Find the installed package directory
package_path <- find.package("spdep")  # or any other package you are using
cat("Package path:", package_path)

# List all files within the data folder of the package
data_files <- list.files(file.path(package_path, "data"))
print(data_files)

# If downloading the data from a URL:
download.file("https://geodata.ucdavis.edu/geodata/elv/GMB_elv_msk.zip", destfile = "gambia_elevation.zip")

# Unzip the file
unzip("gambia_elevation.zip", exdir = "gambia_elevation")

# Load the raster from the extracted folder
r <- rast("gambia_elevation/GMB_elv_msk.tif")


pal <- colorNumeric("viridis", values(r), na.color = "transparent")

leaflet() %>% addProviderTiles(providers$CartoDB.Positron) %>%
  addRasterImage(r, colors = pal, opacity = 0.5) %>%
  addLegend("bottomright", pal = pal, values = values(r), title = "Altitude") %>%
  addScaleBar(position = c("bottomleft"))

library(terra)
library(dplyr)

# Assuming `d` has longitude and latitude columns
# Extract coordinates and ensure it is a data.frame
coords <- d[, c("long", "lat")]

# Convert to SpatialPoints object if needed
coordinates(coords) <- ~ long + lat

# Extract raster values at these coordinates
d$alt <- extract(r, coords)
head(d)

d$alt <- extract(r, d[, c("long", "lat")])
head(d)
# Build mesh

library(INLA)
coo <- cbind(d$long, d$lat)
mesh <- inla.mesh.2d(loc = coo, max.edge = c(0.1, 5), cutoff = 0.01)

mesh$n
head(mesh)
head(mesh, 3)

plot(mesh)
points(coo, col = "red")

# Build the SPDE model on the mesh

spde <- inla.spde2.matern(mesh = mesh, alpha = 2)

# Index set
indexs <- inla.spde.make.index("s", spde$n.spde)
lengths(indexs)

# Projector matrix

A <- inla.spde.make.A(mesh = mesh, loc = coo)

# Prediction data
dp <- rasterToPoints(r)
dp <- terra::as.points(r)
dim(dp)
