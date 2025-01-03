# -------------------------------------------------
# The goal is to predict malaria prevalence
# in The Gambia, Africa
# -------------------------------------------------
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

## ----------------------------------------------------

install.packages("INLA",
                 repos = "https://inla.r-inla-download.org/R/stable", dep = TRUE)

install.packages("fmesher", repos = "https://inla.r-inla-download.org/R/stable")

if (!requireNamespace("INLA", quietly = TRUE)) {
  install.packages("INLA", repos = "https://inla.r-inla-download.org/R/stable")
}

install.packages("C:/Users/HP/Downloads/INLA_24.06.27.zip", repos = NULL, type = "win.binary")

library(INLA)
## ___________________________________________________--
library(terra)
library(sf)
library(rgdal)
library(sp)
library(geoR)
library(raster)
library(dplyr)

## ----------------------------------------------

# loading the geoR package and attach the gambia data

library(geoR)
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

head(gb, 3)
head(gambia,3)

# ---------------------------------------------
# The data is given at individual level and the analysis
# is done at village level, therefore
# we aggregate as below.
# --------------------------------------------

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

# prevalence

library(leaflet)
library(terra)
library(geodata)

# ---------------------------------
# look at the dimension of the unique coordinates
# and see that the 2035 tests are conducted in
# 65 unique locations
# ----------------------------------------

dim(unique(gambia[, c("x", "y")]))

# ------------------------------------------
# Transform coordinates

# The spTransform() function of the sp package.
# helps to transform UTM coordinates in the
# data to geographic coordinates
# ------------------------------------------

library(sp)
library(rgdal)

sps  <- SpatialPoints(d[, c("x", "y")], proj4string = CRS("+proj=utm +zone=28"))
spst <- spTransform(sps, CRS("+proj=longlat +datum=WGS84"))

# --------------------------------------------
# Here we add the longitude and latitude variables
# to the data frame d
# ---------------------------------------------

d[, c("long", "lat")] <- coordinates(spst)

#d[, c("long", "lat")] <- round(coordinates(spst), 3)
head(d)

# -----------------------------------------------
# Map Prevalence
# -----------------------------------------------

library(leaflet)

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

# ---------------------------------------
# We add the elevation variable to the data frame d
# because we will use it as a covariate in the model.
# ---------------------------------------------
d$alt <- extract(r, d[, c("long", "lat")])
head(d)


# -------------------------------------
# Modelling
# We specify the model to predict malaria prevalence
# in The Gambia, and detail the steps to fit the model
# using the INLA and SPDE approaches.
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
dim(A)

# Prediction data

dp <- rasterToPoints(r)
dp <- terra::as.points(r)
dim(dp)


ra <- aggregate(r, fact = 5, fun = mean)

dp <- terra::as.points(ra)
dim(dp)

coop <- dp[, c("x", "y")]
# Extract coordinates as a matrix
coop <- geom(dp)[, c("x", "y")]

# Check dimensions or structure
dim(coop)
head(coop)


# -------------------------------
# Projector matrix
# construct the matrix that projects the locations
# where we will do the predictions.
# ----------------------------------------

Ap <- inla.spde.make.A(mesh = mesh, loc = coop)
dim(Ap)


# Stack data for the estimation and prediction


#stack for estimation stk.e
stk.e <- inla.stack(tag = "est",
data = list(y = d$positive, numtrials = d$total),
A = list(1, A),
effects = list(data.frame(b0 = 1, cov = d$alt), s = indexs))

#stack for prediction stk.p
stk.p <- inla.stack(tag = "pred",
data = list(y = NA, numtrials = NA),
A = list(1, Ap),
effects = list(data.frame(b0 = 1, cov = dp[, 3]), s = indexs))

#stk.full has stk.e and stk.p
stk.full <- inla.stack(stk.e, stk.p)
