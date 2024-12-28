pacman::p_load(
  rio,        # importing data
  here,       # relative file pathways
  janitor,    # data cleaning and tables
  lubridate,  # working with dates
  matchmaker, # dictionary-based cleaning
  epikit,     # age_categories() function
  tidyverse,   # data management and visualization
  dplyr,
  ggplot2,
  leaflet,
  geoR,
  rgdal,
  raster,
  sp,
  spdep,
  SpatialEpi,
  SpatialEpiApp)

install.packages("INLA",
                 repos = "https://inla.r-inla-download.org/R/stable", dep = TRUE)

install.packages("fmesher", repos = "https://inla.r-inla-download.org/R/stable")

if (!requireNamespace("INLA", quietly = TRUE)) {
  install.packages("INLA", repos = "https://inla.r-inla-download.org/R/stable")
}

install.packages("C:/Users/HP/Downloads/INLA_24.06.27.zip", repos = NULL, type = "win.binary")

library(INLA)


install.packages('SpatialEpi',depend=TRUE)
library(SpatialEpi)
library(dplyr)
data(pennLC)
lc_data <- pennLC
head(lc_data,3)
class(lc_data)
names(lc_data)
str(lc_data)

?pennLC

head(lc_data$data)

map <- lc_data$spatial.polygon
plot(map)

d <- group_by(lc_data$data,county) %>%
  summarize(Y = sum(cases))
head(d)

d1 <- aggregate(lc_data$data$cases,
                by = list(county = lc_data$data$county),
                FUN = sum
                )
head(d1)

names(d1) <- c("county", "Y")
head(d1)

lc_data$data <- lc_data$data[order(lc_data$data$county, lc_data$data$race, lc_data$data$gender, lc_data$data$age), ]
lc_data$data

E <- expected(population = lc_data$data$population, cases = lc_data$data$cases, n.strata = 16)

#d$E <- (match(d$county, unique(lc_data$data$county)))
d$E <- E

head(d)


# Smorkers Proportions


d <- merge(d, pennLC$smoking, by = "county")
head(d)

# SMRs = Y/E

d$SMR <- d$Y/d$E
head(d)

# Add data to the map

library(sp)
rownames(d) <- d$county
map <- SpatialPolygonsDataFrame(map, d, match.ID = TRUE)
head(map@data)

# Mapping SMR

library(leaflet)
l <- leaflet(map) %>% addTiles()
pal <- colorNumeric(palette = "YlOrRd", domain = map$SMR)
l %>% addPolygons(color = "grey", weight = 1, fillColor = ~pal(SMR), fillOpacity = 0.5 )%>%
  addLegend(pal = pal, values = ~SMR, opacity = 0.5, title = "SMR", position = "bottomright")


labels <- sprintf("<strong>%s</strong><br/>Observed: %s <br/>Expected: %s <br/>Smokers proportion: %s <br/>SMR: %s",
                  map$county, map$Y,  round(map$E, 2), map$smoking, round(map$SMR, 2)) %>%
  lapply(htmltools::HTML)

l %>% addPolygons(color = "grey", weight = 1, fillColor = ~pal(SMR), fillOpacity = 0.5,
                  highlightOptions = highlightOptions(weight = 4),
                  label = labels,
                  labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"),
                                              textsize = "15px", direction = "auto")) %>%
  addLegend(pal = pal, values = ~SMR, opacity = 0.5, title = "SMR", position = "bottomright")

# Model

library(spdep)
library(INLA)
nb <- poly2nb(map)
head(nb)

nb2INLA("map.adj", nb)
g <- inla.read.graph(filename = "map.adj")
g

# Inference using INLA

map$re_u <- 1:nrow(map@data)
map$re_v <- 1:nrow(map@data)

formula <- Y ~ smoking + f(re_u, model = "besag", graph = g, scale.model = TRUE) + f(re_v, model = "iid")
formula

res <- inla(formula, family = "poisson", data = map@data, E = E, control.predictor = list(compute = TRUE))
res

# Results

summary(res)

# proltting

library(ggplot2)
marginal <- inla.smarginal(res$marginals.fixed$smoking)
marginal <- data.frame(marginal)
ggplot(marginal, aes(x = x, y = y)) + geom_line() + labs(x = expression(beta[1]), y = "Density") +
  geom_vline(xintercept = 0, col = "blue") + theme_bw()

# Adding results to Map

head(res$summary.fitted.values)


map$RR <- res$summary.fitted.values[, "mean"]
map$LL <- res$summary.fitted.values[, "0.025quant"]
map$UL <- res$summary.fitted.values[, "0.975quant"]

# Mapping disease risk

pal <- colorNumeric(palette = "YlOrRd", domain = map$RR)

labels <- sprintf("<strong> %s </strong> <br/> Observed: %s <br/> Expected: %s <br/>
                  Smokers proportion: %s <br/>SMR: %s <br/>RR: %s (%s, %s)",
                  map$county, map$Y,  round(map$E, 2),  map$smoking, round(map$SMR, 2),
                  round(map$RR, 2), round(map$LL, 2), round(map$UL, 2)) %>%
  lapply(htmltools::HTML)

leaflet(map) %>% addTiles() %>%
  addPolygons(color = "grey", weight = 1, fillColor = ~pal(RR),  fillOpacity = 0.5,
              highlightOptions = highlightOptions(weight = 4),
              label = labels,
              labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"),
                                          textsize = "15px", direction = "auto")) %>%
  addLegend(pal = pal, values = ~RR, opacity = 0.5, title = "RR", position = "bottomright")
