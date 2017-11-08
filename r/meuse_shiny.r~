library(gstat)
library(sp)
library(raster)
library(tolerance)
library(data.table)
library(parallel)

## read in processed meuse data
x <- readRDS("./data_output/surfaces.rds")

plot(x$idw[[1]], 
     breaks = c(0,10,20,40), 
     col = terrain.colors(3),
     main="Digital Surface Model (DSM)\n NEON Harvard Forest Field Site")

