library(gstat)
library(sp)
library(raster)
library(tolerance)
library(data.table)
library(parallel)

## read in meuse data
data(meuse)
data(meuse.grid)
class(meuse)

## convert meuse data to spatial
meuse <- SpatialPointsDataFrame(meuse[c("x", "y")], data = meuse)
class(meuse)

## build table for storing results
tab <- data.table(analyte = c("cadmium", "copper", "lead", "zinc"),
                  coords = list(as(meuse, "SpatialPoints")))
tab[,vals:=lapply(analyte, function(x) meuse[[x]])]

## explore meuse data
par(mfrow = c(2,2))
lapply(tab$vals, function(x) hist(x))
lapply(tab$vals, function(x) plot(density(x)))
lapply(tab$vals, function(x) hist(log(x)))
lapply(tab$vals, function(x) plot(density(log(x))))

## calculate criteria for each analyte
tol <- function(x){
    normtol.int(x, 0.1, 0.9, log.norm = T)$"1-sided.upper"
}
tab[,logTol9090:=lapply(vals, tol)]

## generate IDW interpolation for metals
idwFun <- function(val, coords){
    coords <- SpatialPointsDataFrame(coords, data = data.frame(val = val))
    idwOut <- idw(val~1,
                  coords,
                  newdata = SpatialPointsDataFrame(meuse.grid[c("x", "y")], data = meuse.grid),
                  nmax = 10,
                  nmin = 4,
                  omax = 0,
                  maxdist = 1000,
                  block = numeric(0),
                  na.action = na.pass,
                  idp = 2.0,
                  debug.level = 1)
    idwOut <- raster(SpatialPixelsDataFrame(idwOut, idwOut@data))
    return(idwOut)
}

tab[,idw:=mapply(idwFun, vals, coords)]

saveRDS(tab, "./data_output/surfaces.rds")
