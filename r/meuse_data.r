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
tab <- data.table(analyte = c("cadmium", "copper", "lead", "zinc"))
tab[,valsSp:=lapply(analyte, function(x){x <- meuse[x]; names(x) <- "val"; return(x)})]
tab[,meuseGrid:=lapply(1:nrow(tab), function(x) SpatialPointsDataFrame(meuse.grid[c("x", "y")], data = meuse.grid)),]

## explore meuse data
par(mfrow = c(2,2))
lapply(tab$valsSp, function(x) hist(x$val))
lapply(tab$valsSp, function(x) plot(density(x$val)))
lapply(tab$valsSp, function(x) hist(log(x$val)))
lapply(tab$valsSp, function(x) plot(density(log(x$val))))

## calculate criteria for each analyte
tol <- function(x){
    normtol.int(x$val, 0.1, 0.9, log.norm = T)$"1-sided.upper"
}
tab[,logTol9090:=lapply(tab$valsSp, tol)]

## generate IDW interpolation for metals
idwFun <- function(valSp, meuseGrid){
    idwOut <- idw(val~1,
                  valSp,
                  newdata = meuseGrid,
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

tab[,idw:=mapply(idwFun, valsSp, meuseGrid)]
saveRDS(tab, "./data_output/surfaces.rds")
