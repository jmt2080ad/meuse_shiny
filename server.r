require(shiny)
require(data.table)
require(gstat)
require(sp)
require(ggplot2)

## read in processed meuse data
x <- readRDS("./data_output/surfaces.rds")

## generate IDW interpolation for metals, calculate MAE, plot results
idwFun <- function(valsSp, meuseGrid, nmin, nmax, maxdist, idp){
    idwOut <- idw(val~1,
                  valsSp,
                  newdata = meuseGrid,
                  nmin = nmin,
                  nmax = nmax,
                  maxdist = maxdist,
                  idp = idp)
    idwOut$X  <- idwOut@coords[,1]
    idwOut$Y  <- idwOut@coords[,2]
    idwOut    <- data.table(idwOut@data)
    idwOut.cv <- data.table(krige.cv(val~1,
                                     valsSp,
                                     nmin = nmin,
                                     nmax = nmax,
                                     maxdist = maxdist,
                                     set = list(idp = idp))@data)
    idwMAE   <- round(idwOut.cv[,mean(abs(residual)),], 3)
    idwSWAC  <- round(mean(idwOut$var1.pred), 3)
    valsSp$X <- valsSp@coords[,1]
    valsSp$Y <- valsSp@coords[,2]
    vals     <- data.table(valsSp@data) 
    mycols   <- colorRampPalette(c("skyblue2", "palegoldenrod", "orange", "red"))
    return(
        ggplot(data = idwOut, aes(x = X, y = Y)) +
        geom_raster(aes(fill = var1.pred)) +
        scale_fill_gradientn(colors = mycols(10)) +
        geom_point(data = vals, aes(x = X, y = Y), size = 0.5) + 
        theme_bw() +
        coord_fixed() +
        xlab("") +
        ylab("") +
        labs(title = paste0("MAE = ", idwMAE, "\nSWAC = ", idwSWAC),
             fill = "Concentration\n(mg/kg)",
             caption = "MAE = mean absolute error\nSWAC = surface weighted average concentration")
    )
}

server <- function(input, output){
    output$rastPlot <- renderPlot({
        validate(
            need(input$nmin < input$nmax, "Minimum number must be less than maximum number")
            )
        validate(
            need(input$nmin >= 0, "Minimum and maximum number of points must be greater than 0.")
            )
        validate(
            need(input$idp > 0, "IDW power must be greater than 0")
            )
        validate(
            need(input$maxdist > 0, "Search distance must be greater than 0")
            )
        idwFun(x[analyte == input$SelAna,valsSp][[1]],
               x[analyte == input$SelAna,meuseGrid][[1]],
               as.numeric(input$nmin),
               as.numeric(input$nmax),
               as.numeric(input$maxdist),
               as.numeric(input$idp)
               )
    })
}
