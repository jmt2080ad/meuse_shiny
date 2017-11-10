library(shiny)
library(data.table)
library(raster)
library(gstat)
library(sp)
library(ggplot2)

## read in processed meuse data
x <- readRDS("./data_output/surfaces.rds")
colRamp <- colorRampPalette(c('lightblue', 'yellow3', 'sandybrown', 'red'))

## generate IDW interpolation for metals
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
    idwOut.cv <- krige.cv(log(val)~1,
                          valsSp,
                          nmin = nmin,
                          nmax = nmax,
                          maxdist = maxdist,
                          nfold = 10,
                          set = list(idp = idp))@data
    idwOut.cv <- data.table(idwOut.cv)
    idwRMSE   <- round(sqrt(idwOut.cv[,mean(residual^2),by=fold][,mean(V1),]), 2)
    return(eval({
        ggplot(idwOut, aes(X, Y, fill = var1.pred)) +
            geom_raster() +
            scale_fill_gradient(low = "skyblue3", high = "red") +
            xlab("") +
            ylab("") +
            labs(fill = "Predicted\nValues", title = paste("RMSE (5-fold) =", idwRMSE)) +
            coord_fixed()
    }))
}

valsSp = x$valsSp[[1]]
nmin = 0
nmax = 10
maxdist = 500
idp = 1

## build shiny app
ui <- fluidPage(
    titlePanel("Meuse IDW with RMSE"),
    sidebarLayout(
        sidebarPanel(
            selectInput("SelAna",
                        "Analyte",
                        c("Cadmium" = "cadmium",
                          "Copper"  = "copper",
                          "Lead"    = "lead",
                          "Zinc"    = "zinc")),
            numericInput("nmin", "Minimum Points", 0),
            numericInput("nmax", "Maximum Points", 5),
            numericInput("maxdist", "Maximum Search Distance", 500),
            numericInput("idp", "IDW Power", 2)
            ),
        mainPanel(
            plotOutput(outputId = "rastPlot", height = "550px")
            )
        )
    )

server <- function(input, output){
    output$rastPlot <- renderPlot({
        validate(
            need(input$nmin < input$nmax, "Minimum number must be less than maximum number")
        );
        idwFun(x[analyte == input$SelAna,valsSp][[1]],
               x[analyte == input$SelAna,meuseGrid][[1]],
               as.numeric(input$nmin),
               as.numeric(input$nmax),
               as.numeric(input$maxdist),
               as.numeric(input$idp)
               )
    })
}

shinyApp(ui = ui, server = server)
