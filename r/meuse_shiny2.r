library(shiny)
library(data.table)
library(raster)
library(gstat)
library(sp)

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
                  omax = 0,
                  maxdist = maxdist,
                  idp = idp)
    idwOut <- raster(SpatialPixelsDataFrame(idwOut, idwOut@data))
    return(plot(idwOut, col = colRamp(length(idwOut))))
}

## build shiny app
ui <- fluidPage(
    sidebarLayout(
        sidebarPanel(
            selectInput("SelAna",
                        "Analyte",
                        c("Cadmium" = "cadmium",
                          "Copper" = "copper",
                          "Lead" = "lead",
                          "Zinc" = "zinc")),
            numericInput("nmin", "Minimum Points", 0),
            numericInput("nmax", "Maximum Points", 10),
            numericInput("maxdist", "Maximum Search Distance", 500),
            numericInput("idp", "IDW Power", 2)
            ),
        mainPanel(
            plotOutput(outputId = "rastPlot")
            )
        )
    )

server <- function(input, output){
    output$rastPlot <- renderPlot({
        validate(
            need(input$nmin < input$nmax, "Minimum number must be less than maximum number")
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

shinyApp(ui = ui, server = server)


