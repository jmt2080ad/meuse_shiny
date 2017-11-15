library(shiny)
library(data.table)
library(gstat)
library(sp)
library(ggplot2)

## read in processed meuse data
x <- readRDS("./data_output/surfaces.rds")
colRamp <- colorRampPalette(c('lightblue', 'yellow3', 'sandybrown', 'red'))

## generate IDW interpolation for metals, calculate MAE, plot results
idwFun <- function(valsSp, meuseGrid, nmin, nmax, maxdist, idp){
    idwOut <- idw(val~1,
                  valsSp,
                  newdata = meuseGrid,
                  nmin = nmin,
                  nmax = nmax,
                  maxdist = maxdist,
                  idp = idp)
    idwOut$X <- idwOut@coords[,1]
    idwOut$Y <- idwOut@coords[,2]
    idwOut <- data.table(idwOut@data)
    idwOut.cv <- data.table(
        krige.cv(val~1,
                 valsSp,
                 nmin = nmin,
                 nmax = nmax,
                 maxdist = maxdist,
                 set = list(idp = idp))@data
        )
    idwMAE <- round(idwOut.cv[,mean(abs(residual)),], 3)
    mycols <- colorRampPalette(c("skyblue2", "palegoldenrod", "palegreen3", "red"))
    return(
        ggplot(data = idwOut, aes(x = X, y = Y)) +
        geom_raster(aes(fill = var1.pred)) +
        scale_fill_gradientn(colors = mycols(10)) +
        xlab("") +
        ylab("") +
        theme_bw() +
        coord_fixed() +
        labs(title = paste("MAE =", idwMAE),
             fill = "Concentration\n(mg/kg)")
        )
}

## build shiny app
ui <- fluidPage(
    titlePanel("Meuse IDW with MAE (Mean Abolute Error)"),
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

shinyApp(ui = ui, server = server)
