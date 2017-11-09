library(shiny)
library(raster)
library(data.table)
library(sp)

## read in processed meuse data
x <- readRDS("./data_output/surfaces.rds")
colRamp <- colorRampPalette(c('lightblue', 'yellow3', 'sandybrown', 'red'))

## build shiny app
ui <- fluidPage(
    selectInput("SelAna",
                "Select Analyte",
                c("Cadmium" = "cadmium",
                  "Copper" = "copper",
                  "Lead" = "lead",
                  "Zinc" = "zinc")),
    plotOutput(outputId = "rastPlot")
    )

server <- function(input, output){
    output$rastPlot <- renderPlot(plot(x[analyte == input$SelAna,idw][[1]],
                                       col = colRamp(length(x[analyte == input$SelAna,idw][[1]]))))
}

shinyApp(ui = ui, server = server)

