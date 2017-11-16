library(shiny)
library(ggplot2)

## build shiny app
ui <- fluidPage(
    titlePanel("Meuse IDW with MAE (Mean Absolute Error)"),
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
