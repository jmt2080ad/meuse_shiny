require(shiny)

## build shiny app
ui <- fluidPage(
    fluidRow(
        titlePanel("IDW with the Meuse dataset.")
    ),
    fluidRow(
        column(4,
               includeMarkdown("./markdown/intro.md")
               ),
        column(2,
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
        column(6,
               plotOutput(outputId = "rastPlot", height = "600px")
               )
    )
)


