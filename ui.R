
shinyUI(fluidPage(

    # Application title
    titlePanel("Exploration of concepts from Norton and Dowd (2017)"),
    
    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            sliderInput("beta0",
                "Intercept for underlying regression:",
                min = -6,
                max = 6,
                value = 0
            ),
            sliderInput("errSD",
                "SD of error for underlying regression:",
                min = 0.01,
                max = 3,
                value = 1
            ),
            sliderInput("propTx",
                "Proportion treated:",
                min = 0,
                max = 1,
                value = 0.5
            )
        ), # end panel
    
        # Show a plot of the generated distribution
        mainPanel(
            plotOutput("predPlot"),
            h6("Breakdown of treatment groups:"),
            verbatimTextOutput("showtabOrig"),
            h6("Results from regressions:"),
            verbatimTextOutput("showResOrig")

        )
    ) # end sidebarLayout
))
