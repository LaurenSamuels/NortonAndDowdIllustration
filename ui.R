
shinyUI(fluidPage(

    # Application title
    titlePanel("Exploration of concepts from Norton and Dowd (2017)"),
    
    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            sliderInput("propTx",
                "Proportion treated:",
                min = 0,
                max = 1,
                value = 0.5
            ),
            h6("Treatment groups:"),
            verbatimTextOutput("showtabTreated"),
            tags$hr(),
            tags$hr(),
            sliderInput("beta0",
                "Intercept for underlying continuous variable (higher intercept -> more outcome events):",
                min = -6,
                max = 6,
                value = 0
            ),
            sliderInput("errSD",
                "SD of error for underlying continuous variable (coefficients are in [0.5, 3]):",
                min = 0.01,
                max = 3,
                value = 1
            ),
            plotOutput("ystarHist"),
            h6("Outcomes:"),
            verbatimTextOutput("showtabOutcome")
        ), # end panel
    
        mainPanel(
            h4("Coefficient estimates"),
            plotOutput("resPlot.x1"),
            tags$hr(),
            plotOutput("resPlot.xd"),
            tags$hr(),
            h4("Predicted probabilities"),
            plotOutput("predPlot.x1"),
            h4("Average marginal effects"),
            plotOutput("aveMargDiffPlot.x1"),
            plotOutput("aveMargDiffPlot.xd")

        )
    ) # end sidebarLayout
))
