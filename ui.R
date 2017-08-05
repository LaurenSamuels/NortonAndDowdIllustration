
shinyUI(fluidPage(

    # Application title
    titlePanel("Exploration of concepts from Norton and Dowd (2017)"),
    
    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            sliderInput("beta0",
                "Intercept for underlying continuous variable (higher intercept -> more outcome events):",
                min = -6,
                max = 6,
                value = 0
            ),
            sliderInput("errSD",
                "SD of error for underlying continuous variable:",
                min = 0.01,
                max = 3,
                value = 1
            ),
            sliderInput("propTx",
                "Proportion treated:",
                min = 0,
                max = 1,
                value = 0.5
            ),
            plotOutput("ystarHist"),
            h6("Treatment groups:"),
            verbatimTextOutput("showtabTreated"),
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
            tags$hr(),
            plotOutput("predPlot.xd")

        )
    ) # end sidebarLayout
))
