
shinyUI(fluidPage(

    # Application title
    titlePanel(HTML(paste0("Exploration of some concepts from Norton, E. C., & Dowd, B. E. (2017).",
        " Log Odds and the Interpretation of Logit Models.",
        " Health Services Research, 80(1), 123–20 ",
        tags$a("(http://doi.org/10.1111/1475-6773.12712)", 
            href= "http://doi.org/10.1111/1475-6773.12712",
            target= "_blank")))),
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
            verbatimTextOutput("showtabOutcome"),
            tags$hr(),
            h6("Version 0.2.0"), 
            tags$a("GitHub repository",
                href= "https://github.com/LaurenSamuels/NortonAndDowdIllustration",
                target= "_blank")
        ), # end panel
    
        mainPanel(
            h4("Coefficient estimates for x1"),
            plotOutput("resPlot.x1"),
            tags$hr(),
            h4("Predicted probabilities, with other continuous covariate(s) fixed"),
            plotOutput("predPlot.x1"),
            h4("Average marginal effects for x1"),
            plotOutput("aveMargDiffPlot.x1")

        )
    ) # end sidebarLayout
))
