
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(ggplot2)
library(rms)

shinyServer(function(input, output) {

    Z975 <- qnorm(0.975)
    
    #myseed <- reactive(input$randseed)
    myseed <- reactive(
        3517
    )
    
    mysd <- reactive(input$errSD)
    myproptx <- reactive(input$propTx)
    
    mylink <- reactive(
        "logit"    
    )
    
    b0  <- reactive(input$beta0)
    bd  <- reactive(1)
    b1  <- reactive(1)
    b2  <- reactive(1)
    b3  <- reactive(1)
    b4  <- reactive(1)
    
    
    n <- 10^2
    
    dat <- reactive({
        # Take care of the random seed.
        #    Code from Cole in nbpMatching pkg
        if(exists(".Random.seed", envir = .GlobalEnv)) {
            save.seed <- get(".Random.seed", envir= .GlobalEnv)
            on.exit(assign(".Random.seed", save.seed, envir = .GlobalEnv))
        } else {
            on.exit(rm(".Random.seed", envir = .GlobalEnv))
        }
        set.seed(myseed())
        
        x0 <- rep(1, n)
        xd <- rbinom(n, 1, myproptx())
        x1 <- rnorm(n)
        x2 <- rnorm(n)
        x3 <- rnorm(n)
        x4 <- rnorm(n)
        
        if (mylink() == "logit") {
            err <- rlogis(n, location= 0, scale= mysd())    
        } else {
            # TODO
        }
        
        dat <- data.frame(x0, xd, x1, x2, x3, x4, err)
        multvec <- c(b0(), bd(), b1(), b2(), b3(), b4(), 1)
        dat <- within(dat, {
            ystar <- as.matrix(dat) %*% multvec
            y <- as.numeric(ystar > 0)
        })
        
        dat    
    })

    output$showtabOrig <- renderPrint({
        print(table(dat()$y))    
    })

    fitOrig <- reactive({
        myfit <- glm(y ~ xd + x1 + x2,
            family= binomial(link= mylink()),
            data= dat()
        )        
        myfit
    })
    fitFull <- reactive({
        myfit <- glm(y ~ xd + x1 + x2 + x3 + x4,
            family= binomial(link= mylink()),
            data= dat()
        )        
        myfit
    })
    
    getLPs <- function(fit) {
        fit$linear.predictors    
    }
    lpOrig <- reactive({
        getLPs(fitOrig())
    })
    lpFull <- reactive({
        getLPs(fitFull())
    })
    
    makeMargDiffs <- function(dat, fit) {
        d1 <- dat
        d1$xd <- rep(1, n)
        p1 <- predict(fit, newdata= d1, type= "response", se.fit= FALSE) 
        
        d0 <- dat
        d0$xd <- rep(0, n)
        p0 <- predict(fit, newdata= d0, type= "response", se.fit= FALSE) 
        
        p1 - p0
    }
    margDiffsOrig <- reactive({
        makeMargDiffs(dat(), fitOrig())
    })
    margDiffsFull <- reactive({
        makeMargDiffs(dat(), fitFull())
    })
    
    output$showResOrig <- renderPrint({
        #print(fitOrig())
        # TODO: convert to table
        print(exp(coef(fitOrig())["xd"]))
        print(exp(coef(fitFull())["xd"]))
        print(mean(margDiffsOrig()))    
        print(mean(margDiffsFull()))    
    })
    makePred <- function(dat, fit) {
        mylen <- 100
        xd <- 0:1
        x1 <- seq(min(dat$x1), max(dat$x1), length.out= mylen)
        nd <- expand.grid(xd= xd, x1= x1)
        nr <- nrow(nd)
        nd <- within(nd, {
            x2 <- rep(median(dat$x2), nr)
            x3 <- rep(median(dat$x3), nr)
            x4 <- rep(median(dat$x4), nr)
        })
        pred1 <- predict(fit, 
            newdata= nd,
            type= "response",
            se.fit= TRUE) 
        cat(str(pred1))
        nd <- within(nd, {
            yhat <- pred1$fit
            lower <- yhat - Z975 * pred1$se.fit
            upper <- yhat + Z975 * pred1$se.fit
            xd <- factor(xd)
        })
        nd
    }
    predOrig <- reactive({
        dat <- makePred(dat(), fitOrig())
        dat$fit <- "Orig"
        dat
    })
    predFull <- reactive({
        dat <- makePred(dat(), fitFull())
        dat$fit <- "Full"
        dat
    })
    
    # TODO: next do same for linear probability model
    output$predPlot <- renderPlot({
        ggplot(data= rbind(predOrig(), predFull()),
            mapping= aes(x= x1, y= yhat, ymin= lower, ymax= upper, 
                colour= xd, fill= xd, linetype= fit)) +
            geom_ribbon(colour= NA, alpha= 0.2) +
            geom_line() +
            ylab("Predicted probability") +
            ylim(-0.1, 1.1) +
            theme_bw()
    })

})
