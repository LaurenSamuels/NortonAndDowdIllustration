
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(ggplot2)
library(rms)
library(dplyr)

shinyServer(function(input, output) {

    Z975 <- qnorm(0.975)
    
    # Color scales
    # CB-friendly, from colorbrewer2
    my.colorscale.2 <- c("#7570b3", "#d95f02")
    my.colorscale.3 <- c("#d95f02", "#1b9e77", "#7570b3")
    my.colorscale.4 <- c('#a6cee3','#1f78b4','#b2df8a','#33a02c')
    
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
            Treated <- factor(xd)
            ystar <- as.matrix(dat) %*% multvec
            y <- as.numeric(ystar > 0)
        })
        
        dat    
    })

    output$showtabOrig <- renderPrint({
        print(table(dat()$y))    
    })

    fitx1x2 <- reactive({
        myfit <- glm(y ~ xd + x1 + x2,
            family= binomial(link= mylink()),
            data= dat()
        )        
        myfit
    })
    fitx1x2x3 <- reactive({
        myfit <- glm(y ~ xd + x1 + x2 + x3,
            family= binomial(link= mylink()),
            data= dat()
        )        
        myfit
    })
    fitx1x2x3x4 <- reactive({
        myfit <- glm(y ~ xd + x1 + x2 + x3 + x4,
            family= binomial(link= mylink()),
            data= dat()
        )        
        myfit
    })
    
    getLPs <- function(fit) {
        fit$linear.predictors    
    }
    lpx1x2 <- reactive({
        getLPs(fitx1x2())
    })
    lpx1x2x3 <- reactive({
        getLPs(fitx1x2x3())
    })
    
    lpx1x2x3x4 <- reactive({
        getLPs(fitx1x2x3x4())
    })
    
    makeMargDiffs.xd <- function(dat, fit) {
        d1 <- dat
        d1$xd <- rep(1, n)
        p1 <- predict(fit, newdata= d1, type= "response", se.fit= FALSE) 
        
        d0 <- dat
        d0$xd <- rep(0, n)
        p0 <- predict(fit, newdata= d0, type= "response", se.fit= FALSE) 
        
        p1 - p0
    }
    margDiffsx1x2.xd <- reactive({
        makeMargDiffs.xd(dat(), fitx1x2())
    })
    margDiffsx1x2x3.xd <- reactive({
        makeMargDiffs.xd(dat(), fitx1x2x3())
    })
    margDiffsx1x2x3x4.xd <- reactive({
        makeMargDiffs.xd(dat(), fitx1x2x3x4())
    })
    
    getORAndCI <- function(fit, vname) {
        est <- coef(fit)[vname]
        se <- sqrt(vcov(fit)[vname, vname])
        exp(c(est, est - Z975 * se, est + Z975 * se))
    }
    
    getORsAndCIs <- function(f1, f2, f3, vname) {
        mydat <- data.frame(rbind(
            getORAndCI(f1, vname),   
            getORAndCI(f2, vname),    
            getORAndCI(f3, vname)    
        ))
        names(mydat) <- c("OR", "LB", "UB")
        
        # assuming these correspond to f1 f2 f3 
        mydat$fit <- c("x1x2", "x1x2x3", "x1x2x3x4")
        
        mydat
    }
    ORsAndCIs.xd <- reactive({
        getORsAndCIs(fitx1x2(), fitx1x2x3(), fitx1x2x3x4(), "xd")
    })
    
    output$showResOrig <- renderPrint({
        #print(fitx1x2())
        # TODO: convert to table
        print(exp(coef(fitx1x2())["xd"]))
        print(exp(coef(fitx1x2x3())["xd"]))
        print(exp(coef(fitx1x2x3x4())["xd"]))
        print(mean(margDiffsx1x2.xd()))    
        print(mean(margDiffsx1x2x3.xd()))    
        print(mean(margDiffsx1x2x3x4.xd()))    
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
        #cat(str(pred1))
        nd <- within(nd, {
            yhat <- pred1$fit
            lower <- yhat - Z975 * pred1$se.fit
            upper <- yhat + Z975 * pred1$se.fit
            xd <- factor(xd)
        })
        nd
    }
    predx1x2 <- reactive({
        dat <- makePred(dat(), fitx1x2())
        dat$fit <- "x1x2"
        dat
    })
    predx1x2x3 <- reactive({
        dat <- makePred(dat(), fitx1x2x3())
        dat$fit <- "x1x2x3"
        dat
    })
    predx1x2x3x4 <- reactive({
        dat <- makePred(dat(), fitx1x2x3x4())
        dat$fit <- "x1x2x3x4"
        dat
    })
    stackedPredDat <- reactive({
        rbind(predx1x2(), predx1x2x3(), predx1x2x3x4())
    })
    
    ##########################################################
    ##########################################################
    #############  Plots #####################################
    ##########################################################
    ##########################################################
    
    output$rawPlot.x1 <- renderPlot({
        
        ggplot(data= dat(),
            mapping= aes(x= x1, y= ystar, shape= Treated, linetype= Treated)) +
            geom_point() +
            # TODO: this isn't right
            geom_smooth(method= "lm") +
            geom_hline(yintercept= 0, colour= "red") +
            theme_bw()
    })
    
    
    # TODO: next do same for linear probability model
    output$predPlot.x1 <- renderPlot({
        ggplot(data= stackedPredDat(),
            mapping= aes(x= x1, y= yhat, ymin= lower, ymax= upper, 
                colour= fit, fill= fit, linetype= xd)) +
            scale_colour_manual("Model", values= my.colorscale.3) +
            scale_fill_manual("Model", values= my.colorscale.3) +
            scale_linetype_manual("Treated", 
                values= c('solid', 'dotted')) +
            geom_ribbon(colour= NA, alpha= 0.2) +
            geom_line(size= 1) +
            xlab("x1, with other continuous covariate(s) fixed") +
            ylab("Predicted probability") +
            ylim(-0.1, 1.1) +
            theme_bw()
    })
    output$predPlot.xd <- renderPlot({
        mydat <- stackedPredDat() %>%
            group_by(fit, xd) %>%
            slice(floor(n()/2))%>%
            ungroup()
            
        ggplot(data= mydat,
            mapping= aes(x= xd, y= yhat, ymin= lower, ymax= upper, 
                colour= fit, size= fit)) +
            scale_colour_manual("Model", values= my.colorscale.3) +
            scale_size_manual("Model", 
                values= c(2.5, 2, 1)) +
            geom_linerange(alpha= 0.5) +
            geom_point(size= 2) +
            xlab("xd, with other covariate(s) fixed") +
            ylab("Predicted probability") +
            ylim(-0.1, 1.1) +
            theme_bw()
    })

    output$resPlot.xd <- renderPlot({
        ggplot(data= ORsAndCIs.xd(),
            mapping= aes(x= fit, y= OR, ymin= LB, ymax= UB, colour= fit)) +
            scale_colour_manual("Model", values= my.colorscale.3) +
            geom_linerange() +
            geom_point(size= 2) +
            ylab("OR for xd, with 95% CI") +
            theme_bw()
    })

})
