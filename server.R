library(shiny)
library(ggplot2)
library(rms)
library(dplyr)
library(margins)

shinyServer(function(input, output) {

    Z975 <- qnorm(0.975)
    n <- 10^4
    
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
    
    #myseed <- reactive(input$link)
    mylink <- reactive(
        "logit"    
    )
    
    b0  <- reactive(input$beta0)
    # From N&D p12, footnote to table 1; 
    # creating as reactive in case I ever want to have them
    #    changeable
    bd  <- reactive(0.5)
    b1  <- reactive(1)
    b2  <- reactive(2)
    b3  <- reactive(1)
    b4  <- reactive(3)
    
    
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
        
        # Note that in their supplement, N&D use a Normal error distribution
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

    output$showtabTreated <- renderPrint({
        print(table(dat()$xd))    
    })
    output$showtabOutcome <- renderPrint({
        print(table(dat()$y))    
    })

    
    ################################################################
    ################################################################
    ########### Logistic models ####################################
    ################################################################
    ################################################################
    
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
    
    ################################################################
    ################################################################
    ########### Linear models ####################################
    ################################################################
    ################################################################
    
    fitx1x2.lm <- reactive({
        myfit <- lm(y ~ xd + x1 + x2,
            data= dat()
        )        
        myfit
    })
    fitx1x2x3.lm <- reactive({
        myfit <- lm(y ~ xd + x1 + x2 + x3,
            data= dat()
        )        
        myfit
    })
    fitx1x2x3x4.lm <- reactive({
        myfit <- lm(y ~ xd + x1 + x2 + x3 + x4,
            data= dat()
        )        
        myfit
    })
    
    ################################################################
    ################################################################
    ########### Marginal effectss ###############################
    ################################################################
    ################################################################
    
    getAveMargDiff <- function(dat, fit, vname, wantExp) {
        res <- matrix(NA, nrow= 2, ncol= 4)
        for (i in 0:1) {
            if (wantExp) {
                m <- margins(fit, type= "response", 
                    data= dat[dat$xd == i, ])
            } else {
                m <- margins(fit, data= dat[dat$xd == i, ])
            }
            msum <- summary(m)
            est <- msum[msum$factor == vname, "AME"]
            #print(est)
            se  <- msum[msum$factor == vname, "SE"]
            res[i + 1, ] <- c(i, est, est - Z975 * se, est + Z975 * se)
        }
        #print(res)
        res
    }
    getAveMargDiffs <- function(dat, f1, f2, f3, f1.lm, f2.lm, f3.lm, vname) {
        mydat <- data.frame(rbind(
            cbind(getAveMargDiff(dat, f1, vname, wantExp= TRUE), rep("Logistic", 2)),   
            cbind(getAveMargDiff(dat, f2, vname, wantExp= TRUE), rep("Logistic", 2)),    
            cbind(getAveMargDiff(dat, f3, vname, wantExp= TRUE), rep("Logistic", 2)),   
            cbind(getAveMargDiff(dat, f1.lm, vname, wantExp= FALSE), rep("Linear", 2)),   
            cbind(getAveMargDiff(dat, f2.lm, vname, wantExp= FALSE), rep("Linear", 2)),    
            cbind(getAveMargDiff(dat, f3.lm, vname, wantExp= FALSE), rep("Linear", 2))    
        ), stringsAsFactors= FALSE)
        names(mydat) <- c("xd", "Est", "LB", "UB", "Type")
        mydat <- within(mydat, {
            Est <- as.numeric(Est)    
            LB <- as.numeric(LB)    
            UB <- as.numeric(UB)    
        })
        
        # assuming these correspond to f1 f2 f3 
        mydat$fit <- rep(rep(c("x1x2", "x1x2x3", "x1x2x3x4"), each= 2), 2)
        
        #print(str(mydat))
        mydat
    }
    aveMargDiffs.x1 <- reactive({
        getAveMargDiffs(dat(), fitx1x2(), fitx1x2x3(), fitx1x2x3x4(), 
            fitx1x2.lm(), fitx1x2x3.lm(), fitx1x2x3x4.lm(), 
            "x1")
    })
    aveMargDiffs.xd <- reactive({
        getAveMargDiffs(fitx1x2(), fitx1x2x3(), fitx1x2x3x4(), 
            fitx1x2.lm(), fitx1x2x3.lm(), fitx1x2x3x4.lm(), 
            "xd")
    })
    
    ################################################################
    ################################################################
    ########### Estimates and CIs ###############################
    ################################################################
    ################################################################
    
    getEstAndCI <- function(fit, vname, wantExp) {
        est <- coef(fit)[vname]
        se <- sqrt(vcov(fit)[vname, vname])
        res <- c(est, est - Z975 * se, est + Z975 * se)
        if (wantExp) exp(res) else res
    }
    getEstsAndCIs <- function(f1, f2, f3, f1.lm, f2.lm, f3.lm, vname) {
        mydat <- data.frame(rbind(
            c(getEstAndCI(f1, vname, wantExp= TRUE), "Logistic"),   
            c(getEstAndCI(f2, vname, wantExp= TRUE), "Logistic"),    
            c(getEstAndCI(f3, vname, wantExp= TRUE), "Logistic"),   
            c(getEstAndCI(f1.lm, vname, wantExp= FALSE), "Linear"),   
            c(getEstAndCI(f2.lm, vname, wantExp= FALSE), "Linear"),    
            c(getEstAndCI(f3.lm, vname, wantExp= FALSE), "Linear")    
        ), stringsAsFactors= FALSE)
        names(mydat) <- c("Est", "LB", "UB", "Type")
        mydat <- within(mydat, {
            Est <- as.numeric(Est)    
            LB <- as.numeric(LB)    
            UB <- as.numeric(UB)    
        })
        
        # assuming these correspond to f1 f2 f3 
        mydat$fit <- rep(c("x1x2", "x1x2x3", "x1x2x3x4"), 2)
        
        #print(str(mydat))
        mydat
    }
    EstsAndCIs.x1 <- reactive({
        getEstsAndCIs(fitx1x2(), fitx1x2x3(), fitx1x2x3x4(), 
            fitx1x2.lm(), fitx1x2x3.lm(), fitx1x2x3x4.lm(), 
            "x1")
    })
    EstsAndCIs.xd <- reactive({
        getEstsAndCIs(fitx1x2(), fitx1x2x3(), fitx1x2x3x4(), 
            fitx1x2.lm(), fitx1x2x3.lm(), fitx1x2x3x4.lm(), 
            "xd")
    })
    
    
    ################################################################
    ################################################################
    ########### Predicted probabilitie##############################
    ################################################################
    ################################################################
    
    makeNewdat <- function(dat) {
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
        nd
    }
    newdat <- reactive({
        makeNewdat(dat())    
    })
    makePred <- function(nd, fit, fitname) {
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
            fit <- fitname
        })
        #cat(str(nd))
        nd
    }
    allPreds <- function(nd, f1, f2, f3, f1.lm, f2.lm, f3.lm, vname) {
        mydat.logistic <- data.frame(rbind(
            makePred(nd, f1, "x1x2"),   
            makePred(nd, f2, "x1x2x3"),    
            makePred(nd, f3, "x1x2x3x4")
        ), stringsAsFactors= FALSE)
        mydat.logistic$Type <- "Logistic"
        #cat(str(mydat.logistic))
        
        mydat.linear <- data.frame(rbind(
            makePred(nd, f1.lm, "x1x2"),   
            makePred(nd, f2.lm, "x1x2x3"),    
            makePred(nd, f3.lm, "x1x2x3x4")    
        ), stringsAsFactors= FALSE)
        mydat.linear$Type <- "Linear"
        
        mydat <- rbind(mydat.logistic, mydat.linear)
        #cat(str(mydat))
        mydat
    }
    stackedPredDat <- reactive({
        allPreds(newdat(), fitx1x2(), fitx1x2x3(), fitx1x2x3x4(),
            fitx1x2.lm(), fitx1x2x3.lm(), fitx1x2x3x4.lm(),
            "xd")
    })
    
    ##########################################################
    ##########################################################
    #############  Plots #####################################
    ##########################################################
    ##########################################################
    
    output$ystarHist <- renderPlot({
        ggplot(data= dat(),
            mapping= aes(x= ystar)) +
            geom_histogram(bins= 30) +
            geom_vline(xintercept= 0, colour= "red") +
            xlab("Y* (underlying continuous variable)") +
            xlim(-35, 35) +
            theme(strip.text.x = element_text(size = 14)) +
            theme(axis.title.x = element_text(size = 14)) +
            theme(axis.title.y = element_text(size = 14)) +
            theme(axis.text.x = element_text(size = 12)) +
            theme(axis.text.y = element_text(size = 12)) +
            theme_bw()
    })
    
    output$resPlot.x1 <- renderPlot({
        ggplot(data= EstsAndCIs.x1(),
            mapping= aes(x= fit, y= Est, ymin= LB, ymax= UB, colour= fit)) +
            scale_colour_manual("Model", values= my.colorscale.3, guide= FALSE) +
            geom_linerange(size= 1.5) +
            geom_point(size= 2.5, shape= 15) +
            xlab("Model") +
            ylab("Beta or OR, with 95% CI") +
            theme_bw() +
            theme(strip.text.x = element_text(size = 14)) +
            theme(axis.title.x = element_text(size = 14)) +
            theme(axis.title.y = element_text(size = 14)) +
            theme(axis.text.x = element_text(size = 12)) +
            theme(axis.text.y = element_text(size = 12)) +
            facet_wrap(~ Type, ncol= 2)
    })
    
    output$predPlot.x1 <- renderPlot({
        ggplot(data= stackedPredDat(),
            mapping= aes(x= x1, y= yhat, ymin= lower, ymax= upper, 
                colour= fit, fill= fit, linetype= xd)) +
            scale_colour_manual("Model", values= my.colorscale.3) +
            scale_fill_manual("Model", values= my.colorscale.3) +
            scale_linetype_manual("xd", values= c('solid', 'dotted')) +
            geom_ribbon(colour= NA, alpha= 0.5) +
            geom_line(size= 1.5) +
            xlab("x1") +
            ylab("Predicted probability, with 95% CI") +
            ylim(-0.2, 1.2) +
            geom_hline(yintercept= 0, size= 0.5, linetype= "dotted") +
            geom_hline(yintercept= 1, size= 0.5, linetype= "dotted") +
            theme_bw() +
            theme(strip.text.x = element_text(size = 14)) +
            theme(axis.title.x = element_text(size = 14)) +
            theme(axis.title.y = element_text(size = 14)) +
            theme(axis.text.x = element_text(size = 12)) +
            theme(axis.text.y = element_text(size = 12)) +
            facet_wrap(~ Type, ncol= 2)
    })

    output$aveMargDiffPlot.x1 <- renderPlot({
        ggplot(data= aveMargDiffs.x1(),
            mapping= aes(x= fit, y= Est, ymin= LB, ymax= UB, colour= fit,
                linetype= xd, shape= xd)) +
            scale_colour_manual("Model", values= my.colorscale.3, guide= FALSE) +
            geom_linerange(size= 1.5) +
            geom_point(size= 2.5) +
            scale_linetype_manual("xd", values= c('solid', 'dotted')) +
            xlab("Model") +
            ylab("Average marginal effect, with 95% CI") +
            theme_bw() +
            theme(strip.text.x = element_text(size = 14)) +
            theme(axis.title.x = element_text(size = 14)) +
            theme(axis.title.y = element_text(size = 14)) +
            theme(axis.text.x = element_text(size = 12)) +
            theme(axis.text.y = element_text(size = 12)) +
            facet_wrap(~ Type, ncol= 2)
    })
    
    # not currently using this one
    output$predPlot.xd <- renderPlot({
        mydat <- stackedPredDat() %>%
            group_by(Type, fit, xd) %>%
            slice(floor(n()/2))%>%
            ungroup()
            
        ggplot(data= mydat,
            mapping= aes(x= xd, y= yhat, ymin= lower, ymax= upper, 
                colour= fit, size= fit)) +
            scale_colour_manual("Model", values= my.colorscale.3) +
            scale_size_manual("Model", 
                values= c(3, 2, 1)) +
            geom_linerange(alpha= 0.5) +
            geom_point() +
            xlab("xd") +
            ylab("Predicted probability, with other covariate(s) fixed") +
            ylim(-0.2, 1.2) +
            theme_bw() +
            facet_wrap(~ Type, ncol= 2)
    })
    # not currently using this one
    output$resPlot.xd <- renderPlot({
        ggplot(data= EstsAndCIs.xd(),
            mapping= aes(x= fit, y= Est, ymin= LB, ymax= UB, colour= fit)) +
            scale_colour_manual("Model", values= my.colorscale.3) +
            geom_pointrange(size= 1) +
            xlab(NULL) +
            ylab("Beta or OR for xd, with 95% CI") +
            theme_bw() +
            facet_wrap(~ Type, ncol= 2)
    })
    # not currently using this one
    output$aveMargDiffPlot.xd <- renderPlot({
        ggplot(data= aveMargDiffs.xd(),
            mapping= aes(x= fit, y= Est, ymin= LB, ymax= UB, colour= fit)) +
            scale_colour_manual("Model", values= my.colorscale.3) +
            geom_pointrange(size= 1) +
            xlab(NULL) +
            ylab("Average marginal effect for xd, with 95% CI") +
            theme_bw() +
            facet_wrap(~ Type, ncol= 2)
    })
    
})
