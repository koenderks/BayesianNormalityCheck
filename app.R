list.of.packages <- c("shiny", "shinydashboard", "shinyjs", "diptest", "moments", 
                      "bridgesampling", "rstan", "inline", "shinycssloaders", "sn",
                      "xtable", "Rcpp")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library(shiny); library(shinydashboard); library(shinyjs); library(diptest); 
library(moments); library(bridgesampling); library(rstan); library(inline);
library(shinycssloaders); library(sn); library(xtable); library(Rcpp)

stanMixture <- readRDS("stanMixture_linux.rds")
stanNormal <- readRDS("stanNormal_linux.rds")
stanSkew <- readRDS("stanSkew_linux.rds")

CosNormSample <- function(x, lambda1 = NULL, lambda2 = NULL, lambda0 = NULL, theta = NULL, sigma = NULL){
    
    prob <- (theta * (1/sqrt(2*pi*(sigma^2))) * exp(-(1/(2*(sigma^2)))*(x - lambda1)^2)) + ((1-theta) * (1/sqrt(2*pi*(sigma^2))) *  exp(-(1/(2*(sigma^2)))*(x - lambda2)^2))
    
    return(prob)
    
}

parameternamesfunc <- function(choice, value){
    if(choice == "Normal" & value == 1){
        return("Mean")
    }
    if(choice == "Gamma" & value == 1){
        return("Shape")
    }
    if(choice == "Cauchy" & value == 1){
        return("Location")
    }
    if(choice == "Normal" & value == 2){
        return("Std. deviation")
    } 
    if(choice == "Gamma" & value == 2){
        return("Scale")
    }
    if(choice == "Cauchy" & value == 2){
        return("Scale")
    }
}

ui <- fluidPage(
    
    tags$style(HTML(".js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar {background: #337ab7}")),
    
    tags$style(type="text/css",
               ".shiny-output-error { visibility: hidden; }",
               ".shiny-output-error:before { visibility: hidden; }"
    ),
    
    title = "Bayesian Normality Check",
    useShinyjs(),
    fluidRow(
        column(3,
               h3("A Bayesian Test for Normality"),
               a("Click here to read the article", href="https://www.google.nl",target="_blank"),
               p(),
               fileInput(inputId = "file", "Upload CSV File",
                         multiple = FALSE,
                         accept = c("text/csv",
                                    "text/comma-separated-values,text/plain",
                                    ".csv")),
               selectInput('column', 'Test variable', choices = list("Upload data first...")),
               fluidRow(
                   column(4,
                          actionButton(inputId = "restart", label = "Start", icon = icon("play-circle"),
                                       style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
                   ), 
                   column(1,
                          actionButton(inputId = "button", label = "Prior parameters", icon = icon("wrench"),
                                       style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
                   )
               ),
               p(),
               sliderInput(inputId = "bins",label = "No. of histogram breaks",min = 10,max = 50,value = 10),
               checkboxInput(inputId = "paramest", label = "Show parameter estimates",value = FALSE)
        ),
        column(3,
               h3("Priors for the Normal Model", id = "title.normal"),
               fluidRow(
                   column(3,
                          selectInput('mu_normal', "Mu", list("Normal", "Cauchy", "Gamma"),selected = "Normal",width = "100%")
                   ),
                   column(3,
                          numericInput("mu_normal_value1", "value", 0, min = -1000, max = 1000, width = "150%")
                   ),
                   column(3,
                          numericInput("mu_normal_value2", "value", 100, min = -1000, max = 1000, width = "100%")
                   )
               ),
               fluidRow(
                   column(3,
                          selectInput('sigma_normal', "Sigma", list("Normal", "Cauchy", "Gamma"),selected = "Normal",width = "100%")
                   ),
                   column(3,
                          numericInput("sigma_normal_value1", "value", 1, min = -1000, max = 1000, step = 0.5, width = "100%")
                   ),
                   column(3,
                          numericInput("sigma_normal_value2", "value", 100, min = -1000, max = 1000, step = 0.5, width = "100%")
                   )
               )
        ),
        column(3,
               h3("Priors for the Mixture-normal Model", id = "title.mixture"),
               fluidRow(
                   column(3,
                          selectInput('theta_mixture', "Theta", list("Normal", "Cauchy", "Gamma"),selected = "Normal",width = "100%")
                   ),
                   column(3,
                          numericInput("theta_mixture_value1", "value", 0.5, min = -1000, max = 1000, width = "100%")
                   ),
                   column(3,
                          numericInput("theta_mixture_value2", "value", .3, min = -1000, max = 1000, width = "100%")
                   )
               ),
               fluidRow(
                   column(3,
                          selectInput('lambda_mixture', "Lambda1", list("Normal", "Cauchy", "Gamma"),selected = "Normal",width = "100%")
                   ),
                   column(3,
                          numericInput("lambda_mixture_value1", "value", 0, min = -1000, max = 1000, width = "100%")
                   ),
                   column(3,
                          numericInput("lambda_mixture_value2", "value", 100, min = -1000, max = 1000, width = "100%")
                   )
               ),
               fluidRow(
                   column(3,
                          selectInput('sigma_mixture', "Sigma", list("Normal", "Cauchy", "Gamma"),selected = "Normal",width = "100%")
                   ),
                   column(3,
                          numericInput("sigma_mixture_value1", "value", 1, min = -1000, max = 1000, width = "100%")
                   ),
                   column(3,
                          numericInput("sigma_mixture_value2", "value", 100, min = -1000, max = 1000, width = "100%")
                   )
               )
        ),
        column(3,
               h3("Priors for the Skew-normal Model", id = "title.skew"),
               fluidRow(
                   column(3,
                          selectInput('mu_skew', "Mu", list("Normal", "Cauchy", "Gamma"),selected = "Normal",width = "100%")
                   ),
                   column(3,
                          numericInput("mu_skew_value1", "value", 0, min = -1000, max = 1000, width = "100%")
                   ),
                   column(3,
                          numericInput("mu_skew_value2", "value", 100, min = -1000, max = 1000, width = "100%")
                   )
               ),
               fluidRow(
                   column(3,
                          selectInput('sigma_skew', "Sigma", list("Normal", "Cauchy", "Gamma"),selected = "Normal",width = "100%")
                   ),
                   column(3,
                          numericInput("sigma_skew_value1", "value", 1, min = -1000, max = 1000, width = "100%")
                   ),
                   column(3,
                          numericInput("sigma_skew_value2", "value", 100, min = -1000, max = 1000, width = "100%")
                   )
               ),
               fluidRow(
                   column(3,
                          selectInput('alpha_skew', "Alpha", list("Normal", "Cauchy", "Gamma"),selected = "Normal",width = "100%")
                   ),
                   column(3,
                          numericInput("alpha_skew_value1", "value", 0, min = -1000, max = 1000, width = "100%")
                   ),
                   column(3,
                          numericInput("alpha_skew_value2", "value", 100, min = -1000, max = 1000, width = "100%")
                   )
               )
        )
    ),
    
    hr(),
    
    fixedRow(
        column(4,
               shinycssloaders::withSpinner(ui_element = tableOutput("result"),type = 3, 
                                            color = "#337ab7", size = 3, color.background = "white"),
               tableOutput(outputId = "secondtable"),
               tableOutput(outputId = "estimates")
        ),
        column(5,
               shinycssloaders::withSpinner(ui_element = plotOutput("plot"),type = 3,
                                            color = "#337ab7",size = 3, color.background = "white")
        )
    )
)

server <- function(input, output, session) {
    
    fits <- reactiveValues()
    fits$keeptrack <- "funny placeholder thingy"
    
    MakeEmptyTables <- function(){
        
        fits$storetable <- fits$table
        fits$storeBFmat <- fits$BFmat
        fits$storeestim <- fits$estimates
        
        d <- data.frame("p.value" = c("", ""),
                        "BF01" = c("", ""),
                        "log.BF01" = c("", ""))
        rownames(d) <- c("H0: Normality vs. H1: Non-normality", 
                         "H0: Normality vs. H2: Skewness")
        fits$table <- d
        
        BFmat <- data.frame("H0" = c("", "", ""),
                            "H1" = c("", "", ""), 
                            "H2" = c("", "", ""))
        rownames(BFmat) <- colnames(BFmat)
        
        fits$BFmat <- BFmat
        
        f <- data.frame("Estimate" = "")
        rownames(f) <- ""
        
        fits$estimates <- f
    }
    
    RestoreTables <- function(){
        
        fits$table <- fits$storetable
        fits$BFmat <- fits$storeBFmat
        fits$estimates <- fits$storeestim
            
    }
    
    shinyjs::hide(id = "title.normal")
    shinyjs::hide(id = "mu_normal")
    shinyjs::hide(id = "mu_normal_value1")
    shinyjs::hide(id = "mu_normal_value2")
    shinyjs::hide(id = "sigma_normal")
    shinyjs::hide(id = "sigma_normal_value1")
    shinyjs::hide(id = "sigma_normal_value2")
    shinyjs::hide(id = "title.mixture")
    shinyjs::hide(id = "theta_mixture")
    shinyjs::hide(id = "theta_mixture_value1")
    shinyjs::hide(id = "theta_mixture_value2")
    shinyjs::hide(id = "lambda_mixture")
    shinyjs::hide(id = "lambda_mixture_value1")
    shinyjs::hide(id = "lambda_mixture_value2")
    shinyjs::hide(id = "sigma_mixture")
    shinyjs::hide(id = "sigma_mixture_value1")
    shinyjs::hide(id = "sigma_mixture_value2")
    shinyjs::hide(id = "title.skew")
    shinyjs::hide(id = "mu_skew")
    shinyjs::hide(id = "mu_skew_value1")
    shinyjs::hide(id = "mu_skew_value2")
    shinyjs::hide(id = "sigma_skew")
    shinyjs::hide(id = "sigma_skew_value1")
    shinyjs::hide(id = "sigma_skew_value2")
    shinyjs::hide(id = "alpha_skew")
    shinyjs::hide(id = "alpha_skew_value1")
    shinyjs::hide(id = "alpha_skew_value2")
    
    contentsrea <- reactive({
        inFile <- input$file
        if (is.null(inFile))
            return(NULL)
        read.csv(inFile$datapath)
    })
    
    observe({
        updateSelectInput(session, "column", choices = names(contentsrea()))
        
        updateNumericInput(session, "mu_normal_value1", label = parameternamesfunc(input$mu_normal, 1))
        updateNumericInput(session, "sigma_normal_value1", label = parameternamesfunc(input$sigma_normal, 1))
        updateNumericInput(session, "theta_mixture_value1", label = parameternamesfunc(input$theta_mixture, 1))
        updateNumericInput(session, "lambda_mixture_value1", label = parameternamesfunc(input$lambda_mixture, 1))
        updateNumericInput(session, "sigma_mixture_value1", label = parameternamesfunc(input$sigma_mixture, 1))
        updateNumericInput(session, "mu_skew_value1", label = parameternamesfunc(input$mu_skew, 1))
        updateNumericInput(session, "sigma_skew_value1", label = parameternamesfunc(input$sigma_skew, 1))
        updateNumericInput(session, "alpha_skew_value1", label = parameternamesfunc(input$alpha_skew, 1))
        
        updateNumericInput(session, "mu_normal_value2", label = parameternamesfunc(input$mu_normal, 2))
        updateNumericInput(session, "sigma_normal_value2", label = parameternamesfunc(input$sigma_normal, 2))
        updateNumericInput(session, "theta_mixture_value2", label = parameternamesfunc(input$theta_mixture, 2))
        updateNumericInput(session, "lambda_mixture_value2", label = parameternamesfunc(input$lambda_mixture, 2))
        updateNumericInput(session, "sigma_mixture_value2", label = parameternamesfunc(input$sigma_mixture, 2))
        updateNumericInput(session, "mu_skew_value2", label = parameternamesfunc(input$mu_skew, 2))
        updateNumericInput(session, "sigma_skew_value2", label = parameternamesfunc(input$sigma_skew, 2))
        updateNumericInput(session, "alpha_skew_value2", label = parameternamesfunc(input$alpha_skew, 2))
    })
    
    observeEvent(input$button, {
        
        if(input$button %% 2 == 0){
            shinyjs::hide(id = "title.normal")
            shinyjs::hide(id = "mu_normal")
            shinyjs::hide(id = "mu_normal_value1")
            shinyjs::hide(id = "mu_normal_value2")
            shinyjs::hide(id = "sigma_normal")
            shinyjs::hide(id = "sigma_normal_value1")
            shinyjs::hide(id = "sigma_normal_value2")
            shinyjs::hide(id = "title.mixture")
            shinyjs::hide(id = "theta_mixture")
            shinyjs::hide(id = "theta_mixture_value1")
            shinyjs::hide(id = "theta_mixture_value2")
            shinyjs::hide(id = "lambda_mixture")
            shinyjs::hide(id = "lambda_mixture_value1")
            shinyjs::hide(id = "lambda_mixture_value2")
            shinyjs::hide(id = "sigma_mixture")
            shinyjs::hide(id = "sigma_mixture_value1")
            shinyjs::hide(id = "sigma_mixture_value2")
            shinyjs::hide(id = "title.skew")
            shinyjs::hide(id = "mu_skew")
            shinyjs::hide(id = "mu_skew_value1")
            shinyjs::hide(id = "mu_skew_value2")
            shinyjs::hide(id = "sigma_skew")
            shinyjs::hide(id = "sigma_skew_value1")
            shinyjs::hide(id = "sigma_skew_value2")
            shinyjs::hide(id = "alpha_skew")
            shinyjs::hide(id = "alpha_skew_value1")
            shinyjs::hide(id = "alpha_skew_value2")
        }else{
            shinyjs::show(id = "title.normal")
            shinyjs::show(id = "mu_normal")
            shinyjs::show(id = "mu_normal_value1")
            shinyjs::show(id = "mu_normal_value2")
            shinyjs::show(id = "sigma_normal")
            shinyjs::show(id = "sigma_normal_value1")
            shinyjs::show(id = "sigma_normal_value2")
            shinyjs::show(id = "title.mixture")
            shinyjs::show(id = "theta_mixture")
            shinyjs::show(id = "theta_mixture_value1")
            shinyjs::show(id = "theta_mixture_value2")
            shinyjs::show(id = "lambda_mixture")
            shinyjs::show(id = "lambda_mixture_value1")
            shinyjs::show(id = "lambda_mixture_value2")
            shinyjs::show(id = "sigma_mixture")
            shinyjs::show(id = "sigma_mixture_value1")
            shinyjs::show(id = "sigma_mixture_value2")
            shinyjs::show(id = "title.skew")
            shinyjs::show(id = "mu_skew")
            shinyjs::show(id = "mu_skew_value1")
            shinyjs::show(id = "mu_skew_value2")
            shinyjs::show(id = "sigma_skew")
            shinyjs::show(id = "sigma_skew_value1")
            shinyjs::show(id = "sigma_skew_value2")
            shinyjs::show(id = "alpha_skew")
            shinyjs::show(id = "alpha_skew_value1")
            shinyjs::show(id = "alpha_skew_value2")
        }
    })
    
    results <- observeEvent(input$restart, {
        
        withProgress(message = 'Sampling Models', value = 0, {
            
            req(input$file)
            
            if(!is.null(input$file)){
                
                tryStan <- try({
                    
                    tryCatch(
                        {
                            df <- read.csv(input$file$datapath,
                                           header = TRUE,
                                           sep = ",")
                        })
                    
                    if(length(unique(df[,which(colnames(df) == input$column)])) == 2){
                        showNotification(ui = "The analysis is not possible for binary variables. Please specify another test variable.", 
                                         duration = NULL,type = "error",closeButton = TRUE)   
                        return()
                    }
                    
                    if(is.factor(df[,which(colnames(df) == input$column)])){
                        showNotification(ui = "The analysis is not possible for character factors. Please specify another test variable.", 
                                         duration = NULL,type = "error",closeButton = TRUE)   
                        return()
                    }
                    
                    if(length(df[,which(colnames(df) == input$column)]) < 8 | length(df[,which(colnames(df) == input$column)]) > 46340){
                        showNotification(ui = "The analysis must be performed on at least 8 observations and at most 46340 observations.", 
                                         duration = NULL,type = "error",closeButton = TRUE)   
                        return()
                    }
                    
                    df[,which(colnames(df) == input$column)] <- as.numeric(df[,which(colnames(df) == input$column)])
                    
                    testresult1 <- shapiro.test(df[,which(colnames(df) == input$column)])
                    testresult2 <- agostino.test(df[,which(colnames(df) == input$column)])
                    
                    if(input$mu_normal == "Normal"){
                        choice1 <- 1
                    } else if (input$mu_normal == "Cauchy"){
                        choice1 <- 2
                    } else if (input$mu_normal == "Gamma"){
                        choice1 <- 3
                    }
                    
                    if(input$sigma_normal == "Normal"){
                        choice2 <- 1
                    } else if (input$sigma_normal == "Cauchy"){
                        choice2 <- 2
                    } else if (input$sigma_normal == "Gamma"){
                        choice2 <- 3
                    }
                    
                    incProgress(1/4, detail = "Normal Model")
                    
                    stanfitH0 <- rstan::sampling(stanNormal, data = list(X = df[,which(colnames(df) == input$column)],
                                                                         n = length(df[,which(colnames(df) == input$column)]),
                                                                         mu_normal_value1 = input$mu_normal_value1,
                                                                         mu_normal_value2 = input$mu_normal_value2,
                                                                         sigma_normal_value1 = input$sigma_normal_value1,
                                                                         sigma_normal_value2 = input$sigma_normal_value2,
                                                                         choice1 = choice1,
                                                                         choice2 = choice2),
                                                 iter = 5e3, warmup = 1000, chains = 1, cores = 1)
                    fits$stanfitH0 <- stanfitH0
                    
                    if(input$lambda_mixture == "Normal"){
                        choice1 <- 1
                    } else if (input$lambda_mixture == "Cauchy"){
                        choice1 <- 2
                    } else if (input$lambda_mixture == "Gamma"){
                        choice1 <- 3
                    }
                    
                    if(input$sigma_mixture == "Normal"){
                        choice2 <- 1
                    } else if (input$sigma_mixture == "Cauchy"){
                        choice2 <- 2
                    } else if (input$sigma_mixture == "Gamma"){
                        choice2 <- 3
                    }
                    
                    if(input$theta_mixture == "Normal"){
                        choice3 <- 1
                    } else if (input$theta_mixture == "Cauchy"){
                        choice3 <- 2
                    } else if (input$theta_mixture == "Gamma"){
                        choice3 <- 3
                    }
                    
                    incProgress(1/4, detail = "Mixture-normal Model")
                    
                    stanfitH1 <- rstan::sampling(stanMixture, data = list(X = df[,which(colnames(df) == input$column)],
                                                                          n = length(df[,which(colnames(df) == input$column)]),
                                                                          lambda_mixture_value1 = input$lambda_mixture_value1,
                                                                          lambda_mixture_value2 = input$lambda_mixture_value2,
                                                                          sigma_mixture_value1 = input$sigma_mixture_value1,
                                                                          sigma_mixture_value2 = input$sigma_mixture_value2,
                                                                          theta_mixture_value1 = input$theta_mixture_value1,
                                                                          theta_mixture_value2 = input$theta_mixture_value2,
                                                                          choice1 = choice1,
                                                                          choice2 = choice2,
                                                                          choice3 = choice3),
                                                 iter = 5e3, warmup = 1000, chains = 1, cores = 1)
                    fits$stanfitH1 <- stanfitH1
                    
                    if(input$mu_skew == "Normal"){
                        choice1 <- 1
                    } else if (input$mu_skew == "Cauchy"){
                        choice1 <- 2
                    } else if (input$mu_skew == "Gamma"){
                        choice1 <- 3
                    }
                    
                    if(input$sigma_skew == "Normal"){
                        choice2 <- 1
                    } else if (input$sigma_skew == "Cauchy"){
                        choice2 <- 2
                    } else if (input$sigma_skew == "Gamma"){
                        choice2 <- 3
                    }
                    
                    if(input$alpha_skew == "Normal"){
                        choice3 <- 1
                    } else if (input$alpha_skew == "Cauchy"){
                        choice3 <- 2
                    } else if (input$alpha_skew == "Gamma"){
                        choice3 <- 3
                    }
                    
                    incProgress(1/4, detail = "Skew-normal Model")
                    
                    stanfitH2 <- rstan::sampling(stanSkew, data = list(X = df[,which(colnames(df) == input$column)],
                                                                       n = length(df[,which(colnames(df) == input$column)]),
                                                                       mu_skew_value1 = input$mu_skew_value1,
                                                                       mu_skew_value2 = input$mu_skew_value2,
                                                                       sigma_skew_value1 = input$sigma_skew_value1,
                                                                       sigma_skew_value2 = input$sigma_skew_value2,
                                                                       alpha_skew_value1 = input$alpha_skew_value1,
                                                                       alpha_skew_value2 = input$alpha_skew_value2,
                                                                       choice1 = choice1,
                                                                       choice2 = choice2,
                                                                       choice3 = choice3),
                                                 iter = 5e3, warmup = 1000, chains = 1, cores = 1)
                    fits$stanfitH2 <- stanfitH2
                    
                })
                
                if(class(tryStan) == "try-error"){
                    stop("The bridge between Shiny and Stan could not be made.")
                }
                
                incProgress(1/4, detail = "Bridge sampling")
                
                H0.bridge <- bridge_sampler(stanfitH0, silent = TRUE)
                H1.bridge <- bridge_sampler(stanfitH1, silent = TRUE)
                H2.bridge <- bridge_sampler(stanfitH2, silent = TRUE)
                
                fits$H0.bridge <- H0.bridge
                fits$H1.bridge <- H1.bridge
                fits$H2.bridge <- H2.bridge
                
                BF01 <- bf(H0.bridge, H1.bridge,log = FALSE)$bf
                BF02 <- bf(H0.bridge, H2.bridge,log = FALSE)$bf
                BF12 <- bf(H1.bridge, H2.bridge,log = FALSE)$bf
                
                logBF01 <- bf(H0.bridge, H1.bridge,log = TRUE)$bf
                logBF02 <- bf(H0.bridge, H2.bridge,log = TRUE)$bf
                
                d <- data.frame("p.value" = c(testresult1$p.value, testresult2$p.value),
                                "BF01" = c(round(BF01,2),round(BF02,2)),
                                "log.BF01" = c(logBF01, logBF02))
                rownames(d) <- c("H0: Normality vs. H1: Non-normality", 
                                 "H0: Normality vs. H2: Skewness")
                fits$table <- d
                
                BFmat <- data.frame("H0" = c("-", "", ""),
                                    "H1" = c(round(BF01,2), "-", ""), 
                                    "H2" = c(round(BF02,2), round(BF12,2), "-"))
                rownames(BFmat) <- colnames(BFmat)
                
                fits$BFmat <- BFmat
                
                if(!is.null(fits$stanfitH0)){
                    e0 <- rstan::extract(fits$stanfitH0)
                    e1 <- rstan::extract(fits$stanfitH1)      
                    e2 <- rstan::extract(fits$stanfitH2)
                }
                
                estim <- data.frame("Estimate" = c(
                    "",
                    round(median(e0$mu),3),
                    round(median(e0$sigma),3),
                    "",
                    round(median(e1$theta),3),
                    round(median(e1$lambda1),3),
                    round(median(e1$lambda2),3),
                    round(median(e1$sigma),3),
                    "",
                    round(median(e2$xi),3),
                    round(median(e2$omega),3),
                    round(median(e2$alpha),3)
                ))
                rownames(estim) <- c("Normal model:", "Mu", "Sigma", 
                                     "Mixture-normal model:", "Theta", "Lambda.1", "Lambda.2", "Sigma,",
                                     "Skew-normal model:", "Mu.", "Sigma.", "Alpha")
                
                fits$estimates <- estim
                
                fits$keeptrack <- input$column
                
            }
            
        })
        
    })
    
    obsB <- observeEvent(input$column, {
        if(input$column != fits$keeptrack){
            MakeEmptyTables()
        } else {
            RestoreTables()
        }
    })
    
    output$result <- renderTable(fits$table, 
                                 rownames = TRUE,
                                 caption = "Analysis results",
                                 caption.placement = getOption("xtable.caption.placement", "top"), 
                                 caption.width = getOption("xtable.caption.width", NULL))
    
    output$secondtable <- renderTable(fits$BFmat, 
                                      rownames = TRUE, 
                                      caption = "Bayes factor matrix",
                                      caption.placement = getOption("xtable.caption.placement", "top"), 
                                      caption.width = getOption("xtable.caption.width", NULL))
    
    output$estimates <- renderTable({if(input$paramest) return(fits$estimates)},
                                    rownames = TRUE,
                                    caption = "Parameter estimates",
                                    caption.placement = getOption("xtable.caption.placement", "top"),
                                    caption.width = getOption("xtable.caption.width", NULL))
    
    output$plot <- renderPlot({
        
        req(input$file)
        
        tryCatch(
            {
                df <- read.csv(input$file$datapath,
                               header = TRUE,
                               sep = ",")
            })
        
        df[,which(colnames(df) == input$column)] <- as.numeric(df[,which(colnames(df) == input$column)])
        
        h <- hist(df[,which(colnames(df) == input$column)], breaks = input$bins, plot = FALSE)
        hist(df[,which(colnames(df) == input$column)], main = "", probability = TRUE, las = 1,
             xlab = "", cex.lab = 1.2, col = "#337ab7", xlim = c(h$breaks[1], h$breaks[length(h$breaks)]),
             xaxt = "n", yaxt = "n", breaks = input$bins, ylim = c(0, max(h$density) + (max(h$density) * 0.2)))
        axis(1, at = round(h$breaks,1),lwd = 3, cex.axis = 1.2, labels = round(h$breaks, 2))
        axis(2, at = round(seq(0, max(h$density), length.out = 5),2),lwd = 3, cex.axis = 1.2, las = 1)
        
        if(!is.null(fits$stanfitH0) && input$column == fits$keeptrack){
            e0 <- rstan::extract(fits$stanfitH0)
            e1 <- rstan::extract(fits$stanfitH1)      
            e2 <- rstan::extract(fits$stanfitH2)
            curve(dnorm(x, mean = median(e0$mu), sd = median(e0$sigma)), 
                  from = h$breaks[1], to = h$breaks[length(h$breaks)], add = TRUE, lwd = 5, col = "black", lty = 1)
            curve(CosNormSample(x, lambda1 = median(e1$lambda1), theta = median(e1$theta), 
                                sigma = median(e1$sigma), lambda2 = median(e1$lambda2)), 
                  from = h$breaks[1], to = h$breaks[length(h$breaks)], add = TRUE, lwd = 5, col = "red", lty = 2)
            curve(dsn(x, xi = median(e2$xi), omega = median(e2$omega), alpha = median(e2$alpha)),
                  from = h$breaks[1], to = h$breaks[length(h$breaks)], add = TRUE, lwd = 5, col = "green", lty = 3)
            legend("topright", legend = c("Normal", "Mixture-normal", "Skew-normal"), bty = "n",
                   lty = c(1,1,1), col = c("black", "red", "green"), lwd = 5)
        }
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)

