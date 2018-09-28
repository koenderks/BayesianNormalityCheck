library(shiny)
library(shinydashboard)
library(shinyjs)
library(diptest)
library(moments)

# Define UI for application that draws a histogram
ui <- fluidPage(
    
    title = "Bayesian Normality Check",
    hr(),
    useShinyjs(),
    fluidRow(
        column(3,
               h3("A Bayesian Assumption Check for Normality"),
               a("Click here to read the article", href="https://www.google.nl",target="_blank"),
               p(),
               p(),
               fileInput("file", "Upload CSV File",
                         multiple = FALSE,
                         accept = c("text/csv",
                                    "text/comma-separated-values,text/plain",
                                    ".csv")),
               selectInput('column', 'Target Column', choices = list("Upload data first...")),
               actionButton(inputId = "button", label = "Prior Options")
        ),
        column(3,
               h3("Priors for the Normal Distribution", id = "title.normal"),
               fluidRow(
                   column(3,
                          selectInput('mu_normal', "Mu", list("Normal", "Cauchy", "Gamma"),selected = "Normal",width = "100%")
                   ),
                   column(3,
                          numericInput("mu_normal.value1", "value", 0, min = -1000, max = 1000, width = "150%")
                   ),
                   column(3,
                          numericInput("mu_normal.value2", "value", 1, min = -1000, max = 1000, width = "100%")
                   )
               ),
               fluidRow(
                   column(3,
                          selectInput('sigma_normal', "Sigma", list("Normal", "Cauchy", "Gamma"),selected = "Gamma",width = "100%")
                   ),
                   column(3,
                          numericInput("sigma_normal.value1", "value", 2, min = -1000, max = 1000, step = 0.5, width = "100%")
                   ),
                   column(3,
                          numericInput("sigma_normal.value2", "value", 1, min = -1000, max = 1000, step = 0.5, width = "100%")
                   )
               )
        ),
        column(3,
               h3("Priors for the Mixture-normal Distribution", id = "title.mixture"),
               fluidRow(
                   column(3,
                          selectInput('theta_mixture', "Theta", list("Normal", "Cauchy", "Gamma"),selected = "Normal",width = "100%")
                   ),
                   column(3,
                          numericInput("theta_mixture.value1", "value", 0.5, min = -1000, max = 1000, width = "100%")
                   ),
                   column(3,
                          numericInput("theta_mixture.value2", "value", .1, min = -1000, max = 1000, width = "100%")
                   )
               ),
               fluidRow(
                   column(3,
                          selectInput('lambda_mixture', "Lambda1", list("Normal", "Cauchy", "Gamma"),selected = "Normal",width = "100%")
                   ),
                   column(3,
                          numericInput("lambda_mixture.value1", "value", 0, min = -1000, max = 1000, width = "100%")
                   ),
                   column(3,
                          numericInput("lambda_mixture.value2", "value", 1, min = -1000, max = 1000, width = "100%")
                   )
               ),
               fluidRow(
                   column(3,
                          selectInput('sigma_mixture', "Sigma", list("Normal", "Cauchy", "Gamma"),selected = "Gamma",width = "100%")
                   ),
                   column(3,
                          numericInput("sigma_mixture.value1", "value", 2, min = -1000, max = 1000, width = "100%")
                   ),
                   column(3,
                          numericInput("sigma_mixture.value2", "value", 1, min = -1000, max = 1000, width = "100%")
                   )
               )
        ),
        column(3,
               h3("Priors for the Skew-normal Distribution", id = "title.skew"),
               fluidRow(
                   column(3,
                          selectInput('mu_skew', "Mu", list("Normal", "Cauchy", "Gamma"),selected = "Normal",width = "100%")
                   ),
                   column(3,
                          numericInput("mu_skew.value1", "value", 0, min = -1000, max = 1000, width = "100%")
                   ),
                   column(3,
                          numericInput("mu_skew.value2", "value", 1, min = -1000, max = 1000, width = "100%")
                   )
               ),
               fluidRow(
                   column(3,
                          selectInput('sigma_skew', "Sigma", list("Normal", "Cauchy", "Gamma"),selected = "Gamma",width = "100%")
                   ),
                   column(3,
                          numericInput("sigma_skew.value1", "value", 2, min = -1000, max = 1000, width = "100%")
                   ),
                   column(3,
                          numericInput("sigma_skew.value2", "value", 1, min = -1000, max = 1000, width = "100%")
                   )
               ),
               fluidRow(
                   column(3,
                          selectInput('alpha_skew', "Alpha", list("Normal", "Cauchy", "Gamma"),selected = "Cauchy",width = "100%")
                   ),
                   column(3,
                          numericInput("alpha_skew.value1", "value", 0, min = -1000, max = 1000, width = "100%")
                   ),
                   column(3,
                          numericInput("alpha_skew.value2", "value", .2, min = -1000, max = 1000, width = "100%")
                   )
               )
        )
    ),
    
    hr(),
    
    fluidRow(
        column(4,
               tableOutput("result")
        ),
        column(5,
               plotOutput("plot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    
    shinyjs::hide(id = "title.normal")
    shinyjs::hide(id = "mu_normal")
    shinyjs::hide(id = "mu_normal.value1")
    shinyjs::hide(id = "mu_normal.value2")
    shinyjs::hide(id = "sigma_normal")
    shinyjs::hide(id = "sigma_normal.value1")
    shinyjs::hide(id = "sigma_normal.value2")
    shinyjs::hide(id = "title.mixture")
    shinyjs::hide(id = "theta_mixture")
    shinyjs::hide(id = "theta_mixture.value1")
    shinyjs::hide(id = "theta_mixture.value2")
    shinyjs::hide(id = "lambda_mixture")
    shinyjs::hide(id = "lambda_mixture.value1")
    shinyjs::hide(id = "lambda_mixture.value2")
    shinyjs::hide(id = "sigma_mixture")
    shinyjs::hide(id = "sigma_mixture.value1")
    shinyjs::hide(id = "sigma_mixture.value2")
    shinyjs::hide(id = "title.skew")
    shinyjs::hide(id = "mu_skew")
    shinyjs::hide(id = "mu_skew.value1")
    shinyjs::hide(id = "mu_skew.value2")
    shinyjs::hide(id = "sigma_skew")
    shinyjs::hide(id = "sigma_skew.value1")
    shinyjs::hide(id = "sigma_skew.value2")
    shinyjs::hide(id = "alpha_skew")
    shinyjs::hide(id = "alpha_skew.value1")
    shinyjs::hide(id = "alpha_skew.value2")
    
    contentsrea <- reactive({
        inFile <- input$file
        if (is.null(inFile))
            return(NULL)
        read.csv(inFile$datapath)
    })
    observe({
        updateSelectInput(session, "column", choices = names(contentsrea()))
    })
    
    observeEvent(input$button, {
        
        if(input$button %% 2 == 0){
            shinyjs::hide(id = "title.normal")
            shinyjs::hide(id = "mu_normal")
            shinyjs::hide(id = "mu_normal.value1")
            shinyjs::hide(id = "mu_normal.value2")
            shinyjs::hide(id = "sigma_normal")
            shinyjs::hide(id = "sigma_normal.value1")
            shinyjs::hide(id = "sigma_normal.value2")
            shinyjs::hide(id = "title.mixture")
            shinyjs::hide(id = "theta_mixture")
            shinyjs::hide(id = "theta_mixture.value1")
            shinyjs::hide(id = "theta_mixture.value2")
            shinyjs::hide(id = "lambda_mixture")
            shinyjs::hide(id = "lambda_mixture.value1")
            shinyjs::hide(id = "lambda_mixture.value2")
            shinyjs::hide(id = "sigma_mixture")
            shinyjs::hide(id = "sigma_mixture.value1")
            shinyjs::hide(id = "sigma_mixture.value2")
            shinyjs::hide(id = "title.skew")
            shinyjs::hide(id = "mu_skew")
            shinyjs::hide(id = "mu_skew.value1")
            shinyjs::hide(id = "mu_skew.value2")
            shinyjs::hide(id = "sigma_skew")
            shinyjs::hide(id = "sigma_skew.value1")
            shinyjs::hide(id = "sigma_skew.value2")
            shinyjs::hide(id = "alpha_skew")
            shinyjs::hide(id = "alpha_skew.value1")
            shinyjs::hide(id = "alpha_skew.value2")
        }else{
            shinyjs::show(id = "title.normal")
            shinyjs::show(id = "mu_normal")
            shinyjs::show(id = "mu_normal.value1")
            shinyjs::show(id = "mu_normal.value2")
            shinyjs::show(id = "sigma_normal")
            shinyjs::show(id = "sigma_normal.value1")
            shinyjs::show(id = "sigma_normal.value2")
            shinyjs::show(id = "title.mixture")
            shinyjs::show(id = "theta_mixture")
            shinyjs::show(id = "theta_mixture.value1")
            shinyjs::show(id = "theta_mixture.value2")
            shinyjs::show(id = "lambda_mixture")
            shinyjs::show(id = "lambda_mixture.value1")
            shinyjs::show(id = "lambda_mixture.value2")
            shinyjs::show(id = "sigma_mixture")
            shinyjs::show(id = "sigma_mixture.value1")
            shinyjs::show(id = "sigma_mixture.value2")
            shinyjs::show(id = "title.skew")
            shinyjs::show(id = "mu_skew")
            shinyjs::show(id = "mu_skew.value1")
            shinyjs::show(id = "mu_skew.value2")
            shinyjs::show(id = "sigma_skew")
            shinyjs::show(id = "sigma_skew.value1")
            shinyjs::show(id = "sigma_skew.value2")
            shinyjs::show(id = "alpha_skew")
            shinyjs::show(id = "alpha_skew.value1")
            shinyjs::show(id = "alpha_skew.value2")
        }
    })
    
    output$result <- renderTable({
        
        req(input$file)
        
        if(!is.null(input$file)){
        
        tryCatch(
            {
                df <- read.csv(input$file$datapath,
                               header = TRUE,
                               sep = ",")
            })
            
            testresult1 <- shapiro.test(df[,which(colnames(df) == input$column)])
            testresult2 <- dip.test(df[,which(colnames(df) == input$column)])
            testresult3 <- agostino.test(df[,which(colnames(df) == input$column)])
            
            d <- data.frame("p.value" = c(testresult1$p.value, testresult2$p.value,testresult3$p.value),
                            "BayesFactor" = rep(NA, 3))
            rownames(d) <- c("Normality vs. Non-normality", 
                             "Normality vs. Bi-modality", 
                             "Normality vs. Skewness")
            
            return(d)
            
        } else {
            
            d <- data.frame("p.value" = rep(NA, 3),
                            "BayesFactor" = rep(NA, 3))
            rownames(d) <- c("Normality vs. Non-normality", 
                             "Normality vs. Bi-modality", 
                             "Normality vs. Skewness")
            
            return(d)
            
        }
        
    }, rownames = TRUE)
    
    output$plot <- renderPlot({
        
        req(input$file)
        
        tryCatch(
            {
                df <- read.csv(input$file$datapath,
                               header = TRUE,
                               sep = ",")
            })
        
        h <- hist(df[,which(colnames(df) == input$column)], plot = FALSE)
        hist(df[,which(colnames(df) == input$column)], main = "", probability = TRUE, las = 1,
             xlab = "", cex.lab = 1.2, col = "gray80", xlim = c(h$breaks[1], h$breaks[length(h$breaks)]),
             xaxt = "n", yaxt = "n")
        axis(1, at = h$breaks,lwd = 3, cex.axis = 1.2)
        axis(2, at = round(seq(0, max(h$density), length.out = 5),2),lwd = 3, cex.axis = 1.2, las = 1)
        
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)

