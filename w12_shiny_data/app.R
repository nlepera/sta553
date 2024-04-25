#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(shinyjs)

### Read in penguin data
penguin <- read.csv("https://nlepera.github.io/sta553/w12_shiny_data/Data/w03-penguins.csv")
penguin$body_mass_kg <- (penguin$body_mass_g / 1000)
penguin$year <- as.Date(penguin$year)
x.names = names(penguin)[-c(1:3,7,8)]
y.names =names(penguin)[-c(1:3,7,8)]

# Define UI for application 
ui <- fluidPage(
  shinyjs::useShinyjs(),
  
  withMathJax(),
  ### Important: The following tag allows inline equations in MathJax
  tags$div(HTML("<script type='text/x-mathjax-config' >
            MathJax.Hub.Config({
            tex2jax: {inlineMath: [['$','$'], ['\\(','\\)']]}
            });
            </script >
            ")),

  #well panel
    wellPanel(style = "background-color:#002252; margin: 2px 1px 2px 1px",
  # Application title
    titlePanel(h1("Waddling Weights: An Analysis of Penguin Species", align = "center", style = "color: white; font-variant: small-caps; font-weight: bold"))),

    # Sidebar with various inputs
    sidebarLayout(
        sidebarPanel( style = "background-color:#5CA0FF; height: 1000px",
            
            p(h4("Instructions:", style = "color: white; font-weight: bold")),
            p(h5(em("Utilize the following fields to select response and predictor variables", style = "font-weight: bold"))),
            br(),
       
            
            #input1: species (penguin species)
            p(radioButtons(inputId = "species",
                          label = tags$label("Penguin Species", style = "color:white"),
                          choices = c("Adelie",
                                      "Gentoo",
                                      "Chinstrap",
                                      "All"),
                          selected = "All",
                        )),
            #input1.b: island (penguin island)
            p(radioButtons(inputId = "island",
                           label = tags$label("Island", style = "color:white"),
                           choices = c("Torgersen",
                                       "Biscoe",
                                       "Dream",
                                       "All"),
                           selected = "All",
            )),
            #input1.c: sex (penguin sex)
            p(radioButtons(inputId = "sex",
                           label = tags$label("Penguin Sex", style = "color:white"),
                           choices = c("Male",
                                       "Female",
                                       "All"),
                           selected = "All",
            )),
            br(),
            #Input2: x (Predictor variable)          
            p(selectInput(inputId = "x",
                          label = tags$label("Select Predictor Variable (x)", style = "color:white"),
                          choices = x.names,
                          selected = x.names[1])),
          
         
            #Input3: y (Response variable)          
            p(selectInput(inputId = "y",
                          label = tags$label("Select Response Variable (y)",  style = "color:white"),
                          choices = y.names,
                          selected = y.names[2])),
         
            br(),
            #Input4: xi (New X for predictions tab)
            sliderInput(inputId = "xi",
                        label = tags$label("Select new x value for Prediction (xi)",  style = "color:white"),
                        min = 0,
                        max = 250,
                        step = 1,
                        value = 10),
       
            br(),
            HTML('<p><center><img src="https://nlepera.github.io/sta553/w12_shiny_data/penguin_cute.png"  
                           width="140" height="100"></center></p>'),
            ),
        

        # Show a plot of the generated distribution
        mainPanel( 
          column(12, style = "background-color:#CCE0FF; height: 1000px; align: center",
                 tabsetPanel(type = "tabs",
                             tabPanel(h5("Scatter Plot", style ="color:#002252; font-weight: bold"), plotOutput("plot", height = "900px", width = "1000px")),
                             tabPanel(h5("Regression Coefficients", style ="color:#002252; font-weight: bold"), tableOutput("table")),
                             tabPanel(h5("Diagnostics", style ="color:#002252; font-weight: bold"), plotOutput("diagnosis", height = "900px", width = "1000px")),
                             tabPanel(h5("Prediction", style ="color:#002252; font-weight: bold"), plotOutput("predPlt", height = "900px", width = "1000px"))),
        )
    ))
)

# Define server logic required to create outputs for each tab
server <- function(input, output) {

  #### Subsetting data based on Species
  workDat = function(){
    if (input$species == "Adelie") {
      workingData = penguin[which(penguin$species == "Adelie"),]
    } else if (input$species == "Gentoo") {
      workingData = penguin[which(penguin$species == "Gentoo"),]
    } else if (input$species == "Chinstrap") {
      workingData = penguin[which(penguin$species == "Chinstrap"),]
    } else {
      workingData = penguin
    }
    workingData 
  }
  
  
  #### Subsetting data based on island
  workDat = function(){
    if (input$species == "Torgersen") {
      workingData = penguin[which(penguin$species == "Torgersen"),]
    } else if (input$species == "Biscoe") {
      workingData = penguin[which(penguin$species == "Biscoe"),]
    } else if (input$species == "Dream") {
      workingData = penguin[which(penguin$species == "Dream"),]
    } else {
      workingData = penguin
    }
    workingData 
  }
  
  
  #### Subsetting data based on Species
  workDat = function(){
    if (input$species == "Adelie") {
      workingData = penguin[which(penguin$species == "Adelie"),]
    } else if (input$species == "Gentoo") {
      workingData = penguin[which(penguin$species == "Gentoo"),]
    } else if (input$species == "Chinstrap") {
      workingData = penguin[which(penguin$species == "Chinstrap"),]
    } else {
      workingData = penguin
    }
    workingData 
  }
  
  
  #### Subsetting data based on Sex
  workDat = function(){
    if (input$species == "Male") {
      workingData = penguin[which(penguin$species == "male"),]
    } else if (input$species == "Female") {
      workingData = penguin[which(penguin$species == "female"),]
    } else {
      workingData = penguin
    }
    workingData 
  }
  
  
  #######   Scatter Plots
  ######################################
  output$plot <- renderPlot({
    par(bg="#CCE0FF")
    dataset = workDat()[,-c(1:3,7,8)]   # define the working data set
    #####
    plot(dataset[,input$x], dataset[,input$y], 
         xlab = input$x,
         ylab = input$y,
         main = paste("Relationship between", input$y, "and", input$x)
    )
    ## adding a regression line to the plot
    abline(lm(dataset[,input$y] ~ dataset[,input$x]),
           col = "blue",
           lwd = 2)
  })
  
  #######   Regression Table
  ######################################
  output$table <- renderTable({
    br()
    br()
    dataset = workDat()[,-c(1:3,7,8)]
    # define the working data set
    m0 = lm(dataset[,input$y] ~ dataset[,input$x])
    #summary(m0)
    regcoef = data.frame(coef(summary(m0)))
    
    ##
    regcoef$Pvalue = regcoef[,names(regcoef)[4]]
    ###
    regcoef$Variable = c("Intercept", input$x)
    regcoef[,c(6, 1:3, 5)]
  
})
  #######   Diagnostics
  ######################################
  output$diagnosis <- renderPlot({
    par(bg="#CCE0FF")
    dataset = workDat()[,-c(1:3,7,8)]   # define the working data set
    m1=lm(dataset[,input$y] ~ dataset[,input$x])
    par(mfrow=c(2,2))
    plot(m1)
  })   
  
  #######   Predictions
  ######################################
  output$predPlt <- renderPlot({
    par(bg="#CCE0FF")
    dataset = workDat()[,-c(1:3,7,8)]   # define the working data set
    
    ###
    m3 = lm(dataset[,input$y] ~ dataset[,input$x])
    
    predict.y = coef(m3)[1] + coef(m3)[2]*input$xi
    #####
    plot(dataset[,input$x], dataset[,input$y], 
         xlab = input$x,
         ylab = input$y,
         main = paste("Relationship between", input$y, "and", input$x)
    )
    ## adding a regression line to the plot
    abline(m3,
           col = "#4C1273",
           lwd = 1,
           lty=2)
    points(input$xi, predict.y, pch = 25, col = "#4C1273", bg = "#4C1273", cex = 2)
  })
}


# Run the application 
shinyApp(ui = ui, server = server)
