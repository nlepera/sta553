#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
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

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel( style = "background-color:#aac1b3; height: 700px",
                      br(),
                      br(),
            p(h4("Instructions:", style = "color: white; font-weight: bold")),
            p(h5(em("Select the sample size and population mean to generate the poisson distribution", style = "font-weight: bold"))),
            br(),
            br(),
            
            #input1: k (number of occurances)
            p(numericInput(inputId = "k",
                          label = "Number of Occurances/Sample Size (k)",
                          min = 0,
                          max = 100,
                          value = 15,
                        )),
            br(),
            br(),
            
            #Input2: Lambda (known population mean)          
            p(sliderInput(inputId = "lambda",
                          label = "Poisson Population Mean Number of Occurances per Given Unit (λ)",
                          min = 0,
                          max = 100,
                          value = 45)),
          
            ),
        

        # Show a plot of the generated distribution
        mainPanel( 
          column(12, style = "background-color:#90af9c; height: 700px; text-align: center",
           plotOutput(outputId = "poisPlot", height = "100%")
        )
    ))
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$poisPlot <- renderPlot({
      pois.input <- rpois(input$k, input$lambda)
      density.pois <- density(pois.input)
      par(bg = "#90af9c")
      hist(pois.input, probability = TRUE, col = '#dde6bb', border = 'white',
           height = "500px",
             xlab = "Number of Occurances (k) [Sample Size]",
           ylab = 'Probability of Seeing K Number of Occurances',
           main = 'Simulated Poisson Distribution')
      lines(density.pois, col="blue", lwd=1, lty=2)})
  
}

# Run the application 
shinyApp(ui = ui, server = server)
