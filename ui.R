library(shiny)
library(shinycssloaders)

fluidPage(
  titlePanel("D Optimal Design by Terry Xu, Giselle Qiu, Shubham Patil"),
  sidebarPanel(
    # Additional inputs for the rutgers.optimal function
    hr(),  # Horizontal line for visual separation
    h4("D Optimal Design"),
    numericInput("n_fact", "Number of Factors", value = 2),
    numericInput("n_cat", "Number of Categories", value = 3),
    textInput("in_frml", "Formula", value = '~(a+b+c)^2'),
    numericInput("blocks", "Number of Blocks", value = 5),
    numericInput("size", "Size Per Block", value = 8),
    numericInput("r_starts", "Number of Random Starts", value = 10),
    numericInput("iter", "Number of Iterations", value = 2000),
    checkboxInput("random_blocks", "Include Random Blocks", value = TRUE),
    actionButton("calc", "Calculate Optimal Design"),
    div(style = "height: 20px;"), 
    h4("Post-calculation Experiment Controls"),
    textInput("resp", "Response Formula + Interaction", value='.2*X$a+.5*X$b+.1*X$a*X$b'), # a, b plus interaction
    textInput("catf", "Cat Formula", value='(-.2,.5,1.1)'), # c
    #textInput("ablock", "Actual Blocks", value='5,0,1'),
    numericInput("rblock", "Random Block Effect", value='40'), # add random block effect
    actionButton("exp", "Run Experiment"),
    h6("(Based on Last Calculated Optimal Design)")
    
  ),
  mainPanel(
    tabsetPanel(
      tabPanel("Design Evaluation", 
               tableOutput("rutgers_result"),
               h4("D Efficiency"),
               withSpinner(verbatimTextOutput("deff_output"), size = 0.4, proxy.height = "50px"),
               h5("A Efficiency"),
               withSpinner(verbatimTextOutput("aeff_output"), size = 0.4, proxy.height = "50px"),
               h5("G Efficiency"),
               withSpinner(verbatimTextOutput("geff_output"), size = 0.4, proxy.height = "50px"),
               h4("Power Analysis"),
               withSpinner(tableOutput("power_output"), size = 0.4, proxy.height = "50px"),
               
      ),
      tabPanel("Design Matrix",
               withSpinner(tableOutput("xbase_output"), size = 0.4, proxy.height = "50px"),
      ),
      tabPanel("Annova",
               h4("Fixed"),
               withSpinner(tableOutput("anovaFixedOutput"), size = 0.4, proxy.height = "50px"),
               h4("Random"),
               withSpinner(tableOutput("anovaRandomOutput"), size = 0.4, proxy.height = "50px")
      ),
      tabPanel("Blocks Plot",
               withSpinner(plotOutput("plot"), size = 0.4, proxy.height = "50px")
      )
    ),
    downloadButton("download_excel", "Download Design Info"),
    downloadButton("download_excel2", "Download Tests"),
    h4("Time Lapsed"),
    withSpinner(textOutput("time_lapsed"), size = 0.2, proxy.height = "25px"),
  )
)
