library(shiny)
library(tidyverse)

ui <- fluidPage(
  
  titlePanel("Calculate study cost"),
  
  # Choice of recruitment platform
  # radioButtons("platform", 
  #                    "Choose the recruitment platform",
  #                    choiceNames = c("Amazon MTurk", "Prolific"),
  #                    choiceValues = c("mturk", "prolific"),
  #                    inline = T,
  #                    selected = character(0)),
  
  # checkboxInput("platform2", 
  #                    "Choose the recruitment platform",
  #                    choiceNames = c("Amazon MTurk", "Prolific"),
  #                    choiceValues = c("mturk", "prolific"),
  #                    inline = T,
  #                    selected = "mturk"),
  
  fluidRow(
    column(4,
           numericInput("n_t1",
                        "Number of participants T1:",
                        min = 0,
                        value = 100)),
    
    column(4,
           numericInput("t_t1",
                        "Length of study T1 [minutes]:",
                        min = 0,
                        value = 5))
    
  ),
  
  fluidRow(
    column(4,
           numericInput("n_t2",
                        "Number of participants T2:",
                        min = 0,
                        value = 100)),
    
    column(4,
           numericInput("t_t2",
                        "Length of study T2 [minutes]:",
                        min = 0,
                        value = 5))
    
  ),
  
  fluidRow(
    column(4,
           numericInput("n_t3",
                        "Number of participants T3:",
                        min = 0,
                        value = 100)),
    
    column(4,
           numericInput("t_t3",
                        "Length of study T3 [minutes]:",
                        min = 0,
                        value = 5))
    
  ),
  
  fluidRow(
    column(4,
           # Hourly rate paid to participants
           sliderInput("rate",
                       "Hourly rate [$]",
                       min = 0,
                       max = 20,
                       value = 7,
                       step = .1)),
    
    column(4,
           numericInput("bonus",
                        "Bonus payment [$]:",
                        min = 0,
                        value = 1))
    
  ),

  
  # Output: Table summarizing the values entered ----
  
  h4("Cost breakdown"),
  
  tableOutput("values")
  
) # closing bracket for fluidPage


# Define server logic for slider examples ----
server <- function(input, output, session) {
  
  # Reactive expression to create data frame of all input values ----
  numericValues <- reactive({
    
      # Participant payment
      part_pay <- sum(
      input$n_t1*(input$t_t1/60*input$rate),
      input$n_t2*(input$t_t2/60*input$rate),
      input$n_t3*(input$t_t3/60*input$rate)
    )
      
      # Platform payment/service fee
      
      # Amazon
      mturk_fee <- (part_pay + input$bonus) * .2
      
      # Prolific
      prolific_fee <- (part_pay + input$bonus) * .33
      
      # # Amazon bonus fee
      # mturk_bonus <- part_pay * .4
      
      # Amazon vs. Prolific
      # if_else(
      #   input$platform == "prolific",
      #   plat_pay <- part_pay * .5,
      #   plat_pay <- part_pay * 1
      # )
      # 
      # if(input$platform == "mturk"){
      #   plat_pay <- part_pay * .2
      # } else if(input$platform == "prolific") {
      #   plat_pay <- part_pay * .33
      # }
      
      
      # Total pay
      tot_pay_mturk <- part_pay + mturk_fee
      tot_pay_prolific <- part_pay + prolific_fee
    
      
      # MTurk cost breakdown
    # data.frame(
    #   Component = c("Participant payment",
    #                 "Service fee MTurk",
    #                 "Total"),
    #   Cost = as.character(c(part_pay,
    #                         mturk_fee,
    #                         tot_pay_mturk)),
    #   stringsAsFactors = FALSE)
    
    # Prolific cost breakdown
    data.frame(
      Component = c("Participant payment",
                    "Service fee",
                    "Total"),
      Cost.MTurk = as.character(c(part_pay,
                                  mturk_fee,
                                  tot_pay_mturk)),
      Cost.Prolific = as.character(c(part_pay,
                                     prolific_fee,
                                     tot_pay_prolific)),
      stringsAsFactors = FALSE)
    
  })
  
  # Show the values in an HTML table ----
  output$values <- renderTable({
    numericValues()
  })
  
}

# Create Shiny app ----
shinyApp(ui, server)











