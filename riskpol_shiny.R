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
                        value = 5)),
    
    # column(4,
    #        radioButtons("masters",
    #                     "Require Masters Qualification (MTurk only)",
    #                     choices = c("Yes", "No"),
    #                     selected = character(0))),
    
    column(4,
           numericInput("n_premium",
                        "Number of premium qualifications (MTurk):",
                        min = 0,
                        value = 0,
                        step = .5))
    
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
  
  tableOutput("cost_table"),
  
  withTags({
    div("Notes:",
    ul(
        li("Service fee MTurk (", 
           a(href = "https://requester.mturk.com/pricing", "Click here", target="_blank"), 
           "for pricing details): 20%"),
        br(),
        li("Service fee MTurk for large (>9) assignments/participants: 40%"),
        br(),
        li("Additional service fee for MTurk Masters Qualifications (higher quality participants: 5%)"),
        br(),
        li("Premium qualifications (MTurk only): median additional fee of $0.40 per assignment"),
        br(),
        li("Service fee Prolific (",
           a(href = "https://www.prolific.co/pricing", "Click here", target="_blank"),
           "for pricing details): 33%")
        ))

    
  }),
  


) # closing bracket for fluidPage


# Define server logic for slider examples ----
server <- function(input, output) {
  
  # Reactive expression to create data frame of all input values ----
  numericValues <- reactive({
    
      # Participant payment
      part_pay <- sum(
      input$n_t1*(input$t_t1/60*input$rate),
      input$n_t2*(input$t_t2/60*input$rate),
      input$n_t3*(input$t_t3/60*input$rate)
    )
      
      n_tot <- sum(input$n_t1, input$n_t2, input$n_t3)
      t_tot <- sum(input$t_t1, input$t_t2, input$t_t3)
      

      # Platform payment/service fee
      
      # Master Qualification
      # observeEvent(input$masters, {
      #   if_else(input$masters == "Yes",
      #           mturk_fee <- (part_pay + input$bonus) * .2 + part_pay * .05,
      #           mturk_fee <- (part_pay + input$bonus) * 100)
      #         }
      #                             )
      
      # mqual <- input$masters
      # 
      # if_else(mqual != "No",
      #         mturk_fee <- (part_pay + input$bonus) * .2 + part_pay * .05,
      #         mturk_fee <- (part_pay + input$bonus) * 100
      #         )
      # 
      # Amazon
      
      premium_fee <- .4
      
      mturk_fee <- (part_pay + input$bonus) * .2 + input$n_premium * premium_fee *  n_tot
      
      # Amazon + Masters qual
      masters_fee <- (part_pay + input$bonus) * .2 + part_pay * .05 + input$n_premium * premium_fee *  n_tot
      
      # Prolific
      prolific_fee <- (part_pay + input$bonus) * .33
      

      # Total pay
      tot_pay_mturk <- part_pay + mturk_fee
      tot_pay_masters <- part_pay + masters_fee
      tot_pay_prolific <- part_pay + prolific_fee
    

    # Prolific cost breakdown
    data.frame(
      Component = c("Participant payment",
                    "Service fee",
                    "Total"),
      Cost.MTurk = as.character(c(part_pay,
                                  mturk_fee,
                                  tot_pay_mturk)),
      Cost.MTurk.Masters = as.character(c(part_pay,
                                          masters_fee,
                                          tot_pay_masters)),
      Cost.Prolific = as.character(c(part_pay,
                                     prolific_fee,
                                     tot_pay_prolific)),
      stringsAsFactors = FALSE)
    
  })
  
  # Show the values in an HTML table ----
  output$cost_table <- renderTable({
    numericValues()
  })
  
}

# Create Shiny app ----
shinyApp(ui, server)











