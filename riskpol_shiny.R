library(shiny)
library(tidyverse)


# UI ----------------------------------------------------------------------

ui <- fluidPage(
  
  titlePanel("Study cost calculator"),
  
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
                        value = 1000)),
    
    column(4,
           numericInput("t_t1",
                        "Length of study T1 [minutes]:",
                        min = 0,
                        value = 10)),
    
    # column(4,
    #        radioButtons("masters",
    #                     "Require Masters Qualification (MTurk only)",
    #                     choices = c("Yes", "No"),
    #                     selected = character(0))),
    
    column(4,
           numericInput("n_premium",
                        "Number of premium qualifications*:",
                        min = 0,
                        max = 2,
                        value = 0))
    
  ),
  
  fluidRow(
    column(4,
           numericInput("n_t2",
                        "Number of participants T2:",
                        min = 0,
                        value = 0)),
    
    column(4,
           numericInput("t_t2",
                        "Length of study T2 [minutes]:",
                        min = 0,
                        value = 0))
    
  ),
  
  fluidRow(
    column(4,
           numericInput("n_t3",
                        "Number of participants T3:",
                        min = 0,
                        value = 0)),
    
    column(4,
           numericInput("t_t3",
                        "Length of study T3 [minutes]:",
                        min = 0,
                        value = 0))
    
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
                        value = 0,
                        step = .5))
    
  ),

  
  # Output: Table summarizing the values entered ----
  
  h4("Cost breakdown"),
  
  tableOutput("cost_table"),
  
  withTags({
    div("Notes:",
    ul(
        li("MTurk service fee (", 
           a(href = "https://requester.mturk.com/pricing", "Click here", target="_blank"), 
           "for pricing details): 20%"),
        br(),
        li("MTurk service fee for large (>9) assignments/participants: 40%"),
        br(),
        li("Additional service fee for MTurk Masters Qualifications (higher quality participants: 5%)"),
        br(),
        li("*Premium qualifications (MTurk only): median additional fee of $0.40 per assignment (range from $0.05 to $1.00); MTurk allows a maximum of 2 premium qualifications per HIT"),
        br(),
        li("CloudResearch service fee (",
           a(href = "https://go.cloudresearch.com/en/knowledge/turkprime-fees-for-using-the-mturk-toolkit", "Click here", target="_blank"),
           "for pricing details): 10% plus 20% MTurk service fee; no CR fee for bonuses (standard MTurk fee)"),
        br(),
        li("Prolific service fee (",
           a(href = "https://www.prolific.co/pricing", "Click here", target="_blank"),
           "for pricing details): 33%")
        ))

    
  }),
  


) # closing bracket for fluidPage



# SERVER ------------------------------------------------------------------

# Define server logic for slider examples ----
server <- function(input, output) {
  
  # Reactive expression to create data frame of all input values ----
  numericValues <- reactive({
  
      
      # total N over all studies
      n_tot <- sum(input$n_t1, input$n_t2, input$n_t3)
      
      # total study time (in minutes)
      t_tot <- sum(input$t_t1, input$t_t2, input$t_t3)
      
      # Participant payment excl. bonus
      part_pay <- sum(
        input$n_t1*(input$t_t1/60*input$rate),
        input$n_t2*(input$t_t2/60*input$rate),
        input$n_t3*(input$t_t3/60*input$rate)
      )
      
      # participant payment excl. bonus
      # part_pay <- n_tot * (t_tot / 60 * input$rate)
      
      # bonus payment
      bonus <- if_else(input$n_t3 != 0 & input$t_t3 != 0, 
                       input$n_t3 * input$bonus, 
                       if_else(input$n_t2 != 0 & input$t_t2 != 0, 
                               input$n_t2 * input$bonus,
                               input$n_t1 * input$bonus))
      
      
      
      # if(input$n_t3 != 0 & input$t_t3 != 0){
      #   bonus <- input$n_t3 * input$bonus
      # } else if(input$n_t2 != 0 & input$t_t2 != 0){
      #   bonus <- input$n_t2 * input$bonus
      # } else (input$n_t1 != 0 & input$t_t1 != 0){
      #   bonus <- input$n_t1 * input$bonus
      # }
        
      # total payment for participants
      tot_pay <- part_pay + bonus
      
      

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
      
      
      ## Service fees
      # MTurk
      
      premium_fee <- .4
      
      mturk_fee <- (part_pay + bonus) * .2 + input$n_premium * premium_fee *  n_tot
      
      # MTurk + Masters qual
      masters_fee <- (part_pay + bonus) * .2 + part_pay * .05 + input$n_premium * premium_fee *  n_tot
      
      # CloudResearch (MTurk + CR fee)
      # CR does not issue fee for bonuses
      cr_fee <- (part_pay) * .3 + bonus * .2 + input$n_premium * premium_fee *  n_tot
      
      # Prolific
      prolific_fee <- (part_pay + bonus) * .33
      

      # Total pay
      tot_pay_mturk <- tot_pay + mturk_fee
      tot_pay_masters <- tot_pay + masters_fee
      tot_pay_cr <- tot_pay + cr_fee
      tot_pay_prolific <- tot_pay + prolific_fee
    

    # Prolific cost breakdown
    data.frame(
      Component = c("Participant payment",
                    "Service fee",
                    "Total"),
      Cost.MTurk = as.character(c(round(tot_pay, 2),
                                  round(mturk_fee, 2),
                                  round(tot_pay_mturk, 2))),
      Cost.MTurk.Masters = as.character(c(round(tot_pay, 2),
                                          round(masters_fee, 2),
                                          round(tot_pay_masters, 2))),
      Cost.CloudResearch = as.character(c(round(tot_pay, 2),
                                          round(cr_fee, 2),
                                          round(tot_pay_cr, 2))),
      Cost.Prolific = as.character(c(round(tot_pay, 2),
                                     round(prolific_fee, 2),
                                     round(tot_pay_prolific, 2))),
      stringsAsFactors = FALSE)
    
  })
  
  # Show the values in an HTML table ----
  output$cost_table <- renderTable({
    numericValues()
  })
  
}

# Create Shiny app ----
shinyApp(ui, server)











