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
                        "No. of participants (assignments) T1:",
                        min = 0,
                        value = 0)),
    
    column(4,
           numericInput("t_t1",
                        "Length of study T1 (minutes):",
                        min = 0,
                        value = 0)),
    
    # column(4,
    #        radioButtons("masters",
    #                     "Require Masters Qualification (MTurk only)",
    #                     choices = c("Yes", "No"),
    #                     selected = character(0))),
    
    
    column(4,
           numericInput("bonus_t1",
                        "Bonus payment per participant T1 ($):",
                        min = 0,
                        value = 0,
                        step = .1))
    
  ),
  
  fluidRow(
    column(4,
           numericInput("n_t2",
                        "No. of participants (assignments) T2:",
                        min = 0,
                        value = 0)),
    
    column(4,
           numericInput("t_t2",
                        "Length of study T2 [minutes]:",
                        min = 0,
                        value = 0)),
    
    column(4,
           numericInput("bonus_t2",
                        "Bonus payment per participant T2 ($):",
                        min = 0,
                        value = 0,
                        step = .1))
    
  ),
  
  fluidRow(
    column(4,
           numericInput("n_t3",
                        "No. participants (assignments) T3:",
                        min = 0,
                        value = 0)),
    
    column(4,
           numericInput("t_t3",
                        "Length of study T3 [minutes]:",
                        min = 0,
                        value = 0)),
    
    column(4,
           numericInput("bonus_t3",
                        "Bonus payment per participant T3 ($):",
                        min = 0,
                        value = 0,
                        step = .1))
    
  ),
  
  fluidRow(
    column(4,
           numericInput("n_premium",
                        "No. of premium qualifications*:",
                        min = 0,
                        max = 2,
                        value = 0)),
    
    column(4,
           # Hourly rate paid to participants
           sliderInput("rate",
                       "Hourly rate ($)",
                       min = 0,
                       max = 20,
                       value = 7.5,
                       step = .1)),
    
    column(4,
           # VAT / tax
           numericInput("vat",
                        "VAT / tax (%):",
                        min = 0,
                        # max = 10,
                        value = 7.7)),
    
    column(4,
           actionButton("reset", "Reset",
                        style="color: #fff; background-color: #337ab7; border-color: #2e6da4"))
    
    
  ),

  
  # Output: Table summarizing the values entered ----
  
  h4("Cost breakdown (in $)"),
  
  tableOutput("cost_table"),
  
  withTags({
    div("Notes:",
    ul(
        li("MTurk service fee (", 
           a(href = "https://requester.mturk.com/pricing", "Click here", target="_blank"), 
           "for pricing details): 20%"),
        br(),
        li("MTurk service fee for large (>9) assignments/participants: 40%. Circumvent this by breaking down large assignments."),
        br(),
        li("Additional service fee for MTurk Masters Qualifications (higher quality participants: 5%)"),
        br(),
        li("* Premium qualifications (MTurk only): median additional fee of $0.40 per assignment (range from $0.05 to $1.00); MTurk allows a maximum of 2 premium qualifications per HIT"),
        br(),
        li("** Check what level of VAT / tax applies to you depending on your region:",
           a(href = "https://aws.amazon.com/tax-help/", "Amazon Web Services Tax Help", target="_blank"),
        ),
        br(),
        li("CloudResearch service fee (",a(href = "https://go.cloudresearch.com/en/knowledge/turkprime-fees-for-using-the-mturk-toolkit", "Click here", target="_blank"),
           "for pricing details): 10% plus 20% MTurk service fee; no CR fee for bonuses (standard MTurk fee)"),
        br(),
        li("Prolific service fee (",
           a(href = "https://www.prolific.co/pricing", "Click here", target="_blank"),
           "for pricing details): 33%"),
        br(),
        li("All amounts in the cost breakdown are estimates only. Real fees may differ.")
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
      
      
      # Total participant payment excl. bonus
      part_pay_tot <- 
        input$n_t1*(input$t_t1/60*input$rate) +
        input$n_t2*(input$t_t2/60*input$rate) +
        input$n_t3*(input$t_t3/60*input$rate)
      
      # Bonus payment for all participants
      bonus_t1_tot <- ifelse(input$n_t1 != 0 & input$t_t1 != 0, 
                          input$n_t1 * input$bonus_t1,
                          0) 
      
      bonus_t2_tot <- ifelse(input$n_t2 != 0 & input$t_t2 != 0, 
                          input$n_t2 * input$bonus_t2,
                          0)
      
      bonus_t3_tot <- ifelse(input$n_t3 != 0 & input$t_t3 != 0, 
                          input$n_t3 * input$bonus_t3,
                          0)
      
      bonus_tot <- bonus_t1_tot + bonus_t2_tot + bonus_t3_tot
      
      
      # total payment for all participants
      tot_pay <- part_pay_tot + bonus_tot
      
      # payment for a single participant, excl. bonus
      part_pay_t1 <- ifelse(input$n_t1 != 0 & input$t_t1 != 0,
                             (input$t_t1/60*input$rate),
                             0) 
      
      part_pay_t2 <- ifelse(input$n_t2 != 0 & input$t_t2 != 0,
                            (input$t_t2/60*input$rate),
                            0)
      
      part_pay_t3 <- ifelse(input$n_t3 != 0 & input$t_t3 != 0,
                            (input$t_t3/60*input$rate),
                            0)
      
      
      # Bonus payment per participant
      bonus_t1 <- ifelse(input$n_t1 != 0 & input$t_t1 != 0, 
                         input$bonus_t1,
                         0) 
      
      bonus_t2 <- ifelse(input$n_t2 != 0 & input$t_t2 != 0, 
                         input$bonus_t2,
                         0)
      
      bonus_t3 <- ifelse(input$n_t3 != 0 & input$t_t3 != 0, 
                         input$bonus_t3,
                         0)
      
      # total bonus per person
      bonus_tot_1 <- bonus_t1 + bonus_t2 + bonus_t3
      
      
      part_pay_tot_1 <- sum(
        part_pay_t1,
        part_pay_t2,
        part_pay_t3,
        (bonus_t1),
        (bonus_t2),
        (bonus_t3)
      )
      

      ## Service fees
      
      # VAT
      vat <- input$vat/100 # percent
      
      # MTurk
      
      premium_fee <- .4
      
      mturk_fee <- (part_pay_tot + bonus_tot) * .2 + input$n_premium * premium_fee *  n_tot
      mturk_vat <- (tot_pay + mturk_fee) * vat
      
      # MTurk large batch (N > 9)
      # UNCLEAR IF 40% COMMISSION IS ONLY ON PAYMENT OR ALSO BONUS
      mturk_fee_large <- (part_pay_tot + bonus_tot) * .4 + input$n_premium * premium_fee *  n_tot
      mturk_vat_large <- (tot_pay + mturk_fee_large) * vat
      
      # MTurk + Masters qual
      masters_fee <- (part_pay_tot + bonus_tot) * .2 + part_pay_tot * .05 + input$n_premium * premium_fee *  n_tot
      masters_vat <- (tot_pay + masters_fee) * vat
      
      # CloudResearch (MTurk + CR fee)
      # CR does not issue fee for bonuses
      cr_fee <- (part_pay_tot) * .3 + bonus_tot * .2 + input$n_premium * premium_fee *  n_tot
      cr_vat <- (tot_pay + cr_fee) * vat
      
      # Prolific
      prolific_fee <- (part_pay_tot + bonus_tot) * .33
      prolific_vat <- (tot_pay + prolific_fee) * vat
      
      # Total pay
      tot_pay_mturk <- tot_pay + mturk_fee + mturk_vat
      tot_pay_mturk_large <- tot_pay + mturk_fee_large + mturk_vat_large
      tot_pay_masters <- tot_pay + masters_fee + masters_vat
      tot_pay_cr <- tot_pay + cr_fee + cr_vat
      tot_pay_prolific <- tot_pay + prolific_fee + prolific_vat

    # Cost breakdown
    data.frame(
      Component = c("Reward per participant (incl. bonus)",
                    "Total participant payment",
                    "Total service fee",
                    "Total VAT",
                    "Total cost"),
      "MTurk (N < 10)" = as.character(c(round(part_pay_tot_1, 2),
                                        round(tot_pay, 2),
                                        round(mturk_fee, 2),
                                        round(mturk_vat, 2),
                                        round(tot_pay_mturk, 2))),
      "MTurk (N > 9)" = as.character(c(round(part_pay_tot_1, 2),
                                       round(tot_pay, 2),
                                       round(mturk_fee_large, 2),
                                       round(mturk_vat_large, 2),
                                       round(tot_pay_mturk_large, 2))),
      "MTurk Masters (N < 10)" = as.character(c(round(part_pay_tot_1, 2),
                                                round(tot_pay, 2),
                                                round(masters_fee, 2),
                                                round(masters_vat, 2),
                                                round(tot_pay_masters, 2))),
      CloudResearch = as.character(c(round(part_pay_tot_1, 2),
                                     round(tot_pay, 2),
                                     round(cr_fee, 2),
                                     round(cr_vat, 2),
                                     round(tot_pay_cr, 2))),
      Prolific = as.character(c(round(part_pay_tot_1, 2),
                                round(tot_pay, 2), 
                                round(prolific_fee, 2),
                                round(prolific_vat, 2),
                                round(tot_pay_prolific, 2))),
      stringsAsFactors = FALSE,
      check.names=FALSE) # this allows special characters (e.g., spaces) in column names
    
  })
  
  # Show the values in an HTML table ----
  output$cost_table <- renderTable({
    numericValues()
  },
  striped = T)
  
  # Reset button
  observeEvent(input$reset, {
    updateNumericInput(session = getDefaultReactiveDomain(), "n_t1", value = 0)
    updateNumericInput(session = getDefaultReactiveDomain(), "t_t1", value = 0)
    updateNumericInput(session = getDefaultReactiveDomain(), "bonus_t1", value = 0)
    updateNumericInput(session = getDefaultReactiveDomain(), "n_t2", value = 0)
    updateNumericInput(session = getDefaultReactiveDomain(), "t_t2", value = 0)
    updateNumericInput(session = getDefaultReactiveDomain(), "bonus_t2", value = 0)
    updateNumericInput(session = getDefaultReactiveDomain(), "n_t3", value = 0)
    updateNumericInput(session = getDefaultReactiveDomain(), "t_t3", value = 0)
    updateNumericInput(session = getDefaultReactiveDomain(), "bonus_t3", value = 0)
    updateNumericInput(session = getDefaultReactiveDomain(), "n_premium", value = 0)
    updateNumericInput(session = getDefaultReactiveDomain(), "rate", value = 7.5)
    updateNumericInput(session = getDefaultReactiveDomain(), "vat", value = 7.7)
  })
  
}

# Create Shiny app ----
shinyApp(ui, server)











