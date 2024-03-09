library(shiny)
library(shinythemes)
library(tidyverse)

# UI ----------------------------------------------------------------------

ui <- fluidPage(
  theme = shinythemes::shinytheme("flatly"),  # Apply the flatly theme
  title = "Study cost calculator",
  
  # Custom CSS for hover effect with adjustabe content
  tags$head(
    tags$style(HTML('
    body {
        font-family: "Arial", sans-serif;
      }

    .popup-card {
      position: relative;
      display: inline-block;
      cursor: pointer;
    }

    .popup-content {
      display: none;
      position: absolute;
      background: #333;
      color: #fff;
      padding: 5px;
      border-radius: 5px;
      top: -40px;
      left: 0;
      width: 200px;
      text-align: left;
      font-size: 12px;
    }

    .popup-card:hover .popup-content {
      display: block;
    }

    .navbar {
        border-radius: 0;
        height: 80px;
        }
      # .table {
      #   margin-bottom: 0;
      # }
      # hr {
      #   border-top-color: #bbb;
      # }

      .footer {
        font-size: 15px;
        text-align: center;
        padding: 20px;
      }
  '))
  ),
  
  # Custom header/banner
  div(class = "header-banner", 
      style = "background-color: #2C3E50; color: white; padding: 10px 100px 0px 10px; height: 90px;",
      h3("Online study cost calculator", 
         style = "margin: 0;"),
      br(),
      div(style = "font-size: 15px;", 
          "This tool helps you estimate the costs for your next online study on Amazon Mechanical Turk (incl. CloudResearch) or Prolific.")
  ),
  
  fluidRow(
    div(style = "height: 20px;")
  ),
  
  fluidRow(
    
    column(4,
           h4("No. of participants"),
           
           numericInput("n_t1",
                        span("T1:",
                             tags$div(
                               class = "popup-card",
                               tags$style("z-index: 1000;"),  # Set a high z-index value
                               icon("info-sign", lib = "glyphicon"),
                               tags$div(
                                 class = "popup-content",
                                 "Enter the number of participants (or assignments on MTurk) you plan on recruiting. If you are planning a study with multiple time points T, enter the number of participants for each time point (T1-T3).",
                               )
                             )
                        ),
                        min = 0,
                        value = 0))
    ,
    
    column(4,
           h4("Length of study (minutes)"),
           numericInput("t_t1",
                        span("T1:",
                             tags$div(
                               class = "popup-card",
                               tags$style("z-index: 1000;"),  # Set a high z-index value
                               icon("info-sign", lib = "glyphicon"),
                               tags$div(
                                 class = "popup-content",
                                 "Enter the estimated duration of your study for an individual participant. If you are planning a study with multiple time points T, enter the study duration for each time point (T1-T3).",
                               )
                             )
                        ),
                        min = 0,
                        value = 0)),
    
    column(4,
           h4("Bonus per participant ($)"),
           numericInput("bonus_t1",
                        span("T1:",
                             tags$div(
                               class = "popup-card",
                               tags$style("z-index: 1000;"),  # Set a high z-index value
                               icon("info-sign", lib = "glyphicon"),
                               tags$div(
                                 class = "popup-content",
                                 "Enter the bonus payment each participant will get. If you are planning a study with multiple time points T, enter the bonus payment per participant for each time point (T1-T3).",
                               )
                             )
                        ),
                        min = 0,
                        value = 0,
                        step = .1))
    
  ),
  
  fluidRow(
    column(4,
           numericInput("n_t2",
                        "T2:",
                        min = 0,
                        value = 0)),
    
    column(4,
           numericInput("t_t2",
                        "T2:",
                        min = 0,
                        value = 0)),
    
    column(4,
           numericInput("bonus_t2",
                        "T2:",
                        min = 0,
                        value = 0,
                        step = .1))
    
  ),
  
  fluidRow(
    column(4,
           numericInput("n_t3",
                        "T3:",
                        min = 0,
                        value = 0)),
    
    column(4,
           numericInput("t_t3",
                        "T3:",
                        min = 0,
                        value = 0)),
    
    column(4,
           numericInput("bonus_t3",
                        "T3:",
                        min = 0,
                        value = 0,
                        step = .1)),
    
  ),
  
  
  fluidRow(
    column(4,
           numericInput("n_premium",
                        span("No. of premium qualifications:", 
                             tags$div(
                               class = "popup-card",
                               icon("info-sign", lib = "glyphicon"),
                               tags$div(
                                 class = "popup-content",
                                 "MTurk only: median additional fee of $0.40 per assignment (range from $0.05 to $1.00); MTurk allows a maximum of 2 premium qualifications per HIT",
                               )
                             )
                        ),
                        min = 0,
                        max = 2,
                        value = 0)),
    
    column(4,
           # Hourly rate paid to participants
           numericInput("rate",
                       "Hourly rate ($)",
                       min = 0,
                       max = 20,
                       value = 9,
                       step = .1)),
    
    column(4,
           # VAT / tax
           numericInput("vat",
                        span("VAT / tax (%):",
                             tags$div(
                               class = "popup-card",
                               icon("info-sign", lib = "glyphicon"),
                               tags$div(
                                 class = "popup-content",
                                 "Check what level of VAT / tax applies to you depending on your region:",
                                 a(href = "https://aws.amazon.com/tax-help/", "Amazon Web Services Tax Help", target="_blank")                               
                               )
                             )
                             
                        ),
                        min = 0,
                        # max = 10,
                        value = 8.1)) # current MTurk VAT in Switzerland
  ),
  
  fluidRow(
    column(4,
           actionButton("reset", "Reset",
                        style="color: #fff; background-color: #337ab7; border-color: #2e6da4"))
  ),
  
  # Add an empty gap with a specified height (e.g., 20 pixels)
  fluidRow(
    div(style = "height: 20px;")
  ),
  
  
  # Output: Table summarizing the values entered ----
  
  h4("Cost breakdown ($)",
     tags$div(
       class = "popup-card",
       tags$style("z-index: 1000; h5"),  # Set a high z-index value
       icon("info-sign", lib = "glyphicon"),
       tags$div(
         class = "popup-content",
         style = "width: 400px;",
         "MTurk service fee: 20% - ", 
         a(href = "https://www.mturk.com/pricing", "Pricing details", target="_blank"),
         br(),
         br(),
         "MTurk service fee for large (>9) assignments/participants: 40%. Circumvent this by breaking down large assignments.",
         br(),
         br(),
         "Additional service fee for MTurk Masters Qualifications (higher quality participants): 5%",
         br(),
         br(),
         "CloudResearch service fee: 20% plus 20% MTurk service fee; no CR fee for bonuses (standard MTurk fee) - ",
         a(href = "https://go.cloudresearch.com/en/knowledge/turkprime-fees-for-using-the-mturk-toolkit", "Pricing details", target="_blank"),
         br(),
         br(),
         "Prolific service fee (academia): 33% - ",
         a(href = "https://researcher-help.prolific.com/hc/en-gb/articles/360009223533-What-is-your-pricing", "Pricing details", target="_blank"),
         br(),
         br(),
         strong("Note: "),
         "All service fees are calculated as the proportion of total participant payment. Fees are as of March 2024."
       )
     )
  ),
  
  tableOutput("cost_table"),
  
  div(
    strong("Caution:"),
    "All amounts in the cost breakdown are estimates only, and real fees may differ. We disclaim any responsibility for discrepancies in the actual fees."
  ),
  
  div(class = "footer",
      br(),
      hr(),
      "Created by Olivia Fischer. The R code for this app is available on",
      a(href = "https://github.com/oliviafischer/cost_calculator", "GitHub.", target = "_blank")
  )
  
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
    
    # total payment per person
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
    # mturk_fee_large <- (part_pay_tot + bonus_tot) * .4 + input$n_premium * premium_fee *  n_tot # 40% charged on everything
    mturk_fee_large <- (part_pay_tot) * .4 + bonus_tot * .2+ input$n_premium * premium_fee *  n_tot # 40% charged on only participant payment (not bonus)
    mturk_vat_large <- (tot_pay + mturk_fee_large) * vat
    
    # MTurk + Masters qual
    masters_fee <- (part_pay_tot + bonus_tot) * .2 + part_pay_tot * .05 + input$n_premium * premium_fee *  n_tot
    masters_vat <- (tot_pay + masters_fee) * vat
    
    # CloudResearch (CR fee + MTurk)
    # CR rounds service fee (20%) to nearest penny *per participant*
    # CR does not issue fee for bonuses
    cr_fee <- (round((sum(part_pay_t1, part_pay_t2,part_pay_t3) * .2), 2)) * n_tot + (part_pay_tot + bonus_tot) * .2 + input$n_premium * premium_fee *  n_tot
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