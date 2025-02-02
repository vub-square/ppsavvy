################################################################################
# Title: EXCITE
# Author: Perseverence Savieri
# Date: Sys.Date()
# Version: 001
################################################################################

options(shiny.maxRequestSize = 10000 * 1024^2)

#################
# Load libraries
#################
library(shiny)
library(ggplot2)
library(plotly)
library(DT)
library(formatR)
library(knitr)
library(haven)
library(readxl)
library(readr)
library(openxlsx)
library(dplyr)
library(tidyr)
library(tidyverse)
library(car)
library(MASS)
library(lindia)
library(summarytools)
library(shinymeta)
library(shinyBS)
library(shinythemes)
library(sjPlot)
library(sjmisc)
library(sjlabelled)
library(kableExtra)
library(shinyFeedback)
library(shinyAce)
library(bslib)
library(tinytex)
library(webshot2)
library(rpart)
library(rpart.plot) # For visualizing the tree
library(palmerpenguins)
library(stargazer)
library(gridExtra) # For arranging plots
library(effectsize) # For effect sizes
library(tidyr) # For data manipulation

#####################################
# Define UI
#####################################
ui <- fluidPage(
  shinyFeedback::useShinyFeedback(),
  # tags$head(
  # Google Analytics tracking
  #   HTML(
  #     "<!-- Google tag (gtag.js) -->
  #       <script async src='https://www.googletagmanager.com/gtag/js?id=G-FEB7W5B2B0'></script>
  #       <script>
  #       window.dataLayer = window.dataLayer || [];
  #     function gtag(){dataLayer.push(arguments);}
  #     gtag('js', new Date());
  #
  #     gtag('config', 'G-FEB7W5B2B0');
  #     </script>"
  #   )
  # ),

  # Open navbarPage for main headings
  navbarPage(
    id = "navbar",
    # Application title
    # tags$p("EXCITE", style = "color:#003399"),
    title = "EXCITE: Exploring Complex Interactions with Tree Models",
    theme = shinytheme("flatly"),

    # Main tabs for the web application
    ###################################
    # Home Tab
    ###################################
    tabPanel(
      tags$p("Home", style = "color:#FF6600"),
      sidebarLayout(
        sidebarPanel(
          # Welcome page sidebar content
          width = 4,
          img(src = "logo_bisi_rgb.jpg", width = 420, height = 100),
          br(),
        ),
        mainPanel(
          width = 8,
          tags$h4("Welcome to EXCITE", style = "color:#003399"),
          tags$p("Welcome to EXCITE (Exploring Complex Interactions using Tree-based models)! This research tool helps researchers uncover and visualise complex interactions in their data using a combination of ANOVA and decision tree modeling. Designed for ease of use, EXCITE offers a seamless workflow from data import to advanced statistical analysis, with the ability to download reports summarising your results."),
          tags$p(
            "EXCITE was developed by the ",
            tags$a(href = "https://square.research.vub.be/", "Support for Quantitative and Qualitative Research (SQUARE)"),
            " core facility, part of the ",
            tags$a(href = "https://bisi.research.vub.be/", "Biostatistics and Medical Informatics research group (BISI)"),
            " at the Vrije Universiteit Brussel (VUB). We aim to provide researchers with complimentary statistical support."
          ),
          br(),
          tags$h4("Key Features", style = "color:#003399"),
          tags$ul(
            tags$li("Interactive data import and management options."),
            tags$li("Two- and Three-Way ANOVA modeling with clear visualizations."),
            tags$li("Decision tree exploration to uncover complex interactions."),
            tags$li("Dynamic report generation in multiple formats (HTML, PDF, Word).")
          ),
          br(),
          tags$h4("Terms of use", style = "color:#003399"),
          tags$p(
            "EXCITE is not exhaustive and is suitable only for the use cases described in the app. Uploaded data and outputs will not be stored on our servers. Please refrain from uploading sensitive information as the tool is provided ",
            strong("WITHOUT ANY WARRANTY"), ". You are solely responsible for the confidentiality, availability, security, and any potential misuse of your data. However, you can download all results and modified datasets directly."
          ),
          br(),
          tags$h4("Feedback", style = "color:#003399"),
          tags$p(
            "We value your feedback! If you have suggestions, encounter issues, or want to share your experience, please email ",
            tags$a(href = "mailto:perseverence.savieri@vub.be", "Perseverence.Savieri@vub.be"),
            ". Your input helps us improve EXCITE for the research community."
          ),
          br(), br(),
          tags$h4(
            strong("Click on the Dataset tab to get started."),
            style = "color:#FF6600"
          ),
          br(), br(), br(), br()
          # Optional: Uncomment if in testing phase
          # tags$p(
          #   strong("Note: This app is currently in the testing phase."),
          #   style = "color:#EB0018"
          # )
        )
      ),
      br(), br(), br()
    ),
    ###################################
    # Data Tab
    ###################################
    tabPanel(
      tags$p("Dataset", style = "color:#FF6600"),
      sidebarLayout(
        sidebarPanel(
          width = 3,
          # Example Data
          tags$p(strong("In this tab, you can upload your dataset or use example datasets provided in the app. You can also view and manage your data before proceeding to analysis.", style = "color:#003399")),
          tags$hr(),
          selectizeInput(
            inputId = "exdata", label = strong("Choose example data", style = "color:#003399"), selected = "",
            choices = c("", "Penguins", "BP reduction", "Pain reduction", "VO2 Max improvement", "Type 2 Diabetes (3-way)"),
            options = list(placeholder = "Choose example data")
          ),
          tags$hr(),
          # Upload own data
          tags$h5(em("OR load your own data file", style = "color:#FF6600")),
          tags$p(
            em("Ensure your file is in the correct format and contains numeric outcome variables 
  and categorical predictors for ANOVA and decision tree analysis.")
          ),
          radioButtons("ext",
            label = strong("Select file extension", style = "color:#003399"),
            choices = list(
              "Text file (.txt)" = "txt",
              "CSV file (.csv)" = "csv",
              "Excel file (.xlsx)" = "xlsx",
              "SPSS file (.sav)" = "sav",
              "Stata Dataset (.dta)" = "dta"
            ),
            selected = ""
          ),
          conditionalPanel(
            condition = "input.ext == 'txt' | input.ext == 'csv' | input.ext == 'xlsx' | input.ext == 'sav' | input.ext == 'dta'",
            tryCatch(
              fileInput("file1", strong("Select file"),
                accept = c(
                  ".txt", ".csv", ".xlsx", ".sav", ".dta",
                  ".CSV", ".TXT", ".XLSX", ".SAV", ".DTA"
                )
              )
            )
          ),
          tags$hr(),
          # Data management
          uiOutput("edit_vars"),
          uiOutput("data_management"),
          uiOutput("manage_data_options"),
          uiOutput("var_name_extension"),
          uiOutput("changes_button"),
          tags$hr(),
          tags$p(
            "Use the ", strong("View Data"), " tab to preview your dataset and verify the data has been loaded correctly. 
  Navigate to the ", strong("Data Summary"), " tab for descriptive statistics and variable distributions."
          ),
        ),
        mainPanel(
          width = 9,
          # Data loading/uploading main content
          tabsetPanel(
            id = "datatabs",
            # Data preview
            tabPanel(
              tags$p("View Data", style = "color:#003399"),
              value = "datatab1",
              br(), br(),
              dataTableOutput("data_preview"),
              br(), br(),
            ),
            # Data summary
            tabPanel(
              tags$p("Data Summary", style = "color:#003399"),
              value = "datatab2",
              # br(), br(),
              # h5(strong("Data summary")),
              htmlOutput("data_summ"),
              br(), br(),
            )
          )
        )
      )
    ),
    ###################################
    # Two-way ANOVA Model Tab
    ###################################
    tabPanel(
      tags$p("Two-way ANOVA", style = "color:#FF6600"),
      sidebarLayout(
        sidebarPanel(
          width = 3,
          tags$p(strong("This tab allows you to conduct a Two-Way ANOVA to investigate the main effects and interaction effects of two categorical predictors on a numeric outcome variable.", style = "color:#003399")),
          tags$hr(),
          tags$p(
            em("Select one numeric outcome variable and two categorical predictors to fit the Two-Way ANOVA model.")
          ),
          tags$hr(),
          uiOutput("outcome_selector"),
          uiOutput("factor1_selector"),
          uiOutput("factor2_selector"),
          actionButton("run_anova", "Run Two-Way ANOVA",
            class = "btn-primary"
          ),
          tags$hr(),
          tags$p(
            "Explore the results of your ANOVA model, including statistical summaries, boxplots of the main effects, and diagnostics to assess model assumptions."
          ),
        ),
        mainPanel(
          width = 9,
          tabsetPanel(
            tabPanel(
              tags$p("ANOVA Results", style = "color:#003399"),
              br(),
              uiOutput("two_aov_sum")
              # uiOutput("two_aov_efs")
            ),
            tabPanel(
              tags$p("Boxplots", style = "color:#003399"),
              br(), br(),
              uiOutput("two_aov_boxplots")
            ),
            tabPanel(
              tags$p("Diagnostics", style = "color:#003399"),
              br(), br(),
              uiOutput("two_aov_diag")
            )
          ),
          br(), br(), br()
        )
      )
    ),

    ###################################
    # Three-way ANOVA Model Tab
    ###################################
    tabPanel(
      tags$p("Three-way ANOVA", style = "color:#FF6600"),
      sidebarLayout(
        sidebarPanel(
          width = 3,
          tags$p(
            strong("Use this tab to perform a Three-Way ANOVA to examine how three categorical predictors interact to influence a numeric outcome variable.", style = "color:#003399")
          ),
          tags$hr(),
          tags$p(
            em("Select one numeric outcome variable and three categorical predictors for the analysis.")
          ),
          tags$hr(),
          uiOutput("threeway_outcome_selector"),
          uiOutput("threeway_factor1_selector"),
          uiOutput("threeway_factor2_selector"),
          uiOutput("threeway_factor3_selector"),
          actionButton("run_threeway_anova", "Run Three-Way ANOVA",
            class = "btn-primary"
          ),
          tags$hr(),
          tags$p(
            "In the ", strong("ANOVA Results"), " tab, view statistical summaries of the model. 
  Use the ", strong("Box Plots"), " tab to visualise main effects and interaction plots for 
  detailed exploration of factor relationships."
          )
        ),
        mainPanel(
          width = 9,
          tabsetPanel(
            tabPanel(
              tags$p("ANOVA Results", style = "color:#003399"),
              br(),
              uiOutput("three_aov_sum")
            ),
            tabPanel(
              tags$p("Box Plots", style = "color:#003399"),
              br(), br(),
              uiOutput("three_aov_boxplots")
            ),
            tabPanel(
              tags$p("Interaction Plots", style = "color:#003399"),
              br(), br(),
              selectInput("interaction_view", "Select Interaction View:",
                choices = c(
                  "Factor1 x Factor2",
                  "Factor1 x Factor3",
                  "Factor2 x Factor3"
                )
              ),
              selectInput("conditioning_factor", "Condition on:",
                choices = NULL
              ), # Will be updated in server
              plotOutput("threeway_interaction_plot"),
              br(), br(), br()
            ),
            tabPanel(
              tags$p("Diagnostics", style = "color:#003399"),
              br(), br(),
              uiOutput("three_aov_diag")
            )
          )
        )
      )
    ),

    ###################################
    # Decision Tree Tab
    ###################################

    tabPanel(
      tags$p("Decision Tree", style = "color:#FF6600"),
      sidebarLayout(
        sidebarPanel(
          width = 3,
          tags$p(
            strong("This tab provides an interactive approach to explore complex interactions in your dataset using decision trees. Decision trees can help confirm and visualize interactions found in your ANOVA models.", style = "color:#003399")
          ),
          tags$hr(),
          tags$p(
            em("Select one numeric outcome variable and up to three categorical predictors to fit the decision tree model.")
          ),
          tags$hr(),
          uiOutput("tree_outcome_selector"),
          uiOutput("tree_factor1_selector"),
          uiOutput("tree_factor2_selector"),
          uiOutput("tree_factor3_selector"),
          checkboxInput("use_three_factors", "Include third factor", value = FALSE),
          uiOutput("tree_complexity_selector"),
          # uiOutput("tree_complexity_slider"),
          actionButton("run_tree", "Fit Decision Tree",
            class = "btn-success"
          ),
          tags$hr(),
          tags$p(
            "The ", strong("Tree Visualisation"), " tab shows the fitted decision tree, 
  while the ", strong("Variable Importance"), " tab highlights the most important predictors in your model."
          )
        ),
        mainPanel(
          width = 9,
          tabsetPanel(
            tabPanel(
              tags$p("Tree Visualisation", style = "color:#003399"),
              uiOutput("decision_tree")
            ),
            tabPanel(
              tags$p("Variable Importance", style = "color:#003399"),
              br(), br(),
              plotOutput("variable_importance_plot"),
              br(), br()
            )
          )
        )
      )
    ),

    ###################################
    # Reports Tab
    ###################################
    tabPanel(
      tags$p("Reports", style = "color:#FF6600"),
      sidebarLayout(
        sidebarPanel(
          width = 3,
          # Report generation sidebar content
          tags$p(
            strong("Generate a report summarizing your ANOVA and decision tree analyses. 
  Choose a format (HTML, PDF, or Word) and click the button to download.", style = "color:#003399")
          ),
          tags$hr(),
          selectInput("report_type",
            label = strong("Select Report Type", style = "color:#003399"),
            choices = c("Two-way ANOVA Report", "Three-way ANOVA Report"),
            selected = "Two-way ANOVA Report"
          ),
          tags$hr(),
          selectInput("report_format", label = strong("Select Report Format", style = "color:#003399"), choices = c("HTML (Recommended)" = "HTML", "PDF" = "PDF", "Word" = "Word")),
          downloadButton("download_report", "Download Report")
        ),
        mainPanel(
          width = 9,
          tags$p("The analysis report includes the following sections:"),
          tags$ol(
            tags$li("ANOVA analysis", style = "color:#003399"),
            tags$p("Summary, results, and plots for Two- and Three-way ANOVA models.."),
            tags$li("Decision tree exploration", style = "color:#003399"),
            tags$p("Visualisations and variable importance for decision trees."),
          )
        )
      )
    ),
    ###################################
    # Manual Tab
    ###################################
    tabPanel(
      tags$p("Manual", style = "color:#FF6600"),
      fluidPage(
        img(src = "logoVUB.png", height = 50, align = "right"),
        titlePanel(h3("Documentation", style = "color:#003399")),
        tags$hr(),
        tags$h3("Steps on how to use the app"),
        br(),
        tags$h4("Step 1: Load data"),
        tags$p(
          "Start by selecting an example dataset or uploading your own data in the ", strong("Dataset"), " tab on the sidebar panel."
        ),
        tags$p("Make sure to choose the correct file extension to ensure proper data loading and ensure your data contains a numeric outcome variable and categorical predictors."),
        tags$p(
          "If you need to change the data type of a variable, select the variable in question and then choose the ", em("New data type."), "Apply changes by clicking the ", em("Change data type"), " button."
        ),
        tags$p(
          "To verify that the data has been loaded correctly, check the ", strong("View Data"), " tab, which displays a preview (10 rows by default). You can adjust this setting through the ",
          em("Show entries"), " dropdown. A summary of your dataset, including descriptive statistics and variable distributions, can be found in the ",
          strong("Data Summary"), " tab."
        ),
        br(),
        tags$h4("Step 2: Run ANOVA"),
        tags$p("Use the ", strong("Two-Way ANOVA"), " or ", strong("Three-Way ANOVA"), " tabs to fit your model. Specify outcome and predictor variables, then click ", em("Run ANOVA"), "."),
        tags$p("You can view the model results and visualise main effects under the ", strong("ANOVA Results"), "and", strong("Boxplots"), " tabs on the main panels."),
        br(),
        tags$h4("Step 3: Explore Interactions"),
        tags$p("Use the ", strong("Decision Tree"), " tab to explore and visualise interactions in your data. Select outcome and predictor variables, and fit tree model using the", strong("Fit Decision Tree"), "button."),
        tags$p("For Three-Way interactions, you'll need to tick the", em("Include third factor"), "box to customize the tree model."),
        tags$p("Confirm and study interactions by exploring the decision tree. You can choose which interactions to show from the radio buttons."),
        br(),
        tags$h4("Step 5: Generate a report"),
        tags$p(
          "After your analysis, you can generate a report by selecting the desired format (HTML, PDF, or Word)."
        ),
        br(),
        tags$h4("Tips and Common Issues"),
        tags$ul(
          tags$li("Ensure your dataset is formatted correctly, with clear variable names and types."),
          tags$li("If you encounter errors, check that your outcome variable is numeric and predictors are categorical.")
        ),
        br(),
        tags$h4("Note"),
        tags$p(
          "At any time, you can refresh the session and start over by clicking the ", strong("Power"), " button."
        ),
        br(),
        br(),
        br(),
        br(),
        br()
      )
    ),

    ###################################
    # Contact Us Tab
    ###################################
    tabPanel(
      tags$p("Contact Us", style = "color:#FF6600"),
      fluidPage(
        img(src = "logo_bisi_rgb.jpg", height = 60, align = "right"),
        titlePanel(h3("Developers", style = "color:#003399")),
        hr(),
        # h3("Perseverence Savieri"),
        fluidRow(
          column(2,
            img(src = "HR_BIBI_Percy2.jpg", height = 200),
            br(), br(),
            style = "text-align: center;"
          ),
          column(
            8,
            br(), br(),
            tags$p("Perseverence Savieri is a doctoral researcher in the", tags$a(href = "https://bisi.research.vub.be/", "Biostatistics and Medical Informatics research group (BISI)"), "at the Vrije Universiteit Brussel medical campus Jette. He is also a principal statistical consultant for the humanities and social sciences at campus Etterbeek through the", tags$a(href = "https://square.research.vub.be/", "Support for Quantitative and Qualitative Research (SQUARE)"), "core facility. Here, he offers statistical and methodological quantitative support in the form of consultations, statistical coaching, data analyses and workshops.", style = "text-align: center;"),
            h5(tags$a(href = "mailto:perseverence.savieri@vub.be", "Perseverence.Savieri@vub.be"), style = "text-align: center;"),
            br(),
            h4("Supervisors", style = "text-align: center;"),
            h5("dr. Lara Stas & Prof. dr. Kurt Barbe", style = "text-align: center;")
            # Kurt Barb\xe9
          )
        )
      ),
      absolutePanel(
        bottom = 10,
        left = 0,
        right = 0,
        height = "auto",
        fixed = TRUE,
        tags$div(
          style = "text-align: center; width: 100%; padding: 10px;",
          tags$h6(
            em("Copyright 2025. Support for Quantitative and Qualitative Research. Version 25.01.25")
          )
        )
      )
    ),
    navbarMenu(
      title = tagList(icon("power-off", style = "color:#FF6600")),
      tabPanel("Stop", value = "stop"),
      tabPanel("Refresh", value = "refresh"),
      tabPanel("New session", value = "new_session")
    ),
    position = c("fixed-top"),
    tags$style(type = "text/css", "body{padding-top: 90px;}")
  )
)

#####################################
# Load datasets and expressions used in the server
#####################################
load("penguins_df.RData")
load("data_2x2.RData")
load("data_3x2.RData")
load("data_3x3.RData")
load("data_3way.RData")

#####################################
# Define server logic
#####################################
server <- function(input, output, session) {
  # INPUTS --------------------------------------------------------------------
  observe({
    if (input$navbar == "stop") {
      stopApp() # This will stop the Shiny app
    } else if (input$navbar == "refresh") {
      session$reload() # This will refresh the session
    } else if (input$navbar == "new_session") {
      session$reload() # Assuming starting a new session is equivalent to reloading
    }
  })

  # Load/upload data
  data_input <- reactive({
    inFile <- input$file1
    exdata <- input$exdata

    # Example dataset ----------------------------------------------------------
    if (is.null(inFile)) {
      if (exdata == "") {
        return(NULL)
      } else if (exdata == "Penguins") {
        return(as.data.frame(penguins_df))
      } else if (exdata == "BP reduction") {
        return(as.data.frame(data_2x2))
      } else if (exdata == "Pain reduction") {
        return(as.data.frame(data_3x2))
      } else if (exdata == "VO2 Max improvement") {
        return(as.data.frame(data_3x3))
      } else if (exdata == "Type 2 Diabetes (3-way)") {
        return(as.data.frame(data_3way))
      }
    }

    # Load own dataset ---------------------------------------------------------
    if (!is.null(inFile)) {
      if (input$ext == "txt") {
        return(read.csv(inFile$datapath, sep = "", header = T))
      } else if (input$ext == "csv") {
        return(read.csv(inFile$datapath, header = T))
      } else if (input$ext == "xlsx") {
        return(as.data.frame(read_excel(inFile$datapath, sheet = 1)))
      } else if (input$ext == "sav") {
        # return(read.spss(inFile$datapath, to.data.frame = T, use.value.labels = F))
        return(as.data.frame(read_sav(inFile$datapath)))
      } else if (input$ext == "dta") {
        # return(read.dta(inFile$datapath))
        return(as.data.frame(read_dta(inFile$datapath)))
      }
    }
  })

  dataset <- reactiveVal(NULL)

  # Read data
  observeEvent(data_input(), {
    dataset(data_input())
  })

  # Generate preprocessing options based on user selection
  output$edit_vars <- renderUI({
    req(dataset())

    # Generate variable selection
    var_names <- colnames(dataset())
    tagList(
      tags$h5(em("Data manipulation and preprocessing", style = "color:#FF6600")),
      selectizeInput("selected_var", label = strong("Select variable(s)", style = "color:#003399"), choices = c(" ", var_names), selected = NULL, multiple = FALSE)
    )
  })

  # Show data management options when variables are selected
  output$data_management <- renderUI({
    req(input$selected_var)

    selectizeInput("data_management", label = strong("Select procedure", style = "color:#003399"), choices = c("", "Change data type", "Transform"))
  })

  # Conditional panels for data management options
  output$manage_data_options <- renderUI({
    req(input$selected_var, input$data_management)

    if (input$data_management == "Change data type") {
      selectizeInput("new_data_type", label = strong("New data type", style = "color:#003399"), choices = c("", "Factor", "Numeric", "Integer", "Character", "Date (ddmmyy)"))
    } else if (input$data_management == "Transform") {
      selectizeInput("transform_type", label = strong("Transformation type", style = "color:#003399"), choices = c("", "Ln (natural log)", "Ln (X+1)", "Exp", "Square", "Cube", "Square root", "Standardize", "Center", "Inverse"))
    }
  })

  # Conditional panel for variable name extension
  output$var_name_extension <- renderUI({
    req(input$selected_var, input$transform_type)

    if (!is.null(input$transform_type)) {
      textInput("var_name_extension", label = strong("Variable name extension", style = "color:#003399"), placeholder = "Optional")
    }
  })

  output$changes_button <- renderUI({
    req(dataset())
    actionButton("apply_changes", "Apply Changes")
  })

  # Apply changes to dataset
  observeEvent(input$apply_changes, {
    # req(input$selected_var)
    new_data <- dataset()

    if (input$data_management == "Change data type") {
      new_data <- new_data %>%
        mutate(!!input$selected_var := switch(input$new_data_type,
          "Factor" = as.factor(!!sym(input$selected_var)),
          "Numeric" = as.numeric(!!sym(input$selected_var)),
          "Integer" = as.integer(!!sym(input$selected_var)),
          "Character" = as.character(!!sym(input$selected_var)),
          "Date (dmy)" = as.Date(!!sym(input$selected_var), format = "%d/%m/%Y")
        ))
    } else if (input$data_management == "Transform") {
      transform_func <- switch(input$transform_type,
        "Ln (natural log)" = log,
        "Ln (X+1)" = function(x) log(x + 1),
        "Exp" = exp,
        "Square" = function(x) x^2,
        "Cube" = function(x) x^3,
        "Square root" = sqrt,
        "Standardize" = scale,
        "Center" = function(x) x - mean(x),
        "Inverse" = function(x) 1 / x
      )

      new_var <- paste(input$selected_var, input$var_name_extension, sep = "_")

      new_data <- new_data %>%
        mutate(!!new_var := transform_func(!!sym(input$selected_var)))
    }

    dataset(new_data)
  })

  # OUTPUTS --------------------------------------------------------------------

  ## OVERVIEW OF DATA
  # Data table from DT
  output$data_preview <- renderDataTable({
    req(dataset())
    validate(need(dataset(), "Please, upload own dataset or select one from the examples to continue."))
    n <- dataset()
    dprint <- format(n, digits = 3)
    dprint
  })

  # Data summary using dfSummary() from summarytools
  output$data_summ <- renderUI({
    req(dataset())
    print(
      dfSummary(dataset(),
        graph = TRUE, valid.col = FALSE, graph.magnif = 0.75,
        style = "grid"
      ),
      max.tbl.height = 800, method = "render",
      headings = FALSE, bootstrap.css = FALSE
    )
  })

  # Dynamic UI for Outcome and Factor Selection
  output$outcome_selector <- renderUI({
    req(dataset())
    selectInput("outcome", "Select Outcome Variable",
      choices = names(dataset())[sapply(dataset(), is.numeric)]
    )
  })

  output$factor1_selector <- renderUI({
    req(dataset())
    selectInput("factor1", "Select First Factor",
      choices = names(dataset())[sapply(dataset(), is.factor)]
    )
  })

  output$factor2_selector <- renderUI({
    req(dataset())
    selectInput("factor2", "Select Second Factor",
      choices = names(dataset())[sapply(dataset(), is.factor)]
    )
  })


  #------------------------------------------------------------------
  # Two-way ANOVA Analysis
  anova_model <- eventReactive(input$run_anova, {
    req(input$outcome, input$factor1, input$factor2)
    aov_model <- aov(as.formula(paste(input$outcome, "~", input$factor1, "*", input$factor2)),
      data = dataset()
    )
    return(aov_model)
  })

  # ANOVA Summary with effect sizes
  output$anova_summary <- renderPrint({
    req(anova_model())
    summary(anova_model())
  })

  output$effect_sizes <- renderPrint({
    req(anova_model())
    eta_squared(anova_model(), partial = TRUE)
  })

  # Interaction plot
  output$interaction_plot <- renderPlot({
    req(input$outcome, input$factor1, input$factor2)
    ggplot(
      dataset(),
      aes_string(x = input$factor1, y = input$outcome, color = input$factor2)
    ) +
      stat_summary(fun = mean, geom = "point", size = 3) +
      stat_summary(fun = mean, geom = "line", aes(group = get(input$factor2))) +
      stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.2) +
      theme_minimal() +
      labs(title = "Points show means, error bars show standard errors")
  })

  # Model diagnostics
  output$model_comparison <- renderPrint({
    req(anova_model())
    # Calculate R-squared for ANOVA
    aov_rsq <- summary.lm(anova_model())$r.squared

    cat("ANOVA Model Diagnostics:\n")
    cat("R-squared:", round(aov_rsq, 4), "\n")
    cat("Adjusted R-squared:", round(summary.lm(anova_model())$adj.r.squared, 4), "\n")
    cat("\nShapiro-Wilk test for normality of residuals:\n")
    print(shapiro.test(residuals(anova_model())))
  })

  # Boxplots
  # 1-------------------------
  fact1_boxplot <- reactive({
    req(input$outcome, input$factor1)
    ggplot(
      dataset(),
      aes_string(x = input$factor1, y = input$outcome, fill = input$factor1)
    ) +
      geom_boxplot() +
      theme_bw() +
      labs(title = paste("Boxplot of", input$outcome, "by", input$factor1))
  })

  output$factor1_boxplot <- renderPlot({
    fact1_boxplot()
  })

  # 2------------------------
  fact2_boxplot <- reactive({
    req(input$outcome, input$factor2)
    ggplot(
      dataset(),
      aes_string(x = input$factor2, y = input$outcome, fill = input$factor2)
    ) +
      geom_boxplot() +
      theme_bw() +
      labs(title = paste("Boxplot of", input$outcome, "by", input$factor2))
  })

  output$factor2_boxplot <- renderPlot({
    fact2_boxplot()
  })

  # By grouping variable
  # 1------------------------------------
  fact12_boxplot <- reactive({
    req(input$outcome, input$factor1)
    ggplot(
      dataset(),
      aes_string(x = input$factor1, y = input$outcome, fill = input$factor2)
    ) +
      geom_boxplot() +
      theme_bw() +
      labs(title = paste("Boxplot of", input$outcome, "by", input$factor1))
  })

  output$factor12_boxplot <- renderPlot({
    fact12_boxplot()
  })

  # 2------------------------------------
  fact21_boxplot <- reactive({
    req(input$outcome, input$factor2)
    ggplot(
      dataset(),
      aes_string(x = input$factor2, y = input$outcome, fill = input$factor1)
    ) +
      geom_boxplot() +
      theme_bw() +
      labs(title = paste("Boxplot of", input$outcome, "by", input$factor2))
  })

  output$factor21_boxplot <- renderPlot({
    fact21_boxplot()
  })

  # UI elements
  output$two_aov_sum <- renderUI({
    req(anova_model())
    tagList(
      tags$p("Model summary", style = "color:#003399"),
      verbatimTextOutput("anova_summary"),
      br(),
      tags$p("Effect Sizes", style = "color:#003399"),
      verbatimTextOutput("effect_sizes"),
      br()
    )
  })

  output$two_aov_boxplots <- renderUI({
    req(anova_model())
    tagList(
      tags$p("Plots without grouping", style = "color:#003399"),
      br(),
      fluidRow(
        column(1, ),
        column(
          5,
          plotOutput("factor1_boxplot")
        ),
        column(
          5,
          plotOutput("factor2_boxplot")
        )
      ),
      br(), br(),
      tags$p("Plots with grouping", style = "color:#003399"),
      br(),
      fluidRow(
        column(1, ),
        column(
          5,
          plotOutput("factor12_boxplot")
        ),
        column(
          5,
          plotOutput("factor21_boxplot")
        )
      ),
      br(),
      tags$p("Two-way interaction plot", style = "color:#003399"),
      plotOutput("interaction_plot")
    )
  })

  output$two_aov_diag <- renderUI({
    req(anova_model())
    tagList(
      tags$p("Model diagnostics", style = "color:#003399"),
      verbatimTextOutput("model_comparison")
    )
  })


  #----------------------------------------------------------------------
  # Three-way ANOVA Analysis
  output$threeway_outcome_selector <- renderUI({
    req(dataset())
    selectInput("threeway_outcome", "Select Outcome Variable",
      choices = names(dataset())[sapply(dataset(), is.numeric)]
    )
  })

  output$threeway_factor1_selector <- renderUI({
    req(dataset())
    selectInput("threeway_factor1", "Select First Factor",
      choices = names(dataset())[sapply(dataset(), is.factor)]
    )
  })

  output$threeway_factor2_selector <- renderUI({
    req(dataset(), input$threeway_factor1)
    choices <- names(dataset())[sapply(dataset(), is.factor)]
    choices <- choices[choices != input$threeway_factor1]
    selectInput("threeway_factor2", "Select Second Factor", choices = choices)
  })

  output$threeway_factor3_selector <- renderUI({
    req(dataset(), input$threeway_factor1, input$threeway_factor2)
    choices <- names(dataset())[sapply(dataset(), is.factor)]
    choices <- choices[!choices %in% c(input$threeway_factor1, input$threeway_factor2)]
    selectInput("threeway_factor3", "Select Third Factor", choices = choices)
  })

  # Update conditioning factor choices based on interaction view
  observe({
    req(input$threeway_factor1, input$threeway_factor2, input$threeway_factor3)

    conditioning_choices <- switch(input$interaction_view,
      "Factor1 x Factor2" = input$threeway_factor3,
      "Factor1 x Factor3" = input$threeway_factor2,
      "Factor2 x Factor3" = input$threeway_factor1
    )

    updateSelectInput(session, "conditioning_factor",
      choices = unique(dataset()[[conditioning_choices]])
    )
  })

  # Three-way ANOVA model
  threeway_anova_model <- eventReactive(input$run_threeway_anova, {
    req(
      input$threeway_outcome, input$threeway_factor1,
      input$threeway_factor2, input$threeway_factor3
    )

    formula <- as.formula(paste(
      input$threeway_outcome, "~",
      input$threeway_factor1, "*",
      input$threeway_factor2, "*",
      input$threeway_factor3
    ))

    aov_model <- aov(formula, data = dataset())
    return(aov_model)
  })

  # Three-way ANOVA outputs
  output$threeway_anova_summary <- renderPrint({
    req(threeway_anova_model())

    # Get the ANOVA table
    aov_table <- summary(threeway_anova_model())[[1]]

    # Format the table more compactly
    formatted_table <- data.frame(
      "Df" = aov_table[, "Df"],
      "Sum.Sq" = round(aov_table[, "Sum Sq"], 2),
      "Mean.Sq" = round(aov_table[, "Mean Sq"], 2),
      "F.value" = round(aov_table[, "F value"], 2),
      "P.value" = format.pval(aov_table[, "Pr(>F)"], digits = 2)
    )

    # Add significance stars
    rownames(formatted_table) <- rownames(aov_table)
    sig_level <- symnum(aov_table[, "Pr(>F)"],
      cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1),
      symbols = c("***", "**", "*", ".", " ")
    )

    formatted_table$Sig <- sig_level
    formatted_table$F.value[is.na(formatted_table$F.value)] <- ""
    formatted_table$P.value[is.na(formatted_table$P.value)] <- ""
    formatted_table$Sig[formatted_table$Sig == "?"] <- ""

    # Print with appropriate width
    options(width = 100) # Adjust this value based on your screen
    print(formatted_table)

    cat("\nSignif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1")
  })

  output$threeway_effect_sizes <- renderPrint({
    req(threeway_anova_model())
    eta_squared(threeway_anova_model(), partial = TRUE)
  })

  # Three-way interaction plot
  output$threeway_interaction_plot <- renderPlot({
    req(threeway_anova_model(), input$interaction_view, input$conditioning_factor)

    # Get the relevant factors based on selected view
    factors <- switch(input$interaction_view,
      "Factor1 x Factor2" = c(input$threeway_factor1, input$threeway_factor2, input$threeway_factor3),
      "Factor1 x Factor3" = c(input$threeway_factor1, input$threeway_factor3, input$threeway_factor2),
      "Factor2 x Factor3" = c(input$threeway_factor2, input$threeway_factor3, input$threeway_factor1)
    )

    # Filter data for selected conditioning factor level
    plot_data <- dataset()[dataset()[[factors[3]]] == input$conditioning_factor, ]

    ggplot(plot_data, aes_string(
      x = factors[1], y = input$threeway_outcome,
      color = factors[2], group = factors[2]
    )) +
      stat_summary(fun = mean, geom = "point", size = 3) +
      stat_summary(fun = mean, geom = "line") +
      stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.2) +
      theme_minimal() +
      labs(
        title = paste("Interaction Plot for", factors[1], "and", factors[2]),
        subtitle = paste("Conditioned on", factors[3], "=", input$conditioning_factor)
      )
  })

  # Three-way boxplots
  # 1-------------------------
  three_boxplot1 <- reactive({
    req(input$threeway_outcome, input$threeway_factor1)
    ggplot(
      dataset(),
      aes_string(
        x = input$threeway_factor1, y = input$threeway_outcome,
        fill = input$threeway_factor1
      )
    ) +
      geom_boxplot() +
      theme_minimal() +
      labs(title = paste("Boxplot of", input$threeway_outcome, "by", input$threeway_factor1))
  })

  output$threeway_boxplot1 <- renderPlot({
    three_boxplot1()
  })

  # 2-------------------------
  three_boxplot2 <- reactive({
    req(input$threeway_outcome, input$threeway_factor2)
    ggplot(
      dataset(),
      aes_string(
        x = input$threeway_factor2, y = input$threeway_outcome,
        fill = input$threeway_factor2
      )
    ) +
      geom_boxplot() +
      theme_minimal() +
      labs(title = paste("Boxplot of", input$threeway_outcome, "by", input$threeway_factor2))
  })

  output$threeway_boxplot2 <- renderPlot({
    three_boxplot2()
  })

  # 3-------------------------------
  three_boxplot3 <- reactive({
    req(input$threeway_outcome, input$threeway_factor3)
    ggplot(
      dataset(),
      aes_string(
        x = input$threeway_factor3, y = input$threeway_outcome,
        fill = input$threeway_factor3
      )
    ) +
      geom_boxplot() +
      theme_minimal() +
      labs(title = paste("Boxplot of", input$threeway_outcome, "by", input$threeway_factor3))
  })

  output$threeway_boxplot3 <- renderPlot({
    three_boxplot3()
  })

  # Three-way ANOVA diagnostics
  output$threeway_model_comparison <- renderPrint({
    req(threeway_anova_model())
    # Calculate R-squared
    aov_rsq <- summary.lm(threeway_anova_model())$r.squared

    cat("Three-way ANOVA Model Diagnostics:\n")
    cat("R-squared:", round(aov_rsq, 4), "\n")
    cat(
      "Adjusted R-squared:",
      round(summary.lm(threeway_anova_model())$adj.r.squared, 4), "\n"
    )
    cat("\nShapiro-Wilk test for normality of residuals:\n")
    print(shapiro.test(residuals(threeway_anova_model())))
  })

  # UI elements for three way anova
  output$three_aov_sum <- renderUI({
    req(threeway_anova_model())
    tagList(
      tags$p("Model summary", style = "color:#003399"),
      verbatimTextOutput("threeway_anova_summary"),
      br(),
      tags$p("Effect Sizes", style = "color:#003399"),
      verbatimTextOutput("threeway_effect_sizes"),
      br(), br(), br()
    )
  })

  output$three_aov_diag <- renderUI({
    req(threeway_anova_model())
    tagList(
      tags$p("Model diagnostics", style = "color:#003399"),
      verbatimTextOutput("threeway_model_comparison"),
      br(), br(), br()
    )
  })

  output$three_aov_boxplots <- renderUI({
    req(threeway_anova_model())
    tagList(
      tags$p("Plots without grouping", style = "color:#003399"),
      br(),
      fluidRow(
        column(1, ),
        column(
          5,
          plotOutput("threeway_boxplot1")
        ),
        column(
          5,
          plotOutput("threeway_boxplot2")
        )
      ),
      br(), br(),
      fluidRow(
        column(1, ),
        column(
          5,
          plotOutput("threeway_boxplot3")
        ),
        column(5, )
      ),
      br(), br(), br(), br(), br()
    )
  })

  #-----------------------------------------------------------------------------
  # Decision Tree UI
  output$tree_outcome_selector <- renderUI({
    req(dataset())
    selectInput("tree_outcome", "Select Outcome Variable",
      choices = names(dataset())[sapply(dataset(), is.numeric)]
    )
  })

  output$tree_factor1_selector <- renderUI({
    req(dataset())
    selectInput("tree_factor1", "Select First Factor",
      choices = names(dataset())[sapply(dataset(), is.factor)]
    )
  })

  output$tree_factor2_selector <- renderUI({
    req(dataset(), input$tree_factor1)
    choices <- names(dataset())[sapply(dataset(), is.factor)]
    choices <- choices[choices != input$tree_factor1]
    selectInput("tree_factor2", "Select Second Factor", choices = choices)
  })

  output$tree_factor3_selector <- renderUI({
    req(dataset(), input$tree_factor1, input$tree_factor2)
    choices <- names(dataset())[sapply(dataset(), is.factor)]
    choices <- choices[!choices %in% c(input$tree_factor1, input$tree_factor2)]
    selectInput("tree_factor3", "Select Third Factor", choices = choices)
  })

  output$tree_complexity_selector <- renderUI({
    req(dataset())
    radioButtons(
      "tree_complexity_option",
      "Show:",
      choices = list(
        "Model interactions" = "pvalue",
        "All possible interactions" = "zero",
        "Significant interactions" = "significant"
      ),
      selected = "pvalue" # Default selection
    )
  })

  # Tree model to handle three factors
  observe({
    # Update the checkbox when three-way model exists
    if (!is.null(threeway_anova_model())) {
      updateCheckboxInput(session, "use_three_factors", value = TRUE)
    }
  })

  tree_model <- eventReactive(input$run_tree, {
    req(input$tree_outcome, input$tree_factor1, input$tree_factor2)
    tree_data <- dataset()

    # Determine cp value based on radio button selection
    cp_value <- switch(input$tree_complexity_option,
      "pvalue" = {
        if (input$use_three_factors && !is.null(threeway_anova_model())) {
          summary(threeway_anova_model())[[1]][7, "Pr(>F)"] # Three-way interaction
        } else {
          summary(anova_model())[[1]][3, "Pr(>F)"] # Two-way interaction
        }
      },
      "zero" = 0,
      "significant" = {
        if (input$use_three_factors && !is.null(threeway_anova_model())) {
          pval <- summary(threeway_anova_model())[[1]][7, "Pr(>F)"]
        } else {
          pval <- summary(anova_model())[[1]][3, "Pr(>F)"]
        }
        if (pval < 0.05) {
          pval
        } else {
          showNotification(
            "No significant interactions found (p-value > 0.05). Showing no tree nor summary.",
            type = "warning",
            duration = 20
          )
          return(NULL)
        }
      }
    )

    # Check if cp_value is NULL (from "significant" case with no interactions)
    req(!is.null(cp_value))

    # Build formula based on number of factors
    if (input$use_three_factors) {
      req(input$tree_factor3)

      tree_formula <- as.formula(paste(
        input$tree_outcome, "~",
        input$tree_factor1, "+",
        input$tree_factor2, "+", input$tree_factor3
      ))
    } else {
      tree_formula <- as.formula(paste(
        input$tree_outcome, "~",
        input$tree_factor1, "+", input$tree_factor2
      ))
    }

    # Fit the tree model
    tree_model <- rpart(
      tree_formula,
      data = tree_data,
      method = "anova",
      control = rpart.control(cp = cp_value),
      model = TRUE
    )

    return(tree_model)
  })


  # Decision tree plot
  tree_plot <- reactive({
    req(tree_model())
    rpart.plot(tree_model(),
      main = "Decision Tree Visualisation",
      roundint = FALSE,
      fallen.leaves = TRUE,
      box.palette = "GnBu",
      branch.lty = 2,
      shadow.col = "gray"
      # nn = TRUE
    )
  })

  output$decision_tree_plot <- renderPlot({
    tree_plot()
  })

  output$tree_summary <- renderPrint({
    req(tree_model())
    printcp(tree_model())
  })

  # Variable importance plot
  var_imp_plot <- reactive({
    req(tree_model())

    # Check if there are any splits in the tree
    if (length(tree_model()$variable.importance) == 0) {
      # Create an empty plot with warning message
      ggplot() +
        annotate("text",
          x = 0.5, y = 0.5,
          label = "No variables were used in tree construction.\nThis suggests no significant interactions in the ANOVA model.",
          size = 5, hjust = 0.5
        ) +
        theme_void() +
        xlim(0, 1) +
        ylim(0, 1)
    } else {
      # Create the regular variable importance plot
      importance_data <- data.frame(
        variable = names(tree_model()$variable.importance),
        importance = tree_model()$variable.importance
      )

      ggplot(importance_data, aes(x = reorder(variable, importance), y = importance)) +
        geom_bar(stat = "identity", fill = "steelblue") +
        coord_flip() +
        theme_minimal() +
        labs(
          title = "Variable Importance in Decision Tree",
          x = "Variables",
          y = "Importance Score"
        )
    }
  })

  output$variable_importance_plot <- renderPlot({
    var_imp_plot()
  })

  # UI elements for decision tree
  # output$decision_tree <- renderUI({
  #   req(input$run_tree)
  #   tagList(
  #     plotOutput("decision_tree_plot", height = "600px"),
  #     tags$p("Tree Summary", style = "color:#003399"),
  #     verbatimTextOutput("tree_summary"),
  #     br(), br(), br(), br()
  #   )
  # })

  output$decision_tree <- renderUI({
    req(input$run_tree)
    tagList(
      # h3("Decision Tree Analysis", style = "color:#003399"),

      # Tree Plot Section
      plotOutput("decision_tree_plot", height = "600px"),

      # Interpretation Guide
      div(
        style = "background-color: #f5f5f5; padding: 15px; border-radius: 5px; margin: 20px 0;",
        tags$p("How to interpret the Decision Tree:", style = "color:#003399"),
        tags$ul(
          tags$li(
            "Each node contains two values:",
            tags$ul(
              tags$li("Top number: Average value for that group"),
              tags$li("Percentage: Portion of total data in that node")
            )
          ),
          tags$li("The tree splits (i.e., the interactions) based on decision rules"),
          tags$li("Follow paths from top to bottom to see decision sequences"),
          tags$li("Color coding (green vs blue) shows different decision pathways")
        )
      ),
      br(),
      # Tree Summary Section
      # Original Tree Summary Output
      tags$p("Technical Summary", style = "color:#003399"),
      verbatimTextOutput("tree_summary"),
      div(
        style = "background-color: #f5f5f5; padding: 15px; border-radius: 5px; margin: 20px 0;",
        tags$p("Understanding the Tree Summary:", style = "color:#003399"),
        tags$ul(
          tags$li("Key variables used for predictions"),
          tags$li("Root Node Error: Represents initial data variance"),
          tags$li(
            "Complexity Parameter (CP):",
            tags$ul(
              tags$li("Lower CP = more complex tree"),
              tags$li("Higher CP = simpler tree"),
              tags$li("Optimal CP balances complexity and accuracy")
            )
          ),
          tags$li("Xerror: Shows model performance at each split")
        )
      )
    )
  })

  # # Report Generation
  #############################################
  # output$download_report <- downloadHandler(
  #   filename = function() {
  #     paste("EXCITE_Analysis_Report",
  #       switch(input$report_format,
  #         "HTML" = ".html",
  #         "PDF" = ".pdf",
  #         "Word" = ".docx"
  #       ),
  #       sep = ""
  #     )
  #   },
  #   content = function(file) {
  #     # Temporary R Markdown file
  #     temp_report <- tempfile(fileext = ".Rmd")
  # 
  #     # Write R Markdown template based on selected report type
  #     report_content <- if (input$report_type == "Two-way ANOVA Report") {
  #       c(
  #         "---",
  #         "title: 'Two-way ANOVA Analysis Report'",
  #         "date: '`r Sys.Date()`'",
  #         "output:",
  #         "  html_document: default",
  #         "  pdf_document: default",
  #         "  word_document: default",
  #         "---",
  #         "",
  #         "## Dataset Overview",
  #         "",
  #         "### Structure of the Dataset",
  #         "```{r echo=FALSE}",
  #         "str(dataset())",
  #         "```",
  #         "",
  #         "### Summary of the Dataset",
  #         "```{r echo=FALSE}",
  #         "summary(dataset())",
  #         "```",
  #         "",
  #         "## Two-way ANOVA Results",
  #         "",
  #         "### Model Summary",
  #         "```{r model-summary, echo=FALSE}",
  #         "summary(anova_model())",
  #         "```",
  #         "",
  #         "### Effect Sizes",
  #         "```{r effect-sizes, echo=FALSE}",
  #         "eta_squared(anova_model(), partial = TRUE)",
  #         "```",
  #         "",
  #         "### Boxplots",
  #         "```{r boxplots, echo=FALSE, fig.width=10, fig.height=8}",
  #         "gridExtra::grid.arrange(fact1_boxplot(), fact2_boxplot(), fact12_boxplot(), fact21_boxplot(), ncol = 2, nrow = 2)",
  #         "```",
  #         "",
  #         "## Decision Tree Summary",
  #         "",
  #         "### Decision Tree Plot",
  #         "```{r echo=FALSE, fig.width=7, fig.height=5}",
  #         "rpart.plot(tree_model(), main = 'Decision Tree Visualisation', roundint = FALSE, fallen.leaves = TRUE, box.palette = 'GnBu', branch.lty = 2, shadow.col = 'gray')",
  #         "```",
  #         "",
  #         "### Tree Summary",
  #         "```{r echo=FALSE}",
  #         "printcp(tree_model())",
  #         "```",
  #         "",
  #         "### Variable Importance Plot",
  #         "```{r echo=FALSE, fig.width=5, fig.height=3}",
  #         "var_imp_plot()",
  #         "```"
  #       )
  #     } else {
  #       c(
  #         "---",
  #         "title: 'Three-way ANOVA Analysis Report'",
  #         "date: '`r Sys.Date()`'",
  #         "output:",
  #         "  html_document: default",
  #         "  pdf_document: default",
  #         "  word_document: default",
  #         "---",
  #         "",
  #         "## Dataset Overview",
  #         "",
  #         "### Structure of the Dataset",
  #         "```{r echo=FALSE}",
  #         "str(dataset())",
  #         "```",
  #         "",
  #         "## Three-way ANOVA Results",
  #         "",
  #         "### Model Summary",
  #         "```{r model-summary, echo=FALSE}",
  #         "summary(threeway_anova_model())",
  #         "```",
  #         "",
  #         "### Effect Sizes",
  #         "```{r effect-sizes, echo=FALSE}",
  #         "eta_squared(threeway_anova_model(), partial = TRUE)",
  #         "```",
  #         "",
  #         "### Boxplots",
  #         "```{r boxplots, echo=FALSE, fig.width=10, fig.height=8}",
  #         "gridExtra::grid.arrange(three_boxplot1(), three_boxplot2(), three_boxplot3(), NULL, ncol = 2, nrow = 2)",
  #         "```",
  #         "",
  #         "## Decision Tree Summary",
  #         "",
  #         "### Decision Tree Plot",
  #         "```{r echo=FALSE, fig.width=10, fig.height=7}",
  #         "rpart.plot(tree_model(), main = 'Decision Tree Visualisation', roundint = FALSE, fallen.leaves = TRUE, box.palette = 'GnBu', branch.lty = 2, shadow.col = 'gray')",
  #         "```",
  #         "",
  #         "### Tree Summary",
  #         "```{r echo=FALSE}",
  #         "printcp(tree_model())",
  #         "```",
  #         "",
  #         "### Variable Importance Plot",
  #         "```{r echo=FALSE, fig.width=5, fig.height=3}",
  #         "var_imp_plot()",
  #         "```"
  #       )
  #     }
  # 
  #     # Write the content to the temporary file
  #     writeLines(report_content, temp_report)
  # 
  #     # Render the report
  #     rmarkdown::render(
  #       temp_report,
  #       output_file = file,
  #       params = list(data = dataset())
  #     )
  #   }
  # )
  #############################################

  output$download_report <- downloadHandler(
    filename = function() {
      paste("EXCITE_Analysis_Report",
            switch(input$report_format,
                   "HTML" = ".html",
                   "PDF" = ".pdf",
                   "Word" = ".docx"
            ),
            sep = ""
      )
    },
    content = function(file) {
      # Temporary R Markdown file
      temp_report <- tempfile(fileext = ".Rmd")
      
      # Write R Markdown template based on selected report type
      report_content <- if (input$report_type == "Two-way ANOVA Report") {
        c(
          "---",
          "title: 'Two-way ANOVA Analysis Report'",
          "date: '`r Sys.Date()`'",
          "output:",
          "  html_document: default",
          "  pdf_document: default",
          "  word_document: default",
          "---",
          "",
          "## Dataset Overview",
          "",
          "Understanding the dataset is crucial before performing any statistical analysis. ",
          "The following sections provide an overview of the data structure and summary statistics.",
          "",
          "### Structure of the Dataset",
          "The dataset's structure reveals the types of variables present and their formats.",
          "```{r echo=FALSE}",
          "str(dataset())",
          "```",
          "",
          "### Summary of the Dataset",
          "A summary of the dataset provides basic descriptive statistics, such as means, standard deviations, and missing values.",
          "```{r echo=FALSE}",
          "summary(dataset())",
          "```",
          "",
          "## Two-way ANOVA Results",
          "",
          "A Two-Way ANOVA examines the effect of two categorical predictors on a continuous outcome variable. ",
          "This analysis identifies significant main effects and interaction effects.",
          "",
          "### Model Summary",
          "The model summary below provides the ANOVA table, including degrees of freedom, sum of squares, F-values, and p-values.",
          "```{r model-summary, echo=FALSE}",
          "summary(anova_model())",
          "```",
          "",
          "### Effect Sizes",
          "Effect sizes help determine the magnitude of observed differences. Here, partial eta squared values are presented.",
          "```{r effect-sizes, echo=FALSE}",
          "eta_squared(anova_model(), partial = TRUE)",
          "```",
          "",
          "### Boxplots",
          "Boxplots allow for visualizing differences in group means and variability across categories.",
          "```{r boxplots, echo=FALSE, fig.width=10, fig.height=8}",
          "gridExtra::grid.arrange(fact1_boxplot(), fact2_boxplot(), fact12_boxplot(), fact21_boxplot(), ncol = 2, nrow = 2)",
          "```",
          "",
          "## Decision Tree Summary",
          "",
          "Decision trees complement ANOVA by identifying interaction effects and hierarchical relationships in the data. ",
          "The following sections visualize the decision tree model and its key insights.",
          "",
          "### Decision Tree Plot",
          "The decision tree provides an intuitive visualization of how different predictors influence the outcome variable.",
          "```{r echo=FALSE, fig.width=7, fig.height=5}",
          "rpart.plot(tree_model(), main = 'Decision Tree Visualisation', roundint = FALSE, fallen.leaves = TRUE, box.palette = 'GnBu', branch.lty = 2, shadow.col = 'gray')",
          "```",
          "",
          "### Tree Summary",
          "A summary of the decision tree model, including complexity parameter values and pruning information.",
          "```{r echo=FALSE}",
          "printcp(tree_model())",
          "```",
          "",
          "### Variable Importance Plot",
          "The variable importance plot ranks predictors by their contribution to the decision tree model.",
          "```{r echo=FALSE, fig.width=5, fig.height=3}",
          "var_imp_plot()",
          "```"
        )
      } else {
        c(
          "---",
          "title: 'Three-way ANOVA Analysis Report'",
          "date: '`r Sys.Date()`'",
          "output:",
          "  html_document: default",
          "  pdf_document: default",
          "  word_document: default",
          "---",
          "",
          "## Dataset Overview",
          "",
          "Understanding the dataset is crucial before performing any statistical analysis. ",
          "The following sections provide an overview of the data structure and summary statistics.",
          "",
          "### Structure of the Dataset",
          "The dataset's structure reveals the types of variables present and their formats.",
          "```{r echo=FALSE}",
          "str(dataset())",
          "```",
          "",
          "## Three-way ANOVA Results",
          "",
          "A Three-Way ANOVA extends the Two-Way ANOVA by examining interactions among three categorical variables. ",
          "This helps assess whether the interaction of three factors significantly impacts the outcome variable.",
          "",
          "### Model Summary",
          "The model summary below provides the ANOVA table, including degrees of freedom, sum of squares, F-values, and p-values.",
          "```{r model-summary, echo=FALSE}",
          "summary(threeway_anova_model())",
          "```",
          "",
          "### Effect Sizes",
          "Effect sizes help determine the magnitude of observed differences. Here, partial eta squared values are presented.",
          "```{r effect-sizes, echo=FALSE}",
          "eta_squared(threeway_anova_model(), partial = TRUE)",
          "```",
          "",
          "### Boxplots",
          "Boxplots allow for visualizing differences in group means and variability across categories.",
          "```{r boxplots, echo=FALSE, fig.width=10, fig.height=8}",
          "gridExtra::grid.arrange(three_boxplot1(), three_boxplot2(), three_boxplot3(), NULL, ncol = 2, nrow = 2)",
          "```",
          "",
          "## Decision Tree Summary",
          "",
          "Decision trees complement ANOVA by identifying interaction effects and hierarchical relationships in the data. ",
          "The following sections visualize the decision tree model and its key insights.",
          "",
          "### Decision Tree Plot",
          "The decision tree provides an intuitive visualization of how different predictors influence the outcome variable.",
          "```{r echo=FALSE, fig.width=10, fig.height=7}",
          "rpart.plot(tree_model(), main = 'Decision Tree Visualisation', roundint = FALSE, fallen.leaves = TRUE, box.palette = 'GnBu', branch.lty = 2, shadow.col = 'gray')",
          "```",
          "",
          "### Tree Summary",
          "A summary of the decision tree model, including complexity parameter values and pruning information.",
          "```{r echo=FALSE}",
          "printcp(tree_model())",
          "```",
          "",
          "### Variable Importance Plot",
          "The variable importance plot ranks predictors by their contribution to the decision tree model.",
          "```{r echo=FALSE, fig.width=5, fig.height=3}",
          "var_imp_plot()",
          "```"
        )
      }
      
      # Write the content to the temporary file
      writeLines(report_content, temp_report)
      
      # Render the report
      rmarkdown::render(
        temp_report,
        output_file = file,
        params = list(data = dataset())
      )
    }
  )
  
  
}

#################################
# Run the Shiny app
shinyApp(ui, server)
