# Packages ----
library(shiny)
library(googlesheets4)
library(dplyr)

# Data ----
omics_resources <- read_sheet("https://docs.google.com/spreadsheets/d/1_4VN5MQVPO6KK14mH0P8zBz25s7a-he67qS6Fst6ZTo/edit?usp=sharing",
           sheet = "main")

# Identify unique values in columns:
# 'molecule', 'name', 'technique', 'identification', 'target',
# 'data_stage', 'language_used', 'cloud_based', 'what_makes_this_tool_or_resource_unique',

# Show these unique values in 'selectInput()'

# Final output: values in 'tutorials_and_tool_links'
ui <- fluidPage(
  titlePanel("Omics Resources Explorer (WIP)"),
  sidebarLayout(
    sidebarPanel(
      selectInput("molecule", "Molecule", choices = unique(omics_resources$molecule)),
      selectInput("name", "Name", choices = NULL),
      selectInput("technique", "Technique", choices = NULL),
      selectInput("id", "Identification", choices = NULL),
      selectInput("target", "Target", choices = NULL),
      selectInput("data_stage", "Data Stage", choices = NULL),
      selectInput("language_used", "Language Used", choices = NULL),
      selectInput("cloud_based", "Cloud Based", choices = NULL),
      selectInput("unique_tool_resource", "Unique Aspects of Tool or Resource", choices = NULL)
    ),
    mainPanel(
      uiOutput("customer"),
      tableOutput("data")
    )
  )
)


# Define server ----
server <- function(input, output) {
  molecule <- reactive({
    filter(omics_resources, molecule == input$molecule)
  })
  observeEvent(molecule(), {
    choices <- unique(molecule()$name)
    updateSelectInput(inputId = "name", choices = choices)
  })

  name <- reactive({
    req(input$name)
    filter(molecule(), name == input$name)
  })
  observeEvent(name(), {
    choices <- unique(name()$technique)
    updateSelectInput(inputId = "technique", choices = choices)
  })

  technique <- reactive({
    req(input$technique)
    filter(name(), technique == input$technique)
  })
  observeEvent(technique(), {
    choices <- unique(technique()$identification)
    updateSelectInput(inputId = "id", choices = choices)
  })

  identification <- reactive({
    req(input$id)
    filter(technique(), identification == input$id)
  })
  observeEvent(identification(), {
    choices <- unique(identification()$target)
    updateSelectInput(inputId = "target", choices = choices)
  })

  target <- reactive({
    req(input$target)
    filter(identification(), target == input$target)
  })
  observeEvent(target(), {
    choices <- unique(target()$data_stage)
    updateSelectInput(inputId = "data_stage", choices = choices)
  })

  data_stage <- reactive({
    req(input$data_stage)
    filter(target(), data_stage == input$data_stage)
  })
  observeEvent(data_stage(), {
    choices <- unique(data_stage()$language_used)
    updateSelectInput(inputId = "language_used", choices = choices)
  })

  language_used <- reactive({
    req(input$language_used)
    filter(data_stage(), language_used == input$language_used)
  })
  observeEvent(language_used(), {
    choices <- unique(language_used()$cloud_based)
    updateSelectInput(inputId = "cloud_based", choices = choices)
  })

  cloud_based <- reactive({
    req(input$cloud_based)
    filter(language_used(), cloud_based == input$cloud_based)
  })
  observeEvent(cloud_based(), {
    choices <- unique(cloud_based()$what_makes_this_tool_or_resource_unique)
    updateSelectInput(inputId = "unique_tool_resource", choices = choices)
  })
}

# Run the application ----
shinyApp(ui = ui, server = server)
