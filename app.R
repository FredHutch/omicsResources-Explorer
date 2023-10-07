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
      selectInput("language", "Language Used", choices = NULL),
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
}

# Run the application ----
shinyApp(ui = ui, server = server)
