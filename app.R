# Packages ----
library(shiny)
library(googlesheets4)
library(dplyr)

# Setup ----
googlesheets4::gs4_deauth()

# Define UI ----
ui <- fluidPage(
  # Stylesheet
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "hutch_theme.css")
  ),
  titlePanel(tagList(
    "Omics Resources Explorer",
    span(
      actionButton("raw_data",
                   label = "Data",
                   icon = icon("database"),
                   width = "77px",
                   onclick ="window.open(`https://docs.google.com/spreadsheets/d/1_4VN5MQVPO6KK14mH0P8zBz25s7a-he67qS6Fst6ZTo/edit?usp=sharing`, '_blank')"),
      actionButton("help",
                   label = "Help",
                   icon = icon("circle-exclamation"),
                   width = "77px",
                   onclick ="window.open(`https://github.com/FredHutch/omicsResources-Explorer#getting-help`, '_blank')"),
      actionButton("github",
                   label = "Code",
                   icon = icon("github"),
                   width = "77px",
                   onclick ="window.open(`https://github.com/FredHutch/omicsResources-Explorer`, '_blank')"),
      style = "position:absolute;right:2em;"
    )
  ),
  windowTitle = "Omics Resources Explorer (WIP)"),
  hr(),
  sidebarLayout(
    sidebarPanel(
      uiOutput("molecule_ui"),
      selectInput("name", "Name", choices = NULL),
      selectInput("technique", "Technique", choices = NULL),
      selectInput("id", "Identification", choices = NULL),
      selectInput("target", "Target", choices = NULL),
      selectInput("data_stage", "Data Stage", choices = NULL),
      selectInput("language_used", "Language Used", choices = NULL),
      selectInput("cloud_based", "Cloud Based", choices = NULL),
      selectInput("unique_tool_resource", "Unique Aspects of Tool or Resource", choices = NULL),
      br(),
      h5("Built with",
         img(src = "https://www.rstudio.com/wp-content/uploads/2014/04/shiny.png", height = "30px"),
         "by",
         img(src = "img/posit.jpeg", height = "30px")
      ),
      tags$img(src = "img/logo.png", width = "90%")
    ),
    mainPanel(
      uiOutput("link")
    )
  )
)


# Define server ----
server <- function(input, output, session) {
  # Reactive Dataset
  omics_resources <- reactiveFileReader(1000,
                                        session,
                                        "https://docs.google.com/spreadsheets/d/1_4VN5MQVPO6KK14mH0P8zBz25s7a-he67qS6Fst6ZTo/edit?usp=sharing",
                                        googlesheets4::read_sheet,
                                        sheet = "main")

  output$molecule_ui <- renderUI({
    selectInput("molecule", "Molecule", choices = unique(omics_resources()$molecule))
  })

  molecule <- reactive({
    req(input$molecule)
    filter(omics_resources(), molecule == input$molecule)
  })
  observeEvent(molecule(), {
    # ensures that any reactives or outputs that use the input won’t be updated
    freezeReactiveValue(input, "name")
    choices <- unique(molecule()$name)
    updateSelectInput(inputId = "name", choices = choices)
  })

  name <- reactive({
    req(input$name)
    filter(molecule(), name == input$name)
  })
  observeEvent(name(), {
    freezeReactiveValue(input, "technique")
    choices <- unique(name()$technique)
    updateSelectInput(inputId = "technique", choices = choices)
  })

  technique <- reactive({
    req(input$technique)
    filter(name(), technique == input$technique)
  })
  observeEvent(technique(), {
    freezeReactiveValue(input, "id")
    choices <- unique(technique()$identification)
    updateSelectInput(inputId = "id", choices = choices)
  })

  identification <- reactive({
    req(input$id)
    filter(technique(), identification == input$id)
  })
  observeEvent(identification(), {
    freezeReactiveValue(input, "target")
    choices <- unique(identification()$target)
    updateSelectInput(inputId = "target", choices = choices)
  })

  target <- reactive({
    req(input$target)
    filter(identification(), target == input$target)
  })
  observeEvent(target(), {
    freezeReactiveValue(input, "data_stage")
    choices <- unique(target()$data_stage)
    updateSelectInput(inputId = "data_stage", choices = choices)
  })

  data_stage <- reactive({
    req(input$data_stage)
    filter(target(), data_stage == input$data_stage)
  })
  observeEvent(data_stage(), {
    freezeReactiveValue(input, "language_used")
    choices <- unique(data_stage()$language_used)
    updateSelectInput(inputId = "language_used", choices = choices)
  })

  language_used <- reactive({
    req(input$language_used)
    filter(data_stage(), language_used == input$language_used)
  })
  observeEvent(language_used(), {
    freezeReactiveValue(input, "cloud_based")
    choices <- unique(language_used()$cloud_based)
    updateSelectInput(inputId = "cloud_based", choices = choices)
  })

  cloud_based <- reactive({
    req(input$cloud_based)
    filter(language_used(), cloud_based == input$cloud_based)
  })
  observeEvent(cloud_based(), {
    freezeReactiveValue(input, "unique_tool_resource")
    choices <- unique(cloud_based()$what_makes_this_tool_or_resource_unique)
    updateSelectInput(inputId = "unique_tool_resource", choices = choices)
  })

  # Final output
  output$link <- renderUI({
    req(input$unique_tool_resource)
    link <- cloud_based() %>%
      filter(what_makes_this_tool_or_resource_unique == input$unique_tool_resource) %>%
      pull(tutorials_and_tool_links)

    a(link, href = link, target = "_blank")
  })
}

# Run the application ----
shinyApp(ui = ui, server = server)
