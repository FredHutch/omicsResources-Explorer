# Packages ----
library(shiny)
library(shinyglide)
library(shinyWidgets)
library(shinyglide)
library(googlesheets4)
library(dplyr)


ui <- fluidPage(style = "max-width: 500px;",
                shinyWidgets::setBackgroundImage("background.jpg"),
                titlePanel("Omics Resources Explorer", windowTitle = "Omics Resources Explorer (WIP)"),
                br(),
                glide(
                  height = "350px",
                  screen(
                    p("Omics Resources Explorer is an open source web application that offers suggestions for genomics tutorials and
                     tools based on your specific search criteria, including molecules, techniques, and programming languages."),
                    p("As you make a selection for the molecule category, it will display the relevant choices in the subsequent selection boxes. You can then proceed to choose the name, followed by the technique, identification, and so on."),
                    p("Please click on Next to go to the next screen.")
                  ),
                  screen(
                    h3("Question 1"),
                    uiOutput("molecule_ui")
                  ),
                  screen(
                    h3("Question 2"),
                    selectInput("technique", "Which technique is used?", choices = NULL)
                  ),
                  screen(
                    h3("Question 3"),
                    selectInput("molecule_aspect", "Which molecular aspect was targeted?", choices = NULL)
                  ),
                  screen(
                    h3("Question 4"),
                    selectInput("specialty", "Which specialty target are you analyzing?", choices = NULL)
                  ),
                  screen(
                    h3("Question 5"),
                    selectInput("date_stage", "Which data stage or info are you looking for?", choices = NULL)
                  )
                )
)


server <- function(input, output, session) {
  omics_resources <- reactive({
    readxl::read_xlsx("raw_data.xlsx",
                      sheet = "main",
                      col_names = c("molecule", "technique", "molecule_aspect", "specialty_target",
                                    "date_stage", "programming_language", "cloud", "description",
                                    "tutorials_and_tool_links"),
                      skip = 1)
  })

  output$molecule_ui <- renderUI({
    selectInput("molecule", "Which molecule was analyzed in the data?", choices = unique(omics_resources()$molecule))
  })
  molecule <- reactive({
    req(input$molecule)
    filter(omics_resources(), molecule == input$molecule)
  })
  observeEvent(molecule(), {
    freezeReactiveValue(input, "technique")
    choices <- unique(molecule()$technique)
    updateSelectInput(inputId = "technique", choices = choices)
  })

  technique <- reactive({
    req(input$technique)
    filter(molecule(), technique == input$technique)
  })
  observeEvent(technique(), {
    freezeReactiveValue(input, "molecule_aspect")
    choices <- unique(technique()$molecule_aspect)
    updateSelectInput(inputId = "molecule_aspect", choices = choices)
  })

}

shinyApp(ui, server)

# Setup ----
# googlesheets4::gs4_deauth()
#
# controls <- tags$div(
#   tags$div(class="my-control prev-screen"),
#   tags$div(class="my-control next-screen"),
#   div(`data-glide-el`="controls",
#       tags$a(
#         class="my-control last-screen",
#         `data-glide-dir` = "<<",
#         icon("repeat", lib = "glyphicon")
#       )
#   )
# )
#
# css <- "
# .container-fluid {
#   max-width: 1400px;
#   padding: 0 20px;
# }
# .glide__slides {
#   margin: 0 2em;
# }
# "
#
# # Define UI ----
# ui <- fluidPage(
#   tags$head(
#     tags$style(css)
#   ),
#   # # Add background image
#   shinyWidgets::setBackgroundImage("background.jpg"),
#   # # Stylesheet
#   # tags$head(
#   #   tags$link(rel = "stylesheet", type = "text/css", href = "i/hutch_theme.css")
#   # ),
#   # Favicon
#   # tags$head(tags$link(rel="shortcut icon", href="i/img/favicon.ico")),
#   fluidRow(
#     column(4, offset = 4, titlePanel("Omics Resources Explorer", windowTitle = "Omics Resources Explorer (WIP)"))
#   ),
#   br(),
#   br(),
#   fluidRow(
#     column(4, offset = 4,
#            shinyglide::glide(
#              next_label = icon("chevron-right", lib="glyphicon"),
#              previous_label = icon("chevron-left", lib="glyphicon"),
#              shinyglide::screen(
#                uiOutput("molecule_ui")
#              ),
#              shinyglide::screen(
#                selectizeInput("technique", "What technique was analyzed for this dataset?",
#                             choices = NULL)
#              )
#            )
#     )
#   )
# )
#
#
#
#
#
#
#
#
#
# #  sidebarLayout(
# #   sidebarPanel(
# #     uiOutput("molecule_ui"),
# #     selectInput("name", "Name", choices = NULL),
# #     selectInput("technique", "Technique", choices = NULL),
# #     selectInput("id", "Identification", choices = NULL),
# #     selectInput("target", "Target", choices = NULL),
# #     selectInput("data_stage", "Data Stage", choices = NULL),
# #     selectInput("language_used", "Language Used", choices = NULL),
# #     selectInput("cloud_based", "Cloud Based", choices = NULL),
# #     selectInput("unique_tool_resource", "Unique Aspects of Tool or Resource", choices = NULL),
# #     br(),
# #     h5("Built with",
# #        img(src = "https://www.rstudio.com/wp-content/uploads/2014/04/shiny.png", height = "30px"),
# #        "by",
# #        img(src = "i/img/posit.jpeg", height = "30px")
# #     ),
# #     tags$img(src = "i/img/logo.png", width = "90%")
# #   ),
# #   mainPanel(
# #     uiOutput("link")
# #   )
# # )
#
#
#
# # Define server ----
# server <- function(input, output, session) {
#   # Reactive Dataset
#   # omics_resources <- reactiveFileReader(1000,
#   #                                       session,
#   #                                       "https://docs.google.com/spreadsheets/d/1_4VN5MQVPO6KK14mH0P8zBz25s7a-he67qS6Fst6ZTo/edit?usp=sharing",
#   #                                       googlesheets4::read_sheet,
#   #                                       sheet = "main",
#   #                                       col_names = c("molecule",
#   #                                                     "technique",
#   #                                                     "molecule_aspect",
#   #                                                     "specialty_target",
#   #                                                     "date_stage",
#   #                                                     "programming_language",
#   #                                                     "cloud",
#   #                                                     "description",
#   #                                                     "tutorials_and_tool_links"),
#   #                                       skip = 1)
#
#   omics_resources <- reactive({
#     readxl::read_xlsx("raw_data.xlsx",
#                       sheet = "main",
#                       col_names = c("molecule",
#                                     "technique",
#                                     "molecule_aspect",
#                                     "specialty_target",
#                                     "date_stage",
#                                     "programming_language",
#                                     "cloud",
#                                     "description",
#                                     "tutorials_and_tool_links"),
#                       skip = 1)
#
#   })
#
#   output$molecule_ui <- renderUI({
#     shiny::selectInput("n", "Label", choices = c("Test1", "Test2"))
#   })
#   molecule <- reactive({
#     req(input$molecule)
#     filter(omics_resources(), molecule == input$molecule)
#   })
#   observeEvent(molecule(), {
#     # ensures that any reactives or outputs that use the input won’t be updated
#     freezeReactiveValue(input, "technique")
#     choices <- unique(molecule()$technique)
#     updateRadioButtons(inputId = "technique", choices = choices, inline = TRUE)
#   })
# }
#
#
#
# #
# #   name <- reactive({
# #     req(input$name)
# #     filter(molecule(), name == input$name)
# #   })
# #   observeEvent(name(), {
# #     freezeReactiveValue(input, "technique")
# #     choices <- unique(name()$technique)
# #     updateSelectInput(inputId = "technique", choices = choices)
# #   })
# #
# #   technique <- reactive({
# #     req(input$technique)
# #     filter(name(), technique == input$technique)
# #   })
# #   observeEvent(technique(), {
# #     freezeReactiveValue(input, "id")
# #     choices <- unique(technique()$identification)
# #     updateSelectInput(inputId = "id", choices = choices)
# #   })
# #
# #   identification <- reactive({
# #     req(input$id)
# #     filter(technique(), identification == input$id)
# #   })
# #   observeEvent(identification(), {
# #     freezeReactiveValue(input, "target")
# #     choices <- unique(identification()$target)
# #     updateSelectInput(inputId = "target", choices = choices)
# #   })
# #
# #   target <- reactive({
# #     req(input$target)
# #     filter(identification(), target == input$target)
# #   })
# #   observeEvent(target(), {
# #     freezeReactiveValue(input, "data_stage")
# #     choices <- unique(target()$data_stage)
# #     updateSelectInput(inputId = "data_stage", choices = choices)
# #   })
# #
# #   data_stage <- reactive({
# #     req(input$data_stage)
# #     filter(target(), data_stage == input$data_stage)
# #   })
# #   observeEvent(data_stage(), {
# #     freezeReactiveValue(input, "language_used")
# #     choices <- unique(data_stage()$language_used)
# #     updateSelectInput(inputId = "language_used", choices = choices)
# #   })
# #
# #   language_used <- reactive({
# #     req(input$language_used)
# #     filter(data_stage(), language_used == input$language_used)
# #   })
# #   observeEvent(language_used(), {
# #     freezeReactiveValue(input, "cloud_based")
# #     choices <- unique(language_used()$cloud_based)
# #     updateSelectInput(inputId = "cloud_based", choices = choices)
# #   })
# #
# #   cloud_based <- reactive({
# #     req(input$cloud_based)
# #     filter(language_used(), cloud_based == input$cloud_based)
# #   })
# #   observeEvent(cloud_based(), {
# #     freezeReactiveValue(input, "unique_tool_resource")
# #     choices <- unique(cloud_based()$what_makes_this_tool_or_resource_unique)
# #     updateSelectInput(inputId = "unique_tool_resource", choices = choices)
# #   })
# #
# #   # Final output
# #   output$link <- renderUI({
# #     req(input$unique_tool_resource)
# #     link <- cloud_based() %>%
# #       filter(what_makes_this_tool_or_resource_unique == input$unique_tool_resource) %>%
# #       pull(tutorials_and_tool_links)
# #
# #     a(link, href = link, target = "_blank")
# #   })
#
#
# # Run the application ----
# addResourcePath("/i", file.path(getwd(), "www"))
# options <- list()
# if (!interactive()) {
#   options$port <- 3838
#   options$launch.browser <- FALSE
#   options$host <- "0.0.0.0"
#
# }
# shinyApp(ui = ui, server = server, options = options)
