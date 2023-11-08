# Packages ----
library(shiny)
library(shinyglide)
library(shinyWidgets)
library(shinyglide)
library(googlesheets4)
library(readxl)
library(dplyr)

# Setup ----
googlesheets4::gs4_deauth()

css <-
"
/* Omics Resource Explorer */
h2 {
  font-family: Times;
  font-size: 38px;
  color: #1c3b61;
  font-weight: bold;
}

/* Question 1, 2, etc */
h3 {
  font-family: Times;
  font-size: 28px;
  color: #1c3b61;
  font-weight: bold;
}

/* Paragraph */
p {
 font-family: Times;
 font-size: 17px;
}

/* selectInput Label */
.control-label {
  font-family: Arial;
  color: #1c3b61;
}

/* Next, Back buttons */
.my-control {
  display: block;
  position: absolute;
  line-height: 1;
  font-size: 1.5em;
  color: #1c3b61;
  opacity: 0.8;
  cursor: pointer;
}
.prev-screen {
  left: 20px;
}
.next-screen,
.last-screen {
  right: 10px;
}
"

controls <- tags$div(
  tags$div(class="my-control prev-screen"),
  tags$div(class="my-control next-screen"),
  div(`data-glide-el`="controls",
      tags$a(
        class="my-control last-screen",
        `data-glide-dir` = "<<",
        icon("repeat", lib = "glyphicon")
      )
  )
)


# Shiny App ----
ui <- fluidPage(style = "max-width: 500px;",
                # background image
                shinyWidgets::setBackgroundImage("/i/background.jpg"),
                # css
                tags$head(
                  tags$style(css),
                  tags$head(tags$link(rel="shortcut icon", href="/i/img/favicon.ico"))
                ),
                # title
                titlePanel("Omics Resource Explorer", windowTitle = "Omics Resource Explorer"),
                br(),
                glide(
                  height = "350px",
                  custom_controls = controls,
                  screen(
                    p("Omics Resources Explorer is an open source web application that offers suggestions for genomics tutorials and
                     tools based on your specific search criteria, including molecules, techniques, and programming languages.
                      It utilizes a publicly accessible ", a("Google Sheet", href = "https://docs.google.com/spreadsheets/d/1_4VN5MQVPO6KK14mH0P8zBz25s7a-he67qS6Fst6ZTo/edit?usp=sharing", target = "_blank"),
                      "to populate the selection boxes with relevant values and show the appropriate link to a tutorial or tool.", .noWS = "outside"),
                    p("It employs a ", a("hierarchical selection box approach", href = "https://mastering-shiny.org/action-dynamic.html?q=hier#hierarchical-select", .noWS = "outside"),
                      " to dynamically populate the selection boxes based on your prior choices. As you make a selection for the molecule
                      category, it will display the relevant choices in the subsequent selection boxes.
                      You can then proceed to choose the technique, followed by the molecular aspect, specialty target, and so on."),

                    p("Please click on Next to go to the next screen.")
                  ),
                  screen(
                    h3("Question 1"),
                    uiOutput("molecule_ui")
                  ),
                  screen(
                    h3("Question 2"),
                    selectInput("technique", "What technique is used?", choices = NULL)
                  ),
                  screen(
                    h3("Question 3"),
                    selectInput("molecule_aspect", "Which molecular aspect was targeted for this data?", choices = NULL)
                  ),
                  screen(
                    h3("Question 4"),
                    selectInput("specialty_target", "Which specialty target are you analyzing?", choices = NULL)
                  ),
                  screen(
                    h3("Question 5"),
                    selectInput("data_stage", "Which data stage or info are you looking for?", choices = NULL)
                  ),
                  screen(
                    h3("Question 6"),
                    selectInput("programming_language", "What programming language do you prefer?", choices = NULL)
                  ),
                  screen(
                    h3("Question 7"),
                    selectInput("cloud", "Do you need a cloud based tool?", choices = NULL)
                  ),
                  screen(
                    h3("Links to Tutorials/Tools:")
                    # htmlOutput("tutorial_and_tool_link")
                  )
                )
)


server <- function(input, output, session) {
  omics_resources <- reactiveFileReader(1000, session,
                                        "https://docs.google.com/spreadsheets/d/1_4VN5MQVPO6KK14mH0P8zBz25s7a-he67qS6Fst6ZTo/edit?usp=sharing",
                                        googlesheets4::read_sheet,
                                        sheet = "main",
                                        col_names = c("molecule", "technique", "molecule_aspect", "specialty_target",
                                                      "data_stage", "programming_language", "cloud", "description", "tutorial_and_tool_link",
                                                      "selector"),
                                        skip = 1)

  # omics_resources <- reactive({
  #   read_excel("raw_data.xlsx", sheet = "main",
  #              col_names = c("molecule", "technique", "molecule_aspect", "specialty_target",
  #                            "data_stage", "programming_language", "cloud", "description",
  #                            "tutorial_and_tool_link"),
  #              skip = 1)
  # })

  # molecule
  output$molecule_ui <- renderUI({
    selectInput("molecule", "What molecule was analyzed in the data?",
                choices = unique(omics_resources()$molecule))
  })
  molecule <- reactive({
    req(input$molecule)
    filter(omics_resources(), molecule == input$molecule)
  })

  # technique
  observeEvent(molecule(), {
    freezeReactiveValue(input, "technique")
    choices <- unique(molecule()$technique)
    updateSelectInput(inputId = "technique", choices = choices)
  })
  technique <- reactive({
    req(input$technique)
    filter(molecule(), technique == input$technique)
  })

  # molecule aspect
  observeEvent(technique(), {
    freezeReactiveValue(input, "molecule_aspect")
    choices <- unique(technique()$molecule_aspect)
    updateSelectInput(inputId = "molecule_aspect", choices = choices)
  })
  molecule_aspect <- reactive({
    req(input$molecule_aspect)
    filter(technique(), molecule_aspect == input$molecule_aspect)
  })

  # specialty target
  observeEvent(molecule_aspect(), {
    freezeReactiveValue(input, "specialty_target")
    choices <- unique(molecule_aspect()$specialty_target)
    updateSelectInput(inputId = "specialty_target", choices = choices)
  })
  specialty_target <- reactive({
    req(input$specialty_target)
    filter(molecule_aspect(), specialty_target == input$specialty_target)
  })

  # data stage
  observeEvent(specialty_target(), {
    freezeReactiveValue(input, "data_stage")
    choices <- unique(specialty_target()$data_stage)
    updateSelectInput(inputId = "data_stage", choices = choices)
  })
  data_stage <- reactive({
    req(input$data_stage)
    filter(specialty_target(), data_stage == input$data_stage)
  })

  # programming language
  observeEvent(data_stage(), {
    freezeReactiveValue(input, "programming_language")
    choices <- unique(data_stage()$programming_language)
    updateSelectInput(inputId = "programming_language", choices = choices)
  })
  programming_language <- reactive({
    req(input$programming_language)
    filter(data_stage(), programming_language == input$programming_language)
  })

  # cloud
  observeEvent(programming_language(), {
    freezeReactiveValue(input, "cloud")
    choices <- unique(programming_language()$cloud)
    updateSelectInput(inputId = "cloud", choices = choices)
  })
  cloud <- reactive({
    req(input$cloud)
    filter(programming_language(), cloud == input$cloud)
  })


  output$tutorial_and_tool_link <- renderUI({
    tags$iframe(src = "https://informatics.fas.harvard.edu/atac-seq-guidelines.html", height=300, width=300)
  })


}


# Run the application ----
addResourcePath("/i", file.path(getwd(), "www"))
options <- list()
if (!interactive()) {
  options$port <- 3838
  options$launch.browser <- FALSE
  options$host <- "0.0.0.0"

}

shinyApp(ui, server, options=options)
