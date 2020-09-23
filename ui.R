## ui.R
library(shiny)
library(shinydashboard)
library(recommenderlab)
library(data.table)
library(ShinyRatingInput)
library(shinyjs)

source('functions/UI_intro.R')

shinyUI(
    dashboardPage(
          skin = "blue",
          dashboardHeader(title = "Reading Recommender"),
          
          dashboardSidebar(disable = TRUE),

          dashboardBody(includeCSS("css/b.css"),
              fluidRow(
                  box(width = 12, title = "To see your recommendation: Rate as many books as possible", status = "info", solidHeader = TRUE, collapsible = TRUE,
                      div(class = "rateitems",
                          uiOutput('ratings')
                      )
                  )
                ),
              fluidRow(
                  useShinyjs(),
                  box(
                    width = 12, status = "info", solidHeader = TRUE,
                    title = "Step 2: Discover books you might like",
                    br(),
                    withBusyIndicatorUI(
                      actionButton("btn", "Click the button to get your recommendations", class = "btn-warning")
                    ),
                    br(),
                    tableOutput("results")
                  )
               )
          )
    )
) 