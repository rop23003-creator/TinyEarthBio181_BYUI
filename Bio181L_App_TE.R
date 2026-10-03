# ============================================================
# app.R
#
# PURPOSE
#   A minimal Shiny app that lets you browse every cleaned Tiny
#   Earth table in one place, with related sample/institution
#   context automatically attached for readability. This is the
#   DISPLAY half of the project -- letting the user pick columns
#   to run statistical tests on comes later, as a separate,
#   additional piece built on top of this.
#
# BEFORE YOU RUN THIS
#   install.packages(c("shiny", "DT"))
#   This file expects 01_clean_data.R and the Data_Raw/ folder to
#   be in the same project (it re-uses that script directly --
#   see the note by source() below).
#
# HOW TO RUN
#   Open this file in RStudio and click "Run App" in the top-right
#   of the editor pane (RStudio recognizes this as a Shiny app
#   automatically because it ends with a shinyApp() call).
# ============================================================

library(shiny)
library(DT)

# This line re-runs 01_clean_data.R from scratch: reading all 14
# raw files, cleaning them, and defining join_with_sample(),
# join_with_institution(), and build_display_table() -- everything
# this app needs ends up in memory as a side effect of sourcing it.
# Our dataset is small, so re-cleaning on every app launch is fast
# (under a couple seconds) and guarantees the app never shows stale
# data. If that ever starts to feel slow once more data is added,
# the fix is to swap this line for:
#   tep_data <- <read each table from the Data/ folder's .csv files>
# -- but you'd still need the three functions defined somewhere the
# app can see them, so we're keeping it this way for now.
source("Data_Clean.R")

# ------------------------------------------------------------
# UI -- one dropdown to choose a table, one table to show it
# ------------------------------------------------------------
ui <- fluidPage(
  titlePanel("Tiny Earth Data Browser"),
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "table_choice",
        label   = "Choose a table to view:",
        choices = names(tep_data)
      )
    ),
    mainPanel(
      DTOutput("data_table")
    )
  )
)

# ------------------------------------------------------------
# Server -- build_display_table() (from 01_clean_data.R) does the
# actual work of attaching sample/institution context; the server
# just reacts to the dropdown and renders whatever it returns.
# ------------------------------------------------------------
server <- function(input, output, session) {
  output$data_table <- renderDT({
    build_display_table(input$table_choice)
  }, options = list(pageLength = 15, scrollX = TRUE))
}

# Combines ui + server into a runnable app. RStudio looks for this
# exact call to recognize the file as a Shiny app.
shinyApp(ui = ui, server = server)
