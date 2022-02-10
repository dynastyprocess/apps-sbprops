suppressPackageStartupMessages({

  # Data import
  library(arrow)

  # Data manipulation
  library(tidyverse)
  library(glue)

  # Shiny
  library(shiny)
  library(bs4Dash)
  library(shinyWidgets)
  library(DT)
  library(waiter)
  library(sever)
  library(tantastic)

  # Data output
  library(writexl)
  options(dplyr.summarise.inform = FALSE)
})

#### UI FUNCTIONS ####

ui_header <- function(title, ...) {
  bs4Dash::dashboardHeader(
    skin = "dark",
    fixed = TRUE,
    border = TRUE,
    # compact = TRUE,
    shiny::span(title, style = "font-size:1.5em;color:#ffffff"),
    ...
  )
}

ui_sidebar <- function(...) {
  bs4Dash::dashboardSidebar(
    title = "Apps",
    fixed = TRUE,
    skin = "dark",
    elevation = 3,
    collapsed = TRUE,
    opacity = 0.8,
    url = "https://dynastyprocess.com",
    expand_on_hover = TRUE,
    src = "https://avatars2.githubusercontent.com/u/63691873?s=400&u=d9289a2540799f19ca6d8ad866e219ee2d602ba9&v=4",
    bs4Dash::sidebarMenu(...)
  )
}

external_menuItem <- function(text = NULL, href = NULL, icon = NULL) {
  tags$li(
    tags$a(span(icon(icon), style = "font-size:1.1rem;"),
           p(text, style = "margin-left: .5rem;"),
           class = "nav-link", href = href
    ), class = "nav-item")
}


sever_dp <- function(){

  sever::sever(
    shiny::tagList(
      shiny::h1("Disconnected"),
      shiny::br(),
      shiny::p(shiny::em(joker::dadjoke())),
      shiny::br(),
      shiny::tags$button(
        "Reload",
        style = "color:#000;background-color:#fff;",
        class = "button button-raised",
        onClick = "location.reload();"
      )
    ),
    bg_color = "#000"
  )
}
