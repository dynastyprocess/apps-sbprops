options(dplyr.summarise.inform = FALSE)
pkgload::load_all()

df_questions <- read_csv("matspropgame.csv") |>
  mutate(index = row_number()) |>
  pivot_longer(c(-question,-index), names_to = NULL, values_to = "choices") |>
  filter(!is.na(choices)) |>
  mutate(choices = str_squish(choices) |> str_remove_all("\\)$") |> paste0(")")) |>
  extract(choices,remove = FALSE,regex = "\\(([0-9]+)",into = "score", convert = TRUE)

question_select_table <- df_questions |>
  group_by(index,question) |>
  summarise(
    choices = list(c("Select", choices)),
    scores = list(score)
  ) |>
  ungroup() |>
  mutate(
    Select = map2_chr(paste0("question_",row_number()), choices, ~selectInput(.x,label = NULL, choices = .y) |> as.character())
  ) |>
  select(Question = question, Select)

ui <- dashboardPage(
  sidebar_collapsed = TRUE,
  title = "TDM Super Bowl Props Contest",
  navbar = ui_header("2022 TDM Super Bowl Props Contest"),
  sidebar = ui_sidebar(
    menuItem("Contest Entry", tabName = "contest", icon = "hat-wizard"),
    external_menuItem("More by DynastyProcess", "https://dynastyprocess.com")
  ),
  body = dashboardBody(
    includeCSS("dp.css"),
    useWaiter(),
    waiterOnBusy(html = spin_dots(), color = transparent(0.3)),
    use_sever(),
    tabItems(
      tabItem(
        tabName = "contest",
        box(
          width = 12,
          inputId = "box-inputs",
          status = "gray-dark",
          title = "ABOUT",
          closable = FALSE,
          includeMarkdown("about.md"),
          hr(),
          fluidRow(
            column(4, textInput("nickname", label = "Nickname (Discord/Twitter)")),
            column(4, textInput("email", label = "Email")),
          )
        ),
        box(
          width = 12,
          status = "gray-dark",
          closable = FALSE,
          title = "SELECTIONS",
          DTOutput("contest_table"),
          footer = div(actionButton("review_entry",label = "Review your entry!", class = "btn-success", status= "success"),
                       style = "text-align:center;")
        )
      )
    )
  )
)

server <- function(input, output, session) {

  observeEvent(input$debug,browser())
  sever_dp()

  output$contest_table <- renderDT({

    question_select_table |>
      datatable(
        rownames = FALSE,
        escape = FALSE,
        class = "compact stripe",
        selection = "none",
        options = list(
          paging = FALSE,
          ordering = FALSE,
          searching = FALSE,
          info = FALSE,
          scrollX = TRUE,
          preDrawCallback = JS("function() {Shiny.unbindAll(this.api().table().node()); }"),
          drawCallback = JS("function() { Shiny.bindAll(this.api().table().node()); } ")
        )
      )
  })

  selections <- reactive({
    question_select_table |>
      select(-Select) |>
      mutate(
        Selection = read_inputs(paste0("question_",row_number()))
      ) |>
      left_join(df_questions |> select(Question = question, Selection = choices, SelectionScore = score), by = c("Question","Selection"))
  })

  observeEvent(input$review_entry, {
    showModal(modalDialog(
      size = "xl",
      title = "Review Entry",
      need(input$nickname, "Please provide your nickname/handle!"),
      br(),
      need(!("Select" %in% selections()$Selection), "Missed some selections!"),
      br(),
      glue("Selections for {input$nickname}:"),
      br(),
      DTOutput("selections_list"),
      footer = list(
        actionButton("submit_entry", label = "Submit!", class = "btn-success"),
        modalButton("Cancel")
      )
    ))
  })

  output$selections_list <- renderDT({
    selections() %>%
      datatable(
        class = "compact stripe",
        rownames = FALSE,
        width = "100%",
        selection = "none",
        options = list(
          paging = FALSE,
          ordering = FALSE,
          searching = FALSE,
          scrollX = TRUE,
          info = FALSE
        )
      )
  })

  entries_append <- reactive({
    selections() %>%
      mutate(entry_nickname = input$nickname,
             entry_email = input$email,
             entry_date = Sys.time()) %>%
      select(starts_with('entry'), everything())
  })

  observeEvent(input$submit_entry,{

    req(nrow(selections())>0,
        # !("Select" %in% selections()$Selection),
        input$nickname,
        input$email)


    showModal(modalDialog('Saving your entry!'))

    write_dataset(dataset = entries_append(),
                  format = "parquet",
                  path = "storage",
                  partitioning = c("entry_email"),
                  hive_style = TRUE,
                  basename_template = glue::glue("{input$nickname}_{{i}}.parquet"))

    Sys.sleep(2)

    showModal(modalDialog(
      size = "l",
      title = "Successfully saved to server!",
      shiny::markdown("Please make sure you [submit your donation receipt](https://girlswhocode.com/donate) into the #sbprops channel on TDM!"),
      footer = div(modalButton(), downloadButton("download_entry","Download Entry"),)
      ))

  })

  download_entry <- downloadHandler(
    filename = "tdm-sbprops.csv",
    content = function(file) write.csv(entries_append(), file = file, quote = TRUE,rownames = FALSE)
  )


}

shinyApp(ui, server)
