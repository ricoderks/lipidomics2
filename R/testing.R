ui <- bslib::page_navbar(
  id = "main",
  title = "Testing",
  bslib::nav_panel(
    title = "Panel 1",
    bslib::card(
      "hi"
    )
  ),
  bslib::nav_panel(
    title = "Panel 2",
    value = "panel2",
    bslib::navset_card_tab(
      id = "settings",
      bslib::nav_panel(
        title = "Panel 2-1",
        shiny::radioButtons(
          inputId = "select_panel",
          label = "Show / hide panel 2",
          choices = c("Show", "Hide"),
          selected = "Show"
        )
      ),
      bslib::nav_panel(
        title = "Panel 2-2",
        value = "panel22",
        bslib::card("Do some nice stuff here")
      )
    )
  )
)

server <- function(input, output, session) {
  shiny::observeEvent(input$select_panel, {
    switch(
      input$select_panel,
      "Show" = bslib::nav_show(id = "settings",
                               target = "panel22"),
      "Hide" = bslib::nav_hide(id = "settings",
                               target = "panel22")
    )
  })
}

shiny::shinyApp(
  ui = ui,
  server = server
)
