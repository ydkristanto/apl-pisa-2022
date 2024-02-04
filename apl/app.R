shinyApp(
  page_navbar(
    title = "Conditional sidebar",
    id = "nav",
    sidebar = sidebar(
      conditionalPanel(
        "input.nav === 'Page 1'",
        "Page 1 sidebar"
      ),
      conditionalPanel(
        "input.nav === 'Page 2'",
        "Page 2 sidebar"
      )
    ),
    nav_panel("Page 1", "Page 1 contents"),
    nav_panel("Page 2", "Page 2 contents")
  ),
  server = function(...) {
    # no server logic required
  }
)