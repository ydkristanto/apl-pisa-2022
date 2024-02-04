library(shiny)
library(tidyverse)
library(bslib)

ui <- page_navbar(
  title = "Indonesia dalam PISA 2022",
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
  nav_panel("Hasil Belajar", "Page 1 contents"),
  nav_panel("Proses dan Konten Matematika", "Page 2 contents"),
  nav_panel("Kesetaraan", "Page 3 contents"),
  nav_panel("Tren Hasil Belajar dan Kesetaraan", "Page 3 contents")
)
server <-  function(...) {
  
}

shinyApp(ui, server)