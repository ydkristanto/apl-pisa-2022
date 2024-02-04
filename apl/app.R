library(shiny)
library(tidyverse)
library(bslib)

# Data ----
hasil_belajar <- read_csv(file = "https://raw.githubusercontent.com/ydkristanto/apl-pisa-2022/main/set-data/hasil_belajar.csv",
                          show_col_types = FALSE)
mat_idn <- hasil_belajar %>% 
  filter(iso3c == "IDN" & mapel == "Matematika") %>% 
  select(iso3c, skor_rerata) %>% 
  mutate(skor_rerata = round(skor_rerata, 0))

baca_idn <- hasil_belajar %>% 
  filter(iso3c == "IDN" & mapel == "Membaca") %>% 
  select(iso3c, skor_rerata) %>% 
  mutate(skor_rerata = round(skor_rerata, 0))

sains_idn <- hasil_belajar %>% 
  filter(iso3c == "IDN" & mapel == "Sains") %>% 
  select(iso3c, skor_rerata) %>% 
  mutate(skor_rerata = round(skor_rerata, 0))

# Antarmuka ----
ui <- page_navbar(
  title = "Indonesia dalam PISA 2022",
  id = "nav",
  sidebar = sidebar(
    conditionalPanel(
      "input.nav === 'Hasil Belajar'",
      selectInput("mapel", p("Literasi:", style = "font-weight: bold;"),
                  choices = c("Matematika" = "mat",
                              "Membaca" = "baca",
                              "Sains" = "sains"),
                  selected = "mat"),
      hr(),
      p("Tampilkan:", style = "font-weight: bold;"),
      checkboxInput("enam_atas", "Enam negara teratas",
                    value = FALSE)
    ),
    conditionalPanel(
      "input.nav === 'Proses dan Konten'",
      "Page 2 sidebar"
    )
  ),
  nav_panel("Hasil Belajar",
            layout_columns(
              uiOutput("kotak_mat"),
              uiOutput("kotak_baca"),
              uiOutput("kotak_sains"),
              card(card_header(textOutput("teks_mapel")),
                   plotOutput("plot_mapel")
                   ),
              col_widths = c(4, 4, 4, 12)
            )),
  nav_panel("Proses dan Konten", "Page 2 contents"),
  nav_panel("Kesetaraan", "Page 3 contents"),
  nav_panel("Tren", "Page 3 contents")
)

# Peladen ----
server <-  function(input, output) {
  ## Kotak nilai matematika ----
  output$kotak_mat <- renderUI({
    if (input$mapel == "mat") {
      value_box("Literasi Matematika",
                as.numeric(mat_idn$skor_rerata[1]),
                theme = "pink")
    } else {
      value_box("Literasi Matematika",
                as.numeric(mat_idn$skor_rerata[1]))
    }
  })
  
  ## Kotak nilai sains ----
  output$kotak_sains <- renderUI({
    if (input$mapel == "sains") {
      value_box("Literasi Sains",
                as.numeric(sains_idn$skor_rerata[1]),
                theme = "blue")
    } else {
      value_box("Literasi Sains",
                as.numeric(sains_idn$skor_rerata[1]))
    }
  })
  
  ## Kotak nilai membaca ----
  output$kotak_baca <- renderUI({
    if (input$mapel == "baca") {
      value_box("Literasi Membaca",
                as.numeric(baca_idn$skor_rerata[1]),
                theme = "green")
    } else {
      value_box("Literasi Membaca",
                as.numeric(sains_idn$skor_rerata[1]))
    }
  })
  
  ## Teks literasi mata pelajaran ----
  output$teks_mapel <- renderText({
    if (input$mapel == "mat") {
      "Literasi Matematika"
    } else if (input$mapel == "baca") {
      "Literasi Membaca"
    } else {
      "Literasi Sains"
    }
  })
  
  ## Plot literasi mata pelajaran ----
  output$plot_mapel <- renderPlot({
    if (input$enam_atas == TRUE) {
      data <- hasil_belajar %>% 
        select(iso3c, mapel, skor_rerata, skor_10,
               skor_25, skor_50, skor_75, skor_90) %>% 
        filter(iso3c %in% c("SGP", "JPN", "KOR", "EST", "CHE",
                            "CAN", "USA", "MEX", "BRA", "IDN", "PHL")) %>% 
        mutate(idn = ifelse(iso3c == "IDN", TRUE, FALSE))
    } else {
      data <- hasil_belajar %>% 
        select(iso3c, mapel, skor_rerata, skor_10,
               skor_25, skor_50, skor_75, skor_90) %>% 
        filter(iso3c %in% c("USA", "MEX", "BRA", "IDN", "PHL")) %>% 
        mutate(idn = ifelse(iso3c == "IDN", TRUE, FALSE))
    }
    
    if (input$mapel == "mat") {
      data %>% 
        filter(mapel == "Matematika") %>% 
        rename(rerata = skor_rerata) %>% 
        pivot_longer(cols = starts_with("skor_"),
                     names_to = "posisi",
                     values_to = "nilai") %>% 
        mutate(iso3c = fct_reorder(iso3c, -rerata)) %>% 
        ggplot(aes(x = iso3c)) +
        geom_line(aes(y = nilai, group = iso3c, color = idn),
                  alpha = .5, linewidth = 1) +
        geom_point(aes(y = nilai, color = idn), alpha = .5) +
        geom_point(aes(y = rerata, color = idn),
                   size = 3) +
        ylim(200, 750) +
        theme_bw(base_size = 14) +
        scale_color_manual(values = c("TRUE" = "#bf007f",
                                      "FALSE" = "gray40")) +
        theme(legend.position = 'none',
              panel.grid.major.x = element_blank(),
              panel.grid.minor = element_blank()) +
        labs(x = "Negara", y = "Skor")
    } else if (input$mapel == "baca") {
      data %>% 
        filter(mapel == "Membaca") %>% 
        rename(rerata = skor_rerata) %>% 
        pivot_longer(cols = starts_with("skor_"),
                     names_to = "posisi",
                     values_to = "nilai") %>% 
        mutate(iso3c = fct_reorder(iso3c, -rerata)) %>% 
        ggplot(aes(x = iso3c)) +
        geom_line(aes(y = nilai, group = iso3c, color = idn),
                  alpha = .5, linewidth = 1) +
        geom_point(aes(y = nilai, color = idn), alpha = .5) +
        geom_point(aes(y = rerata, color = idn),
                   size = 3) +
        ylim(200, 750) +
        theme_bw(base_size = 14) +
        scale_color_manual(values = c("TRUE" = "#00891a",
                                      "FALSE" = "gray40")) +
        theme(legend.position = 'none',
              panel.grid.major.x = element_blank(),
              panel.grid.minor = element_blank()) +
        labs(x = "Negara", y = "Skor")
    } else {
      data %>% 
        filter(mapel == "Sains") %>% 
        rename(rerata = skor_rerata) %>% 
        pivot_longer(cols = starts_with("skor_"),
                     names_to = "posisi",
                     values_to = "nilai") %>% 
        mutate(iso3c = fct_reorder(iso3c, -rerata)) %>% 
        ggplot(aes(x = iso3c)) +
        geom_line(aes(y = nilai, group = iso3c, color = idn),
                  alpha = .5, linewidth = 1) +
        geom_point(aes(y = nilai, color = idn), alpha = .5) +
        geom_point(aes(y = rerata, color = idn),
                   size = 3) +
        ylim(200, 750) +
        theme_bw(base_size = 14) +
        scale_color_manual(values = c("TRUE" = "#007bc2",
                                      "FALSE" = "gray40")) +
        theme(legend.position = 'none',
              panel.grid.major.x = element_blank(),
              panel.grid.minor = element_blank()) +
        labs(x = "Negara", y = "Skor")
    }
    
  })
}

# Aplikasi Shiny ----
shinyApp(ui, server)