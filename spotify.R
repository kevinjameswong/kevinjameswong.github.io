spotify = read.csv("~/Documents/KevinWebsite/kevinjameswong.github.io/files/SpotifyWrapped.csv")

spotify =
  spotify %>%
  select(Year,
         Ranking,
         Song,
         Album,
         Artist.1,
         Artist.2,
         Artist.3,
         Artist.4,
         Artist.5,
         Artist.6,
         Approx.Genre)

spotify = spotify %>%
  group_by(Year) %>%
  mutate(
    Score = max(row_number()) + 1 - row_number(),
    Count = 1,
    RecordInt = row_number()
  ) %>%
  filter(Song != "")

names(spotify) = c("Year", "Ranking", "Song", "Album", "Artist", "Artist2", "Artist3", "Artist4", "Artist5", "Artist6", "Genre", "Score", "Count", "RecordInt")

#https://mastering-shiny.org/basic-ui.html

#selectInput = dropdown
#sliderInput = slider
#textInput
#passwordInput
#textAreaInput
#numericInput("num", "Number one", value = 0, min = 0, max = 100),
#dateInput("dob", "When were you born?"),
#dateRangeInput

uix = fluidPage(
  selectInput(inputId = "dataset", label = "Dataset", choices = spotify$Artist %>% unique() %>% sort(), selected = "TWICE"),
  textOutput("textdelulu"),
  verbatimTextOutput("summary"),
  tableOutput("table"),
  mainPanel(
    textOutput("DataTable")
  )
)

serverx = function(input, output, session) {
  output$textdelulu = renderText({paste("Ok babo")})
  output$summary = renderPrint({
    dataset = get(input$dataset, spotify)
    max(dataset)
  })
  output$table = renderText({
    spotify %>%
      select(Year, Ranking, Song, Artist, Genre, Score) %>%
      filter(Artist == input$dataset) %>%
      kable(format = "html", align = "l") %>%
      kable_styling(bootstrap_options = c("hover", "responsive")) %>%
      scroll_box(width = "100%", height = "100%")
  })
}

shinyApp(ui = uix, server = serverx)