library(dplyr)
library(gghighlight)
library(ggplot2)
library(googlesheets4)
library(kableExtra)
library(knitr)
library(lubridate)
library(rlang)
library(scales)
library(shiny)
library(shinydashboard)
library(stringr)
library(tidyverse)

gs4_deauth()
spotify = read_sheet(ss = "https://docs.google.com/spreadsheets/d/183Zf6fyC4CtSpcp3HdBRfhwbF04Ez1sKJgDSDHg0R8k", sheet = "Spotify Wrapped") %>%
  mutate(
    Song = Song %>% as.character(),
    Album = Album %>% as.character()
  ) %>%
  arrange(Year, Ranking)

spotify =
  spotify %>%
  select(
    Year,
    Ranking,
    Song,
    Album,
    Artist1,
    Artist2,
    Artist3,
    Artist4,
    Artist5,
    Artist6,
    SongID,
    AlbumID,
    Artist1ID,
    Artist2ID,
    Artist3ID,
    Artist4ID,
    Artist5ID,
    Artist6ID,
    ApproxGenre,
    `Album Year`,
    `Song Length`
  )

spotify = spotify %>%
  group_by(Year) %>%
  mutate(
    Score = max(row_number()) + 1 - row_number(),
    Count = 1,
    RecordInt = row_number()
  ) %>%
  filter(Song != "")

names(spotify) = c("Year", "SongRanking", "Song", "Album", "ArtistName", "Artist2Name", "Artist3Name", "Artist4Name", "Artist5Name", "Artist6Name", "SongID", "AlbumID", "Artist1ID", "Artist2ID", "Artist3ID", "Artist4ID", "Artist5ID", "Artist6ID", "Genre", "AlbumYear", "SongLengthChar", "Score", "Count", "RecordInt")

spotify$TotalSeconds = (str_sub(string = spotify$SongLengthChar, start = 1, end = 1) %>% as.integer() * 60) + str_sub(string = spotify$SongLengthChar, start = 3) %>% as.integer()
spotify$Minutes = spotify$TotalSeconds %>% ceiling() %>% "%/%"(60)
spotify$Seconds = spotify$TotalSeconds %>% ceiling() %>% "%%"(60)

spotify$YearDifference = (spotify$Year - spotify$AlbumYear) %>% as.integer()

spotify = spotify %>% as.data.frame()

total_time = function(seconds) {
  m = (seconds) %>% round(digits = 0) %>% sum() %>% "%/%"(60) %>% "%%"(60) %>% paste(":01 ", today(), sep = "") %>% as.POSIXct(format = "%M:%S %Y-%m-%d") %>% str_sub(start = 15, end = 16)
  s = (seconds) %>% round(digits = 0) %>% sum() %>% "%%"(60) %>% paste(":01 ", today(), sep = "") %>% as.POSIXct(format = "%M:%S %Y-%m-%d") %>% str_sub(start = 15, end = 16)
  t = paste(m, ":", s, sep = "")
  return(t)
}

Minutes_All = bind_cols(
  as.data.frame(c(2018, 2020, 2021, 2022, 2023, 2024)),
  as.data.frame(c(36663, 38198, 2768, 11681, 6591, 16228))
)

names(Minutes_All) = c("Year", "Minutes")

TopArtists_AllTime =
  spotify %>%
  group_by(ArtistName) %>%
  summarize(xt = sum(Score), Song_Count = n()) %>%
  rename("Score" = xt) %>%
  arrange(desc(Score)) %>%
  mutate(Rank = row_number(), xx = str_sub(string = row_number(), start = (row_number() %>% str_length()), end = 100) %>% as.integer())

TopArtists_AllTime = 
  TopArtists_AllTime %>%
  mutate(
    Rank_Text = case_when(
      xx == 1 ~ paste(Rank %>% as.character(), "st", sep = ""),
      xx == 2 ~ paste(Rank %>% as.character(), "nd", sep = ""),
      xx == 3 ~ paste(Rank %>% as.character(), "rd", sep = ""),
      .default = paste(Rank %>% as.character(), "th", sep = "")
    )
  ) %>%
  select(ArtistName, Score, Song_Count, Rank, Rank_Text)

ttt = c("Gone West", "TWICE", "NAYEON", "TZUYU", "Morgan Wallen", "Georgia Webster", "Red Rocks Worship", "Caitlyn Smith", "Bethel Music", "OneRepublic", "Hillsong UNITED", "The Fray", "Lady A", "Jesus Culture", "Elevation Worship", "Keane", "Gavin DeGraw", "Hanson", "Ed Sheeran", "Train")
# ttt = spotify$Artist


TopArtists_ByYear =
  spotify %>%
  group_by(ArtistName, Year) %>%
  summarize(xt = sum(Score), Song_Count = n()) %>%
  rename("Score" = xt) %>%
  arrange(Year, desc(Score), ArtistName) %>%
  group_by(Year) %>%
  mutate(Rank = row_number(Year), xx = str_sub(string = row_number(Year), start = (row_number(Year) %>% str_length()), end = 100) %>% as.integer()) %>%
  ungroup() %>%
  group_by(ArtistName) %>%
  mutate(
    tt = cumsum(case_when(
      Song_Count > 0 ~ 1,
      .default = 0
    )
    ), NewArtistFlag = case_when(tt <= 1 ~ "New Artist", .default = "Repeat Artist")
    
  ) %>%
  ungroup() %>%
  as.data.frame() %>%
  complete(ArtistName, Year, fill = list(Score = 0, Song_Count = 0, Rank = 0))

TopArtists_ByYear = 
  TopArtists_ByYear %>%
  mutate(
    Year_Rank_Label = case_when(
      Rank == 0 ~ "",
      xx == 1 ~ paste(Rank %>% as.character(), "st", sep = ""),
      xx == 2 ~ paste(Rank %>% as.character(), "nd", sep = ""),
      xx == 3 ~ paste(Rank %>% as.character(), "rd", sep = ""),
      .default = paste(Rank %>% as.character(), "th", sep = "")
    )
  ) %>%
  select(ArtistName, Year, Score, Song_Count, Rank, Year_Rank_Label, NewArtistFlag)

#TopArtists_ByYear$Year = as.factor(TopArtists_ByYear$Year)
TopArtists_ByYear$Rank = as.integer(TopArtists_ByYear$Rank)

#dec42024 11100

TopArtists_ByYear$Brave = case_when(
  TopArtists_ByYear$Rank == 0 ~ 0,
  .default = max(TopArtists_ByYear$Rank) + 1
  #.default = 100
)
TopArtists_ByYear$NewRank = TopArtists_ByYear$Brave - TopArtists_ByYear$Rank

TopArtists_ByYear = TopArtists_ByYear %>% as.data.frame()





TopGenres_ByYear =
  spotify %>%
  group_by(Genre, Year) %>%
  summarize(xt = sum(Score), Song_Count = n()) %>%
  rename("Score" = xt) %>%
  arrange(Year, desc(Score), Genre) %>%
  group_by(Year) %>%
  mutate(Rank = row_number(Year), xx = str_sub(string = row_number(Year), start = (row_number(Year) %>% str_length()), end = 100) %>% as.integer()) %>%
  as.data.frame() %>%
  complete(Genre, Year, fill = list(Score = 0, Song_Count = 0))

TopGenres_ByYear = 
  TopGenres_ByYear %>%
  mutate(
    Year_Rank_Label = case_when(
      xx == 1 ~ paste(Rank %>% as.character(), "st", sep = ""),
      xx == 2 ~ paste(Rank %>% as.character(), "nd", sep = ""),
      xx == 3 ~ paste(Rank %>% as.character(), "rd", sep = ""),
      .default = paste(Rank %>% as.character(), "th", sep = "")
    )
  ) %>%
  select(Genre, Year, Score, Song_Count, Rank, Year_Rank_Label)

TopGenres_ByYear$Year = as.factor(TopGenres_ByYear$Year)
TopGenres_ByYear$Rank = as.integer(TopGenres_ByYear$Rank)

TopGenres_ByYear$Brave = max(TopGenres_ByYear$Rank) + 1
TopGenres_ByYear$NewRank = TopGenres_ByYear$Brave - TopGenres_ByYear$Rank

TopGenres_ByYear = TopGenres_ByYear %>% as.data.frame()





#https://mastering-shiny.org/basic-ui.html

#selectInput = dropdown
#sliderInput = slider
#textInput
#passwordInput
#textAreaInput
#numericInput("num", "Number one", value = 0, min = 0, max = 100),
#dateInput("dob", "When were you born?"),
#dateRangeInput

#navbarMenu is a navbar, except with multiple options in a dropdown. example is "Data Analysis Projects"

uix = dashboardPage(
  skin = "green",
  dashboardHeader(
    title = "Spotify Data",
    disable = FALSE),  
  dashboardSidebar(
    sidebarMenu(
      menuItem(
        text = "Year",
        icon = icon(name = "calendar"),
        tabName = "Year",
        badgeLabel = "NEW",
        selected = TRUE),
      menuItem(
        text = "Artists",
        icon = icon(name = "music", lib = "font-awesome"),
        tabName = "Artists",
        badgeLabel = "NEW")#,
      #menuItem(
        #text = "Listening Statistics", #view most songs by artist in single year (top 5), highest score in a single year (top 5), most songs all time, highest score all time, most years appearing in spotify wrapped
        #icon = icon(name = "th"),
        #tabName = "Records",
        #badgeLabel = "NEW")
    )
  ),
  
  dashboardBody(
    #tags$head(
    #tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
    #),
    tags$script(HTML("$('body').addClass('fixed');")),
    
    tabItems(
      # First tab content
      tabItem(tabName = "Year",
              fluidRow(
                box(width = 6,
                    selectInput(
                      inputId = "SelectedYear",
                      label = "Select Year:",
                      choices = TopArtists_ByYear$Year %>% unique() %>% sort(),
                      #choices = c("2020", "2021", "2022", "2023", "2024"),
                      #selected = TopArtists_ByYear$Year %>% unique() %>% sort() %>% tail()
                      selected = c("2024")
                    )
                )
              ),
              #box(width = 12, title = "I'M LIKE TT",
              #fluidRow(
              #width = 12,
              #valueBoxOutput("Minnn"),
              #valueBoxOutput("CountArtist"),
              #valueBoxOutput("CountAlbumID")
              #)
              #),
              
              fluidRow(width = 12,
                       valueBoxOutput(outputId = "Minnn"),
                       valueBoxOutput(outputId = "CountArtist"),
                       valueBoxOutput(outputId = "CountAlbumID")
              ),
              
              fluidRow(width = 12,
                       column(width = 5, height = 5, plotOutput(outputId = "plot_genrexxx")),
                       column(width = 7, height = 10, tableOutput(outputId = "X123"))
              ),
              
              br(),
              
              #fluidRow(width = 12,
              #valueBoxOutput("Count_NewArtist"),
              #valueBoxOutput("Count_RepeatArtist"),
              #valueBoxOutput("UUU2")
              #),
              
              fluidRow(width = 12,
                       column(width = 6, plotOutput(outputId = "pie_Chart22")),
                       column(width = 6, plotOutput(outputId = "plot_genreByear"))
              ),
              br(),
              fluidRow(width = 12,
                       column(width = 6, tableOutput(outputId = "Top5Shortest")),
                       column(width = 6, tableOutput(outputId = "Top5Longest"))
              ),
              br(),
              fluidRow(width = 12,
                       valueBoxOutput(outputId = "AvgSongLength"),
                       valueBoxOutput(outputId = "AvgSongRelease")
                       
              ),
              br(),
              fluidRow(width = 12,
                       column(width = 6, tableOutput(outputId = "Top5Earliest")),
                       column(width = 6, tableOutput(outputId = "Top5Latest"))
              ),
              br(),
              fluidRow(width = 12,
                       column(width = 6, plotOutput(outputId = "sayyouloveme"))
              )
              
              #box(width = 3, solidHeader = TRUE, plotOutput("plot2"))
              
              #actionButton("clear", "Update")
      ),
      
      # Second tab content
      tabItem(tabName = "Artists",
              h2("Artist"),
              
              htmlOutput("Keeps"),
              fluidRow(
                column(width = 5,
                       selectInput(
                         inputId = "SelectedArtist",
                         label = "Select Artist:",
                         choices = ttt %>% unique() %>% sort(), selected = "TWICE"
                       )
                )),
              fluidRow(
                column(width = 8, tableOutput(outputId = "ArtistDiscopgraphy1")),
                column(width = 4, plotOutput(outputId = "plot2"))
              )
      ))
    
    #titlePanel(title = "Spotify Data"),
    #br(), #line break
    
    #sidebarPanel(
    #selectInput(inputId = "SelectedArtist", label = "Select Artist:", choices = ttt %>% unique() %>% sort(), selected = "TWICE"),
    #selectInput(inputId = "SelectedYear", label = "Select Year:", choices = TopArtists_ByYear$Year %>% unique() %>% sort(), selected = TopArtists_ByYear$Year %>% unique() %>% sort() %>% tail())
    #),
    #htmlOutput("Keeps"),
    
    #tableOutput("table"),
    
    #plotOutput("plot2"),
    #textOutput("Minnn"),
    #textOutput(ttt),
    #textOutput("Rank_Textx"),
    #textOutput("textdelulu"),
    #textOutput("textdelulu3"),
    #verbatimTextOutput("summary")
  )
)

serverx = function(input, output, session) {
  output$ArtistDiscopgraphy1 = renderText({
    spotify %>%
      select(Year, SongRanking, Song, ArtistName, Album) %>%
      filter(ArtistName == input$SelectedArtist) %>%
      ungroup() %>% #this line and the line afterwards are optional if I want to remove Aritst name as a row
      select(Year, SongRanking, Song, ArtistName, Album) %>%
      kable(format = "html", align = "llll", col.names = c("Year", "Song Ranking", "Song", "Artist Name", "Album")) %>%
      kable_styling(bootstrap_options = c("hover", "responsive", "striped")) %>%
      scroll_box(width = "100%", height = "100%") #%>%
    #row_spec(2, background = "#683659")
    
  })
  
  output$Minnn = renderValueBox({
    Minutes_All %>%
      filter(Year == input$SelectedYear) %>%
      select(Minutes) %>%
      sum() %>%
      format(nsmall = 0, big.mark = ",") %>%
      valueBox(subtitle = "Minutes Streamed", color = "green", icon = icon(name = "fab fa-spotify"), width = 4)
  })
  
  output$CountArtist = renderValueBox({
    spotify %>%
      filter(Year == input$SelectedYear) %>%
      select(ArtistName) %>%
      table() %>%
      length() %>%
      format(nsmall = 0, big.mark = ",") %>%
      valueBox(subtitle = "Distinct Artists", color = "teal", icon = icon(name = "users"), width = 4)
    #red, yellow, aqua, blue, light-blue, green, navy, teal, olive, lime, orange, fuchsia, purple, maroon, black.
    
  })
  
  output$CountAlbumID = renderValueBox({
    spotify %>%
      filter(Year == input$SelectedYear) %>%
      select(AlbumID) %>%
      ungroup() %>%
      table() %>%
      length() %>%
      format(nsmall = 0, big.mark = ",") %>%
      valueBox(subtitle = "Distinct Albums", color = "aqua", icon(name = "fas fa-music"), width = 4)
    #red, yellow, aqua, blue, light-blue, green, navy, teal, olive, lime, orange, fuchsia, purple, maroon, black.
  })
  
  output$CountGenre = renderValueBox({
    spotify %>%
      filter(Year == input$SelectedYear) %>%
      select(Genre) %>%
      ungroup() %>%
      table() %>%
      length() %>%
      format(nsmall = 0, big.mark = ",") %>%
      valueBox(subtitle = "Distinct Genres", color = "purple", icon(name = "fas fa-music"), width = 4)
  })
  
  output$AvgSongLength = renderValueBox({
    spotify %>%
      filter(Year == input$SelectedYear) %>%
      ungroup() %>%
      select(TotalSeconds) %>%
      unlist() %>%
      mean() %>%
      total_time() %>%
      valueBox(subtitle = "Average Song Length", color = "red", icon(name = "fas fa-music"), width = 4)
  })
  
  output$AvgSongRelease = renderValueBox({
    tt = spotify %>%
      filter(Year == input$SelectedYear) %>%
      ungroup() %>%
      select(YearDifference) %>%
      unlist() %>%
      mean() %>%
      round(digits = 2)
    
    tt = input$SelectedYear %>% as.integer() - tt
    
    tt %>%
      valueBox(subtitle = "Average Song Release", color = "blue", icon(name = "fas fa-music"), width = 4)
  })
  
  
  output$Keeps = renderUI({
    
    t1 = TopArtists_AllTime[which(TopArtists_AllTime$ArtistName == input$SelectedArtist), 5] #try to make the 5 dynamic
    xxx1 = paste("According to Spotify Wrapped, ", input$SelectedArtist, " is my ", t1, " favorite artist overall. Here are my favorite songs from ", input$SelectedArtist, ", according to the data.", sep = "")
    
    t11 = TopArtists_AllTime %>% filter(TopArtists_AllTime$ArtistName == input$SelectedArtist) %>% select(Song_Count)
    xxx2 = paste(t11, " songs from ", input$SelectedArtist, " have appeared in my Spotify Wrapped all time.", sep = "")
    
    HTML(paste(xxx1, "</br>", "</br>", xxx2, "</br>", "</br>", sep = ""))
  })
  
  

  output$sayyouloveme = renderPlot({
    spotify %>%
      filter(Year == input$SelectedYear) %>%
      group_by(Year, AlbumYear) %>%
      mutate(TT = n()) %>%
      ggplot(aes(x = AlbumYear, y = TT)) + 
      geom_bar(stat = "identity", fill = "darkblue", width = 1) +
      #geom_text(aes(label = Year_Rank_Label %>% format(nsmall = 2)), vjust = 1.6, color = "white", size = 1) +
      xlab(label = "Album Year") +
      ylab(label = "Song Count") +
      ggtitle(label = "Song Count by Album Year") +
      theme(plot.title = element_text(hjust = 0.5),
            #axis.title.y = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank()
      )
  })
  
  
  
  output$plot2 = renderPlot({
    
    ggplot(data = TopArtists_ByYear %>% filter(ArtistName == input$SelectedArtist), aes(x = Year, y = NewRank)) +
      geom_bar(stat = "identity", fill = "darkblue") +
      geom_text(aes(label = Year_Rank_Label %>% format(nsmall = 2)), vjust = 1.6, color = "white", size = 5) +
      xlab(label = "Year") +
      ylab(label = "Rank") +
      ggtitle(label = "Artist Rank by Year") +
      theme(plot.title = element_text(hjust = 0.5),
            #axis.title.y = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank()
      ) +
    #scale_x_discrete(limits = c(2020, 2021, 2022, 2023, 2024))
    scale_x_continuous(breaks = min(TopArtists_ByYear$Year):max(TopArtists_ByYear$Year))
  })
  
  output$X123 = renderText({
    TopArtists_ByYear %>%
      filter(Year == input$SelectedYear, Rank <= 10, Rank > 0) %>%
      ungroup() %>%
      select(ArtistName, Score, Song_Count) %>%
      arrange(desc(Score), desc(Song_Count)) %>%
      kable(format = "html", align = "llrr", col.names = c("Artist Name", "Score", "Song Count")) %>%
      kable_styling(bootstrap_options = c("hover", "responsive", "striped")) %>%
      scroll_box(width = "100%", height = "100%")
  })
  
  output$Top5Shortest = renderText({
    spotify %>%
      filter(Year == input$SelectedYear) %>%
      arrange(TotalSeconds) %>%
      head(n = 5) %>%
      ungroup() %>%
      select(SongRanking, Song, ArtistName, SongLengthChar) %>%
      kable(format = "html", align = "lllr", caption = "Top 5 Shortest Songs", col.names = c("Song Ranking", "Song", "Artist Name", "Song Length")) %>%
      kable_styling(bootstrap_options = c("hover", "responsive", "striped")) %>%
      scroll_box(width = "100%", height = "100%")
  })
  
  output$Top5Longest = renderText({
    spotify %>%
      filter(Year == input$SelectedYear) %>%
      arrange(TotalSeconds %>% desc()) %>%
      head(n = 5) %>%
      ungroup() %>%
      select(SongRanking, Song, ArtistName, SongLengthChar) %>%
      kable(format = "html", align = "lllr", caption = "Top 5 Longest Songs", col.names = c("Song Ranking", "Song", "Artist Name", "Song Length")) %>%
      kable_styling(bootstrap_options = c("hover", "responsive", "striped")) %>%
      scroll_box(width = "100%", height = "100%")
  })
  
  output$Top5Earliest = renderText({
    spotify %>%
      filter(Year == input$SelectedYear) %>%
      arrange(AlbumYear %>% desc()) %>%
      head(n = 5) %>%
      ungroup() %>%
      select(SongRanking, Song, ArtistName, Album, AlbumYear) %>%
      kable(format = "html", align = "lllr", caption = "Top 5 Earliest Songs", col.names = c("Song Ranking", "Song", "Artist Name", "Album", "Album Year")) %>%
      kable_styling(bootstrap_options = c("hover", "responsive", "striped")) %>%
      scroll_box(width = "100%", height = "100%")
  })
  
  output$Top5Latest = renderText({
    spotify %>%
      filter(Year == input$SelectedYear) %>%
      arrange(AlbumYear) %>%
      head(n = 5) %>%
      ungroup() %>%
      select(SongRanking, Song, ArtistName, Album, AlbumYear) %>%
      kable(format = "html", align = "lllr", caption = "Top 5 Latest Songs", col.names = c("Song Ranking", "Song", "Artist Name", "Album", "Album Year")) %>%
      kable_styling(bootstrap_options = c("hover", "responsive", "striped")) %>%
      scroll_box(width = "100%", height = "100%")
  })
  
  output$plot_genrexxx = renderPlot({
    
    ggplot(data = TopGenres_ByYear %>% filter(Year == input$SelectedYear, Rank <= 5), aes(x = Song_Count, y = reorder(Genre, Song_Count))) +
      geom_bar(stat = "identity", fill = "#08A45C", width = 0.75) +
      geom_text(aes(label = Song_Count %>% format(nsmall = 0)), hjust = 2, vjust = 0.5, color = "white", size = 4) +
      xlab(label = "Song Count") +
      #ylab(label = "Genre") +
      ggtitle(label = "Top 5 Genres") +
      theme(plot.title = element_text(hjust = 0.5),
            axis.title.y = element_blank(),
            #axis.title.x = element_blank(),
            #axis.text.y = element_blank(),
            #axis.text.y = Genre,
            axis.ticks.y = element_blank()
            #axis.text.x = 
      )
  })
  
  output$Count_NewArtist = renderValueBox({
    Data_NewArtist = Year_Artist %>%
      filter(Year.y == input$SelectedYear) %>%
      ungroup() %>%
      select("NewArtist2") %>%
      sum() %>%
      format(nsmall = 0, big.mark = ",")
    Data_NewArtist %>%
      valueBox(subtitle = "New Artists", color = "green", icon = icon(name = "fab fa-spotify"), width = 4)
  })
  
  output$Count_RepeatArtist = renderValueBox({
    Data_RepeatArtist = Year_Artist %>%
      filter(Year.y == input$SelectedYear) %>%
      ungroup() %>%
      select("RepeatArtist2") %>%
      sum() %>%
      format(nsmall = 0, big.mark = ",")
    Data_RepeatArtist %>%
      valueBox(subtitle = "Repeat Artists", color = "darkgreen", icon = icon(name = "fab fa-spotify"), width = 4)
  })
  
  output$UUU3 = renderValueBox({
    spotify %>%
      filter(Year == input$SelectedYear) %>%
      select(Year) %>%
      sum() %>%
      format(nsmall = 0, big.mark = ",") %>%
      valueBox(subtitle = "Minutes Streamed", color = "darkgreen", icon = icon(name = "fab fa-spotify"), width = 4)
  })
  
  
  output$pie_Chart22 = renderPlot({
    Data_PieChart =
      TopArtists_ByYear %>%
      ungroup() %>%
      group_by(NewArtistFlag) %>%
      #arrange(Year, Song_Count) %>%
      filter(Year == input$SelectedYear, is.na(NewArtistFlag) == FALSE) %>%

      summarize(`Song Count` = sum(Song_Count)) %>%
      arrange(NewArtistFlag)
    #arrange(desc(`Song Count`)) %>%
    #ungroup()
    
    
    ggplot(data = Data_PieChart, aes(x = "", y = `Song Count`, group = NewArtistFlag, fill = NewArtistFlag)) +
      #geom_bar(stat = "identity", width = 1, fill = c("#08A45C", "#D9D9D9")) +
      geom_bar(stat = "identity", width = 1) +
      coord_polar(theta = "y", start = 0) +
      #geom_col(color = "black", position = "stack", orientation = "x") +
      scale_fill_manual(values = c("Repeat Artist" = c("#333333"), "New Artist" = c("#08A45C"))) +
      theme_void() +
      #theme_minimal() +
      #labs(fill = "TTX") + 
      geom_text(aes(label = `Song Count`), color = "white", size = 5, position = position_stack(vjust = 0.5)) +
      #xlab(label = "Song Count") +
      #ylab(label = "Genre") +
      ggtitle(label = "Songs by Artist Breakdown") +
      theme(plot.title = element_text(hjust = 0.5),
            axis.title.y = element_blank(),
            legend.position = "right",
            panel.grid = element_blank(),
            axis.ticks = element_blank(),
            #axis.title.x = element_blank(),
            #axis.text.y = element_blank(),
            #axis.text.y = Genre,
            axis.ticks.y = element_blank(),
            axis.text.x = element_blank(),
            legend.key.size = unit(x = 1, units = "cm"), #change legend key size
            legend.key.height = unit(x = 1, units = "cm"), #change legend key height
            legend.key.width = unit(x = 1, units = "cm"), #change legend key width
            legend.title = element_text(size = 14), #change legend title font size
            legend.text = element_text(size = 10) #change legend text font size
      ) +
      guides(fill = guide_legend(title = "Artist Type"))
  })
  
  
  output$plot_genreByear = renderPlot({
    
    ggplot(data = TopGenres_ByYear, aes(x = Year, y = Song_Count, fill = Genre)) +
      geom_bar(position = "fill", stat = "identity", width = 0.75, aes(alpha = Year == input$SelectedYear)) +
      #geom_text(aes(label = Song_Count %>% format(nsmall = 0)), hjust = 2, vjust = 0.5, color = "white", size = 4) +
      #xlab(label = "Year") +
      ylab(label = "Song Percentage") +
      #ggtitle(label = "Genre Count by Year") +
      theme(plot.title = element_text(hjust = 0.5),
            #axis.title.y = element_blank(),
            #axis.title.x = element_blank(),
            #axis.text.y = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank()
            #axis.text.x = 
      ) +
      scale_alpha_manual(values = c("TRUE" = 1, "FALSE" = 0.5), guide = F)
  })
  
  
}







shinyApp(
  ui = uix,
  server = serverx
)