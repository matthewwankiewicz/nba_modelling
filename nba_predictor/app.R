library(shiny)
library(tidyverse)

nba_games <- read_csv("games.csv")
teams <- read_csv("teams.csv")
schedule <- read_csv("2021schedule.csv")
nba_games_ff <- read_csv("nbafourfactors.csv")


# create model
model <- glm(Home_team_wins ~ efg_diff_off + efg_diff_def + tov_diff_off + tov_diff_def +
                 orb_diff_off + orb_diff_def + ft_diff_off + ft_diff_def,
             data = schedule, family = binomial())
summary(model)

# get list of NBA team matchups

matchups <- read_csv("matchups.csv")

# get predictions
matchups$estimate <- predict.glm(model, newdata = matchups,
                                     type = "response")


# Define UI for application that draws a histogram
ui <- navbarPage("Four Factors Model",
        tabPanel("Model",
            sidebarLayout(
            sidebarPanel(
            selectInput(inputId = "home",
                        label = "Select Home Team:",
                        choices = sort(unique(matchups$home_teams))),
            selectInput(inputId = "away",
                        label = "Select Away Team:",
                        choices = sort(unique(matchups$away_teams)))),
        mainPanel(
            textOutput("match"),
            tableOutput("matches"),
            textOutput("setup")
        ))),
        tabPanel("About",
                 mainPanel(h1("About the Model"),
                           "This model was created by Matthew Wankiewicz. It uses Four Factors data in order to predict the winner of basketball games. This is still a work in progress but as of right now, it is running at about 63% accuracy for the 2020-21 season. The four factors data is from https://cleaningtheglass.com/stats/league/fourfactors.
                           Gitub repo is available at https://github.com/matthewwankiewicz/nba_modelling."))
)


server <- function(input, output) {
    output$match <- renderText({
        paste("You have selected the", input$home, "versus the", input$away, sep = " ")
    })
    output$matches <- renderTable({
        nba_games_ff %>% 
            filter(Home == input$home | Away == input$home,
                   Home == input$away | Away == input$away) %>% 
            select(Home, Away, PTS_home, PTS_away) %>% 
            mutate(winner = ifelse(PTS_home > PTS_away, "Home", "Away"),
                   MOV = abs(PTS_away - PTS_home))
    })
    output$setup <- renderText({
        matchup <- matchups %>% 
            filter(home_teams == input$home,
                   away_teams == input$away)
        prediction <- round(predict.glm(model, newdata = matchup,
                                  type = "response")[1], digits = 3)*100
        paste("The", input$home, "have a", prediction, "percent chance of beating", input$away,
              "when the", input$home, "are the home team.")
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
