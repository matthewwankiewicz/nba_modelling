library(shiny)
library(tidyverse)

nba_games <- read_csv("games.csv")
teams <- read_csv("teams.csv")
schedule <- read_csv("2021schedule.csv")
nba_games_ff <- read_csv("nbafourfactors.csv")
matchups <- read_csv("matchups.csv")

# create model
model <- glm(HOME_TEAM_WINS ~ efg_diff_off + efg_diff_def + tov_diff_off + tov_diff_def +
                 orb_diff_off + orb_diff_def + ft_diff_off + ft_diff_def,
             data = matchups, family = binomial())
summary(model)

# get predictions
matchups$estimate <- predict.glm(model, newdata = matchups,
                                     type = "response")


# Define UI for application that draws a histogram
ui <- navbarPage("Four Factors Model",
        tabPanel("Model",
            sidebarLayout(
            sidebarPanel(
            selectInput(inputId = "home",
                        label = "Select Team 1:",
                        choices = sort(unique(matchups$home_teams))),
            selectInput(inputId = "away",
                        label = "Select Team 2:",
                        choices = sort(unique(matchups$away_teams)))),
        mainPanel(
            textOutput("match"),
            tableOutput("matches"),
            textOutput("setup")
        ))),
        tabPanel("About",
                 mainPanel(h1("About the Model"),
                           "This model was created by Matthew Wankiewicz. It uses Four Factors data in order to predict the winner of basketball games. This is still a work in progress but as of right now, it is running at about 60% accuracy for the 2020-21 season. The four factors data is from https://cleaningtheglass.com/stats/league/fourfactors.
                           Gitub repo is available", tags$a(href="https://www.github.com/matthewwankiewicz/nba_modelling", "here.")))
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
        paste("The", input$home, "have a", prediction, "percent chance of beating", input$away)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
