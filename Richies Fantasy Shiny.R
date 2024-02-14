# load libraries
library(shiny)
library(tidyverse)
library(showtext)
library(bslib)
library(gridExtra)
library(stringr)
library(DT)
library(ggplot2)
library(ggrepel)

#font_add_google(name = "Montserrat", family = "Montserrat")
#font_add_google(name = "Fira Code", family = "Fira Code")
#showtext_auto()

# load data
data <- read.csv("dynasty game data.csv")
names(data)[names(data) == "W.L"] <- "Result"
data$xwLoss <- 1 - data$wPercent
data$wtLoss <- 1 - data$tPercent

ui <- fluidPage(
  titlePanel(h1("Richie's Fantasy Shiny App", align = "center")),
  navbarPage(" ", 
    id = "tabset",
    tabPanel(
      "Richie's Dashboard",
      h3("Home Dashboard", align = "center"),
      fluidRow(column(3,
                      selectInput(
                        "home_season",
                        "Select a season",
                        choices = c("All Time", 2020, 2021, 2022, 2023),
                        selected = 2023,
                        width = "100%"
                      )),
               column(3,
                      selectInput(
                        "home_model",
                        "Select an exp model",
                        choices = c("Weekly", "Total"),
                        selected = "Total",
                        width = "100%"
                      ))
               ),
      fluidRow(
        plotOutput("home_plot", width = "100%")
        ),
      fluidRow(
        dataTableOutput("home_table")
        )
      ),
    tabPanel(
      "Team Analysis",
      h3("Team Analysis", align = "center"),
      fluidRow(column(3,
                      selectInput(
                        "team_owner",
                        "Select an owner",
                        choices = c(' ', unique(sort(data$Owner))),
                        width = "100%"
                        )),
               column(3,
                      selectInput(
                        "team_season",
                        "Select a season",
                        choices = unique(sort(data$Season)),
                        selected = 2023,
                        width = "100%"
                        )),
               column(4,
                      "* red line is the average PPG 
                      for 4th placed teams historically")
               ),
      fluidRow(
        column(8, plotOutput("team_plot", width = "100%"))
        ),
      fluidRow(
        column(8, dataTableOutput("team_table"))
        )
    ),
    tabPanel("All Time Records", 
             h3("Richie's Record Book", align = "center"),
             fluidRow(
               column(6, h5("All Time High Scores", align = "center")),
               column(6, h5("All Time Low Scores", align = "center"))
             ),
             fluidRow(
               column(6, dataTableOutput("record_high")),
               column(6, dataTableOutput("record_low"))
               ),
             fluidRow(column(8, h5("All Time Toughest Losses", align = "center"))),
             fluidRow(
               column(8, dataTableOutput("record_loss"))
             )),
    tabPanel("Historic Starts",
             h3("Richie's Starts", 
                align = "center"),
             fluidRow(column(4,
                             selectInput(
                               "start_model",
                               "Select an expectation model",
                               choices = c("Weekly", "Total"),
                               selected = "Total",
                               width = "100%"
                             ))
                      ),
             fluidRow(
               plotOutput("start_actPlot", width = "100%")
               ),
             fluidRow(br()),
             fluidRow(
               plotOutput("start_expPlot", width = "100%")
               ))
  )
)

server <- function(input, output, session) {
  
  home_table <- reactive({
    if(input$home_season != "All Time" && input$home_model == "Weekly")
      data %>% filter(Season == input$home_season) %>%
      group_by(Owner) %>%
      summarise(
        Wins = sum(Result),
        Losses = sum(Result == 0),
        Win_Pct = round(Wins/sum(Wins, Losses), 3),
        xWins = round(sum(wPercent), digits = 1),
        xLosses = round(sum(xwLoss), digits = 1),
        xWin_Pct = format(round(xWins/sum(Wins, Losses), digits = 3), nsmall = 3),
        Pts_For = format(round(sum(PF), digits = 1), nsmall = 1),
        Pts_Ag = format(round(sum(PA), digits = 1), nsmall = 1),
        Pt_Diff = format(round(sum(PF) - sum(PA), digits = 1), nsmall = 1)
        )
    else
      if(input$home_season != "All Time" && input$home_model == "Total")
        data %>% filter(Season == input$home_season) %>%
      group_by(Owner) %>%
      summarise(
        Wins = sum(Result),
        Losses = sum(Result == 0),
        Win_Pct = round(Wins/sum(Wins, Losses), 3),
        xWins = round(sum(tPercent), digits = 1),
        xLosses = round(sum(xwLoss), digits = 1),
        xWin_Pct = format(round(xWins/sum(Wins, Losses), digits = 3), nsmall = 3),
        Pts_For = format(round(sum(PF), digits = 1), nsmall = 1),
        Pts_Ag = format(round(sum(PA), digits = 1), nsmall = 1),
        Pt_Diff = format(round(sum(PF) - sum(PA), digits = 1), nsmall = 1)
      )
    else
      if(input$home_season == "All Time" && input$home_model == "Weekly")
        data %>%
        group_by(Owner) %>%
        summarise(
          Wins = sum(Result),
          Losses = sum(Result == 0),
          Win_Pct = round(Wins/sum(Wins, Losses), 3),
          xWins = round(sum(wPercent), digits = 1),
          xLosses = round(sum(xwLoss), digits = 1),
          xWin_Pct = format(round(xWins/sum(Wins, Losses), digits = 3), nsmall = 3),
          Pts_For = format(round(sum(PF), digits = 1), nsmall = 1),
          Pts_Ag = format(round(sum(PA), digits = 1), nsmall = 1),
          Pt_Diff = format(round(sum(PF) - sum(PA), digits = 1), nsmall = 1)
          )
    else
      if(input$home_season == "All Time" && input$home_model == "Total")
        data %>%
      group_by(Owner) %>%
      summarise(
        Wins = sum(Result),
        Losses = sum(Result == 0),
        Win_Pct = round(Wins/sum(Wins, Losses), 3),
        xWins = round(sum(tPercent), digits = 1),
        xLosses = round(sum(xwLoss), digits = 1),
        xWin_Pct = format(round(xWins/sum(Wins, Losses), digits = 3), nsmall = 3),
        Pts_For = format(round(sum(PF), digits = 1), nsmall = 1),
        Pts_Ag = format(round(sum(PA), digits = 1), nsmall = 1),
        Pt_Diff = format(round(sum(PF) - sum(PA), digits = 1), nsmall = 1)
      )
  })
  
  home_max <- reactive({
    max(data %>% filter(Season == input$home_season) %>% select(Week))
  })
  
  output$home_table <- DT::renderDataTable(home_table() %>%
                                         arrange(-Win_Pct),
                                       options = list(
                                         pageLength = 8,
                                         scrollY = FALSE,
                                         columnDefs = list(list(
                                           targets = '_all', 
                                           className = 'dt-center'))),
                                         rownames = FALSE,
                                         filter = list(position = "none"))
  
  output$home_plot <- renderPlot({home_table() %>%
      ggplot(aes(x = Win_Pct, y = xWin_Pct, colour = Owner)) +
      geom_point() + 
      labs(title = paste("Actual vs Expected Win Percent ", 
                         if(input$home_season == "All Time")
                           paste("of All Time")
                         else
                           paste("through Week ", home_max())
                         )
           ) +
      xlab("Actual Win Percentage") +
      ylab("Expected Win Percentage") +
      theme_minimal(base_family = "Montserrat", base_size = 14) +
      theme(panel.background = element_rect(fill = "transparent", color = NA),
            axis.line = element_blank(),
            axis.ticks = element_blank(),
            panel.grid.minor = element_blank(),
            plot.title = element_text(hjust = 0.5)
            ) + 
      geom_text_repel(aes(label = paste("(",Owner, ", ", Pts_For,")")))
    
  })
  
  team_table <- reactive({
    data$Result <- ifelse(data$Result == 1, "W", "L")
    
    data %>% filter(Owner == input$team_owner, Season == input$team_season) %>%
      select(Opp, Week, Result, tPercent, wPercent, PF, PA) %>%
      rename(Opponent = Opp, txWin = tPercent, wxWin = wPercent)
  })
  
  team_max <- reactive({max(data %>% filter(Season == input$team_season) %>% 
                              select(Week))})
  
  output$team_plot <- renderPlot({
    team_table() %>%
      ggplot(aes(x = Week, y = PF)) + 
      geom_smooth(se = FALSE) +
      geom_point(aes(colour = Result)) +
      labs(title = paste(input$team_owner, "'s Team Analysis for ", 
                         input$team_season)
           ) +
      xlab("Week") +
      ylab("Points For") +
      theme_minimal(base_family = "Montserrat", base_size = 14) +
      theme(panel.background = element_rect(fill = "transparent", color = NA),
            axis.line = element_blank(),
            axis.ticks = element_blank(),
            panel.grid.minor = element_blank(),
            panel.grid.major.x = element_blank(),
            plot.title = element_text(hjust = 0.5),
            legend.position = "none"
            ) + 
      scale_x_continuous(limits = c(1,max(team_max())), 
                         breaks = seq(1, max(team_max()), by = 1)
                         ) + 
      scale_y_continuous(limits = c(50, 200),
                         breaks = seq(50, 200, by = 25)) + 
      geom_hline(aes(yintercept = 128.5, color = "red",
                     label = "Avg PPG for 4th Placed Teams Historically")) 
  })
  
  output$team_table <- DT::renderDataTable(team_table(),
                                           options = list(
                                             pageLength = 17,
                                             scrollY = FALSE,
                                             columnDefs = list(list(
                                               targets = '_all', 
                                               className = 'dt-center'))),
                                             rownames = FALSE,
                                             filter = list(position = "none"))
  
  maxWeek <- max(data %>% filter(Season == max(unique(data$Season)))%>% 
                   select(Week))
  
  start_data <- reactive({
    if(input$start_model == "Weekly")
      data %>% filter(Week <= maxWeek) %>%
      group_by(Owner, Season) %>%
      summarise(
        Win_Pct = round(sum(Result)/maxWeek, digits = 3),
        xtWin_Pct = round(sum(tPercent)/maxWeek, digits = 3),
        xwWin_Pct = round(sum(wPercent)/maxWeek, digits = 3),
        xWin_Pct = xwWin_Pct
      )
    else
      data %>% filter(Week <= maxWeek) %>%
      group_by(Owner, Season) %>%
      summarise(
        Win_Pct = round(sum(Result)/maxWeek, digits = 3),
        xtWin_Pct = round(sum(tPercent)/maxWeek, digits = 3),
        xwWin_Pct = round(sum(wPercent)/maxWeek, digits = 3),
        xWin_Pct = xtWin_Pct
      )
    
  })
  
  output$start_actPlot <- renderPlot({
    start_data() %>%
      ggplot(aes(x = Win_Pct, y = xWin_Pct, colour = Owner)) +
      geom_point() + 
      labs(title = "Actual vs Expected Win Percentage",
           subtitle = paste("Through Week ", maxWeek))+
      xlab("Actual Win Percentage") +
      ylab(paste("Expected Win Percentage (", input$start_model, ")")) +
      theme_minimal(base_family = "Montserrat", base_size = 14) +
      theme(panel.background = element_rect(fill = "transparent", color = NA),
            axis.line = element_blank(),
            axis.ticks = element_blank(),
            panel.grid.minor = element_blank(),
            plot.title = element_text(hjust = 0.5),
            plot.subtitle = element_text(hjust = 0.5)
            ) +
      geom_text_repel(aes(label = paste(Owner, ", ", Season))) + 
      scale_x_continuous(limits = c(0,1), breaks = seq(0, 1, 0.1)) + 
      scale_y_continuous(limits = c(0,1), breaks = seq(0, 1, 0.1))
    })
  
  output$start_expPlot <- renderPlot({
    start_data() %>%
      ggplot(aes(x = xtWin_Pct, y = xwWin_Pct, colour = Owner)) +
      geom_point() + 
      labs(title = "Total vs Weekly Expected Win Percentage",
           subtitle = paste("Through Week ", maxWeek))+
      xlab("Expected Win Percentage (Total)") +
      ylab("Expected Win Percentage (Weekly)") +
      theme_minimal(base_family = "Montserrat", base_size = 14) +
      theme(panel.background = element_rect(fill = "transparent", color = NA),
            axis.line = element_blank(),
            axis.ticks = element_blank(),
            panel.grid.minor = element_blank(),
            plot.title = element_text(hjust = 0.5),
            plot.subtitle = element_text(hjust = 0.5)
      ) +
      geom_text_repel(aes(label = paste(Owner, ", ", Season))) + 
      scale_x_continuous(limits = c(0,1), breaks = seq(0, 1, 0.1)) + 
      scale_y_continuous(limits = c(0,1), breaks = seq(0, 1, 0.1))
  })
  
  high <- {data %>%
    select(PF, Owner, Opp, Season, Week) %>%
    rename(Score = PF, Opponent = Opp) %>%
    arrange(-Score) %>%
    subset(Score >= Score[20])}
  
  low <- {data %>%
      select(PF, Owner, Opp, Season, Week) %>%
      rename(Score = PF, Opponent = Opp) %>%
      arrange(Score) %>%
      subset(Score <= Score[20])}
  
  loss <- {data %>% filter(Result == 0) %>%
      select(tPercent, PF, Owner, Opp, Season, Week) %>%
      rename(xWin = tPercent, Score = PF, Opponent = Opp) %>%
      arrange(-xWin) %>%
      subset(xWin >= xWin[20])}
    
  
  output$record_high <- DT::renderDataTable(
    high,
    options = list(
      pageLength = 20,
      scrollY = FALSE,
      columnDefs = list(list(
        targets = '_all',
        className = 'dt-center'))),
    rownames = FALSE,
    filter = list(position = "none")
        )
  
  output$record_low <- DT::renderDataTable(
    low,
    options = list(
      pageLength = 20,
      scrollY = FALSE,
      columnDefs = list(list(
        targets = '_all',
        className = 'dt-center'))),
    rownames = FALSE,
    filter = list(position = "none")
  )
  
  output$record_loss <- DT::renderDataTable(
    loss,
    options = list(
      pageLength = 20,
      scrollY = FALSE,
      columnDefs = list(list(
        targets = '_all',
        className = 'dt-center'))),
    rownames = FALSE,
    filter = list(position = "none")
  )
}

shinyApp(ui, server)
