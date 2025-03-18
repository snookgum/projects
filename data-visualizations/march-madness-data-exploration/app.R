#---------------------------------------------------------------------------------
#Packages
library(shiny)
library(tidyverse)
library(ggthemes)
library(dplyr)
#---------------------------------------------------------------------------------
#Data
CBB_13_19 <- read.csv("cbb.csv", header = TRUE, sep = ",")
CBB_21 <- read.csv("cbb21.csv", header = TRUE, sep = ",")

#Merge 2021 and 2013-19 data
CBB <- rbind(CBB_13_19, CBB_21)

#filter out teams that didn't make it to playoffs
CBB <- CBB[!is.na(CBB$POSTSEASON),]

#filter out teams that didn't make it to official round of 64
CBB <- CBB %>% filter(POSTSEASON != 'R68')

#create new variable for regular season win %
CBB <- CBB %>% mutate(WINPERC = round((W/G*100), 1))

#change SEED from int to a double
CBB$SEED <- as.numeric(as.character(CBB$SEED))

#create new column for POSTSEASON result (relabel)
CBB$RESULT[CBB$POSTSEASON == "Champions"] <- "1st (Champions)"
CBB$RESULT[CBB$POSTSEASON == "2ND"] <- "2nd (Runner-Up)"
CBB$RESULT[CBB$POSTSEASON == "F4"] <- "4 (Final Four)"
CBB$RESULT[CBB$POSTSEASON == "E8"] <- "8 (Elite Eight)"
CBB$RESULT[CBB$POSTSEASON == "S16"] <- "Top 16 (Sweet Sixteen)"
CBB$RESULT[CBB$POSTSEASON == "R32"] <- "Top 32"
CBB$RESULT[CBB$POSTSEASON == "R64"] <- "Top 64"

#create new dataset for different bracket levels
CBB_overall <- CBB

CBB_R64 <- CBB %>%
  mutate(
    RESULT = ifelse(POSTSEASON != "R64", "Advanced", "Eliminated")
  )

CBB_R32 <- CBB %>%
  filter(POSTSEASON != 'R64') %>%
  mutate(
    RESULT = ifelse(POSTSEASON != "R32", "Advanced", "Eliminated")
  )

CBB_S16 <- CBB %>%
  filter(POSTSEASON != 'R64' & POSTSEASON != 'R32') %>%
  mutate(
    RESULT = ifelse(POSTSEASON != "S16", "Advanced", "Eliminated")
  )

CBB_E8 <- CBB %>%
  filter(POSTSEASON != 'R64' & POSTSEASON != 'R32' & POSTSEASON != 'S16') %>%
  mutate(
    RESULT = ifelse(POSTSEASON != "E8", "Advanced", "Eliminated")
  )

CBB_F4 <- CBB %>%
  filter(POSTSEASON != 'R64' & 
           POSTSEASON != 'R32' & 
           POSTSEASON != 'S16' &
           POSTSEASON != 'E8') %>%
  mutate(
    RESULT = ifelse(POSTSEASON != "F4", "Advanced", "Eliminated")
  )

CBB_T2 <- CBB %>%
  filter(POSTSEASON == '2ND' | POSTSEASON == 'Champions') %>%
  mutate(
    RESULT = ifelse(POSTSEASON != "2ND", "Champion", "Runner-Up")
  )
#---------------------------------------------------------------------------------
#UI
ui <- fluidPage(
  
  #Application title
  titlePanel(strong("NCAA Men's Basketball Tournament Results & Statistics")),
  
  #Subtitles
  h5("Based on data from 2013-2021 tournaments"),
  h6(em("***2020 data is omitted due to Covid-19 and the cancellation of the tournament***")),
  br(),
  h5(strong("Overview: ")),
  h6("You can select various options in the right panel. Select the level of the tournament you're interested in and the variable you'd like to analyze."),
  h6("Number of bins indicates how many histogram bars will be generated. The output histogram shows the breakdown of tournament results for each bar."),
  
  #Sidebar
  sidebarLayout(
    position = "right",
    
    sidebarPanel(
      
      #radio button
      radioButtons(
        inputId = "var_data",
        label = "Bracket Level to Explore:",
        choices = list("Overall",
                       "Championship Game",
                       "Final 4",
                       "Elite 8",
                       "Sweet 16",
                       "Round of 32",
                       "Round of 64"
                       ),
        selected = "Overall"
      ),
      
      #drop down
      selectInput(
        inputId = "var",
        label = "Team Statistic to Explore:",
        choices = list("Seed", 
                       "Regular Season Win %",
                       "Power Rating", 
                       "Wins Above Bubble",
                       "Adjusted Offensive Efficiency",
                       "Adjusted Defensive Efficiency",
                       "Effective Field Goal %",
                       "Effective Field Goal % Allowed",
                       "3-Point Shooting %",
                       "3-Point Shooting % Allowed",
                       "Free Throw Shooting %",
                       "Free Throw Shooting % Allowed",
                       "Offensive Rebound Rate",
                       "Offensive Rebound Rate Allowed"
        ),
        selected = "Seed"
      ),
      
      #slider
      sliderInput("bins",
                  "Number of Bins:",
                  min = 2,
                  max = 50,
                  value = 16),
    ),
    
    #Main
    mainPanel(
      # Output
      plotOutput("distPlot"),
      h6("Data sourced from: ", a("www.kaggle.com/andrewsundberg/college-basketball-dataset?resource=download ")), 
      h6("Originally scraped from: ", a("barttorvik.com/trank.php#")),
      br(),
      br()
    )
  )
)
#---------------------------------------------------------------------------------
#Server
    server <- function(input, output) {
    
    output$distPlot <- renderPlot({
      
      data_var <- switch(input$var_data,
                         "Overall" = CBB_overall,
                         "Round of 64" = CBB_R64,
                         "Round of 32" = CBB_R32,
                         "Sweet 16" = CBB_S16,
                         "Elite 8" = CBB_E8,
                         "Final 4" = CBB_F4,
                         "Championship Game" = CBB_T2
                         
      )
      
      color_var <- switch(input$var_data,
                          "Overall" = c("#FF3333", "#FF9933", "#FFFF33", "#33FF99", "#3399FF", "#9933FF", "#FF33FF"),
                          "Round of 64"  = c("#3399FF", "#FF3333"),
                          "Round of 32"  = c("#3399FF", "#FF3333"),
                          "Sweet 16"  = c("#3399FF", "#FF3333"),
                          "Elite 8" = c("#3399FF", "#FF3333"),
                          "Final 4" = c("#3399FF", "#FF3333"),
                          "Championship Game" = c("#FFD700", "#C0C0C0")
      )
      
      var_hist <- case_when(
        input$var == "Seed" ~ pull(data_var, SEED),
        input$var == "Regular Season Win %" ~ pull(data_var, WINPERC),
        input$var == "Power Rating" ~ pull(data_var, BARTHAG),
        input$var == "Effective Field Goal %" ~ pull(data_var, EFG_O),
        input$var == "Effective Field Goal % Allowed" ~ pull(data_var, EFG_D),
        input$var == "3-Point Shooting %" ~ pull(data_var, X3P_O),
        input$var == "3-Point Shooting % Allowed" ~ pull(data_var, X3P_D),
        input$var == "Free Throw Shooting %" ~ pull(data_var, FTR),
        input$var == "Free Throw Shooting % Allowed" ~ pull(data_var, FTRD),
        input$var == "Offensive Rebound Rate" ~ pull(data_var, ORB),
        input$var == "Offensive Rebound Rate Allowed" ~ pull(data_var, DRB),
        input$var == "Adjusted Offensive Efficiency" ~ pull(data_var, ADJOE),
        input$var == "Adjusted Defensive Efficiency" ~ pull(data_var, ADJDE),
        input$var == "Wins Above Bubble" ~ pull(data_var, WAB)
      )
      
      x_label <- switch(input$var,
                        "Seed" = "Tournament Seed (1-16)", 
                        "Regular Season Win %" = "Regular Season Win %",
                        "Power Rating" = "Power Rating \n(chance of beating Division I team)", 
                        "Effective Field Goal %" = "Effective Field Goal %",
                        "Effective Field Goal % Allowed" = "Effective Field Goal % Allowed",
                        "3-Point Shooting %" = "3-Point Shooting %",
                        "3-Point Shooting % Allowed" = "3-Point Shooting % Allowed",
                        "Free Throw Shooting %" = "Free Throw Shooting %",
                        "Free Throw Shooting % Allowed" = "Free Throw Shooting % Allowed",
                        "Offensive Rebound Rate" = "Offensive Rebound Rate \n(rebounds/game)",
                        "Offensive Rebound Rate Allowed" = "Offensive Rebound Rate Allowed \n(rebounds/game)",
                        "Adjusted Offensive Efficiency" = "Adjusted Offensive Efficiency \n(pts scored/100 possessions vs average Division I team)",
                        "Adjusted Defensive Efficiency" = "Adjusted Defensive Efficiency \n(pts allowed/100 possessions vs average Division I team)",
                        "Wins Above Bubble" = "Wins Above Bubble"
      )
      
      x <- var_hist
      
      #for seed, fix the x-axis to 1-16
      if(input$var == "Seed"){
        bin_breaks <- seq(0, 16, length.out = input$bins+1) 
      } else{
        #for other vars, let x-axis adjust
        bin_breaks <- seq(min(x), max(x), length.out = input$bins+1)
      }
      
      # draw the histogram with the specified number of bins
      ggplot(data = data_var, aes(x = x)) +
        geom_histogram(breaks = bin_breaks,
                       aes_string(fill = "RESULT"),
                       color = "black") +
        labs(
          x = x_label,
          y = 'Count'
        ) +
        scale_fill_manual(
          name = "RESULT",
          values = color_var,
        ) +
        theme_fivethirtyeight() +
        theme(
          axis.title = element_text(),
          legend.position = "right",
          legend.direction = "vertical",
          legend.title.align = 0.5,
        ) +
        guides(fill = guide_legend(title.position = "top"))
    })
  }
#---------------------------------------------------------------------------------
#Run
shinyApp(ui = ui, server = server)
