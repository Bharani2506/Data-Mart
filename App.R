#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
#########                   load libraries and data                    #########

library(shiny)
library(ggplot2)
library(readr)
library(magrittr)
library(dplyr)
library(plotly)
library(shinydashboard)

DataMart <- read_csv("DataMart.csv")

################################################################################
# SHINY APPLICATION  
################################################################################

# Define UI
ui <- fluidPage(
  titlePanel("Online Gambling: User and Product Impact on Business"),
  
  tabsetPanel(
    ### Tab 1: Geographical Impact
    tabPanel("Geographical Impact",
             selectInput("map", "Choose category:",
                         choices = c("CustomerEarning", "NumUsersperCountry"),
                         selected = "CustomerEarning"),
             plotlyOutput("Maps")
    ),
    
    ### Tab 2: User Analysis
    tabPanel("User Analysis",
             plotOutput("barChartLOR"),
             plotOutput("barChartTimeToFirstBet"),
             plotOutput("barChartCountry")
    ),
    
    ### Tab 3: LoR Related Analysis
    tabPanel("LoR Related Analysis",
             plotlyOutput("scatterPlotLORvsTotalBets"),
             plotlyOutput("scatterPlotLORvsAvgWinnings"),
             plotlyOutput("scatterPlotEarningsvsLOR"),
             plotlyOutput("scatterPlotTimesOfPlayingvsLOR"),
             plotlyOutput("scatterPlotTransactionDaysvsLOR")
    ),
    
    ### Tab 4: Gender Related Analysis
    tabPanel("Gender Related Analysis",
             plotlyOutput("pieChartGender")
    ),
    
    ### Tab 5: User Spend Analysis
    tabPanel("User Spend Analysis",
             h4("Explore how user spend category impacts business"),
             plotOutput("SpendCategoryPlot"),
             selectInput("axis", "Choose category:",
                         choices = c("AvgWinnings", "Earning"),
                         selected = "AvgWinnings"),
             plotOutput("SpendCatPlots"),
             h4("What do average betting stakes look like across users?"),
             plotOutput("AvgStakesPlot"),
             h4("How does playing time and transaction days impact earnings?"),
             selectInput("x_axis", "Choose category:",
                         choices = c("TransactionDays", "TimesOfPlaying"),
                         selected = "TimesOfPlaying"),
             plotOutput("EarningsPlots")
    ),
    
    ### Tab 6: Production Combination Analysis
    tabPanel("Production Combination Analysis",
             h4("Explore the top 10 most popular product combinations among users and their business impacts"),
             selectInput("y_axis", "Choose category:",
                         choices = c("ProductComboCount", "AvgDaysToFirstBet", "AvgTotalBets", "TotalAvgWins"),
                         selected = "ProductComboCount"),
             plotOutput("topProductComboChart")
    )
  )
)

################################################################################
# Define Server 
server <- function(input, output) {
  
  #######
  # Map: Customer Earning per Country
  output$Maps <- renderPlotly({
    country_data <- switch(input$map,
                           "CustomerEarning" = DataMart %>%
                             group_by(`Country Name`) %>%
                             summarise(value = sum(TotalSpend, na.rm = TRUE)),
                           "NumUsersperCountry" = DataMart %>%
                             group_by(`Country Name`) %>%
                             summarise(value = n())
    )
    
    plot_ly(country_data, type = "choropleth", locations = ~`Country Name`, locationmode = "country names", z = ~value) %>%
      layout(title = switch(input$map,
                            "CustomerEarning" = "Customer Earning per Country",
                            "NumUsersperCountry" = "Number of Users per Country"))
  })
  
  #######  
  # Bar chart: Length of Relationship (LOR)
  output$barChartLOR <- renderPlot({
    ggplot(DataMart, aes(x = LoR_days)) +
      geom_bar(stat = "count", fill = "#5bc0de", alpha = 0.7) +
      labs(title = "Length of Relationship (LOR) Distribution", x = "Length of Relationship (Days)", y = "Number of Users") +
      theme_minimal()
  })
  
  # Bar chart: Time to first bet
  output$barChartTimeToFirstBet <- renderPlot({
    time_to_first_bet_data <- DataMart %>% 
      group_by(UserID) %>%
      summarise(TimeToFirstBet_days = max(TimeToFirstBet_days, na.rm = TRUE))
    
    ggplot(time_to_first_bet_data, aes(x = TimeToFirstBet_days)) +
      geom_bar(stat = "count", fill = "#62c462", alpha = 0.7) +
      labs(title = "Time to First Bet Distribution", x = "Time to First Bet (Days)", y = "Count") +
      theme_minimal()
  })
  
  # Bar chart: User distribution by country
  output$barChartCountry <- renderPlot({
    country_data <- DataMart %>%
      group_by(`Country Name`) %>%
      summarise(count = n())
    
    ggplot(country_data, aes(x = reorder(`Country Name`, count), y = count)) +
      geom_bar(stat = "identity", fill = "#f0ad4e", alpha = 0.7) +
      labs(title = "User Distribution by Country", x = "Country", y = "Count") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  #######  
  # Scatter plot: LOR vs Total Bets
  output$scatterPlotLORvsTotalBets <- renderPlotly({
    scatter_data <- DataMart %>% filter(TotalBets > 0)
    
    plot_ly(scatter_data, x = ~LoR_days, y = ~TotalBets, mode = "markers",
            marker = list(size = 10, opacity = 0.8, color = ~TotalBets, colorscale = "Viridis")) %>%
      layout(title = "Scatter Plot: LOR vs Total Bets",
             xaxis = list(title = "Length of Relationship (Days)"),
             yaxis = list(title = "Total Bets"),
             paper_bgcolor = "#f5f5f5",
             font = list(color = "#333333"))
  })
  
  # Scatter plot: LOR vs Avg Winnings
  output$scatterPlotLORvsAvgWinnings <- renderPlotly({
    scatter_data <- DataMart %>% filter(AvgWinnings > 0)
    
    plot_ly(scatter_data, x = ~LoR_days, y = ~AvgWinnings, mode = "markers",
            marker = list(size = 10, opacity = 0.8, color = ~AvgWinnings, colorscale = "Viridis")) %>%
      layout(title = "Scatter Plot: LOR vs Avg Winnings",
             xaxis = list(title = "Length of Relationship (Days)"),
             yaxis = list(title = "Average Winnings"),
             paper_bgcolor = "#f5f5f5",
             font = list(color = "#333333"))
  })
  
  # Scatter plot: Earnings vs LOR
  output$scatterPlotEarningsvsLOR <- renderPlotly({
    scatter_data <- DataMart %>% filter(Earning > 0, LoR_days > 0)
    
    plot_ly(scatter_data, x = ~LoR_days, y = ~Earning, mode = "markers",
            marker = list(size = 10, opacity = 0.8, color = ~Earning, colorscale = "Viridis")) %>%
      layout(title = "Scatter Plot: Earnings vs LOR",
             xaxis = list(title = "Length of Relationship (Days)"),
             yaxis = list(title = "Earnings"),
             paper_bgcolor = "#f5f5f5",
             font = list(color = "#333333"))
  })
  
  # Scatter plot: Times of Playing vs LOR
  output$scatterPlotTimesOfPlayingvsLOR <- renderPlotly({
    scatter_data <- DataMart %>% filter(TimesOfPlaying > 0, LoR_days > 0)
    
    plot_ly(scatter_data, x = ~LoR_days, y = ~TimesOfPlaying, mode = "markers",
            marker = list(size = 10, opacity = 0.8, color = ~TimesOfPlaying, colorscale = "Viridis")) %>%
      layout(title = "Scatter Plot: Times of Playing vs LOR",
             xaxis = list(title = "Length of Relationship (Days)"),
             yaxis = list(title = "Times of Playing"),
             paper_bgcolor = "#f5f5f5",
             font = list(color = "#333333"))
  })
  
  # Scatter plot: Transaction Days vs LOR
  output$scatterPlotTransactionDaysvsLOR <- renderPlotly({
    scatter_data <- DataMart %>% filter(TransactionDays > 0, LoR_days > 0)
    
    plot_ly(scatter_data, x = ~LoR_days, y = ~TransactionDays, mode = "markers",
            marker = list(size = 10, opacity = 0.8, color = ~TransactionDays, colorscale = "Viridis")) %>%
      layout(title = "Scatter Plot: Transaction Days vs LOR",
             xaxis = list(title = "Length of Relationship (Days)"),
             yaxis = list(title = "Transaction Days"),
             paper_bgcolor = "#f5f5f5",
             font = list(color = "#333333"))
  })
  
  #######
  # Pie chart: Gender distribution
  output$pieChartGender <- renderPlotly({
    gender_data <- DataMart %>%
      group_by(Gender) %>%
      summarise(count = n())
    
    plot_ly(gender_data, labels = ~Gender, values = ~count, type = "pie", hole = 0.6)
  })
  
  #######
  # Pie chart: Spend Category 
  output$SpendCategoryPlot <- renderPlot({
    
    pie_data <- table(DataMart$SpendCategory)
    pie_chart <- pie(pie_data, main = "Distribution of Spending Categories Among Users")
    pie_chart
  })
  
  # Bar chart: Spend category plot 
  output$SpendCatPlots <- renderPlot({
    
    ggplot(DataMart, aes(x = SpendCategory, y = !!sym(input$axis), fill = SpendCategory)) +
      geom_bar(stat = "identity") +
      labs(title = paste(input$axis,"by Spend Category Bar Chart"), x = "Spend Category", y = input$axis) +
      theme_minimal()
  })
  
  # Plot: Average Stakes 
  output$AvgStakesPlot <- renderPlot({
    filtered_data <- DataMart %>% filter(AvgStakes > 0)
    
    ggplot(filtered_data, aes(x = UserID, y = AvgStakes)) +
      geom_point(color = "skyblue", alpha = 0.7) +
      labs(title = "Distribution of Average Stakes per User", x = "User ID", y = "Average Stakes") +
      theme_minimal()
  })
  
  # Plot: Earnings Related 
  output$EarningsPlots <- renderPlot({
    
    ggplot(DataMart, aes(x = !!sym(input$x_axis), y = Earning)) +
      geom_point(color = "skyblue", alpha = 0.5) +
      labs(title = paste(input$x_axis, " vs Earning"), x = input$x_axis, y = "Earning") +
      theme_minimal()
  })
  
  #######  
  # Bar chart: Top 10 product combo
  output$topProductComboChart <- renderPlot({
    # Get the top 10 rows based on ProductComboCount
    top_10_products <- DataMart %>%
      select(AllProductDescriptions, ProductComboCount, !!input$y_axis) %>%
      distinct(AllProductDescriptions, .keep_all = TRUE) %>%
      arrange(desc(ProductComboCount)) %>%
      head(10)
    # Create the bar graph
    ggplot(top_10_products, aes(x = reorder(AllProductDescriptions, -ProductComboCount), y = !!sym(input$y_axis), fill = AllProductDescriptions)) +
      geom_bar(stat = "identity") +
      labs(title = "Top 10 Most Common Product Combos",
           x = NULL,
           y = input$y_axis) +
      theme_minimal() +
      theme(axis.text.x = element_blank(),  # Hide x-axis labels
            legend.title = element_blank(),  # Hide legend title
            legend.position = "right")
  }, width = 1000, height = 400)
  
}


# Run the application 
shinyApp(ui = ui, server = server)

  