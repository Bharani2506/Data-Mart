# Betting Company Marketing Data Mart

## Overview
This repository houses an R Shiny application designed to provide interactive marketing insights at a customer level for a betting company. The application leverages detailed marketing metrics to offer a deep dive into user demographics, betting habits, and product interaction patterns.

## Data Mart Structure
The data mart is structured around a comprehensive table with one row per customer. Key metrics include:
- **User Demographics**: Country, language, gender.
- **Activity Metrics**: Dates of first activities, total bets, total winnings, and stakes.
- **Engagement Metrics**: Product usage, days since first and last bet, loyalty calculations.
- **Financial Metrics**: Spending categories, earnings per user, average bets.

### Table of Marketing Metrics
Included in the data mart are metrics such as:
- `UserID`: Unique identifier for each user.
- `TotalAggDays`: Total days of active betting.
- `AvgWinnings`: Average winnings per bet.
- `SpendCategory`: Categorization of spending into low, medium, high, or profit risk.

## Interactive Application Features
The Shiny app includes several tabs each focusing on different aspects of user data:
- **Geographical Impact**: Visualizes user distribution and earnings by country.
- **User Analysis**: Analyzes user retention and betting initiation.
- **Length of Relationship**: Examines the correlation between user engagement and duration of the betting relationship.
- **Gender Insights**: Breakdown of user demographics by gender.
- **Spending Categorization**: Analysis of spending behaviors across different categories.
- **Product Combination Insights**: Overview of the most common product combinations and their performance.

## Accessing the Application
The application can be accessed at [this link](https://datamart-group2.shinyapps.io/datamart_app/). The app is interactive and allows for dynamic filtering and exploration of the data.

## Setup and Installation
To run this application locally:
1. Clone this repository.
2. Ensure you have R and the necessary packages installed (`shiny`, `dplyr`, `ggplot2`, `plotly`).
3. Run the app by opening the app script in RStudio and clicking 'Run App'.

## Contributions
Contributions are welcome. Please submit a pull request or create an issue if you find any bugs or have suggestions for additional metrics or features.

## License
This project is licensed under the MIT License - see the [LICENSE.md](LICENSE) file for details.

## Contact
For further inquiries, please reach out via GitHub issues or directly through the repository's contact links.
