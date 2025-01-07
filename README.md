# Play Store Games Analysis Shiny App

This Shiny web application provides an interactive analysis of Google Play Store game data from 2023. It allows users to explore univariate and multivariate aspects of the dataset, including histograms, statistics, and category-based comparisons.

## Features

- **Univariate Analyses:**
  - Choose different variables (Total Ratings, Numeric Installs, Average Rating, Rate Ratio).
  - Customize histogram color and bin width.
  - Display key statistics like mean, standard deviation, five-number summary.
  - Identify games with the highest and lowest values.
  - Optionally display a related image.

- **Multivariate Analyses:**
  - Analyze variables (Rate Ratio, Numeric Installs, Growth 30 Days, Growth 60 Days) across game categories.
  - Visualize data with boxplots.
  - Optionally view summary tables grouped by category.

## Getting Started

### Prerequisites

Make sure you have the following R packages installed:

```r
install.packages(c("ggplot2", "dplyr", "tibble", "shiny", "rsconnect"))
```

### Data

Place the dataset file `android-games.csv` in the project directory. This CSV file should contain Google Play Store game data.

### Running the App

1. Open the project in RStudio or another R environment.
2. Load the necessary libraries and source the code, or simply run the provided script.
3. Start the Shiny app by running:

   ```r
   shiny::runApp()
   ```

   or if you've sourced the script, the last line `shinyApp(ui = ui, server = server)` will launch the application automatically.

4. Interact with the app through your web browser to explore the dataset:

https://mpianrhko0129.shinyapps.io/filmspark/

## Project Structure

- **app.R**: Main script containing both UI and server logic for the Shiny application.
- **android-games.csv**: Data file with Google Play Store games information.

## Author

Minho Park
