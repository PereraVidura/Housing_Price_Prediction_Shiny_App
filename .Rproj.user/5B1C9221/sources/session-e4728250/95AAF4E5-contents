# Load necessary libraries
library(shiny)
library(shinydashboard)
library(randomForest)  # You can replace this with the same regression algorithm used in the model
library(ggplot2)
library(DBI)
library(RMySQL)

# Load the saved model
model <- readRDS("housing_price_model.rds")

# Create a database connection
con <- dbConnect(RMySQL::MySQL(), 
                 user = 'root', 
                 password = '', 
                 dbname = 'housing_db', 
                 host = '127.0.0.1')

# Define UI for the Shiny app
ui <- dashboardPage(
  dashboardHeader(
    title = "Housing Price Prediction",
    titleWidth = 250
  ),
  dashboardSidebar(
    width = 250,
    # Input fields for relevant features
    numericInput("longitude", "Longitude:", value = -122.25),
    numericInput("latitude", "Latitude:", value = 37.85),
    numericInput("housing_median_age", "Housing Median Age:", value = 30),
    numericInput("total_rooms", "Total Rooms:", value = 4000),
    numericInput("total_bedrooms", "Total Bedrooms:", value = 800),
    numericInput("population", "Population:", value = 2000),
    numericInput("households", "Households:", value = 700),
    numericInput("median_income", "Median Income:", value = 3.4296, step = 0.0001),  # Change step to 0.0001
    selectInput("ocean_proximity", "Ocean Proximity:", choices = unique(california_housing$ocean_proximity), selected = "NEAR BAY"),
    # Add more input fields as needed based on your features
    actionButton("predict_button", "Predict", class = "btn-success")  # Apply a success button style (green)
  ),
  dashboardBody(
    box(
      title = "Predicted Housing Price",
      status = "primary",
      solidHeader = TRUE,
      width = 7,
      height = 150,
      textOutput("predicted_price")  # Use textOutput instead of h3Output
    ),
    box(
      title = "Scatter Plot",
      status = "info",
      solidHeader = TRUE,
      width = 12,
      height = 500,
      plotOutput("prediction_plot")
    )
  )
)

# Define server logic
server <- function(input, output) {
  predictions <- eventReactive(input$predict_button, {
    new_data <- data.frame(
      longitude = input$longitude,
      latitude = input$latitude,
      housing_median_age = input$housing_median_age,
      total_rooms = input$total_rooms,
      total_bedrooms = input$total_bedrooms,
      population = input$population,
      households = input$households,
      median_income = input$median_income,
      ocean_proximity = input$ocean_proximity
      # Add more features as needed based on your model
    )
    
    predicted_price <- predict(model, new_data)
    
    # Store user inputs and predictions in the database
    dbWriteTable(con, "predictions_table", 
                 data.frame(
                   Longitude = new_data$longitude,
                   Latitude = new_data$latitude,
                   Housing_Median_Age = new_data$housing_median_age,
                   Total_Rooms = new_data$total_rooms,
                   Total_Bedrooms = new_data$total_bedrooms,
                   Population = new_data$population,
                   Households = new_data$households,
                   Median_Income = new_data$median_income,
                   Ocean_Proximity = new_data$ocean_proximity,
                   Predicted_Price = predicted_price
                 ), 
                 append = TRUE)
    
    return(predicted_price)
  })
  
  # Display the predicted housing price with styling
  output$predicted_price <- renderText({
    if (input$predict_button > 0) {
      paste("Predicted Housing Price: $", round(predictions(), 2))
    }
  })
  
  # Generate scatter plot using ggplot2
  output$prediction_plot <- renderPlot({
    if (input$predict_button > 0) {
      input_data <- data.frame(
        Median_Income = rep(input$median_income, 100),
        Predicted_Price = rep(predictions(), 100)
      )
      
      # Remove rows with missing values
      input_data <- na.omit(input_data)
      
      ggplot(input_data, aes(x = Median_Income, y = Predicted_Price)) +
        geom_point(color = "#1f77b4", size = 3) +
        labs(
          x = "Median Income",
          y = "Predicted Price",
          title = "Scatter Plot of Predicted Prices against Median Income"
        ) +
        theme_minimal() +
        theme(
          panel.grid.major = element_line(color = "lightgray", linetype = "dashed"),
          panel.grid.minor = element_blank(),
          panel.background = element_rect(fill = "white"),
          axis.line = element_line(color = "black")
        )
    }
  })
}

# Close the database connection when the Shiny app is stopped
onStop(function() {
  dbDisconnect(con)
})

# Run the Shiny app
shinyApp(ui = ui, server = server)
