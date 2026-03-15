library(shiny)
library(dplyr)
library(readr)
library(bslib)

# Load data
df <- read_csv("data/mars-weather.csv")

month_choices <- unique(df$month)
month_choices <- month_choices[
  order(as.numeric(gsub("Month ", "", month_choices)))
]
month_choices <- c("All", month_choices)

#ui
ui <- page_fluid(
  
  theme = bs_theme(
    bootswatch = "darkly",
    bg = "#222222",
    fg = "#FFAD70"
  ),
  
  h1("Mars Pressure Dashboard", style = "text-align:center; color:#FFAD70; margin-bottom:30px;"),
  
  fluidRow(
    column(
      width = 4,
      selectInput("month", "Select Martian Month", choices = month_choices, selected = "All")
    ),
    column(
      width = 8,  
      value_box(title = "Average Pressure", value = textOutput("avg_pressure"), theme_color = "secondary"),
      
      plotOutput("pressure_plot", height = "300px")
    )
  )
)

server <- function(input, output, session) {
  
  filtered_month <- reactive({
    
    if (input$month == "All") {
      df
    } else {
      df %>% filter(month == input$month)
    }
    
  })
  
  output$avg_pressure <- renderText({
    data <- filtered_month()
    if (nrow(data) == 0) {
      return("N/A")
    }
    paste0(round(mean(data$pressure, na.rm = TRUE), 2), " Pa")
  })
  
  output$pressure_plot <- renderPlot({
    
    data <- filtered_month()
    
    plot(
      data$terrestrial_date,
      data$pressure,
      type = "l",
      col = "#FFAD70",
      lwd = 2,
      xlab = "Date",
      ylab = "Pressure (Pa)",
      main = "Mars Atmospheric Pressure Over Time"
    )
    
  })
}

shinyApp(ui = ui, server = server)
