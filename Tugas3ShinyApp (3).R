library(shiny)
library(ggplot2)
library(DT)


data <- read.csv("C:/Users/LENOVO/Desktop/Tugas3AVD/weather.csv", sep = ";")

numeric_vars <- names(data)[sapply(data, is.numeric)]
all_vars <- names(data)

ui <- fluidPage(
  titlePanel("Visualisasi Data Interaktif"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("xvar", "Pilih Variabel X:", choices = all_vars),
      selectInput("yvar", "Pilih Variabel Y (hanya scatter/line):",
                  choices = numeric_vars),
      
      radioButtons(
        "plotType",
        "Jenis Visualisasi:",
        choices = c(
          "Scatter Plot" = "scatter",
          "Line Plot"    = "line",
          "Bar Plot"     = "bar",
          "Tabel Data"   = "table"
        )
      )
    ),
    
    mainPanel(
      uiOutput("outputUI")
    )
  )
)

server <- function(input, output) {
  
  output$outputUI <- renderUI({
    if (input$plotType == "table") {
      DTOutput("dataTable")
    } else {
      plotOutput("plot")
    }
  })
  
  # Render tabel
  output$dataTable <- renderDT({
    datatable(data)
  })
  
  # Render plot
  output$plot <- renderPlot({
    x <- input$xvar
    y <- input$yvar
    
    # SCATTER
    if (input$plotType == "scatter") {
      ggplot(data, aes_string(x = x, y = y)) +
        geom_point(color = "blue", size = 3) +
        theme_minimal()
    }
    # LINE
    else if (input$plotType == "line") {
      ggplot(data, aes_string(x = x, y = y)) +
        geom_line(color = "red", size = 1) +
        geom_point(color = "black", size = 2) +
        theme_minimal()
    }
    # BAR (auto: bar untuk kategorik, histogram untuk numerik)
    else if (input$plotType == "bar") {
      
      if (!is.numeric(data[[x]])) {
        ggplot(data, aes_string(x = x)) +
          geom_bar(fill = "green", color="black", bindwidth = 0.5) +
          theme_minimal()
      } else {
        ggplot(data, aes_string(x = x)) +
          geom_histogram(fill = "green", color="black", bindwidth = 0.5) +
          theme_minimal()
      }
    }
  })
}

shinyApp(ui, server)
