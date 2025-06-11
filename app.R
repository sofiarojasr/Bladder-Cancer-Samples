library(shiny)
library(dplyr)
library(DT)
library(ggplot2)
library(forcats)
library(RColorBrewer)

# ✅ Load the data from GitHub (not local)
url <- "https://raw.githubusercontent.com/sofiarojasr/Bladder-Cancer-Samples/main/bladder_samples.csv"
bladder_samples <- read.csv(url)

# UI
ui <- fluidPage(
  titlePanel("Bladder Sample Data Viewer"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("Data from bladder_samples.csv")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Table", DT::dataTableOutput("bladder_data")),
        tabPanel("Histology Pie Chart", plotOutput("hist_pie")),
        tabPanel("Histology Bar Chart", plotOutput("hist_bar"))
      )
    )
  )
)

# Server
server <- function(input, output) {
  
  # 📋 Table
  output$bladder_data <- renderDataTable({
    bladder_samples %>%
      select(turbt_cystectomy_JD, histology_JD, OrigMetaID, batchflex_study) %>%
      datatable(options = list(pageLength = 10))
  })
  
  # 🥧 Pie Chart
  output$hist_pie <- renderPlot({
    hist_data <- bladder_samples %>%
      count(histology_JD) %>%
      arrange(desc(n)) %>%
      mutate(
        percent = n / sum(n) * 100,
        label = paste0(histology_JD, " (", round(percent, 1), "%)")
      )
    
    ggplot(hist_data, aes(x = "", y = n, fill = histology_JD)) +
      geom_col(width = 1, color = "white") +
      coord_polar(theta = "y") +
      scale_fill_manual(values = colorRampPalette(brewer.pal(8, "Pastel1"))(length(hist_data$histology_JD))) +
      labs(title = "Histology Type Distribution") +
      theme_void() +
      theme(legend.title = element_blank(),
            plot.title = element_text(hjust = 0.5, size = 16, face = "bold"))
  })
  
  # 📊 Bar Chart
  output$hist_bar <- renderPlot({
    bladder_samples %>%
      count(histology_JD) %>%
      ggplot(aes(x = reorder(histology_JD, -n), y = n)) +
      geom_bar(stat = "identity", fill = "#AED6F1") +
      labs(title = "Number of Samples by Histology",
           x = "Histology Type",
           y = "Sample Count") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
}

# ✅ Run the app
shinyApp(ui = ui, server = server)
