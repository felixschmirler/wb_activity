# Load required libraries
library(shiny)
library(DT)
library(ggplot2)
library(dplyr)

# Define UI
ui <- fluidPage(
    titlePanel("Activity Entry and Persistence"),
    sidebarLayout(
        sidebarPanel(
            textInput("name", "Name"),
            textInput("activity", "Activity"),
            numericInput("minutes", "Minutes", value = 0),
            numericInput("meters", "Meters", value = 0),
            numericInput("wellbeing", "Wellbeing", value = 0),
            dateInput("date", "Date", value = Sys.Date()),
            actionButton("addBtn", "Add Entry"),
            actionButton("clearBtn", "Clear Entries"),
            actionButton("saveBtn", "Save Data")
        ),
        mainPanel(
            tabsetPanel(
                tabPanel("Data Table", DTOutput("dataTable")),
                tabPanel("Wellbeing Graph", plotOutput("wellbeingPlot")),
                tabPanel("Activity Graph", plotOutput("activityPlot"))
            )
        )
    )
)

# Define server logic
server <- function(input, output, session) {
    
    # Initialize data storage
    data <- reactiveValues(
        entries = data.frame(Name = character(), Activity = character(), Minutes = numeric(),
                             Meters = numeric(), Wellbeing = numeric(),
                             Date = as.Date(character()), stringsAsFactors = FALSE)
    )
    
    # Add entry button functionality
    observeEvent(input$addBtn, {
        if (input$name != "" && input$activity != "" && input$minutes >= 0 && input$meters >= 0 &&
            input$wellbeing >= 0 && !is.null(input$date)) {
            new_entry <- data.frame(Name = input$name, Activity = input$activity, 
                                    Minutes = input$minutes, Meters = input$meters,
                                    Wellbeing = input$wellbeing, Date = input$date)
            data$entries <- rbind(data$entries, new_entry)
        }
        # Reset input fields
        updateTextInput(session, "name", value = "")
        updateTextInput(session, "activity", value = "")
        updateNumericInput(session, "minutes", value = 0)
        updateNumericInput(session, "meters", value = 0)
        updateNumericInput(session, "wellbeing", value = 0)
        updateDateInput(session, "date", value = Sys.Date())
    })
    
    # Clear entries button functionality
    observeEvent(input$clearBtn, {
        data$entries <- data.frame(Name = character(), Activity = character(), Minutes = numeric(),
                                   Meters = numeric(), Wellbeing = numeric(),
                                   Date = as.Date(character()), stringsAsFactors = FALSE)
    })
    
    # Save data button functionality
    observeEvent(input$saveBtn, {
        if (!is.null(data$entries)) {
            saveRDS(data$entries, "saved_data.rds")
        }
    })
    
    # Render the data table
    output$dataTable <- renderDT({
        datatable(data$entries, editable = TRUE)
    })
    
    # Generate the wellbeing score plot
    output$wellbeingPlot <- renderPlot({
        data$entries %>%
            ggplot(aes(x = Date, y = Wellbeing, color = Name)) +
            geom_line(size = 1) + # Make lines thicker
            geom_point(size = 3) + # Add markers for data points
            stat_summary(fun = mean, geom = "line", aes(group = 1), size = 1, color = "black", linetype = "dashed") +
            labs(title = "Wellbeing over Time", x = "Date", y = "Wellbeing") +
            theme(legend.position = "top")
    })
    
    # Generate the activity plot
    output$activityPlot <- renderPlot({
        data$entries %>%
            group_by(Name) %>%
            summarise(TotalMinutes = sum(Minutes)) %>%
            ggplot(aes(x = Name, y = TotalMinutes, fill = Name)) +
            geom_bar(stat = "identity") +
            labs(title = "Total Activity Minutes by Name", x = "Name", y = "Total Minutes") +
            theme(axis.text.x = element_text(angle = 45, hjust = 1))
    })
    
    # Load data on session start
    if (file.exists("saved_data.rds")) {
        data$entries <- readRDS("saved_data.rds")
    }
}

# Run the application
shinyApp(ui = ui, server = server)
