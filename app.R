# Load required libraries
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(dplyr)
library(ggplot2)
library(tidyr)
library(DT)
library(rsconnect)
# UI
ui <- dashboardPage(
  dashboardHeader(title = "Olympic Games Analysis"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Medal Counts", tabName = "medals", icon = icon("medal")),
      menuItem("Athletes vs Events", tabName = "athletes", icon = icon("running"))
    ),
    br(),
    pickerInput("sports", "Select Sports:",
                choices = NULL,
                multiple = TRUE,
                options = list(
                  `actions-box` = TRUE,
                  `select-all-text` = "Select All",
                  `deselect-all-text` = "Deselect All",
                  `none-selected-text` = "No sports selected"
                )),
    pickerInput("nocs", "Select NOCs:",
                choices = NULL,
                multiple = TRUE,
                options = list(
                  `actions-box` = TRUE,
                  `select-all-text` = "Select All",
                  `deselect-all-text` = "Deselect All",
                  `none-selected-text` = "All NOCS are selected"
                )),
    sliderInput("year_range", "Select Year Range:",
                min = 1896, max = 2016, 
                value = c(1896, 2016),
                step = 4,
                sep = ""),
    radioButtons("gender", "Select Gender:",
                 choices = c("Both" = "both", "Male" = "M", "Female" = "F"),
                 selected = "both")
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "medals",
              fluidRow(
                box(width = 12, 
                    textOutput("medal_overview"),
                    plotOutput("medal_plot", height = "600px")
                ),
                box(width = 12,
                    h3("Medals Scoreboard by NOCs"),
                    dataTableOutput("medal_table")
                )
              )
      ),
      tabItem(tabName = "athletes",
              fluidRow(
                box(width = 12, 
                    textOutput("athlete_overview"),
                    plotOutput("scatter_plot", height = "600px"))
              )
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Read the data
  data <- reactive({
    read.csv("athlete_events.csv")
  })
  
  # Update sports choices
  observe({
    sports <- sort(unique(data()$Sport))
    updatePickerInput(session, "sports", choices = sports, selected = sports)
  })
  
  # Update NOC choices
  observe({
    nocs <- sort(unique(data()$NOC))
    updatePickerInput(session, "nocs", choices = nocs)
  })
  
  # Filter data based on selected sports, NOCs, years, and gender
  filtered_data <- reactive({
    req(input$sports, input$year_range)
    d <- data() %>% 
      filter(Sport %in% input$sports,
             Year >= input$year_range[1],
             Year <= input$year_range[2])
    
    if (length(input$nocs) > 0) {
      d <- d %>% filter(NOC %in% input$nocs)
    }
    
    if (input$gender != "both") {
      d <- d %>% filter(Sex == input$gender)
    }
    
    d
  })
  
  # Process data for medal counts
  medal_data <- reactive({
    filtered_data() %>%
      filter(Season=="Summer") %>%
      filter(!is.na(Medal)) %>%
      distinct(NOC, Event, Games, Medal, Sex) %>%
      group_by(NOC, Sex) %>%
      summarize(
        Gold = sum(Medal == "Gold", na.rm = TRUE),
        Silver = sum(Medal == "Silver", na.rm = TRUE),
        Bronze = sum(Medal == "Bronze", na.rm = TRUE),
        Total = n()
      ) %>%
      arrange(desc(Total))
  })
  
  # Process data for scatter plot
  games_data <- reactive({
    filtered_data() %>%
      filter(Season=="Summer") %>%
      group_by(Games, Year, City, Sex) %>%
      summarize(
        num_athletes = n_distinct(ID),
        num_events = n_distinct(Event)
      ) %>%
      ungroup()
  })
  
  # Render medal plot overview
  output$medal_overview <- renderText({
    total_medals <- sum(medal_data()$Total)
    top_noc <- medal_data() %>% 
      group_by(NOC) %>% 
      summarise(Total = sum(Total)) %>% 
      arrange(desc(Total)) %>% 
      slice(1) %>% 
      pull(NOC)
    
    paste(
      "This chart shows the distribution of Olympic medals across different National Olympic Committees (NOCs) in Summer Olympics.",
      "The total number of medals awarded in the selected time period and sports is", total_medals, ".",
      "The NOC with the highest medal count is", top_noc, ".",
      "Medals are stacked in order of Gold, Silver, Bronze from bottom to top.",
      sep = "\n"
    )
  })
  
  # Render medal plot
  output$medal_plot <- renderPlot({
    req(nrow(medal_data()) > 0)
    plot_data <- medal_data() %>%
      pivot_longer(cols = c(Gold, Silver, Bronze), names_to = "Medal", values_to = "Count") %>%
      mutate(Medal = factor(Medal, levels = c("Bronze", "Silver", "Gold")))
    
    p <- ggplot(plot_data, aes(x = reorder(NOC, Total), y = Count, fill = Medal)) +
      geom_bar(stat = "identity", position = "stack") +
      scale_fill_manual(values = c("Bronze" = "brown", "Silver" = "gray70", "Gold" = "gold")) +
      labs(title = "Medal Counts by Nation",
           subtitle = paste("Sports Played:", paste(input$sports, collapse = ", "),
                            "\nTime Period:", input$year_range[1], "-", input$year_range[2],
                            "\nData Source: https://www.kaggle.com/datasets/heesoo37/120-years-of-olympic-history-athletes-and-results"),
           x = "NOC",
           y = "Number of Medals") +
      theme_minimal() +
      theme(plot.title = element_text(size = 16, face = "bold"),
            plot.subtitle = element_text(size = 10)) + 
      coord_flip()
    
    if (input$gender == "both") {
      p + facet_wrap(~ Sex, scales = "free_x", ncol = 2)
    } else {
      p
    }
  })
  
  # Render medal counts table
  output$medal_table <- DT::renderDT({
    data <- medal_data() %>%
      select(NOC, Sex, Gold, Silver, Bronze, Total) %>%
      arrange(desc(Total))
    
    if (input$gender != "both") {
      data <- data %>% filter(Sex == input$gender)
    }
    
    data
  }, options = list(
    pageLength = 10,
    lengthMenu = c(10, 25, 50),
    scrollY = "300px",
    scrollCollapse = TRUE
  ))
  
  
  # Render athlete plot overview
  output$athlete_overview <- renderText({
    total_athletes <- sum(games_data()$num_athletes)
    total_events <- sum(games_data()$num_events)
    max_athletes_year <- games_data() %>% 
      group_by(Year) %>% 
      summarise(total_athletes = sum(num_athletes)) %>% 
      arrange(desc(total_athletes)) %>% 
      slice(1) %>% 
      pull(Year)
    
    paste("This scatter plot shows the relationship between the number of athletes and the number of events in Olympic Games over time.",
          "In the selected time period and sports, there were a total of", total_athletes, "athlete participations across", total_events, "events.",
          "The year with the highest number of athletes was", max_athletes_year, ".")
  })
  
  # Render scatter plot
  output$scatter_plot <- renderPlot({
    req(nrow(games_data()) > 0)
    p <- ggplot(games_data(), aes(x = num_athletes, y = num_events, size = Year)) +
      geom_point(alpha = 0.7) +
      geom_text(aes(label = City), vjust = -0.5, hjust = 0.5, size = 3, check_overlap = TRUE) +
      scale_color_continuous(low = "blue", high = "red") +
      labs(title = "Number of Athletes vs Number of Events in Olympic Games",
           subtitle = paste("Sports Played:", paste(input$sports, collapse = ", "),
                            "\nTime Period:", input$year_range[1], "-", input$year_range[2],
                            if(length(input$nocs) > 0) paste("\nSelected NOCs:", paste(input$nocs, collapse = ", ")),
                            "\nData Source: https://www.kaggle.com/datasets/heesoo37/120-years-of-olympic-history-athletes-and-results"),
                          
           x = "Number of Athletes",
           y = "Number of Events") +
      theme_minimal() +
      theme(plot.title = element_text(size = 16, face = "bold"),
            plot.subtitle = element_text(size = 10))
    
    if (input$gender == "both") {
      p + facet_wrap(~ Sex, scales = "free", ncol = 2)
    } else {
      p
    }
  })
}

# Run the app
shinyApp(ui = ui, server = server)
