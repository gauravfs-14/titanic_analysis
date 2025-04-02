library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(DT)

# Load dataset
data <- read.csv("https://raw.githubusercontent.com/datasciencedojo/datasets/master/titanic.csv")

# UI
ui <- dashboardPage(
  dashboardHeader(title = "Titanic Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("chart-bar")),
      sliderInput("age", "Age Range", min = 0, max = 80, value = c(0, 80)),
      selectInput("sex", "Sex", choices = c("All", unique(data$Sex)), selected = "All"),
      selectInput("pclass", "Passenger Class", choices = c("All", unique(data$Pclass)), selected = "All")
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "dashboard",
              fluidRow(
                valueBoxOutput("totalBox"),
                valueBoxOutput("survivedBox"),
                valueBoxOutput("diedBox")
              ),
              fluidRow(
                box(title = "Survival by Gender", status = "primary", solidHeader = TRUE, width = 6,
                    plotOutput("survivalGender")),
                box(title = "Survival by Class", status = "primary", solidHeader = TRUE, width = 6,
                    plotOutput("survivalClass"))
              ),
              fluidRow(
                box(title = "Survival by Age Group", status = "primary", solidHeader = TRUE, width = 6,
                    plotOutput("survivalAge")),
                box(title = "Survival by Embarkation Port", status = "primary", solidHeader = TRUE, width = 6,
                    plotOutput("survivalEmbarked"))
              ),
              fluidRow(
                box(title = "Data Table", width = 12, DTOutput("dataTable"))
              )
      )
    )
  )
)

# Server
server <- function(input, output) {
  filtered <- reactive({
    df <- data %>%
      filter(Age >= input$age[1], Age <= input$age[2])
    
    if (input$sex != "All") df <- df %>% filter(Sex == input$sex)
    if (input$pclass != "All") df <- df %>% filter(Pclass == as.numeric(input$pclass))
    
    return(df)
  })
  
  output$totalBox <- renderValueBox({
    valueBox(nrow(filtered()), "Total Passengers", icon = icon("users"), color = "aqua")
  })
  
  output$survivedBox <- renderValueBox({
    survived <- filtered() %>% filter(Survived == 1)
    valueBox(nrow(survived), "Survived", icon = icon("heart"), color = "green")
  })
  
  output$diedBox <- renderValueBox({
    died <- filtered() %>% filter(Survived == 0)
    valueBox(nrow(died), "Did Not Survive", icon = icon("skull"), color = "red")
  })
  
  output$survivalGender <- renderPlot({
    df <- filtered() %>%
      group_by(Sex, Survived) %>%
      summarise(Count = n(), .groups = "drop")
    
    ggplot(df, aes(x = Sex, y = Count, fill = factor(Survived))) +
      geom_bar(stat = "identity", position = "dodge") +
      scale_fill_manual(values = c("#DC863B", "#74A089"), labels = c("No", "Yes")) +
      labs(title = "Survival by Gender", fill = "Survived")
  })
  
  output$survivalClass <- renderPlot({
    df <- filtered() %>%
      group_by(Pclass, Survived) %>%
      summarise(Count = n(), .groups = "drop")
    
    ggplot(df, aes(x = factor(Pclass), y = Count, fill = factor(Survived))) +
      geom_bar(stat = "identity", position = "dodge") +
      scale_fill_manual(values = c("#DC863B", "#74A089"), labels = c("No", "Yes")) +
      labs(title = "Survival by Class", x = "Class", fill = "Survived")
  })
  
  output$survivalAge <- renderPlot({
    df <- filtered() %>%
      mutate(AgeGroup = cut(Age,
                            breaks = c(0, 12, 19, 59, 80),
                            labels = c("Child", "Teen", "Adult", "Senior"))) %>%
      group_by(AgeGroup, Survived) %>%
      summarise(Count = n(), .groups = "drop")
    
    ggplot(df, aes(x = AgeGroup, y = Count, fill = factor(Survived))) +
      geom_bar(stat = "identity", position = "dodge") +
      scale_fill_manual(values = c("#DC863B", "#74A089"), labels = c("No", "Yes")) +
      labs(title = "Survival by Age Group", fill = "Survived", x = "Age Group")
  })
  
  output$survivalEmbarked <- renderPlot({
    df <- filtered() %>%
      filter(!is.na(Embarked)) %>%
      group_by(Embarked, Survived) %>%
      summarise(Count = n(), .groups = "drop")
    
    ggplot(df, aes(x = Count, y = Embarked, fill = factor(Survived))) +
      geom_bar(stat = "identity") +
      scale_fill_manual(values = c("#DC863B", "#74A089"), labels = c("No", "Yes")) +
      labs(
        title = "Survival by Embarkation Port",
        x = "Passenger Count",
        y = "Embarkation Port",
        fill = "Survived"
      ) +
      theme_minimal()
  })
  
  output$dataTable <- renderDT({
    filtered()
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
