library(tidyverse)
library(ggplot2)
library(plotly)
library(shiny)
library(shinythemes)
library(DT)
bu_data <- read_csv("data/bu.csv", guess_max = 1001)

dept <- bu_data %>% pull(tDept) %>% unique() %>% sort()

phy_prof <- bu_data %>% group_by(tDept, prof_name) %>% summarise(tDept, avg_rating=mean(rate_prof), avg_diff = mean(level_difficulty)) %>% unique()

courses_rate <- bu_data %>% group_by(tDept, course_name) %>% summarise(course_name, avg_hw_lvl=mean(hw_level), avg_diff = mean(level_difficulty)) %>% unique()


#clean the course data
courses_rate <- courses_rate %>% filter(!is.na(avg_hw_lvl) & !is.na(avg_diff)) %>% filter(str_length(course_name) == 5)

top_prof <- bu_data %>% group_by(prof_name) %>% mutate(avg_hw_lvl=round(mean(hw_level), digits = 2)) %>% ungroup() %>% filter(str_length(course_name) == 5) %>% summarise(prof_name, course_name, rate_prof, tDept, avg_hw_lvl) %>% group_by(course_name) %>% mutate(rank = dense_rank(desc(rate_prof))) %>% filter(rank == 1) %>% ungroup() %>% arrange(tDept) %>% summarise(prof_name, course_name, rate_prof, tDept, avg_hw_lvl) %>% unique()

departments <- top_prof %>% pull(tDept) %>% unique() %>% sort()
courses <- top_prof %>% pull(course_name) %>% unique() %>% sort()

ui <- navbarPage("NACC",
                 theme = shinytheme("flatly"),
                 tabPanel( "Professor Ranking",
                           sidebarLayout(
                             sidebarPanel(
                               selectInput(inputId = "dept", label = "dept", choices = dept, multiple = FALSE, selected = "Accounting department")
                             ),
                             mainPanel(
                               plotlyOutput("distPlot"))
                           ),
                 ),
                 tabPanel( "Course Ranking",
                           sidebarLayout(
                             sidebarPanel(
                               selectInput(inputId = "dept2", label = "dept", choices = dept, multiple = FALSE, selected = "Accounting department")
                             ),
                             mainPanel(
                               plotlyOutput("distPlot2"))
                           )
                 ),
                 tabPanel( "Rank by Professor",
                           fluidRow(
                             column(4,
                                    selectInput(inputId = "dept3", label = "dept", choices = dept, multiple = FALSE, selected = "Accounting department")
                             )
                           ),
                           # Create a new row for the table.
                           DT::dataTableOutput("table")
                 )
)


server <- function(input, output) {
  output$distPlot <- renderPlotly({
    p <- phy_prof %>% filter(tDept == input$dept) %>% ggplot(aes_string(x = "avg_rating", y = "avg_diff", color = "prof_name")) + geom_point()
    ggplotly(p) %>% 
      partial_bundle()
  })
  
  output$distPlot2 <- renderPlotly({
    p <- courses_rate %>% filter(tDept == input$dept2) %>% ggplot(aes_string(x = "avg_hw_lvl", y = "avg_diff", color = "course_name")) + geom_point() 
    ggplotly(p) %>% 
      partial_bundle()
  })
  
  output$table <- DT::renderDataTable(DT::datatable({
    data <- top_prof %>% filter(top_prof$tDept == input$dept3)
    data
  }))
}

shinyApp(ui = ui, server = server)