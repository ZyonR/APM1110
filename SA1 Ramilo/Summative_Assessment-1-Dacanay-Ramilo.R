library(shiny)
library(shinydashboard)
library(tidyverse)
library(DT)
library(echarts4r)
library(shinyWidgets)

ui<-dashboardPage(skin = "black",
                  dashboardHeader(title="SA1 Ramilo-Dacanay"),
                  dashboardSidebar(
                    sidebarMenu(
                      menuItem("3 Factories Problem",tabName = "factory-problem",icon = icon("industry")),
                      menuItem("Front End Univariate & Bivariate",tabName = "univariate-bivariate",icon = icon("square")),
                      menuItem("Search Engine Simulation",tabName = "search-engine-simulation",icon = icon("magnifying-glass"))
                    )
                  ),
                  dashboardBody(
                    tabItems(
                      tabItem(tabName = "factory-problem",
                              box(width = 12,status = "primary",
                                  title = "Factory Problem",
                                  fluidRow(
                                    column(width = 6,
                                         p("Please Express in Decimal Form.")
                                    )),
                                  column(width = 6,
                                         uiOutput("input_x1"),
                                         uiOutput("input_x2"),
                                         actionBttn("validateInput",
                                                    label = "Enter",
                                                    style = "pill",
                                                    color = "primary",
                                                    size = "xs",
                                                    block = FALSE,
                                                    no_outline = TRUE
                                         )
                                         ),
                                  column(width = 6,
                                         uiOutput("input_y1"),
                                         uiOutput("input_y2"),
                                  )
                                  ),
                              box(width = 12,status = "primary",
                                  column(width = 6,
                                         fluidRow("The probability that a randomly selected product is defective.")
                                         ),
                                  column(width = 6,verbatimTextOutput("defective_probab")))
                              ),
                      tabItem(tabName = "univariate-bivariate",
                              column(width = 4,
                                     box(width = 12,status = "primary")
                                     ),
                              column(width = 6,
                                     box(width = 12,status = "primary")
                                     )
                              ),
                      tabItem(tabName = "search-engine-simulation",
                              column(width = 6,
                                     box(width = 12, status = "primary",
                                         fluidRow(column(width = 12,
                                                         uiOutput("inputProbab"))),
                                         fluidRow(column(width = 12,
                                                         DTOutput("searchEngineSimulated"))),
                                         )
                                     ),
                              column(width = 6,
                                     tabBox(width = 12,
                                       tabPanel(title = "Plot PDF",
                                                echarts4rOutput("plot_searchEngine")
                                                ),
                                       tabPanel(title = "Mean and Variance",
                                                fluidRow(width = 12,
                                                column(width = 6,
                                                       box(width = 12,"Mean"),
                                                       verbatimTextOutput("mean")
                                                       ),
                                                column(width = 6,
                                                       box(width = 12,"Variance"),
                                                       verbatimTextOutput("variance")
                                                )),
                                                fluidRow(width = 12,
                                                         box(width = 12,"Three searches have been carried out without success.",status = "primary"),
                                                         column(width = 6,
                                                                box(width = 12,"Mean"),
                                                                verbatimTextOutput("mean_3")
                                                         ),
                                                         column(width = 6,
                                                                box(width = 12,"Variance"),
                                                                verbatimTextOutput("variance_3")
                                                         ))
                                       )
                                     )
                                     )
                              )
                    )
                  )
                  )
server <- function(input,output){
  output$input_x1 <- renderUI({
    numericInput("x1","Product produced by Factory 1:",min = 0.1,max = 0.4,value = 0.1,step = 0.01)
  })
  output$input_x2 <- renderUI({
    numericInput("x2","Product produced by Factory 2:",min = 0.1,max = 0.4,value = 0.1,step = 0.01)
  })
  output$input_y1 <- renderUI({
    numericInput("y1","Defective produced by Factory 1:",min = 0.01,max = 0.05,value = 0.01,step = 0.001)
  })
  output$input_y2 <- renderUI({
    numericInput("y2","Defective produced by Factory 2:",min = 0.01,max = 0.05,value = 0.01,step = 0.001)
  })
  
  observeEvent(input$validateInput,{
    triggered <- FALSE
    input_vector_x <- c(input$x1,input$x2)
    input_vector_y <- c(input$y1,input$y2)
    for(answer in seq_along(input_vector_x)){
      if(input_vector_x[answer]<0.1 | input_vector_x[answer]>0.4){
        showNotification(paste("Error Invalid Value! on","Produced Product Factory ",answer))
        triggered <- TRUE
      }
    }
    for(answer in seq_along(input_vector_y)){
      if(input_vector_y[answer]<0.01 | input_vector_y[answer]>0.05){
        showNotification(paste("Error Invalid Value! on","Defective Product Factory ",answer))
        triggered <- TRUE
      }
    }
    x3 <- reactive({
      1 - input$x1 - input$x2
    })
    y3 <- reactive({
      0.12 - input$y1 - input$y2
    })
    if(!triggered){
      showModal(
        modalDialog(
          title = "YAY 🥳 you have entered valid inputs!",easyClose = TRUE,footer = NULL,
               fluidRow(box(width = 12,status = "primary",
                   paste("Factory 3 is calculated to be ",x3()," and its defective products is calculated to be ",y3(),". Please click anywhere to close.")))
        )
      )
    }
    input_vector_x <- c(input_vector_x,x3())
    input_vector_y <- c(input_vector_y,y3())
    propability <- c()
    for(i in seq_along(input_vector_x)){
      probab <- input_vector_x[i]*input_vector_y[i]
      propability <- c(propability,probab)
    }
    defective_probab <- reactive({
      sum(propability)
    })
    output$defective_probab <- renderPrint({
      defective_probab()
    })
  })
  output$inputProbab<- renderUI({
    numericInput("keyWordProbab","Input Probability of finding Keyword: ",min = 0,max = 1,value = 0.1,step = 0.01)
  })
  simulateSearch <- reactive({
    data <- data.frame(Search_Number = 1:10000, 
                       Number_of_Searches = rgeom(10000, input$keyWordProbab))
    return(data)
  })
  output$searchEngineSimulated <- renderDT({
    datatable(simulateSearch(),
              colnames = c("Search Number","N Searches Before Key is Found"),
              options = list(scrollY="25vh",scrollX="100%")
              )
  })
  observeEvent(input$keyWordProbab,{
    if(input$keyWordProbab<0 | input$keyWordProbab>1){
      showNotification("Error Invalid Value! Only Values from 0 to 1 Please!")-2
    }
  })
  output$plot_searchEngine<-renderEcharts4r({
    simulateSearch() %>% 
      group_by_(~Number_of_Searches) %>% 
      summarise(PDF = n()/10000) %>% 
      e_charts(Number_of_Searches) %>% 
      e_line(PDF)
  })
  output$mean<-renderPrint({
    mean(simulateSearch()$Number_of_Searches)
  })
  output$variance<-renderPrint({
    var(simulateSearch()$Number_of_Searches)
  })
  simulateSearch_3<-reactive({
    data <- simulateSearch()
    data[data$Number_of_Searches > 3, ]
  })
  output$mean_3<-renderPrint({
    mean(simulateSearch_3()$Number_of_Searches - 3)
  })
  output$variance_3<-renderPrint({
    var(simulateSearch_3()$Number_of_Searches - 3)
  })
  
}
shinyApp(ui=ui,server=server)
