library(shiny)
library(ggplot2)
library(plotly)
library(shinydashboard)
library(readr)
library(dplyr)
library(DT)


             
ui = dashboardPage(
  dashboardHeader(title = tags$b("Sodros duomenys", style = "font-weight:bold;"),
                  titleWidth=300),
  dashboardSidebar(width = 300,
     sidebarMenu(
       selectizeInput(inputId = "company_input",
                      label="Imonės pavadinimas",
                      choices= NULL, selected= NULL),
       selectizeInput(inputId = "company_code_input",
                      label="Imonės kodas",
                      choices= NULL, selected=NULL),
        menuItem("Grafikas", tabName = "page1", icon = icon("dashboard")),
        menuItem("Lentelė", tabName = "page2", icon = icon("table")),
        menuItem("K-means clustering", tabName = "page3", icon = icon("circle-nodes",lib = "font-awesome")))
       
    ),
  
  dashboardBody(tags$head(
    h3("451100: Automobilių ir lengvųjų variklinių transporto priemonių pardavimas"),
      tags$style(
      HTML("
        
        .form-group .control-label {margin-bottom: 5px;}
        .form-group .control-input {margin-bottom: 0px;}
        .form-group .selectize-control {
              margin-bottom: 0px;
              width: 100%;
              font-family: 'Roboto', sans-serif !important;}
          
        *{font-family: 'Roboto', sans-serif;}
        h1, h2, h3 {  font-family: 'Roboto', sans-serif; font-weight: bold;}
        .box-body .form-group:first-child { margin-top: 0px;}
        .box-title {margin-top: 0px;}
        h3 {margin-left: 20px;}
        
      "))),
      
    
      tabItems(
        tabItem(tabName = "page1",
                fluidRow(
                  infoBoxOutput("EmployeeBox"),
                  infoBoxOutput("TaxBox"),
                  infoBoxOutput("SalaryBox")
                ),
                fluidRow(box(title = "Vid. atlyginimų grafikas",
                             width = 12,
                             plotlyOutput("wage_plot"),
                             collapsible = TRUE)),
                fluidRow(box(title = "Apdraustų darb. grafikas",
                             width = 12,
                             plotlyOutput("insured_plot"),
                             collapsible = TRUE))),
        
        tabItem(tabName = "page2", h3("Duomenų lentelė"),
                box(collapsible=TRUE,width='12',DTOutput("table"))),
        tabItem(tabName = "page3", h3("Cluster Analysis"),
                fluidRow(
                  column(width=8,
                         plotOutput("plot2")),
                  column(width=4,
                         selectInput("xcol", "X Variable", c("code", "month","avgWage","numInsured","tax"), "avgWage"),
                         selectInput("ycol", "Y Variable", c("code", "month","avgWage","numInsured","tax"), "tax"),
                         numericInput("clusters", "Cluster count", 3, min = 1, max = 9, step = 1),
                         br())
                         
                )
        )
      )

    
  )
)

# Define server logic for the app
server = function(input, output, session) {
  data = read_csv("https://github.com/kestutisd/KTU-duomenu-vizualizacija/raw/main/laboratorinis/data/lab_sodra.csv")
  data = data %>%
        filter(ecoActCode == 451100)
  
  data = data %>%
    mutate( month_date = as.Date(paste0(data$month, "01"), format = "%Y%m%d"),
            formatted_date = format(month_date, "%y'%m"))
  
  
  
  observe({
    unique_companies = unique(data$name)
    updateSelectizeInput(session, "company_input", choices = unique_companies,
                         server = TRUE, selected = "UAB AUTOPARK")
  })
  
  observe({
    unique_codes = sort(unique(data$code))
    updateSelectizeInput(session, "company_code_input", choices = unique_codes,
                         server = TRUE, selected = "83552")
  })
  
  # observe({
  #   unique_codes = names(data)[c(1,7,8,9,12)]
  #   updateSelectizeInput(session, "xcol", choices = unique_codes,
  #                        server = TRUE, selected = 1)
  #   updateSelectizeInput(session, "ycol", choices = unique_codes,
  #                        server = TRUE, selected = 2)
  # })
  
  observeEvent(input$company_code_input, {
    req(input$company_code_input)
    company_name = data %>% filter(code == input$company_code_input) %>% select(name) %>% unique()
    updateSelectizeInput(session, "company_input", selected = company_name)
  })
  
  observeEvent(input$company_input, {
    selected_company <- data %>% filter(name == input$company_input)
    updateSelectInput(session, "company_code_input", selected = selected_company$code)
  })
  
  filtered_data = reactive({
    req(input$company_input)
      
    data %>%
      filter(name == input$company_input)
  })
  
  output$wage_plot = renderPlotly({
    req(nrow(filtered_data()) > 0)
    
    plot_data = filtered_data()
    p = plot_ly(plot_data, x = ~formatted_date, y = ~avgWage, type = "scatter", mode = "lines+markers") %>%
      layout(xaxis = list(title = "Data"),
             yaxis = list(title = "Vidutinis atlyginimas"),
             hovermode = "x",
             margin = list(r = 50))  # Adjust the right margin
    p
  })
  
  output$insured_plot = renderPlotly({
    req(nrow(filtered_data()) > 0)
    
    plot_data = filtered_data()
    p = plot_ly(plot_data, x = ~formatted_date, y = ~numInsured, type = "scatter", mode = "lines+markers") %>%
      layout(xaxis = list(title = "Data"),
             yaxis = list(title = "Apdraustųjų skaičius"),
             hovermode = "x",
             margin = list(r = 50))
    p
  })
  
  output$table = renderDT({
    req(nrow(filtered_data()) > 0)
    
    filtered_data() %>% 
      select(-avgWage2, -ecoActCode, -ecoActName, -numInsured2,-month_date, -month)%>%
      datatable(options = list(pageLength = 30, dom = 'tip', lengthMenu = c(5,15,30,50,100)))})
  
  selectedData = reactive({
    req(input$xcol, input$ycol)
    
    data[, c(input$xcol, input$ycol)]%>%
      na.omit() %>%
      filter(complete.cases(.))
    
    })
  
  clusters = reactive({kmeans(selectedData(), centers = input$clusters)})
  
  output$plot2 = renderPlot({
    palette(c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3",
              "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999"))
    
    par(mar = c(5.1, 4.1, 0, 1))
    plot(selectedData(),
         col = clusters()$cluster,
         pch = 20, cex = 3)
    points(clusters()$centers, pch = 4, cex = 4, lwd = 4)
  })
  
  last_period_employees = reactive({
    req(nrow(filtered_data()) > 0)
    
    filtered_data() %>%
      arrange(desc(formatted_date)) %>%
      slice(1) %>%
      pull(numInsured)
  })
  
  output$EmployeeBox = renderValueBox({
    req(input$company_input)
    valueBox(last_period_employees(), "Darbuotojai", icon = icon("users"),
      color = "blue")
  })
  
  
  output$TaxBox <- renderValueBox({
    req(nrow(filtered_data()) > 0)
    tax_sum <- sum(filtered_data()$tax, na.rm = TRUE)
    tax_sum <- ifelse(is.na(tax_sum), "Nėra informacijos", 
                      paste0( formatC(tax_sum, format = "f", 
                                      big.mark = ",", decimal.mark = ".", digits = 0)," €"))

    valueBox(tax_sum, "Sumokėti mokesčiai",  
             icon = icon("file-invoice-dollar"), color = "purple",width=3)
  })
  
  output$SalaryBox <- renderValueBox({
    req(nrow(filtered_data()) > 0)
    
    avg_wage = mean(filtered_data()$avgWage, na.rm = TRUE)
    avg_wage <- ifelse(is.na(avg_wage), 0, avg_wage)
    valueBox(paste0(formatC(avg_wage, format = "f", big.mark = ",",
                            decimal.mark = ".", digits = 2), " €"),
                            "Vidutinis atlyginimas", icon = icon("dollar"),
                            color = "yellow")}
  )
}
# Run the app
#75524
shinyApp(ui, server)

