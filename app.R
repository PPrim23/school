#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(readxl)
library(flexdashboard)
library(shiny)
library(ggplot2)
library(ggalluvial)
library(plotly)
library(igraph)
library(networkD3)
library(dbscan)
library(rcartocolor)
library(treemapify)
library(DT)
library(wesanderson)
library(tidyr)
library(dplyr)
library(shinythemes)

ui <- fluidPage(
  titlePanel("รายงานความสามารถในการอ่านและเขียนของนักเรียน"),
  sidebarLayout(
    sidebarPanel(
      uiOutput("template"),
      fileInput("file1", "ขั้นที่ 2 : Upload ไฟล์ MS Excel ตาม template", accept = c(".xlsx")),
      p("ขั้นที่ 3 : กดปุ่ม Analyze เพื่อวิเคราะห์ข้อมูล"),
      actionButton("goButton", "Analyze")
    ),
    mainPanel(
      conditionalPanel(
        condition = "input.goButton > 0",
        fluidRow(
          column(3, gaugeOutput("gaugeOutput1", height = "100%")),
          column(3, gaugeOutput("gaugeOutput2", height = "100%")),
          column(3, gaugeOutput("gaugeOutput3", height = "100%"))
        ),
        br(),
        br(),
        fluidRow(
          column(6, plotOutput("BarsChart", height = "350px", width = 900))),
        tabsetPanel(
          type = "tabs",
          tabPanel("นักเรียนรายบุคคล",
                   DTOutput("table")
          )
        )
      )
    )
  )
)

server <- function(input, output, session) {
  data <- reactive({
    req(input$file1)
    read_excel(input$file1$datapath)
  })
  
  url_template<-a("template", href = "https://github.com/PPrim23/school/blob/main/data_template.xlsx")
  output$template <- renderUI({
    tagList("ขั้นที่ 1","ดาวน์โหลด", url_template,"สำหรับกรอกข้อมูล")
  })
  
  output$gaugeOutput1 <- renderGauge({
    dat <- data()
    if (is.null(dat)) return()
    
    read_risk <- dat %>% filter(read < 5) %>% count() %>% as.numeric()
    
    gauge(
      value = read_risk,
      min = 0,
      max = nrow(dat),
      symbol = 'คน',
      label = "ปรับปรุงการอ่าน",
      sectors = gaugeSectors(
        success = c(0, 0),
        warning = c(0, read_risk),
        danger = c(100, 100),
        colors = c("success", "warning", "danger")
      )
    )
  })
  
  output$gaugeOutput2 <- renderGauge({
    dat <- data()
    if (is.null(dat)) return()
    
    write_risk <- dat %>% filter(write < 4) %>% count() %>% as.numeric()
    
    gauge(
      value = write_risk,
      min = 0,
      max = nrow(dat),
      symbol = 'คน',
      label = "ปรับปรุงการเขียน",
      sectors = gaugeSectors(
        success = c(0, 0),
        warning = c(0, write_risk),
        danger = c(100, 100),
        colors = c("success", "warning", "danger")
      )
    )
  })
  
  output$gaugeOutput3 <- renderGauge({
    dat <- data()
    if (is.null(dat)) return()
    
    total_risk <- dat %>% filter(total < 9) %>% count() %>% as.numeric()
    
    gauge(
      value = total_risk,
      min = 0,
      max = nrow(dat),
      symbol = 'คน',
      label = "ปรับปรุงการอ่านการเขียน",
      sectors = gaugeSectors(
        success = c(0, 0),
        warning = c(0, total_risk),
        danger = c(100, 100),
        colors = c("success", "warning", "danger")
      )
    )
  })
  
  output$BarsChart <- renderPlot({
    dat <- data()
    if (is.null(dat)) return()
    
    dat$read <- cut(dat$read, breaks = c(0, 4, 9, 15, 20), labels = c("ปรับปรุง", "พอใช้", "ดี", "ดีมาก"))
    dat$write <- cut(dat$write, breaks = c(0, 3, 7, 12, 16), labels = c("ปรับปรุง", "พอใช้", "ดี", "ดีมาก"))
    dat$total <- cut(dat$total, breaks = c(0, 8, 17, 26, 36), labels = c("ปรับปรุง", "พอใช้", "ดี", "ดีมาก"))
    
    data_long <- tidyr::pivot_longer(dat, cols = c("read", "write", "total"), names_to = "group", values_to = "value")
    data_count <- data_long %>% group_by(group, value) %>% summarise(count = n())
    
    data_count_no_na <- data_count[!is.na(data_count$value),]
    
    ggplot(data_count_no_na, aes(x = value, y = count, fill = group)) +
      geom_bar(position = "dodge", stat = "identity") +
      geom_text(aes(label = count), position = position_dodge(width = 0.9), vjust = -0.5) +
      labs(x = "", y = "จำนวนนักเรียน", title = "ระดับการประเมินความสามารถในการอ่านและการเขียน") +
      theme_minimal() +
      theme(text = element_text(family = "ChulaCharasNew"), plot.title = element_text(hjust = 0.5))
  })
  
  output$table <- renderDT({
    dat <- data()
    if (is.null(dat)) return()
    dat$read <- cut(dat$read, breaks = c(0, 4, 9, 15, 20), labels = c("ปรับปรุง", "พอใช้", "ดี", "ดีมาก"))
    dat$write <- cut(dat$write, breaks = c(0, 3, 7, 12, 16), labels = c("ปรับปรุง", "พอใช้", "ดี", "ดีมาก"))
    dat$total <- cut(dat$total, breaks = c(0, 8, 17, 26, 36), labels = c("ปรับปรุง", "พอใช้", "ดี", "ดีมาก"))
    
    tab <- dat %>% select(name, read, write, total)
    names(tab) <- c("ชื่อนักเรียน", "ความสามารถในการอ่าน", "ความสามารถในการเขียน", "รวมความสามารถทั้ง 2 ด้าน")
    datatable(tab, extensions = c('ColReorder', 'Responsive'), 
              options = list(
                colReorder = list(realtime = FALSE),
                columnDefs = list(list(className = 'dt-center', targets = '_all'))
              )) %>%
      formatStyle(
        columns = 2:4,
        backgroundColor = styleEqual(
          levels = c("ปรับปรุง", "พอใช้", "ดี", "ดีมาก"),
          values = c('#FF6600', '#FFD700', '#ADFF2F', '#006400')
        ),
        color = styleEqual(
          levels = c("ปรับปรุง", "พอใช้", "ดี", "ดีมาก"),
          values = c("white", "black", "black", "white")
        )
      )
  })
}

shinyApp(ui = ui, server = server)
