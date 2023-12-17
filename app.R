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

# Define UI for application 
ui <- fluidPage(
                titlePanel("รายงานความสามารถในการอ่านและเขียนของนักเรียน"),
                p("วัดผลโดยเครื่องมือคัดกรอง ความสามารถในการอ่านและการเขียน สพฐ"),
                sidebarLayout(
                  sidebarPanel(
                    uiOutput("template"),
                    fileInput("file1", "ขั้นที่ 2 : Upload ไฟล์ MS Excel ตาม template",
                              accept = c(".xlsx")
                    ),
                    p("ขั้นที่ 3 : กดปุ่ม Analyze เพื่อวิเคราะห์ข้อมูล"),
                    actionButton("goButton", "Analyze"),
                    br(),
                    br(),
                    uiOutput("contact"),
                    width=3),
                  
                  #แสดงguage
                    mainPanel(
                      conditionalPanel(
                        condition = "input.goButton > 0",
                        fluidRow(
                          column(3, gaugeOutput("gaugeOutput1", width = 3 )),
                          column(3, gaugeOutput("gaugeOutput2", width = 3 )),
                          column(3, gaugeOutput("gaugeOutput3", width = 3 ))
                          ),
                        # แสดง BarsChart
                        fluidRow(
                          style = "border: 1px dashed #ddd; padding: 10px; margin-bottom: 10px;",
                          column(6,uiOutput("output$BarsChart")),
                          column(6, 
                                 plotOutput("treemap", height = "300px"))),
                        fluidRow(
                          tabsetPanel(type = "tabs",
                                    br(),
                                    tabPanel("นักเรียนรายบุคคล",
                                             style = "border: 1px dashed #ddd; padding: 10px; margin-bottom: 10px;",
                                             
                                             DTOutput("table")
                                            )
                        
                        fluidRow(column(6, plotOutput("BarsChart", height = "300px"))),
                        tabsetPanel(
                          type = "tabs",
                          tabPanel("Risk Insight (1)",
                                   DTOutput("table")
                          )
                        )
                      )
                    )
                  )
                )
)
server <- function(input, output, session) {
  data <- reactive({
    inFile <- input$file1
    if (is.null(inFile))
      return()
    read_excel(inFile$datapath)
  })
  
  url_template<-a("template", href = "https://github.com/PPrim23/school/blob/main/data_template.xlsx")
  output$template <- renderUI({
    tagList("ขั้นที่ 1","ดาวน์โหลด", url_template,"สำหรับกรอกข้อมูล")
  })
  
    
    ### สร้าง gauge
    output$gaugetext <- renderUI({
      req(input$goButton > 0)  # ใช้ req() ที่นี่เช่นกัน
      h4("จำนวนนักเรียนที่จะต้องปรับปรุงความสามารถด้านการอ่านและการเขียน")
    })
    
    #### ด้านการอ่าน
    output$gaugeOutput1 <- renderGauge({
      #จัดการข้อมูล
      dat <- data
      
      if (is.null(dat)) {
        return()
      }
      
      #filter gauge1
      read_risk = dat %>% 
        filter(read < 5 ) %>% 
        count() %>%
        as.numeric()
      
      gauge(value = read_risk,
            min = 0,
            max = dim(dat)[1],
            symbol = 'คน' ,
            label = "ปรับปรุงการอ่าน",
            sectors = gaugeSectors(
              success = c(0,0),
              warning = c(0,read_risk%>%as.numeric()),
              danger = c(100,100),
              colors = c("success", "warning", "danger")
            ))
    })
    
    #### ด้านการเขียน
    output$gaugeOutput2 <- renderGauge({
      #จัดการข้อมูล
      dat <- data
      
      if (is.null(dat)) {
        return()
      }
      
      #filter gauge2
      write_risk = dat %>% 
        filter(write < 4 ) %>% 
        count() %>%
        as.numeric()
      
      gauge(value = write_risk,
            min = 0,
            max = dim(dat)[1],
            symbol = 'คน' ,
            label = "ปรับปรุงการเขียน",
            sectors = gaugeSectors(
              success = c(0,0),
              warning = c(0,write_risk%>%as.numeric()),
              danger = c(100,100),
              colors = c("success", "warning", "danger")
            ))
    })
    
    #### รวมทั้ง 2 ด้าน
    output$gaugeOutput3 <- renderGauge({
      #จัดการข้อมูล
      dat <- data
      
      if (is.null(dat)) {
        return()
      }
      
      #filter gauge3
      total_risk = dat %>% 
        filter(total < 9 ) %>% 
        count() %>%
        as.numeric()
      
      gauge(value = total_risk,
            min = 0,
            max = dim(dat)[1],
            symbol = 'คน' ,
            label = "ปรับปรุงการอ่านการเขียน",
            sectors = gaugeSectors(
              success = c(0,0),
              warning = c(0,total_risk%>%as.numeric()),
              danger = c(100,100),
              colors = c("success", "warning", "danger")
            ))
    })
    
    ### BarsChart 
    
    output$BarsChart <- renderPlot({
      
      
      p<- data 
      
      # แปลงค่าในคอลัมน์ read, write, total เป็น factor แบ่งเป็น "ปรับปรุง", "พอใช้", "ดี",าก"
      data$read <- cut(data$read, breaks = c(0, 4, 9, 15, 20), labels = c("ปรับปรุง", "พอใช้", "ดี", "ดีมาก"))
      data$write <- cut(data$write, breaks = c(0, 3, 7, 12, 16), labels = c("ปรับปรุง", "พอใช้", "ดี", "ดีมาก"))
      data$total <- cut(data$total, breaks = c(0, 8, 17, 26, 36), labels = c("ปรับปรุง", "พอใช้", "ดี", "ดีมาก"))
      
      # จัดการข้อมูลให้อยู่ในรูปแบบที่เหมาะสำหรับ ggplot
      data_long <- tidyr::pivot_longer(data, cols = c("read", "write", "total"), names_to = "group", values_to = "value")
      
      data_count <- data_long %>%
        group_by(group, value) %>%
        summarise(count = n())
      
      # ลบแถวที่มีค่า NA
      data_count_no_na <- data_count[!is.na(data_count$value),]
      
      # สร้างแผนภูมิ grouped bars
      ggplot(data_count_no_na, aes(x = value, y = count, fill = group)) +
        geom_bar(position = "dodge", stat = "identity") +
        geom_text(aes(label = count), position = position_dodge(width = 0.9), 
                  vjust = -0.5) +
        labs(x = "", y = "จำนวนนักเรียน",
             title ="ระดับการประเมินความสามารถในการอ่านและการเขียน") +
        theme_minimal() +
        theme(text = element_text(family = "ChulaCharasNew"),
              plot.title = element_text(hjust = 0.5))
      p
    })
    
    
    output$table <- renderDT({
      tab <- data %>% 
        select(name, read, write, total)
      #tab$econ <- as.numeric(factor(tab$econ,levels=c("ยากจน","ปานกลาง","ร่ำรวย")))"
      names(tab)[1]<-"ชื่อนักเรียน"
      names(tab)[2]<-"ความสามารถในการอ่าน"
      names(tab)[3]<-"ความสามารถในการเขียน"
      names(tab)[4]<-"รวมความสามารถทั้ง 2 ด้าน"
      datatable(tab, extensions = c('ColReorder','Responsive'), 
                options = list(colReorder = list(realtime = FALSE),
                               columnDefs = list(
                                 list(className = 'dt-center', targets = '_all')
                               ))) %>%
        ### จัด format การอ่าน
        formatStyle(
          columns = 2,
          backgroundColor = styleEqual(
            levels = c("ปรับปรุง","พอใช้","ดี","ดีมาก"), 
            values = c('#FF6600','#FFD700', '#ADFF2F', '#006400')
          ),
          color = styleEqual(
            levels = c("ปรับปรุง","พอใช้","ดี","ดีมาก"), 
            values = c("white","black","black","white")
          )
          
          
        ) %>%
        ### จัด format การเขียน
        formatStyle(
          column = 3,
          backgroundColor = styleEqual(
            levels = c("ปรับปรุง","พอใช้","ดี","ดีมาก"), 
            values = c('#FF6600','#FFD700', '#ADFF2F', '#006400')
          ),
          color = styleEqual(
            levels = c("ปรับปรุง","พอใช้","ดี","ดีมาก"), 
            values = c("white","black","black","white")
          )
          
        ) %>%
        ### จัด format รวมทั้ง 2 ด้าน
        formatStyle(
          column = 4,
          backgroundColor = styleEqual(
            levels = c("ปรับปรุง","พอใช้","ดี","ดีมาก"), 
            values = c('#FF6600','#FFD700', '#ADFF2F', '#006400')
          ),
          color = styleEqual(
            levels = c("ปรับปรุง","พอใช้","ดี","ดีมาก"), 
            values = c("white","black","black","white")
          )
          
        ) 
      
    }) 
    
    
 }


# Run the application 
shinyApp(ui = ui, server = server)
