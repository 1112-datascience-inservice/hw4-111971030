library(shiny)
library(DT)

ui <- fluidPage(
  DTOutput("iris_table")
)

server <- function(input, output) {
  # 將 iris 資料集儲存在 reactive 變數中
  iris_reactive <- reactive({
    iris
  })
  
  # 渲染可互動的資料表
  output$iris_table <- renderDT({
    DT::datatable(iris_reactive())
  })
}

shinyApp(ui, server)
