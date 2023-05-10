# library(FactoMineR)
# library(factoextra)
# 
# # 讀取 iris 資料集並選取需要的欄位
# data(iris)
# iris_ca <- iris[, c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width", "Species")]
# 
# # 將非數值型別的變數轉換為數值型別的變數
# iris_ca$Species <- as.numeric(iris_ca$Species)
# 
# # 做 Correspondence analysis
# res.ca <- CA(iris_ca)
# 
# # 繪製 Correspondence analysis 圖
# fviz_ca_biplot(res.ca,
#                col.var = "black", # 標籤顏色
#                col.ind = iris_ca$Species, # 群集顏色
#                legend.title = "Species", # 群集標籤
#                # ylim = c(-1,1) # y軸範圍
# )
library(shiny)
library(FactoMineR)
library(factoextra)

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      
    ),
    mainPanel(
      plotOutput("ca_plot")
    )
  )
)

server <- function(input, output) {
  # 讀取 iris 資料集並選取需要的欄位
  data(iris)
  iris_ca <- iris[, c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width", "Species")]
  
  # 將非數值型別的變數轉換為數值型別的變數
  iris_ca$Species <- as.numeric(iris_ca$Species)
  
  # 做 Correspondence analysis
  res.ca <- CA(iris_ca)
  
  # 繪製 Correspondence analysis 圖
  output$ca_plot <- renderPlot({
    fviz_ca_biplot(res.ca,
                   col.var = "black",
                   col.ind = iris_ca$Species,
                   legend.title = "Species",
    )
  })
}

shinyApp(ui, server)
