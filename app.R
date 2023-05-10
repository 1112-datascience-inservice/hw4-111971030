

if(!require('shiny')) {
  install.packages('shiny')
  library('shiny')
}
if(!require('ggbiplot')) {
  install.packages('remotes', dependencies = TRUE)
  remotes::install_github("vqv/ggbiplot")
}
if(!require('DT')) {
  install.packages('DT')
  library(DT)
}
library(FactoMineR)
library(factoextra)


data(iris)

# Define UI for app that draws a histogram ----
ui <- fluidPage(
  # navbar
  navbarPage(
    title = "111971030 黃哲偉 HW4 PCA", # 應用標題
    tabPanel("PCA", 
             # tabsetPanel(
               # tabPanel("pca plot", 
                        fluidPage(
                          fluidRow(
                            column(4, 
                              h2("PCA"),
                              selectInput("x_var", "X-Axis Variable:",
                                          choices = c("PC1", "PC2", "PC3", "PC4"),
                                          selected = "PC1"),
                              selectInput("y_var", "Y-Axis Variable:",
                                          choices = c("PC1", "PC2", "PC3", "PC4"),
                                          selected = "PC2")
                             ),
                            column(8, 
                              plotOutput("pca_plot")
                            )
                          )
                        )
               # ),
               # tabPanel("result data", ""),
               # tabPanel("input data(log)", ""),
               # tabPanel("extended results", "")
              # )
             ),
    tabPanel("CA", 
             # tabsetPanel(
             #   tabPanel("ca plot", ""),
             #   tabPanel("extended results", "")
             #   )
             fluidPage(
               plotOutput("ca_plot")
               )
             ),
    tabPanel("iris data",
             fluidPage(
                DTOutput("iris_table")
               )
             )
  )
)

# Server
server <- function(input, output, session) {
  # log transform 
  log.ir <- log(iris[, 1:4])
  ir.species <- iris[, 5]
  # apply PCA - scale. = TRUE is highly advisable, but default is FALSE. 
  ir.pca <- prcomp(log.ir, center = TRUE, scale. = TRUE)
  
  # define PCA
  
  pca_data <- reactive({
    x_var <- switch(input$x_var,
                    "PC1" = ir.pca$x[, 1],
                    "PC2" = ir.pca$x[, 2],
                    "PC3" = ir.pca$x[, 3],
                    "PC4" = ir.pca$x[, 4])
    y_var <- switch(input$y_var,
                    "PC1" = ir.pca$x[, 1],
                    "PC2" = ir.pca$x[, 2],
                    "PC3" = ir.pca$x[, 3],
                    "PC4" = ir.pca$x[, 4])
    data.frame(x = x_var, y = y_var, species = ir.species)
  })
  
  # plot PCA
  plotPCA <- function() {
    ggbiplot(ir.pca, obs.scale = 1, var.scale = 1, groups = ir.species, ellipse =TRUE, 
             choices = c(which(colnames(ir.pca$x) == input$x_var), which(colnames(ir.pca$x) == input$y_var))) +
      geom_point(aes(x = pca_data()$x, y = pca_data()$y, color = pca_data()$species)) +
      theme(legend.direction = 'horizontal', legend.position = 'top') + 
      scale_color_discrete(name = '')
  }
  
  # render PCA plot
  output$pca_plot <- renderPlot({ plotPCA() })
  
  # Check for equal X and Y inputs and update them accordingly
  observeEvent(c(input$x_var, input$y_var), {
    if(input$x_var == input$y_var) {
      updateSelectInput(session, "y_var", choices = setdiff(c("PC1", "PC2", "PC3", "PC4"), input$x_var))
    }
    output$pca_plot <- renderPlot({ plotPCA() })
  })
  
  
  # define CA
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
  
  iris_reactive <- reactive({
    iris
  })
  
  output$iris_table <- renderDT({
    DT::datatable(iris_reactive())
  })
}




# 執行應用
shinyApp(ui, server)

