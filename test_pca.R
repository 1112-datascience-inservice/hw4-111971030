if(!require('shiny')) {
  install.packages('shiny')
  library('shiny')
}
if(!require('ggbiplot')) {
  install.packages('remotes', dependencies = TRUE)
  remotes::install_github("vqv/ggbiplot")
  library('ggbiplot')
}


data(iris)

# Define UI for app that draws a histogram ----
ui <- fluidPage(
     h2("PCA"),
     selectInput("x_var", "X-Axis Variable:",
                 choices = c("PC1", "PC2", "PC3", "PC4")),
     selectInput("y_var", "Y-Axis Variable:",
                 choices = c("PC1", "PC2", "PC3", "PC4")),
     plotOutput("pca_plot")
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
  
}



# 執行應用
shinyApp(ui, server)

