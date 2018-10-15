#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
 
library(shiny)
#  The data
library(ggplot2)
library(shiny)
library(RColorBrewer)
setwd("~/RNASeqExample/Assignment7/")
dataset <- read.csv('sample_info.csv',header = TRUE, sep = ",", quote = "\"", dec = ".", fill = TRUE, row.names = 1)
headerNames=colnames(dataset)
headerNames[13] = "RdPu"
headerNames[14] = "RdGy"
headerNames[15] = "Set1"
scheme1=data.frame(cbind(brewer.pal(7,'RdPu'), brewer.pal(7,'RdGy'), brewer.pal(7, 'Set1')))
colnames(scheme1)<-c('RdPu', 'RdGy', 'Set1')
colorNames=colnames(scheme1)
ui <- fluidPage(
  pageWithSidebar(
    
    headerPanel("Data Explorer (try geom_point+ coord_polar + geom_density_2d
                 also try color_scheme)"),
    sidebarPanel(
      selectInput('x', 'X', choices=c("None"=FALSE, headerNames[1:12]),headerNames[4]),
      selectInput('y', 'Y', choices=c("None"=FALSE,headerNames[1:12]),headerNames[5]),
      selectInput('fill', 'Fill', choices=c("None"=T, headerNames[3:11]),headerNames[5]),
      selectInput('size', 'Size', choices=c("None"=FALSE,headerNames[1:12]),headerNames[5]),
      selectInput('colour', 'Colour', choices =c("None"=TRUE, headerNames[3:11]), headerNames[5]),
      selectInput('facet_row', 'Facet Row', choices=c(None=TRUE, headerNames[2], headerNames[12])),
      selectInput('facet_col', 'Facet Column', choices=c(None='.', headerNames[1:12])),
      selectInput('color_scheme', 'Color_scheme', choices=c("None"=F,headerNames[13:15]), selected=colorNames[1]), 
      
      checkboxInput('geom_point', 'geom_point',TRUE),
      checkboxInput('geom_dotplot', 'geom_dotplot'),
      checkboxInput('geom_bar', 'geom_bar'),
      checkboxInput('geom_violin', 'geom_violin'),
      checkboxInput('geom_histogram', 'geom_histogram'),
      checkboxInput('geom_density_2d', 'geom_density_2d'),
      checkboxInput('geom_bind2d', 'geom_bind2d'),
      checkboxInput('coord_polar', 'coord_polar')
  ),
    mainPanel(
      plotOutput('plot')
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  
  output$plot <- renderPlot({
    p <- ggplot(dataset, aes_string(
      x=input$x, fill=input$fill, size=input$size, colour=input$colour)) + scale_color_distiller(palette = input$color_scheme, aesthetics = c("colour", "fill"), na.value="red1")
    if (input$geom_point)
      p <- p + geom_point(aes_string(y=input$y)) 
    facets <- paste(input$facet_row, '~', input$facet_col) #+ geom_point(scheme1, aes(color=input$color_scheme))
    if (facets != '. ~ .')
      p <- p + facet_grid(facets)
    if (input$geom_bar)
      p <- p + geom_bar() 
    # geom_dotplot doesn't requires an y input
    if (input$geom_dotplot)
      p <- p + geom_dotplot() 
    if (input$geom_violin)
      p <- p + geom_violin(aes_string(y=input$y))
    if (input$geom_histogram)
      p <- p + geom_histogram() 
    if (input$geom_density_2d)
      p <- p + geom_density_2d(aes_string(y=input$y))
    if (input$geom_bind2d)
      p <- p + geom_bin2d(aes_string(y=input$y))
    if (input$coord_polar)
      p <- p + coord_polar()
    
    print(p)
    
  }, height=700)
  
}
# Run the application 
shinyApp(ui = ui, server = server)