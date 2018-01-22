# Phyllotaxis 3D simulator Shiny app by Hernando Cortina
# Based on Stephen Wolfram's 3D Phyllotaxis from New Kind of Science

library(shiny)
library(plotly)

# Define UI for application that draws a histogram
ui <- fluidPage(
   # Application title
   titlePanel('Phyllotaxis in 3D by @cortinah.'),
   print("Based on Stephen Wolfram's New Kind of Science (page 411 and CDF)"),
   hr(),
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         sliderInput("n",
                     "Number of points:",
                     min = 2,
                     max = 1000,
                     value = 250),
        sliderInput("size",
                    "Size of points:",
                    min = 1,
                    max = 30,
                    value = 15),
        sliderInput("angle",
                    "Angle (rads, default=golden angle):",
                    min = 0,
                    max = 6.283,
                    value = pi*(3-sqrt(5)) ),
        sliderInput("pointiness",
                    "Pointiness:",
                    min = 0,
                    max = 2.001,
                    value = 0.25),
        sliderInput("pexp",
                    "Pointiness Exponent:",
                    min = 0,
                    max = 0.65,
                    value = 0.45),
        selectInput(inputId = "pal",
                    label = "Color Palette:",
                    choices = c('Greens','Viridis','Blackbody','Bluered','Blues','Earth',
                                 'Electric','Greys','Hot','Jet','Picnic','Portland',
                                 'Rainbow','RdBu','Reds','YlGnBu','YlOrRd'))
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
        plotlyOutput("philloPlot")
      )
   )
)


# Define server logic required to draw a histogram
server <- function(input, output) {
  options(warn = -1)
   output$philloPlot <- renderPlotly({
      # generate df based on input$ from ui.R
     i <- (1:input$n)
     x <- sqrt(i) * sin(i*input$angle)
     y <- sqrt(i) * cos(i*input$angle)
     z <- sqrt(i) * -input$pointiness * (i + 1^-10)^input$pexp
     
     df <- data.frame(i, x, y, z)
     df$z <- df$z + max(abs(df$z))
     
      # draw the scatterplot
     ax <- list(
       title = '',
       zeroline = FALSE,
       showline = FALSE,
       showticklabels = FALSE,
       showgrid = FALSE,
     range=c(-sqrt(input$n),sqrt(input$n)) )
     
     
       p <- plot_ly(df, x = ~x, y = ~y, z = ~z , hoverinfo='none', marker=list(size=~input$size, color=~-(x^2+y^2+z^2)^(1/3), colorscale=input$pal, showscale=F)) %>% add_markers() %>%
         layout(width=540, height=540,scene=list(xaxis=ax,yaxis=ax,zaxis=ax[1:5],
                                                 camera = list(eye = list(x = -1.5, y = 0, z = 1))),margin=list(
                                                   l = 0, r = 0, b = 0, t = 0, pad = 5), paper_bgcolor='#F0F0F0')
    
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

