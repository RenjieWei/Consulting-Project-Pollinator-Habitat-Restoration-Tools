library(shiny)
library(ggplot2)
library(grid)
library(gridExtra)
library(reshape2)


r_colors <- rgb(t(col2rgb(colors()) / 255))
names(r_colors) <- colors()


# Define UI for data upload app ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Plant Community Analysis"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(

      selectInput(
        'month', 'Select a month', 
        choices = c("June"=6,
                    "August"=8,
                    "October"=10),
        selectize = FALSE
      ),
      
      selectInput(
        'flo', 'Select Intended Flower', 
        choices = c("Palepurpleconeflower"=12,
                    "Blackeyedsusan"=4,
                    "Bergamot"=3,
                    "Goldenalexanders"=5,
                    "GreyhcFlo"=6,
                    "Illinoisbundleflower"=7,
                    "Lanceleafedcoreopsis"=8,
                    "Leadplant	Lupine"=9,
                    "Mountainmint"=10,
                    "NewEnglandaster"=11,
                    "Partridgepea"=13,
                    "Purpleconeflower"=14,
                    "Purpleprairieclover"=15,
                    "Roundheadedbushclover"=16,
                    "Royalcatchfly"=17,
                    "Showyticktrefoil"=18,
                    "Stiffgoldenrod"=19,
                    "Sweetconeflower"=20,
                    "Whiteprairieclover"=21
      ), 
         selectize = FALSE
      )
      
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Tabset w/ summary, plot, and map ----
      tabsetPanel(type = "tabs",
                  tabPanel("Intended flower", 

                           plotOutput("plot1")
                           
                  ),
                  
                  tabPanel("Forb Analysis", 
                           
                           plotOutput("plot2"),
                           plotOutput("plot3")
                           
                  )
                
                  
                  
      ))
    
  )
)

# Define server logic to read selected file ----
server <- function(input, output, session) {
  
  output$plot1 <- renderPlot({
    data <- read.csv("data21.csv",
                   header = T,
                   sep = ",",
                   quote = '  " '
    )
    data <- data[data$Month ==input$month,]
    aa <- as.numeric(input$flo)
    ab <- c(1,2,aa,70,71,72)
    data <- data[,ab]
    p <- ggplot(data, aes(x=data[[3]], fill=Plot)) +
       geom_density(alpha=.3)
    p <- p + labs(x = "Numbers Count",  
                           y = "Density")  
    p
  })
  
  runReg <- reactive({
    data <- read.csv("data21.csv",
                     header = T,
                     sep = ",",
                     quote = '  " '
    )
    data1 <- as.data.frame(data1)
    data1$Stratified = as.factor(data1$Stratified)
    data1$Seedmix = as.factor(data1$Seedmix)
    data1$Application = as.factor(data1n$Application)
    lm(as.formula(paste("Ratio ~ Stratified	+ Seedmix	+ Application")), data=data1)
  })
  
  output$reg <- renderTable({
    summary(runReg())$coefficients
  })
  
  output$summary <- renderPrint({
    data <- read.csv("data22.csv",
                     header = T,
                     sep = ",",
                     quote = '  " '
    )
    data <- data[data$Month==input$month,]
    summary(data)
  })
  
  output$plot2 <- renderPlot({
    data <- read.csv("data22.csv",
                     header = T,
                     sep = ",",
                     quote = '  " '
    )
    ggplot(data = data, mapping = aes(x = Plot, y = Count, fill = Name)) + geom_bar(stat= 'identity', position = 'stack')
  })
  
  output$plot3 <- renderPlot({
    data <- read.csv("data23.csv",
                     header = T,
                     sep = ",",
                     quote = '  " '
    )
    ggplot(data = data, mapping = aes(x = Month, y = Count, fill = Name)) + geom_bar(stat= 'identity', position = 'stack')
  })
  
  
}



# Create Shiny app ----  
shinyApp(ui, server)


