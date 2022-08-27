
####### STAGE_3 HackBio########
library(dplyr)
library(tidyr)
library(ggplot2)
library(shiny)
library(shinythemes)
library(shinyWidgets)

#data preprocessing
data<-read.csv("file.csv")
View(data)


genes <- data$ID #collecting the genes namesIDs
genes

#removing empty col
data<-data[-2]
head(data)

dup<-data[,1:10]

#colnames(dup) <- c("ID", "LetR1", "LetR1", "LetR1", "LetR3", "LetR3", "LetR3", "MCF7", "MCF7", "MCF7")
head(dup)
######


NewData <- dup %>% gather(`Cell Line`, `Expression Level`, 2:10)
head(NewData)
View(NewData)

NewData$`Cell Line` <- replace(NewData$`Cell Line`, startsWith(NewData$`Cell Line`, 'LetR1'), 'LetR1')
head(NewData)
NewData$`Cell Line` <- replace(NewData$`Cell Line`, startsWith(NewData$`Cell Line`, 'LetR3'), 'LetR3')
NewData$`Cell Line` <- replace(NewData$`Cell Line`, startsWith(NewData$`Cell Line`, 'MCF7'), 'MCF7')
View(NewData)

##just to check on one of the genes
NewData %>%
  filter(NewData$ID == "TC01000001.hg.1") %>%
  ggplot(aes(x = `Cell Line`, y = `Expression Level`, col = `Cell Line`)) +
  geom_boxplot()


cell_lines<-c("LetR1","LetR3","MCF7")
################### APPLICATION ###############
# Define UI for application
ui <- fluidPage(
  # Application title
  headerPanel("My Application_HackBio_Stage3"),
  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      selectizeInput(inputId = "genes",
                     label = h3("Genes:"),
                     choices = NULL  ,
                     options = list(placeholder = "Type of gene", maxOptions = 1000) ),
      h5("*Type your gene of interest (only first 1000 genes displayed)"),
      pickerInput(inputId = "cell_lines",
                  label = "",
                  choices = cell_lines,
                  options = pickerOptions(actionsBox = TRUE, title = "Cell Lines",  size = 10, maxOptions = 3, liveSearch = TRUE),
                  multiple = TRUE)
    ),
    # Show a plot of the generated distribution
    mainPanel(titlePanel("Gene Expression"),
              plotOutput("distPlot")
    )
  )
)
# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
    updateSelectizeInput(session, "genes", choices = genes, server = TRUE)
    updatePickerInput(session, "cell_lines", choices = cell_lines)
  })
  
  
  data <- reactive({
    dat <- NewData[NewData$ID == input$genes & NewData$`Cell Line` %in% input$cell_lines,]
    
    print("Something")
    print(dat)
  })
  print("Some")
  
## to draw box plot as output
  output$distPlot <- renderPlot({
    ggplot(data(),aes(x=`Cell Line`,y=`Expression Level`,col=`Cell Line`, fill=`Cell Line`)) + geom_boxplot()
  })
}
# Run the application
shinyApp(ui = ui, server = server)

