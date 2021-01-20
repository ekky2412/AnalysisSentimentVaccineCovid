#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(vroom)
library(here)
library(dplyr)
library(ggplot2)
library(RColorBrewer)
library(SnowballC)
library(Rstem)
library(sentiment)
library(plyr)

# Define UI for application that draws a histogram
ui <- fluidPage(
  headerPanel("Analisis Sentimen Vaksin Covid-19"),
  headerPanel("Menggunakan Algoritma Naive Bayes"),
            mainPanel(
                tabsetPanel(
                    tabPanel("Data Twitter", DT::dataTableOutput('dataTwitter')),
                    tabPanel("Data Cleaned", DT::dataTableOutput('dataCleaned')),
                    tabPanel("Data Sentimen", DT::dataTableOutput('tbl')),
                    tabPanel("Plot Emotion Analysis", plotOutput("sent1")),
                    tabPanel("Plot Popularity Tweets", plotOutput("sent2"))
                )
            )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    dataTwitter<- vroom(here("D://Kuliah//SEMESTER 5//Data Science/Proyek//dataTwitter.csv"))
    dataTwitter<- data.frame(dataTwitter)
    # Output Data
    output$dataTwitter = DT::renderDataTable({
      DT::datatable(dataTwitter, options = list(lengthChange = FALSE))
    })
    
    sent_df<- vroom(here("D://Kuliah//SEMESTER 5//Data Science/Proyek//dataSentimen.csv"))
    sent_df <- data.frame(sent_df)
    dataCleaned<- vroom(here("D://Kuliah//SEMESTER 5//Data Science/Proyek//dataCleaned.csv"))
    dataCleaned<- data.frame(dataCleaned)
    # Output Data
    output$dataCleaned = DT::renderDataTable({
      DT::datatable(dataCleaned, options = list(lengthChange = FALSE))
    })
  
    sent_df<- vroom(here("D://Kuliah//SEMESTER 5//Data Science/Proyek//dataSentimen.csv"))
    sent_df <- data.frame(sent_df)
    # Output Data
    output$tbl = DT::renderDataTable({
      DT::datatable(sent_df, options = list(lengthChange = FALSE))
    })
    
    # plot distribution of emotions
    ggplot(sent_df, aes(x=emotion)) +
      geom_bar(aes(y=..count.., fill=emotion)) +
      scale_fill_brewer(palette="Dark2") +
      labs(x="emotion categories", y="number of tweets") +
      labs(title = "Sentiment Analysis of Vaccine Covid-19",
           plot.title = element_text(size=12))
    plotSentiments1 <- function(sentiment_dataframe, title) 
    {
      library(ggplot2)
      ggplot(sentiment_dataframe, aes(x=emotion)) + 
        geom_bar(aes(y=..count.., fill=emotion)) + 
        scale_fill_brewer(palette="Dark2") + 
        ggtitle(title) + 
        theme(legend.position="right") + 
        ylab("Number of Tweets") + 
        xlab("Emotion Categories")
    }
    #plotting tweets emotions
    output$sent1 <- renderPlot({
      plotSentiments1(sent_df, "Sentiment Analysis of Vaccine Covid-19")
    })
    
    # plot distribution of polarity
    ggplot(sent_df, aes(x=polarity)) +
      geom_bar(aes(y=..count.., fill=polarity)) +
      scale_fill_brewer(palette="RdGy") +
      labs(x="polarity categories", y="number of tweets") +
      labs(title = "Sentiment Analysis of Vaccine Covid-19",
           plot.title = element_text(size=12))
    plotSentiments2 <- function(sent_df, title)
    {
      library(ggplot2)
      ggplot(sent_df, aes(x=polarity)) +
        geom_bar(aes(y=..count.., fill=polarity)) +
        scale_fill_brewer(palette="RdGy") +
        ggtitle(title) +
        theme(legend.position="right") +
        ylab("Number of Tweets") +
        xlab("Polarity Categories")
    }
    
    output$sent2 <- renderPlot({
      plotSentiments2(sent_df, "Sentiment Analysis of Vaccine Covid-19")
    })
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
