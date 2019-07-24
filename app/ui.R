# setwd("C:/Users/harric17/Desktop/R Stuff/coursera/capstone_final")
library(shiny)



shinyUI(fluidPage(
  
  sidebarLayout(
    
    sidebarPanel(
      
      tabsetPanel(
                  tabPanel("Inputs",
  
  radioButtons("radio", label = h3("Model Type"),
               choices = list("Back Propogation Model" = "BACK", "Equal Weights Model" = "EQUAL", "Skewed Weights Model" = "WEIGHTED"), 
               selected = "BACK"),

  textInput("text", label = h3("Text input"), value = "Enter text..."),
  actionButton("action", label = "Enter")
                  ),
      tabPanel("Instructions",
               h4("1.  Select a model type."),
               "- Back Propogation model makes predictions by preferentially selecting 4 grams>3 grams>2 grams. 'Type' is the n-gram used",
               br(),
               "- Weighted model aggregates predicted word probability across all n-grams, weighting 4 grams>3 grams>2 grams.",
               br(),
               "- Equal model aggregates predicted word probability across all n-grams, weighting all n-grams evenly.",
               br(),
               h4("2. Enter a word or phrase into the box."),
               br(),
               h4("3. Press the 'Enter' button."),
               br(),
               h4("4. A list and barplot of predictions will be displayed.")
               
               
               
               )
      )
  ),
  
  mainPanel(
    h3("Predictions"),
    verbatimTextOutput("value4"),
  
  hr(),
  h3("Bar Plot"),
  plotOutput("value5")
  
  )
  )
  
))