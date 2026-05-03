library(gbm)
library(glmnet)
library(caret)
library(ggplot2)
library(pROC)
library(ggforce)
library(shiny)
library(gridExtra)





#define UI
ui <- fluidPage(
  titlePanel("Gallstone risk assessment in MASLD patients(For preliminary screening and research purposes only, not for final diagnosis)"),     #Big title
  sidebarLayout(             #Border Layout
    sidebarPanel(
      
      
      
      #######################################################
      fluidRow(
        column(8,
               selectInput("gender",h5("gender(Female/Male)"),
                           choices = list("Female" = 2,
                                          "Male" = 1),
                           selected = 1))),
      #####################################################
      fluidRow(
        column(8,
               selectInput("hypertension",h5("hypertension(yes/no)"),
                           choices = list("yes" = 1,
                                          "no" = 0),
                           selected = 1))),
      
      
      
      
      
      ####################################################
      fluidRow(
        column(8,
               numericInput("age",h5("age(year)"),
                            min = 0,max = 100,value = 50))),
      
      ####################################################
      fluidRow(
        column(8,
               numericInput("glycated_hemoglobin",h5("glycated_hemoglobin(%)"),
                            min = 0,max = 20,value = 5))),
      
      
      ####################################################
      fluidRow(
        column(8,
               numericInput("Fasting_blood_glucose",h5("Fasting_blood_glucose(mmol/L)"),
                            min = 0,max = 30,value = 6))),
      
      
      
      ####################################################
      fluidRow(
        column(8,
               numericInput("AST",h5("AST(U/L)"),
                            min = 0,max = 2000,value = 15))),
      
      
      ####################################################
      fluidRow(
        column(8,
               numericInput("alkaline_phosphatase",h5("alkaline_phosphatase(U/L)"),
                            min = 0,max = 400,value = 30))),
      
      
      ####################################################
      fluidRow(
        column(8,
               numericInput("hemoglobin",h5("hemoglobin(g/L)"),
                            min = 0,max = 300,value = 100))),
      
      
      
      
      ##########################################################
      actionButton("goButton", "Predict")
    ),
    
    mainPanel(
      print(""),
      plotOutput("piediagram")
    )
    
  ))