
library(gbm)
library(glmnet)
library(caret)
library(ggplot2)
library(pROC)
library(ggforce)
library(shiny)
library(gridExtra)
#define server
server <- function(input,output){observeEvent(input$goButton, {
  output$piediagram<-renderPlot({
    var=c("Result", "hemoglobin","alkaline_phosphatase","AST","Fasting_blood_glucose","glycated_hemoglobin","age","hypertension","gender")
    
    data=read.csv("data.csv",header = T,encoding = "GBK")
    colnames(data)
    
    data$Result = factor(data$Result,levels = c(0,1),labels = c('No','Yes'))
    
    set.seed(52)
    inTrain = createDataPartition(y=data[,"Result"], p=0.7, list=F)
    traindata = data[inTrain,]
    testdata = data[-inTrain,]
    
    dev = traindata[,var]
    
    set.seed(520)
    train.control <- trainControl(
      method = 'repeatedcv',
      number = 10, 
      repeats = 5, 
      classProbs = TRUE, 
      summaryFunction = twoClassSummary)
    
    gbm.tune.grid = expand.grid(n.trees = 100, interaction.depth = 2,shrinkage = 0.05, n.minobsinnode = 3)
    
    
    
    load("model.RData")
    
    ###############################################################
    vaddata=data.frame(hemoglobin=input$hemoglobin,alkaline_phosphatase=input$alkaline_phosphatase,
                       gender=input$gender,hypertension= input$hypertension,age = input$age,glycated_hemoglobin = input$glycated_hemoglobin,Fasting_blood_glucose = input$Fasting_blood_glucose,AST = input$AST)  
    
    vaddata <- as.data.frame(lapply(vaddata, function(x) {
      if (is.character(x)) {
        return(as.numeric(x))
      }
      return(x)
    }))
    
    
    test_pro = predict(model, newdata = vaddata, type = 'prob')[2]
    test_pro = as.numeric(test_pro[,1])
    
    
    ratio = c(test_pro*100,(1-test_pro)*100)
    disease = c("Risk", "non-Risk")
    
    A = data.frame(ratio, disease)
    A$ratio=round(A$ratio)
    ggplot(A)+
      geom_arc_bar(data=A,
                   stat = "pie",
                   aes(x0=0,y0=0,r0=1,r=2,
                       amount=ratio,fill=disease
                   ))+
      theme_bw()+
      theme(
        axis.text = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.background = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major = element_blank(),
        plot.background = element_blank(),
        panel.border = element_blank(),
        legend.position = "top",
        legend.text = element_text(size = 25),
        legend.spacing.x = unit(1,"cm"),
        legend.key.width = unit(1, "cm"),
        plot.title = element_text(hjust=0.5,face = "bold",size = 8),
        legend.title = element_blank()
      )+
      #labs(title = "Male individuals")+
      scale_x_continuous(breaks = NULL)+
      scale_y_continuous(breaks = NULL)+
      scale_fill_manual(values=c("#ED0000FF", "#0099B4FF"),
                        breaks=c("Risk","non-Risk"))+
      geom_text(x=0.1, y=0.3, label ="Risk", fontface=2,size=9)+
      geom_text(x=0.1, y=-0.3, label =scales::percent(test_pro, 0.01),
                fontface=2,size=9)
    
    
    
  })
})
  
}