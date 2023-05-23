library(shiny)
library(randomForest)
library(shinythemes)
library(dplyr)
library(remotes)
library(fastDummies)
library(caret)
#--------make sure all library is installed-------------
#setwd('D:/Crystal/Capstone_project') #set the working directory
traindataset<- read.csv("strokeTrain.csv",header=TRUE)  #read the train dataset from the directory
traindataset <- dplyr::select(traindataset, -X) #remove the X value which is ID

traindataset$stroke <- factor(traindataset$stroke,
                               levels = c(1, 0),
                               labels = c('Yes', 'No'))
str(traindataset)
traindataset$stroke<-as.factor(traindataset$stroke)
# convert the target variable (stroke) as factor
str(traindataset) #check structure of dataset
model <- randomForest(stroke ~ ., data = traindataset) # build the random forest model
# start building the UI interface
ui <- fluidPage(theme = shinytheme("united"),
                tags$head(
                  tags$style(HTML("
                  .button {
                    background-color: #e95420;
                    color: white;
                    padding: 10px 20px;
                    border: none;
                    text-align: center;
                    text-decoration: none;
                    display: inline-block;
                    font-size: 16px;
                    margin: 4px 2px;
                    cursor: pointer;
                    border-radius: 8px;
                    width:100%;
                  }"))),
                navbarPage(
                  # theme = "cerulean",  # <--- To use a theme, uncomment this
                  "Vegetable  Chicken Group",
                  tabPanel("Model",
                           sidebarLayout(position = "left",
                                         sidebarPanel(
                                           tags$h3("Input:"),
                                           radioButtons("gender", h4("Gender"),     #build radio button for gender
                                                        choices = list("MALE" = 2,
                                                                       "FEMALE" = 1),
                                                        selected = 1),
                                           fluidRow(  # 2 column in 1 row
                                             column(6,
                                                    numericInput("age",
                                                                 h4("Age"),
                                                                 value = 1)),
                                             column(6,
                                                    selectInput("evermaried", h4("Ever Married"),
                                                                choices = list("Yes" = 2, "No" = 1),
                                                                selected = 1)),
                                             column(6,
                                                    numericInput("avg_glucose_level",
                                                                 h4("Average Glucose Level"),
                                                                 value = 1)),
                                             column(6,
                                                    numericInput("bmi",
                                                                 h4("BMI"),
                                                                 value = 1)),
                                             column(6,
                                                    selectInput("heart_disease", h4("Heart Disease"),
                                                                choices = list("Yes" = 2, "No" = 1),
                                                                selected = 1)),
                                           ),
                                             actionButton("submit","Predict",class = "button") #add an action button
                                         ),
                                         mainPanel(
                                          div(
                                            style = "height: 440px; text-align: center;",
                                            imageOutput("Image")
                                          ),
                                           verbatimTextOutput("myOutput"),  #output field
                                           verbatimTextOutput("final")      #output field
                                         ),

                           ) # mainPanel
                  ),# sidebarPanel
                ), # Navbar 1, tabPanel
) # navbarPage
# fluidPage
#this is the place where the server backend process your model
server <- function(input, output) {
  output$Image<-renderImage({
    image_path<-"WhatsApp Image 2023-05-14 at 1.26.31 AM.jpeg"
    list(
      src=image_path,
      alt="Stroke Image",
      width="100%",
      height="420px")
  },deleteFile = FALSE)
  formData <-reactiveValues(data = data.frame())
  observeEvent(input$submit,{  #this event will do after the action button was click
    set.seed(123)
    # get all the data and make them into a dataframe
    new_row <- data.frame(gender = input$gender,
                          age = input$age,
                          ever_married = input$evermaried,
                          avg_glucose_level = input$avg_glucose_level,
                          bmi = input$bmi,
                          heart_disease = input$heart_disease)
    formData$data <- rbind(formData$data, new_row)
    new_row <- new_row %>% mutate(avg_glucose_level = 1/avg_glucose_level) #reciprocal avg glucose level
    new_row <- as.data.frame(lapply(new_row, as.numeric))  #convert all data into numeric
    print(str(new_row)) #check the structure of input dataset
    # Perform prediction using the trained model
    prediction <- predict(model, newdata = new_row,"prob")  #predict the model and output in probability
    final <- predict(model, newdata = new_row)              #predict the model and output in yes/no
    output$myOutput <- renderText({                         #this is the output field
      doesnot_have_stroke <- prediction[1]
      have_stroke <- prediction[2]
      paste("Dont Have stroke:", have_stroke, ", Have stroke:", doesnot_have_stroke)
    })
    output$final <- renderText({
      paste("answer:",final)
    })})}
#this is the place where the UI run
shinyApp(ui = ui, server = server)

