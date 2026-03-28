# Prepare the models from RDS files.
cart_model <- readRDS("./data//cart.rds")
knn_model <- readRDS("./data//knn.rds")
logreg_model <- readRDS("./data//logreg.rds")
naivebayes_model <- readRDS("./data//naivebayes.rds")
rf_model <- readRDS("./data//rf.rds")
svm_model <- readRDS("./data//svm.rds")

# Load libraries.
library(shiny)
library(dplyr)
library(bslib)
library(caret)
library(kernlab)
library(ranger)
library(caTools)
library(naivebayes)

ui <- page_fillable(
  "Stroke Prediction",
  layout_columns(
    card(helpText("Select below:"),
         selectInput(
           "gender", label = "Gender:",
           choices = list("Male","Female","Other"),multiple = FALSE),
         numericInput(
           "age",label="Age:",value=20),
         checkboxInput("hypertension", "I have hypertension", value = FALSE),
         checkboxInput("heart_disease", "I have heart disease", value = FALSE),
         checkboxInput("ever_married", "I'm married/have been married", value = FALSE),
         selectInput(
           "work_type", label = "Type of employment: (Select children if you are below 18)",
           choices = list("children","Govt_job","Never_worked","Private","Self-employed"), multiple=FALSE),
         selectInput(
           "Residence_type", label = "Type of residence:",
           choices = list("Rural","Urban"),multiple=FALSE),
         numericInput(
           "avg_glucose_level", label= "Average glucose level:", value=106.1477),
         # Use mean as default.
         numericInput(
           "bmi", label="BMI:", value=28.92663),
         # Use mean as default
         selectInput(
           "smoking_status", label="Smoking status:",
           choices = list("formerly smoked","never smoked","smokes","Unknown")),),
    card(
      accordion(  
        accordion_panel( 
          title = "Logistic Regression", 
          textOutput("logreg")
        ),  
        accordion_panel(
          title = "CART",
          textOutput("cart")
        ),  
        accordion_panel(
          title = "K-nearest Neighbors",
          textOutput("knn")
        ),  
        accordion_panel(
          title = "Naive Bayesian",
          textOutput("naivebayes") 
        ),
        accordion_panel(
          title = "Random forest",
          textOutput("rf") 
        ),
        accordion_panel(
          title = "Support vector machine (linear)",
          textOutput("svm") 
        )
      ),
    ),
    col_widths = c(3,9) 
  )   
)


# Define server logic.
server <- function(input, output) {

  patient_data <- reactive({
    data.frame(
      gender = input$gender,
      age = input$age,
      hypertension = input$hypertension, # Models often prefer 0/1
      heart_disease = input$heart_disease,
      ever_married = input$ever_married, # Match your training labels!
      work_type = input$work_type,
      Residence_type = input$Residence_type,
      avg_glucose_level = input$avg_glucose_level,
      bmi = input$bmi,
      smoking_status = input$smoking_status,
      stringsAsFactors = TRUE # Critical for most R models
    )
  })
  
  generate_prediction <- function(model, model_name) {
    renderText({
      prediction <- predict(model, patient_data())
      
      is_high_risk <- if(is.factor(prediction)) prediction == "1" else prediction == 1
      
      if (is_high_risk) {
        paste("According to the", model_name, "model, you have a high chance for stroke.")
      } else {
        paste("According to the", model_name, "model, you have a low chance for stroke.")
      }
    })
  }
  
  # 3. Assigning the outputs
  output$logreg     <- generate_prediction(logreg_model, "Logistic Regression")
  output$cart       <- generate_prediction(cart_model, "CART")
  output$knn        <- generate_prediction(knn_model, "K-nearest Neighbors")
  output$naivebayes <- generate_prediction(naivebayes_model, "Naive Bayesian")
  output$rf         <- generate_prediction(rf_model, "Random Forest")
  output$svm        <- generate_prediction(svm_model, "SVM")
}

# Run the application 
shinyApp(ui = ui, server = server)
