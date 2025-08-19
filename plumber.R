library(plumber)
library(xgboost)

# Load the saved model
model <- readRDS("model.rds")

#* @post /predict
#* @serializer json
function(req, res) {
  input <- jsonlite::fromJSON(req$postBody)
  
  # Debug logging — shows in Render logs
  print("---- Incoming data ----")
  print(input)

  # Mappings
  Q1.1_map = c("Legitimate" = 1, "Violent" = 2)
  Q1.2_map = c("Effective" = 1, "Useless" = 2)
  Q2_3_4_map = c("1_completely agree_with_statement_A" = 1, "2" = 2, "3" = 3,"4" = 4,"5" = 5,"6_completely_agree_with_statement_B"=6)
  Q5_6_map = c("1_strongly_disagree" = 1, "2" = 2, "3" = 3,"4" = 4,"5" = 5,"6_strongly_agree"=6)

  # Convert inputs
  q1.1 <- as.numeric(Q1.1_map[input$Q1_1])
  q1.2 <- as.numeric(Q1.2_map[input$Q1_2])
  q2 <- as.numeric(Q2_3_4_map[input$Q2])
  q3 <- as.numeric(Q2_3_4_map[input$Q3])
  q4 <- as.numeric(Q2_3_4_map[input$Q4])
  q5 <- as.numeric(Q5_6_map[input$Q5])
  q6 <- as.numeric(Q5_6_map[input$Q6])
  

  # Create dataframe
  newdata <- data.frame(
    F9a.1_1 = q1.1,
    F9a.2_1 = q1.2,
    F2.6_1 = q3,
    F5.13_1 = q5,
    F2.10_1 = q4,
    F2.4_1 = q2,
    D1.1_1 = q6
  )

  pred <- predict(model, newdata)

  list(segment = as.character(pred))
}
