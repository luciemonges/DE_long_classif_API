library(plumber)
library(xgboost)


# Load the saved model
model <- readRDS("model.rds")

#* @post /predict
#* @serializer json
function(req, res) {
  input <- data.frame(jsonlite::fromJSON(req$postBody))
  
  # Debug logging — shows in Render logs
  print("---- Incoming data ----")
  print(input[c(1:10),])
  
  # Mappings
  Q1.1_map = c("Legitimate" = 1, "Violent" = 2)
  Q1.2_map = c("Effective" = 1, "Useless" = 2)
  Q2_3_4_map = c("1_completely agree_with_statement_A" = 1, "2" = 2, "3" = 3,"4" = 4,"5" = 5,"6_completely_agree_with_statement_B"=6)
  Q5_6_map = c("1_strongly_disagree" = 1, "2" = 2, "3" = 3,"4" = 4,"5" = 5,"6_strongly_agree"=6)
  
  
  # Convert inputs
  input$Q1_1 = Q1.1_map[input$Q1_1]
  input$Q1_2 = Q1.2_map[input$Q1_2]
  input$Q2 = Q2_3_4_map[input$Q2]
  input$Q3 = Q2_3_4_map[input$Q3]
  input$Q4 = Q2_3_4_map[input$Q4]
  input$Q5 = Q5_6_map[input$Q5]
  input$Q6 = Q5_6_map[input$Q6]
  
  
  
  newdata = data.frame(
    F9a.1_1 = input$Q1_1
    F9a.2_1 = input$Q1_2
    F2.6_1 = input$Q3
    F5.13_1 = input$Q5
    F2.10_1 = input$Q4
    F2.4_1 = input$Q2
    D1.1_1 = input$Q6
  )

    
  fi_pred =  stats::predict(model, as.matrix(newdata))
  
  test_prediction  <- matrix(fi_pred, nrow = 7,
                             ncol=length(fi_pred)/7) %>%
    t() %>%
    data.frame() %>%
    mutate(max_prob = max.col(., "last"))
  
  
  list(segments=(as.character(test_prediction$max_prob)))
}
