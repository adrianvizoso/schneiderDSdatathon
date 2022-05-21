library(data.table)
library(lightgbm)
library(Matri)
library(xgboost)

set.seed(123)

#Read data after defining path
path <- "../"
all_data <- readRDS(paste0(path, "all_data.rds"))
test_submit <- readRDS(paste0(path, "test.rds"))

######################## FEATURES #########################################
## Feature selection based on correaltion with the target
target <- "pollutant"
num_vars <- c("max_wind_speed","avg_wind_speed","min_wind_speed","max_temp",
              "avg_temp","min_temp","fog_days")

cor(x = as.numeric(all_data[[target]]),
    y = all_data[,..num_vars],method = "spearman")*100

# All vars are less than 1% correlated with the response

# Selecting vars with predictive potential 
vars_model <- c("country", "sector_name", "main_activity_label")
target <- "pollutant"

df <- all_data[,.(city,country,
                          # facility_id, year,month,day,  # id, location and yearmonthday vars
                          sector_name,main_activity_label,  # info vars
                          max_wind_speed,avg_wind_speed,min_wind_speed,max_temp,avg_temp,min_temp,fog_days, # Num var, must drop someones
                          pollutant)] # response var
df_copy <- data.table::copy(df)

# Convert from factor to numeric due to issues with xgboost nomenclatures
df[, ':='(main_activity_label = as.numeric(main_activity_label),
          sector_name = as.numeric(sector_name),
          country = as.numeric(country),
          pollutant = as.numeric(pollutant))]

# Define vars
vars_model <- c("country", "sector_name", "main_activity_label")
target <- "pollutant"

######################### MODELING #############################

# Define extensive grid
grid_default <- expand.grid(
  nrounds = c(200, 300, 500),
  max_depth = c(2, 4, 6),
  learning_rate = c(0.001, 0.01),
  early_stopping_rounds = 30,
  eta = c(0.1, 0.3, 0.7),
  gamma = c(0.3, 0.5, 0.8),
  subsample = c(0.5, 0.7)
)
grid_default <- as.data.table(grid_default)

grid_default[, index := 1:nrow(grid_default)]

# Perform bayesian sampling of 200 combos out of all possible to find minimum
index_random = sample(grid_default$index, 200)

grid_default <- grid_default[index %in% index_random]

# Do custom 5-fold cross-validation with bayesian hyper opt.
# Optimize towards the F1-score

grid_default[, index := 1:nrow(grid_default)]
k_folds <- 5
for(i in 1:nrow(grid_default)){
  f1_scores <- c()
  for(m in 1:k_folds){
      
      print(paste0("Looping on k_folds = ", m, " & round = ", i))
      
      ind <- sample(2, nrow(df), replace = TRUE, prob = c(0.7, 0.3))
      train_data <- df[ind==1,]
      test_data <- df[ind==2,]
      
    
      train_matrix = xgb.DMatrix(as.matrix(train_data[,..vars_model]), label = train_data$pollutant)
      
      test_matrix = xgb.DMatrix(as.matrix(test_data[,..vars_model]), label = test_data$pollutant)
      
      
      model <- xgboost(data = train_matrix, 
                                    label = train_data$pollutant, 
                                    objective = "multi:softmax",
                                    num_class = 4,
                                    nrounds = grid_default[i,]$nrounds,
                                    early_stopping_rounds = grid_default[i,]$early_stopping_rounds,
                                    learning_rate = grid_default[i,]$learning_rate,
                                    max_depth = grid_default[i,]$max_depth,
                                    eta = grid_default[i,]$eta,
                                    gamma = grid_default[i,]$gamma,
                                    subsample = grid_default[i,]$subsample,
                                    verbose = 0
                                    )
      
      f1 <- mean(confusionMatrix(as.factor(predict(model, test_matrix)), as.factor(test_data$pollutant))[[4]][,7])
      f1_scores <- c(f1_scores, f1)
    }
  
  grid_default[index == i, f1_score := mean(f1_scores)]
}

# Select best combinations based on f1 score
best_combs <- grid_default[f1_score == max(f1_score)]


######################### PREDICTIONS #############################

# Obtain proper format of test for submission
test_submit <- as.data.table(test_submit)
test_submit <- test_submit[,.(country = countryName, sector_name = eprtrSectorName, main_activity_label = EPRTRAnnexIMainActivityLabel)]

test_submit[, ':='(main_activity_label = as.numeric(as.factor(main_activity_label)),
                   sector_name = as.numeric(as.factor(sector_name)),
                   country = as.numeric(as.factor(country)))]

# Perform predictions in numeric format
test_matrix = xgb.DMatrix(as.matrix(test_submit[,..vars_model]), label = test_submit$country)
test_submit[, pollutant := predict(model, test_matrix)]

# Convert numeric to corresponding values
test_submit[pollutant == 1, pollutant_char := "Carbon dioxide (CO2)"]
test_submit[pollutant == 2, pollutant_char := "Methane (CH4)"]
test_submit[pollutant == 3, pollutant_char := "Nitrogen oxides (NOX)"]

test_submit[, pollutant := NULL]
setnames(test_submit, "pollutant_char", "pollutant")

# Store file
write.csv(test_submit, paste0(path, "/test_submission.csv"), row.names = FALSE)

