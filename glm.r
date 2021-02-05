library(h2o)
h2o.init()

df = h2o.importFile('Data/Datasets/grades.csv', sep=",", header= TRUE)

predictors <- c("famsup","schoolsup", "paid", "Pstatus", "famrel","transition")
response <- "transition"

df_splits <- h2o.splitFrame(data =  df, ratios = 0.8, seed = 1234)
train <- df_splits[[1]]
test <- df_splits[[2]]

# build a GLM model
prostate_glm <- h2o.glm(family = "binomial",
                        x = predictors,
                        y = response,
                        training_frame = train,
                        lambda = 0,
                        compute_p_values = TRUE)

# predict using the GLM model and the testing dataset
predict <- h2o.predict(object = prostate_glm, newdata = test)

y_pred = ifelse(predict$p0 >= 0.5, 0, 1)

# Print the coefficients table
#prostate_glm@model$coefficients_table

# Retrieve a graphical plot of the standardized coefficient magnitudes
#h2o.std_coef_plot(prostate_glm)


