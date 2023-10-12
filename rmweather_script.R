library(readxl)
library(rmweather)
library(dplyr)
library(openair)
library(ranger)
head(data_london)
data_london
DATA_Pune <- read_excel("DATA_Pune.xlsx", 
                        sheet = "Sheet8", col_types = c("date", 
                                                        "numeric", "text", "numeric", "numeric", 
                                                        "numeric", "numeric"))
DATA_Pune <- read_excel("DATA_Pune.xlsx", 
                        col_types = c("date", "date", "text", 
                                      "numeric", "numeric", "numeric", 
                                      "numeric", "numeric", "numeric"))
View(DATA_Pune)




data_prepared <- DATA_Pune %>%
  filter(variable == "no2") %>%
  
  rmw_prepare_data(na.rm = TRUE)

# Grow/train a random forest model and then create a meteorological normalised trend 
list_normalised <- rmw_do_all(
  data_prepared,
  variables = c(
    "date_unix", "day_julian", "weekday", "temp", "rh", "wd", "ws", 
    "atmospheric_pressure"),
  n_trees = 300,
  n_samples = 300,
  verbose = TRUE
)

# What units are in the list? 
names(list_normalised)

# Check model object's performance
rmw_model_statistics(list_normalised$model)

# Plot variable importances
list_normalised$model %>% 
  rmw_model_importance() %>% 
  rmw_plot_importance()

# Check if model has suffered from overfitting
rmw_predict_the_test_set(
  model = list_normalised$model,
  df = list_normalised$observations
) %>% 
  rmw_plot_test_prediction()

# How long did the process take? 
list_normalised$elapsed_times

# Plot normalised trend
rmw_plot_normalised(list_normalised$normalised)

TheilSen(list_normalised$normalised, pollutant = "value_predict", xlab = "date", ylab = "NO2",
         date.format = "%d-%m-%Y", alpha =0.001)

TheilSen(DATA_Pune, pollutant = "value", xlab = "date", ylab = "NO2", 
         date.format = "%d-%m-%Y", alpha =0.001)

# Investigate partial dependencies, if variable is NA, predict all
data_pd <- rmw_partial_dependencies(
  model = list_normalised$model, 
  df = list_normalised$observations,
  variable = NA
)

# Plot partial dependencies
data_pd %>% 
  filter(variable != "date_unix") %>% 
  rmw_plot_partial_dependencies()
