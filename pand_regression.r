# Load necessary libraries
library(dplyr)
library(stats)

# ------------------------------------------------------------------------------
# Multivariate Poisson regression reults
# ------------------------------------------------------------------------------

# Fit Poisson regression model
poisson_model <- glm(daily_deaths ~ policy_1 + policy_2 + policy_3,
                     family = poisson, data = country_df)

# Summarize the model
summary(poisson_model)

# Model performance
null_model <- glm(daily_deaths ~ 1, family = poisson, data = country_df)

# Calculate deviance for the null model
null_deviance <- null_model$deviance

# Calculate deviance for the fitted model
fitted_deviance <- poisson_model$deviance

# Calculate the difference in deviance
deviance_difference <- null_deviance - fitted_deviance

# Print the results
print(paste("Null Deviance:", null_deviance))
print(paste("Fitted Deviance:", fitted_deviance))
print(paste("Deviance Difference:", deviance_difference))

# ------------------------------------------------------------------------------
# Automated Poisson regression reults tables with calculated deviance difference
# ------------------------------------------------------------------------------

# List of response variables
response_variables <- c("daily_deaths", "daily_cases", "daily_hospitalizations", "daily_icu", "daily_test")

# List of country data frames
country_dfs <- list(brazil_df, china_df, india_df, israel_df, italy_df, senegal_df, 
                    sout_africa_df, south_korea_df, united_kingdom_df, united_states_df)

# Create an empty data frame to store results
results_df <- data.frame(Response_Variable = character(), 
                         Country = character(),  # Add a column for the country name
                         Estimate = numeric(),
                         Std_Error = numeric(), 
                         Z_Value = numeric(),
                         Pr_Z = numeric(), 
                         Lower_CI = numeric(), 
                         Upper_CI = numeric(),
                         Deviance = numeric(),
                         Null_Deviance = numeric(),  # Add a column for null deviance
                         Fitted_Deviance = numeric(),  # Add a column for fitted deviance
                         Deviance_Difference = numeric(),  # Add a column for deviance difference
                         stringsAsFactors = FALSE)

# Loop through country data frames
for (country_df in country_dfs) {
  # Get the country name (assuming a "Country" column exists)
  country_name <- unique(country_df$Country)
  
  # Loop through response variables
  for (response_var in response_variables) {
    # Fit Poisson regression model
    poisson_model <- glm(paste(response_var, "~ policy_1 + policy_2 + policy_3", sep = ""),
                         family = poisson, data = country_df)
    
    # Fit null model
    null_model <- glm(paste(response_var, "~ 1", sep = ""), family = poisson, data = country_df)
    
    # Calculate deviance for the null model
    null_deviance <- null_model$deviance
    
    # Calculate deviance for the fitted model
    fitted_deviance <- poisson_model$deviance
    
    # Calculate the difference in deviance
    deviance_difference <- null_deviance - fitted_deviance
    
    # Extract relevant information and store in results_df
    results_df <- rbind(results_df, data.frame(Response_Variable = response_var,
                                               Country = country_name,
                                               Estimate = exp(coef(poisson_model)),
                                               Std_Error = summary(poisson_model)$coefficients[, "Std. Error"],
                                               Z_Value = summary(poisson_model)$coefficients[, "z value"],
                                               Pr_Z = summary(poisson_model)$coefficients[, "Pr(>|z|)"],
                                               Lower_CI = exp(confint(poisson_model))[,"(Intercept)"],
                                               Upper_CI = exp(confint(poisson_model))[,"(Intercept)"],
                                               Deviance = poisson_model$deviance,
                                               Null_Deviance = null_deviance,
                                               Fitted_Deviance = fitted_deviance,
                                               Deviance_Difference = deviance_difference))
  }
}

# Print the results
print(results_df)