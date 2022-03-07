#' Risks from Extreme Temperature Events
#' 
#' This function determines a level of risk associated with extreme temperature events.
#' @param temperature_threshold_extreme Threshold at which temperature is categorized as extreme (very dangerous)
#' @param temperature_threshold Threshold at which temperature is categorized as potentially dangerous
#' @param temperature Maximum daily temperature (C)
#' @param income Individual income (low, medium, high)
#' @param age Individual age (child, adult, elder)
#' @param occupation Individual occupation(primarily outdoor, primarily indoor, or combination of both)
#' @return risk (very low, low, medium, high, very high) and n_extremes (number of extreme temperature days between 1988 and 2010)

#function definition
temp_risk = function(temperature_threshold_extreme = 35, temperature_threshold = 30, temperature, occupation, age, income, x) {
  
  # temperature risk scale (0-100) if temperature has been 32 degrees or more for 3 or more consecutive days
  temp_risk_value = case_when(temperature < temperature_threshold ~ 0, #low risk
                     temperature >= temperature_threshold & temperature < temperature_threshold_extreme ~ 50, #medium risk
                     temperature >= temperature_threshold_extreme ~ 100) #high risk
  
  #occupation risk scale (0-10); added risk based on occupation
  occupation_risk_value = case_when(occupation == "outdoor" ~ 10,
                                    occupation == "indoor" ~ 0,
                                    occupation == "combination" ~ 5,
                                    occupation == "none" ~ 0)
  
  #age risk scale (0-10); added risk based on age
  age_risk_value = case_when(age == "child" ~ 10,
                             age == "adult" ~ 0,
                             age == "elder" ~ 10)
  
  #income risk scale (0-10); added risk based on income
  income_risk_value = case_when(income == "high" ~ 0,
                                income == "medium" ~ 5,
                                income == "low" ~ 10)
  
  risk_value = temp_risk_value + occupation_risk_value + age_risk_value + income_risk_value
  
  #risk categories (very low, low, medium, high, very high)
  risk = case_when(risk_value < 30 ~ "very low", #low temp risk, high other categories
                   risk_value >= 30 & risk_value <= 50 ~ "low", #medium temp risk, low other categories
                   risk_value > 50 & risk_value <= 80 ~ "medium", #medium temp risk, medium/high other categories
                   risk_value >=80 & risk_value <= 100 ~ "high", #high temp risk, low other categories
                   risk_value > 100 ~ "very high") #high temp risk, medium/high other categories

  #count number of extreme temperatures
  n_extremes = sum(temperature >= 35)
  
  #only return risk if length is less than one (i.e., if adding to a column in a data frame)
  if (length(x) <= 1)
    return(risk) else
      return(list(risk=risk, n_extremes=n_extremes))
}

