#Dif-in-Dif Model for Labor Productivity
install.packages("dplyr")
install.packages("ggplot2")
install.packages("stargazer")
install.packages("broom")
install.packages("gridExtra")
install.packages("gt")
install.packages("webshot")
install.packages("vtable")
install.packages("lmtest")
install.packages("sandwich")

library(ggplot2)
library(dplyr)
library(stargazer)
library(broom)
library(gridExtra)
library(gt)
library(webshot)
library(vtable)
library(lmtest)
library(sandwich)


#Load Data
LaborProduct_df <- read.csv("C:/Users/karan/Downloads/LaborProductivityCleaned 2018-2023.csv")

#Rename Columns
colnames(LaborProduct_df) <- c("industry", "year", "labor_productivity", "real_sectoral_output", "unit_labor_cost", "output_per_worker", "employment", "hourly_comp", "sec_out_price_def")

#Create Dummy Variables
LaborProduct_df <- LaborProduct_df %>% mutate(hybrid = ifelse(industry %in% c(5112, 5173, 5412, 5615, 52211, 54131, 54133, 54181, 56131), 1, 0))
LaborProduct_df <- LaborProduct_df %>% mutate(post_covid = ifelse(year >= 2022, 1, 0))
LaborProduct_df <- LaborProduct_df %>% mutate(hybrid_post = hybrid * post_covid)

#Remove 2020 and 2021
LaborProduct_df <- LaborProduct_df %>% filter(year != 2020 & year!=2021)
pre_treatment_data <- LaborProduct_df %>% filter(year < 2022)

#Balance Check
test_vars <- c("labor_productivity", "real_sectoral_output", "unit_labor_cost",
               "output_per_worker", "employment", "hourly_comp", "sec_out_price_def")


balance_results <- lapply(test_vars, function(var) {
  formula <- as.formula(paste(var, "~ hybrid"))
  lm(formula, data = pre_treatment_data)
})


robust_se_balance <- lapply(balance_results, function(model) {
  coeftest(model, vcov = vcovHC(model, type = "HC1"))
})



stargazer(robust_se_balance, type = "html", out = "balancefinal.html",
          title = "Balance Check with Robust Standard Errors",
          column.labels = c("Labor Productivity", "Real Sectoral Output", "Unit Labor Cost", 
                            "Output per Worker", "Employment", "Hourly Compensation", "Sectoral Output Price Deflator"),
          covariate.labels = c("Hybrid"))



#Plot the parallel trends for labor productivity
trend_data <- LaborProduct_df %>%
  group_by(hybrid, year) %>%
  summarize(mean_productivity = mean(labor_productivity, na.rm = TRUE))

ggplot(trend_data, aes(x = year, y = mean_productivity, color = factor(hybrid))) +
  geom_line(size = 1) +
  geom_vline(xintercept = 2022, linetype = "dashed", color = "black") +
  labs(title = "Parallel Trends in Labor Productivity",
       x = "Year",
       y = "Mean Labor Productivity",
       color = "Treatment Group") +
  scale_color_manual(values = c("0" = "red", "1" = "blue"),
                     labels = c("Control", "Treatment")) +
  theme_minimal()

# Plot parallel trends for output per worker

trend_data_output <- LaborProduct_df %>%
  group_by(hybrid, year) %>%
  summarize(mean_output_per_worker = mean(output_per_worker, na.rm = TRUE))

ggplot(trend_data_output, aes(x = year, y = mean_output_per_worker, color = factor(hybrid))) +
  geom_line(size = 1) +
  geom_vline(xintercept = 2022, linetype = "dashed", color = "black") +  
  labs(
    title = "Parallel Trends in Output per Worker",
    x = "Year",
    y = "Mean Output per Worker",
    color = "Treatment Group"
  ) +
  scale_color_manual(
    values = c("0" = "red", "1" = "blue"),  
    labels = c("Control", "Treatment")
  ) +
  theme_minimal()


#DiD Model

#Labor Productivity
did_model<- lm(labor_productivity ~ hybrid + post_covid + hybrid_post + 
                       real_sectoral_output + unit_labor_cost + employment + hourly_comp + sec_out_price_def, 
                     data = LaborProduct_df)
summary(did_model)

#Output per Worker
did_model2<- lm(output_per_worker ~ hybrid + post_covid + hybrid_post + 
                       real_sectoral_output + unit_labor_cost + employment + hourly_comp + sec_out_price_def, 
                     data = LaborProduct_df)
summary(did_model2)

#Robust Standard Errors
robust_se_labor <- coeftest(did_model, vcov = vcovHC(did_model, type = "HC1"))
summary(robust_se_labor)

robust_se_output <- coeftest(did_model2, vcov = vcovHC(did_model2, type = "HC1"))
summary(robust_se_output)


#Summary Table for DiD
stargazer(robust_se_labor, robust_se_output, type = "html", out = "finaltable.html",
          title = "DiD Models with Robust Standard Errors",
          column.labels = c("Labor Productivity", "Output per Worker"),
          covariate.labels = c("Hybrid","PostCOVID", "Hybrid X Post", "Real Sectoral Output", "Unit Labor Cost", 
                               "Employment", "Hourly Compensation", "Sectoral Output Price Deflator"),
          omit.stat = c("f", "ser"))
