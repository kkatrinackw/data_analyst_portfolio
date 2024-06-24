# import data
hr <- read.csv("~/Desktop/Coding Temple/R/Capstone_II/HR_capstone_dataset.csv")
View(hr)

summary(hr)

#identify duplicated value
duplicated(hr)

# count duplicated data
sum(duplicated(hr))

# remove duplicated data
hr_processed <- hr %>% distinct()

# double check if there is any duplicated data
sum(duplicated(hr_processed))
summary(hr_processed)
write.csv(hr_processed, file = 'hr_processed.csv', row.names = TRUE)
View(hr_processed)

hr_processed %>% group_by(Department) %>% summarise(count=n())
hr_processed %>% group_by(years_at_company, has_left_company) %>% summarise(count=n())

# df: stay employees VS. left employees
hr_left_employees <- hr_processed %>%
  filter(has_left_company == 1) 
View(hr_left_employees)
summary(hr_left_employees)

hr_stay_employees <- hr_processed %>%
  filter(has_left_company == 0) 
View(hr_stay_employees)
summary(hr_stay_employees)

# logistic regression for categorical variables
install.packages("caTools")
install.packages("ROCR")
library(caTools)
library(ROCR)

hr_logistic_regression_model <- glm(has_left_company ~ had_work_accident + promoted_in_last_5_years + Department + salary_level,
                                family = "binomial", data = hr_processed)
summary(hr_logistic_regression_model)
str(hr_logistic_regression_model)

hr_mode_coeff <- summary(hr_logistic_regression_model)$coefficients
print(hr_mode_coeff)

hr_mode_extraction <- data.frame(
  factors = c("had_work_accident", "promoted_in_last_5_years", "salary_levellow"),
  p_values = hr_mode_coeff[c("had_work_accident", "promoted_in_last_5_years", "salary_levellow"), "Pr(>|z|)"]
)

## bar chart to represent the last 3 factors w. the smaller p-values 
ggplot(data = hr_mode_extraction , aes(x = factors, y = p_values)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  geom_text(aes(label = paste("p = ", round(p_values, 12))), vjust = -0.) + 
  labs(title = "Barplot of Factors with Smallest p-values", x = "variables", y = "p-values") +
  scale_y_continuous(trans = "log10") + 
  theme_minimal()

#linear regression for continuous variables 
hr_linear_regression_model <- lm(has_left_company ~ satisfaction_level + last_performance_rating + number_of_projects + avg_monthly_hours + years_at_company,
                                 data = hr_processed)
summary(hr_linear_regression_model)

# wilcoxon test: compare the means of left_employees and stay_employees
##satisfaction level
#compare_sat_level <- wilcox.test(hr_stay_employees$satisfaction_level, hr_left_employees$satisfaction_level)
#compare_sat_level

#Impact of Work Accident on Satisfaction Levels
had_work_accident_rename <- factor(hr_processed$had_work_accident, levels = c(0, 1), labels = c("No", "Yes"))
ggplot(hr_processed, aes(x = had_work_accident_rename, y = satisfaction_level, fill = factor(has_left_company))) +
  geom_violin(scale = "width", trim = FALSE) +
  labs(x = "Had Work Accident", y = "Satisfaction Level", fill = "Employment Status") +
  ggtitle("Violin Plot of Satisfaction Level by Work Accident and Employment Status") +
  scale_fill_manual(values = c("orange", "purple"), name = "Employment Status", labels = c("Stay", "Leave")) +
  theme_minimal()


# Load the gplots package
library(gplots)


#Impact of Last Performance Test on Salary Levels
salary_level_rename <- factor(hr_processed$salary_level, levels = c("high", "medium", "low"))
has_left_company_rename <- factor(hr_processed$has_left_company, levels = c(0, 1), labels = c("No", "Yes"))
has_left_company_rename
#comparison 
ggplot(data = hr_processed, aes(x = salary_level_rename, y = last_performance_rating, fill = has_left_company_rename)) +
  geom_boxplot() +
  labs(title = "Relationship Between Salary Level and Last Performance Rating", x = "Salary Level", y = "Last Performance Rating", fill = "Has Left Company") +
  scale_fill_manual(values = c("No" = "blue", "Yes" = "red")) + 
  theme(plot.title = element_text(size = 13, hjust = 0.5))


#Impact of Job Tenure on Promotion Opportunity
hr_processed %>%
  group_by(promoted_in_last_5_years) %>%
  summarise(count = n()) %>%
  mutate(percentage = count / sum(count) * 100)

promotion_opportunity <- factor(hr_processed$promoted_in_last_5_years, 
                                levels = c(0,1),
                                labels = c("No", "Yes"))
levels(promotion_opportunity)

ggplot(data = hr_processed, aes(x = years_at_company, y = promotion_opportunity, color = has_left_company_rename)) +
  geom_point(position = "jitter") +
  labs(title = "Relationship Between Job Tenure and Promotion Opportunity", x = "Years at Company", y = "Promoted in Last 5 Years") +
  theme(plot.title = element_text(size = 14, hjust = 0.5))


#Impact of Workload on Average Working Hours
ggplot(hr_processed, aes(x = number_of_projects, y = avg_monthly_hours, color = has_left_company_rename))+
  geom_point(show.legend = TRUE) +
  facet_wrap(~ Department) 

ggplot(hr_processed, aes(x = number_of_projects, y = avg_monthly_hours, color = has_left_company_rename))+
  geom_point(show.legend = TRUE) +
  labs(title = "Relationship Between Number of Project and Average Monthly Hours", x = "Number of Project", y = "Average Monthly Hours", fill = "Employment Status")

