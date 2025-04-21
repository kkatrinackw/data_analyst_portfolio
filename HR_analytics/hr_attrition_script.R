hr <- read.csv("~/Desktop/Coding Temple/R/HR_capstone_dataset.csv")
View(hr)

summary(hr)
sum(duplicated(hr)
unique(updated_hr)


left_employee <- hr %>%
#  group_by(has_left_company) %>%
  filter(has_left_company == 1)
  summarise(count = n())
View(left_employee)
summary(left_employee)




accident_employee <- hr %>%
  group_by(had_work_accident) %>%
  summarise(count = n())
accident_employee

left_and_accident <- hr %>%
  filter(has_left_company == 1, had_work_accident == 1) %>%
  summarise(count = n())
left_and_accident
