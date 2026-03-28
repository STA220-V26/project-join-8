## Sanne analysis draft R script

## Libraries
library(dplyr)
library(ggplot2)

## 1. Create binary vaccination variable for any vaccine
vaccinated <- immunizations |>
  distinct(patient) |>
  mutate(vaccinated = 1)

## 2. Create binary COVID vaccination variable for any COVID vaccine (3 in total)
covid_vaccinated <- immunizations |>
  filter(grepl("COVID", description)) |>
  distinct(patient) |>
  mutate(covid = 1)

## 3. Merge with patient-level data
data <- patients |>
  left_join(vaccinated, by = c("id" = "patient")) |>
  mutate(vaccinated = ifelse(is.na(vaccinated), 0, 1)) |>
  mutate(insured = ifelse(healthcare_coverage > 0, 1, 0)) |>
  left_join(covid_vaccinated, by = c("id" = "patient")) |>
  mutate(covid = ifelse(is.na(covid), 0, 1))

## 4. Sanity checks
table(data$vaccinated)
table(data$insured)
table(data$covid)
table(data$insured, data$covid)

## 5. Descriptive analysis
prop.table(table(data$insured, data$covid), 1)

## 6. Logistic regression
model <- glm(covid ~ insured, family = binomial, data = data)
summary(model)
exp(coef(model))

## 7. Results
summary(model)
exp(coef(model))

## 8. Visualization 
ggplot(data, aes(x = factor(insured), fill = factor(covid))) +
  geom_bar(position = "fill") +
  labs(
    x = "Insurance status (0 = uninsured, 1 = insured)",
    y = "Proportion",
    fill = "COVID vaccinated"
  )

## 9. Check assumptions (not done yet)

# Sufficient sample size
# No perfect separation
# Multi-collinearity
# Independent observations




















