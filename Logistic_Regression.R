# Library Installations and Working Directory
library(cowplot)
library(tidyverse)
library(haven)
library(ggplot2)
library(rstudioapi)
library(ggeffects)
library(see)
library(effects)
library(caTools)
library(sjmisc)
library(sjPlot)
setwd(dirname(getActiveDocumentContext()$path))

# Data Import
data <- read_dta("../Data/data.dta")

# Data Exploration and Cleaning
head(data)
str(data)
data <- data %>%
  uncount(weights = n) %>%
  rename("age_group" = "age", "education" = "educ", "desire_children" = "desire", "contraceptive_use" = "cuse") %>%
  mutate(age_group = factor(age_group, levels = paste0(1:4), labels = c("< 25", "25 - 29", "30 - 39", "40 - 49")),
         education = factor(education, levels = paste0(0:1), labels = c("No Education", "Some Education")),
         desire_children = factor(desire_children, levels = paste0(0:1), labels = c("Wants more", "Does not want more")),
         contraceptive_use = factor(contraceptive_use, levels = paste0(0:1), labels = c("Does not Use", "Uses")))

# Descriptive Statistics
ggplot(data, aes(age_group, color = education, fill = education, shape = education)) + 
  geom_bar() +
  ylab("Count") +
  xlab("Age Group") +
  ggtitle("Respondents by Age and Education Level") +
  theme(plot.title = element_text(hjust = .5))

ggplot(data, aes(age_group, color = desire_children, fill = desire_children, shape = desire_children)) + 
  geom_bar() +
  ylab("Count") +
  xlab("Age Group") +
  ggtitle("Respondents by Age and Desire for Children") +
  theme(plot.title = element_text(hjust = .5)) 

ggplot(data, aes(age_group, color = contraceptive_use, fill = contraceptive_use, shape = contraceptive_use)) + 
  geom_bar() +
  ylab("Count") +
  xlab("Age Group") +
  ggtitle("Respondents by Age and Contraceptive Use") +
  theme(plot.title = element_text(hjust = .5))

# Checking to see if there is representation in each demographic
xtabs(~ contraceptive_use + age_group, data = data)
xtabs(~ contraceptive_use + education, data = data)
xtabs(~ contraceptive_use + desire_children, data = data)

# Create Training and Test Data
split <- sample.split(data$contraceptive_use, SplitRatio = .75)
data_train <- subset(data, split == TRUE)
data_test <- subset(data, split == FALSE)

# Logistic Regression - Predict contraceptive Use by age group, education, and desire for children
lr_model <- glm(contraceptive_use ~ ., data = data_train, family = "binomial")
summary(lr_model)
test <- predict(lr_model, newdata = data_test, type = "response")

# contraceptive_use = -2.06 + .4619(25 - 29 AG) + .9565(30 - 39 AG) + 1.267(40 - 49 AG) + .4268(some_education) + .807(no_more_children)
# The odds associated with someone between the ages of 25 & 29 is 1.5871 when compared to the reference group (< 25)
# The odds associated with someone between the ages of 30 & 39 is 2.6026 when compared to the reference group (< 25)
# The odds associated with someone between the ages of 40 & 49 is 3.5502 when compared to the reference group (< 25)
# The odds associated with some education is 1.5323 when compared to the reference group (no education)
# The odds associated with does not want more children is 2.2412 when compared to the reference group (wants more children)

# Model Visualization
set_theme(axis.textsize = .8)
plot_model(lr_model, type = "pred", terms = c("age_group", "education", "desire_children"),  colors = "metro") + 
  labs(title = "Logit Model - Predicted Values", color = "Education") +
  xlab("Age Group") +
  ylab("Probability of Contraceptive Use") 

# Forest Plot
input <- setNames(data.frame(matrix(ncol = 3, nrow = 5)), c("group", "beta", "se"))
input[,1] <- c("25 - 29 Age Group", "30 - 29 Age Group", "40 - 49 Age Group", "Some Education", "No More Children")
input[,2] <- c(log(1.5871), log(2.6026), log(3.5502), log(1.5323), log(2.2412))
input[,3] <- c(.2043, .1929, .2482, .1438, .1352)

# Assign values for plotting
labs <- input$group
yi   <- input$beta
sei  <- input$se

# Combine data into summary estimate
res  <- rma(yi = yi, sei = sei, method = "FE")
summary(res)

# Plot combined data
forest(res, transf = exp, refline = 1, xlab = "Odds Ratio (95% CI)", slab = labs, mlab = "Summary Estimate")
mtext(paste("Forest Plot"))

# ROC Curve
roc <- roc(response = data$contraceptive_use, predictor = as.matrix(lr_fitted), data = data, plot = TRUE, col = "#377eb8", lwd = 4)
coords(roc, "best")
print(roc)

# Confusion Matrix
con_matrix <- table(data_test$contraceptive_use, test > .356)
print(con_matrix)
con_matrix_accuracy <- (con_matrix[1,1] + con_matrix[2,2])/(con_matrix[1,1] + con_matrix[1,2] + con_matrix[2,1] + con_matrix[2,2])
print(con_matrix_accuracy)