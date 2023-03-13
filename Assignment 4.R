
install.packages("readxl")
library(readxl)
estimation <- read_excel("BBBCData.xlsx", sheet = "Estimation Sample")

log_reg = glm(Choice ~ Gender + Amt_purchased + Frequency + 
                Last_Purchase + First_purchase + P_Child + P_Youth + P_Cook + 
                P_DIY + P_Art, 
              data = estimation, family = binomial)

summary(log_reg)$coef

install.packages("mfx")
library(mfx)

mfx <- logitmfx(Choice ~ Gender + Amt_purchased + Frequency + 
                  Last_Purchase+ First_purchase + P_Child + P_Youth + P_Cook + 
                  P_DIY + P_Art, 
                data = estimation)

mfx$mfxest


library(dplyr)

estimation <- estimation %>% mutate(decile = ntile(Last_Purchase, 10))

table(estimation$Last_Purchase)


estimation$R_decile = .bincode(estimation$Last_Purchase, 
                               quantile(estimation$Last_Purchase, probs = seq(0, 1, 0.1), 
                                        right = TRUE, include.lowest = TRUE))

estimation$R_decile = .bincode(estimation$Last_Purchase, 
         quantile(estimation$Last_Purchase, probs = seq(0, 1, 0.1)), 
         right = TRUE, include.lowest = TRUE)

estimation$R_decile = 11 - estimation$R_decile


table(estimation$R_decile)

estimation %>% group_by(decile) %>% summarize(response_rate = mean(Choice))

estimation %>% group_by(R_decile) %>% summarize(response_rate = mean(Choice))

estimation$probabilities = predict(log_reg, type = "response")


validation <- read_excel("BBBCData.xlsx", sheet = "Validation Sample")

validation$probabilities = predict(log_reg, newdata = validation, type = "response")

validation$pred_choice = ifelse(validation$probabilities >= 0.5, TRUE, FALSE)
table(validation$Choice, validation$pred_choice)

estimation$prob_decile = .bincode(estimation$probabilities, 
                               quantile(estimation$probabilities, probs = seq(0, 1, 0.1)), 
                               right = TRUE, include.lowest = TRUE)

validation$prob_decile = .bincode(validation$probabilities, 
                                  quantile(validation$probabilities, probs = seq(0, 1, 0.1)), 
                                  right = TRUE, include.lowest = TRUE)
validation$prob_decile = 11 - validation$prob_decile
table(validation$prob_decile)


tapply(validation$Choice,validation$prob_decile,sum)

tapply(validation$Choice,validation$prob_decile,mean)


validation$R_decile = .bincode(validation$Last_Purchase, 
                           quantile(estimation$Last_Purchase, 
                                    probs = seq(0, 1, 0.1), na.rm = TRUE), 
                           right = TRUE, include.lowest = TRUE)

validation$R_decile = 11 - validation$R_decile

tapply(validation$Choice,validation$R_decile,sum)
table(validation$R_decile)
