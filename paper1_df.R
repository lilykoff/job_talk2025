test = read_csv(here::here("docs", "data", "new_testing_data2.csv"))
train = read_csv(here::here("docs", "data", "new_training_data2.csv"))
# gridnum <- read.csv("/Users/yanzhang/Desktop/WIT/Regression/gridnum2.csv")

library(tidyverse)
library(dplyr)
library(caret)

class <- unique(train$subject_id)
output_hz <- data.frame()
output_preds <- matrix(nrow = 5760, ncol = length(class))

one_percent <- list()

for(i in 1:length(class)){
  pt <- train %>% filter(subject_id==i) %>% pivot_longer(cols = -subject_id) %>%
    group_by(name) %>% summarize(subn=sum(value)) %>% mutate(int=substring(name, 1, 2)) %>%
    group_by(int) %>% mutate(n=sum(subn)) %>% ungroup() %>%
    mutate(pt = subn/n) %>% filter(pt >= .001) %>%
    select(name) %>% as.vector()

  dt <- train %>% select(c(subject_id, pt$name))
  dt_test <- test %>% select(colnames(dt))
  dt$class <- ifelse(dt$subject_id==class[i], 1, 0)
  dt_test$class <- ifelse(dt_test$subject_id==class[i], 1, 0)

  logits <- glm(class ~., data = dt[,-1], family = binomial(link ="logit"))
  pred <- predict(logits, dt_test[, -1], type = "response")

  accuracy <- (confusionMatrix(data=factor(as.numeric(pred>0.5)), reference=factor(dt_test$class)))$table
  acc <- sum(diag(accuracy))/sum(accuracy)
  output_hz <- rbind(output_hz, data.frame(class = class[i], Accuracy = acc))
  output_preds[,i] <- pred
}

output_preds <- data.frame(output_preds)
output_preds$id <- test$subject_id
colnames(output_preds) <- c(seq(1,32,1), "subject_id")
pps <- output_preds %>% group_by(subject_id) %>% summarize(across(1:32, mean))
preds <- t(as.matrix(pps[,-1]))

# figure 3: one vs. rest logistic regression plot
par(mar=c(5,7,1,1))
plot(-1000,-1000, xlim=c(1,32), xaxt = "n", ylim=c(0,1), xlab="Subject", ylab=expression(hat(P)(y[i]==k)),
     main="Logistic Regression", cex.axis=0.77, cex.lab=1, cex.main=1, las=1, bty="L")
axis(1, at=seq(1:32), las=1, cex.axis=0.77)
for(i in 1:32){
  col_i <- rep(1,32)
  col_i[i] <- 3
  points(jitter(rep(i, 32), 0.1), preds[i,], col=col_i, pch=16, cex=0.8)
}

## create PRED df
pred_df =
  preds %>%
  as_tibble() %>%
  janitor::clean_names() %>%
  mutate(true_subject = seq(1:ncol(preds))) %>%
  pivot_longer(cols = -true_subject,
               names_to = "pred_subject",
               values_to = "prob",
               names_transform = ~as.numeric(sub(".*v", "", .x)))
write_rds(pred_df, here::here("docs", "data", "paper1_preds.rds"))
