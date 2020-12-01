
justice_2 %>% 
  mutate(direction = as.factor(case_when(
    direction == 0 ~ 0,
    direction == 1 ~ 1)))

justice_2 %>% 
  sample_n(5)

set.seed(10)
justice_split <- initial_split(justice_2, prob = 0.20)
justice_train <- training(justice_split)
justice_test  <- testing(justice_split)
justice_folds <- vfold_cv(justice_train, v = 5)


stan_glm(data = justice_train,
         direction ~ chief + issue_area + term,
         refresh = 0,
         family = binomial())

logit <- glm(data = justice_train,
             direction ~ chief + issue_area + term + justice_name,
             family = "binomial")
summary(logit)
confint(logit)

current_justice <- justice_2 %>% 
  filter(justice_name %in% c("JGRoberts", "CThomas",
                             "SGBreyer", "SAAlito",
                             "SSotomayor"))


c_justice_split <- initial_split(current_justice, prob = 0.20)
c_justice_train <- training(c_justice_split)
c_justice_test  <- testing(c_justice_split)
current_logit <- glm(data = c_justice_train,
                     direction ~ issue_area + justice_name,
                     family = "binomial")
print(current_logit)
summary(current_logit)
confint(current_logit)
exp(coef(current_logit))
exp(cbind(OR = coef(current_logit), confint(current_logit)))

# Just to look at the number of directions/general trends
# justice_2 %>% 
#   group_by(chief) %>% 
#   summarize(avg_decision = mean(direction), .groups = "drop") %>% 
#   ggplot(aes(x = chief, y = avg_decision)) + 
#   geom_point()


# Basically just took this directly from chapter 12 in the book.

glm_wfl <- workflow() %>% 
  add_model(logistic_reg() %>%
              set_engine("glm") %>%
              set_mode("classification")) %>% 
  add_recipe(recipe(direction ~ chief + issue_area,
                    data = justice_train) %>% 
               step_dummy(chief)
  )

# Nothing from this point down works on the glm_wfl so come 
# back to this and try to figure it out. 

glm_metrics <- glm_wfl %>% 
  fit_resamples(resamples = justice_folds)

collect_metrics(glm_metrics)

glm_wfl %>% 
  fit(data = justice_train) %>% 
  predict(newdata = justice_train) %>% 
  bind_cols(justice_train %>% select(decision)) %>% 
  ggplot(aes(y = decision, x = `.pred_class`)) +
  geom_jitter(height = 0.2, alpha = 0.01) +
  labs(x = "predicted approval",
       y = "approval")

glm_wfl %>% 
  fit(justice_train) %>% 
  predict(justice_train)

# Trying a cart_wfl to see if that works. But it also doesn't
# seem to work at all. bruh

cart_wfl <- workflow() %>% 
  add_model(decision_tree() %>%
              set_engine("rpart",
                         model = TRUE) %>%
              set_mode("classification")) %>% 
  add_recipe(recipe(direction ~ justice_name + chief + issue_area,
                    data = justice_train) %>% 
               step_dummy(justice_name, chief))

deez <- cart_wfl %>% 
  fit_resamples(resamples = justice_folds) %>% 
  collect_metrics()

cart_metrics

cart_wfl %>% 
  fit(data = justice_train) %>% 
  predict(new_data = justice_train) %>% 
  bind_cols(justice_train %>% select(direction)) %>% 
  ggplot(aes(y = approval, x = `.pred_class`)) +
  geom_jitter(height = 0.2, alpha = 0.01) +
  labs(title = "Predicting Approval",
       subtitle = "Using demographic predictors",
       x = "Predicted Approval",
       y = "Approval"
  )

