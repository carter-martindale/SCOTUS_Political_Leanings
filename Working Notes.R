
test$lib <- 0
test$lib[which(test$direction == 1)] <- 1
# 1 is a conservative vote, 0 liberal
binom <- stan_glm(lib ~ justice_name + chief + term +I(term^2),
         data = test,
         refresh = 0,
         family = binomial())
print(binom, digits = 3)

tree <- rpart(lib ~ justice_name + chief + term,
              data = test,
              cp = .01)

rpart.plot(tree, type = 2)


plot(test$term, test$lib)

binom %>% 
  as_tibble() %>% 
  rename(Warren = justice_nameEWarren, Vinson = justice_nameFMVinson,
         Black = justice_nameHLBlack, Douglas = justice_nameWODouglas) %>%
  pivot_longer(cols = Warren:Douglas,
               names_to = "justice",
               values_to = "lib_direction") %>% 
  ggplot(aes(x = lib_direction)) +
  geom_histogram(aes(y = after_stat(count/sum(count)),
                     fill = justice),
                 alpha = 0.5, 
                 bins = 100, 
                 position = "identity") +
  labs(title = "Posterior Probability Distribution",
       subtitle = "Average lib Direction for Four Justices on Issues of the 1st Amendment",
       x = "Lib Direction",
       y = "Probability") +
  scale_y_continuous(labels = scales::percent_format()) +
  theme_classic()

d$issue_area[which(d$issue_area == 1)] <- "Criminal Procedure"
d$issue_area[which(d$issue_area == 2)] <- "Civil Rights"
d$issue_area[which(d$issue_area == 3)] <- "1st Amendment"
d$issue_area[which(d$issue_area == 4)] <- "Due Process"
d$issue_area[which(d$issue_area == 5)] <- "Privacy"
d$issue_area[which(d$issue_area == 6)] <- "Attorney/Government Fees and Compensation"
d$issue_area[which(d$issue_area == 7)] <- "Unions"
d$issue_area[which(d$issue_area == 8)] <- "Economic Activity"
d$issue_area[which(d$issue_area == 9)] <- "Judicial Power"
d$issue_area[which(d$issue_area == 10)] <- "Federalism"
d$issue_area[which(d$issue_area == 11)] <- "Interstate Relations"
d$issue_area[which(d$issue_area == 12)] <- "Federal Taxation"
d$issue_area[which(d$issue_area == 13)] <- "Misc"
d$issue_area[which(d$issue_area == 14)] <- "Private Laws"
d$issue_area

justice_2 <- justice %>%
  select(term, chief,
         issue, issue_area,
         decision_direction, justice, justice_name, vote,
         decision_direction_dissent, maj_opin_writer, direction)

justice_2$issue_area[which(justice_2$issue_area == 1)] <- "Criminal Procedure"
justice_2$issue_area[which(justice_2$issue_area == 2)] <- "Civil Rights"
justice_2$issue_area[which(justice_2$issue_area == 3)] <- "1st Amendment"
justice_2$issue_area[which(justice_2$issue_area == 4)] <- "Due Process"
justice_2$issue_area[which(justice_2$issue_area == 5)] <- "Privacy"
justice_2$issue_area[which(justice_2$issue_area == 6)] <- "Attorney/Government Fees and Compensation"
justice_2$issue_area[which(justice_2$issue_area == 7)] <- "Unions"
justice_2$issue_area[which(justice_2$issue_area == 8)] <- "Economic Activity"
justice_2$issue_area[which(justice_2$issue_area == 9)] <- "Judicial Power"
justice_2$issue_area[which(justice_2$issue_area == 10)] <- "Federalism"
justice_2$issue_area[which(justice_2$issue_area == 11)] <- "Interstate Relations"
justice_2$issue_area[which(justice_2$issue_area == 12)] <- "Federal Taxation"
justice_2$issue_area[which(justice_2$issue_area == 13)] <- "Misc"
justice_2$issue_area[which(justice_2$issue_area == 14)] <- "Private Laws"

test <- justice_2 %>% 
  filter(justice_name %in% c("HLBlack", "WODouglas", "FMVinson",
                             "EWarren"),
         issue_area == "1st Amendment") %>% 
  filter(direction %in% c(1, 2))
  
         
fit <- stan_glm(data = test,
         direction ~ justice_name + decision_direction - 1,
         refresh = 0)
print(fit, digits = 3)

fit_obj <- fit %>% 
  as_tibble()

fit %>% 
  as_tibble() %>% 
  select(-sigma) %>% 
  rename(Warren = justice_nameEWarren, Vinson = justice_nameFMVinson,
         Black = justice_nameHLBlack, Douglas = justice_nameWODouglas) %>%
  pivot_longer(cols = Warren:Douglas,
               names_to = "justice",
               values_to = "vote_direction") %>% 
  ggplot(aes(x = vote_direction)) +
  geom_histogram(aes(y = after_stat(count/sum(count)),
                     fill = justice),
                 alpha = 0.5, 
                 bins = 100, 
                 position = "identity") +
  labs(title = "Posterior Probability Distribution",
       subtitle = "Average Vote Direction for Four Justices on Issues of the 1st Amendment",
       x = "Vote Direction",
       y = "Probability") +
  scale_y_continuous(labels = scales::percent_format()) +
  theme_classic()


fit <- justice_2 %>% 
  filter(justice_name %in% c("JGRoberts", "CThomas",
                             "SGBreyer", "SAAlito",
                             "SSotomayor"),
         issue_area == "Privacy")
fit_obj <- stan_glm(data = fit,
                    direction ~ justice_name - 1,
                    refresh = 0)
checking <- fit_obj %>% 
  as_tibble()


fit <- justice %>% 
  filter(justice_name %in% c("JGRoberts", "CThomas",
                             "SGBreyer", "SAAlito",
                             "SSotomayor")) %>% 
  drop_na(direction)
fit_obj <- stan_glm(data = fit,
                    direction ~ justice_name - 1,
                    refresh = 0)

rmse_1 <- tibble(reality = fit$direction,
                 guess = predict(fit_obj)) %>% 
  mutate(sq_dif = (guess - reality)^2) %>% 
  summarise(rmse = sqrt(mean(sq_dif)))

justice_privacy <- justice %>% 
  filter(justice_name %in% c("JGRoberts", "CThomas",
                                    "SGBreyer", "SAAlito",
                                    "SSotomayor"),
         issue_area == 3) %>% 
  select(justice_name, issue_area, direction) %>% 
  drop_na(direction)

justice_filtered <- justice %>% 
  filter(justice_name %in% c("JGRoberts", "CThomas",
                             "SGBreyer", "SAAlito",
                             "SSotomayor")) %>% 
  drop_na(direction) %>% 
  select(justice_name, issue_area, direction)

set.seed(10)
p_fit_1 <- stan_glm(data = justice_privacy,
         direction ~ justice_name - 1,
         refresh = 0)

p_fit_2 <- stan_glm(data = justice_filtered,
                    direction ~ justice_name + issue_area - 1,
                    refresh = 0)
print(p_fit_1, digits = 4)
print(p_fit_2, digits = 4)
