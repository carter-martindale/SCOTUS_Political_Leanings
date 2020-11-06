d$lib <- 0
d$lib[which(d$decision_direction == 1)] <- 1 
stan_glm(lib ~ chief + term,
         data = d,
         refresh = 0,
         family = binomial())
d$test <- "ya mum"
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
  select(voteId, term, chief,
         caseOriginState, issue, issueArea,
         decisionDirection, justice, justiceName, vote,
         decisionDirectionDissent, majOpinWriter, direction) %>% 
  clean_names()

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