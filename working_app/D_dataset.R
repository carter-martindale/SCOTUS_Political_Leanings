## D 

d<- read_csv("raw_data/SCDB_2020_01_caseCentered_LegalProvision.csv",
             col_names = TRUE, cols(
               caseId = col_character(),
               docketId = col_character(),
               caseIssuesId = col_character(),
               voteId = col_character(),
               dateDecision = col_character(),
               decisionType = col_double(),
               usCite = col_character(),
               sctCite = col_character(),
               ledCite = col_character(),
               lexisCite = col_character(),
               term = col_double(),
               naturalCourt = col_double(),
               chief = col_character(),
               docket = col_character(),
               caseName = col_character(),
               dateArgument = col_character(),
               dateRearg = col_character(),
               petitioner = col_double(),
               petitionerState = col_double(),
               respondent = col_double(),
               respondentState = col_double(),
               jurisdiction = col_double(),
               adminAction = col_double(),
               adminActionState = col_double(),
               threeJudgeFdc = col_double(),
               caseOrigin = col_double(),
               caseOriginState = col_double(),
               caseSource = col_double(),
               caseSourceState = col_double(),
               lcDisagreement = col_double(),
               certReason = col_double(),
               lcDisposition = col_double(),
               lcDispositionDirection = col_double(),
               declarationUncon = col_double(),
               caseDisposition = col_double(),
               caseDispositionUnusual = col_double(),
               partyWinning = col_double(),
               precedentAlteration = col_double(),
               voteUnclear = col_double(),
               issue = col_double(),
               issueArea = col_double(),
               decisionDirection = col_double(),
               decisionDirectionDissent = col_double(),
               authorityDecision1 = col_double(),
               authorityDecision2 = col_double(),
               lawType = col_double(),
               lawSupp = col_double(),
               lawMinor = col_character(),
               majOpinWriter = col_double(),
               majOpinAssigner = col_double(),
               splitVote = col_double(),
               majVotes = col_double(),
               minVotes = col_double())) %>% 
  clean_names() %>% 
  mutate(date_decision_new = mdy(date_decision),
         date_argument_new = mdy(date_argument),
         date_rearg_new = mdy(date_rearg)) %>% 
  select(us_cite, term, chief, case_origin_state, issue,
         issue_area, jurisdiction, decision_direction) %>% 
  drop_na(decision_direction)

# This is where I read in my first dataset. I cleaned up the names, reformatted
# the dates, selected the variables I felt were pertinent, and then dropped
# all na's in decision_direction since I knew I was going to be working
# with that column primarily. 

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

# Changing up the variables in issue_area to read what they were, and not just
# numbers seemed an intuitive change that would help out my user
# interface and plot appearance later. 

d$decision_direction[which(d$decision_direction == 2)] <- 0

# Finally, one of my friends pointed out early on that it seemed wrong that
# a liberal vote was coined as 2 and thus farther right on the graph. So 
# changing a 2 (liberal vote) to 0 put Liberal on the left side and Conservative
# on the right, which is much more intuitive for people.

d %>% 
  filter(issue_area == "Privacy") %>% 
  mutate(chief = fct_relevel(chief,
                             "Vinson", "Warren",
                             "Burger", "Rehnquist",
                             "Roberts")) %>% 
  ggplot(aes(x = term, fill = chief)) +
  geom_bar(position = "dodge") +
  scale_fill_brewer(name = "Chief",
                    breaks = c("Vinson", "Warren",
                               "Burger", "Rehnquist",
                               "Roberts"),
                    palette = "Accent") +
  labs(title = "Historical Occurence of Privacy Cases",
       subtitle = "Privacy cases consistently came before the court beginning in 1970",
       x = "Term", y = "Number of Cases")

# This code makes a plot that I used to display on my Interesting 
# findings page. It shows how many Privacy cases came before the court in
# each term. 
