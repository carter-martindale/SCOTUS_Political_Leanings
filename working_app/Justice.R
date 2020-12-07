## Justice

justice <- read_csv("raw_data/SCDB_2020_01_justiceCentered_Citation.csv", col_names = TRUE, cols(
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
  minVotes = col_double(),
  justice = col_double(),
  justiceName = col_character(),
  vote = col_double(),
  opinion = col_double(),
  direction = col_double(),
  majority = col_double(),
  firstAgreement = col_double(),
  secondAgreement = col_double())) %>% 
  select(voteId, term, chief,
         caseOriginState, issueArea,
         decisionDirection, direction, justice, justiceName, vote,
         decisionDirectionDissent) %>% 
  clean_names()

# This read in my second dataset, Justice, which I do a lot more work with. 
# I cleaned up names, and once again selected the columns I felt were
# important. 

justice$issue_area[which(justice$issue_area == 1)] <- "Criminal Procedure"
justice$issue_area[which(justice$issue_area == 2)] <- "Civil Rights"
justice$issue_area[which(justice$issue_area == 3)] <- "1st Amendment"
justice$issue_area[which(justice$issue_area == 4)] <- "Due Process"
justice$issue_area[which(justice$issue_area == 5)] <- "Privacy"
justice$issue_area[which(justice$issue_area == 6)] <- "Attorney/Government Fees and Compensation"
justice$issue_area[which(justice$issue_area == 7)] <- "Unions"
justice$issue_area[which(justice$issue_area == 8)] <- "Economic Activity"
justice$issue_area[which(justice$issue_area == 9)] <- "Judicial Power"
justice$issue_area[which(justice$issue_area == 10)] <- "Federalism"
justice$issue_area[which(justice$issue_area == 11)] <- "Interstate Relations"
justice$issue_area[which(justice$issue_area == 12)] <- "Federal Taxation"
justice$issue_area[which(justice$issue_area == 13)] <- "Misc"
justice$issue_area[which(justice$issue_area == 14)] <- "Private Laws"

# Similar change to what I did with dataset D, this will allow users to see
# what exact issue area they are selecting, and it makes the interactive
# plots much easier to work with. Originally, I had added on the names
# after keeping the number and was using str_select when it came to filtering
# my data, but it was much easier to just change the variable to entirely
# get rid of the number. 

justice$direction[which(justice$direction == 2)] <- 0

# Once again, this put a liberal vote on the left of any visual plot and 
# a conservative vote on the right, a change that makes the plots more 
# intuitive for anyone who is looking at them for the first time. 

fit <- justice %>% 
  filter(justice_name %in% c("JGRoberts", "CThomas",
                             "SGBreyer", "SAAlito",
                             "SSotomayor"),
         issue_area %in% c("1st Amendment"))

fit_obj <- stan_glm(data = fit,
                    direction ~ justice_name,
                    refresh = 0)

# This code was to create a small table that will go with the
# interactive plot. I made a model of regressing vote direction onto
# each individual justice, but only dealing with the 5 Justices who have
# been on the court for 10 years or more and only pertaining to 1st Amendment
# issues. 

tbl_regression(fit_obj, intercept = TRUE) %>%
  as_gt() %>% 
  tab_header(title = "Regression of Justice Vote Direction", 
             subtitle = "Looking at 1st Amendment Cases") %>%
  tab_source_note(md("Source: The Supreme Court Database"))

# This is the table I will end up displaying. 

modern_justice <- justice %>% 
  filter(term > 1980)

greats <- stan_glm(direction ~ justice_name + term,
                   data = modern_justice,
                   refresh = 0)

# This is looking at the various justices over the last 50 years. This will
# allow me to compare and see who is (relatively) the most conservative and the
# most liberal over the last 50 years). Note that this is currently compared
# to justice Kennedy.

# print(greats, digits = 4)

# Based on the output, it looks like CThomas, AAlito, and NMGorsuch are the
# most conservative Justices. For the liberals, TMarshall, WJBrennan, and 
# JPStevens were the top 3. Ginsburg came in at 5th. 

comparison_justice <- modern_justice %>% 
  filter(justice_name %in% c("CThomas", "SAAlito", "NMGorsuch", "TMarshall",
                             "WJBrennan", "JPStevens", "RBGinsburg",
                             "AMKennedy")) %>% 
  mutate(justice_name = fct_relevel(justice_name,
                             "AMKennedy", "CThomas",
                             "SAAlito", "NMGorsuch",
                             "TMarshall", "WJBrennan",
                             "JPStevens", "RBGinsburg"))

# I used a relevel here to have my table be output with the most
# conservative Justices on top, and then the Liberal Justices after that
# rather than being somewhat randomly distributed like they were at first.


polarized <- stan_glm(direction ~ justice_name,
                      data = comparison_justice,
                      refresh = 0)

# This is just going to give me output with the previously identified
# 8 Justices. I figured it would be easier to run stan_glm again rather 
# than filter the table from a stan with all of the Justices.

tbl_regression(polarized, intercept = TRUE) %>% 
  as_gt() %>%
  tab_header(title = "Linear Regression of Justice Vote Direction",
             subtitle = "A selection of the most conversative and liberal justices") %>%
  tab_source_note(md("Source: Supreme Court Database"))

# This is the final project, the table that I output onto my app.
