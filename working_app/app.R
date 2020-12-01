
library(shiny)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(readr)
library(lubridate)
library(janitor)
library(tibble)
library(readxl)
library(tidymodels)
library(forcats)
library(rstanarm)
library(gtsummary)
library(broom.mixed)

d<- read_csv("SCDB_2020_01_caseCentered_LegalProvision.csv",
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

justice <- read_csv("SCDB_2020_01_justiceCentered_Citation.csv", col_names = TRUE, cols(
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

justice$direction[which(justice$direction == 2)] <- 0

d$decision_direction[which(d$decision_direction == 2)] <- 0

d$jurisdiction[which(d$jurisdiction == 1)] <- "cert"
d$jurisdiction[which(d$jurisdiction == 2)] <- "appeal"
d$jurisdiction[which(d$jurisdiction == 3)] <- "bail"
d$jurisdiction[which(d$jurisdiction == 4)] <- "certification"
d$jurisdiction[which(d$jurisdiction == 5)] <- "docketing fee"
d$jurisdiction[which(d$jurisdiction == 6)] <- "rehearsing or restored to calendar for reargument"
d$jurisdiction[which(d$jurisdiction == 7)] <- "injunction"
d$jurisdiction[which(d$jurisdiction == 8)] <- "mandamus"
d$jurisdiction[which(d$jurisdiction == 9)] <- "original"
d$jurisdiction[which(d$jurisdiction == 10)] <- "prohibition"
d$jurisdiction[which(d$jurisdiction == 12)] <- "stay"
d$jurisdiction[which(d$jurisdiction == 13)] <- "writ of error"
d$jurisdiction[which(d$jurisdiction == 14)] <- "writ of habeus corpus"
d$jurisdiction[which(d$jurisdiction == 15)] <- "unspecified"

read_petition_total <- function(x, y) {
    z <- read_excel(x, skip = 1) %>% 
        drop_na() %>% 
        clean_names() %>% 
        rename("granted" = "terminated",
               "denied" = "x5",
               "dismissed" = "x6",
               "nature_of_proceeding" = "circuit_and_nature_of_proceeding") %>% 
        mutate(year = y)
    w <- z[c(7:10, 12:15, 17:20, 22:25, 27:30, 32:35, 37:40,
             42:45, 47:50, 52:55, 57:60, 62:65), c(1, 3:6, 8)]
    w %>%
        mutate(district = c(rep("DC", 4), rep("1st", 4),
                            rep("2nd", 4), rep("3rd", 4),
                            rep("4th", 4), rep("5th", 4),
                            rep("6th", 4),
                            rep("7th", 4), rep("8th", 4),
                            rep("9th", 4), rep("10th", 4),
                            rep("11th", 4)))
}

read_p <- function(x, y) {
    z <- read_excel(x, skip = 9
    ) %>%
        drop_na() %>% 
        clean_names() %>% 
        rename("nature_of_proceeding" = "of_proceeding") %>% 
        mutate(year = y)
    w <- z[c(7:10, 12:15, 17:20, 22:25, 28:31, 33:36, 38:41,
             43:46, 48:51, 54:57, 59:62, 64:67), c(1, 3:6, 8)]
    w %>%
        mutate(district = c(rep("DC", 4), rep("1st", 4),
                            rep("2nd", 4), rep("3rd", 4),
                            rep("4th", 4), rep("5th", 4),
                            rep("6th", 4),
                            rep("7th", 4), rep("8th", 4),
                            rep("9th", 4), rep("10th", 4),
                            rep("11th", 4)))
}

read_annoyed <- function(x, y) {
    z <- read_excel(x, skip = 11) %>% 
        drop_na() %>% 
        clean_names() %>% 
        rename("nature_of_proceeding" = "of_proceeding") %>% 
        mutate(year = y)
    w <- z[c(7:10, 12:15, 17:20, 22:25, 28:31, 33:36, 38:41,
             43:46, 48:51, 54:57, 59:62, 64:67), c(1, 3:6, 8)]
    w %>%
        mutate(district = c(rep("DC", 4), rep("1st", 4),
                            rep("2nd", 4), rep("3rd", 4),
                            rep("4th", 4), rep("5th", 4),
                            rep("6th", 4),
                            rep("7th", 4), rep("8th", 4),
                            rep("9th", 4), rep("10th", 4),
                            rep("11th", 4)))
}

read_2008 <- function(x, y) {
    z <- read_excel(x, skip = 9) %>%
        drop_na() %>% 
        clean_names() %>% 
        rename("nature_of_proceeding" = "of_proceeding") %>% 
        mutate(year = y)
    w <- z[c(7:10, 12:15, 17:20, 22:25, 28:31, 33:36, 38:41,
             43:46, 48:51, 53:56, 58:61, 63:66), c(1, 3:6, 8)]
    w %>%
        mutate(district = c(rep("DC", 4), rep("1st", 4),
                            rep("2nd", 4), rep("3rd", 4),
                            rep("4th", 4), rep("5th", 4),
                            rep("6th", 4),
                            rep("7th", 4), rep("8th", 4),
                            rep("9th", 4), rep("10th", 4),
                            rep("11th", 4)))
}

p_2019 <- read_petition_total("jb_b2_0930.2019.xlsx", 2019)
p_2018 <- read_petition_total("jb_b2_0930.2018.xlsx", 2018)
p_2017 <- read_petition_total("jb_b2_0930.2017.xlsx", 2017)
p_2016 <- read_petition_total("jb_b2_0930.2016.xlsx", 2016)
p_2015 <- read_p("B02Sep15.xlsx", 2015)
p_2014 <- read_p("B02Sep14.xlsx", 2014)
p_2013 <- read_p("B02Sep13.xlsx", 2013)
p_2012 <- read_p("B02Sep12.xlsx", 2012)
p_2011 <- read_p("B02Sep11.xlsx", 2011)
p_2010 <- read_annoyed("B02Sep10.xlsx", 2010)
p_2009 <- read_p("B02Sep09.xlsx", 2009)
p_2008 <- read_2008("B02Sep08.xlsx", 2008)
p_2007 <- read_p("B02Sep07.xlsx", 2007)
p_2006 <- read_annoyed("b2_2.xlsx", 2006)
p_2005 <- read_p("b2_1.xlsx", 2005)

p_2016$nature_of_proceeding[which(p_2016$nature_of_proceeding == "U.S. Private")] <- 
    "Private Civil"

roberts_circuit <- rbind(p_2019, p_2018, p_2017, p_2016, p_2015, p_2014, p_2013, p_2012, p_2011, p_2010, p_2009, p_2008, p_2007, p_2006, p_2005)

roberts_circuit$granted[which(roberts_circuit$granted ==
                                  "-")] <- 0
roberts_circuit$denied[which(roberts_circuit$denied ==
                                 "-")] <- 0
roberts_circuit$dismissed[which(roberts_circuit$dismissed ==
                                    "-")] <- 0
roberts_circuit$filed[which(roberts_circuit$filed ==
                                "-")] <- 0

roberts_circuit <- roberts_circuit %>%   
    mutate(percent_granted =
               (as.numeric(granted)/as.numeric(filed)) *100,
           percent_denied =
               (as.numeric(denied)/as.numeric(filed)) *100)

roberts_circuit$percent_granted[which(roberts_circuit$percent_granted == "NaN")] <- 0
roberts_circuit$percent_denied[which(roberts_circuit$percent_denied == "Inf")] <- 0

fit <- stan_glm(percent_granted ~ district,
                data = roberts_circuit,
                refresh = 0)
new_obs <- tibble(district = c("1st", "2nd", "3rd", "4th", "5th",
                               "6th", "7th", "8th", "9th", "10th",
                               "11th", "DC"))
ep <- posterior_epred(fit,
                      newdata = new_obs) %>% 
    as_tibble() %>% 
    mutate_all(as.numeric) %>% 
    rename(DC = `12`) %>% 
    pivot_longer(cols = 1:DC,
                 names_to = "Circuit",
                 values_to = "Prediction")

# Define UI for application that draws a histogram
ui <- navbarPage(
    "Supreme Court Over the Years",
    tabPanel("A Historical Overview of SCOTUS Cases",
             h3("Case Salience"),
             p("Acting as background knowledge for my models, this page focuses
            on the historicy of voting decisions within the court. 
            This plot visually displays the number of cases under each Chief
              Justice by issue area. See if you can notice any patterns in the
              type of cases brought before each court."),
             fluidPage(
                 selectInput("a", "Issue Area",
                             choices = c("Criminal Procedure", "Civil Rights",
                                         "1st Amendment", "Due Process",
                                         "Privacy", "Attorney/Government Fees and Compensation",
                                         "Unions", "Economic Activity",
                                         "Judicial Power", "Federalism", "Interstate Relations",
                                         "Federal Taxation", "Misc", "Private Laws"
                             )),
                 plotOutput("plot4"),
                 h3("Which Way Did a Court Lean?"),
                 p("Many claim that certain courts have been inherently more
            Conservative or Liberal in their voting bias.
            This plot gives you the chance to choose a Chief Justice,
              and see whether the majority of their decisions were liberal,
              conservative, or somewhere in between")),
             fluidPage(
                 selectInput("c", "Chief", choices = c("Vinson", "Warren",
                                                       "Burger", "Rehnquist",
                                                       "Roberts")),
                 selectInput("b", "Issue Area",
                             choices = c("Criminal Procedure", "Civil Rights",
                                         "1st Amendment", "Due Process",
                                         "Privacy", "Attorney/Government Fees and Compensation",
                                         "Unions", "Economic Activity",
                                         "Judicial Power", "Federalism", "Interstate Relations",
                                         "Federal Taxation", "Misc", "Private Laws"
                             )),
                 plotOutput("plot5"))),
    
    tabPanel("Model- Voting Centered",
            h3("Can We Predict How a Justice will Vote?"),
            p("The central question of this page is whether or not we can
            predict how a given justice will vote on a certain issue. I was
            able to gather information on every decision made by each Supreme
            Court Justice since they have been on the court, and created a model
            predicting how they will vote on a given issue. Voting `direction`
            has been divided into Conservative or Liberal, and for the sake
            of convenience I have preselected justices who are currently alive
            and who have been on the court for at least 10 years."),
            p("Choose an issue area that you would like to look at more closely."),
            fluidPage(
                selectInput("I", "Issue Area",
                            choices = c("Criminal Procedure", "Civil Rights",
                                        "1st Amendment", "Due Process",
                                        "Privacy",
                                        "Attorney/Government Fees and Compensation",
                                        "Unions", "Economic Activity",
                                       "Judicial Power", "Federalism",
                                       "Interstate Relations",
                                       "Federal Taxation", "Misc", "Private Laws"
                )),
                plotOutput("plot_regress")),
            p("I'll probably include some more analysis here, or maybe another
              plot with more to choose from than just the Justice.")
            ),
    
    tabPanel("Model- Petition Centered",
            h3("Can We Predict What Cases Will be Chosen?"),
            p("The central question in this model is the following: Can we 
              predict what type of cases the Supreme Court will choose? We will
              be drawing upon information from all of the petitions for
              writ of Certiori (essentially a plea for the Supreme Court
              to hear a case) that have been presented while Justice Roberts
              has been Chief Justice."),
            p("First let's look at some tables. For my first model, I analyzed
              the percentage of petitions that were successful based on the
              nature of the case, which circuit court it came from, and what
              year the petition was submitted. Put simply, a higher Beta value
              means a higher predicted success rate for a given case that
              meets the given condition."),
            tags$img(height = 500, 
                     width = 400, 
                     src = "table1.png",
                     align = "center"),
            p("Based on the MAD_SD of my model, each of the predictions for the
            nature_of_proceeding characteristics disprove the null hypothesis,
            so I am considering them significant for our purposes. This means
            that while there is a negative directionality for each of the 
            proceedings, both U.S. and Private Civil cases are less likely to 
            have their petition approved than a criminal case. Unfortunately, I
            didn't have enough data to draw a meaningful conclusion about
            adminsistrative affair cases."),
            p("Other values that can be considered significant are the coefficient
            for year, and those from the 3rd, 7th, 9th, 11th, and DC circuits. 
            ased on the MAD_SD, all of the nature. It appears that as time has
            gone on, the Roberts court has been less likely to approve a
            petition. It also seems that DC and the 7th Circuit Court are
            predicted to have a higher chance of being chosen than those in the 
            3rd, 9th, and 11th Circuit by 3 - 5 percent."),
            plotOutput("plot_circuit"),
            p("This plot shows the predicted probability for the likelihood
              that a case will be granted its petition based on the origin
              location. As we saw in the first table, DC is predicted to
              have the highest percentage of petitions granted."),
            p("The following table is a continuation of the idea in table 1, but
              this time we are looking to see if there is any significant 
              interaction between the type of case and the origin location."),
            tags$img(height = 400, 
                     width = 400, 
                     src = "table2.png",
                     align = "center"),
            tags$img(height = 400,
                     width = 400,
                     src = "table3.png",
                     align = "center"),
            p("Using the MAD_SD, proceedings from the 11th Circuit,
            U.S. Civil cases from the 2nd, 3rd, 8th, and 9th Circuit,
            Private civil cases from the 5th, and Criminal cases from the
            3rd and 8th are all reliable predictions. Similarly, any prediction
            from DC is reliable."),
            p("While the overall trend of DC having the highest predicted
              successful petitions, when looking at the interaction of case
              nature AND location we see that...")
            ),
    
    tabPanel("Interesting Findings",
             h3("The Liberal Warren Court"),
             p("I found this graph very intersesting.
                The Warren Court by far has been the most liberal court in the last
                70 years, but both the Burger and Rehnquist courts seemed to
                step back from the liberal rulings under Chief Justice Warren."),
             fluidPage(
                 plotOutput("plot3")
             )),
    
    tabPanel("Discussion",
             titlePanel("What I Don Did"),
             p("I drew primarily on three sources of data for my project.
             I used two datasets from the Supreme Court Database which gave 
             me access to every Supreme Court Decision since the 1950's. Cases
             were already divided into preset issue areas, and decisions by
             the overall court were classified as either liberal or conservative. 
             The second dataset from the Supreme Court Database focused on the 
             decisions of individual justices, which allowed me to make a model
             predicting the decisions of current Justices."),
             p("The last source of data I used was compiled from uscourts.gov and
               it contained information about how many cases were petitioned to the 
               Supreme Court in a given year. This data was already divided based
               on which court the case came from. This data was the basis of my
               second model, allowing me to predict the percentage of cases that
               would be granted a writ of Certiori based on the nature of the 
               case and the origin of that case. Unfortunately, those were 
               the only two variables readily available. In the future, a more
               in depth analysis of other factors (who such cases were 
               represented by, how many briefs were submitted in support of it,
               media coverage of a case, etc) would be highly beneficial
               to the accuracy of any predictive model.")),
    
    tabPanel("About", 
             titlePanel("About"),
             h3("Project Background and Motivations"),
             p("Long story short I became a Supreme Court case law
               nerd my senior year of high school. This project 
               is essentially trying to draw meaningful conclusions
               about the history of the Supreme Court, and specifically
               how the court has ruled historically on various hot button
               issues, and how they may rule on those issues today."),
             p("Check out my repo",
               a("here", 
                 href = "https://github.com/carter-martindale/milestone_4"),
               "to see the rest of my work."),
             h3("About Me"),
             p("My name is Carter Martindale and I'm pursuing an AB in 
             Government. I'm originally from Hyde Park, Utah, and am a 
             current sophomore at Harvard studying remotely from Provo, Utah. 
             Outside of class I'm involved with Harvard Model Congress, The
               Small Claims Advisory Service, and my church group. 
               I'm also a student coordinator for",
               a("HFAI,", 
                 href = "https://college.harvard.edu/carter-martindale"),
               "so check out that page to learn more about us.
               You can reach me at carter_martindale@college.harvard.edu.")
             ))

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    
    output$plot2 <- renderPlot({
       d %>% 
            drop_na(input$z) %>% 
        ggplot(aes(.data[[input$z]])) +
       geom_bar() + 
            labs(y = "Count",
                 title = "Summmary of Various Topics in the Supreme Court since 1954",
                 x = case_when(
                     input$z == "case_origin_state" ~ "Case Origin State",
                     input$z == "jurisdiction" ~ "Jurisdiction",
                     input$z == "issue_area" ~ "Issue Area",
                     input$z == "decision_direction" ~ "Decision Direction")) +
            theme_classic() +
            theme(axis.text.x = element_text(angle = 45,
                                             size = 8,
                                             color = "salmon"))
    })
    
    output$plot3 <- renderPlot({
        d %>%
            filter(decision_direction %in% c(0, 1)) %>%
            ggplot(aes(x = term, fill = chief)) +
            geom_bar(position = "dodge") +
            facet_wrap(~decision_direction,
                       labeller = labeller(decision_direction = c(
                           "0" = "Liberal", "1" = "Conservative"))) +
            scale_fill_discrete(name = "Chief",
                                breaks = c("Vinson", "Warren",
                                           "Burger", "Rehnquist",
                                           "Roberts")) +
            labs(title = "Direction of SCOTUS Rulings",
                 x = "Term",
                 y = "Number of Rulings")
    })

    output$plot4 <- renderPlot({
        d %>% 
            filter(issue_area == input$a) %>% 
                       # str_sub(input$a, 1, 1)) %>% 
            mutate(chief = fct_relevel(chief,
                                         "Vinson", "Warren",
                                         "Burger", "Rehnquist",
                                         "Roberts")) %>% 
        ggplot(aes(x = issue_area, fill = chief)) +
            geom_bar(position = "dodge") +
            scale_fill_discrete(name = "Chief",
                                breaks = c("Vinson", "Warren",
                                           "Burger", "Rehnquist",
                                           "Roberts")) +
            labs(title = "Salience of Cases by Issue Area",
                 x = "Issue Area",
                 y = "Number of Cases") +
            scale_fill_brewer(palette = "Accent")
    })
    
    output$plot5 <- renderPlot({
        d %>% 
            filter(issue_area == input$b,
                   chief == input$c) %>%
            ggplot(aes(x = issue_area)) +
            geom_bar(fill = "deepskyblue4",
                     aes(y = (..count..)/sum(..count..))) +
            scale_y_continuous(labels = scales::percent) +
            facet_wrap(~decision_direction,
                     labeller = labeller(decision_direction = c(
                "0" = "Liberal", "1" = "Conservative"))) +
            labs(title = "Decision Direction by Issue",
                 x = "Issue Area",
                 y = "Number of Cases")
    })
    
    output$plot_regress <- renderPlot ({
        fit <- justice %>% 
            filter(justice_name %in% c("JGRoberts", "CThomas",
                                       "SGBreyer", "SAAlito",
                                       "SSotomayor"),
                   issue_area == input$I)
        fit_obj <- stan_glm(data = fit,
                 direction ~ justice_name - 1,
                 refresh = 0)
        new_obj <- tibble(justice_name = c("JGRoberts", "CThomas",
                                           "SGBreyer", "SAAlito",
                                           "SSotomayor"))
        ep <- posterior_epred(fit_obj, newdata = new_obj) %>% 
            as_tibble() %>% 
            mutate_all(as.numeric) %>% 
            rename(Roberts = `1`,
                   Thomas = `2`,
                   Breyer = `3`,
                   Alito = `4`,
                   Sotomayor = `5`) %>% 
            pivot_longer(cols = Roberts:Sotomayor,
                         names_to = "Justice",
                         values_to = "Vote")

        ep %>% 
            ggplot(aes(x = Vote)) +
            geom_histogram(aes(y = after_stat(count/sum(count)),
                               fill = Justice),
                           alpha = 0.5, 
                           bins = 100, 
                           position = "identity",
                           color = "white") +
            geom_vline(xintercept = 0.5, lty = 2,
                       color = "red") +
            labs(title = "Posterior Probability Distribution",
                 subtitle = (paste("Predicted Vote Direction on",input$I)),
                 x = "Vote Direction",
                 y = "Probability",
                 caption = "Data falling between 0 and 0.5 indicate a predicted
                 Liberal vote from a given justice. 
                 Data falling between 0.5 and 1 indicate a predicted
                 Conservative vote from a justice.") +
            scale_y_continuous(labels = scales::percent_format()) +
            theme_classic() +
            scale_fill_brewer(palette = "Set1") +
            xlim(0, 1)
    })
    
    output$plot_circuit <- renderPlot({
        ep %>%
            ggplot(aes(Prediction)) +
            geom_histogram(aes(y = after_stat(count/sum(count))),
                           bins = 100, 
                           position = "identity",
                           color = "deepskyblue4") +
            facet_wrap(~Circuit) +
            labs(x = "Percentage of Successful Petitions",
                 y = "Probability",
                 title = "Percentage of Successful Petitions by Circuit Origin",
                 subtitle = "DC seems to be the most reliable circuit for a successful petition") +
            theme_classic()
    })

}
# Run the application 
shinyApp(ui = ui, server = server)
