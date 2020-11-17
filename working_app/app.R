
library(shiny)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(readr)
library(lubridate)
library(janitor)
library(tibble)
library(forcats)
library(rstanarm)

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

# Define UI for application that draws a histogram
ui <- navbarPage(
    "Supreme Court Over the Years",
    tabPanel("Home", 
             titlePanel("Welcome to my Page!"),
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
               You can reach me at carter_martindale@college.harvard.edu.")),
    tabPanel("Model- The Justices",
            h3("Let's talk about the Justices..."),
            p("So I need to refine my code for this section, but essentially
              we're going to try to predict which way a justice will vote
              on a certain issue of your choice. I've preselected justices 
              who are currently alive and who have been on the court for at
              least 10 years to ensure that we have a good amount of data to 
              draw on."),
            p("This may take a few seconds to create the plot once you choose an
              issue area."),
            fluidPage(
                selectInput("I", "Issue Area",
                            choices = c("Criminal Procedure", "Civil Rights",
                                        "1st Amendment", "Due Process",
                                        "Privacy", "Attorney/Government Fees and Compensation",
                                        "Unions", "Economic Activity",
                                       "Judicial Power", "Federalism", "Interstate Relations",
                                       "Federal Taxation", "Misc", "Private Laws"
                )),
                plotOutput("plot_regress"))),
     # tabPanel("SCOTUS Over the Years",
     #        h3("Most Commmon..."),
     #        p("This plot can be used to find out the most common x of 
     #        SCOTUS- the most common state for a case to come from, the most
     #        common issue area for a case to fall under, etc."),
     #        p("Honestly I am probably going to delete this section since it 
     #          isn't really doing much and will just take a lot of time to 
     #          keep cleaning it up."),
     #        fluidPage(
     #             selectInput("z", "X Variable", choices = c("case_origin_state",
     #                                                        "jurisdiction",
     #                                                        "issue_area",
     #                                                        "decision_direction")),
     #             plotOutput("plot2"))),
    
    tabPanel("The Cases",
            h3("Case Salience"),
            p("This plot shows trends in the number of cases under each chief
              by issue area. You can notice some historical trends in the type
              of cases brought before each court."),
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
            p("This plot gives you the chance to choose a Chief Justice,
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
               on which court the case came from.")),
    
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
               "to see the rest of my work.")))

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
                 y = "Number of Cases")
    })
    
    output$plot5 <- renderPlot({
        d %>% 
            filter(issue_area == input$b,
                   chief == input$c) %>%
            ggplot(aes(x = issue_area)) +
            geom_bar(fill = "green",
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
        fit_obj %>% 
            as_tibble() %>% 
            rename(Roberts = justice_nameJGRoberts, Thomas = justice_nameCThomas,
                   Breyer = justice_nameSGBreyer, Alito = justice_nameSAAlito,
                   Sotomayor = justice_nameSSotomayor) %>%
            pivot_longer(cols = Thomas:Sotomayor,
                         names_to = "justice",
                         values_to = "vote_direction") %>% 
            ggplot(aes(x = vote_direction)) +
            geom_histogram(aes(y = after_stat(count/sum(count)),
                               fill = justice),
                           alpha = 0.5, 
                           bins = 100, 
                           position = "identity",
                           color = "white") +
            geom_vline(xintercept = 0.5, lty = 2,
                       color = "red") +
            labs(title = "Posterior Probability Distribution",
                 subtitle = (paste("Average Vote Direction on",input$I)),
                 x = "Vote Direction",
                 y = "Probability",
                 caption = "Data falling within 0 and 0.5 indicate an instance
                 in which that justice would cast a Liberal vote.
                 Data in between 0.5 and 1 represent a Conservative vote") +
            scale_y_continuous(labels = scales::percent_format()) +
            theme_classic()
    })
}
# Run the application 
shinyApp(ui = ui, server = server)
