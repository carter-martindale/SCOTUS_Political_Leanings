
library(shiny)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(readr)
library(lubridate)
library(janitor)
library(tibble)
library(forcats)

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
           issue_area, jurisdiction, decision_direction)

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
           caseOriginState, issue, issueArea,
           decisionDirection, justice, justiceName, vote,
           decisionDirectionDissent, majOpinWriter) %>% 
    clean_names()

# Define UI for application that draws a histogram
ui <- navbarPage(
    "Supreme Court Over the Years",
    tabPanel("Home", 
             titlePanel("Welcome to my Page!"),
             h3("About Me"),
             p("My name is Carter Martindale and I study History.
               I'm originally from Hyde Park, Utah, and am a current
               sophomore at Harvard. I'm also a student coordinator for",
               a("HFAI,", 
                 href = "https://college.harvard.edu/carter-martindale"),
               "so feel free to check ouut that page.
               You can reach me at carter_martindale@college.harvard.edu.")),
    tabPanel("Model",
             # h3("Babysteps"),
             # p("This first model is pretty primitive, but sometimes a coherent
             #   plot is produced if you choose the right variables."),
             # fluidPage(
             #     selectInput("x", "X variable", choices = names(d)),
             #     selectInput("y", "Y variable", choices = names(d)),
             #     # selectInput("facet", "Facet By", choices = names(d)),
             #     selectInput("geom", "geom", c("point", "column", "jitter")),
             #     plotOutput("plot")),
            h3("Most Commmon..."),
            p("This plot can be used to find out the most common x of 
            SCOTUS- the most common state for a case to come from, the most
            common issue area for a case to fall under, etc."),
            fluidPage(
                 selectInput("z", "X Variable", choices = c("case_origin_state",
                                                            "jurisdiction",
                                                            "issue_area",
                                                            "decision_direction")),
                 plotOutput("plot2"))),
    
    tabPanel("The Good Stuff",
            h3("Case Salience"),
            p("This plot shows trends in the number of cases under each chief
              by issue area. You can notice some historical trends in the type
              of cases brought before each court."),
            fluidPage(
                sliderInput("a", "Issue Area", min = 1,
                            max = 14, value = 3)),
            plotOutput("plot4"),
            h3("Which Way Did a Court Lean?"),
            p("This plot gives you the chance to choose a Chief Justice,
              and see whether the majority of their decisions were liberal,
              conservative, or somewhere in between"),
            fluidPage(
                selectInput("c", "Chief", choices = c("Vinson", "Warren",
                                                      "Burger", "Rehnquist",
                                                      "Roberts")),
                sliderInput("b", "Issue Area", min = 1,
                            max = 14, value = 3),
                plotOutput("plot5"))),
    
    tabPanel("Interesting Findings",
             h3("The Liberal Warren Court"),
             p("So this isn't reactive, but I found this graph very intersesting.
                The Warren Court by far has been the most liberal court in the last
                70 years, but both the Burger and Rehnquist courts seemed to
                step back from the liberal rulings under Chief Justice Warren"),
             fluidPage(
                 plotOutput("plot3")
             )),
    
    tabPanel("Discussion",
             titlePanel("Discussion Title"),
             p("Eventually I'll include a discussion about my data
               here and the choices I made. For now, can anyone help me with
               working with github and large files?")),
    
    tabPanel("About", 
             titlePanel("About"),
             h3("Project Background and Motivations"),
             p("Long story short I became a Supreme Court case law
               nerd my senior year of high school. This project 
               is essentially trying to draw meaningful conclusions
               about the history of the Supreme Court, and specifically
               how the court has ruled historically on various hot button
               issues."),
             p("So far I have begun working with two different datasets.
               The first is from the Supreme Court Data Project and contains
               a fairly comprehensive list of all Supreme Court decisions from
               the Vinson Court until 2019. This dataset focused primarily on
               the legal issue presented in each case. The second dataset is from a
               project called SCOTUS opinions. This dataset also includes
               a list of Supreme Court Decisions, however this dataset focuses
               more on the justices themselves and how they voted on each issue.
               Check out my repo",
               a("here", 
                 href = "https://github.com/carter-martindale/milestone_4"),
               "to see the rest of my work")))

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    # plot_geom <- reactive({
    #     switch(input$geom,
    #            point = geom_point(),
    #            #smooth = geom_smooth(se = TRUE, na.rm = TRUE),
    #            jitter = geom_jitter(),
    #            column = geom_col()
    #     )
    # })
    # 
    # output$plot <- renderPlot({
    #     ggplot(d, aes(.data[[input$x]], .data[[input$y]])) +
    #         plot_geom()
    # }, res = 96)
    
    output$plot2 <- renderPlot({
       ggplot(d, aes(.data[[input$z]])) +
       geom_bar()
    })

    output$plot4 <- renderPlot({
        d %>% 
            filter(issue_area == input$a) %>% 
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
                 y = "Number of Cases",
                 caption = "For reference, the Issue Areas are as follows:
                 Criminal Procedure = 1,
                 Civil Rights = 2,
                 1st Amendment = 3,
                 Due Process = 4,
                 Privacy = 5,
                 Attorney/Government Fees and Compensation = 6,
                 Unions = 7,
                 Economic Activity = 8,
                 Judicial Power = 9,
                 Federalism = 10,
                 Interstate Relations = 11,
                 Federal Taxation = 12,
                 Misc = 13,
                 Private Laws = 14")
    })
    
    output$plot5 <- renderPlot({
        d %>% 
            filter(issue_area == input$b,
                   chief == input$c) %>%
            ggplot(aes(x = issue_area)) +
            geom_bar() +
            facet_wrap(~decision_direction,
                     labeller = labeller(decision_direction = c(
                "1" = "Conservative", "2" = "Liberal"))) +
            labs(title = "Decision Direction by Issue",
                 x = "Issue Area",
                 y = "Number of Cases")
    })
    
    output$plot3 <- renderPlot({
        d %>%
            filter(decision_direction %in% c(1, 2)) %>% 
        ggplot(aes(x = term, fill = chief)) +
        geom_bar(position = "dodge") +
        facet_wrap(~decision_direction,
                   labeller = labeller(decision_direction = c(
                       "1" = "Conservative", "2" = "Liberal"))) +
        scale_fill_discrete(name = "Chief",
                            breaks = c("Vinson", "Warren",
                                       "Burger", "Rehnquist",
                                       "Roberts")) +
        labs(title = "Direction of SCOTUS Rulings",
             x = "Term",
             y = "Number of Rulings")
    })
}
# Run the application 
shinyApp(ui = ui, server = server)
