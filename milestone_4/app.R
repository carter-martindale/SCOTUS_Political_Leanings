
library(shiny)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(readr)
library(lubridate)
library(janitor)
library(tibble)

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
    select(vote_id, date_decision, us_cite, term, chief,
           case_origin_state, issue, issue_area, decision_direction,
           maj_opin_writer)

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
    select(voteId, dateDecision, term, chief,
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
             h3("This first model is pretty primitive, but sometimes a coherent
               plot is produced if you choose the right variables."),
             fluidPage(
                 selectInput("x", "X variable", choices = names(d)),
                 selectInput("y", "Y variable", choices = names(d)),
                 # selectInput("facet", "Facet By", choices = names(d)),
                 selectInput("geom", "geom", c("point", "column", "jitter")),
                 plotOutput("plot"))),
    tabPanel("Model 2",
             h3("I'm not really sure what I'm going to do with this
             second set of data, but it focuses more on individual Justice
             votes, so I will likely try to focus on individual Justice trends
             rather than overall court trends."),
    fluidPage(
        verbatimTextOutput("table")
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
                 href = "https://github.com/carter-martindale/final_project_test"),
               "to see the rest of my work")))

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    plot_geom <- reactive({
        switch(input$geom,
               point = geom_point(),
               #smooth = geom_smooth(se = TRUE, na.rm = TRUE),
               jitter = geom_jitter(),
               column = geom_col()
        )
    })
    
    output$plot <- renderPlot({
        ggplot(d, aes(.data[[input$x]], .data[[input$y]])) +
            plot_geom()
    }, res = 96)
    
    output$table <- renderText({
        summary(justice)
    })
}
# Run the application 
shinyApp(ui = ui, server = server)
