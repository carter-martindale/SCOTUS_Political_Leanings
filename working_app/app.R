
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
library(gt)

source('D_dataset.R')
source('Justice.R')
source('model_1.R')


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
                 plotOutput("plot5")),
             p("Some issues, and even some courts overall have had a fairly
               bipartisan balance. Other courts and other issues, however,
               clearly lean one way over the other. Moving forward from this,
               we will be trying to predict how certain justices today would
               vote on each of these issues.")
             ),
    
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
            drop_na(decision_direction) %>% 
            ggplot(aes(x = issue_area)) +
            geom_bar(fill = "deepskyblue4",
                     aes(y = (..count..)/sum(..count..))) +
            scale_y_continuous(labels = scales::percent) +
            facet_wrap(~decision_direction,
                     labeller = labeller(decision_direction = c(
                "0" = "Liberal", "1" = "Conservative", "3" = "Neither"))) +
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
        
        # This filters my data to current Justices who have been on the court
        # for 10 or more years, and then filters it more by the issue
        # area that is chosen by the user. 
        
        fit_obj <- stan_glm(data = fit,
                 direction ~ justice_name - 1,
                 refresh = 0)
        
        # Basic model that regresses voting direction onto each individual
        # justice. 
        
        new_obj <- tibble(justice_name = c("JGRoberts", "CThomas",
                                           "SGBreyer", "SAAlito",
                                           "SSotomayor"))
        
        # This creates the new_data I need for my posterior_epred. I decided
        # to use epred since I didn't want any values less than one, since
        # that would not make sense for a vote direction. 
        
        pp <- posterior_epred(fit_obj, newdata = new_obj) %>% 
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
        
        # This manipulates my tibble to a form that will be easiest to work
        # with for my ggplot.

        pp %>% 
            ggplot(aes(x = Vote)) +
            geom_histogram(aes(y = after_stat(count/sum(count)),
                               fill = Justice),
                           alpha = 0.5, 
                           bins = 100, 
                           position = "identity",
                           color = "white") +
            geom_vline(xintercept = 0.5, lty = 2,
                       color = "red") +
            
            # This vline I felt made it easier for users to see the divide
            # or the line where if the majority of a justice's data fell
            # to the left it would be a Liberal vote, or if it was to the
            # right it would be a conservative vote. 
            
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
