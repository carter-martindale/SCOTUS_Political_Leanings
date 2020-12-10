
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
library(shinythemes)

source('D_dataset.R')
source('Justice.R')
source('model_1.R')

# I used source here to prevent me just dumping in all of the messy data 
# cleaning and manipulating I did as a precursor to actually making the
# shinyapp. 


# Define UI for application that draws a histogram
ui <- navbarPage(
    "Supreme Court Decisions- Apolitical or Not?",
    tabPanel("A Historical Overview of SCOTUS Cases",
             h3("Case Salience"),
             p("Acting as background knowledge for my models, this page focuses
            on the history of voting decisions within the court. 
            This plot visually displays the number of cases under each Chief
              Justice by issue area. See if you can notice any patterns in the
              type of cases brought before each court."),
             fluidPage(theme = shinytheme("cerulean"),
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
               bipartisan balance (for example, the Vinson Court on Civil
               Rights issues). Other courts and other issues, however,
               clearly lean one way over the other (for example, the Roberts
               Court on Federal Taxation). Moving forward from this,
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
            p("As you can see, some issues are much more 'clear cut' than others
              when it comes to predicting which way a Justice will lean. I took
              a closer look at specifically issues on the 1st amendment and came
              up with these results:"),
            fluidPage(theme = shinytheme("cerulean"),
                      fluidRow(
                          column(5, gt_output("model_table1"), offset = 4))),
            
            # I wanted to put the table in the middle of the page rather than 
            # just off to the left side. I also didn't like how it appeared
            # originally, so I used the column() function to determine how wide
            # it would be and where it would appear on the page. 

            p("In this instance, the intercept (or reference point) represents
              Justice Thomas. The Beta value here represents the relative 
              conservatism of a judge. A value of 0.66 shows that Justice Thomas
              is predicted to be
              fairly conservative on issues of the 1st Amendment. The rest of
              the values show each justice in comparison to Thomas- Alito is 
              somewhat more liberal, although likely to still cast a 
              conservative vote based on our plot. Sotomayor on the other hand 
              is significantly
              less conservative than Thomas. However, it's important to note
              that this table by itself simply shows directionality- IE more or
              less conservative. This is opposed to the plot above, which 
              displays the predicted likelihood of a conservative vote. ")
            ),
    
    tabPanel("Model- Petition Centered",
            h3("Can We Predict What Cases Will be Chosen?"),
            p("The central question in this model is the following: Can we 
              predict what type of cases the Supreme Court will choose? We will
              be drawing upon information from all of the petitions for
              writ of Certiori that have been presented while Justice Roberts
              has been Chief Justice."),
            p("For reference, parties who are not satisfied with the decision
              of a lower court may petition the Supreme Court to hear their case.
             The primary means to petition the court for review is to ask it to
              grant a writ of certiorari,  or an order from the Supreme Court to
              to a lower court to send up the record of the case for review.
              The Supreme Court is under no obligation to hear these cases, and 
              four of the nine Justices must vote to accept such a petition."),
            
            
            p("First let's look at some tables. For my first model, I analyzed
              the percentage of petitions that were successful based on the
              nature of the case, which circuit court it came from, and what
              year the petition was submitted. You can think of the Intercept
              as a starting value. In this case, it represents an administrative
              appeals case from the First Circuit filed in 2005. The Beta
              values then describe the predicted success rate relative to that 
              first case- so a higher Beta value means a higher predicted 
              success rate, and a negative Beta value means that case is less
              likely to be granted a petition."),
            gt_output("model_table2"),
            
            p("Each of the predictions for the
            nature_of_proceeding characteristics can be considered significant
            for our purposes. This means that administrative appeals cases are 
            predicted to have the highest percentage of success, and that
            both U.S. and Private Civil cases are less likely to 
            have their petition approved than a criminal case."),
            p("My model also predicts that the later a case is filed, the less
            likely a case is to have its petition granted. Finally, while not
            all of the Circuit coefficients can be considered statistically 
            significant, my model predicts that cases from the DC and 7th
            Circuit courts will have the greatest percentage of success compared
            to other Circuits."),
            plotOutput("plot_circuit"),
            p("This plot shows the predicted probability for the percentage of 
              cases that will be granted a petition based on their origin
              location. As we saw in the first table, DC is predicted to
              have the highest percentage of petitions granted."),
            p("The following table is a continuation of the idea in table 1, but
              this time we are looking to see if there is any significant 
              interaction between the type of case and the origin location."),
            
            fluidPage(theme = shinytheme("cerulean"),
                fluidRow(
                column(5, gt_output("model_table3"), offset = 1),
                column(5, gt_output("model_table4"), offset = 1))),
            
            p("Looking at the table, you will notice the massive range of the
            confidence interval, and that none of them disprove the null 
            hypothesis. For these reasons, I don't think the interaction term
            of origin circuit court and case type give us any significant
            findings. This could be because I don't have enough data,
              or because there really isn't any significant relationship
              between the interaction of case origin and case type on the 
              success of a petition."),
            
            p("Moving forward, I think the lack of more variables really hampers 
              my model. I think that future models could
              try to gather information pertaining to whether or not there was
              media coverage around a certain case, who filed each case, the
              number of amicus Curae briefs submitted on behalf of a case, and 
              other factors that, in my mind, might also play into whether or
              not a case is chosen by the Supreme Court.")
            ),
    
    tabPanel("Interesting Findings",
             h3("The Liberal Warren Court"),
             p("Returning to one of my original questions, regarding the 
             liberality or conservativity of each court, I think this plot is a 
             very simple representation of my findings. 
                The Warren Court by far has been the most liberal court in the last
                70 years, but both the Burger and Rehnquist courts seemed to
                step back from the liberal rulings under Chief Justice Warren.
               The Roberts court seems to have thus far had a balance between
               liberal and conservative rulings, and the Vinson Court had more
               Conservative than Liberal rulings."),
             fluidPage(theme = shinytheme("cerulean"),
                 plotOutput("plot3")
             ),
             h3("The Emergence of Privacy in Case Law"),
             p("As I was making the plot displayed on the first page, I noticed
             that the Vinson and Warren Courts had almost no cases regarding 
             the right to Privacy. At first I was somewhat surprised, but after 
             thinking about it this has a purely historical explanation. 
             The right to privacy is not explicitly stated anywhere in the 
             Constitution, and it wasn't until the mid-to-late 60's that 
             discussions of a 'right to privacy' really occured on the Supreme
             Court. In 1965 the Courts recognized 'zones of privacy' in their 
             ruling on Griswold v. Connecticut and they expanded those 
             protections in 1972 in Roe v. Wade."),
             plotOutput("plot_privacy"),
             p("In the 70's this new explicit use of the 'right to privacy' was
             expanded to touch on many different issues, explaining the sharp
             uptake of privacy cases just prior to 1980"),
             h3("Who is Really the Most Polarized?"),
             p("Especially in our day and with the passing of RGB, we often
               refer to her as one of the most liberal of all Supreme Court
               Justices. I wanted to see if that was true. Again, the Beta 
               value in this table represents the relative
               leaning of a Justice's decisions. Although they are relative to 
               Justie Anthony Kennedy, someone who has had a fairly balanced 
               voting record, as a rule of thumb a higher beta value represents
               being more conservative, and a lower value represents being
               more liberal. After analyzing the results I have included
               the 'top 3' of each direction, while also included RBG for 
               reference. "),
             gt_output("model_table_greats"),
             p("As you can see, Justice Thomas appears as the most conservative
               Justice, with Alito and Gorsuch taking the next two spots. It may
               be interesting to note that Justice Scalia does not appear in the 
               top 3 conservative Justices. On the other side, Justice Marshall 
               is the most liberal of Justices to date, with Justices Brennan
               and Stevens in the next two spots. RBG, based on my findings, is
               the 5th most Liberal Justice the Court has seen since the 1950's.
               ")
             ),
    
    
    tabPanel("About", 
             titlePanel("About"),
             h3("Project Background and Motivations"),
             p("I became a Supreme Court case law nerd my senior 
             year of high school when I participated in 'We The People', 
             essentially a constitutional history and law competition for 
             students. I wanted to undertake this project as an attempt to draw
             conclusions about the history SCOTUS, and specifically how they
             have ruled on various issues and how they may rule on those
             same issues today."),
             p("Check out my repo",
               a("here", 
                 href = "https://github.com/carter-martindale/SCOTUS_Political_Leanings"),
               "to see the rest of my work."),
             h3("Discussion of the Data"),
             p("I drew primarily from three sources of data for my project.
             I used two datasets from the Supreme Court Database which gave 
             me access to every Supreme Court Decision since the 1950's. Cases
             were already divided into preset issue areas, and decisions by
             the overall court were classified as either liberal or conservative. 
             The second dataset from the Supreme Court Database focused on the 
             decisions of individual justices, which allowed me to make a model
             predicting the decisions of current Justices."),
             p("The last source of data I used was compiled from uscourts.gov and
               it contained information about how many cases were petitioned to 
               the 
               Supreme Court in a given year. This data was already divided based
               on which court the case came from. This data was the basis of my
               second model, allowing me to predict the percentage of cases that
               would be granted a writ of Certiori based on the nature of the 
               case and the origin of that case. Unfortunately, those were 
               the only two variables readily available. In the future, a more
               in depth analysis of other factors (who such cases were 
               represented by, how many briefs were submitted in support of it,
               media coverage of a case, etc) would be highly beneficial
               to the accuracy of any predictive model."),
             h3("About Me"),
             p("My name is Carter Martindale and I'm pursuing an AB in 
             Government. I'm originally from Hyde Park, Utah, and am a 
             current sophomore at Harvard studying remotely from Provo, Utah. 
             Outside of class I'm involved with Harvard Model Congress, The
               Small Claims Advisory Service, and my church group. 
               I'm also a student coordinator for",
               a("HFAI,", 
                 href = "https://college.harvard.edu/carter-martindale"),
               "so feel free to check out that page to learn more about HFAI.
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
            
            # This was to order the legend to match up with the way the
            # plot was displaying the data (chronologically from left to
            # right). 
            
            labs(title = "Direction of SCOTUS Rulings",
                 x = "Term",
                 y = "Number of Rulings")
        
        # This is a fairly simple plot displaying all of the SCOTUS
        # decisions from the 1950's onward. It's faceted by decision direction
        # so that you can compare liberal vs conservative rulings, and then it
        # is color coordinated by Chief Justice. 
    })

    output$plot4 <- renderPlot({
        d %>% 
            filter(issue_area == input$a) %>% 
            
            # Really the only fun thing on this graph, this allows
            # users to choose what issue area they want to look at
            # and then see how each court has ruled on that issue. 
            
            mutate(chief = fct_relevel(chief,
                                         "Vinson", "Warren",
                                         "Burger", "Rehnquist",
                                         "Roberts")) %>% 
            
            # This releveling made sure that the data was displayed in 
            # chronological order from left to right. 
            
        ggplot(aes(x = issue_area, fill = chief)) +
            geom_bar(position = "dodge") +
            scale_fill_brewer(name = "Chief",
                                breaks = c("Vinson", "Warren",
                                           "Burger", "Rehnquist",
                                           "Roberts"),
                                    palette = "Accent") +
            labs(title = "Salience of Cases by Issue Area",
                 x = "Issue Area",
                 y = "Number of Cases")
    })
    
    output$plot5 <- renderPlot({
        d %>% 
            filter(issue_area == input$b,
                   chief == input$c) %>%
            
            # This has room for users to customize two things- both the
            # issue area ruled on, and then the Chief Justice that was currently
            # appointed. 
            
            drop_na(decision_direction) %>% 
            ggplot(aes(x = issue_area)) +
            geom_bar(fill = "deepskyblue4",
                     aes(y = (..count..)/sum(..count..))) +
            scale_y_continuous(labels = scales::percent) +
            facet_wrap(~decision_direction,
                     labeller = labeller(decision_direction = c(
                "0" = "Liberal", "1" = "Conservative", "3" = "Neither"))) +
            
            # For my other work on this app I basically got rid of the 'neither'
            # category, but I noticed that for some courts and some issues they 
            # only ruled in a 'neither' direction, so I included it in this graph
            # and needed to add a label that didn't show up as simply NA. 
            
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
    
    output$model_table1 <- render_gt({
        
        tbl_regression(fit_obj, intercept = TRUE,
                       estimate_fun = function(polarized)
                           style_sigfig(polarized, digits = 3)) %>%
            
            # Snagged this code from the class slack- I wanted to have more than
            # just one decimal place displayed, and this allows me to choose how
            # many decimal places to output in the table. 
            
            as_gt() %>% 
            tab_header(title = "Logistic Regression of Justice Vote Direction", 
                       subtitle = "Looking at 1st Amendment Cases") %>%
            tab_source_note(md("Source: The Supreme Court Database"))
        
        # What I did for this table can be explained in my model_1 R doc
    })
    
    output$model_table2 <- render_gt({
        tbl_regression(roberts_new_intercept, intercept = TRUE,
                       estimate_fun = function(polarized)
                           style_sigfig(polarized, digits = 3)) %>%
            as_gt() %>%
            tab_header(title = "Logistic Regression of Granted Petitions for Writ of Certiori",
                       subtitle = "The Effect of case type, origin, and year on petition success") %>%
            tab_source_note(md("Source: Circuit Court Data"))
        
        # See above- the work and explanations can be found in model_1 R
    })
    
    output$model_table3 <- render_gt({
        tbl_regression(roberts_model_2, intercept = TRUE,
                       include = c("(Intercept)", "nature_of_proceeding",
                                   "district"),
                       estimate_fun = function(polarized)
                           style_sigfig(polarized, digits = 3)) %>% 
            as_gt() %>% 
            tab_header(title = "Logistic Regression of Granted Petitions for Writ of Certiori",
                       subtitle = "The Effect of case type and origin interaction on petition success") %>%
            tab_source_note(md("Source: Circuit Court Data"))
    })
    
    output$model_table4 <- render_gt({
        tbl_regression(roberts_model_2, intercept = TRUE,
                       include = c("nature_of_proceeding:district"),
                       estimate_fun = function(polarized)
                           style_sigfig(polarized, digits = 3)) %>% 
            as_gt() %>% 
            tab_header(title = "Logistic Regression of Granted Petitions for Writ of Certiori",
                       subtitle = "The Effect of case type and origin interaction on petition success") %>%
            tab_source_note(md("Source: Circuit Court Data"))
    })
    
    
    output$plot_circuit <- renderPlot({
        ep_new %>%
            ggplot(aes(Prediction)) +
            geom_histogram(aes(y = after_stat(count/sum(count))),
                           bins = 100, 
                           position = "identity",
                           color = "deepskyblue4") +
            
            # This displays the actual predictions of the likelihood a case
            # is chosen based on origin Circuit. The rest of the work creating ep
            # can be found in model_1.R 
            
            facet_wrap(~Circuit_f) +
            labs(x = "Percentage of Successful Petitions",
                 y = "Probability",
                 title = "Percentage of Successful Petitions by Circuit Origin",
                 subtitle = "DC seems to be the most reliable circuit for a successful petition") +
            theme_classic()
    })
    
    output$plot_privacy <- renderPlot ({
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
        
        # Simple plot displaying privacy cases by term, not by chief Justice
        # which is something that I have done elsewhere on my app. This is just
        # showing an interesting historical trend that I noticed while looking
        # at my data. 
        
    })
    
    output$model_table_greats <- render_gt({
        tbl_regression(polarized, intercept = TRUE,
                       estimate_fun = function(polarized)
                           style_sigfig(polarized, digits = 3)) %>% 
            as_gt() %>%
            tab_header(title = "Linear Regression of Justice Vote Direction",
                       subtitle = "A selection of the most conversative and liberal justices") %>%
            tab_source_note(md("Source: Supreme Court Database"))
        
        # Explanation for this work can be found in the Justice.R doc
    })

}
# Run the application 
shinyApp(ui = ui, server = server)
