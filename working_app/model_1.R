## Petition Centered Model

# I needed to get data from the circuit courts regarding all 
# of the petitions for a writ of certiori, and lots of the
# data came in differently formatted spreadsheets (or even
# pdfs). So the following functions read in and clean the data
# into a form that I can then draw some meaningful analysis
# from.

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

# The spreadsheets from 2016 - 2019 can be read in using this formula

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

# 2005 - 2015 can be read in this way, with three exceptions
# addressed below

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

# Basically the same formula as above, just needed to skip
# a few more rows of data for these two years for some reason. I was very
# annoyed when I first had to do this, and thus the function name. 

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

# 2008 was a really special year apparently. This went by a different format
# from the general functions I made, and the two other exceptions. 

p_2019 <- read_petition_total("raw_data/jb_b2_0930.2019.xlsx", 2019)
p_2018 <- read_petition_total("raw_data/jb_b2_0930.2018.xlsx", 2018)
p_2017 <- read_petition_total("raw_data/jb_b2_0930.2017.xlsx", 2017)
p_2016 <- read_petition_total("raw_data/jb_b2_0930.2016.xlsx", 2016)
p_2015 <- read_p("raw_data/B02Sep15.xlsx", 2015)
p_2014 <- read_p("raw_data/B02Sep14.xlsx", 2014)
p_2013 <- read_p("raw_data/B02Sep13.xlsx", 2013)
p_2012 <- read_p("raw_data/B02Sep12.xlsx", 2012)
p_2011 <- read_p("raw_data/B02Sep11.xlsx", 2011)
p_2010 <- read_annoyed("raw_data/B02Sep10.xlsx", 2010)
p_2009 <- read_p("raw_data/B02Sep09.xlsx", 2009)
p_2008 <- read_2008("raw_data/B02Sep08.xlsx", 2008)
p_2007 <- read_p("raw_data/B02Sep07.xlsx", 2007)
p_2006 <- read_annoyed("raw_data/b2_2.xlsx", 2006)
p_2005 <- read_p("raw_data/b2_1.xlsx", 2005)

# Using the functions created above, I then read in all the data. 

p_2016$nature_of_proceeding[which(p_2016$nature_of_proceeding == "U.S. Private")] <- 
  "Private Civil"

# For some reason 2016 is the only year they called Private
# Civil U.S. Private, so we had to change that. 

roberts_circuit <- rbind(p_2019, p_2018, p_2017, p_2016, p_2015, p_2014, p_2013,
                         p_2012, p_2011, p_2010, p_2009, p_2008, p_2007, p_2006,
                         p_2005)

# This created the ultimate dataset that I wanted to begin with- circuit court
# information from every year that Chief Justice Roberts has been on the court.

roberts_circuit$granted[which(roberts_circuit$granted ==
                                "-")] <- 0
roberts_circuit$denied[which(roberts_circuit$denied ==
                               "-")] <- 0
roberts_circuit$dismissed[which(roberts_circuit$dismissed ==
                                  "-")] <- 0
roberts_circuit$filed[which(roberts_circuit$filed ==
                              "-")] <- 0

# The original formatting had hyphens in the place of zeros which was
# really messing with my functions when I tried to create new variables,
# so I manually replaced a hyphen with a 0 in each of the columns I would
# be working with. 

roberts_circuit <- roberts_circuit %>%   
  mutate(percent_granted =
           (as.numeric(granted)/as.numeric(filed)) *100,
         percent_denied =
           (as.numeric(denied)/as.numeric(filed)) *100)

# This will allow my models to work with a percentage of cases granted
# rather than simply the overall amount of cases. This will control for
# different amounts of cases being submitted for petition in different
# places and during different years. 

roberts_circuit$percent_granted[which(roberts_circuit$percent_granted == "NaN")] <- 0
roberts_circuit$percent_denied[which(roberts_circuit$percent_denied == "Inf")] <- 0

# Since there were a few rows that ended up with 0 divided by
# 0 we got some strange outputs, so I changed those to be 0 since for our
# purposes that is essentially what it is saying. 

roberts_new <- roberts_circuit
roberts_new$district <- factor(roberts_new$district,
                         levels = c("1st", "2nd", "3rd", "4th", "5th",
                                    "6th", "7th", "8th", "9th", "10th",
                                    "11th", "DC"))

# At first it was spitting out data with funny levels- it had the 11th
# Circuit first, and then 10th, and then 1st, etc. So I decided to 
# reorder my data so 1st came first and DC came last.

fit <- stan_glm(percent_granted ~ district,
                data = roberts_new,
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

# This was the code that I needed to be able to create the plot that shows
# the predicted percentage of petitions that are successful based on district. 
# The stan_glm creates the model where the percent_granted is regressed onto
# district. I then created a new_obs to use with posterior_epred that then
# predicts the percent of successful petitions in each of the 12 Circuit courts.
# The aesthetic changes and pivot I added to this pipe to make
# the actual plot output that appears in my app.r document simpler. 

ep_new <- ep
ep_new$Circuit_f <- factor(ep_new$Circuit,
                               levels = c("1", "2", "3", "4", "5",
                                          "6", "7", "8", "9", "10",
                                          "11", "DC"))

# The last thing I did was to make a new variable circuit_f which was
# the circuit value, but as a factor. Reordering that factor then made
# my plot appear in numerical order, rather than the formerly apparently
# random order it appeared in at first. 

# From this point on is the code I used to create the tables that 
# are displayed with my first model. The objects created here come primarily from 

roberts_model <- stan_glm(percent_granted ~
                            nature_of_proceeding + year
                          + district - 1,
                          data = roberts_circuit,
                          refresh = 0)

# My first model- very simple, just regressing the percent of
# granted cases onto the nature of the case, the year
# it was petitioned, and the district it came from. 

roberts_new_intercept <- stan_glm(percent_granted ~
                          nature_of_proceeding + year
                        + district,
                        data = roberts_new,
                        refresh = 0)

# This is the same model as my first, just with an intercept this time. 

roberts_model_2 <- stan_glm(percent_granted ~
                              nature_of_proceeding*district,
                            data = roberts_new,
                            refresh = 0)

# My second model- this one looked to see if there was any 
# significant interaction between the district a case came
# from and the nature of that case (IE is a criminal case more
# likely to be heard if it is from a certain district?).
# Using the MAD_SD, proceedings from the 11th Circuit,
# U.S. Civil cases from the 2nd Circuit, Criminal and U.S. Civil
# from the 3rd, Criminal from Circuit 4, Private Civil from
# Circuit 5, Criminal and U.S. Civil from Circuit 8,
# U.S. Civil from Circuit 9, and all proceedings from the
# DC Circuit disprove the null hypothesis. 

tbl_regression(roberts_new_intercept, intercept = TRUE) %>%
  as_gt() %>%
  tab_header(title = "Regression of Granted Petitions for Writ of Certiori",
             subtitle = "The Effect of case type, origin, and year on petition success") %>%
  tab_source_note(md("Source: Circuit Court Data"))

# w/ the intercept. Based on the MAD_SD, all of the nature
# predictions can be regarded as significant, in addition to 
# the value for year. Regarding individual districts,
# only Districts 3, 7, 9, 11, and DC disprove the null 
# hypothesis. I also put this in as an image.

tbl_regression(roberts_model_2, intercept = TRUE,
               include = c("(Intercept)",
                           "nature_of_proceeding:district")) %>%
  as_gt() %>%
  tab_header(title = "Regression of Granted Petitions for Writ of Certiori",
             subtitle = "The Effect of case type and origin interaction on petition success") %>%
  tab_source_note(md("Source: Circuit Court Data"))

tbl_regression(roberts_model_2, intercept = TRUE) %>% 
  as_gt() %>% 
  tab_header(title = "Logistic Regression of Granted Petitions for Writ of Certiori",
             subtitle = "The Effect of case type and origin interaction on petition success") %>%
  tab_source_note(md("Source: Circuit Court Data"))

# For the sake of convenience, this chooses only the rows with the interaction
# terms. All of the other data that would have been put out by this table
# is displayed by the first table. I put this in as an image,
# so there is no need to actually run the code in this r doc.


