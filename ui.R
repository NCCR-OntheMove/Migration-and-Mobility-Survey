#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinythemes)
library(knitr)
library(kableExtra)
library(highcharter)
library(XML)
#library(tidyverse)
library(dplyr)
library(forcats)
library(tidyr)
library(naniar)
library(XML)
library(readr)
library(leaflet)
library(htmlwidgets)
library(htmltools)

load("./data/dmms.Rdata")
load("./data/countries.Rdata")
SAMPLING<-read_csv("./data/SAMPLING.csv")


#load("./data/mapa16.Rdata")
#load("./data/mapa18.Rdata")

# QUESTION A SURVEY #####

AQUESTIONS <- structure(c("A1","A3","A4_1","A5_1","A6",
                          "A7","A8","A9", "A10","A11"),
                        #"A12","A13"), 
                        .Names = c("Sex and age structure",
                                   "In which country were you born?",
                                   "What nationality(ies) did you have when you were born?",
                                   "What nationality(ies) do you hold?",
                                   "In what year have you last immigrated to Switzerland?",
                                   "What type of residence permit do you have?",
                                   "What is your civil status?",
                                   "Do you have a partner?",
                                   "Where do you live in Switzerland?",
                                   "Do you still live in Switzerland?"))#,
#"Do you think that you have left Switzerland permanently or do you think you will return once for a period of more than three months?",
#"There are many reasons that can explain the departure of persons of foreign origin from Switzerland. Among those, which ones correspond to your actual situation?"))


# QUESTION Ab SURVEY #####

ABISQUESTIONS <- structure(c("sex1","age_group","CONT3", "A6","A7", "A8","A9",
                             "B2CAT","B4","B8_1","B8_2","B8_3","B8_4","B8_5","B8_6","B8_7","B8_8","B8_9","B8_10","B9","B10",
                             "B13_1","B13_2","B13_3","B13_4","B13_5","B13_6",
                             "C1","C2_1","C2_2","C2_3","C2_4","C2_5","C2_6","C2_7",
                             "C3_1","C3_2","C3_3","C3_4","C3_5","C3_6","C3_7","C3_8",
                             "D1","D3","D4",
                             "E1_1","E1_2","E1_3","E1_4","E1_5","E1_6","E1_7","E1_8","E1_9",
                             "E2", "E4","E5",
                             "E11_1","E11_2","E11_3","E11_4","E11_5","E11_6","E11_7","E11_8","E11_9",
                             "E12", "E15",
                             "E16_1","E16_2","E16_3","E16_4","E16_5","E16_6","E16_7","E16_8","E16_9",
                             "E18","E21", "E23","E25", "E27",
                             "F6",
                             "F7_1","F7_2","F7_3","F7_4","F7_5","F7_6","F7_7","F7_8","F7_9",
                             "G1_1","G2","G3","G6","G7","G8", "G11","G13_1","G13_2", "G16_1","G16_2",
                             "H3", 
                             "H8_1","H8_2","H8_3","H8_4","H8_5","H8_6","H8_7","H8_8",
                             "H9_1","H9_2","H9_3","H9_4","H9_6",
                             "H11_1", "H11_2"), 
                           .Names = c("Sex",
                                      "Age",
                                      "Country of nationality: EU/EFTA vs NON-EU/EFTA EFTA",
                                      "In what year have you last immigrated to Switzerland?",
                                      "What type of residence permit do you have?",
                                      "What is your civil status?",
                                      "Are you currently in a relationship with a partner?",
                                      "Except Switzerland and your country of birth, in how many different countries have you lived for three or more months?",
                                      "Is this the first time you have lived for three or more months in Switzerland?",
                                      "What were the reasons for migrating to Switzerland in [A6]?: Professional reasons",
                                      "What were the reasons for migrating to Switzerland in [A6]?: Educational and/or study reasons",
                                      "What were the reasons for migrating to Switzerland in [A6]?: To start a family",
                                      "What were the reasons for migrating to Switzerland in [A6]?: To accompany family",
                                      "What were the reasons for migrating to Switzerland in [A6]?: Lifestyle reasons",
                                      "What were the reasons for migrating to Switzerland in [A6]?: Gain new experiences",
                                      "What were the reasons for migrating to Switzerland in [A6]?: Social network in Switzerland",
                                      "What were the reasons for migrating to Switzerland in [A6]?: Tax reasons",
                                      "What were the reasons for migrating to Switzerland in [A6]?: Political reasons",
                                      "What were the reasons for migrating to Switzerland in [A6]?: Other reasons",
                                      
                                      "Were you married or in a relationship when coming to Switzerland?",
                                      "What was the situation when you moved to Switzerland in [A6]: Which of you moved to Switzerland first, or did you move to Switzerland together?",
                                      "When moving to Switzerland in [A6], did you receive any support in one of the following areas? Allowance for or payment of moving cost",
                                      "When moving to Switzerland in [A6], did you receive any support in one of the following areas? Housing",
                                      "When moving to Switzerland in [A6], did you receive any support in one of the following areas? Dealing with administrative issues",
                                      "When moving to Switzerland in [A6], did you receive any support in one of the following areas? Allowance for or payment of language course",
                                      "When moving to Switzerland in [A6], did you receive any support in one of the following areas? School/childcare",
                                      "When moving to Switzerland in [A6], did you receive any support in one of the following areas? Spouse/partner employment support",
                                      #### C #####
                                      "Do you intend to apply for Swiss nationality in the future?",
                                      "Why do you not wish to acquire the Swiss citizenship nationality? I do not fulfill the requirements",
                                      "Why do you not wish to acquire the Swiss citizenship nationality? I do not intend to stay in Switzerland for good",
                                      "Why do you not wish to acquire the Swiss citizenship nationality? I do not feel a bond with Switzerland",
                                      "Why do you not wish to acquire the Swiss citizenship nationality? I do not see any benefit in it",
                                      "Why do you not wish to acquire the Swiss citizenship nationality? I do not want to give up my current nationality",
                                      "Why do you not wish to acquire the Swiss citizenship nationality? I do not want to lose my rights/benefits of my country of origin",
                                      "Why do you not wish to acquire the Swiss citizenship nationality? I do not want to go through the process, which is too expensive/complicated/long",
                                      
                                      "Why would you like to acquire the Swiss citizenship nationality? My spouse/partner and/or close family members are Swiss",
                                      "Why would you like to acquire the Swiss citizenship nationality? I wish to vote in national elections, to get involved in my local community",
                                      "Why would you like to acquire the Swiss citizenship nationality? It makes it easier to visit my country of origin or other countries",
                                      "Why would you like to acquire the Swiss citizenship nationality? I feel that I belong in Switzerland",
                                      "Why would you like to acquire the Swiss citizenship nationality? It will give me better professional opportunities",
                                      "Why would you like to acquire the Swiss citizenship nationality? It will protect me from being expelled from Switzerland",
                                      "Why would you like to acquire the Swiss citizenship nationality? It simplifies administrative procedures",
                                      "Why would you like to acquire the Swiss citizenship nationality? Other reasons",
                                      
                                      ##### D ####
                                      "What is the highest level of education you have successfully completed?",
                                      "What is the highest level of education that you have completed in your country of origin [B1]?",
                                      "Did you make an official request in Switzerland in order to obtain a certificate of equivalence for your educational qualifications?",
                                      
                                      #### E ####
                                      "What was your labor market situation in [if B2=0 then response A3 if B2>0 then response B7] before moving to Switzerland in [A6]? Were you in full-time employment.",
                                      "What was your labor market situation in [if B2=0 then response A3 if B2>0 then response B7] before moving to Switzerland in [A6]? Were you in part-time employment.",
                                      "What was your labor market situation in [if B2=0 then response A3 if B2>0 then response B7] before moving to Switzerland in [A6]? Were you working in more than one part-time job.",
                                      "What was your labor market situation in [if B2=0 then response A3 if B2>0 then response B7] before moving to Switzerland in [A6]? Were you seeking a job (whether or not registered for unemployment insurance).",
                                      "What was your labor market situation in [if B2=0 then response A3 if B2>0 then response B7] before moving to Switzerland in [A6]? Were you undergoing training (school, studies, apprenticeship).",
                                      "What was your labor market situation in [if B2=0 then response A3 if B2>0 then response B7] before moving to Switzerland in [A6]? Were you looking after home or family.",
                                      "What was your labor market situation in [if B2=0 then response A3 if B2>0 then response B7] before moving to Switzerland in [A6]? Were you disabled or partially disabled.",
                                      "What was your labor market situation in [if B2=0 then response A3 if B2>0 then response B7] before moving to Switzerland in [A6]? Were you in  retired (in receipt of state (retirement) or other pension), or pensioner in receipt of benefit other than disability insurance.",
                                      "What was your labor market situation in [if B2=0 then response A3 if B2>0 then response B7] before moving to Switzerland in [A6]? Were you in another non-employed situation.",
                                      
                                      "What was your occupational status before moving to Switzerland in [A6]?",
                                      "Did you have a job contract or a job offer in Switzerland at the time you immigrated to Switzerland in [A6]?",
                                      "Was it a transfer within the same company?",
                                      
                                      "What was your labor market situation once you arrived in Switzerland in [A6]? Were you in full-time employment.",
                                      "What was your labor market situation once you arrived in Switzerland in [A6]? Were you in part-time employment.",
                                      "What was your labor market situation once you arrived in Switzerland in [A6]? Were you in more than one part-time job.",
                                      "What was your labor market situation once you arrived in Switzerland in [A6]? Were you seeking a job (whether or not registered for unemployment insurance).",
                                      "What was your labor market situation once you arrived in Switzerland in [A6]? Were you undergoing training (school, studies, apprenticeship).",
                                      "What was your labor market situation once you arrived in Switzerland in [A6]? Were you looking after home or family.",
                                      "What was your labor market situation once you arrived in Switzerland in [A6]? Were you disabled or partially disabled.",
                                      "What was your labor market situation once you arrived in Switzerland in [A6]? Were you in  retired (in receipt of state (retirement) or other pension), or pensioner in receipt of benefit other than disability insurance.",
                                      "What was your labor market situation once you arrived in Switzerland in [A6]? Were you in another non-employed situation.",
                                      
                                      "What was your occupational status once you arrived in Switzerland in [A6]?",
                                      "Do you still have the same job and position as when you arrived in Switzerland in [A6]?",
                                      
                                      "What is your current labor market situation? Are you in full-time employment.",
                                      "What is your current labor market situation? Are you in part-time employment.",
                                      "What is your current labor market situation? Are you in more than one part-time job.",
                                      "What is your current labor market situation? Are you seeking a job (whether or not registered for unemployment insurance).",
                                      "What is your current labor market situation? Are you undergoing training (school, studies, apprenticeship).",
                                      "What is your current labor market situation? Are you looking after home or family.",
                                      "What is your current labor market situation? Are you disabled or partially disabled.",
                                      "What is your current labor market situation? Are you in  retired (in receipt of state (retirement) or other pension), or pensioner in receipt of benefit other than disability insurance.",
                                      "What is your current labor market situation? Are you in another non-employed situation.",
                                      
                                      "What is your current occupational status?",
                                      "Do you have a work contract of.",
                                      "What type of education do you feel is most appropriate for your current job?",
                                      "On a scale from 0 (not at all) to 7 (to a very high extent), to what extent are your knowledge and overall skills utilized in your current work?",
                                      "Regarding your professional situation, what would you say overall when comparing your situation today with your situation before moving to Switzerland in [A6]? It has.",
                                      
                                      #### F #####
                                      "What is the highest level of education your spouse/partner has successfully completed?",
                                      
                                      "What is your spouse/partner's current labor market situation? Is he/she in full-time employment.",
                                      "What is your spouse/partner's current labor market situation? Is he/she in part-time employment.",
                                      "What is your spouse/partner's current labor market situation? Is he/she in more than one part-time job.",
                                      "What is your spouse/partner's current labor market situation? Is he/she seeking a job (whether or not registered for unemployment insurance).",
                                      "What is your spouse/partner's current labor market situation? Is he/she undergoing training (school, studies, apprenticeship).",
                                      "What is your spouse/partner's current labor market situation? Is he/she looking after home or family.",
                                      "What is your spouse/partner's current labor market situation? Is he/she disabled or partially disabled.",
                                      "What is your spouse/partner's current labor market situation? Is he/she in  retired (in receipt of state (retirement) or other pension), or pensioner in receipt of benefit other than disability insurance.",
                                      "What is your spouse/partner's current labor market situation? Is he/she in another non-employed situation.",
                                      
                                      "Which language do you relate to and master best?",
                                      "How well do you understand the local language?",
                                      "How well do you speak the local language?",
                                      "Where do your very good friends live?",
                                      "Have you visited your country of origin since you have arrived in Switzerland?",
                                      "How often have you visited your country of origin since you have arrived in Switzerland?",
                                      "Have you ever been visited by family of friends from your country of origin [B1]?",
                                      "On a scale from 0 (not at all) to 7 (to a very high extent), to what extent are you interested in news and current events in Switzerland",
                                      "On a scale from 0 (not at all) to 7 (to a very high extent), to what extent are you interested in news and current events in your country",
                                      "How interested would you say you are in politics in Switzerland", 
                                      "How interested would you say you are in politics in your country",
                                      
                                      ##### H #####
                                      "How often have you considered emigrating from Switzerland in the last three months?",
                                      "In your opinion, on the basis of what actual or assigned characteristic(s) has this discrimination taken place? Because of racist reasons.",
                                      "In your opinion, on the basis of what actual or assigned characteristic(s) has this discrimination taken place? Because of your immigrant background, origin, nationality.",
                                      "In your opinion, on the basis of what actual or assigned characteristic(s) has this discrimination taken place? Because of your religion.",
                                      "In your opinion, on the basis of what actual or assigned characteristic(s) has this discrimination taken place? Because of your gender.",
                                      "In your opinion, on the basis of what actual or assigned characteristic(s) has this discrimination taken place? Because of your disability, impairment or chronic disease.",
                                      "In your opinion, on the basis of what actual or assigned characteristic(s) has this discrimination taken place? Because of your age.",
                                      "In your opinion, on the basis of what actual or assigned characteristic(s) has this discrimination taken place? Because of your sexual orientation.",
                                      "In your opinion, on the basis of what actual or assigned characteristic(s) has this discrimination taken place? Because of other",
                                      
                                      "Where did you experience this discrimination? Was it during education and work",
                                      "Where did you experience this discrimination? Was it in shops, in public and/or during leisure activities",
                                      "Where did you experience this discrimination? Was it in healthcare and care",
                                      "Where did you experience this discrimination? Was it by public authorities",
                                      "Where did you experience this discrimination? Was it in another situation or by other persons",
                                      
                                      
                                      "On a scale from 0 (no feeling of attachment) to 7 (strong feeling of attachment), to what extent do you have a feeling of attachment to Switzerland",
                                      "On a scale from 0 (no feeling of attachment) to 7 (strong feeling of attachment), to what extent do you have a feeling of attachment to your country"))


# QUESTION B SURVEY #####
BQUESTIONS <- structure(c("B1","B16","B17","B18", "B2","B4","B5","B6",
                          "B8_1","B9","B10","B11","B12", "B13","B14","B21","B22","B23","B24","B25", "B15"), 
                        .Names = c("Which country do you consider to be your country of origin?",
                                   "At the time of our first survey, at the end of 2016, you were living in Switzerland. Can you confirm that you left Switzerland after the 2016 Survey and then you came back in [A6]?",
                                   "How long did you stay abroad before returning to Switzerland in [A6]?",
                                   "What were the reasons explaining your last stay abroad?",
                                   "Except Switzerland and your country of birth, in how many different countries have you lived for three or more months?",
                                   "Is this the first time you have lived for three or more months in Switzerland?",
                                   "How many times (2016) / years (2018) have you previously lived for three or more months in Switzerland?",
                                   "Have you ever been a cross-border commuter in Switzerland before your current stay in Switzerland?",
                                   "What were the reasons for migrating to Switzerland in [A6]?",
                                    "Were you married or in a relationship when coming to Switzerland?",
                                   "What was the situation when you moved to Switzerland in [A6]: Which of you moved to Switzerland first, or did you move to Switzerland together?",
                                   "Which of you had a job here in Switzerland first?",
                                   "When you arrived in Switzerland in [A6], did you have relatives already living here? By relatives, we mean persons who are related to you by blood, marriage or adoption.",
                                   "When moving to Switzerland in [A6], did you receive any support in one of the following areas?",
                                   "From whom did you receive support?",
                                   "Who provided you the support for the payment of moving costs?",
                                   "Who provided you the support for housing?",
                                   "Who provided you the support for dealing with administrative issues?",
                                   "Who provided you the support for school/childcare?",
                                   "Who provided you the support for the spouse/partner employment?",
                                   "On a scale from 0 (not problematic at all) to 7 (very problematic), how problematic were the following aspects when moving to Switzerland?"))
# QUESTION C SURVEY #####
CQUESTIONS <- structure(c("C1","C2_1","C3"), 
                        .Names =c("Do you intend to apply for the Swiss nationality in the future?",
                                  "Why do you not wish to acquire the Swiss nationality?",
                                  "Why would you like to acquire the Swiss nationality?"))

# QUESTION D SURVEY #####
DQUESTIONS <- structure(c("D1","D2","D3","D4", "D7"), 
                        .Names =c("What is the highest level of education you have successfully completed?",
                                  "In which country did you attain your highest educational qualification? If you have several diplomas of the same level, please consider the last one obtained for your answer.",
                                  "What is the highest level of education that you have completed in your country of origin [B1]?",
                                  "Did you make an official request in Switzerland in order to obtain a certificate of equivalence for your educational qualifications?",
                                  "Please enter the formal education in which you are currently engaged."))

# QUESTION E SURVEY #####
EQUESTIONS <- structure(c("E1","E30","E31", "E2","E3", "E32","E33", "E34","E4","E5","E43","E44", "E6","E7","E8","E9","E10",
                          "E11","E12","E13","E14","E15","E16","E36","E37","E17","E18","E19",
                          "E21","E22","E38", "E23","E24", "E25","E26","E39", "E27",
                          "E40","E41","E42"), 
                        .Names =c("What was your labor market situation before moving to Switzerland in [A6]? Were you.",
                                  "Before moving to Switzerland in [A6], did you ever work abroad (Abroad means in another country than Switzerland)?",
                                  "Before moving to Switzerland in [A6], did you work as a cross-border worker in Switzerland?",
                                  "What was your occupational status before moving to Switzerland in [A6]? If you had several jobs, please answer with reference to your main occupation. Were...",
                                  "What sector of business or industry was your company or institution active in for the most part?",
                                  
                                  "Just before moving to Switzerland in [A6], were you actively looking for a new job in the labor market, either in Switzerland or abroad?",
                                  "Please indicate the situation that best describes your job search just before moving to Switzerland in [A6]",
                                  "What were the main reasons explaining that you were looking for a job in Switzerland? ",
                                  
                                  
                                  "Did you have a job contract or a job offer in Switzerland before you immigrated to Switzerland in [A6]?",
                                  "Was it a transfer within the same company?",
                                  "Under what type of work contract did you work when arriving in Switzerland?",
                                  "If you had been given the choice, which type of work contract would you have preferred when coming to Switzerland?",
                                  "Before moving to Switzerland in [A6], in which countries did you look for a job? Did you search in.",
                                  "How did you go about looking for a job in Switzerland? Did you.",
                                  "Have you had any of the following problems or difficulties when looking for a job in Switzerland?",
                                  "Sometimes people receive help when looking for job. From whom did you receive assistance? Was it from.",
                                  "Once you arrived in Switzerland in [A6], how long did you spend looking for a job before finding one?",
                                  "What was your labor market situation once you arrived in Switzerland in [A6]?",
                                  "What was your occupational status once you arrived in Switzerland in [A6]? If you had several jobs, please answer with reference to your main occupation. Were you.",
                                  "Once you arrived in Switzerland in [A6], what sector of business, or industry was your company or institution active in for the most part? ",
                                  "In total, how many years have you been in paid work in your whole life?",
                                  "Do you still have the same job and position as when you arrived in Switzerland in [A6]?",
                                  "What is your current labor market situation? Are you.",
                                  "How long have you been looking for a job in Switzerland?",
                                  "Among the following statements, which best describes your situation regarding your job search in Switzerland?",
                                  "In what year did you start your current job?",
                                  "What is your current occupational status? If you have several jobs, please answer with reference to your main occupation. Are you.",
                                  "What sector of business, or industry is your company or institution active in for the most part?",
                                  "Do you have a work contract of.",
                                  "In which locality is the company that you work for?",
                                  "Do you think that your current level of education is appropriate for your current job?",
                                  "What type of education do you feel is most appropriate for your current job?",
                                  "What are the reasons for you currently occupying a job that does not correspond to your educational level?",
                                  "On a scale from 0 'not at all' to 7 'to a very high extent', to what extent are your knowledge and overall skills utilized in your current work? By knowledge and overall skills we mean your formal education as well as the skills you obtained while working (on-the-job training).",
                                  "What are the reasons for you currently occupying a job that does not utilize your knowledge and overall skills?",
                                  "On a scale from 0 'not satisfied at all' to 7 'fully satisfied', what is your level of satisfaction with your current occupation?",
                                  "Regarding your professional situation, what would you say overall when comparing your situation today with your situation before moving to Switzerland in [A6]? It has.",
                                  "Have you had business activities in the past 12 months with people or institutions in countries other than Switzerland?",
                                  "With which countries do you have such business activities?",
                                  "What kinds of business activities have you had in the past 12 months with people and institutions in countries other than Switzerland?"))


# QUESTION F SURVEY #####
FQUESTIONS <- structure(c("F17","F18","F19", "F1","F2","F3", "F4","F6","F7","F8","F10", "F11", "F12", "F13",
                          "F24","F25","F26","F27","F28","F29"), 
                        .Names =c("Where does your spouse/partner live? If he/she lives in several places, please consider his/her main place of residence.",
                                  "Which is, among the following proposals, the one which fits the best with your current situation?",
                                  "What are the reasons explaining why your spouse/partner lives abroad?",
                                  "Including yourself, how many people live in your household? Please consider all persons who have their principal and registered residence in the same dwelling as you.",
                                  "In what country does your spouse/partner currently live?",
                                  "Does your spouse/partner live in the same household?",
                                  "In which country was your spouse/partner born?",
                                  "What is the highest level of education your spouse/partner has successfully completed?",
                                  "What is your spouse/partner's current labor market situation? Is he/she.",
                                  "How many children do you have? Please also consider children who do not live in the same household as you.",
                                  "Where do your children live?",
                                  "Do you get regular help with childcare from one or more of the following providers?",
                                  "What type of school currently attend?",
                                  "In what language mainly taught at school?",
                                  "When you arrived in Switzerland in [A6], what was your first type of housing?",
                                  "Did you move since then?",
                                  "What kind of housing do you live in now?",
                                  "Do you live since your arrival in Switzerland in the same municipality?",
                                  "Are you satisfied with your move to the municipality you currently live in?",
                                  "Are you satisfied to have chosen to live in your current municipality?"))




# QUESTION G SURVEY #####
GQUESTIONS <- structure(c("G1","G2","G3","G4","G5","G20","G6","G21","G22",
                          "G7","G8","G23","G9", "G10","G11","G12","G13",
                          "G14","G14B","G15","G16","G17","G17B","G18","G19","G26","G27","G28"), 
                        .Names =c("Which language do you relate to and master best?",
                                  "How well do you understand the local language? By local language we mean the language that is spoken in the commune where you live.",
                                  "How well do you speak the local language? By local language we mean the language that is spoken in the commune where you live.",
                                  "Do you have relatives living in Switzerland?",
                                  "How are they related to you?",
                                  "Do you have friends or relatives living in Switzerland you can count on for practical or emotional support (for instance doing the shopping for you when sick, taking you to the doctor or giving useful advice in case of problems)?",
                                  
                                  "Where do your very good friends live?",
                                  "Among the following statements, which best describes your situation concerning your friends here in Switzerland?",
                                  "How often do you have contacts with friends or family in your country of origin by telephone, email, skype, whatsapp or other forms of online communications?",
                                  "Have you visited your country of origin since you have arrived in Switzerland?",
                                  "How often have you visited your country of origin since you have arrived in Switzerland?",
                                  "Is there a place (family house, apartment) in your country of origin that you own or rent and where you can go when you stay there?",
                                  "When you visit your country of origin [B1], do you mainly stay.",
                                  "How at home do you feel on your visits to your home country of origin [B1]?",
                                  "Since your arrival in Switzerland, have you ever been visited by family of friends from your home country of origin [B1]?",
                                  "Do you have a place of residence abroad outside of Switzerland where you live for three or more months per year?",
                                  "On a scale from 0 ('not at all') to 7 ('to a very high extent'), to what extent are you interested in news and current events in.",
                                  "Have you engaged in voluntary work for one of the following associations or charity organizations during the last 12 months in Switzerland?",
                                  "Have you engaged in voluntary work for one of the following associations or charity organizations during the last 12 months in your country?",
                                  "If there was a general election tomorrow in Switzerland and you had the right to vote would you vote?",
                                  "How interested would you say you are in politics in ...",
                                  "There are different ways of trying to improve things or help prevent things from going wrong. During the last 12 months, have you done any of the following? In Switzerland",
                                  "There are different ways of trying to improve things or help prevent things from going wrong. During the last 12 months, have you done any of the following? In your country",
                                  "On a scale from 0 (not at all confident) to 10 (completely confident), how confident are you in your own ability to participate in politics in your home country [B1]?",
                                  "On a scale from 0 (not at all easy) to 10 (extremely easy), how easy do you personally find it to take part in politics ...?",
                                  "On a scale from 0 (not problematic at all) to 7 (very problematic), how problematic are the following aspects of your life in Switzerland?",
                                  "To what extent do you agree with the following statements?",
                                  "How is your health in general?"))

# QUESTION H SURVEY #####
HQUESTIONS <- structure(c("H1","H2","H3","H4","H5","H6","H7","H8", "H9","H12","H10",
                          "H11","H13", "H14","H15","H16" ), 
                        .Names =c("Is your stay in Switzerland limited in time?",
                                  "How many more years would like to stay in Switzerland?",
                                  "How often have you considered emigrating from Switzerland in the last three months? Was it.",
                                  "Do you plan to emigrate from Switzerland within the next 12 months?",
                                  "To what country do you intend to move at the end of your stay in Switzerland?",
                                  "On a scale from 0 (not at all satisfied) to 10 (completely satisfied) can you indicate your degree of satisfaction for each of the following points?",
                                  "Discrimination means that a person is treated less favorably than other people because of different characteristics. Have you experienced situations of prejudice or discrimination in Switzerland in the last 24 months",
                                  "In your opinion, on the basis of what actual or assigned characteristic(s) has this discrimination taken place? Because of.",
                                  "Where did you experience this discrimination? Was it.",
                                  "To deal with the discrimination of foreigners in the labour market, some people slightly modify their curriculum vitae to hide their origin. Did you use one of the following strategies when writing your curriculum vitae in order to increase the possibility of success, when looking for a job in Switzerland?",
                                  "Considering all of your income and your expenses over a year, would you say you currently.",
                                  "On a scale from 0 (no feeling of attachment) to 7 (strong feeling of attachment), to what extent do you have a feeling of attachment.",
                                  "How often have you positive contacts with Swiss people? This could be on public transport, in the street, in shops or in the neighbourhood.",
                                  "How often have you negative contacts with Swiss people? This could be on public transport, in the street, in shops or in the neighbourhood.",
                                  "When arriving in Switzerland, did you have any plans regarding the duration of your stay?",
                                  "And now, what are your plans regarding the duration of your stay in Switzerland?"))





# QUESTION k covid19 SURVEY #####
KQUESTIONS <- structure(c("K1","K2","K3","K4","K5","K6","K7","K8", "K9"), 
                        .Names =c("During the confinement period beginning in March 2020, where did you spend most of your time (working and free time)?",
                                  "Which statement best describes how you felt at that moment?",
                                  "Which impact did the partial lockdown in spring have on your professional situation in Switzerland?",
                                  "Which impact did the partial lockdown have on your professional situation in Switzerland?",
                                  "Which statement best fits how you felt about your residence status during that period?",
                                  "Did you experience discriminatory or unfair treatment based on your nationality in relation to the Covid-19 outbreak?",
                                  "Did you experience support or empathy from the population living in Switzerland because of how your country of origin was affected by the Covid-19 outbreak?",
                                  "Did the Covid-19 outbreak affect your plans regarding your stay in Switzerland?",
                                  "Did the Covid-19 outbreak affect your plans regarding naturalization?"))









# SIDE CONTROLS ####
magnitude <- structure(c("n_nw", "weight"), 
                       .Names = c("Non weighted", "Weighted"))

magnitude2 <- structure(c("Absolute", "Relative"), 
                        .Names = c("Absolute", "Relative"))

sex <- structure(c(1, 2), 
                 .Names = c("Males", "Female"))


####### SHINY UI #####
# !diagnostics off
shinyUI(fluidPage(
  
  #tags$head(includeScript("google_analytics.js")),
  navbarPage(div(img(src="NCCR_LOGO.png",height = 40, width = 110),
                 title="NCCR ON THE MOVE WEBSITE", href="https://nccr-onthemove.ch/"),
             windowTitle="Migration and mobility Survey", 
             theme = shinytheme("spacelab"),
             
             
             tabPanel(h5(span("Presentation")),
                      tags$head(
                        tags$style(type = 'text/css', 
                                   HTML('.navbar { background-color: red;}
                          .navbar-default .navbar-brand{color: white;}
                          .tab-panel{ background-color: red; color: white}
                          .navbar-default .navbar-nav > .active > a, 
                           .navbar-default .navbar-nav > .active > a:focus, 
                           .navbar-default .navbar-nav > .active > a:hover {
                                color: #555;
                                background-color: #c1c1c1;
                            }')
                        )),
                      
                      
                      
                      mainPanel(
                        
                        h5(strong("The Migration-Mobility Survey")),
                        
                        p(style="text-align: justify;","The Migration-Mobility Survey (MMS) is conducted every two years with the aim of providing 
                            valuable data for improving the reception and integration of foreign nationals 
                            in Switzerland. The first round of the survey was conducted in fall 2016 on a 
                            population of persons born abroad and with a foreign citizenship who have moved
                            to Switzerland in the last ten years. To be eligible for the survey, respondents 
                            had to be between the age of 24 and 64 at the time of the survey, and at least 18 
                            years old when they arrived in Switzerland. A total of 5'973 people replied to the 
                            questionnaire. The second wave of the MMS was organized in fall 2018 and 7'740 
                            foreigners answered the questionnaire. The last wave of the MMS was conducted in 2020 and
                          7'393 people replied to the questionnaire. Given the uniqueness of the year in relation to
                          the pandemic, a new section was included with a set of questions on the impact of COVID-19 on different dimensions of the lives of those surveyed." ),      
                        p(style="text-align: justify;","By sharing their experience in Switzerland, respondents help us raise awareness 
                            and contribute to a better understanding of the migration process."),
                        
                        h5(strong("Topics covered by the survey")),
                        p(style="text-align: justify;","The participants were asked about issues such as the migration experience, 
                            employment, family configuration, social contacts and links with their 
                            countries of origin, as well as their living conditions in Switzerland. 
                            The 2018 questionnaire provided the same indicators as in 2016, in order 
                            to allow for longitudinal analysis of migration-related issues. In addition, 
                            it included specific questions referring to the current research projects of 
                            the nccr - on the move."),
                        
                        h5(strong("About this platform")),
                        p(style="text-align: justify;","The aim of this platform is to provide the NCCR members, and general audience, 
                          an easy way to access, visualize and download the results of the MMS. 
                          Users can explore different topics covered by the survey and access the 
                          descriptive results for each question. The information can be access in both, 
                          absolute and relative terms, and over the basis of weighted or not weighted 
                          data. Additionally, in the ", strong("Crosstables section,"),"users can define the variables
                            to explore and download the resulting outcomes."),
                        
                        p(style="text-align: justify;","This platform grants access to",strong("140 questions"),
                          "of the MMS, more than ", strong("500 interactive and downloadable graphs"),
                          "and more than", strong("1000 downloadable tables.")," This is an online tool 
                            for facilitating the knowledge-transfer provided by the NCCR On the Move 
                            which aims to complement the already established", a(strong("NCCR Mobility-Migration indicators,"),
                                                                                 href=("https://indicators.nccr-onthemove.ch/")),
                          " a series of commented interactive visualizations based on the same survey data."),
                        
                        h5(strong("Elaboration")),
                        # br(),
                        p("Survey coordinator: ",a(strong("Philippe Wanner"),
                                                   href="https://nccr-onthemove.ch/who-is-who/people/?start=w&p_id=156")),
                        
                        p("Field work: ", a(strong("IDESO"),
                                            href="https://www.unige.ch/sciences-societe/ideso/"),
                          "and ",a(strong("LINK Institute"),
                                   href="https://www.link.ch/en/")),
                        
                        p("Platform: ", a(strong("Juan Galeano"),
                                          href="https://nccr-onthemove.ch/who-is-who/people/?start=g&p_id=9211")),
                        
                        h5(strong("Suggested citation:")),
                        p(a(strong("nccr - on the move, Migration-Mobility Survey. Neuchatel: nccr - on the move, 2020."),
                            href="https://nccr-onthemove.ch/research/migration-mobility-survey/")),
                        
                        h5(strong("Codebook:")),
                        p(style="text-align: justify;","The questionnaire of the Migration and Mobility Survey is composed by more than 140 different questions grouped under 7 items. The codebook of the MMS can be accessed ", a(strong("HERE."),
                                                                                                                                                                                                       href="https://www.nccr-onthemove.ch/DataManagement/Codebooks/MigrationMobilitySurvey/MM_Survey.pdf"),"If the file does not display automatically, just resfresh the page."),
                        
                        
                        h5(strong("Sampling")),
                        p(style="text-align: justify;","Eleven countries or regions were defined for sampling purposes."),
                        
                        htmlOutput("sampling"),
                        
                        br(),
                        
                        h5(strong("Acknowledgements")),
                        
                        p(style="text-align: justify;","This application has been developed using the web application framework , ", a("Shiny", href="http://shiny.rstudio.com/"), "for ", 
                          a("the R language", href="https://www.r-project.org/"),". For designing of the interface we use packages ",
                          a("shinythemes", href="https://cran.r-project.org/web/packages/shinythemes/index.html"),", ",
                          a("shinyWidgets", href="https://cran.r-project.org/web/packages/shinyWidgets/index.html"),", ",
                          a("knitr", href="https://cran.r-project.org/web/packages/knitr/index.html"),"and",
                          a("kableExtra", href="https://cran.r-project.org/web/packages/kableExtra/index.html"),
                          
                          
                          " and for the different plots and maps the packages ", 
                          a("Leaflet", href="https://cran.r-project.org/web/packages/leaflet/index.html"),"and ", 
                          a("highcharter",href="https://cran.r-project.org/web/packages/highcharter/index.html"), 
                          ". In addition, for the manipulation of statistical data we used the packages ",
                          
                          a("dplyr",href="https://cran.r-project.org/web/packages/dplyr/index.html"),", ", 
                          a("tidyr",href="https://cran.r-project.org/web/packages/tidyr/index.html" ),", ", 
                          a("forcats", href="https://cran.r-project.org/web/packages/forcats/index.html"),"and ",
                          a("naniar", href="https://cran.r-project.org/web/packages/naniar/index.html"),". 
                          We sincerely appreciate the contribution of all those involved in the development of the R community."),
                        
                        p(strong("Bugs reporting: "),"Juan.Galeano@unige.ch"),
                        
                        
                      )
                      
             ),
             
             
             ###### 1 -Screening #####                 
             
             ##### SURVEY #####
             navbarMenu(h5("Migration and Mobility Survey"),
             tabPanel(h5(span("Screening")),
                      tags$head(
                        tags$style(type = 'text/css', 
                                   HTML('.navbar { background-color: red;}
                          .navbar-default .navbar-brand{color: white;}
                          .tab-panel{ background-color: red; color: white}
                          .navbar-default .navbar-nav > .active > a, 
                           .navbar-default .navbar-nav > .active > a:focus, 
                           .navbar-default .navbar-nav > .active > a:hover {
                                color: #555;
                                background-color: #c1c1c1;
                            }')
                        )),
                      
                      sidebarPanel(
                        
                        selectizeInput("terms", "Choose a question:", 
                                       choices=AQUESTIONS, selected="A1"),
                        
                        conditionalPanel(
                          condition = "input.terms == 'A1'||input.terms == 'A3'||input.terms == 'A4_1'",
                          selectInput(
                            "year", "Year",
                            c(2016,2018,2020))),
                        
                        conditionalPanel(
                          condition = "input.terms == 'A5_1'",
                          selectInput(
                            "year1", "Year",
                            c(2016,2018))),
                        
                        selectizeInput("mag", "Weighted or non weighted data:", 
                                       choices=magnitude, selected="weight"),
                        
                        selectizeInput("mag2", "Select a magnitude:", 
                                       choices=magnitude2, selected="Relative"),
                        
                        #checkboxInput("smooth", "Smooth"),
                        #conditionalPanel(
                        #   condition = "input.terms == A3",
                        #pickerInput("sex","Sex",
                        #           choices=sex, 
                        #          selected=1:2,
                        #         options = list(`actions-box` = TRUE),
                        #        multiple = T),
                        #),
                        
                        #pickerInput("sex","Sex",choices=sex, selected=1:2,options = list(`actions-box` = TRUE),multiple = T),
                        
                        #radioButtons("filetype", strong("Download data associated to this question, choose a file extension:"),
                                    # choices = c("csv", "txt")),
                        #downloadButton('downloadDataPy', 'Download data'),
                        #br(),
                        #br(),
                        p(strong("Downloads: "),"Charts and associated data can be downloaded from the 'Chart context' menu at the upper-right corner of each plot."),
                        
                        
                        p(strong("Elaboration:")),
                        # br(),
                        p("Survey coordinator: ",a(strong("Philippe Wanner"),
                                                   href="https://nccr-onthemove.ch/who-is-who/people/?start=w&p_id=156")),
                        
                        p("Field work: ", a(strong("IDESO"),
                                            href="https://www.unige.ch/sciences-societe/ideso/"),
                          "and ",a(strong("LINK Institute"),
                                   href="https://www.link.ch/en/")),
                        
                        p("Platform: ", a(strong("Juan Galeano"),
                                          href="https://nccr-onthemove.ch/who-is-who/people/?start=g&p_id=9211")),
                        
                        p(strong("Suggested citation:")),
                        p(a(strong("nccr - on the move, Migration-Mobility Survey. Neuchatel: nccr - on the move, 2020."),
                            href="https://nccr-onthemove.ch/research/migration-mobility-survey/"))   
                        
                        
                      ),# este cierra sidebarPanel
                      mainPanel(align="center",
                                
                                conditionalPanel(
                                  condition = "input.terms == 'A1'",
                                  splitLayout(cellWidths = c("75%"),
                                              highchartOutput("pyramidrel", height = 450)),
                                  
                                  splitLayout(cellWidths = c("48.5%", "48.5%"),
                                              htmlOutput("mtcars_kable"),
                                              htmlOutput("mtcars_kable2"))),
                                
                                conditionalPanel(
                                  condition = "input.terms == 'A3'",
                                  splitLayout(cellWidths = c("100%"),
                                              highchartOutput("barras1", height = 450)),
                                  
                                  splitLayout(cellWidths = c("48.5%", "48.5%"),
                                              htmlOutput("nacionalidad16"),
                                              htmlOutput("nacionalidad18"))),
                                
                                conditionalPanel(
                                  condition = "input.terms == 'A4_1'",
                                  splitLayout(cellWidths = c("100%"),
                                              highchartOutput("barras3", height = 450)),
                                  
                                  splitLayout(cellWidths = c("48.5%", "48.5%"),
                                              htmlOutput("nacionalidad16B"),
                                              htmlOutput("nacionalidad18B"))),
                                
                                conditionalPanel(
                                  condition = "input.terms == 'A5_1'",
                                  splitLayout(cellWidths = c("100%"),
                                              highchartOutput("barras5", height = 450)),
                                  
                                  splitLayout(cellWidths = c("48.5%", "48.5%"),
                                              htmlOutput("nacionalidad16C"),
                                              htmlOutput("nacionalidad18C"))),
                                
                                conditionalPanel(
                                  condition = "input.terms == 'A6'",
                                  splitLayout(cellWidths = c("100%"),
                                              highchartOutput("barras7", height = 450)),
                                  
                                  splitLayout(cellWidths = c("48.5%", "48.5%"),
                                              htmlOutput("mtcars_kable9"),
                                              htmlOutput("mtcars_kable10"))),
                                
                                conditionalPanel(
                                  condition = "input.terms == 'A7'",
                                  splitLayout(cellWidths = c("100%"),
                                              highchartOutput("A7_16", height = 450)),
                                  
                                  splitLayout(cellWidths = c("48.5%", "48.5%"),
                                              htmlOutput("TA7_16"),
                                              htmlOutput("TA7_18"))),
                                
                                
                                conditionalPanel(
                                  condition = "input.terms == 'A8'",
                                  splitLayout(cellWidths = c("100%"),
                                              highchartOutput("barras9", height = 450)),
                                  
                                  splitLayout(cellWidths = c("48.5%", "48.5%"),
                                              htmlOutput("mtcars_kable11"),
                                              htmlOutput("mtcars_kable12"))),
                                
                                conditionalPanel(
                                  condition = "input.terms == 'A9'",
                                 
                                  
                                  splitLayout(cellWidths = c("100%"),
                                              highchartOutput("ComposicionPlotly20",height = 450)),
                                  br(),
                                  p(strong("NOTE: ",style="color:#e30513"),"This question was answered by 
                                    those who declared a marital status other than 
                                    married in previous question. Barplot reflects positive answers (yes)
                                    for each marital status.")),
                                
                                conditionalPanel(
                                  condition = "input.terms == 'A10'",
                                  
                                  
                                  splitLayout(cellWidths = c("48.5%", "48.5%"),
                                              leafletOutput("A10_16",width = "100%", height = "650px"),
                                              leafletOutput("A10_18",width = "100%", height = "650px")),
                                  
                                  splitLayout(cellWidths = c("48.5%", "48.5%"),
                                              htmlOutput("TA10_16"),
                                              htmlOutput("TA10_18"))),
                                
                                
                                conditionalPanel(
                                  condition = "input.terms == 'A11'",
                                  splitLayout(cellWidths = c("100%"),
                                             
                                              highchartOutput("A11_18", height = 450)),
                                  
                                  splitLayout(cellWidths = c("48.5%", "48.5%"),
                                              htmlOutput("TA11_16"),
                                              htmlOutput("TA11_18"))),
                                
                                conditionalPanel(
                                  condition = "input.terms == 'A12'",
                                  splitLayout(cellWidths = c("48.5%", "48.5%"),
                                              htmlOutput("A12_16"),
                                              highchartOutput("A12_18", height = 300)),
                                  
                                  splitLayout(cellWidths = c("48.5%", "48.5%"),
                                              htmlOutput("TA12_16"),
                                              htmlOutput("TA12_18")))
                                
                      )
                      
                      
             ),
             # ), # este cierra navbarMenu
             
             
             
             ###### 2 Migratory history #####                 
             #navbarMenu(h5("Migratory history"),
             
             ##### SURVEY #####
             tabPanel(h5(span("Migratory history")),
                      sidebarPanel(
                        
                        selectInput("BQ", "Choose a question:", 
                                    choices=BQUESTIONS, selected="B1"),
                        
                        
                        conditionalPanel(
                          condition = "input.BQ == 'B1'",
                          selectInput(
                            "year3", "Year",
                            c(2016,2018,2020))),
                        
                        selectizeInput("mag1", "Weighted or non weighted data:", 
                                       choices=magnitude, selected="weight"),
                        
                        selectizeInput("mag3", "Select a magnitude:", 
                                       choices=magnitude2, selected="Relative"),
                        
                        p(strong("Downloads: "),"Charts and associated data can be downloaded from the 'Chart context' menu at the upper-right corner of each plot."),
                        
                        p(strong("Elaboration:")),
                        # br(),
                        p("Survey coordinator: ",a(strong("Philippe Wanner"),
                                                   href="https://nccr-onthemove.ch/who-is-who/people/?start=w&p_id=156")),
                        
                        p("Field work: ", a(strong("IDESO"),
                                            href="https://www.unige.ch/sciences-societe/ideso/"),
                          "and ",a(strong("LINK Institute"),
                                   href="https://www.link.ch/en/")),
                        
                        p("Platform: ", a(strong("Juan Galeano"),
                                          href="https://nccr-onthemove.ch/who-is-who/people/?start=g&p_id=9211")),
                        
                        p(strong("Suggested citation:")),
                        p(a(strong("nccr - on the move, Migration-Mobility Survey. Neuchatel: nccr - on the move, 2020."),
                            href="https://nccr-onthemove.ch/research/migration-mobility-survey/"))
                        
                      ),
                      mainPanel(align="center", #####
                                
                                conditionalPanel(
                                  condition = "input.BQ == 'B1'",
                                  splitLayout(cellWidths = c("100%"),
                                              highchartOutput("B1_16", height = 450)),
                                  
                                  splitLayout(cellWidths = c("48.5%", "48.5%"),
                                              htmlOutput("TB1_16"),
                                              htmlOutput("TB1_18"))),
                                
                                conditionalPanel(
                                  condition = "input.BQ == 'B16'",
                                  splitLayout(cellWidths = c("48.5%", "48.5%"),
                                              highchartOutput("B16_16", height = 450),
                                              highchartOutput("B16_18", height = 450)),
                                  
                                  splitLayout(cellWidths = c("48.5%", "48.5%"),
                                              htmlOutput("TB16_16"),
                                              htmlOutput("TB16_18"))),
                                
                                conditionalPanel(
                                  condition = "input.BQ == 'B17'",
                                  splitLayout(cellWidths = c("100%"),
                                              highchartOutput("B17_18", height = 450)),
                                  
                                  splitLayout(cellWidths = c("48.5%", "48.5%"),
                                              htmlOutput("TB17_16"),
                                              htmlOutput("TB17_18"))),
                                
                                conditionalPanel(
                                  condition = "input.BQ == 'B18'",
                                  splitLayout(cellWidths = c("100%"),
                                              highchartOutput("B18_1_18", height = 450)),
                                  br(),
                                  p(strong("NOTE: ",style="color:#e30513"),"Multyiple answer question. 
                                  Bars reflects positive answers (yes)
                                    for each item.")),
                                
                                conditionalPanel(
                                  condition = "input.BQ == 'B2'",
                                  splitLayout(cellWidths = c("100%"),
                                              highchartOutput("B2_16", height = 450))),
                                
                                conditionalPanel(
                                  condition = "input.BQ == 'B4'",
                                  splitLayout(cellWidths = c("100%"),
                                              highchartOutput("B4_16", height = 450))),
                                
                                conditionalPanel(
                                  condition = "input.BQ == 'B5'",
                                  
                                  splitLayout(cellWidths = c("48.5%", "48.5%"),
                                              highchartOutput("B5_16", height = 450),
                                              highchartOutput("B5_18", height = 450)),
                                  br(),
                                  p(strong("NOTE: ",style="color:#e30513"),"Respondents to this question are those who 
                                    answered 'no' to previous question.")),
                                
                                conditionalPanel(
                                  condition = "input.BQ == 'B6'",
                                  
                                  splitLayout(cellWidths = c("48.5%", "48.5%"),
                                              highchartOutput("B6_16", height = 200),
                                              htmlOutput("B6_18"))),
                                
                                conditionalPanel(
                                  condition = "input.BQ == 'B8_1'",
                                  
                                  splitLayout(cellWidths = c("100%"),
                                              highchartOutput("B8_1_16", height = 450)),
                                  p(strong("NOTE: ",style="color:#e30513"),"Multiple answer question. 
                                    Bars represent positive answers for each item."),
                                  br()),
                                
                                conditionalPanel(
                                  condition = "input.BQ == 'B20'",
                                  
                                  splitLayout(cellWidths = c("48.5%", "48.5%"),
                                              htmlOutput("B20_1_16"),
                                              highchartOutput("B20_1_18",height = 300)),
                                  
                                  splitLayout(cellWidths = c("48.5%", "48.5%"),
                                              htmlOutput("B20_2_16"),
                                              highchartOutput("B20_2_18",height = 300)),
                                  
                                  splitLayout(cellWidths = c("48.5%", "48.5%"),
                                              htmlOutput("B20_3_16"),
                                              highchartOutput("B20_3_18",height = 300)),
                                  
                                  splitLayout(cellWidths = c("48.5%", "48.5%"),
                                              htmlOutput("B20_4_16"),
                                              highchartOutput("B20_4_18",height = 300)),
                                  
                                  splitLayout(cellWidths = c("48.5%", "48.5%"),
                                              htmlOutput("B20_5_16"),
                                              highchartOutput("B20_5_18",height = 300)),
                                  
                                  splitLayout(cellWidths = c("48.5%", "48.5%"),
                                              htmlOutput("B20_6_16"),
                                              highchartOutput("B20_6_18",height = 300)),
                                  
                                  splitLayout(cellWidths = c("48.5%", "48.5%"),
                                              htmlOutput("B20_7_16"),
                                              highchartOutput("B20_7_18",height = 300)),
                                  
                                  splitLayout(cellWidths = c("48.5%", "48.5%"),
                                              htmlOutput("B20_8_16"),
                                              highchartOutput("B20_8_18",height = 300)),
                                  
                                  splitLayout(cellWidths = c("48.5%", "48.5%"),
                                              htmlOutput("TB20_16"),
                                              htmlOutput("TB20_18"))),
                                
                                conditionalPanel(
                                  condition = "input.BQ == 'B9'",
                                  splitLayout(cellWidths = c("100%"),
                                              highchartOutput("B9_16", height = 450))),
                                
                                conditionalPanel(
                                  condition = "input.BQ == 'B10'",
                                  
                                  splitLayout(cellWidths = c("100%"),
                                              highchartOutput("B10_16", height = 450)),
                                  p(strong("NOTE: ",style="color:#e30513"),"Respondents to this question are those who answered 'yes' to previous question."),
                                  br()),
                                
                                conditionalPanel(
                                  condition = "input.BQ == 'B11'",
                                  
                                  splitLayout(cellWidths = c("100%"),
                                              highchartOutput("B11_1_16", height = 450))),
                                
                                conditionalPanel(
                                  condition = "input.BQ == 'B12'",
                                  
                                  splitLayout(cellWidths = c("48.5%", "48.5%"),
                                              highchartOutput("B12_16", height = 450),
                                              htmlOutput("B12_18")),
                                  
                                  splitLayout(cellWidths = c("48.5%", "48.5%"),
                                              htmlOutput("TB12_16"),
                                              htmlOutput("TB12_18"))),
                                
                                conditionalPanel(
                                  condition = "input.BQ == 'B13'",
                                  
                                  splitLayout(cellWidths = c("100%"),
                                              highchartOutput("B13_1_16", height = 450)),
                                  br(),
                                  p(strong("NOTE: ",style="color:#e30513"),"Multiple answer question. 
                                    Bars represent positive answers for each item."),
                                  splitLayout(cellWidths = c("100%"),
                                             # htmlOutput("TB12_16"),
                                              htmlOutput("TB13_16"))
                                  
                                  ),
                                
                                conditionalPanel(
                                  condition = "input.BQ == 'B14'",
                                  
                                  splitLayout(cellWidths = c("100%"),
                                              highchartOutput("B14_1_16", height = 450)),
                                  br(),
                                  p(strong("NOTE: ",style="color:#e30513"),"Respondents to this question are those who received any support when arriving to Switzerland")),
                                
                                conditionalPanel(
                                  condition = "input.BQ == 'B21'",
                                  
                                  splitLayout(cellWidths = c("100%"),
                                              
                                              highchartOutput("B21_1_18", height = 450))),
                                
                                conditionalPanel(
                                  condition = "input.BQ == 'B22'",
                                  
                                  splitLayout(cellWidths = c("100%"),
                                              
                                              highchartOutput("B22_1_18", height = 450))),
                                
                                conditionalPanel(
                                  condition = "input.BQ == 'B23'",
                                  
                                  splitLayout(cellWidths = c("100%"),
                                              
                                              highchartOutput("B23_1_18", height = 450))),
                                
                                conditionalPanel(
                                  condition = "input.BQ == 'B24'",
                                  
                                  splitLayout(cellWidths = c("100%"),
                                              
                                              highchartOutput("B24_1_18", height = 450))),
                                
                                conditionalPanel(
                                  condition = "input.BQ == 'B25'",
                                  
                                  splitLayout(cellWidths = c("100%"),
                                              
                                              highchartOutput("B25_1_18", height = 450))),
                                
                                conditionalPanel(
                                  condition = "input.BQ == 'B27'",
                                  
                                  splitLayout(cellWidths = c("100%"),
                                              
                                              highchartOutput("B27_1_18", height = 450))),
                                conditionalPanel(
                                  condition = "input.BQ == 'B15'",
                                  splitLayout(cellWidths = c("100%"),
                                              highchartOutput("B15_1_16", height = 200)),
                                  
                                  splitLayout(cellWidths = c("100%"),
                                              highchartOutput("B15_2_16", height = 200)),
                                  
                                  splitLayout(cellWidths = c("100%"),
                                              highchartOutput("B15_3_16", height = 200)),
                                  
                                  splitLayout(cellWidths = c("100%"),
                                              highchartOutput("B15_4_16", height = 200)),
                                  
                                  splitLayout(cellWidths = c("100%"),
                                              highchartOutput("B15_5_16", height = 200)),
                                  
                                  splitLayout(cellWidths = c("100%"),
                                              highchartOutput("B15_6_16", height = 200)),
                                  
                                  splitLayout(cellWidths = c("100%"),
                                               highchartOutput("B15_7_16", height = 200)),
                                  
                                  splitLayout(cellWidths = c("100%"),
                                              highchartOutput("B15_8_16", height = 200)))
                                
                      )
                      
                      
                      
             ),
             # ), # este cierra navbarMenu
             
             ###### 3 Citizenship #####                 
             # navbarMenu(h5("Citizenship"),
             
             ##### SURVEY #####
             
             tabPanel(h5(span("Citizenship")),
                      sidebarPanel(
                        
                        selectInput("BC", "Choose a question:", 
                                    choices=CQUESTIONS, selected="B1"),
                        
                        selectizeInput("magC", "Weighted or non weighted data:", 
                                       choices=magnitude, selected="weight"),
                        
                        selectizeInput("magC2", "Select a magnitude:", 
                                       choices=magnitude2, selected="Relative"),
                        
                        p(strong("Downloads: "),"Charts and associated data can be downloaded from the 'Chart context' menu at the upper-right corner of each plot."),
                        
                        p(strong("Elaboration:")),
                        # br(),
                        p("Survey coordinator: ",a(strong("Philippe Wanner"),
                                                   href="https://nccr-onthemove.ch/who-is-who/people/?start=w&p_id=156")),
                        
                        p("Field work: ", a(strong("IDESO"),
                                            href="https://www.unige.ch/sciences-societe/ideso/"),
                          "and ",a(strong("LINK Institute"),
                                   href="https://www.link.ch/en/")),
                        
                        p("Platform: ", a(strong("Juan Galeano"),
                                          href="https://nccr-onthemove.ch/who-is-who/people/?start=g&p_id=9211")),
                        
                        p(strong("Suggested citation:")),
                        p(a(strong("nccr - on the move, Migration-Mobility Survey. Neuchatel: nccr - on the move, 2020."),
                            href="https://nccr-onthemove.ch/research/migration-mobility-survey/"))
                        
                      ),
                      mainPanel(align="center",
                                
                                conditionalPanel(
                                  condition = "input.BC == 'C1'",
                                  splitLayout(cellWidths = c("100%"),
                                              highchartOutput("C1_16", height = 450))),
                                
                                conditionalPanel(
                                  condition = "input.BC == 'C2_1'",
                                 
                                  splitLayout(cellWidths = c("100%"),
                                              highchartOutput("C2_1_16", height = 450)),
                                  br(),
                                  p(strong("NOTE: ",style="color:#e30513"),"Respondents to this question are those who answered
                                    'No, probably not' or 'No, certainly not' to previous question. Bars represent
                                    positive answers.")),
                                
                                conditionalPanel(
                                  condition = "input.BC == 'C3'",
                                  
                                  splitLayout(cellWidths = c("100%"),
                                              highchartOutput("C3_1_16", height = 450)),
                                  br(),
                                  p(strong("NOTE: ",style="color:#e30513"),"Respondents to this question are those who answered 
                                    'Yes, probably', 'Yes, certainly' or 'I have already applied 
                                    for the Swiss nationality' to question: 
                                    Do you intend to apply for Swiss nationality in the future?. Bars represent positive answers.")
                                  ))
                      
                      
                      
             ),
             # ), # este cierra navbarMenu# este cierra navbarMenu
             
             ###### 4 Education history and current situation #####                 
             # navbarMenu(h5("Education"),
             
             ##### SURVEY #####
             
             tabPanel(h5(span("Education")),
                      sidebarPanel(
                        
                        selectInput("BD", "Choose a question:", 
                                    choices=DQUESTIONS, selected="D1"),
                        
                        conditionalPanel(
                          condition = "input.BD == 'D2'",
                          selectInput(
                            "yearD", "Year",
                            c(2016,2018,2020))),
                        
                        
                        selectizeInput("magD", "Weighted or non weighted data:", 
                                       choices=magnitude, selected="weight"),
                        
                        selectizeInput("magD2", "Select a magnitude:", 
                                       choices=magnitude2, selected="Relative"),
                        
                        p(strong("Downloads: "),"Charts and associated data can be downloaded from the 'Chart context' menu at the upper-right corner of each plot."),
                        
                        p(strong("Elaboration:")),
                        # br(),
                        p("Survey coordinator: ",a(strong("Philippe Wanner"),
                                                   href="https://nccr-onthemove.ch/who-is-who/people/?start=w&p_id=156")),
                        
                        p("Field work: ", a(strong("IDESO"),
                                            href="https://www.unige.ch/sciences-societe/ideso/"),
                          "and ",a(strong("LINK Institute"),
                                   href="https://www.link.ch/en/")),
                        
                        p("Platform: ", a(strong("Juan Galeano"),
                                          href="https://nccr-onthemove.ch/who-is-who/people/?start=g&p_id=9211")),
                        
                        p(strong("Suggested citation:")),
                        p(a(strong("nccr - on the move, Migration-Mobility Survey. Neuchatel: nccr - on the move, 2020."),
                            href="https://nccr-onthemove.ch/research/migration-mobility-survey/"))
                        
                        
                      ),
                      mainPanel(align="center",
                                
                                conditionalPanel(
                                  condition = "input.BD == 'D1'",
                                  splitLayout(cellWidths = c("100%"),
                                              highchartOutput("D1_16", height = 450))),
                                
                                conditionalPanel(
                                  condition = "input.BD == 'D2'",
                                  splitLayout(cellWidths = c("100%"),
                                              highchartOutput("D2_16", height = 450))),
                                
                                conditionalPanel(
                                  condition = "input.BD == 'D3'",
                                  splitLayout(cellWidths = c("100%"),
                                              highchartOutput("D3_16", height = 450))),
                                
                                conditionalPanel(
                                  condition = "input.BD == 'D4'",
                                  splitLayout(cellWidths = c("100%"),
                                              highchartOutput("D4_16", height = 450))),
                                
                                conditionalPanel(
                                  condition = "input.BD == 'D7'",
                                  splitLayout(cellWidths = c("48.5%", "48.5%"),
                                              highchartOutput("D7_16", height = 450),
                                              htmlOutput("D7_18")),
                                  
                                  splitLayout(cellWidths = c("48.5%", "48.5%"),
                                              htmlOutput("TD7_16"),
                                              htmlOutput("TD7_18")))
                                
                      )
                      
                      
                      
             ),             
             
             
             
             # ), # este cierra navbarMenu
             
             ###### 5 Employment history and current situation #####                 
             # navbarMenu(h5("Employment"),
             
             ##### SURVEY #####
             
             tabPanel(h5(span("Employment")),
                      sidebarPanel(
                        
                        selectizeInput("BE", "Choose a question", 
                                       choices=EQUESTIONS, selected="E1"),
                        
                        selectizeInput("magE", "Weighted or non weighted data:", 
                                       choices=magnitude, selected="weight"),
                        
                        selectizeInput("magE2", "Select a magnitude:", 
                                       choices=magnitude2, selected="Relative"),
                        
                        p(strong("Downloads: "),"Charts and associated data can be downloaded from the 'Chart context' menu at the upper-right corner of each plot."),
                     
                        br(),
                        p(strong("Elaboration:")),
                        # br(),
                        p("Survey coordinator: ",a(strong("Philippe Wanner"),
                                                   href="https://nccr-onthemove.ch/who-is-who/people/?start=w&p_id=156")),
                        
                        p("Field work: ", a(strong("IDESO"),
                                            href="https://www.unige.ch/sciences-societe/ideso/"),
                          "and ",a(strong("LINK Institute"),
                                   href="https://www.link.ch/en/")),
                        
                        p("Platform: ", a(strong("Juan Galeano"),
                                          href="https://nccr-onthemove.ch/who-is-who/people/?start=g&p_id=9211")),
                        
                        p(strong("Suggested citation:")),
                        p(a(strong("nccr - on the move, Migration-Mobility Survey. Neuchatel: nccr - on the move, 2020."),
                            href="https://nccr-onthemove.ch/research/migration-mobility-survey/"))
                        
                        
                      ),# este cierra sidebarPanel
                      mainPanel(align="center",
                                
                                conditionalPanel(
                                  condition = "input.BE == 'E1'",
                                  splitLayout(cellWidths = c("100%"),
                                              highchartOutput("E1_1_16", height = 450)),
                                  br(),
                                  p(strong("NOTE: ",style="color:#e30513"),"Multiple answer question. 
                                    Bars represent positive answers for each item.")),
                                
                                conditionalPanel(
                                  condition = "input.BE == 'E30'",
                                  splitLayout(cellWidths = c("100%"),
                                              highchartOutput("E30_18", height = 450))),
                                
                                conditionalPanel(
                                  condition = "input.BE == 'E31'",
                                  splitLayout(cellWidths = c("100%"),
                                            
                                              highchartOutput("E31_18", height = 450))),
                                
                                conditionalPanel(
                                  condition = "input.BE == 'E2'",
                                  splitLayout(cellWidths = c("100%"),
                                              highchartOutput("E2_16", height = 450))),
                                
                                conditionalPanel(
                                  condition = "input.BE == 'E3'",
                                  splitLayout(cellWidths = c("48.5%", "48.5%"),
                                              highchartOutput("E3_16", height = 450),
                                              htmlOutput("E3_18")),
                                  
                                  splitLayout(cellWidths = c("48.5%", "48.5%"),
                                              htmlOutput("TE3_16"),
                                              htmlOutput("TE3_18"))),
                                
                                
                                
                                conditionalPanel(
                                  condition = "input.BE == 'E32'",
                                  splitLayout(cellWidths = c("100%"),
                                              highchartOutput("E32_18", height = 450))),
                                
                                conditionalPanel(
                                  condition = "input.BE == 'E33'",
                                  splitLayout(cellWidths = c("100%"),
                                             # htmlOutput("E33_16"),
                                              highchartOutput("E33_18", height = 450))),
                                
                                
                                conditionalPanel(
                                  condition = "input.BE == 'E34'",
                                  splitLayout(cellWidths = c("100%"),
                                              
                                              highchartOutput("E34_1_18", height = 450)),
                                  br(),
                                  p(strong("NOTE: ",style="color:#e30513"),"Multiple answer question. 
                                    Bars represent positive answers for each item.")),
                                
                                
                                
                                conditionalPanel(
                                  condition = "input.BE == 'E4'",
                                  splitLayout(cellWidths = c("100%"),
                                              highchartOutput("E4_16", height = 450))),
                                
                                conditionalPanel(
                                  condition = "input.BE == 'E5'",
                                  splitLayout(cellWidths = c("100%"),
                                              highchartOutput("E5_16", height = 450))),
                                
                                conditionalPanel(
                                  condition = "input.BE == 'E43'",
                                  splitLayout(cellWidths = c("100%"),
                                              highchartOutput("E43_20", height = 450))),
                                
                                conditionalPanel(
                                  condition = "input.BE == 'E44'",
                                  splitLayout(cellWidths = c("100%"),
                                              highchartOutput("E44_20", height = 450))),
                                
                                conditionalPanel(
                                  condition = "input.BE == 'E6'",
                                  splitLayout(cellWidths = c("100%"),
                                              highchartOutput("E6_1_16", height = 450)),
                                  br(),
                                  p(strong("NOTE: ",style="color:#e30513"),"Multiple answer question. 
                                    Bars represent positive answers for each item.")),
                                
                                conditionalPanel(
                                  condition = "input.BE == 'E7'",
                                 
                                  splitLayout(cellWidths = c("100%"),
                                              highchartOutput("E7_1_16", height = 450)),
                                  br(),
                                  p(strong("NOTE: ",style="color:#e30513"),"Respondents to this
                                    question are those who answered 'Yes' (Switzerland) to previous question.
                                    Multiple answer question. 
                                    Bars represent positive answers for each item.")),
                                
                                conditionalPanel(
                                  condition = "input.BE == 'E8'",
                                  
                                  splitLayout(cellWidths = c("100%"),
                                              highchartOutput("E8_1_16", height = 450)),
                                  br(),
                                  p(strong("NOTE: ",style="color:#e30513"),"Respondents to this question are 
                                    those who answered 'Yes' (Switzerland) to question: 
                                    Before moving to Switzerland, in which countries did you look for a job?
                                    Multiple answer question. 
                                    Bars represent positive answers for each item."),
                                  splitLayout(cellWidths = c("100%"),
                                              htmlOutput("TE8_16"))),
                                
                                
                                conditionalPanel(
                                  condition = "input.BE == 'E9'",
                                  
                                  splitLayout(cellWidths = c("100%"),
                                              highchartOutput("E9_1_16", height = 450)),
                                  br(),
                                  p(strong("NOTE: ",style="color:#e30513"),"Respondents to this question are 
                                    those who answered 'Yes' (Switzerland) to question: 
                                    Before moving to Switzerland, in which countries did you look for a job?
                                    Multiple answer question. 
                                    Bars represent positive answers for each item."),
                                  splitLayout(cellWidths = c("100%"),
                                              htmlOutput("TE9_16"))),
                                
                                conditionalPanel(
                                  condition = "input.BE == 'E10'",
                                  splitLayout(cellWidths = c("100%"),
                                              highchartOutput("E10_16", height = 450))),
                                
                                conditionalPanel(
                                  condition = "input.BE == 'E11'",
                                  splitLayout(cellWidths = c("100%"),
                                              highchartOutput("E11_1_16", height = 450)),
                                  br(),
                                  p(strong("NOTE: ",style="color:#e30513"),"Multiple answer question. 
                                    Bars represent positive answers for each item.")),
                                
                                conditionalPanel(
                                  condition = "input.BE == 'E12'",
                                  splitLayout(cellWidths = c("100%"),
                                              highchartOutput("E12_16", height = 450))),
                                
                                conditionalPanel(
                                  condition = "input.BE == 'E13'",
                                  splitLayout(cellWidths = c("100%"),
                                              highchartOutput("E13_16", height = 450))),
                                
                                conditionalPanel(
                                  condition = "input.BE == 'E14'",
                                  splitLayout(cellWidths = c("100%"),
                                              highchartOutput("E14_16", height = 450))),
                                  
                                
                                conditionalPanel(
                                  condition = "input.BE == 'E15'",
                                  
                                  splitLayout(cellWidths = c("100%"),
                                              highchartOutput("E15_16", height = 450)),
                                  br(),
                                  p(strong("NOTE: ",style="color:#e30513"),"Respondents to this question are those who answered 'Yes' to categories 'In full-time employment', 
                                    'In part-time employment' and 'Working in more than one part-time job' 
                                    to question: What was your labor market situation once you arrived in 
                                    Switzerland?")),
                                
                                conditionalPanel(
                                  condition = "input.BE == 'E16'",
                                  splitLayout(cellWidths = c("100%"),
                                              highchartOutput("E16_1_16", height = 450))),
                                
                                conditionalPanel(
                                  condition = "input.BE == 'E36'",
                                  splitLayout(cellWidths = c("100%"),
                                              highchartOutput("E36_18", height = 450))),
                                
                                conditionalPanel(
                                  condition = "input.BE == 'E37'",
                                  splitLayout(cellWidths = c("100%"),
                                              highchartOutput("E37_18", height = 450))),
                                
                                conditionalPanel(
                                  condition = "input.BE == 'E17'",
                                  splitLayout(cellWidths = c("100%"),
                                              highchartOutput("E17_16", height = 450))),
                                
                                conditionalPanel(
                                  condition = "input.BE == 'E18'",
                                  splitLayout(cellWidths = c("100%"),
                                              highchartOutput("E18_16", height = 450))),
                                
                                conditionalPanel(
                                  condition = "input.BE == 'E19'",
                                  splitLayout(cellWidths = c("100%"),
                                              highchartOutput("E19_16", height = 450))),
                                
                                
                                conditionalPanel(
                                  condition = "input.BE == 'E21'",
                                  splitLayout(cellWidths = c("100%"),
                                              highchartOutput("E21_16", height = 450))),
                                
                                
                                conditionalPanel(
                                  condition = "input.BE == 'E22'",
                                  splitLayout(cellWidths = c("100%"),
                                              highchartOutput("E22_16", height = 450))),
                                
                                conditionalPanel(
                                  condition = "input.BE == 'E38'",
                                  splitLayout(cellWidths = c("100%"),
                                             highchartOutput("E38_18", height = 450))),
                                
                                conditionalPanel(
                                  condition = "input.BE == 'E23'",
                                  splitLayout(cellWidths = c("100%"),
                                              highchartOutput("E23_16", height = 450))),
                                
                                conditionalPanel(
                                  condition = "input.BE == 'E24'",
                                  splitLayout(cellWidths = c("100%"),
                                              highchartOutput("E24_1_16", height = 450)),
                                  br(),
                                  p(strong("NOTE: ",style="color:#e30513"),"Multiple answer question. 
                                    Bars represent positive answers for each item.")),
                                
                                conditionalPanel(
                                  condition = "input.BE == 'E25'",
                                  splitLayout(cellWidths = c("100%"),
                                              highchartOutput("E25_16", height = 450))),
                                
                                
                                conditionalPanel(
                                  condition = "input.BE == 'E26'",
                                 
                                  splitLayout(cellWidths = c("100%"),
                                              highchartOutput("E26_1_16", height = 450)),
                                  br(),
                                  p(strong("NOTE: ",style="color:#e30513"),"Respondents to this question are those who answered values 0 to 3 in previous question.")),
                                
                                
                                conditionalPanel(
                                  condition = "input.BE == 'E39'",
                                  splitLayout(cellWidths = c("100%"),
                                              highchartOutput("E39_16", height = 450))),
                                
                                
                                conditionalPanel(
                                  condition = "input.BE == 'E27'",
                                  splitLayout(cellWidths = c("100%"),
                                              highchartOutput("E27_16", height = 450))),
                                
                                conditionalPanel(
                                  condition = "input.BE == 'E40'",
                                  splitLayout(cellWidths = c("48.5%", "48.5%"),
                                              htmlOutput("E40_16"),
                                              highchartOutput("E40_18", height = 250))),
                                
                                conditionalPanel(
                                  condition = "input.BE == 'E41'",
                                  splitLayout(cellWidths = c("48.5%", "48.5%"),
                                              htmlOutput("E41_1_16"),
                                              highchartOutput("E41_1_18", height = 450)),
                                  
                                  splitLayout(cellWidths = c("48.5%", "48.5%"),
                                              htmlOutput("E41_2_16"),
                                              highchartOutput("E41_2_18", height = 450))),
                                
                                
                                conditionalPanel(
                                  condition = "input.BE == 'E42'",
                                 
                                  splitLayout(cellWidths = c("100%"),
                                             # htmlOutput("E42_1_16"),
                                              highchartOutput("E42_1_18", height = 450)),
                                  br(),
                                  p(strong("NOTE: ",style="color:#e30513"),"Respondents to this question are those who answered 'Yes' to question: Have you had business activities in the past 12 months with people or institutions in countries other than Switzerland?"))
                                
                      )
                      
             ),
             #   ), # este cierra navbarMenu
             
             ###### 6 Family configuration and household composition #####                 
             #navbarMenu(h5("Family and household"),
             
             ##### SURVEY  #####
             
             tabPanel(h5(span("Family and household")),
                      sidebarPanel(
                        
                        selectizeInput("BF", "Choose a question", 
                                       choices=FQUESTIONS, selected="F1"),
                        
                        conditionalPanel(
                          condition = "input.BF == 'F4'",
                          selectInput(
                            "yearF", "Year",
                            c(2016,2018,2020))),
                        
                        
                        selectizeInput("magF", "Weighted or non weighted data:", 
                                       choices=magnitude, selected="weight"),
                        
                        selectizeInput("magF2", "Select a magnitude:", 
                                       choices=magnitude2, selected="Relative"),
                        
                        p(strong("Downloads: "),"Charts and associated data can be downloaded from the 'Chart context' menu at the upper-right corner of each plot."),
                        p(strong("Elaboration:")),
                        # br(),
                        p("Survey coordinator: ",a(strong("Philippe Wanner"),
                                                   href="https://nccr-onthemove.ch/who-is-who/people/?start=w&p_id=156")),
                        
                        p("Field work: ", a(strong("IDESO"),
                                            href="https://www.unige.ch/sciences-societe/ideso/"),
                          "and ",a(strong("LINK Institute"),
                                   href="https://www.link.ch/en/")),
                        
                        p("Platform: ", a(strong("Juan Galeano"),
                                          href="https://nccr-onthemove.ch/who-is-who/people/?start=g&p_id=9211")),
                        
                        p(strong("Suggested citation:")),
                        p(a(strong("nccr - on the move, Migration-Mobility Survey. Neuchatel: nccr - on the move, 2020."),
                            href="https://nccr-onthemove.ch/research/migration-mobility-survey/"))
                      ),# este cierra sidebarPanel
                      mainPanel(align="center",
                                
                                conditionalPanel(
                                  condition = "input.BF == 'F17'",
                                  splitLayout(cellWidths = c("100%"),
                                             
                                              highchartOutput("F17_18", height = 450))),
                                
                                conditionalPanel(
                                  condition = "input.BF == 'F18'",
                                  splitLayout(cellWidths = c( "100%"),
                                              highchartOutput("F18_18", height = 450))),
                                
                                
                                conditionalPanel(
                                  condition = "input.BF == 'F19'",
                                  splitLayout(cellWidths = c("100%"),
                                              highchartOutput("F19_1_18", height = 450)),
                                  br(),
                                  p(strong("NOTE: ",style="color:#e30513"),"Multiple answer question. 
                                    Bars represent positive answers for each item.")),
                                
                                conditionalPanel(
                                  condition = "input.BF == 'F1'",
                                  splitLayout(cellWidths = c("100%"),
                                              highchartOutput("F1_16", height = 450))),
                                
                                conditionalPanel(
                                  condition = "input.BF == 'F2'",
                                  splitLayout(cellWidths = c("100%"),
                                              highchartOutput("F2_16", height = 450))),
                                
                                conditionalPanel(
                                  condition = "input.BF == 'F3'",
                                  splitLayout(cellWidths = c("48.5%", "48.5%"),
                                              highchartOutput("F3_16", height = 300),
                                              htmlOutput("F3_18")),
                                  
                                  splitLayout(cellWidths = c("48.5%", "48.5%"),
                                              htmlOutput("TF3_16"),
                                              htmlOutput("TF3_18"))),
                                
                                conditionalPanel(
                                  condition = "input.BF == 'F4'",
                                  splitLayout(cellWidths = c("100%"),
                                              highchartOutput("F4_16", height = 450))),
                                
                                conditionalPanel(
                                  condition = "input.BF == 'F6'",
                                  splitLayout(cellWidths = c("100%"),
                                              highchartOutput("F6_16", height = 450))),
                                
                                conditionalPanel(
                                  condition = "input.BF == 'F7'",
                                  splitLayout(cellWidths = c("100%"),
                                              highchartOutput("F7_1_16", height = 450)),
                                  br(),
                                  p(strong("NOTE: ",style="color:#e30513"),"Multiple answer question. 
                                    Bars represent positive answers for each item.")),
                                
                                conditionalPanel(
                                  condition = "input.BF == 'F8'",
                                  splitLayout(cellWidths = c("100%"),
                                              highchartOutput("F8_16", height = 450))),
                                
                                conditionalPanel(
                                  condition = "input.BF == 'F10'",
                                  splitLayout(cellWidths = c("100%"),
                                              highchartOutput("F10_1_16", height = 450)),
                                  
                                  splitLayout(cellWidths = c("100%"),
                                              highchartOutput("F10_2_16", height = 450)),
                                  
                                  splitLayout(cellWidths = c("100%"),
                                              highchartOutput("F10_3_16", height = 450))),
                                
                                conditionalPanel(
                                  condition = "input.BF == 'F11'",
                                  splitLayout(cellWidths = c("100%"),
                                              highchartOutput("F11_1_16", height = 450)),
                                  br(),
                                  p(strong("NOTE: ",style="color:#e30513"),"Multiple answer question. 
                                    Bars represent positive answers for each item.")),
                                
                                conditionalPanel(
                                  condition = "input.BF == 'F12'",
                                  splitLayout(cellWidths = c("100%"),
                                              highchartOutput("F12_1_16", height = 450)),
                                  br(),
                                  p(strong("NOTE: ",style="color:#e30513"),"Multiple answer question. 
                                    Bars represent positive answers for each item.")),
                                
                                conditionalPanel(
                                  condition = "input.BF == 'F13'",
                                  splitLayout(cellWidths = c("100%"),
                                              highchartOutput("F13_1_16", height =450)),
                                  br(),
                                  p(strong("NOTE: ",style="color:#e30513"),"Multiple answer question. 
                                    Bars represent positive answers for each item.")),
                                
                                conditionalPanel(
                                  condition = "input.BF == 'F24'",
                                  splitLayout(cellWidths = c("100%"),
                                              highchartOutput("F24_18", height = 450))),
                                
                                conditionalPanel(
                                  condition = "input.BF == 'F25'",
                                  splitLayout(cellWidths = c("48.5%", "48.5%"),
                                              htmlOutput("F25_16"),
                                              highchartOutput("F25_18", height = 200))),
                                
                                conditionalPanel(
                                  condition = "input.BF == 'F26'",
                                  splitLayout(cellWidths = c("100%"),
                                              
                                              highchartOutput("F26_18", height = 450))),
                                
                                conditionalPanel(
                                  condition = "input.BF == 'F27'",
                                  splitLayout(cellWidths = c("48.5%", "48.5%"),
                                              htmlOutput("F27_16"),
                                              highchartOutput("F27_18", height = 200))),
                                
                                conditionalPanel(
                                  condition = "input.BF == 'F28'",
                                  splitLayout(cellWidths = c("100%"),
                                               highchartOutput("F28_18", height = 450))),
                                
                                conditionalPanel(
                                  condition = "input.BF == 'F29'",
                                  splitLayout(cellWidths = c("100%"),
                                              highchartOutput("F29_18", height = 450)))
                                
                      )
                      
             ),
             # ), # este cierra navbarMenu
             
             ###### 7 Integration #####                 
             #navbarMenu(h5("Integration"),
             
             ##### sURVEY #####
             
             tabPanel(h5(span("Integration")),
                      sidebarPanel(
                        
                        selectizeInput("BG", "Choose a question", 
                                       choices=GQUESTIONS, selected="G1"),
                        
                        selectizeInput("magG", "Weighted or non weighted data:", 
                                       choices=magnitude, selected="weight"),
                        
                        selectizeInput("magG2", "Select a magnitude:", 
                                       choices=magnitude2, selected="Relative"),
                        
                        
                        
                        p(strong("Downloads: "),"Charts and associated data can be downloaded from the 'Chart context' menu at the upper-right corner of each plot."),
                        
                        p(strong("Elaboration:")),
                        # br(),
                        p("Survey coordinator: ",a(strong("Philippe Wanner"),
                                                   href="https://nccr-onthemove.ch/who-is-who/people/?start=w&p_id=156")),
                        
                        p("Field work: ", a(strong("IDESO"),
                                            href="https://www.unige.ch/sciences-societe/ideso/"),
                          "and ",a(strong("LINK Institute"),
                                   href="https://www.link.ch/en/")),
                        
                        p("Platform: ", a(strong("Juan Galeano"),
                                          href="https://nccr-onthemove.ch/who-is-who/people/?start=g&p_id=9211")),
                        
                        p(strong("Suggested citation:")),
                        p(a(strong("nccr - on the move, Migration-Mobility Survey. Neuchatel: nccr - on the move, 2020."),
                            href="https://nccr-onthemove.ch/research/migration-mobility-survey/"))
                        
                      ),# este cierra sidebarPanel
                      mainPanel(align="center",
                                
                                conditionalPanel(
                                  condition = "input.BG == 'G1'",
                                  splitLayout(cellWidths = c("100%"),
                                              highchartOutput("G1_1_16", height = 450))),
                                
                                conditionalPanel(
                                  condition = "input.BG == 'G2'",
                                  splitLayout(cellWidths = c("100%"),
                                              highchartOutput("G2_16", height = 450))),
                                
                                conditionalPanel(
                                  condition = "input.BG == 'G3'",
                                  splitLayout(cellWidths = c("100%"),
                                              highchartOutput("G3_16", height = 450))),
                                
                                conditionalPanel(
                                  condition = "input.BG == 'G4'",
                                  splitLayout(cellWidths = c("48.5%", "48.5%"),
                                              highchartOutput("G4_16", height = 175),
                                              htmlOutput("G4_18")),
                                  
                                  splitLayout(cellWidths = c("48.5%", "48.5%"),
                                              htmlOutput("TG4_16"),
                                              htmlOutput("TG4_18"))),
                                
                                conditionalPanel(
                                  condition = "input.BG == 'G20'",
                                  splitLayout(cellWidths = c("100%"),
                                              highchartOutput("G20_18", height = 450))),
                                
                                
                                conditionalPanel(
                                  condition = "input.BG == 'G5'",
                      
                                  splitLayout(cellWidths = c("100%"),
                                              highchartOutput("G5_1_16", height = 450)),
                                  br(),
                                  p(strong("NOTE: ",style="color:#e30513"),"Respondents to this question are those who answered 'Yes' to previous question.
                                  Multiple answer question. 
                                    Bars represent positive answers for each item.")),
                                
                                
                                conditionalPanel(
                                  condition = "input.BG == 'G6'",
                                  splitLayout(cellWidths = c("100%"),
                                              highchartOutput("G6_16", height = 450))),
                                
                                conditionalPanel(
                                  condition = "input.BG == 'G21'",
                                  splitLayout(cellWidths = c("100%"),
                                              highchartOutput("G21_18", height = 450))),
                                
                                conditionalPanel(
                                  condition = "input.BG == 'G22'",
                                  splitLayout(cellWidths = c("100%"),
                                              highchartOutput("G22_18", height = 450))),
                                
                                conditionalPanel(
                                  condition = "input.BG == 'G7'",
                                  splitLayout(cellWidths = c("100%"),
                                              highchartOutput("G7_16", height = 450))),
                                
                                conditionalPanel(
                                  condition = "input.BG == 'G8'",
                                  splitLayout(cellWidths = c("100%"),
                                              highchartOutput("G8_16", height = 450))),
                                
                                conditionalPanel(
                                  condition = "input.BG == 'G23'",
                                  splitLayout(cellWidths = c("100%"),
                                              highchartOutput("G23_18", height = 450))),
                                
                                
                                conditionalPanel(
                                  condition = "input.BG == 'G9'",
                                  splitLayout(cellWidths = c("100%"),
                                              highchartOutput("G9_16", height = 450))),
                                
                                conditionalPanel(
                                  condition = "input.BG == 'G10'",
                                  splitLayout(cellWidths = c("100%"),
                                              highchartOutput("G10_16", height = 450))),
                                
                                
                                conditionalPanel(
                                  condition = "input.BG == 'G11'",
                                  splitLayout(cellWidths = c("100%"),
                                              highchartOutput("G11_16", height = 450))),
                                
                                conditionalPanel(
                                  condition = "input.BG == 'G12'",
                                  splitLayout(cellWidths = c("48.5%", "48.5%"),
                                              highchartOutput("G12_16", height = 250),
                                              htmlOutput("G12_18"))),
                                
                                conditionalPanel(
                                  condition = "input.BG == 'G13'",
                                  splitLayout(cellWidths = c("100%"),
                                              highchartOutput("G13_1_16", height = 250)),
                                  splitLayout(cellWidths = c("100%"),
                                              highchartOutput("G13_2_16", height = 250))),
                                
                                
                                conditionalPanel(
                                  condition = "input.BG == 'G14'",
                                  splitLayout(cellWidths = c("100%"),
                                              highchartOutput("G14_1_1_16", height = 450))),
                                
                                conditionalPanel(
                                  condition = "input.BG == 'G14B'",
                                  splitLayout(cellWidths = c("100%"),
                                              highchartOutput("G14_2_1_16", height = 450))),
                                
                                conditionalPanel(
                                  condition = "input.BG == 'G15'",
                                  splitLayout(cellWidths = c("100%"),
                                              highchartOutput("G15_16", height = 450))),
                                
                                conditionalPanel(
                                  condition = "input.BG == 'G17'",
                                  splitLayout(cellWidths = c("100%"),
                                              highchartOutput("G17_1_1_16", height = 450)),
                                  p(strong("NOTE: ",style="color:#e30513"),"Multiple answer question. 
                                    Bars represent positive answers for each item.")),
                                
                                conditionalPanel(
                                  condition = "input.BG == 'G17B'",
                                  splitLayout(cellWidths = c("100%"),
                                              highchartOutput("G17_2_1_16", height = 450)),
                                  p(strong("NOTE: ",style="color:#e30513"),"Multiple answer question. 
                                    Bars represent positive answers for each item.")),
                                
                                conditionalPanel(
                                  condition = "input.BG == 'G18'",
                                  splitLayout(cellWidths = c("100%"),
                                              highchartOutput("G18_1_16", height = 450)),
                                  
                                  splitLayout(cellWidths = c("100%"),
                                              highchartOutput("G18_2_16", height = 450))),
                                
                                conditionalPanel(
                                  condition = "input.BG == 'G19'",
                                  splitLayout(cellWidths = c("100%"),
                                              highchartOutput("G19_1_16", height = 450)),
                                  
                                  splitLayout(cellWidths = c("100%"),
                                              highchartOutput("G19_2_16", height = 450))),
                                
                                conditionalPanel(
                                  condition = "input.BG == 'G16'",
                                  splitLayout(cellWidths = c("100%"),
                                              highchartOutput("G16_1_16", height = 450)),
                                  
                                  splitLayout(cellWidths = c("100%"),
                                              highchartOutput("G16_2_16", height = 450))),
                                
                                conditionalPanel(
                                  condition = "input.BG == 'G26'",
                                  splitLayout(cellWidths = c("100%"),
                                              highchartOutput("G26_1_18", height = 200)),
                                  
                                  splitLayout(cellWidths = c("100%"),
                                              highchartOutput("G26_2_18", height = 200)),
                                  
                                  splitLayout(cellWidths = c("100%"),
                                              highchartOutput("G26_3_18", height = 200)),
                                  
                                  splitLayout(cellWidths = c("100%"),
                                              highchartOutput("G26_4_18", height = 200))),
                                
                                conditionalPanel(
                                  condition = "input.BG == 'G27'",
                                  splitLayout(cellWidths = c("100%"),
                                              highchartOutput("G27_1_18", height = 300)),
                                  
                                  splitLayout(cellWidths = c("100%"),
                                              highchartOutput("G27_2_18", height = 300))),
                                  
                                
                                
                                conditionalPanel(
                                  condition = "input.BG == 'G28'",
                                  splitLayout(cellWidths = c("100%"),
                                              highchartOutput("G28_18", height = 450)))
                      )
                      
             ),
             #    ), # este cierra navbarMenu
             
             ###### 8 Life in Switzerland #####                 
             
             tabPanel(h5(span("Life in Switzerland")),
                      sidebarPanel(
                        
                        selectizeInput("BH", "Choose a question", 
                                       choices=HQUESTIONS, selected="H1"),
                        
                        selectizeInput("magH", "Weighted or non weighted data:", 
                                       choices=magnitude, selected="weight"),
                        
                        selectizeInput("magH2", "Select a magnitude:", 
                                       choices=magnitude2, selected="Relative"),
                        
                        
                        p(strong("Downloads: "),"Charts and associated data can be downloaded from the 'Chart context' menu at the upper-right corner of each plot."),
                        
                        p(strong("Elaboration:")),
                        # br(),
                        p("Survey coordinator: ",a(strong("Philippe Wanner"),
                                                   href="https://nccr-onthemove.ch/who-is-who/people/?start=w&p_id=156")),
                        
                        p("Field work: ", a(strong("IDESO"),
                                            href="https://www.unige.ch/sciences-societe/ideso/"),
                          "and ",a(strong("LINK Institute"),
                                   href="https://www.link.ch/en/")),
                        
                        p("Platform: ", a(strong("Juan Galeano"),
                                          href="https://nccr-onthemove.ch/who-is-who/people/?start=g&p_id=9211")),
                        
                        p(strong("Suggested citation:")),
                        p(a(strong("nccr - on the move, Migration-Mobility Survey. Neuchatel: nccr - on the move, 2020."),
                            href="https://nccr-onthemove.ch/research/migration-mobility-survey/"))
                        
                      ),# este cierra sidebarPanel
                      mainPanel(align="center",
                                
                                conditionalPanel(
                                  condition = "input.BH == 'H1'",
                                  splitLayout(cellWidths = c("100%"),
                                              highchartOutput("H1_16", height = 300))),
                                
                                conditionalPanel(
                                  condition = "input.BH == 'H2'",
                                  splitLayout(cellWidths = c("100%"),
                                              highchartOutput("H2_16", height = 300))),
                                
                                conditionalPanel(
                                  condition = "input.BH == 'H3'",
                                  splitLayout(cellWidths = c("100%"),
                                              highchartOutput("H3_16", height = 450))),
                                
                                conditionalPanel(
                                  condition = "input.BH == 'H4'",
                                  splitLayout(cellWidths = c("48.5%", "48.5%"),
                                              highchartOutput("H4_16", height = 200),
                                              htmlOutput("H4_18"))),
                                
                                conditionalPanel(
                                  condition = "input.BH == 'H5'",
                                  splitLayout(cellWidths = c("48.5%", "48.5%"),
                                              highchartOutput("H5_16", height = 450),
                                              highchartOutput("H5_18", height = 450))),
                                
                                conditionalPanel(
                                  condition = "input.BH == 'H7'",
                                  splitLayout(cellWidths = c("48.5%", "48.5%"),
                                              highchartOutput("H7_16", height = 450),
                                              highchartOutput("H7_18", height = 450))),
                                
                                conditionalPanel(
                                  condition = "input.BH == 'H8'",
                  
                                  splitLayout(cellWidths = c("100%"),
                                              highchartOutput("H8_1_16", height = 450)),
                                  br(),
                                  p(strong("NOTE: ",style="color:#e30513"),"Respondents to this question are those who answered 'Yes' in 2018 and 'Sometimes' and 'Frequently' to previous question.
                                    Multiple answer question. 
                                    Bars represent positive answers for each item."),
                                  ),
                                
                                conditionalPanel(
                                  condition = "input.BH == 'H9'",
                                 
                                  splitLayout(cellWidths = c("100%"),
                                              highchartOutput("H9_1_16", height = 450)),
                                  br(),
                                  p(strong("NOTE: ",style="color:#e30513"),"Respondents to this question are those who answered 'Yes' in 2018 and 'Sometimes' and 'Frequently' to question: Discrimination means that a person is treated less favorably than other people because of different characteristics. Have you experienced situations of prejudice or discrimination in Switzerland in the last 24 months?.")),
                                
                                
                                conditionalPanel(
                                  condition = "input.BH == 'H12'",
                                  splitLayout(cellWidths = c("100%"),
                                           highchartOutput("H12_1_18", height = 450))),
                                
                                
                                conditionalPanel(
                                  condition = "input.BH == 'H6'",
                                  splitLayout(cellWidths = c("100%"),
                                              highchartOutput("H6_1_16", height = 300)),
                                  
                                  splitLayout(cellWidths = c("100%"),
                                              highchartOutput("H6_4_16", height = 300)),
                                  
                                  splitLayout(cellWidths = c("100%"),
                                              highchartOutput("H6_5_16", height = 300)),
                                  
                                  splitLayout(cellWidths = c("100%"),
                                              highchartOutput("H6_6_18", height = 300)),
                                  
                                  splitLayout(cellWidths = c("100%"),
                                              highchartOutput("H6_7_18", height = 300)),
                                  
                                  splitLayout(cellWidths = c("100%"),
                                              highchartOutput("H6_2_16", height = 300)),
                                  
                                  splitLayout(cellWidths = c("100%"),
                                              highchartOutput("H6_3_16", height = 300))),
                                  
                                
                                 
                                  
                               
                                
                                
                                conditionalPanel(
                                  condition = "input.BH == 'H10'",
                                  splitLayout(cellWidths = c("100%"),
                                              highchartOutput("H10_16", height = 450))),
                                
                                conditionalPanel(
                                  condition = "input.BH == 'H11'",
                                  splitLayout(cellWidths = c("100%"),
                                              highchartOutput("H11_1_16", height = 300)),
                                  splitLayout(cellWidths = c("100%"),
                                              highchartOutput("H11_2_16", height = 300))),
                                
                                conditionalPanel(
                                  condition = "input.BH == 'H13'",
                                  splitLayout(cellWidths = c("100%"),
                                              highchartOutput("H13_18", height = 450))),
                                
                                conditionalPanel(
                                  condition = "input.BH == 'H14'",
                                  splitLayout(cellWidths = c("100%"),
                                              highchartOutput("H14_18", height = 450))),
                                
                                conditionalPanel(
                                  condition = "input.BH == 'H15'",
                                  splitLayout(cellWidths = c("100%"),
                                             highchartOutput("H15_18", height = 450))),
                                
                                conditionalPanel(
                                  condition = "input.BH == 'H16'",
                                  splitLayout(cellWidths = c("100%"),
                                              highchartOutput("H16_18", height = 450)))
                                
                      )
                      
             ),
             #  ), # este cierra navbarMenu
             
             ###### 9 COVID19 #####                 
             
             tabPanel(h5(span("Covid-19")),
                      sidebarPanel(
                        
                        selectizeInput("BK", "Choose a question", 
                                       choices=KQUESTIONS, selected="K1"),
                        
                        selectizeInput("magK", "Weighted or non weighted data:", 
                                       choices=magnitude, selected="weight"),
                        
                        selectizeInput("magK2", "Select a magnitude:", 
                                       choices=magnitude2, selected="Relative"),
                        
                        
                        p(strong("Downloads: "),"Charts and associated data can be downloaded from the 'Chart context' menu at the upper-right corner of each plot."),
                        
                        p(strong("Elaboration:")),
                        # br(),
                        p("Survey coordinator: ",a(strong("Philippe Wanner"),
                                                   href="https://nccr-onthemove.ch/who-is-who/people/?start=w&p_id=156")),
                        
                        p("Field work: ", a(strong("IDESO"),
                                            href="https://www.unige.ch/sciences-societe/ideso/"),
                          "and ",a(strong("LINK Institute"),
                                   href="https://www.link.ch/en/")),
                        
                        p("Platform: ", a(strong("Juan Galeano"),
                                          href="https://nccr-onthemove.ch/who-is-who/people/?start=g&p_id=9211")),
                        
                        p(strong("Suggested citation:")),
                        p(a(strong("nccr - on the move, Migration-Mobility Survey. Neuchatel: nccr - on the move, 2020."),
                            href="https://nccr-onthemove.ch/research/migration-mobility-survey/"))
                        
                      ),# este cierra sidebarPanel
                      mainPanel(align="center",
                                
                                conditionalPanel(
                                  condition = "input.BK == 'K1'",
                                  splitLayout(cellWidths = c("100%"),
                                              highchartOutput("K1_20", height = 450))),
                                
                                conditionalPanel(
                                  condition = "input.BK == 'K2'",
                                  splitLayout(cellWidths = c("100%"),
                                              highchartOutput("K2_20", height = 450))),
                                
                                conditionalPanel(
                                  condition = "input.BK == 'K3'",
                                  splitLayout(cellWidths = c("100%"),
                                              highchartOutput("K3_20", height = 450))),
                                
                                conditionalPanel(
                                  condition = "input.BK == 'K4'",
                                  splitLayout(cellWidths = c("100%"),
                                              highchartOutput("K4_20", height = 450))),
                                
                                conditionalPanel(
                                  condition = "input.BK == 'K5'",
                                  splitLayout(cellWidths = c("100%"),
                                              highchartOutput("K5_20", height = 450))),
                                
                                conditionalPanel(
                                  condition = "input.BK == 'K6'",
                                  splitLayout(cellWidths = c("100%"),
                                              highchartOutput("K6_20", height = 450))),
                                
                                conditionalPanel(
                                  condition = "input.BK == 'K7'",
                                  splitLayout(cellWidths = c("100%"),
                                              highchartOutput("K7_20", height = 450))),
                                
                                conditionalPanel(
                                  condition = "input.BK == 'K8'",
                                  splitLayout(cellWidths = c("100%"),
                                              highchartOutput("K8_20", height = 450))),
                                
                                conditionalPanel(
                                  condition = "input.BK == 'K9'",
                                  splitLayout(cellWidths = c("100%"),
                                              highchartOutput("K9_20", height = 450)))
                                
                                
                      )
                      
             ),
             
      
             ##### CROSS #####
             
             tabPanel(h5(span("Crosstables")),
                      sidebarPanel(
                        
                        selectizeInput("A", "Choose a question", 
                                       choices=ABISQUESTIONS, selected="sex1"),
                        
                        selectizeInput("B", "Choose a question:", 
                                       choices=ABISQUESTIONS, selected="age_group"),
                        
                        selectizeInput("C", "Weighted or non weighted data:", 
                                       choices=magnitude, selected="weight"),
                        
                        selectizeInput("D", "Select a magnitude:", 
                                       choices=magnitude2, selected="Relative"),
                        
                        
                        
                        
                        radioButtons("filetypeCT", strong("Download data associated to this question, choose a file extension:"),
                                     choices = c("csv", "txt")),
                        downloadButton('downloadDataCT', 'Download data'),
                        br(),
                        br(),
                        p(strong("Elaboration:")),
                        # br(),
                        p("Survey coordinator: ",a(strong("Philippe Wanner"),
                                                   href="https://nccr-onthemove.ch/who-is-who/people/?start=w&p_id=156")),
                        
                        p("Field work: ", a(strong("IDESO"),
                                            href="https://www.unige.ch/sciences-societe/ideso/"),
                          "and ",a(strong("LINK Institute"),
                                   href="https://www.link.ch/en/")),
                        
                        p("Platform: ", a(strong("Juan Galeano"),
                                          href="https://nccr-onthemove.ch/who-is-who/people/?start=g&p_id=9211")),
                        
                        p(strong("Suggested citation:")),
                        p(a(strong("nccr - on the move, Migration-Mobility Survey. Neuchatel: nccr - on the move, 2020."),
                            href="https://nccr-onthemove.ch/research/migration-mobility-survey/"))
                        
                      ),# este cierra sidebarPanel
                      mainPanel(align="center",
                                
                                
                                htmlOutput("cross", height = 300),
                                htmlOutput("cross2", height = 300),
                                htmlOutput("cross3", height = 300)
                                
                                
                      )
             ))#)
  )))
