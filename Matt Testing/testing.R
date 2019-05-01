devtools::install_github("openmindplatform/openmindR")
library(openmindR)

#Read in data
load("data/ass4.RData")
load("data/ass5.RData")
load("data/acc.RData")
load("data/par.RData")
load("data/ins.RData")


om_filter_data(dat.ass4,n_assessments=3,version=4)
# ERROR: 
#> om_filter_data(dat.ass4,n_assessments=3,version=4)
#Error in app.dat %>% mutate(AssessmentsDone = as.numeric(AssessmentsDone)) %>%  : 
#  could not find function "%>%"#
# tidyverse / tidyr & dplyr are required

library(tidyverse)
om_filter_data(dat.ass4,n_assessments=3,version=4)
# script works fine after loading tidyverse


n1v4<-om_filter_data(dat.ass4,n_assessments=1,version=4)
n2v4<-om_filter_data(dat.ass4,n_assessments=2,version=4)
n3v4<-om_filter_data(dat.ass4,n_assessments=3,version=4)
# the above genrates 3 data frames; oddly, there are more people with 2 assessments than 1 assessment. 
# does this mean that when we set the number of assessments argument equal to something, then it only includes
# people with exactly that number of assessments?
# yep. how cool. should make a note of this in the help documentation

nNULLv4<-om_filter_data(dat.ass4,version=4)

#can it accept ranges?
n1plusv4<-om_filter_data(dat.ass4,n_assessments>1,version=4)
# ERROR: Does not like >
#Error in om_filter_data(dat.ass4, n_assessments > 1, version = 4) : 
#  object 'n_assessments' not found
# I think requiring n_assessments to be set at a specific value is better than a range; so i'd keep it like it is. 
# Maybe add a note to help file saying it must be 1 discrete value would be good

#can it accept multiple values?
n1rangev4<-om_filter_data(dat.ass4,n_assessments=c(1,2),version=4)
#yes it can.

# access codes
n1v4a<-om_filter_data(dat.ass4,n_assessments=1,version=4,accesscode="EddySalemStateUniversityF18")
#yes, good

#multiple access codes?
n1v4aplus<-om_filter_data(dat.ass4,n_assessments=1,version=4,accesscode=c("EddySalemStateUniversityF18","EddySalemStateUniversityF18t"))
# this generates a smaller data frame than when i request just1 access code, but it should be bigger
# because 2 classes are bigger than 1

n1v4apluss<-om_filter_data(dat.ass4,n_assessments=1,version=4,accesscode=c("EddySalemStateUniversityF18","EddySalemStateUniversityF18s"))
# it looks like it is taking the last access code in the list and dropping all data from earlier access codes;
# this needs to be fixed XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
# XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX


# can it filter down to all access codes by pattern matching?
n1v4<-om_filter_data(dat.ass4,n_assessments=1,version=4,accesscode=grep("Salem"))
# ***************************************************************
#no... this would be a good feature to add **********************
# ***************************************************************

#v5
n1v5<-om_filter_data(dat.ass5,n_assessments=1,version=5)
n2v5<-om_filter_data(dat.ass5,n_assessments=2,version=5)
n3v5<-om_filter_data(dat.ass5,n_assessments=3,version=5)
# the above generates 3 data frames; oddly, there are more people with 2 assessments than 1 assessment. 
# does this mean that when we set the number of assessments argument equal to something, then it only includes
# people with exactly that number of assessments?
# yep. how cool. should make a note of this in the help documentation

nNULLv5<-om_filter_data(dat.ass5,version=5)

#can it accept ranges?
n1plusv5<-om_filter_data(dat.ass4,n_assessments>1,version=5)
# ERROR: Does not like >
#Error in om_filter_data(dat.ass4, n_assessments > 1, version = 4) : 
#  object 'n_assessments' not found
# I think requiring n_assessments to be set at a specific value is better than a range; so i'd keep it like it is. 
# Maybe add a note to help file saying it must be 1 discrete value would be good

#can it accept multiple values?
n1rangev5<-om_filter_data(dat.ass5,n_assessments=c(1,2),version=5)
#yes it can.


# 5.1
#v5
n1v5.1<-om_filter_data(dat.ass5,n_assessments=1,version=5.1)
n2v5.1<-om_filter_data(dat.ass5,n_assessments=2,version=5.1)
n3v5.1<-om_filter_data(dat.ass5,n_assessments=3,version=5.1)



#########################################
#########################################
#########################################

# calc_correct
calc_correct(dat.par$StepsComplete,dat.par$StepScores,dat.par$StepQuestionTotals)
# spits out a bunch of NAs
# this function should have an argument specifying data frame where those 3 variables are, and convert them to numeric
calc_correct(as.numeric(dat.par$StepsComplete),as.numeric(dat.par$StepScores),as.numeric(par$StepQuestionTotals))
#this also does not work; gives same NA output as before



#########################################
#########################################
#########################################

library(magrittr)
library(tidyverse)
library(glue)
# om_clean_par
clean_par<-om_clean_par(dat.par)
#> om_clean_par(dat.par)
#ERROR XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
# XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
#Error in extract2(., 1) : could not find function "extract2" 
# actually now works... I think...

table(clean_par$StepTimes1)


#########################################
#########################################
#########################################

#clean ppol
?om_clean_ppol()
om_clean_ppol(dat.ass4)
#Error in is_string(match) : object 'var_strings' not found
om_clean_ppol(n2v4)
# Error in is_string(match) : object 'var_strings' not found
require(tidyverse)
om_clean_ppol(n2v4)
