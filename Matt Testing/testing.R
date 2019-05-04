devtools::install_github("openmindplatform/openmindR")
library(openmindR)

setwd("C:/Users/Matt/Dropbox/OM Team/Research/Projects/Current Projects/openmindR/Matt Testing/")

#Read in data
load("data/ass4.RData")
load("data/ass5.RData")
load("data/acc.RData")
load("data/par.RData")
load("data/ins.RData")


om_filter_data(dat.ass4,n_assessments=3,version=4)
# script works fine without loading tidyverse anymore


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
n1v4a<-om_filter_data(dat.ass4,n_assessments=3
                      ,version=4,accesscode="EddySalemStateUniversityF18",exact_search=F)
#yes, good

#multiple access codes?
n1v4aplus<-om_filter_data(dat.ass4,n_assessments=3,version=4,accesscode=c("EddySalemStateUniversityF18","EddySalemStateUniversityF18t","EddySalemStateUniversityF18s"),
                          exact_search=T)
# works... but should be mindful and maybe update help file to make clear that "EddySalemStateUniversityF18" includes ALL Eddy codes with other letters
# appended after the F18 IF exact_search = F



# can it filter down to all access codes by pattern matching?
n1v4<-om_filter_data(dat.ass4,n_assessments=3,version=4,accesscode="CLP",exact_search = F)
# yup!

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
#n1plusv5<-om_filter_data(dat.ass4,n_assessments>1,version=5)
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


clean_par<-om_clean_par(dat.par)
#> works well. slow, but that's to be expected given that this is the biggest data table we work with
# perhaps we should add in some arguments to filter it down
# most likely suspects: AccessCode and country


#########################################
#########################################
#########################################

#clean ppol
?om_clean_ppol()
n3v4cleanppol<-om_clean_ppol(n3v4)
table(n2v4cleanppol$ppol_num)
table(n2v4cleanppol$ppol_raw)
table(n2v4cleanppol$ppol)
table(n2v4cleanppol$ppol_cat)


#########################################
#########################################
#########################################

#construct measures
?om_construct_measures
om_construct_measures(n3v4cleanppol)
# this does not work; requires the data frame generated from om_clean_ppol

n3v4constructed<-om_construct_measures(n3v4cleanppol)
summary(n3v4constructed)
# this outputs the raw numbers from assessment.
# we should rescale all variables to range from 0 to 1 to ease interpretation and visualization
# so, let's add (x/(max possible response for individual item)) to function


#########################################
#########################################
#########################################

#remove dups
?remove_dups
n3v4constructedremdups<-remove_dups(n3v4constructed)
# removed 0 dups

?om_gather
n3v4long<-om_gather(n3v4constructedremdups,which_strings=q_c_strings)



?om_summarize_comparisons()
om_summarize_comparisons(n3v4long)
prepo<-om_summarize_comparisons(n3v4long,compare="PrePost")
prefo<-om_summarize_comparisons(n3v4long,compare="PreFollow")
