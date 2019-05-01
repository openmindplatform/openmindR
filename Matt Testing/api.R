# Script to download data from Airtable using API
# Be careful with this script as it can bring about an openmind armageddon
# Also, do not share the API keys publicly
# 10/11/2018

library(airtabler)
Sys.setenv(AIRTABLE_API_KEY="keyxwOZNxPKLgrwyR")
AIRTABLE_API_KEY="keyxwOZNxPKLgrwyR"


dat.ass.1<-airtable(
  base = "appjU7KUyybZ4rGvT", 
  tables = c("AssessmentV4","AssessmentV5","AccessCodes","ParticipantProgress","InstructorSurvey")
)

#downloads first 100 cases
dat.ass<-air_get("appjU7KUyybZ4rGvT", "AssessmentV4",combined_result = TRUE)

#downloads full data table
dat.ass4 <- dat.ass.1$AssessmentV4$select_all()
dat.ass5 <- dat.ass.1$AssessmentV5$select_all()
dat.acc <- dat.ass.1$AccessCodes$select_all()
dat.par <- dat.ass.1$ParticipantProgress$select_all()
dat.ins <- dat.ass.1$InstructorSurvey$select_all()

# save out data file
save(dat.ass4, file = "data/ass4.RData")
save(dat.ass5, file = "data/ass5.RData")
save(dat.acc, file = "data/acc.RData")
save(dat.par, file = "data/par.RData")
save(dat.ins, file = "data/ins.RData")

# Now, open OpenMindCleaningV4.R and clean the data