#Data Source: Pulickamadhom Sreedhar, Sreehari, 2021, 
#"Student and Educator Perceptions of Online Classes"
#https://doi.org/10.7910/DVN/2AAVSX, Harvard Dataverse, V1 

#@michaelGRU

library(stringr)
library(utils)

mode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

#find the length of a particular string pattern 
count_pattern <- function(str, vector) {
  return(length(vector[stringr::str_detect(vector, str)]))
}


df <- read.csv("data/Student and Educator Perceptions of Online Classes - Base data.csv")

#quick summary of the df 
num_subjects <- dim(df)
feature <- colnames(df)

#student vs. educator analysis
#number of students
students <- subset(df, df$Are.you.a.student.or.an.educator. == "Student")
count_studnets <- nrow(students)

#number of professors and their experience 
educators<- subset(df, df$Are.you.a.student.or.an.educator. == "Educator")
count_educator <- nrow(educators)
exp_boxplot <- boxplot(educators$If.an.educator..for.how.many.years.have.you.been.an.educator.)

#where are you currently employed at
unique_employee_loc_count <- length(unique(df$Where.are.you.currently.employed.enrolled))

#How many are at university vs school
uni_count <- 
  length(df$Where.are.you.currently.employed.enrolled[stringr::str_detect(df$Where.are.you.currently.employed.enrolled, "/")])
school_count <- 
  length(df$Where.are.you.currently.employed.enrolled[stringr::str_detect(df$Where.are.you.currently.employed.enrolled, "oo")])

#extract the last 2 chars 
last_two_letters_extract <- stringr::str_sub(df$Where.are.you.currently.employed.enrolled, start = -2)
###

#group by students vs educators 
tapply(df$How.do.you.rate.yourself.on.your.ability.to.use.virtual.meeting.platforms..on.a.scale.of.1.10., df$Are.you.a.student.or.an.educator., mean)
tapply(df$How.would.you.rate.your.internet.connectivity.on.a.scale.of.1.10., df$Are.you.a.student.or.an.educator., mean)
tapply(df$How.effective.do.you.think.online.classes.are.in.communicating.content.and.intent.in.a.classroom..on.a.scale.of.1.10., df$Are.you.a.student.or.an.educator., mean)
tapply(df$How.would.you.rate.your.ability.to.concentrate.during.online.classes..on.a.scale.of.1.10., df$Are.you.a.student.or.an.educator., mean)

#answer to do long hours in a virtual classroom cause discomfort 
pain_count <- 
  length(df$Do.long.hours.in.a.virtual.classroom.cause.discomfort.such.as.headaches..sleep.disturbances..back.pain.[stringr::str_detect(df$Do.long.hours.in.a.virtual.classroom.cause.discomfort.such.as.headaches..sleep.disturbances..back.pain., "Yes")])
no_pain_count <- 
  length(df$Do.long.hours.in.a.virtual.classroom.cause.discomfort.such.as.headaches..sleep.disturbances..back.pain.[stringr::str_detect(df$Do.long.hours.in.a.virtual.classroom.cause.discomfort.such.as.headaches..sleep.disturbances..back.pain., "No")])

#add a column that converts the binary pain ans 
df$pain_binary <- 
  ifelse(df$Do.long.hours.in.a.virtual.classroom.cause.discomfort.such.as.headaches..sleep.disturbances..back.pain.== "Yes", 1, 0)
#percentage of people who reported pain 
percentage_pain <- mean(df$pain_binary)
#group by students vs. educator 
tapply(df$pain_binary, df$Are.you.a.student.or.an.educator., mean)

##create type 
df$type <- NA

df$type[df$How.would.you.identify.yourself. == "Female" 
        & df$Are.you.a.student.or.an.educator. == "Student"] <- "FemaleStudent"
df$type[df$How.would.you.identify.yourself. == "Female" 
        & df$Are.you.a.student.or.an.educator. == "Educator"] <- "FemaleEducator"
df$type[df$How.would.you.identify.yourself. == "Male" 
        & df$Are.you.a.student.or.an.educator. == "Student"] <- "MaleStudent"
df$type[df$How.would.you.identify.yourself. == "Male" 
        & df$Are.you.a.student.or.an.educator. == "Educator"] <- "MaleEducator"
df$type[df$How.would.you.identify.yourself. != "Male" & df$How.would.you.identify.yourself. != "Female"
        & df$Are.you.a.student.or.an.educator. == "Educator"] <- "OtherEducator"
df$type[df$How.would.you.identify.yourself. != "Male" & df$How.would.you.identify.yourself. != "Female"
        & df$Are.you.a.student.or.an.educator. == "Student"] <- "OtherStudent"

table(df$type)
tapply(df$How.do.you.rate.yourself.on.your.ability.to.use.virtual.meeting.platforms..on.a.scale.of.1.10., 
                df$type, mean)
tapply(df$How.would.you.rate.your.internet.connectivity.on.a.scale.of.1.10., df$type, mean)
tapply(df$How.effective.do.you.think.online.classes.are.in.communicating.content.and.intent.in.a.classroom..on.a.scale.of.1.10., 
                df$type, mean)
tapply(df$How.would.you.rate.your.ability.to.concentrate.during.online.classes..on.a.scale.of.1.10., 
                df$type, mean)
