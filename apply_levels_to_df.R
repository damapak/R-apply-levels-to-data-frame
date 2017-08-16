#most data, including qualtrics downloads, will be automatically imported into factors.
#to prevent this from happening, can use 'as.is' or 'stringsAsFactors' options.

#list of levels used in surveys

level_check = list("true_4"= c('Not True','Slightly True','Mostly True','Very True'),
                   "freq_4" = c('No', 'Sometimes','Usually','Always'),
                   "freq2_4" = c('Never','Rarely','Usually','Always'),
                   "freq_5" = c('Never','Rarely','Sometimes','Often','Very often'),
                   "met_4" = c('Incomplete and/or Not Met','Minimally Met','Adequately Met','Exceeded'),
                   "adequate_3" = c('Inadequate','Adequate', 'Very Adequate'),
                   "helpful_4" = c('Not Helpful','Somewhat Helpful','Helpful','Very Helpful'),
                   "helpful2_4" = c('Not at all helpful','Slightly helpful','Helpful','Very Helpful'),
                   "growth_4" = c('No Growth','Little Growth','Moderate Growth','Significant Growth'),
                   "impact_4" = c('No Impact','Little Impact','Moderate Impact','Significant Impact'),
                   "extent_4" = c('None','Little','Moderate','Significant'),
                   "yesno_2" = c('No','Yes'),
                   "yesno_3" = c('No','Unsure','Yes'),
                   "agree_4" = c('Strongly Disagree','Disagree','Agree','Strongly Agree'),
                   "agree_5a" = c('Strongly Disagree','Disagree','Neither Agree nor Disagree','Agree','Strongly Agree'),
                   "agree_5b" = c('Strongly Disagree','Disagree','Neutral','Agree','Strongly Agree'),
                   "is_rubric_4" = c('Beginning','Developing','Competent','Accomplished'),
                   "satisfaction_3" = c('Dissatisfied','Satisfied','Very Satisfied'),
                   "satisfaction_5" = c('Very Dissatisfied','Dissatisfied','Neutral','Satisfied','Very Satisfied'),
                   "cstp_5" = c('Emerging','Exploring','Applying','Integrating','Innovating'),
                   "custom1_4" = c('Not True','Two','Three','Very True'),
                   "custom2_6" = c('Zero','1','2','3','4','5 or more')
)
level_check = lapply(level_check, toupper) #convert all to upper


dfname <- winDialogString("enter name of data frame", default = "")
survey_data <- get(dfname)

#loop through columns and select what likert scales are
for (x in names(survey_data)) {
  if (is.numeric(survey_data[,x])==FALSE){ #skip numeric columns
    survey_data[,x] <- as.factor (toupper(survey_data[,x]) )
    for (y in level_check) { #for all pre-defined levels
      # if all the levels, excluding those identified in the c() , are in a pre-def level
      if (all(levels(survey_data[,x])[! levels(survey_data[,x]) %in% c(""," ") ] %in% y ==TRUE)) {
        survey_data[,x] = factor(survey_data[,x], y, ordered=TRUE)
      }
    }
  }
}

rm(x,y,dfname)
