#RECLASSIFY CHARACTER FIELDS to ORDERED FACTORS based on Predefined Level Orders (Level_check)

#NOTE ABOUT LEVELS (level_check as defined below):
#       Must have fewer option items listed first - currently code will grab the First match.  
#       This means that a "YES NO" must be listed before "YES UNSURE NO" or else all actual
#       "YES NO" questions will be coded as having "YES UNSURE NO" with no 'UNSURE' values.
#       Order of the levels is always left to right:  1<2<3...

level_check = list("true_4"= c('Not True','Slightly True','Mostly True','Very True'),
                   "freq_4a" = c('No', 'Sometimes','Usually','Always'),
                   "freq_4b" = c('Never','Rarely','Usually','Always'),
                   "freq_4c" = c('Occasionally','Sometimes','Often','Usually'),
                   "freq_5" = c('Never','Rarely','Sometimes','Often','Very often'),
                   "freq_6" = c('Never','Less than once per month','Once per month','Twice per month','Weekly','More than once a week'),
                   "met_4" = c('Incomplete and/or Not Met','Minimally Met','Adequately Met','Exceeded'),
                   "adequate_3" = c('Inadequate','Adequate', 'Very Adequate'),
                   "important_5" = c('Not at all Important', 'Very Unimportant', 'Neither Important nor Unimportant', 'Very Important', 'Extremely Important'),
                   "helpful_4a" = c('Not Helpful','Somewhat Helpful','Helpful','Very Helpful'),
                   "helpful_4b" = c('Not at all helpful','Slightly helpful','Helpful','Very Helpful'),
                   "growth_4" = c('No Growth','Little Growth','Moderate Growth','Significant Growth'),
                   "impact_4" = c('No Impact','Little Impact','Moderate Impact','Significant Impact'),
                   "extent_4" = c('None','Little','Moderate','Significant'),
                   "yesno_2" = c('No','Yes'),
                   "yesno_3a" = c('No','Unsure','Yes'),
                   "yesno_3b" = c('No','Undecided','Yes'),
                   "agree_4a" = c('Strongly Disagree','Disagree','Agree','Strongly Agree'),
                   "agree_4b" = c('Do not agree at all','Slightly agree','Mostly agree','Completely agree'),
                   "agree_5a" = c('Strongly Disagree','Disagree','Neither Agree nor Disagree','Agree','Strongly Agree'),
                   "agree_5b" = c('Strongly Disagree','Disagree','Neither Agree or Disagree','Agree','Strongly Agree'),
                   "agree_5c" = c('Strongly Disagree','Disagree','Neither Disagree or Agree','Agree','Strongly Agree'),
                   "agree_5d" = c('Strongly Disagree','Somewhat Disagree','Neither Agree nor Disagree','Somewhat Agree','Strongly Agree'),
                   "agree_5e" = c('Strongly Disagree','Disagree','Neutral','Agree','Strongly Agree'),
                   "familiar_5" = c('Not familiar at all','Slightly familiar','Moderately familiar','Very familiar','Extremely familiar'),
                   "satisfaction_3" = c('Dissatisfied','Satisfied','Very Satisfied'),
                   "satisfaction_5" = c('Very Dissatisfied','Dissatisfied','Neutral','Satisfied','Very Satisfied'),
                   "weak_4" = c('Very Weak','Weak','Strong','Very Strong'),
                   "confidence_4" = c('Not confident','Somewhat Confident','Confident','Very confident'),
                   "cstp_5" = c('Emerging','Exploring','Applying','Integrating','Innovating'),
                   "indstd_rubric_4" = c('Beginning','Developing','Competent','Accomplished'),
                   "custom_4a" = c('Not True','Two','Three','Very True'),
                   "custom_6a" = c('Zero','1','2','3','4','5 or more')
)
level_check = lapply(level_check, toupper) #convert all to upper


dfname <- winDialogString("enter name of data frame", default = "")
survey_data <- get(dfname)

#loop through columns and select what likert scales are
for (x in names(survey_data)) {
  # skip numeric columns,
  # columns that are all NAs,
  # columns that have more than 25 unique levels 
  if ( is.numeric(survey_data[,x]) == FALSE && 
       all(is.na(survey_data[,x])) == FALSE &&
       length(names(table(survey_data[ , x]))) < 25 ) { 
    
    matched_levels <- list()  
    survey_data[ , x] <- as.factor(toupper(survey_data[ , x]))
    for (y in names(level_check)) { 
      #for all pre-defined level names, if all the levels, excluding those identified in the c() , are in a pre-def level
      if (all(levels(survey_data[ , x])[! levels(survey_data[ , x]) %in% c("", " ", "NA", "NOT APPLICABLE")] %in% level_check[[y]] == TRUE)) {
        matched_levels <- append(matched_levels, y)
      } 
    }
    
    if (length(matched_levels) > 1 ) {
      #multiple level scale matches.  ultimately allow manual selection for multiple matches.
      survey_data[ , x] <- factor(survey_data[ , x], level_check[[matched_levels[[1]] ]], ordered = TRUE)
    } 
    
    else if (length(matched_levels) == 1) {
      #only one pre-defined level scale matched)
      survey_data[ , x] <- factor(survey_data[ , x], level_check[[matched_levels[[1]] ]], ordered = TRUE)
    } 
    
    else {
      
      if (levels(survey_data[,x])[1] %in% c(""," ") ) {
        levels(survey_data[,x])[1] <- NA        
      }
      
    }
  }
}

rm(x,y,dfname, matched_levels, level_check)
str(survey_data)
