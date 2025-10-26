


whichColumnsVary <- function(df,
                             idVariables = c("experiment","participant","item","condition"),
                             timeVariable = "ioiLabel"
                             ){
  #
  require(dplyr)
  
  # Check whether union(idVariables,timeVariable) uniquely identify rows
  #
  doublets = checkUniqueness(df,union(idVariables,timeVariable))
  if (nrow(doublets)!=0){
    warning(
      cat('idVariables + timeVariable do not uniquely identify rows.\n', 
          'Check with: checkUniqueness(df, ', deparse(union(idVariables,timeVariable)),'))')
      )
  }
  
  #
  # Check which columns vary and return those column names
  otherColumns=setdiff(names(df),union(idVariables,timeVariable))
  varyingColumns = df %>% group_by_at(idVariables) %>%
    dplyr::mutate_at(otherColumns,n_distinct) %>% 
    as.data.frame %>%
    dplyr::select(-one_of(union(idVariables,timeVariable))) %>%
    dplyr::select(which(colMeans(.) > 1)) %>%
    names()

  return(varyingColumns)
  
  # example
  # for reshape, cherck output columns vary by id variables
  # example:
  # idVariables=c("recordedFile")
  # timeVariable=c("Syllable")
  # both.wide=stats::reshape(both,
  #                          idvar=idVariable,
  #                          timevar=timeVariable,
  #                          v.names=whichColumnsVary(both,idVariables,timeVariable),
  #                          direction="wide")
  #
}   

# check uniqueness of id columns
checkUniqueness <- function(df,idVariables=c('experiment','item','condition','participant','ioiLabel')){
  
  # add filename to the output if it's a column
  if ("recordedFile" %in% colnames(df)) {
    idVariables = union(idVariables,"recordedFile")
  }
  if ("fileName" %in% colnames(df)) {
    idVariables = union(idVariables,"fileName")
  }
  
  nonUniqueRows = df %>%
    group_by_at(idVariables) %>%
    summarise(count = n()) %>%
    filter(count>1) %>%
    as.data.frame()
  
  return(nonUniqueRows)
  
}


convertVariables <- function(df) {
  # columns that are usually read as factors but should be numeric:
  numericColMatlab = c("trialDuration")
  
  numericColPraatscript = c("rIntensity","rPitch","rDuration","duration", "silence", "duraSil", "phoneLength", "meanPitch", "maxPitch", "maxPitTime", "minPitch", "minPitTime", "pitch1", "pitch1_time", "pitch2", "pitch2_time", "pitch3", "pitch3_time", "pitch4", "pitch4_time", "pitch5", "pitch5_time", "pitch6", "pitch6_time", "pitch7", "pitch7_time", "pitch8", "pitch8_time", "pitch9", "pitch9_time", "pitch10", "pitch10_time", "meanIntensity", "maxIntensity", "maxIntTime", "intensity1", "intensity1_time", "intensity2", "intensity2_time", "intensity3", "intensity3_time", "intensity4", "intensity4_time", "intensity5", "intensity5_time", "intensity6", "intensity6_time", "intensity7", "intensity7_time", "intensity8", "intensity8_time", "intensity9", "intensity9_time", "intensity10", "intensity10_time", "zstart", "zend", "zDuration", "zPhonelength", "zmeanPitch", "zmaxPitch", "zmaxPitTime", "zminPitch", "zminPitTime", "zmeanIntensity", "zmaxIntensity", "zmaxIntTime", "response", "duration", "silence", "durasil", "meanpitch", "maxpitch", "maxPitTime", "minPitch", "minPitTime", "firstpitch", "secondpitch", "thirdpitch", "fourthpitch", "meanIntensity", "maxIntensity", "zduration", "zbeginzone", "zendzone", "zphonelength", "zmeanpitch", "zmaxpitch", "zmaxPitTime", "zminPitch", "zminPitTime", "zfirstpitch", "zsecondpitch", "zthirdpitch", "zfourthpitch", "zmeanIntensity", "zmaxIntensity", "durasil", "meanpit", "maxpitch", "maxPitTime", "minpitch", "minPitTime", "firstpitch", "secondpitch", "thirdpitch", "fourthpitch", "meanIntensity", "maxIntensity", "firstF1", "firstF2", "firstdif", "secondF1", "secondF2", "seconddif", "thirdF1", "thirdF2", "thirddif", "fourthF1", "fourthF2", "fourthdif", "fifthF1", "fifthF2", "fifthdif")
  
  numericColJspsychExperimenter = c("trial_index","time_elapsed","rt","correct","headPhoneScreenerScore") 
  
  numeriColOther = c("F1","F2")
  
  numericCols = c(numericColMatlab,numericColPraatscript, numericColJspsychExperimenter)
  
  nColumns = ncol(df)
  # convert to numeric column, otherwise treat as factor:
  for (i in 1:nColumns) {
    if (colnames(df)[i] %in% numericCols) {
      df[, i] <- as.numeric(as.character(df[, i]))
    } else {
      df[, i] <- as.factor(as.character(df[, i]))
    }
  }
  return(df)
}


convertColumnsExperimenter <- function(df) {
  
  # these columns will be coded as numeric, all others as factors:
  numericColumnsExperimenter = c("trial_index","time_elapsed","rt","correct","headPhoneScreenerScore",
                                 "birth.year",
                                 "firstStartage","firstSpeaking","firstUnderstanding",
                                 "secondStartage","secondSpeaking","secondUnderstanding",
                                 "thirdStartage","thirdSpeaking","thirdUnderstanding",
                                 "fourthStartage","fourthSpeaking","fourthUnderstanding"
  ) 
  
  # convert to numeric column, otherwise treat as factor:
  for (i in 1:ncol(df)) {
    if (colnames(df)[i] %in% numericColumnsExperimenter) {
      df[, i] <- as.numeric(as.character(df[, i]))
    } else {
      df[, i] <- as.factor(as.character(df[, i]))
    }
  }
  return(df)
}


# imports json files and process them, returning a list with two data frames
# one called experimentSettings
# one with the data called experimentTrials

importData <- function(partsToKeep=c('question1','question1'),
                       experimentFolder='.',
                       pathStimulusFile,
                       pathData='data') {
  
  require(jsonlite)
  require(tidyverse)
  
  pathData = paste0(experimentFolder,'/',pathData)
  
  # determine name of stimulusFile based on index.html file
  if (missing(pathStimulusFile)) {
    #
    # load index.html file
    
    pathStimulusFile = readLines(paste0(experimentFolder,'/../index.html'))
    # keep only the line with the text we're looking for
    pathStimulusFile <- pathStimulusFile[grepl(pattern = "  stimulusFile: ", x = pathStimulusFile, fixed = TRUE)]
    # extract stimulusFile name:
    pathStimulusFile  = paste0(regmatches(pathStimulusFile, gregexpr("(?<=\')(.*?)(?=\')", pathStimulusFile, perl = TRUE)))
    # doesn't work:
    #gsub(".*\'|\'.*", "", stimulusFile)
    #str_extract(stimulusFile, "\'.*?\'")
  }
  
  
  # import experiment spreadsheet and turn columns into factors
  studyFile = read.csv(paste0(experimentFolder,'/',pathStimulusFile),
                       sep="\t", header=TRUE) %>% convertColumnsExperimenter()
  
  # create a list of the "data*"  files from your target directory
  fileList <- list.files(path=pathData,pattern="data*")
  # keep only .json files
  fileList  = Filter(function(x) grepl(".json", x), fileList)
  
  d <- data.frame()
  
  # load in  data files  from all participants
  for (i in 1:length(fileList)){
    #print(fileList[i])
    #paste0('data/',fileList[i])
    tempData = fromJSON(paste0(pathData,'/',fileList[i]), flatten=TRUE)
    
    # this line replaces NA for experiments where due to a bug components other than the test trials didn't have participant number added ot them
    tempData$participant = unique(tempData$participant[!is.na(tempData$participant)])
    tempData$pList = as.character(tempData$pList)
    d <- bind_rows(d,tempData)
  }
  
  # tempData = fromJSON(paste0(pathData,'/','data_5f87791cf3c64e1a1f313d66.json'), flatten=TRUE)
  
  # initiate data frame  with participant information
  participants <- data.frame(participant = unique(d$participant))
  
  # questionnaire data:
  # how to convert json cell into  columns (there might be an easier way using  jsonlite more directly?): https://stackoverflow.com/questions/41988928/how-to-parse-json-in-a-dataframe-column-using-r
  
  
  #  extract study parameters into separate variable
  #  only select existing columns:
  
  selectColumns = c("path", "stimulusFile", "testRun", "language", "logFile", "soundCheckFile", 
                    "recordingTimeOut", "completionLink", "completionCode", "pListMethod", 
                    "participantCodeMethod", "displayDataAfterFinish", "showProgressBar", 
                    "fullScreen", "hello", "consent", "languageQuestionnaire", "soundCheck", 
                    "micCheck", "headphoneScreener.includeHeadphoneScreener", 
                    "headphoneScreener.stimuli", "headphoneScreener.numberChances", 
                    "headphoneScreener.threshold", "headphoneScreener.excludeOnFail", 
                    "headphoneScreener.completionFailLink", "headphoneScreener.failMessage", "experimentSessions", "postExperimentQuestionnaire", 
                    "musicQuestionnaire", "goodbye", "experimentOnly")
  
  selectColumns = intersect(selectColumns,colnames(d))
  
  #
  if (nrow(filter(d,component=='experimentSettings'))!=0){
    experimentSettings <- d %>% 
      filter(component=='experimentSettings') %>% 
      dplyr::select(all_of(selectColumns))
    d = d %>%  dplyr::select(-all_of(selectColumns))
  }
  
  #  add post-experiment questionnaire data to participant data frame:
  if (nrow(filter(d,component=='Post-experiment Questionnaire'))!=0){
    participants <- d %>% 
      filter(component=='Post-experiment Questionnaire') %>% 
      dplyr::select(c(participant,responses)) %>%
      mutate(responses = map(responses, ~ fromJSON(.) %>% 
                               as.data.frame())) %>% 
      unnest(responses) %>% 
      right_join(participants, by = c("participant"))
  }
  
  #  add music  questionnaire data to participant data frame:
  if (nrow(filter(d,component=='Music Questionnaire'))!=0){
    participants <- d %>% 
      filter(component=='Music Questionnaire') %>% 
      dplyr::select(c(participant,responses)) %>%
      mutate(responses = map(responses, ~ fromJSON(.) %>% 
                               as.data.frame())) %>% 
      unnest(responses) %>% 
      right_join(participants, by = c("participant"))
  }
  
  # add language questionnaire to participant data frame
  if (nrow(filter(d,component=='Language Questionnaire'))!=0){
    participants <- d %>% 
      filter(component=='Language Questionnaire') %>% 
      dplyr::select(c(participant,responses)) %>%
      mutate(responses = map(responses, ~ fromJSON(.) %>% 
                               as.data.frame())) %>% 
      unnest(responses) %>% 
      right_join(participants, by = c("participant"))
  }
  
  # head headphone screener to participant data frame
  if (nrow(filter(d,component=='Headphone screener'))!=0){
    participants = d %>% 
      filter(component=='Headphone screener'&grepl("Headphone screener question",trialPart)) %>%
      mutate(correct = as.numeric(as.character(correct))) %>%
      group_by(participant) %>%
      summarise(headPhoneScreenerScore=mean(correct)) %>%
      as.data.frame %>%
      right_join(participants, by = c("participant"))
  }
  
  
  # process experimental results, keeping only specified trialparts:
  
  # questions were missing component specification in early experiments:
  d$component[d$trialPart=='question1'&is.na(d$component)]='experiment'
  
  # goal: get all information for a trial on a single line
  
  experimentTrials = d %>% 
    filter(trialPart%in%partsToKeep)
  
  experimentTrials <- experimentTrials %>%
    # combine  with participant information
    right_join(participants,by = c("participant")) %>%
    # turn empty strings (e.g., "",  '',  "  ") into NA
    apply(2, function(x) gsub("^$|^ $", NA, x))  %>%
    as.data.frame %>% convertColumnsExperimenter()
  
  experimentTrials = left_join(experimentTrials,studyFile,by=c("experiment","item","condition")) ## %>%
  #filter(!is.na(chosenOption))
  
  returnList = list("settings" = experimentSettings, "data" =  experimentTrials)
  return(returnList)
  
}


reportComparison = function(model,factorName) {
  # assumes  that last  coefficiient is p-value
  # e.g. from  lmertest for lmer
  nCoefficients = length(colnames(summary(model)$coefficients))
  pValName = colnames(summary(model)$coefficients)[nCoefficients]
  
  output = paste0(
    "$\\beta$ = ", round(coef(summary(model))[factorName,'Estimate'], 2), # β
    "; s.e. = ", round(coef(summary(model))[factorName,'Std. Error'], 2),
    "; p $<$ ", max(round(coef(summary(model))[factorName,pValName], 2),0.001)
  )
  
  return(enc2utf8(enc2native(output)))
}


getParticipantInformation <- function(participantNumbers){
  
  pathLQ = '/Users/chael/Dropbox/Lab/participants/processedData/lq_shortform_2019.txt'
  lq = read.csv(pathLQ, sep='\t')  %>% 
    filter(participant %in% participantNumbers) %>%
    mutate(participant = factor(participant)) %>%
    dplyr::select("participant", "Timestamp", "Participant", "BirthYear", "Gender", 
                  "Country", "Region", "StateProvince", "City", "Second", "Third", 
                  "Fourth", "French", "French.Which", "French.Fluency", "French.Understanding", 
                  "English", "English.Which", "English.Fluency", "English.Understanding", "NewCorpusConsent", 
                  "FrenchType", "EnglishType")
  
  return(lq)
  
}


addLanguageQuestionnaire = function(dataSet){
  dataset = dataSet %>%
    left_join(getParticipantInformation(dataSet$participant), by = c("participant")) %>%
    convertVariables()
}

getMusicQuestionnaire = function(participantNumbers){
  
  mq = read.csv("/Users/chael/Dropbox/lab/participants/lqArchived/mq_saved_nov_2020.tsv",sep='\t') %>%
    convertVariables()
  
  names(mq)[names(mq) == 'Participant..'] <- 'participant'
  
  mq$How.much.do.you.know.about.music.structure.and.theory. = 
    factor(mq$How.much.do.you.know.about.music.structure.and.theory. ,
           levels = c("Nothing", "A little", "A moderate amouunt", "A fair amount", "A great deal")
    )
  
  mq$How.many.years.of.formal.music.training..practice..have.you.had. = 
    factor(mq$How.many.years.of.formal.music.training..practice..have.you.had.,
           levels = c("None", "1 year",  
                      "2 years", "3 years", "4 years", "5 years", "6 years", "7 years", 
                      "8 years", "9 years", "10+ years")
    )
  
  mq$YearsTrainingNumeric = dplyr::recode(mq$How.many.years.of.formal.music.training..practice..have.you.had.,
                                          "None" = 0, 
                                          "1 year" = 1,
                                          "2 years" = 2, 
                                          "3 years" = 3, 
                                          "4 years" = 4, 
                                          "5 years" = 5, 
                                          "6 years" = 6, 
                                          "7 years" = 7, 
                                          "8 years" = 8, 
                                          "9 years" = 9, 
                                          "10+ years" = 10
  )
  
  # zscore of the years someone had music lessons based on 
  mq$YearsTrainingScaled = arm::rescale(as.numeric(mq$How.many.years.of.formal.music.training..practice..have.you.had.))
  
  mq$How.often.do.you.engage.in.professional.music.making..e.g..singing..playing.an.instrument..composing.. = 
    factor(mq$How.often.do.you.engage.in.professional.music.making..e.g..singing..playing.an.instrument..composing..,
           levels = c("Never", "Rarely", "Sometimes","Often", "All the time")
    )
  
  mq$How.often.did.you.or.do.you.practice.or.rehearse.with.an.instrument.or.singing. = 
    factor(mq$How.often.did.you.or.do.you.practice.or.rehearse.with.an.instrument.or.singing.,
           levels = c("Never", "Rarely", "Sometimes","Often", "All the time")
    )
  
  mq$How.often.do.you.engage.in.music.making.as.a.hobby.or.as.an.amateur. = 
    factor(mq$How.often.do.you.engage.in.music.making.as.a.hobby.or.as.an.amateur.,
           levels = c("Never", "Rarely", "Sometimes","Often", "All the time")
    ) 
  
  
  return(mq %>% filter(participant %in% participantNumbers))
  
}

addMusicQuestionnaire = function(dataSet){
  dataset =   dataSet %>%
    left_join(getMusicQuestionnaire(dataSet$participant), by = c("participant")) %>%
    convertVariables()
  
}


addAnnotation = function(df,fileName,identVariables){
  if (missing(identVariables)){
    identVariables = c("recordedFile")
  } 
  annotationDF = read.csv(fileName,sep='\t')
  df = df %>% 
    left_join(annotationDF, by = identVariables) %>%
    convertVariables()
  
  return(df)
}
# example: d=addAnnotation(d,'dataAcoustics/homphInitialSabrina.txt','recordedFile')


  
addAcoustics = function(df,acousticsFilename,idvariable=c('experiment','item','condition','participant'),timevariable='ioiLabel'){
  
  require("tidyverse")
  options(dplyr.summarise.inform = FALSE)
  
  acoustics = read.csv(acousticsFilename,sep='\t') %>% convertVariables()
  
  # check if old extraAcoustics script was used, if so change default timevariable to "woiLabel"  
  if (timevariable == 'ioiLabel'){
    if ("woiLabel" %in% colnames(acoustics)) {
      timevariable = 'woiLabel'
    }
    if ("woilabel" %in% colnames(acoustics)) {
      timevariable = 'woilabel'
    }
  }

  # correct column name for soundfilename in acoustics file if necessary
  if ('recordedFile' %in% idvariable){
    names(acoustics)[names(acoustics) == 'fileName'] <- 'recordedFile'
  }
  
  # create "recordedFile" column in experiment spreadsheet if necessary
  if (!("recordedFile" %in% colnames(df))) {
    df$recordedFile = paste0(df$experiment,"_",df$participant,"_",df$item,"_",df$condition,".wav")
  }
  
  acoustics = acoustics  %>% 
    convertVariables() %>%
    # filter out lines without ioiLabel
    filter(!is.na({{timevariable}})&!({{timevariable}}=="")) %>% 
    stats::reshape(idvar=idvariable,
                   timevar=timevariable,
                   v.names=whichColumnsVary(acoustics,idvariable,timevariable),
                   direction="wide")
  
  df = df %>%
    convertVariables() %>%
    left_join(acoustics, by = idvariable)
  
  return(df)
}


relativeMeasures = function(df,woi1, woi2,label="") {
 
  labelPitch = paste0("rPitch",label)
  labelDuration = paste0("rDuration",label)
  labelIntensity = paste0("rIntensity",label)
  
  # Relative rations
  # semitones:
  df[labelPitch] = 12*log2(df[,paste0('maxPitch.',woi1)]/ df[,paste0('maxPitch.',woi2)])
  # ratio of durations (difference in log duration):
  df[labelDuration] = log(df[,paste0('duration.',woi1)]) - log(df[,paste0('duration.',woi2)])
  # ratio of loudness (difference of dB):
  df[labelIntensity] = log(df[,paste0('maxIntensity.',woi1)]) - log(df[,paste0('maxIntensity.',woi2)])
  
  # Relative measures
  #df$rPitch=12*log2(df$maxPitch.1/df$maxPitch.2)
  # Relative duration (difference in log duration)
  #df$rDuration=log(df$duration.1)-log(df$duration.2)
  # Relative intensity (difference)
  #df$rIntensity=df$maxIntensity.1-df$maxIntensity.2
  
  return(df)
}


# helmert coding

helmertContrasts <- function(df,column,contrastLabels) {
  # df = data frame, column: column to be helmert coded
  # contrasts: name of the Helmert contrasts
  # function assumes that coding will be level 1 vs. later, level 2 vs. later, etc.
  
  if (length(levels(df[[column]]))!=(length(contrastLabels)+1)){
    cntr=NA
  } else {
    # cre  ate helmert coding based on reverse order of levels
    # so that it's level 1 vs. later, level 2 vs. later, etc.
    cntr = contr.helmert(rev(levels(df[[column]])))
    # change ordering of rows and columns to fit with left-to-right definition of contrasts
    cntr = cntr[nrow(cntr):1,ncol(cntr):1]
    # label contrasts
    colnames(cntr) = contrastLabels
    # change each column so that differences between max and min is 1
    # and such that first part of each constrast is negative
    for (x in 1:ncol(cntr)) {
      cntr[,x] = cntr[,x] / -(ncol(cntr) + 2 - x)
    } 
  } 
  # 
  cat("\n Helmert contrasts for variable ",column,":\n\n")
  print(cntr)
  #
  return(cntr)
}


addHelmertPredictors <- function(df,column,contrastLabels) {
  # add variables for each individual helmert contrast
  # get helmert contrasts:
  cntr = helmertContrasts(df,column,contrastLabels)
  # code contrasts:
  contrasts(df[[column]]) = cntr
  # uses model.matrix.lm in order to be sure number of rows is correct
  # even if there are NA in the dependent variable
  for (i in 1:length(contrastLabels)){
    df[[contrastLabels[i]]] = (model.matrix.lm(formula(paste("~ ",column)),df,na.action=na.pass)[,i+1])
  }
  #
  # print out formula snippet for model
  # new variables for contrasts created, here's some code for a potential model:
  labelString = paste0(contrastLabels, collapse = " + ")
  cat("\n data frame with added columns in $df\n ")
  cat("\n Potential code snippet for model:\n\n ")
  formulaPredictors =  paste(labelString, " + (",labelString,"||item) + (",labelString,"||participant)")
  formulaDoublePipe =  paste(labelString, " + (",labelString,"|item) + (",labelString,"|participant)")
  cat('$formulaPredictors: ', formulaPredictors,"\n ")
  cat('$formulaDoublePipe: ', formulaDoublePipe,"\n\n ")
  
  returnList = list("df" = df, 
                    "formulaPredictors" =  formulaPredictors,
                    "formulaDoublePipe" =  formulaDoublePipe
                    )
  return(returnList)
  
}


# example use:

# helmertCoded = addHelmertPredictors(
#   df,
#   "Construction",
#   c(
#     "TrueControl.vs.other",
#     "False.vs.maybe",
#     "Uninverted.vs.otherMaybe",
#     "Inverted.vs.clefts",
#     "ItCleft.vs.PseudoCleft"
#   )
#   )
# 
# d = helmertCoded$df
# formulaPredictors = helmertCoded$formulaPredictors
# formulaDoublePipe = helmertCoded$formulaDoublePipe