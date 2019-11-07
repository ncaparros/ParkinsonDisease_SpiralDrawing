#---------------
#Libraries
#---------------

if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
if(!require(ids)) install.packages("ids", repos = "http://cran.us.r-project.org")
if(!require(tidyr)) install.packages("tidyr", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(anytime)) install.packages("anytime", repos = "http://cran.us.r-project.org")

#---------------
#Read data
#---------------

filePathCtrl = "data/hw_dataset/control/"

fileNamesCtrl <- list.files(path = filePathCtrl)

controls <- do.call(rbind, lapply(fileNamesCtrl, function(x) {
  #generate a random id for the patient
  randID <- proquint_to_int(proquint(), as = "numeric")
  #add id and isPwp as column
  t <- cbind(read.table( file = paste(filePathCtrl,
                                   x, 
                                   sep="")), 
              patientID = randID, 
              isPwp = FALSE)
  }))

filePathPwp = "data/hw_dataset/parkinson/"

fileNamesPwp <- list.files(path = filePathPwp)

pwp <- do.call(rbind, lapply(fileNamesPwp, function(x) {
  #generate a random id for the patient
  randID <- proquint_to_int(proquint(), as = "numeric")
  #add id and isPwp as column
  t <- cbind(read.table( file = paste(filePathPwp,
                                      x, 
                                      sep="")), 
             patientID = randID, 
             isPwp = TRUE)
}))

df <- rbind(controls, pwp)

#---------------
#Prepare Data
#---------------

df <- df %>% separate(V1, c("X", "Y", "Z", "Pressure", "GripAngle","Timestamp", "TestID"),";")

df <- df %>% mutate(X = as.numeric(X),
                    Y = as.numeric(Y),
                    Z = as.numeric(Y),
                    Pressure = as.numeric(Pressure),
                    GripAngle = as.numeric(GripAngle),
                    Timestamp = as.numeric(Timestamp))

#delete useless column Z

nrow(df)

df %>% filter(Y==Z) %>% nrow()

(df %>% filter(Z==Y) %>% nrow()) == nrow(df)

df <- df %>% select(-Z)

completeDf <- data.frame()


patients <- df %>% select(patientID, isPwp) %>% distinct(patientID, isPwp) %>% mutate(test0 = FALSE, 
                                                                                      test1 = FALSE, 
                                                                                      test2 = FALSE, 
                                                                                      id0 = 0,
                                                                                      id1 = 0,
                                                                                      id2 = 0)

for(test in seq(0, by=1, length=3)){
  for(indPatient in seq(1, by=1, length=nrow(patients))){
    temp_df <- df %>% 
      filter(TestID == test & 
               patientID==patients[indPatient,]$patientID) %>% 
      arrange(Timestamp)
    
    initialTimestamp = temp_df[1,]$Timestamp
    
    temp_df <- temp_df %>%
      mutate(Timestamp = Timestamp - initialTimestamp)
    
    completeDf <- rbind(completeDf, temp_df)
  }
}

#---------------
#Preparing matrices in list
#---------------

matricesTest0 <- list()
matricesTest1 <- list()
matricesTest2 <- list()

for(test in seq(0, by=1, length=3)){
  
  maxX <- df %>% filter(TestID == test) %>% pull(X) %>% max()
  maxY <- df %>% filter(TestID == test) %>% pull(Y) %>% max()
  maxDim <- max(maxX, maxY)
  
  for(indPatient in seq(1, by=1, length=nrow(patients))){
    patId <- patients[indPatient,]$patientID
    temp_df <- df %>% 
      filter(TestID == test & 
               patientID == patId) %>% 
      arrange(Timestamp)
    
    temp_mat <- matrix(-1, nrow=maxDim +1, ncol = maxDim +1)
    
    temp_mat[temp_df[1,]$Y, temp_df[1,]$X] = temp_df[1,]$Pressure
    
    n <- 1
    sum <- temp_df[1,]$Pressure
    
    for(i in seq(2, by=1, length=nrow(temp_df))){
      
      if(temp_df[i,]$X == temp_df[i - 1,]$X & temp_df[i,]$Y == temp_df[i - 1,]$Y & i < nrow(temp_df))
      {
        n <- n + 1
        sum <- sum + temp_df[i,]$Pressure
      }
      else if(i < nrow(temp_df))
      {
        temp_mat[temp_df[i - 1,]$Y, temp_df[i - 1,]$X] = sum/n
        n <- 1
        sum = temp_df[i,]$Pressure
      }
      else
      {
        temp_mat[temp_df[i,]$Y, temp_df[i,]$X] = temp_df[i,]$Pressure
      }
    }
    
    if(test == 0){
      
      matricesTest0[[paste("id", patId, sep="")]] <- temp_mat
      patients[indPatient,]$test0 <- TRUE
      patients[indPatient,]$id0 <- indPatient
      
    } else if(test == 1){
      
      matricesTest1[[paste("id", patId, sep="")]] <- temp_mat
      patients[indPatient,]$test1 <- TRUE
      patients[indPatient,]$id1 <- indPatient
      
    }else {
      
      matricesTest2[[paste("id", patId, sep="")]] <- temp_mat
      patients[indPatient,]$test2 <- TRUE
      patients[indPatient,]$id2 <- indPatient
      
    }
    
  }
}

#---------------
#Visualizing images
#---------------

Healthies <- df %>% 
  filter(isPwp == FALSE)

Pwps <- df %>% 
  filter(isPwp == TRUE)



#---------------
#Data cleaning and additionnal info
#---------------
#get patients ID



AnalyseDf <- data.frame()

#Add distance, speed and acceleration

for(test in seq(0,by=1,length=3)){
    for(index in seq(1, by=1, length=nrow(patients))){
      id <- patients[index,]$patientID
      temp <- completeDf %>% 
        filter(patientID == id & TestID == test) %>% 
        arrange(Timestamp) %>% 
        mutate(DistXY=0, CumulDistXY=0)
      
      for(row in seq(2, by=1, length=nrow(temp))){
        temp[row,]$DistXY <- sqrt((temp[row,]$X - temp[row-1,]$X)^2 + (temp[row,]$Y - temp[row-1,]$Y)^2)
        temp[row,]$CumulDistXY <- temp[row - 1,]$CumulDistXY + temp[row,]$DistXY
      }
      AnalyseDf <- rbind(AnalyseDf, temp)
    }
}


resultsDf <- patients %>% mutate(totalDistXY0 = 0, time0 = 0, meanSpeed0 = 0, sdSpeed0 = 0,
                                 totalDistXY1 = 0, time1 = 0, meanSpeed0 = 0, sdSpeed1 = 0,
                                 totalDistXY2 = 0, time2 = 0, meanSpeed2 = 0, sdSpeed2 = 0,
                                 meanPressure0 = 0, meanGripAngle0=0, sdGripAngle0=0,
                                 meanPressure1 = 0, meanGripAngle1=0, sdGripAngle1=0,
                                 meanPressure2 = 0, meanGripAngle2=0, sdGripAngle2=0)
for(test in seq(0, by=1, length=3)){
  for(index in seq(1, by=1, length=nrow(resultsDf))){
    patientId <- resultsDf[index,]$patientID
    temp <- AnalyseDf %>% filter(TestID == test & patientID == patientId)
    resultsDf[index,][paste("totalDistXY", test, sep="")] <- sum(temp$DistXY)
    resultsDf[index,][paste("time", test, sep="")] <- max(temp$Timestamp)
    resultsDf[index,][paste("meanSpeed", test, sep="")] <- mean(temp$DistXY)
    resultsDf[index,][paste("sdSpeed", test, sep="")] <- sd(temp$DistXY)
    resultsDf[index,][paste("meanPressure", test, sep="")] <- mean(temp$Pressure)
    resultsDf[index,][paste("sdPressure", test, sep="")] <- sd(temp$Pressure)
    resultsDf[index,][paste("meanGripAngle", test, sep="")] <- mean(temp$GripAngle)
    resultsDf[index,][paste("sdGripAngle", test, sep="")] <- sd(temp$GripAngle)
  }
}


#Add speed


#---------------
#Create training and validation sets with the ids of the patients and the isPwp (is Person With Parkinson) value
#---------------



test_index <- createDataPartition(y = patients$isPwp, 
                    times = 1, 
                    p = 0.1, 
                    list = FALSE)

training_Patients <- patients[-test_index,]
testing_Patients <- patients[test_index,]


#---------------
#Analizing datas
#Test 1 Static spiral
#---------------

dfTest1 <- df %>% filter(TestID == 0)

#---------------
#Analizing datas
#Test 2 Dynamic spiral
#---------------

#---------------
#Analizing datas
#Test 3 Stability Test on Certain Point
#Sd of X, Y + Pressure >0
#---------------
