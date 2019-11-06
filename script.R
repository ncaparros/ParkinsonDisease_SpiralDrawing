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

completeDf <- data.frame()


patients <- df %>% select(patientID, isPwp) %>% distinct(patientID, isPwp)

for(test in seq(0, by=1, length=3)){
  for(indPatient in seq(1, by=1, length=nrow(patients))){
    temp_df <- df %>% filter(TestID == test & patientID==patients[indPatient,]$patientID) %>% arrange(Timestamp)
    initialTimestamp = temp_df[1,]$Timestamp
    temp_df <- temp_df %>% mutate(Timestamp = Timestamp - initialTimestamp)
    completeDf <- rbind(completeDf, temp_df)
  }
}
#---------------
#Visualizing images
#---------------

Healthies <- df %>% 
  filter(isPwp == FALSE)

Pwps <- df %>% 
  filter(isPwp == TRUE)

exampleHealthyStatic <- Healthies %>% 
  filter(patientID == first(patientID) & 
           TestID == 0) %>% 
  select(X, Y, Pressure)

examplePWPStatic <- Pwps %>% 
  filter(patientID == first(patientID) & 
           TestID == 0) %>% 
  select(X, Y, Pressure)

mHealthyStatic <- matrix(0,nrow = 500, ncol = 500)

for(i in seq(1, by=1, length=nrow(exampleHealthyStatic))){
  
  mHealthyStatic[exampleHealthyStatic[i,]$X, exampleHealthyStatic[i,]$Y] = exampleHealthyStatic[i,]$Pressure
}

image(mHealthyStatic)


mPwpStatic <- matrix(0,nrow = 500, ncol = 500)

for(i in seq(1, by=1, length=nrow(examplePWPStatic))){
  
  mPwpStatic[examplePWPStatic[i,]$X, examplePWPStatic[i,]$Y] = examplePWPStatic[i,]$Pressure
}

image(mPwpStatic)

#---------------
#Data cleaning and additionnal info
#---------------
#get patients ID



AnalyseDf <- data.frame()

#Add distance, speed and acceleration

for(test in seq(0,by=1,length=3)){
    for(index in seq(1, by=1, length=nrow(patients))){
      id <- patients[index,]$patientID
      temp <- completeDf %>% filter(patientID == id & TestID == test) %>% arrange(Timestamp) %>% mutate(DistXY=0, DistXYZ=0)
      
      for(row in seq(2, by=1, length=nrow(temp))){
        temp[row,]$DistXY <- sqrt((temp[row,]$X - temp[row-1,]$X)^2 + (temp[row,]$Y - temp[row-1,]$Y)^2)
        temp[row,]$DistXYZ <- sqrt((temp[row,]$X - temp[row-1,]$X)^2 + (temp[row,]$Y - temp[row-1,]$Y)^2 + (temp[row,]$Z - temp[row-1,]$Z)^2)
      }
      AnalyseDf <- rbind(AnalyseDf, temp)
    }
}


resultsDf <- patients %>% mutate(totalDistXY0 = 0, totalDistXYZ0=0, meanSpeed0 = 0, sdSpeed0 = 0,
                                 totalDistXY1 = 0, totalDistXYZ1=0, meanSpeed0 = 0, sdSpeed1 = 0,
                                 totalDistXY2 = 0, totalDistXYZ2=0, meanSpeed2 = 0, sdSpeed2 = 0)
for(test in seq(0, by=1, length=3)){
  for(index in seq(1, by=1, length=nrow(resultsDf))){
    patientId <- resultsDf[index,]$patientID
    temp <- AnalyseDf %>% filter(TestID == test & patientID == patientId)
    resultsDf[index,]["totalDistXY" + index] = sum(temp$DistXY)
    resultsDf[index,]["totalDistXYZ" + index] = sum(temp$DistXYZ)
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
