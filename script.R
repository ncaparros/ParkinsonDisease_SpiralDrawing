#---------------
#Libraries
#---------------

if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
if(!require(ids)) install.packages("ids", repos = "http://cran.us.r-project.org")
if(!require(tidyr)) install.packages("tidyr", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(anytime)) install.packages("anytime", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(geiger)) install.packages("geiger", repos = "http://cran.us.r-project.org")
if(!require(ggpubr)) install.packages("ggpubr", repos = "http://cran.us.r-project.org")

#---------------
#Read data from cloned GitHub repository
#---------------
#If the directory "data" exists in the working directory, we can assume that the github repository has been cloned. 
#The github repository contains the full dataset, with the small modifications (files moved from on directory to an other). 
#There is no need to download the data again.

#If the directory does not exists, we can assume that only the .R (and/or .Rmd) has been downloaded. 
#We need to download the dataset, which contains three folders (hw_dataset, hw_drawing, new_dataset).
#We need to move the files from new_dataset/parkinson into the hw_dataset/parkinson in order to have all the datas together.
#We will not be using the drawings contained in the hw_drawings folder since the datas are corresponding to the ones in
#hw_dataset/parkinson.

#In each case, the files are listed, then loaded, read, and stored into two datasets : controls and pwp, before being binded into 
#a single dataset df. A random patientID is set for each patient, and the boolean isPwp is added (true if healthy, false if parkinson)

#To minimize code, we set a createDf function which read the files and set the df data.frame.

createDf <- function(){
  
  #File path to the directory containing the datas of the healthy people (controls)
  filePathCtrl = "data/hw_dataset/control/"
  
  #Getting the name of all the files in the directory in order to load the datas
  fileNamesCtrl <- list.files(path = filePathCtrl)
  
  #Reading the data for all the control files
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
  
  #File path to the directory containing the datas of the people with Parkinson (PWP)
  filePathPwp = "data/hw_dataset/parkinson/"
  
  #Getting the name of all the files in the directory in order to load the datas
  fileNamesPwp <- list.files(path = filePathPwp)
  
  #Reading the data for all the Parkinson files
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
  
  #merge controls and pwp df into a single data.frame
  df <- rbind(controls, pwp)
  
  return(df)
}


if(dir.exists("data")){
  #First case : the data already exists on the working directory

  df <- createDf()
  
}else{
    #Second case : the data does not exist on the working directory
    
    #Names for : the created temporary directory, the directory we need to move, main the directory
    dirpath = "data"
    dirToMove = "new_dataset/parkinson"
    dirToKeep = "hw_dataset/parkinson"
    
    #download the .zip containing the datasets from UCI
    dl <- tempfile()
    download.file("https://archive.ics.uci.edu/ml/machine-learning-databases/00395/PARKINSON_HW.zip", dl)
    
    #unziping the archive
    unzip(dl, exdir="data")
    
    #Copy the files from the new_dataset/parkinson into the main directory hw_dataset/parkinson
    sapply(c(list.files(paste0(dirpath,"/",dirToMove))), function(file){
      file.copy(paste0(dirpath,"/",dirToMove,"/",file), dirToKeep)
    })
    
    #building the data frame
    df <- createDf()
    
    #Removing the temporary directory
    unlink(dirpath, recursive = TRUE)
}


#---------------
#Prepare Data
#---------------

df <- df %>% separate(V1, c("X", "Y", "Z", "Pressure", "GripAngle","Timestamp", "TestID"),";")

df <- df %>% mutate(X = as.numeric(X),
                    Y = as.numeric(Y),
                    Z = as.numeric(Z),
                    Pressure = as.numeric(Pressure),
                    GripAngle = as.numeric(GripAngle),
                    Timestamp = as.numeric(Timestamp))

patients <- df %>% select(patientID, isPwp) %>% distinct(patientID, isPwp)


completeDf <- data.frame()

#For each of the tests "test"
for(test in seq(0, by=1, length=3)){
  
  #For each of the patients "indPatient"
  for(indPatient in seq(1, by=1, length=nrow(patients))){
    
    #Create a temporary data frame for patient "indPatient" and test "test"
    temp_df <- df %>% 
      filter(TestID == test & 
               patientID == patients[indPatient,]$patientID) %>% 
      arrange(Timestamp)
    
    #Get first value of timestamp
    initialTimestamp = temp_df[1,]$Timestamp
    
    #Mutate Timestamp so that the very first value of Timestamp for patient "indPatient" 
    #and test "test" is equal to 0
    temp_df <- temp_df %>%
      mutate(Timestamp = Timestamp - initialTimestamp)
    
    #Bind temporary data frame to complete data frame
    completeDf <- rbind(completeDf, temp_df)
    
    
  }
}

ratiosDf <- df %>% 
  group_by(patientID, TestID) %>% 
  summarize(ratio=(max(X)-min(X))/(max(Y)-min(Y)), 
            distY= max(Y)-min(Y), 
            distX = max(X)-min(X), 
            isPwp = first(isPwp))

ratiosDf %>% filter(TestID != 2) %>% 
  ggplot() +
  geom_bar(aes(ratio, fill=TestID), stat="bin")

ratiosDf %>% ggplot() + geom_histogram(aes(TestID, fill=isPwp), stat="count")

ratiosDf %>% filter(TestID==0) %>% nrow

ratiosDf %>% filter(TestID==1) %>% nrow

ratiosDf %>% filter(TestID==2) %>% nrow


ratiosDf %>% filter(TestID != 2) %>% 
  ggplot() +
  geom_bar(aes(ratio), stat="bin")

ratiosDf %>% filter(TestID == 2) %>% 
  ggplot()+
  geom_bar(aes(ratio), stat="bin")


completeCalibratedDf <- data.frame()

for(test in seq(0, by=1, length=3)){
  for(indPatient in seq(1, by=1, length=nrow(patients))){
    
    ratioLines <- ratiosDf %>% 
      filter(patientID == patients[indPatient,]$patientID & 
                                        TestID == test)
    
    if(nrow(ratioLines) >=1){
      
      ratio <- ratioLines[1,]$ratio
      distY <- ratioLines[1,]$distY
      
      temp_df <- df %>% 
        filter(TestID == test & 
                 patientID==patients[indPatient,]$patientID) %>% 
        arrange(Timestamp)
      
      pX <- (temp_df[which.min(temp_df$Timestamp),]$X + min(temp_df$X) + max(temp_df$X))/3
      
      pY <- (temp_df[which.min(temp_df$Timestamp),]$Y+ min(temp_df$Y) + max(temp_df$Y))/3
        
      temp_df <- temp_df %>%
        mutate(X = ((X-pX/2)/ratio)/distY*400, 
               Y = (Y - pY/2)/distY*400)

      
      completeCalibratedDf <- rbind(completeCalibratedDf, temp_df)
    }

  }
}

#Grip Angle
plotGrip <- completeDf %>% 
  group_by(patientID) %>% 
  summarize(meanGrip = mean(GripAngle), sdGrip = sd(GripAngle), isPwp = first(isPwp)) %>% ggplot()

meanGrip <- plotGrip + geom_bar(aes(meanGrip, fill=isPwp), stat="bin")

sdGrip <- plotGrip + geom_bar(aes(sdGrip, fill=isPwp), stat="bin")

ggarrange(meanGrip, sdGrip, common.legend = TRUE, legend="bottom")

#Grip Angle Test 0
plotGrip <- completeDf %>% 
  filter(TestID == 0) %>%
  group_by(patientID) %>% 
  summarize(meanGrip = mean(GripAngle), sdGrip = sd(GripAngle), isPwp = first(isPwp)) %>% ggplot()

meanGrip <- plotGrip + geom_bar(aes(meanGrip, fill=isPwp), stat="bin")

sdGrip <- plotGrip + geom_bar(aes(sdGrip, fill=isPwp), stat="bin")

ggarrange(meanGrip, sdGrip, common.legend = TRUE, legend="bottom")

#Grip Angle Test 1
plotGrip <- completeDf %>% 
  filter(TestID == 1) %>%
  group_by(patientID) %>% 
  summarize(meanGrip = mean(GripAngle), sdGrip = sd(GripAngle), isPwp = first(isPwp)) %>% ggplot()

meanGrip <- plotGrip + geom_bar(aes(meanGrip, fill=isPwp), stat="bin")

sdGrip <- plotGrip + geom_bar(aes(sdGrip, fill=isPwp), stat="bin")

ggarrange(meanGrip, sdGrip, common.legend = TRUE, legend="bottom")

#Grip Angle Test 2
plotGrip <- completeDf %>% 
  filter(TestID == 2) %>%
  group_by(patientID) %>% 
  summarize(meanGrip = mean(GripAngle), sdGrip = sd(GripAngle), isPwp = first(isPwp)) %>% ggplot()

meanGrip <- plotGrip + geom_bar(aes(meanGrip, fill=isPwp), stat="bin")

sdGrip <- plotGrip + geom_bar(aes(sdGrip, fill=isPwp), stat="bin")

ggarrange(meanGrip, sdGrip, common.legend = TRUE, legend="bottom")

#Pressure

completeDf %>%
  filter(TestID==2 & Pressure > 0 & isPwp == FALSE)


completeDf %>% 
  filter(TestID==2) %>% 
  group_by(patientID) %>% 
  summarize(sumPressure = sum(Pressure), 
            isPwp = first(isPwp)) %>% 
  ggplot() + 
  geom_histogram(aes(sumPressure, fill=isPwp))

plot <- completeDf %>% 
  filter(TestID==2) %>% 
  group_by(patientID) %>% 
  summarize(sdX = sd(X), sdY=sd(Y), 
            isPwp = first(isPwp)) %>% 
  ggplot()

sdxPlot <- plot + geom_histogram(aes(sdX, fill=isPwp))

sdYPlot <- plot + geom_histogram(aes(sdY, fill=isPwp))

ggarrange(sdxPlot, sdYPlot, common.legend = TRUE, legend="bottom")

#Time
completeDf %>% 
  filter(TestID == 2) %>% 
  group_by(patientID) %>% 
  summarize(t = max(Timestamp), isPwp = first(isPwp))

completeDf %>% 
  filter(TestID == 2 & isPwp == FALSE) %>% 
  group_by(patientID) %>% 
  summarize(t = max(Timestamp), isPwp = first(isPwp))

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
#Building the archimedean spiral
#---------------
meanXy <- completeDf %>% filter(TestID != 2 & isPwp==FALSE & Timestamp ==0) %>% summarize(mx = mean(X), my=mean(Y))

params <- function(b){
  a <- 4
  t <- seq(0,6*pi, length.out=900)
  x <- (a + b*t) * (cos(t)) + meanXy$mx
  y <- (a + b*t) * -(sin(t))+ meanXy$my
  archimedeanSpiral = data.frame(x,y)
  
  tpatients <- training_Patients %>% filter(isPwp == FALSE)
  
  m <- 0
  
  for(i in seq(1,by=1,length=nrow(tpatients))){
    area <- geiger:::.area.between.curves(archimedeanSpiral$x, 
                                          (completeDf %>% filter(TestID==0 & patientID == tpatients[i,]$patientID)) %>% pull(Y), 
                                        archimedeanSpiral$y)
    
    m= m+ area
  }
  
  return(m)
  
}

re <- sapply(seq(0,by=0.5,length=50), params)

a <- 4
b <- 12
t <- seq(0,6*pi, length.out=2578)
x <- (a + b*t) * (cos(t)) + meanXy$mx
y <- (a + b*t) * -(sin(t)) + meanXy$my
archimedeanSpiral = data.frame(x,y)

datas <- completeDf %>%
                   filter(TestID==0 & 
                            patientID %in% c(patients[1,]$patientID))

ggplot() + geom_path(aes(x,y), color="red") + 
  geom_path(aes(datas$X, datas$Y), lineend="butt")

polygon(c(datas$X, datas$Y), c(x,y), col="red")



patients <- patients %>% mutate(dist0=NA, dist1=NA,dist2=NA)

tpatients <- patients %>% mutate(diffArea0 = NA)

for(i in seq(1,by=1,length=nrow(tpatients))){
  
  datas <- completeDf%>% filter(TestID==0 & patientID == patients[i,]$patientID)
  if(nrow(datas)>0){
    t <- seq(0,6*pi, length.out=nrow(datas))
    x <- (a + b*t) * (cos(t)) + meanXy$mx
    y <- (a + b*t) * -(sin(t)) + meanXy$my
    archimedeanSpiral = data.frame(x,y)
    dist <- cbind(datas, archimedeanSpiral)
    dist <- dist %>% mutate(dist = sqrt((x-X)^2+(y-Y)^2))
    patients[i,]$dist0 <- sum(dist$dist)
  }
}

for(i in seq(1,by=1,length=nrow(tpatients))){
  
  datas <- completeDf%>% filter(TestID==1 & patientID == patients[i,]$patientID)
  if(nrow(datas)>0){
    t <- seq(0,6*pi, length.out=nrow(datas))
    x <- (a + b*t) * (cos(t)) + meanXy$mx
    y <- (a + b*t) * -(sin(t)) + meanXy$my
    archimedeanSpiral = data.frame(x,y)
    dist <- cbind(datas, archimedeanSpiral)
    dist <- dist %>% mutate(dist = sqrt((x-X)^2+(y-Y)^2))
    patients[i,]$dist1 <- sum(dist$dist)
  }
}

patients %>% ggplot() + geom_histogram(aes(dist0, fill=isPwp))
patients %>% ggplot() + geom_histogram(aes(dist1, fill=isPwp))


for(i in seq(1,by=1,length=nrow(patients))){
  area <- geiger:::.area.between.curves(archimedeanSpiral$x, 
                                (completeDf %>% filter(TestID==0 & patientID == patients[i,]$patientID)) %>% pull(Y), 
                                archimedeanSpiral$y)
  patients[i,]$diffArea0 <- area
}

times0 <- completeDf %>% filter(TestID==0) %>%group_by(patientID) %>% summarize(t0 = max(Timestamp))
times1 <- completeDf %>% filter(TestID==1) %>%group_by(patientID) %>% summarize(t1 = max(Timestamp))
times2 <- completeDf %>% filter(TestID==2) %>%group_by(patientID) %>% summarize(t2 = max(Timestamp))

patients <- left_join(patients,times0, by="patientID")
patients <- left_join(patients,times1, by="patientID")
patients <- left_join(patients,times2, by="patientID")

#PathDist
#PathOverlap



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


patients <- patients %>% mutate(meanPressure0 = 0, meanGripAngle0=0, sdGripAngle0=0, sdPressure0=0,
                                 meanPressure1 = 0, meanGripAngle1=0, sdGripAngle1=0, sdPressure1=0,
                                 meanPressure2 = 0, meanGripAngle2=0, sdGripAngle2=0, sdPressure2=0,)
for(test in seq(0, by=1, length=3)){
  for(index in seq(1, by=1, length=nrow(patients))){
    patientId <- patients[index,]$patientID
    temp <- completeDf %>% filter(TestID == test & patientID == patientId)
    patients[index,][paste("meanPressure", test, sep="")] <- mean(temp$Pressure)
    patients[index,][paste("sdPressure", test, sep="")] <- sd(temp$Pressure)
    patients[index,][paste("meanGripAngle", test, sep="")] <- mean(temp$GripAngle)
    patients[index,][paste("sdGripAngle", test, sep="")] <- sd(temp$GripAngle)
  }
}


#Add speed



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

patients <- patients %>% mutate(isPwp = as.numeric(isPwp))
patients <- na.omit(patients)

fit0 <- lm(isPwp ~ dist0 + t0 + meanPressure0 + sdPressure0, data=training_Patients)
fit1 <- lm(isPwp ~ dist1 + t1 + meanPressure1 + sdPressure1, data=training_Patients)
fit2 <- lm(isPwp ~ meanPressure2 + sdPressure2, data=training_Patients)

fit0 <- train(isPwp ~ dist0 + t0 + meanPressure0 + sdPressure0, method="knn", data=training_Patients)
fit1 <- train(isPwp ~ dist1 + t1 + meanPressure1 + sdPressure1, method="knn", data=training_Patients)
fit2 <- train(isPwp ~ meanPressure2 + sdPressure2, method="knn",data=training_Patients)

rf0 <- randomForest(isPwp ~ dist0 + t0 + meanPressure0 + sdPressure0, data=training_Patients)
rf1 <- randomForest(isPwp ~ dist1 + t1 + meanPressure1 + sdPressure1, data=training_Patients)
rf2 <- randomForest(isPwp ~ meanPressure2 + sdPressure2,data=training_Patients)

predict(fit0, testing_Patients)
predict(fit1, testing_Patients)
predict(fit2, testing_Patients)

predict(rf0, testing_Patients)
predict(rf1, testing_Patients)
predict(rf2, testing_Patients)

sum(yhat == testing_Patients$isPwp)/nrow(testing_Patients)

