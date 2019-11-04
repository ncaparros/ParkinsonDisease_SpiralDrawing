#---------------
#Libraries
#---------------

if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
if(!require(ids)) install.packages("ids", repos = "http://cran.us.r-project.org")
if(!require(tidyr)) install.packages("tidyr", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")

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
                    GripAngle = as.numeric(GripAngle))
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
#Create training and validation sets
#---------------

patients <- df %>% select(patientID, isPwp) %>% distinct(patientID, isPwp)

test_index <- createDataPartition(y = patients$isPwp, 
                    times = 1, 
                    p = 0.1, 
                    list = FALSE)

training_Patients <- patients[-test_index,]
testing_Patients <- patients[test_index,]