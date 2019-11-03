#---------------
#Libraries
#---------------

if(!require(dplyr)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(ids)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(stringr)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(tidyr)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(gdata)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")

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

df <- df %>% separate(V1, c("X", "Y", "Z", "Pressure", "GripAngle","Timestamp", "TestID"),";")

#---------------
#Read data
#---------------
