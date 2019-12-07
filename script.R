if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
if(!require(ids)) install.packages("ids", repos = "http://cran.us.r-project.org")
if(!require(tidyr)) install.packages("tidyr", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", dependencies = TRUE, repos = "http://cran.us.r-project.org")
if(!require(anytime)) install.packages("anytime", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(geiger)) install.packages("geiger", repos = "http://cran.us.r-project.org")
if(!require(ggpubr)) install.packages("ggpubr", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")
if(!require(ggarrange)) install.packages("ggarrange", repos = "http://cran.us.r-project.org")
if(!require(sp)) install.packages("sp", repos = "http://cran.us.r-project.org")
if(!require(rgeos)) install.packages("rgeos", repos = "http://cran.us.r-project.org")
if(!require(randomForest)) install.packages("randomForest", repos = "http://cran.us.r-project.org")
if(!require(knitr)) install.packages("knitr", repos = "http://cran.us.r-project.org")
if(!require(ggthemes)) install.packages("ggthemes", repos = "http://cran.us.r-project.org")

##################################################################
###1) III Project overview C- Archimedean spiral
##################################################################

#Here we build two Archimedean spirals. The equations are : x(t)= (a+bt)cos(t) and y(t)= (a+bt)sin(t)
#Archimedean spiral similar to the one used in the tests. a=0, b=10.61, and it does 3 turns (6pi). This spiral is to be plot on the left of the figure.
archSpiral6pi.b <- 10.61
archSpiral6pi.t <- seq(0, 6*pi, length.out = 1100)
archSpiral6pi.x <- archSpiral6pi.b * archSpiral6pi.t * cos(archSpiral6pi.t)
archSpiral6pi.y <- archSpiral6pi.b * archSpiral6pi.t * -(sin(archSpiral6pi.t))

#Ploting the Archimedean spiral.
plot1 <- ggplot() +
  geom_path(aes(archSpiral6pi.x, archSpiral6pi.y)) +
  xlab("x")+
  ylab("y")+
  ggtitle("Length 6pi") +
  coord_fixed(ratio = 1) +
  theme_solarized()

#Archimedean spiral used to show the parameters a (=20), b (=30), t (length=pi/3)
archSpiralpi_3.a <- 20
archSpiralpi_3.b <- 30
archSpiralpi_3.t <- seq(0, pi/3, length.out = 100)

archSpiralpi_3.x <- (archSpiralpi_3.a + archSpiralpi_3.b * archSpiralpi_3.t) * cos(archSpiralpi_3.t)
archSpiralpi_3.y <- (archSpiralpi_3.a + archSpiralpi_3.b * archSpiralpi_3.t) * sin(archSpiralpi_3.t)

#theta (=t = pi/4) is the angle used in the plot to show the parameters a, b and t
theta = pi/4

#The second plot of the figure, showing the Archimedean spiral of length pi/3
plot2 <- ggplot() + 
  #Archimedean spiral
  geom_path(aes(archSpiralpi_3.x, archSpiralpi_3.y)) +
  xlab("x")+
  ylab("y")+
  
  #a when t=0. x(0) = (a+bt)*cos(0) = (a + 0)*1 = a and y(0) = (a+ bt)*sin(0) = (a + 0)*0=0
  geom_path(aes(c(0:archSpiralpi_3.a), 0),
            col = "#FFA434", 
            size = 1.5) +
  geom_text(aes(15, 3),
            label = "a", 
            col = "#FFA434")+
  
  #a when t=pi/4. 
  #x(t) = (a+bt)*cos(t) = a*cos(t) + b*(t)*cos(t) 
  #and y(t) = (a + bt)*sin(t) = a*sin(t) + b*(t)*sin(t)
  #So the a-part of x is a*cos(theta) and the a-part of y is a*sin(theta)
  geom_path(aes(seq(0, archSpiralpi_3.a * cos(theta),
                    length.out = 2),
                seq(0, (archSpiralpi_3.a * sin(theta)), 
                    length.out = 2)),
            col = "#FFA434", 
            size = 1.5)+
  geom_text(aes(5, 10),
            label = "a", 
            col = "#FFA434")+
  
  #b when t=pi/4
  #x(t) = (a+bt)*cos(t) = a*cos(t) + b*(t)*cos(t) 
  #and y(t) = (a + bt)*sin(t) = a*sin(t) + b*(t)*sin(t)
  #So the b-part of x is b*theta*cos(theta) and the b-part of y is b*theta*sin(theta)
  geom_path(aes(seq(archSpiralpi_3.a * cos(theta),
                    archSpiralpi_3.a * cos(theta) + archSpiralpi_3.b * theta * cos(theta),
                    length.out = 2),
                seq((archSpiralpi_3.a * sin(theta)),
                    archSpiralpi_3.a * sin(theta) + archSpiralpi_3.b * theta * sin(theta), 
                    length.out = 2)),
            col = "#006837", 
            size = 1.5) + 
  geom_text(aes(20, 25),
            label = "b*t", 
            col = "#006837") +
  
  #t = pi/4
  #Here we represent the angle on the plot.
  geom_curve(aes(x = 7, 
                 y = 0, 
                 xend = 7 * cos(theta), 
                 yend = 7 * sin(theta)),
             col = "#2F86E0",
             size = 1,
             arrow = arrow(type = "open", 
                           length = unit(7,"points"))) +
  geom_text(aes(9, 6),
            label = "t", 
            col = "#2F86E0") +
  
  coord_fixed(ratio = 1) +
  ggtitle("Length pi/3") +
  theme_solarized()

ggarrange(plot1, plot2, align="v", common.legend = TRUE, legend = "bottom")

##################################################################
###1) IV Dataset overview
##################################################################

#Function to extract the dataframe from either the data in the repository (local, from GitHub) or from the UCI website
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
                                        sep = "")), 
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
  
} else {
  #Second case : the data does not exist on the working directory
  
  #Names for : the created temporary directory, the directory we need to move, main the directory
  dirpath = "data"
  dirToMove = "new_dataset/parkinson"
  dirToKeep = "hw_dataset/parkinson"
  
  #download the .zip containing the datasets from UCI
  dl <- tempfile()
  download.file("https://archive.ics.uci.edu/ml/machine-learning-databases/00395/PARKINSON_HW.zip", dl)
  
  #unziping the archive
  unzip(dl, exdir = "data")
  
  #Copy the files from the new_dataset/parkinson into the main directory hw_dataset/parkinson
  sapply(c(list.files(paste0(dirpath, "/", dirToMove))), function(file){
    file.copy(paste0(dirpath, "/", dirToMove, "/", file), 
              dirToKeep)
  })
  
  #building the data frame df
  df <- createDf()
  
  #Removing the temporary directory
  unlink(dirpath, recursive = TRUE)
}

#Plot the dataset extracted
kable(head(df))

#Extracting values into columns
df <- df %>% 
  separate(V1, c("X", "Y", "Z", "Pressure", "GripAngle", "Timestamp", "TestID"), ";")

kable(head(df))

#Getting every patient in a dataframe 'patients'
patients <- df %>% 
  select(patientID, isPwp) %>% 
  distinct(patientID, isPwp)

kable(head(patients))

##################################################################
###2) II Data cleaning - Format
##################################################################

#Shows the class of each column in the df data frame
lapply(df, class)

#Converting string values into numeric values
df <- df %>% mutate(X = as.numeric(X),
                    Y = as.numeric(Y),
                    Z = as.numeric(Z),
                    Pressure = as.numeric(Pressure),
                    GripAngle = as.numeric(GripAngle),
                    Timestamp = as.numeric(Timestamp))


##################################################################
###2) II Data cleaning - Cleaning TimeStamp
##################################################################

#The date time functions would not give any satisfying result
make_date(as.character(df[1,]$Timestamp))
make_datetime(df[1,]$Timestamp)
dym(as.character(df[1,]$Timestamp))
as.Date(as.character(df[1,]$Timestamp), "%Y%M%D")

kable(head(df))

#create a new dataframe into which convert the timestamp values into more readable and interpretable values
completeDf <- data.frame()

#For each of the tests "test"
for(test in seq(0, by=1, length=3)){
  
  #For each of the patients "indPatient"
  for(indPatient in seq(1, by=1, length=nrow(patients))){
    
    #Create a temporary data frame for patient "indPatient" 
    #and test "test"
    temp_df <- df %>% 
      filter(TestID == test & 
               patientID == patients[indPatient,]$patientID) %>% 
      arrange(Timestamp)
    
    #Get first value of timestamp
    initialTimestamp = temp_df[1,]$Timestamp
    
    #Mutate Timestamp so that the very first value of Timestamp 
    #for patient "indPatient" and test "test" is equal to 0
    temp_df <- temp_df %>%
      mutate(Timestamp = Timestamp - initialTimestamp)
    
    #Bind temporary data frame to complete data frame
    completeDf <- rbind(completeDf, temp_df)
    
    
  }
}

kable(head(completeDf))

##################################################################
###2) II Data cleaning - Calibrating the samples
##################################################################

#Ploting the samples for the SST and DST by patient. It shows the different ratios in the records.
completeDf %>% 
  filter(TestID != 2) %>%
  ggplot() +
  geom_point(aes(X, Y, color = patientID),
             size = 0.75,
             show.legend = FALSE) + 
  ggtitle("Different ratios by patient for SST and DST samples") +
  facet_grid(. ~ TestID) +
  theme_solarized()

#Ploting the samples for healthy controls on the SST, by patient. It shows the spatial shift between the samples
completeDf %>% 
  filter(TestID == 0 & 
           isPwp == FALSE) %>% 
  ggplot() + 
  geom_point(aes(X, Y, 
                 color = patientID),
             size=0.75,
             show.legend = FALSE) + 
  ggtitle("Spatial gap for SST samples") +
  theme_solarized()

#Ploting (bar plot) the time needed by each patient to complete the SST. It shows that the timestamps do not seem to be affected by the material. 
completeDf %>% 
  filter(TestID == 0) %>% 
  group_by(patientID) %>% 
  summarize(maxT = max(Timestamp), 
            isPwp = first(isPwp)) %>% 
  ggplot() + 
  geom_histogram(aes(maxT, 
                     fill = isPwp), 
                 stat = "bin") + 
  xlab("Time needed to complete the SST (by patient)") +
  ggtitle("Sample times for SST by patient")+
  theme_solarized()

#Create a dataframe 'ratiosDf' which contains the ratio X/Y (ratio), the height of the sample (H) and isPwp for each couple (patient,test)
ratiosDf <- df %>% 
  group_by(patientID, TestID) %>% 
  summarize(ratio = (max(X) - min(X)) / (max(Y) - min(Y)), 
            H = max(Y) - min(Y), 
            isPwp = first(isPwp))

#Plot the ratios by test for SST and DST. It shows the gap between ratios. In the ideal case, every ratio would be of one.
ratiosDf %>% 
  filter(TestID != 2) %>% 
  ggplot() +
  geom_bar(aes(ratio, fill = TestID), stat = "bin") + 
  scale_fill_discrete(name = "Test", labels=c("SST", "DST")) +
  ggtitle("X-Y ratios") +
  theme_solarized()

#Ploting the first point of each sample (by isPwp) for the SST. It shows the spatial gap between the samples. In the ideal case, they would all be similar.
completeDf %>% 
  filter(TestID == 0 & 
           Timestamp == 0) %>% 
  ggplot() + 
  geom_point(aes(X, Y, color = isPwp))+ 
  ggtitle("Starting points for SST samples") + 
  theme_solarized()

#Creating an Archimedean spiral. The choice of the arguments is detailed later (see III-C Defining the Archimedean Spiral)
#b was calculated manually, t determined (the spiral is three complete turns, which means 3*2pi =6pi)
b <- 10.61
t <- seq(0, 6*pi, length.out=1100)
x <- b*t * (cos(t))
y <- b*t * -(sin(t))

#data frame containing the values (x,y) of the Archimedean spiral
archimedeanSpiral = data.frame(x, y)

#determined the points used to calibrate the samples. X min, max and first value, Y min and max
points <- archimedeanSpiral %>% 
  filter(x == min(x) | 
           x == max(x) | 
           y == min(y) | 
           y == max(y) | 
           x == first(x))

#Ploting the Archimedean spiral and the points used to calibrate the samples.
archimedeanSpiral %>%
  ggplot() +
  geom_path(aes(x, y)) +
  geom_point(data = points, 
             aes(x, y),
             color = "red") +
  ggtitle("Points used to slide the drawings") +
  theme_solarized()

#new data frame to contain the calibrated X and Y values
completeCalibratedDf <- data.frame()

#for each test
for(test in seq(0, by = 1, length = 2)){
  
  #for each patient
  for(indPatient in seq(1, by = 1, length = nrow(patients))){
    
    #get the ratio for current patient and current test
    ratioLines <- ratiosDf %>% 
      filter(patientID == patients[indPatient,]$patientID & 
               TestID == test)
    
    #if the patient as taken the test (= there are values in the ratio data frame)  
    if(nrow(ratioLines) >= 1){
      
      #get the ratio for current patient and current test
      ratio <- ratioLines[1,]$ratio
      
      #get the height for current patient and current test (we are standardizing X depending of Y)
      H <- ratioLines[1,]$H
      
      #get temporary data frame for current patient and current test, arranged by timestamp
      temp_df <- completeDf %>% 
        filter(TestID == test & 
                 patientID == patients[indPatient,]$patientID) %>% 
        arrange(Timestamp)
      
      #Calculate the X slid parameter
      pX <- (temp_df[which.min(temp_df$Timestamp),]$X + 
               min(temp_df$X) + 
               max(temp_df$X))/3
      
      #Calculate the Y slid parameter
      pY <- (temp_df[which.min(temp_df$Timestamp),]$Y + 
               min(temp_df$Y) + 
               max(temp_df$Y)) /3
      
      #mutate X and Y with 
      temp_df <- temp_df %>%
        mutate(X = ((X - pX) / (ratio*H)) * 400, 
               Y = (Y - pY) / H*400)
      
      #binding the already calibrated values to the ones just calibrated
      completeCalibratedDf <- rbind(completeCalibratedDf, temp_df)
    }
    
  }
}

#parameter to adjust (slide) the Archimedean spiral on the X axis. This will NOT be used for prediction, only for the visualization of the calibrating. That is why it is on the whole dataset (not training set).
#It is the mean of the first X for healthy patients, for SST and DST.
Xadjust <- completeCalibratedDf %>% 
  filter(TestID != 2 & 
           Timestamp == 0 & 
           isPwp == FALSE) %>% 
  pull(X) %>% 
  mean()

#parameter to adjust (slide) the Archimedean spiral on the Y axis. This will NOT be used for prediction, only for the visualization of the calibrating. That is why it is on the whole dataset (not training set).
#It is the mean of the first Y for healthy patients, for SST and DST.
Yadjust <- completeCalibratedDf %>% 
  filter(TestID != 2 & 
           Timestamp == 0 & 
           isPwp == FALSE) %>% 
  pull(Y) %>% 
  mean()

#Archimedean spiral adjusted (slid on X and Y with the previous adjustments parameters)
b <- 10.61
t <- seq(0, 6*pi, length.out = 1100)
x <- b*t * (cos(t)) + Xadjust
y <- b*t * -(sin(t)) + Yadjust

#dataframe containing the X,Y values of the Archimedean spiral
archimedeanSpiral = data.frame(x, y)

#Ploting the calibrated samples and the slid Archimedean spiral to show the improvement in the samples. They are now more consistent.
completeCalibratedDf %>% 
  filter(TestID == 0) %>% 
  ggplot() + 
  geom_point(aes(X, Y, 
                 color = patientID),
             size = 0.5,
             show.legend = FALSE) + 
  facet_grid(. ~ TestID) +
  ggtitle("Calibrated samples for SST and DST") + 
  geom_point(data = archimedeanSpiral, aes(x, y), color = "red") +
  theme_solarized()

##################################################################
###2) III Data analysis - Global analysis
##################################################################

#Ploting the number of each test taken. It shows that not every patient took all the tests.
ratiosDf %>% 
  ggplot() + 
  geom_histogram(aes(TestID, 
                     fill = isPwp),
                 stat = "count") +
  ggtitle("Tests taken") +
  xlab("Test") +
  scale_x_discrete(labels = c("0" = "SST", "1" = "DST", "2" = "SOCPT")) +
  theme_solarized()

##################################################################
###2) III Data analysis - Creating training and testing sets
##################################################################

#Data frame containing the unique couples patient/test. It will allow me to split the datas into the four datasets I need to create training and testings sets with every cases (controls, patients/SOCPT and SST-DST)
groupedPatientTestDf <- df %>% 
  group_by(patientID, TestID) %>% 
  select(patientID, TestID, isPwp) %>% 
  unique()

#data frame with healthy controls who took the SOCPT
subTest2Ctrl <- groupedPatientTestDf %>% 
  filter(TestID == 2 & 
           isPwp == FALSE) %>% 
  ungroup() %>% 
  select(patientID) %>% 
  unique()

#data frame with people with Parkinson's who took the SOCPT
subTest2Parkinsons  <- groupedPatientTestDf %>% 
  filter(TestID == 2 & 
           isPwp == TRUE) %>% 
  ungroup() %>% 
  select(patientID) %>% 
  unique()

#data frame with healthy controls who did not took the SOCPT
subControls <- groupedPatientTestDf %>% 
  filter(isPwp == FALSE & 
           !patientID %in% subTest2Ctrl$patientID) %>% 
  ungroup() %>% 
  select(patientID) %>% 
  unique()

#data frame with people with Parkinson's who did not took the SOCPT
subParkinsons <- groupedPatientTestDf %>% 
  filter(isPwp == TRUE & 
           !patientID %in% subTest2Parkinsons$patientID) %>% 
  ungroup() %>% 
  select(patientID) %>% 
  unique()

#checking if the number of patients in each data frame is consistent with the total number of patients
nrow(subTest2Ctrl) + 
  nrow(subTest2Parkinsons) + 
  nrow(subControls) +
  nrow(subParkinsons) == nrow(patients)

#Getting random samples for each sub-data set
set.seed(1)
testIndexT2Ctrl <- sample(1:nrow(subTest2Ctrl), 
                          floor(nrow(subTest2Ctrl) * 0.1) + 1) 
set.seed(1)
testIndexT2Parkinsons <- sample(1:nrow(subTest2Parkinsons), 
                                floor(nrow(subTest2Parkinsons) * 0.1) + 1) 
set.seed(1)
testIndexCtrl <- sample(1:nrow(subControls), 
                        floor(nrow(subControls) * 0.1) + 1) 
set.seed(1)
testIndexParkinsons <- sample(1:nrow(subParkinsons), 
                              floor(nrow(subParkinsons) * 0.1) + 1) 

#binding the sample patient ids from each sub-data set in a testing data set
testing_patients <- subTest2Ctrl[testIndexT2Ctrl,]

testing_patients <- rbind(testing_patients, 
                          subTest2Parkinsons[testIndexT2Parkinsons,])

testing_patients <- rbind(testing_patients, 
                          subControls[testIndexCtrl,])

testing_patients <- rbind(testing_patients, 
                          subParkinsons[testIndexParkinsons,])

#creating the testing dataset with all columns from the data frame patients
testingSet <- patients %>% 
  filter(patientID %in% testing_patients$patientID)

#creating the training dataset with all columns from the data frame patients
trainingSet <- patients %>% 
  filter(!patientID %in% testing_patients$patientID)

##################################################################
###2) III Data analysis - Distance (Static and Dynamic Spiral) analysis 
##################################################################

########### SST - Archimedean spiral / DST - Archimedean spiral
#Initialisation of the values : 
#total area difference between the drawing 
#and the Archimedean spiral, 
#and the standard deviation of the distance 
#between the drawing and the spiral.
patients <- patients %>% mutate(areaT0 = NA, 
                                areaT1 = NA, 
                                sdT0 = NA, 
                                sdT1 = NA)

#calibrating the Archimedean spiral 
#with first X value of SST 
#and DST from healthy controls
#in the testing data set
Xadjust <- completeCalibratedDf %>% 
  filter(TestID != 2 & 
           Timestamp == 0 & 
           patientID %in% trainingSet$patientID &
           isPwp == FALSE) %>% 
  pull(X) %>% 
  mean()

#calibrating the Archimedean spiral 
#with first Y value of SST 
#and DST from healthy controls
#in the testing data set
Yadjust <- completeCalibratedDf %>% 
  filter(TestID != 2 & 
           Timestamp == 0 & 
           patientID %in% trainingSet$patientID &
           isPwp == FALSE) %>% 
  pull(Y) %>% 
  mean()

#The calculus is applied to the first two tests (Static and Dynamic Spiral test)
for(test in seq(0, by = 1, length = 2)){
  
  #For every patient in the data set
  for(p in seq(1, by = 1, length = nrow(patients))){
    
    #Defining current patient
    patient = patients[p,]$patientID
    
    #Select every unique couple X,Y for current patient on current test
    temp_df <- completeCalibratedDf %>% 
      filter(TestID == test & 
               patientID == patient) %>% 
      select(X,Y) %>% 
      unique()
    
    #if there are values (= if the patient has taken the test)
    if(nrow(temp_df) > 0){
      
      #construct the archimedeanSpiral
      b <- 10.61
      t <- seq(0,6*pi, length.out = nrow(temp_df))
      x <- b*t * (cos(t)) + Xadjust
      y <- b*t * -(sin(t)) + Yadjust
      
      #build a data frame for the archimedean spiral coordinates
      archimedeanSpiral = data.frame(x,y)
      
      #add row number for further left_join
      archimedeanSpiral <- archimedeanSpiral %>% 
        mutate(nr = row_number())
      
      #Convert data frames in spatial points (rgeos package) 
      temp_df_sp <- SpatialPoints(temp_df)
      sp <- SpatialPoints(archimedeanSpiral)
      
      #determine nearest points on the Archimedean spiral
      temp_df$nr <- apply(gDistance(temp_df_sp, sp, byid = TRUE), 1, which.min)
      
      #join tables to get the coordinates of the nearest points
      temp_df <- left_join(temp_df, archimedeanSpiral, by = "nr")
      
      #calculate distance between each point of the sample and it's nearest neighbour on the Archimedean spiral
      temp_df <- temp_df %>% 
        mutate(dist = sqrt((X - x)^2 + (Y - y)^2)) %>% select(X, Y, dist)
      
      #sum all the distances to approximate the area
      patients[p,][paste0("areaT", test)] <- sum(temp_df$dist)
      
      #get the standard deviation
      patients[p,][paste0("sdT", test)] <- sd(temp_df$dist)
    }
    
  }
}

#plot the results (area difference and sd)
plotComparingTestsWithArchimedeanSpiral <- patients %>% 
  ggplot()

#plot the SST area difference with the Archimedean spiral by patient
plotT0Archi <- plotComparingTestsWithArchimedeanSpiral + 
  geom_histogram(aes(areaT0, 
                     fill = isPwp, 
                     color = patientID), 
                 stat = "bin") +
  xlab("Area difference (SST, Archimedean spiral)") +
  theme_solarized()

#plot the DST area difference with the Archimedean spiral by patient
plotT1Archi <- plotComparingTestsWithArchimedeanSpiral + 
  geom_histogram(aes(areaT1, fill = isPwp, color = patientID), 
                 stat="bin") +
  xlab("Area difference (DST, Archimedean spiral)") +
  theme_solarized()

#plot the SST distance standard deviation with the Archimedean spiral by patient
plotSdT0Archi <- plotComparingTestsWithArchimedeanSpiral + 
  geom_histogram(aes(sdT0, fill = isPwp, color = patientID), 
                 stat = "bin") +
  xlab("Standard deviation (SST, Archimedean spiral)") +
  theme_solarized()

#plot the DST distance standard deviation with the Archimedean spiral by patient
plotSdT1Archi <- plotComparingTestsWithArchimedeanSpiral + 
  geom_histogram(aes(sdT1, fill = isPwp, color = patientID), 
                 stat = "bin") +
  xlab("Standard deviation (DST, Archimedean spiral)") +
  theme_solarized()

ggarrange(plotT0Archi, 
          plotT1Archi, 
          plotSdT0Archi,
          plotSdT1Archi, 
          ncol = 2, nrow = 2, common.legend = TRUE)

########### SST - DST
#Initialisation of the values : 
#total area difference between the drawing and the Archimedean spiral, 
#and the standard deviation of the distance between the drawing and the spiral.
patients <- patients %>% 
  mutate(areaT0_T1 = NA, sdT0_T1 = NA)

#For every patient in the data set
for(p in seq(1, by = 1, length = nrow(patients))){
  
  #Defining current patient
  patient = patients[p,]$patientID
  
  #Select every unique couple X,Y for current patient on SST
  temp_df0 <- completeCalibratedDf %>% 
    filter(TestID == 0 & 
             patientID == patient) %>% 
    select(X, Y) %>% 
    unique() %>% 
    mutate(nr = row_number())
  
  #Select every unique couple X,Y for current patient on DST
  temp_df1 <- completeCalibratedDf %>% 
    filter(TestID == 1 & 
             patientID == patient) %>% 
    select(X, Y) %>% 
    unique() %>% 
    mutate(nr = row_number())
  
  #if there are values (= if the patient has taken the test)
  if(nrow(temp_df0) > 0 & nrow(temp_df1) > 0){
    
    #Convert data frames in spatial points (rgeos package) 
    temp_df0_sp <- SpatialPoints(temp_df0)
    temp_df1_sp <- SpatialPoints(temp_df1)
    
    #get the closest point of the DST sample to the SST sample
    temp_df1$nr <- apply(gDistance(temp_df0_sp, 
                                   temp_df1_sp, 
                                   byid = TRUE), 
                         1, which.min)
    #join the tables in the temporary data frame (point on SST, closest on DST)
    temp_df <- inner_join(temp_df1, temp_df0, by = "nr")
    
    
    #calcultate the distance on each line of the data frame
    temp_df <- temp_df %>% 
      mutate(dist = sqrt((X.x - X.y)^2 + (Y.x - Y.y)^2)) %>% 
      select(X.x, Y.y, dist)
    
    #fill the patients data frame with the approximated area (sum(distances)) and the standard deviation of the distance
    patients[p,]$areaT0_T1 <- sum(temp_df$dist)
    patients[p,]$sdT0_T1 <- sd(temp_df$dist)
  }
  
  
}

#get the time needed to perform the SST by patient
times0 <- completeCalibratedDf %>% 
  filter(TestID == 0 & 
           Z == 0) %>% 
  group_by(patientID) %>% 
  summarize(time0 = max(Timestamp))

#get the time needed to perform the DST by patient
times1 <- completeCalibratedDf %>% 
  filter(TestID == 1 & 
           Z == 0) %>% 
  group_by(patientID) %>% 
  summarize(time1 = max(Timestamp))

#join the data frames with times needed to perform SST and DST
times <- left_join(times0, times1, by = "patientID")

#calculate the time differences between SST and DST samples
times <- times %>% 
  mutate(difft1_t0 = time1 - time0) %>% 
  select(difft1_t0, patientID)

#add the time differences to the patients data frame
patients <- left_join(patients, times, by = "patientID")

#initialize a plot
plotT0_T1 <- patients %>% ggplot() 

#plot the area difference between SST and DST
plotAreaT0_T1 <- plotT0_T1 + 
  geom_histogram(aes(areaT0_T1, 
                     fill = isPwp, 
                     color = patientID), 
                 stat = "bin") +
  xlab("Area difference (SST, DST)") +
  theme_solarized()

#plot the distance standard deviation between SST and DST
plotSdT0_T1 <- plotT0_T1 + 
  geom_histogram(aes(sdT0_T1, 
                     fill = isPwp, 
                     color = patientID), 
                 stat = "bin") +
  xlab("Standard deviation (SST, DST)") +
  theme_solarized()

#plot the time difference between SST and DST
plotDiffTimesT0_T1 <- plotT0_T1 + 
  geom_histogram(aes(difft1_t0, 
                     fill = isPwp, 
                     color = patientID), 
                 stat = "bin") +
  xlab("Time difference (SST, DST)") +
  theme_solarized()

ggarrange(plotAreaT0_T1,
          plotSdT0_T1, 
          plotDiffTimesT0_T1, 
          ncol = 2, nrow = 2, common.legend = TRUE)

##################################################################
###2) III Data analysis - Stability On Certain Point analysis
##################################################################

#initialize two new datas :
#Z0_T2 : the number of points on the screen the pen touched
#DistT2 : the total distance of the pen on X and Y 
#it shows if the patient involuntarily touched the screen
#and if his/her hand had tremor
patients <- patients %>% 
  mutate(Z0_T2 = NA, 
         DistT2 = NA)

#for each patient
for(index in seq(1, by = 1, length = nrow(patients))){
  
  #get the patient id
  id <- patients[index,]$patientID
  
  #get the sample for the current patient on the SOCPT
  temp <- completeDf %>% 
    filter(patientID == id & 
             TestID == 2) %>%
    arrange(Timestamp) %>% 
    select(X, Y, Z)
  
  #if the patient has taken the SOCPT
  if(nrow(temp)>0){
    
    #count the number of points on the screen
    patients[index,]$Z0_T2 <- nrow(temp %>% 
                                     filter(Z == 0))
    
    #calculate the total distance on X and Y of the pen
    temp <- temp %>% 
      select(X, Y) %>% 
      unique()
    dist <-0
    for(row in seq(2, by = 1, length = nrow(temp) - 1)){
      dist <- dist + sqrt((temp[row,]$X - temp[row-1,]$X)^2+(temp[row,]$Y - temp[row-1,]$Y)^2)
    }
    patients[index,]$DistT2 <- dist
  }
}

##################################################################
###2) IV Issues - Test inconsistency
##################################################################

#build the data frame with patients and tests
couplesTestsPatients <- df %>% 
  select(patientID, TestID) %>% 
  group_by(patientID, TestID) %>% 
  unique()

#get the patients who had taken the SST
PatientsT0 <- couplesTestsPatients %>% 
  filter(TestID == 0)

#get the patients who had taken the DST
PatientsT1 <- couplesTestsPatients %>% 
  filter(TestID == 1)

#get the patients who had taken the SOCPT
PatientsT2 <- couplesTestsPatients %>% 
  filter(TestID == 2)

#Are there any patients who took the SST 
#but neither the DST nor the SOCP?
PatientsT0 %>% 
  filter(!patientID %in% PatientsT1$patientID & 
           !patientID %in% PatientsT2$patientID) %>% 
  nrow() > 0

#Are there any patients who took the SST 
#and the DST but not the SOCP?
PatientsT0 %>% 
  filter(patientID %in% PatientsT1$patientID & 
           !patientID %in% PatientsT2$patientID) %>% 
  nrow() > 0

#Are there any patients who took the SST 
#and the SOCP but not the DST?
PatientsT0 %>% 
  filter(!patientID %in% PatientsT1$patientID & 
           patientID %in% PatientsT2$patientID) %>% 
  nrow() > 0

#Are there any patients who took the SST, DST and SOCP ?
PatientsT0 %>% 
  filter(patientID %in% PatientsT1$patientID & 
           patientID %in% PatientsT2$patientID) %>% 
  nrow() > 0


#Are there any patients who took the DST 
#but neither the SST nor the SOCPI?
PatientsT1 %>% 
  filter(!patientID %in% PatientsT0$patientID & 
           !patientID %in% PatientsT2$patientID) %>% 
  nrow() > 0

#Are there any patients who took the DST 
#and the SOCP but not the SST?
PatientsT1 %>% 
  filter(patientID %in% PatientsT2$patientID & 
           !patientID %in% PatientsT0$patientID) %>% 
  nrow() > 0
PatientsT2 %>% 
  filter(patientID %in% PatientsT1$patientID & 
           !patientID %in% PatientsT0$patientID) %>% 
  nrow() > 0

#Are there any patients who took the SOCP 
#but neither the SST nor the DST?
PatientsT2 %>% 
  filter(!patientID %in% PatientsT0$patientID & 
           !patientID %in% PatientsT1$patientID) %>% 
  nrow() > 0

##################################################################
###2) IV Prediction algorithms - Completing the training and testing sets
##################################################################

trainingSet <- left_join(trainingSet, patients %>% 
                           select(-isPwp), by = "patientID")

testingSet <- left_join(testingSet, patients %>% 
                          select(-isPwp), by = "patientID")

##################################################################
###2) IV Prediction algorithms - Parameters
##################################################################

#mutate the boolean isPwp to a numeric for prediction
trainingSet <- trainingSet %>% 
  mutate(isPwp = as.numeric(isPwp))

testingSet <- testingSet %>% 
  mutate(isPwp = as.numeric(isPwp))

##################################################################
###2) IV Prediction algorithms - Fitting models
##################################################################

#Static Spiral Test compared to the Dynamic Spiral Test
#random forest
fit_SST_DST <- randomForest(isPwp ~ areaT0_T1 + 
                              sdT0_T1 + 
                              difft1_t0,
                            data = trainingSet, 
                            na.action = na.omit)

yhat_SST_DST <- predict(fit_SST_DST, 
                        testingSet)


#Stability On Certain Point Test
#random forest
fit_SOCPT <- randomForest(isPwp ~ Z0_T2 + 
                            DistT2, 
                          data = trainingSet, 
                          na.action=na.omit)

yhat_SOCPT <- predict(fit_SOCPT, 
                      testingSet)


#Computing the predictions
yhatDf <- data.frame(yhat_SST_DST,
                     yhat_SOCPT)

p_hat <- yhatDf %>% 
  rowwise() %>% 
  mutate(phat = weighted.mean(c(yhat_SST_DST,
                                yhat_SOCPT), 
                              c(1, 4), 
                              na.rm = TRUE)) %>% 
  select(phat)

##################################################################
###3) Results
##################################################################

#if the prediction is above 0.5, 
#the patient is diagnosed as with Parkinson's
Yhat <- ifelse(p_hat > 0.5, 1, 0) %>% 
  factor()

#binding the prediction, 
#the diagnosis and the medical diagnosis 
results <- cbind(p_hat, Yhat, testingSet$isPwp)
results

#get the accuracy
acc <- confusionMatrix(Yhat, 
                       testingSet$isPwp %>% 
                         factor())$overall["Accuracy"]

acc