## Codebook for the project

### Data Set Information:

The experiments have been carried out with a group of 30 volunteers within an age bracket of 19-48 years. Each person performed six activities (WALKING, WALKING_UPSTAIRS, WALKING_DOWNSTAIRS, SITTING, STANDING, LAYING) wearing a smartphone (Samsung Galaxy S II) on the waist. Using its embedded accelerometer and gyroscope, 3-axial linear acceleration and 3-axial angular velocity at a constant rate of 50Hz was captured. The experiments have been video-recorded to label the data manually. The obtained dataset has been randomly partitioned into two sets, where 70% of the volunteers was selected for generating the training data and 30% the test data. 
The sensor signals (accelerometer and gyroscope) were pre-processed by applying noise filters and then sampled in fixed-width sliding windows of 2.56 sec and 50% overlap (128 readings/window). The sensor acceleration signal, which has gravitational and body motion components, was separated using a Butterworth low-pass filter into body acceleration and gravity. The gravitational force is assumed to have only low frequency components, therefore a filter with 0.3 Hz cutoff frequency was used. From each window, a vector of features was obtained by calculating variables from the time and frequency domain. 

### Attribute Information:

For each record in the dataset it is provided: 
- Triaxial acceleration from the accelerometer (total acceleration) and the estimated body acceleration. 
- Triaxial Angular velocity from the gyroscope. 
- A 561-feature vector with time and frequency domain variables. 
- Its activity label. 
- An identifier of the subject who carried out the experiment.

### Transformations

For detailed information on tranformations are descripbed in details in README.md and run_analysis.R. 

The following steps were taken to tranform the raw data:
- 1. The training and the test sets were merged to create one data set.
- 2. Only the measurements on the mean and standard deviation for each measurement were extracted. 
- 3. Descriptive activity names were used to name the activities in the data set.
- 4. Appropriately labeled the data set with descriptive variable names. 
- 5. From the data set in step 4, a second, independent tidy data set was created with the average of each variable for each activity and each subject.

### Varibles

- Unique IDs (type: interger) -- 1 thru 30: that uniquely identifies 1 of 30 volunteers aged between 19-48 years.
- Activity description (type: string) -- 1 of 6 distinct activities during which the feature was measured such as LAYING, SITTING, STANDING, WALKING, WALKING DOWNSTAIRS, WALKING UPSTAIRS.
- Mean and std -- averages of the given feature variable by subject and activity
- Features (string) -- variable that describes how to interpret each feature name (see below)


### Features:

tBodyAcc-XYZ <br/>
tGravityAcc-XYZ <br/>
tBodyAccJerk-XYZ <br/>
tBodyGyro-XYZ <br/>
tBodyGyroJerk-XYZ <br/>
tBodyAccMag <br/>
tGravityAccMag <br/>
tBodyAccJerkMag <br/>
tBodyGyroMag <br/>
tBodyGyroJerkMag <br/>
fBodyAcc-XYZ <br/>
fBodyAccJerk-XYZ <br/>
fBodyGyro-XYZ <br/>
fBodyAccMag <br/>
fBodyAccJerkMag <br/>
fBodyGyroMag <br/>
fBodyGyroJerkMag <br/>
