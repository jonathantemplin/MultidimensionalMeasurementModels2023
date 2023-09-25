This homework assignment is built to get you working with Argon. All tasks must be executed in Argon.

To complete this homework, submit the following:

Your Argon job file (question #4 below)
The R file you created to run the analysis (question #3 below)
The resulting Argon output file (ends with extension *.o*; where o is followed by the job number)
The resulting Argon error file (ends with extension *.e*; where o is followed by the job number)
Assignment Questions:

Using the Class Data List (linked here), find the grade and quarter of the data file that you will be using this semester
Within Argon, copy the file assigned to you from Jonathan's Shared folder to your home directory.
The copy command is "cp"
The name of the data file is [Grade]_[Quarter].RData with underscores between the words of grade. For instance, 2nd Grade in Q1 translates into "2nd_Grade_Q1.RData"
The location of the data file is /Shared/lss_jtemplin/MMM2023/studentData
The data file contains a list object named "temp"
temp$responseMatrix contains the raw data file (item responses coded correct/incorrect)
temp$Qmatrix contains the item alignment
Using similar commands within Argon, copy the Apptainer image to your home directory
The location of the image is /Shared/lss_jtemplin/argonstack.sif
Create an R file that:
Reads in the data
Installs the psych package
Uses the psych package function describe() to create descriptive statistics about the data
Use print(describe(DATA)) to print the results to the output file
Create an Argon job file that executes the R file using the argonstack.sif Argon image
Use the UI queue and only one core
Submit the Argon job to the scheduler