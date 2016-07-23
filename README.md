# Powell_Reef_Oasis

by Robin Elahi
23 July 2016

This repository contains code that relates to the projects developed for the Powell Working Group on coral reef resilience (18-22 July). Since code may be applied across multiple end products or papers, we will store it all in a single repository for now. When each project is ready to be submitted, a new repository should be created for that project that has inside it all the code needed to replicate the results (including source files), data, and a clear README.md file explaining how to use it, along with any dependencies. Then these repos can be made public when appropriate to make our work open and repeatable.

Here are some general rules for the repo:

- DO NOT use spaces in filenames. - preferably use underscores and no '.'
- DO NOT use space in column/object titles, e.g., percentCover or percent_cover
- Comment all code as informatively as you can before sharing code

Here are our guiding principles for version control in Git:

Do:
Commit (informative message), PULL!!!! Then push
Use your scripts folder
Put general R code in the R folder (if we use other languages we can create a new folder)
Use the issues to indicate enhancements and bugs in the workspace or in code

Do NOT Do:
Push large files (50MB or larger – never) - keep these outside of the project repository on your local drive
Alter any files in workspace


This is the structure of the repository:

*create_workspace* - The create_workspace folder contains code that is used to generate the files in the workspace.  DO NOT alter this code unless you are an administrator of the repository.

*data_derived* - This folder is for all "derived" data products that contributors produce with their code.  Files can include RData, csv, text or other data files.  This is where you can share data files with collaborators that you generate and you want other people to use. 

*data_raw* - This folder is for raw data products that are less than 50mb.  Files can include RData, csv, text or other data files. For example, the raw coral data file downloaded from the Moorea LTER website. 

*figures* - This folder is for everyone to share figures or data visualizations (including markdown files) with all contributors. Be careful about the file sizes of these figures - make sure they aren't too big. Feel free to add folders for specific analyses or projects. 

*R* - This is where R code that you want to share with the group should go.  It doesn't have to be perfect, but this is code that you want other people to collaborate with you to develop.  Use an informative header indicating a title, the original author, any additional collaborators who have contributed to code development and the date the code was initiated and the date of any edits by collaborators.  

*scripts* - This folder contains individual folders for each collaborator in the repository.  

*workspace* - This folder contains the output data (i.e., processed raw data, or processed gridded data) that all analysis code should use as a base.  DO NOT alter any of the files in this folder.  Only the administrators should change these files and they should use the code in the create_workspace folder to generate these files.  These changes should only be made infrequently when major updates are required.  



### Here is a template header for your scripts:

### HEADER #####################################################################
##' @title Add your informative title here 
##'
##' @author your name here 
##' @contact your email here
##' @contributor your name here if you contributed
##' 
##' @date Add the date here
##' 
##' @description Add your description here
##' 
##' @log Add a log here
##' Add date and an informative comment here
################################################################################


### Some other guidelines:

**The R folder**: Scripts stored within the R folder should be related to ongoing projects but they do not need to be "perfect" before you push your commits to the repository. Please don't stress too much about code perfection, but do use freqent comments within your code to help explain it.  Ideally, these scripts should be updated often by all code authors and editors, so that we can make rapid forward progress. 

*Remember* to always pull before you push your commits to help avoid conflicts. 

Comments to yourself or to other potential code authors can be made using GitHub Issues, or by using comments that start with TODO or FIXME (this makes it easy to search for these potential bugs or unfinished areas). To stay organized, each script should be roughly structured as such:
 * Commented lines that state what the code does and what project(s) it is intended to relate to
 * Commented lines that state any details on potential problems or bugs in the code, estimated time to run the code (esp. if it is very long or should be run on a server)
 * Commented lines that state code authors: names of anyone who has written or edited the code   
 * Commented lines that state dates of the workshop (or when the code was started)
 * Section for import statements: All the libraries that the code uses should come at the beginning of the document so its easy for a new user to see what they need to have installed. 
 * Section to link to all the datafiles that the code relies on (again, makes it easy for new user to ensure they have access to the data before getting started).
 * Section for source code links or functions: having this at the beginning also helps a new user see what other files they need to have and make sure that the funtions are working before trying to run the code.
 * Section of the script that executes the code (does stuff). This is probably where most of your code goes. If the libraries, data, source scripts, and/or functions all ran, then the main part of your code should be good to go!

**The data_raw and data_derived folder** can be used to store relatively small data files that won't be edited frequently (e.g. mainly static files). Storing data can sometimes cause downstream problems for GitHub repositories that have limited space. 

**The scripts folder** is a space for each individual to use to play with or explore new ideas that you aren't ready to share with other contributors. Please make a subfolder within it that has your name. Within your named folder, you can store and update any scripts that you are using to explore new ideas that you aren't ready to share. Please respect the privacy of others' folders and only view your own. While this space is available for you to use if you would prefer, please do not be shy about sharing and editing your code directly within the code folder so that everyone can see and contribute to code development, even if it isn't "perfect" yet. We are all learning together, and we can all learn and move projects forward more efficiently by collaboratively coding in the *R* folder.  

### Using .Rproj and RStudio

The concept of using a repo with RStudio is that the repo and the .Rproj have the same name and the project is in the base folder of the repo.  Never change the name of the .Rproj and never move it's location or the file paths in peoples code won't work and your personal version of the .Rproj will get pushed to the repo and go on everyone's computer, which can cause problems for everyone.  

If, in the past, you have set the working directory to what ever folder you are working in - stop doing that and use complete file paths from the base of the repo file: "data_processed/datafile.csv" or "scripts/elahi/newscript.R". The reason why we only have a few folders and are trying not to nest files within many sub folders is so that those file paths stay short, but this way we can all run each other's code without editing it at all as the file paths will always stay the same.  


