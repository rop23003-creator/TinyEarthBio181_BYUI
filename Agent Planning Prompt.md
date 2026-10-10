What is this file? (User information only section)

This is a markdown (.md) file. This is particularly used to set up any prompts about this project for the use of any AI agent such as Claude Code from Anthropic. The reason for this is because AI agents need specific instruction to help direct them when assisting in your project. This can set constraints, point out specific goals, and tailor their prompts to suit the needs of your project. Markdown specificly is a text file that can sit nicely in your project folder. Whenever you prompt the AI agent, attach this file to give them and idea of your project without the need for further prompts to set up.

(End User information only section)

---

Preface to Tiny Earth Project Bio 181 Lab BYU-I:

  The Tiny Earth Public Database is an online tool that obtains hundreds of samples of data pertaining to bacteria collection from a variety of schools. This project is lead by the University of Wisconsin-Madison and one of the main focuses of the collection is to isolate potential antibiotic bacteria from soil samples that students collect in their labs. You can find this public data base here: https://publicdata.tinyearth.wisc.edu/
  This project, now known as the Tiny Earth Project Bio 181 Lab BYU-I is a simple but powerful data and statistics project to help the students in the Bio 181 Lab to get a feel for the fields of Biostatistics / Bioinformatics / and Biological Data Science. This project will contain code to pull current data from the Tiny Earth Public Database and clean the data for use by the students in Bio 181 Lab. After the data has been processed, the students can us the accompanied shiny app written in R using R-Studio to both view and analyse the pulled data.
  The purpose of this shiny app is to make analysis of the Tiny Earth data simple for Bio 181 Lab students by picking two factors in the data for comparison. Simple analysis methods are as followed:
    Bivariate, One quantitative response variable and one qualitative explanatory variable - Analysis of Variance (ANOVA) Test
    Bivariate, Both response variable and explanatory variable are quantitative - Simple Linear Regression Test
    Bivariate, Both response variable and explanatory variable are qualitative - Chi-squared Test of Independence
  
  This project is written in R code using R-studio IDE (https://posit.co/downloads) from Posit (https://posit.co/).
  The "Tiny Earth Project Bio 181 Lab BYU-I" project itself originates as a senior project in the 2026 fall semester by Biostatistics major Quinton Roper and assisted by Dr. Todd Kelson and Dr. Michael Terribilini of the Biology Department of Brigham Young University - Idaho, as well as Dr. Garrett Saunders of the Mathematics Department of Brigham Young University - Idaho.
  The project is intended to be used by the students of the Bio 181 Lab and to be maintained by any future student or faculty. All tests and results of this project is intended for academic purposes.
  
Project File Architecture:

  This project is built around two data folders: Data and Data_Raw
    Data_Raw --> All raw data pulled from the Tiny Earth Public Database https://publicdata.tinyearth.wisc.edu/
    Data --> All cleaned data used in the shiny app. The data is cleaned from the raw data. Additionally, new data tables will be created to condense the raw tables as much as possible
    
  Additionally, this project is built around three R scripts: Data_Pull, Data_Clean, Bio181L_App_TE
    Data_Pull --> This R script is to pull the raw data from the Tiny Earth Public Database and store the raw .xlsx files into the Data_Raw folder in our project
    Data_Clean --> This R script is to pull the raw data from the stored .xlsx files and process the data (clean and condense the data) and export it into the Data folder as .csv files 
    Bio181L_App_TE --> This R Script is to pull the .csv files into a shiny app that allows students to both view the data and run an analysis on two factors/variables they chose within the app.
    
  All files in this project are stored on github: https://github.com/rop23003-creator/TinyEarthBio181_BYUI.git
  
Project Constraints:

  This project must be able to be used by multiple users and contributors.
  The Bio181L_App_TE must be able to be use by multiple users on the web simultaneously.These users should not need to download the project files but rather need to input an .html link.
  The preferred data file should be .rds format.
  
  The code should have detailed annotations that explain the function of each part of the code so that new students and professors working on the project can easily understand the code.
  Between a .rmd (R Mark Down) file or a .qmd (Quarto Mark Down), it is preferred that a .qmd is used for this project.
  Most of the statistics tests code should be written by the project designer as well as the explanations but the app - and by extension the .html and .css or JavaScript to build it, should be mainly handled by the agent with accompanying explanation.
  
  MAJOR PROJECT NOTE: This app should be able to run within the folders of the project on any user's device that has R-studio and the github repo downloaded. Any code that causes the app to work out of that - for this app - is not necessary. Keep code simple.
  
Project App Design:
  
  The Bio181L_App_TE application should serve two functions:
    To view the cleaned data files in the Data folder. This should filter and search functions as well as a directory explaining each factor (variable - the columns) and factor level (variable data point - the rows) in the data.
    To run one of the three analysis on the two variables selected by the app user and then visualized with both graphical and numerical summaries so that the user can easily draw inferences on the data.
  Idea on what the layout should look like in the Data Viewer:
    Viewed Data table should be anchored to the right hand side of the page with filter option directly above the table. The number of data points should be listed at the bottom of the table. A Scroll should be added for both vertical and horizontal input and viewing. This should be called the "Table Viewer" section.
    A table selector should be anchored to the top-left side of the page and categorized as: Individual - for all individual tables (The same number as Tiny Earth Database); Combined - For all combined tables; Description - A description of all variables on the currently selected table. The selected table should be indicated. These sections should be collapsible.
    A "Background" button should be placed at the top of this table selector section that explains the Tiny Earth Database as well as the apps purpose. This can be viewed in a new window or perhaps in the table viewer.
    A "Run Analysis" button should be placed at the bottom-left side of this section and aligned evenly with the Selector. When interacted with this will bring up a new selection menu that has the same layout as the Table Selector in the viewer. The selector to the top-left hand side will now list the variables of the currently selected table (The table is still viable in the table viewer at this time) with check boxes and a message above that says select two variables.Two columns of these check boxes will be displayed side by side next to each variable, one column for the Response Variable and Explanatory Variable with an indicator that shows which columns are which. A "Back" button that goes back to the table selector and an "Analyse" button that will light up when two variables are selected should be at the bottom of the selector section.
    When that "Analyse" button is clicked, a new window will be opened with a graph, and numerical summary table. Two buttons should appear at the top of the page, "Learn More" and "Save PDF". At the bottom of the page, a drop down section should read "View More on Test" to which a new section includes the statistics tests for the type of variables selected and the proper diagnostics test for that given statistics test (i.e. QQ-Plot for ANOVA). Give a brief summary of the purpose of the statistics test as well mathematical models and hypothesis tests.
    "Learn More" will bring up another window that explains each statistics test.
    "Save PDF" will print the analysis page and save the files as a PDF to the users computer.
  All of this should run on the web using .html format. This should be ran independently by multiple users simultaneously.
  
AI Response and Planning:

  The AI agent should respond in a brief and strait forward paragraph so that tokens are effectively conserved.
  Any code generated should be followed by a brief explanation of its function and reason of inclusion directly in the generated code and not the response.
  Further explanation should be prompted by the project designer and not the agent.
  Before Response, the agent should plan the response out and confirmed with the project designer.
  There should be no agent involvement in the project itself - meaning that an agent should not be running the actual analysis or cleaning the data directly but rather having that R scripts to perform those functions.
  Outside internet use is accepted, especially any links provided in this file or by the project designer, however, use outside sources sparingly in response.
  The AI agent should build upon the files given instead of heavy rewriting, especially the physical UI of the app. Ask designer for any changes in this part.
  
Final Design Thoughts:

  The theme should be clean and professional for both the app and the R scripts
  The Data Tables should be cleaned and condensed in an organised and accessible format
  Any color scheme should be a combination of sea-green and cyan schemes
  The focus for the code is to be accessible to multiple teams and individuals
  The focus for the app is to be intuitive and accessible to students
  
---
  
Thank you!