# TinyEarthBio181_BYUI

---

Instructions to copy got repo files on computer:

  cd "C:\Users\YourName\Documents"

  git clone https://github.com/rop23003-creator/TinyEarthBio181_BYUI.git    -   Initial repo copy
  
  git pull    -   Pulls files from github
  
  git add .   -   Adds files from your computer to selected commit files
  
  git commit -m"Add what you changed here in a message"   -   Commits files to be updated in git hub repo
  
  git push    -   Sends all committed files to git hub. Make sure to pull again when editing files
  
You will need to use powershell or your console / terminal on your computer to do this. Additionally, you will need to download git to your machine.

---

Preface to Tiny Earth Project Bio 181 Lab BYU-I:

  The Tiny Earth Public Database is an online tool that obtains hundreds of samples of data pertaining to bacteria collection from a variety of schools. This project is lead by the University of Wisconsin-Madison and one of the main focuses of the collection is to isolate potential antibiotic bacteria from soil samples that students collect in their labs. You can find this public data base here: https://publicdata.tinyearth.wisc.edu/
  This project, now known as the Tiny Earth Project Bio 181 Lab BYU-I is a simple but powerful data and statistics project to help the students in the Bio 181 Lab to get a feel for the fields of Biostatistics / Bioinformatics / and Biological Data Science. This project will contain code to pull current data from the Tiny Earth Public Database and clean the data for use by the students in Bio 181 Lab. After the data has been processed, the students can us the accompanied shiny app written in R using R-Studio to both view and analyse the pulled data.
  The purpose of this shiny app is to make analysis of the Tiny Earth data simple for Bio 181 Lab students by picking two factors in the data for comparison. Simple analysis methods are as followed:
    Bivariate, One quantitative response variable and one qualitative explanatory variable - Analysis of Variance (ANOVA) Test
    Bivariate, Both response variable and explanatory variable are quantitative - Simple Linear Regression Test
    Bivariate, Both response variable and explanatory variable are qualitative - Chi-squared Test of Independence
    Bivariate, One two-outcome qualitative response variable (e.g. Yes / No) and one quantitative explanatory variable - Simple Logistic Regression
  
  This project is written in R code using R-studio IDE (https://posit.co/downloads) from Posit (https://posit.co/).
  The "Tiny Earth Project Bio 181 Lab BYU-I" project itself originates as a senior project in the 2026 fall semester by Biostatistics major Quinton Roper and assisted by Dr. Todd Kelson and Dr. Michael Terribilini of the Biology Department of Brigham Young University - Idaho, as well as Dr. Garrett Saunders of the Mathematics Department of Brigham Young University - Idaho.
  The project is intended to be used by the students of the Bio 181 Lab and to be maintained by any future student or faculty. All tests and results of this project is intended for academic purposes.
  
Project File Architecture:

  This project is built around two data folders: Data and Data_Raw
    Data_Raw --> All raw data pulled from the Tiny Earth Public Database https://publicdata.tinyearth.wisc.edu/
    Data --> All cleaned data used in the shiny app. The data is cleaned from the raw data. Additionally, new data tables will be created to condense the raw tables as much as possible
    
  Additionally, this project is built around three R scripts: Data_Pull, Data_Clean, Bio181L_App_TE
    Data_Pull --> This R script is to pull the raw data from the Tiny Earth Public Database and store the raw .xlsx files into the Data_Raw folder in our project
    Data_Clean --> This R script is to pull the raw data from the stored .xlsx files and process the data (clean and condense the data) and export it into the Data folder as .rds files (an .rds file keeps every column type, such as numbers and dates, exactly as R holds it)
    Bio181L_App_TE --> This Quarto Shiny document (.qmd) loads the .rds files into a shiny app that allows students to both view the data and run an analysis on two factors/variables they chose within the app.

  Scripts are run in this order: Data_Pull, then Data_Clean, then Bio181L_App_TE (open it in R-Studio and click "Run Document").

Setup (install once):

  Paste this line into the R-Studio Console. It installs every package the three scripts need (the same line is at the top of each script):

    install.packages(c("tidyverse", "readxl", "httr2", "shiny", "DT", "car", "ggthemes", "scales", "writexl", "rmarkdown", "knitr"))

  The app also needs Quarto, which comes with R-Studio, and an internet connection to draw the equations (KaTeX).

What the app does:

  Main screen
    Table Selector --> Pick any Individual table (matches the Tiny Earth database) or Combined table (adds the related sample and institution columns)
    Table Viewer --> Search, filter, and sort the selected table
    Description --> Shows what each column (variable) of the table means
    Run Analysis --> Tick one Response (y) and one Explanatory (x) variable; the app picks the right test and the Analyze button lights up
    Background --> A short introduction to Tiny Earth and how to use the app

  Results page
    Graphical Summary --> A graph of the two variables (downloadable as PDF or PNG)
    Numerical Summary --> Summary tables (downloadable as CSV)
    View More on Test --> When the test is used, its purpose, the mathematical model and hypotheses (written as real math), the test results with a written conclusion, and the assumption checks (diagnostic plots and tests). If an assumption fails, a backup is added: Kruskal-Wallis for ANOVA, a log(y) refit for regression, and a simulation-based p-value for chi-squared
    Learn More --> A broad guide to the statistics: choosing a test, how hypothesis tests work, reading the graphs, checking assumptions, and links to the BYU-Idaho class notes
    Save PDF --> Prints the results page or saves it as a PDF
    Round buttons --> Download an analysis worksheet (once worksheets are added), explore and download the clean or raw data behind the analysis, and give feedback on the app

  Settings for maintainers (near the top of Bio181L_App_TE, section [D1])
    DEBUG_MODE --> TRUE shows a bug button in the bottom-left corner that opens each test's results page for checking layouts. Set it to FALSE before publishing for students
    WORKSHEET_ENABLED --> Set to TRUE once worksheet files are added to a Worksheets folder (named e.g. anova_worksheet.docx / anova_worksheet.pdf)
    FEEDBACK_URL --> The feedback form the speech-bubble button opens
    GRAPH_STYLE --> "economist" or "minimal" graph look

  All files in this project are stored on github: https://github.com/rop23003-creator/TinyEarthBio181_BYUI.git
