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
  
App Work Update 1 (October 15, 2026 - first agent session):

  Files changed: Bio181L_App_TE.qmd (many edits) and Data_Clean.R (output format only). Data_Pull was never provided and has not been reviewed.
  Where this section disagrees with the older text above (for example ".csv" in Project File Architecture), THIS SECTION IS NEWER and wins.

  Bio181L_App_TE.qmd - changes made:
    Table Selector: Individual and Combined sections are now collapsible (open/closed state is remembered). Description is no longer a list section; it is a button placed to the left of "Run Analysis" in the same bottom row. It toggles the Variable Directory in the right panel and is filled in (solid) while that view is open.
    Variable picker (after Run Analysis): replaced the single checkbox list with two checkbox columns per variable, "Response (y)" and "Explanatory (x)". One tick per column, and a variable cannot be both. Handled by the small JavaScript function tePick() which sends input$sel_pair to R. "Analyse" lights up only for a valid pair.
    Test choice is now decided by the picker: quantitative response + qualitative explanatory = ANOVA; both quantitative = regression; both qualitative = chi-squared. A qualitative response with a quantitative explanatory variable has no test, so the picker shows a warning and keeps Analyse disabled. The old regression "which variable is y / Go" step was removed because it is now redundant.
    Data loading: the app now reads .rds files with readRDS() (see Data_Clean below). data_dictionary.csv is still read with read_csv().
    Chi-squared: when a test cannot run because a variable is left with fewer than 2 categories, the message now states how many rows have BOTH variables filled in and which variable is left with one category. Validation messages are shown in a cyan box instead of faint grey text.
    Small fixes: DataTables long-text column indices are sent as a plain list (unname), and the Back/Analyse and Description/Run Analysis button rows share their width evenly.
    Annotations: comment-only edit (no code changed). The file now opens with a "MAP OF THIS FILE" and every section carries a searchable tag: [A] YAML, [B1]-[B9] CSS, [C] JavaScript, [D1]-[D9] setup/helpers/statistics ([D8] is the DESIGNER SECTION), [E] page shell, [F1]-[F9] server. Please keep adding notes and tags to any new code.

  Data_Clean.R - changes made:
    Section 8 now saves one .rds file per table with saveRDS() (for example Data/isolates.rds and Data/isolates_combined.rds) instead of .csv files. Header comments were corrected. Cleaning and join logic was NOT changed.
    The .rds output replaces the ".csv" wording in "Project File Architecture" above. Old .csv table files in Data/ should be deleted (keep Data/data_dictionary.csv, which is hand-written and not made by any script).

  How it was tested: R 4.3.3 and Quarto 1.6.42 in the agent's sandbox, using SYNTHETIC data only (never the real Tiny Earth data). Simulated app sessions (shiny::testServer) ran all three tests end to end, the Quarto document rendered and served without errors, and Data_Clean.R was run on fake raw .xlsx files. NOT yet checked: a real browser (look at the collapsible arrows, picker spacing and button row width), the real Tiny Earth data, and web hosting.

  Findings and open items for the designer (nothing below has been changed unless stated):
    Chi-squared error on Samples, soil_type x sample_type ("needs at least two categories"): the test only uses rows where both variables are filled in. Most likely soil_type is only filled for soil samples, leaving sample_type with one category. This was reproduced with fake data but NOT confirmed on the real table. Check in the console: table(s$soil_type, s$sample_type, useNA = "ifany") where s <- readRDS("Data/samples.rds"). Only the message was improved.
    ANOVA and regression have the same blind spot (variables that rarely have values on the same rows); they have not received the improved message.
    Chi-squared is already nonparametric. It uses a Monte Carlo p-value when more than 20% of expected counts are below 5. Offered but not done: let the Monte Carlo p-value drive the written conclusion in that case.
    Numeric-coded categories (such as 0/1 codes) count as quantitative, so they cannot be used in a chi-squared test. Text columns with more than 30 different values (MAX_LEVELS) are hidden from the picker. Possible fix: a way to treat a numeric column as categories.
    Data dictionary: Data/data_dictionary.csv is not created by any script, so every variable shows "not yet documented" until it is written. It also has no explanations for factor levels (the row values), which the Project App Design section asks for.
    Data_Clean.R items noticed but not changed: sections are numbered 5, 7, 6, 8 out of order; build_display_table() is defined but never used (so no samples + institutions combined table exists); aa_screen_extracts is commented out; the left joins assume samples$id and institution_name are unique, so a row-count check after the joins would be worth adding.
    Web hosting: the app is a Shiny document, so a plain web page host (such as GitHub Pages) cannot run it. A Shiny host such as shinyapps.io or Posit Connect is needed. No hosting code was added, per the MAJOR PROJECT NOTE.

  Working conventions that held this session: build on the existing files instead of rewriting; keep the sea-green and cyan theme; the statistics code in [D8] belongs to the designer, so only its inputs/outputs and messages were touched; files were saved with their original Windows (CRLF) line endings so git diffs stay small.

App Work Update 2 (October 15, 2026 - second agent session):

  Files changed: Data_Clean.R and this planning file. Bio181L_App_TE.qmd was not touched. Data_Pull has still not been provided or reviewed.
  Where this section disagrees with older text above, THIS SECTION IS NEWER and wins.

  Designer clarification - agent involvement in the data:
    The rule "no agent involvement in the project itself" means there must be NO code or workflow that hands the data to the agent and has the agent output the data in place of a script. All data in Data_Raw and Data must come from the R scripts (Data_Pull, Data_Clean). An agent MAY write, run and debug those scripts (for example on synthetic test data) and may explain results, but the scripts, not the agent, must produce the data.

  Data_Clean.R - changes made:
    Section order fixed: the sections now run 0 through 8 in order (previously 5, 7, 6, 8).
    Removed build_display_table() and its "try it out" comment block. It was never called and repeated what join_with_context() does.
    Input check added right after table_files: the script stops with a plain message listing any missing raw .xlsx files (and reminds the user to open the project through the .Rproj file) instead of failing later inside read_excel().
    Column names: new tidy_names() runs first inside clean_table(). It makes every column name lower-case snake_case (spaces/dashes/symbols become "_", ends trimmed, duplicates get _1, _2). Only names change, never values. id and *_id columns keep their meaning, so joins still work. camelCase names are NOT split (so a name like pH is not broken apart).
    Joins now follow the Tiny Earth schema diagram (institutions -> samples -> isolates -> isolate child tables). One new function, join_parent(), does the work for the three helpers join_with_isolate() (new), join_with_sample() and join_with_institution(). join_with_context() chains isolate -> sample -> institution, and each step only runs if the table has that key.
    Link columns (sample_id, dilution_id, institution_name inside isolates; institution_name inside samples) are carried along unprefixed when a table lacks them, so a child table that only has isolate_id can still be traced up to its sample and institution. This also removes the old fragility where a table without institution_name would fail the institution join.
    Double prefixes fixed: columns that already start with the prefix are left alone, so sample_sample_type is now sample_type and sample_sample_ph is now sample_ph (a prefix is still added if the child table has a column with that exact name).
    Combined tables: every table except institutions now gets a combined version. This adds samples_combined (samples + institutions) and isolates_combined (isolates + samples + institutions). The old rule (only tables with sample_id) is gone.
    New section 7 (join check): confirms samples$id, isolates$id and institutions$institution_name have no duplicates, and that every combined table has the same number of rows as its original. If a check fails, the script stops BEFORE saving any .rds file. On success it prints one confirmation message.
    All new code carries comments explaining what it does and why. Cleaning rules (blank-to-NA, dates, type_convert, num_ columns) and the saving step were NOT changed.

  How it was tested: R 4.3.3 was installed in the agent's sandbox (an earlier note in this session said R was unavailable; it only was not installed). Data_Clean.R was run start to finish on SYNTHETIC raw .xlsx files built to match the schema diagram (including messy headers such as "Isolate ID", blank cells, and a table with only isolate_id). It produced 14 individual and 13 combined .rds files with correct row counts and the new column names. The duplicate-key check and the missing-file check were also confirmed to stop the script. NOT yet checked: the REAL Tiny Earth raw files. Please run Data_Clean.R once in RStudio and confirm the "Join check passed" message appears. If a check stops the script, that points to a real problem in the raw data.

  DESIGNER ACTION NEEDED after the next real run:
    Column names changed (for example sample_sample_type -> sample_type). Update Data/data_dictionary.csv to the new names, and delete old .rds files in Data/ so none are left over from the previous naming.
    The agent has not seen the real raw column names. Run lapply(tep_data, names) after section 3 and share the output so remaining messy names can be fixed in tidy_names().

  Items noticed but intentionally NOT changed (open for the designer):
    aa_screen_extracts is still commented out until the file is available. The schema diagram shows it links through extracts (extract_id), so a join helper for it will be needed then. The diagram also marks it "found on live site only", so it may not be available from the data pull.
    The "condense the raw tables" goal in Final Design Thoughts is only partly addressed: combined tables add columns but nothing is condensed or summarized yet.
    type_convert() can turn ID-like or 0/1-coded text columns into numbers (see the numeric-coded categories note in Update 1).
    fix_folder_name() does not check whether file.rename() succeeded.
    The diagram shows dilutions and isolates linking to each other (dilutions "yields" isolates). Both carry the other's key, so dilutions_combined gets isolate columns and isolates_combined carries dilution_id, but dilution columns are not attached to isolates.

Script Work Update 3 (October 26, 2026 - third agent session):

  Files changed: Data_Pull.R (new), Data_Clean.R, Bio181L_App_TE.qmd and this planning file. Where this section disagrees with older text above, THIS SECTION IS NEWER and wins.

  Data_Pull.R (new): downloads the raw .xlsx export of every table from https://publicdata.tinyearth.wisc.edu/ into Data_Raw. For each table it reads the page's report_id and nonce (behind the "Download All (Excel)" button), requests the file, checks it is a real, readable .xlsx, and only then replaces the old copy. A failed table prints why and keeps its old file. Run order: Data_Pull.R, then Data_Clean.R, then the app.

  Data_Clean.R - changes made (tested on the REAL Tiny Earth files):
    Join safety: na_matches = "never" (a blank key no longer matches a blank key), a name-collision check, clearer date-parse and num_ warnings.
    Repeated ids in isolates and samples are collapsed to one row with a warning (other tables keep repeated ids; there they are real data).
    New section 3b removes all "Legacy" records (and rows tied to a legacy sample or isolate) from every table; drop_legacy = FALSE turns this off. Institutions left with no samples are dropped.
    New section 3c, Fixes 1-11: placeholder zeros -> NA (air_humidity, sample_temperature), one spelling for "none", one name per other_tests$test_type, impossible lat/lng/humidity/depth values, known misspellings, capitalization-only differences, rows entered twice, conflicting results for the same test, 16S genus cleanup (+ genus_confident), num_ columns recounted, completely empty columns dropped.
    Output confirmed: 14 individual + 13 combined .rds files in Data/.

  All three scripts - style pass:
    Libraries grouped at the top and condensed to library(tidyverse) plus readxl (and httr2 in Data_Pull). The app loads its packages in its own chunk [D0], right after the HTML chunk.
    Every |> pipe is now %>%.
    Comments restyled: short notes sit at the end of the line they describe (aligned, lines max 100 characters); longer notes are blocks in the form "# [Heading] =====" / text / "# =====", 150 characters wide in R (the app's YAML/CSS/HTML/JS blocks use the same shape at their older width). Code was confirmed identical before and after.

  Open items:
    aa-screen-extracts cannot be downloaded (the Tiny Earth server returns HTTP 500).
    Two duplicate isolates remain in the source data (to be fixed on Tiny Earth and re-pulled).
    Data/data_dictionary.csv does not exist yet.
    Free-text and multi-select columns are not cleaned yet; sample_type is almost all "Soil".
    After the restyle, the app still needs a click-through test in the browser. Nothing from this session is committed yet.

---
  
Thank you!