# ============================================================
# Data_Clean.R
#
# PURPOSE
#   Reads the raw Excel exports for the bacteria/antibiotics
#   project, fixes a couple of known data-quality issues, and
#   saves each cleaned table as its own .rds file in the "Data"
#   folder. The app script (Bio181L_App_TE) loads these files
#   with readRDS() -- no re-cleaning needed every time the app
#   starts. An .rds file stores the table exactly as R holds it,
#   so column types (numbers, Dates, TRUE/FALSE) come back
#   unchanged -- nothing has to be re-guessed like with a .csv.
#
# BEFORE YOU RUN THIS
#   1. Open this project through its .Rproj file (not by double-
#      clicking the script directly). That makes RStudio's
#      "working directory" the project folder automatically, so
#      the relative file paths below just work.
#   2. Put all the raw .xlsx files, unchanged, in a folder named
#      "Data_Raw" next to this script. (If your folders still use the
#      old names "data-raw" / "data", section 0 below renames them.)
#   3. Install these packages once, if you haven't already:
#        install.packages(c("readxl", "dplyr", "purrr", "stringr",
#                            "readr", "lubridate"))
# ============================================================

library(readxl)     # reads .xlsx files into R
library(dplyr)      # data-cleaning verbs: mutate(), rename(), across()
library(purrr)      # lets us repeat the same steps across many files at once
library(stringr)    # tools for cleaning up text
library(readr)      # type_convert(): re-guesses column types below (reading only; nothing is written as .csv)
library(lubridate)  # ymd(): parses the "date" columns properly

# ------------------------------------------------------------
# 0. Folder name check -- makes the project folders match the
#    names in the planning file EXACTLY: "Data_Raw" (raw .xlsx
#    pulls) and "Data" (cleaned .rds files). If a folder still
#    has an older name, it is renamed in place; if it already
#    has the right name, nothing happens, so this is safe to
#    re-run any time.
# ------------------------------------------------------------
fix_folder_name <- function(old_names, new_name) {
  # list.dirs() reports each folder with the capitalization it truly
  # has on disk. (dir.exists("Data") would say TRUE for a folder named
  # "data" on Windows/Mac, which would hide a case-only mismatch.)
  existing <- list.dirs(".", recursive = FALSE, full.names = FALSE)

  if (new_name %in% existing) return(invisible(NULL))  # already correct

  found <- intersect(old_names, existing)              # any old spelling?
  if (length(found) > 0) {
    # Rename in two hops through a temporary name. Windows/Mac treat
    # "data" and "Data" as the same folder, so a direct one-step
    # rename can silently do nothing.
    temp_name <- paste0(new_name, "_temp_rename")
    file.rename(found[1], temp_name)
    file.rename(temp_name, new_name)
    message("Renamed folder '", found[1], "' to '", new_name, "'")
  } else {
    dir.create(new_name)                               # nothing there yet
    message("Created folder '", new_name, "'")
  }
}

fix_folder_name(c("data-raw", "data_raw", "Data-Raw", "Data_raw", "DATA_RAW"), "Data_Raw")
fix_folder_name(c("data", "DATA"), "Data")

# ------------------------------------------------------------
# 1. List every table we have, and where its file lives
#    (all inside the Data_Raw folder).
#    A named vector like this means the rest of the script can
#    loop over `table_files` instead of repeating the same code
#    14 separate times. To add the 15th table once you have it,
#    you'll just add one more line here -- nothing else changes.
# ------------------------------------------------------------
table_files <- c(
  institutions         = "Data_Raw/institutions.xlsx",
  samples              = "Data_Raw/samples.xlsx",
  dilutions            = "Data_Raw/dilutions.xlsx",
  library_collections  = "Data_Raw/library-collections.xlsx",
  aa_screen_libraries  = "Data_Raw/aa-screen-libraries.xlsx",
  isolates             = "Data_Raw/isolates.xlsx",
  streaks              = "Data_Raw/streaks.xlsx",
  aa_screen_isolates   = "Data_Raw/aa-screen-isolates.xlsx",
  gram_stains          = "Data_Raw/gram-stains.xlsx",
  other_tests          = "Data_Raw/other-tests.xlsx",
  rrna_analyses        = "Data_Raw/rrna-analyses.xlsx",
  antibio_suscepts     = "Data_Raw/antibio-suscepts.xlsx",
  eukaryotics          = "Data_Raw/eukaryotics.xlsx",
  extracts             = "Data_Raw/extracts.xlsx"
  # aa_screen_extracts = "Data_Raw/aa-screen-extracts.xlsx",  # add once you have it
)

# ------------------------------------------------------------
# 2. A function that cleans ONE table.
#    We write the cleaning logic once, then apply it to every
#    file in the loop below. If we find another issue later, we
#    only have to fix it in this one place.
# ------------------------------------------------------------
clean_table <- function(df) {
  df <- df %>%
    # The exported files use a single space " " for missing
    # values instead of leaving the cell truly blank. That means
    # R's is.na() misses them entirely. This line finds every
    # text column, trims whitespace, and turns anything left
    # empty into a real NA (R's built-in "missing value" marker).
    mutate(across(where(is.character), ~ na_if(str_trim(.x), ""))) %>%
    # In every table, the column called "name" is actually a
    # lookup showing the *institution's* name -- not that row's
    # own label (the row's own label lives in "nickname"). We
    # rename it here so it's unambiguous every time you see it
    # later. any_of() just means "rename it if this column
    # exists" so the script won't error on a table that lacks it.
    rename(any_of(c(institution_name = "name")))

  # If this table has a "date" column, parse it from text (e.g.
  # "2024/09/18") into a real R Date. ymd() reads the digits in
  # year-month-day order regardless of the separator used, so it copes
  # fine if the source ever switches between "/" and "-".
  if ("date" %in% names(df)) {
    df <- df %>% mutate(date = lubridate::ymd(date))
  }

  # Columns like sample_ph, air_temperature, or duration are genuinely
  # numeric, but because some cells came in as a blank space, R read the
  # WHOLE column as text (a column can only have one type, so one
  # non-numeric cell forces the rest to text too). Now that those blanks
  # are real NAs, type_convert() re-guesses a sensible type -- numeric,
  # logical, or left as text -- for every remaining character column,
  # the same way it would when reading a fresh CSV.
  df <- suppressMessages(readr::type_convert(df))

  # Columns named "num_something" (e.g. num_streak, num_extract) are
  # rollup COUNTS -- "how many streaks does this isolate have?" -- not
  # measurements. A blank cell there means that count is genuinely zero,
  # not "we don't know." So for just these columns, we convert to a real
  # number and fill any remaining blanks with 0 instead of leaving NA.
  # coalesce(x, 0) means "use x if it has a value, otherwise use 0."
  num_cols <- str_subset(names(df), "^num_")
  df %>%
    mutate(across(all_of(num_cols), ~ coalesce(as.numeric(.x), 0)))
}

# ------------------------------------------------------------
# 3. Read + clean every file, all at once.
#    imap() walks through table_files, and for each entry it
#    gets BOTH the file path (.x) and that entry's name (.y,
#    e.g. "isolates"), so we can print progress as we go.
# ------------------------------------------------------------
tep_data <- imap(table_files, function(path, tbl_name) {
  message("Reading ", tbl_name, " ...")
  # By default, read_excel() only looks at the first 1,000 rows to guess
  # each column's type. A column that's mostly blank in that window gets
  # guessed as "logical" -- then real values further down the sheet get
  # silently turned into NA because they don't fit that guess. Since our
  # tables run up to ~24,000 rows, we raise guess_max well past that so
  # it scans the WHOLE sheet before deciding each column's type.
  read_excel(path, guess_max = 1e5) %>% clean_table()
})

# tep_data is now a named list of tibbles (data frames), e.g.
#   tep_data$isolates
#   tep_data$samples
# Explore any one of them in the RStudio Viewer with:
#   View(tep_data$isolates)

# ------------------------------------------------------------
# 4. Quick sanity check -- print size + missingness for every
#    table so you can eyeball whether anything looks off before
#    trusting the data downstream.
# ------------------------------------------------------------
iwalk(tep_data, function(df, tbl_name) {
  pct_missing <- round(100 * sum(is.na(df)) / (nrow(df) * ncol(df)), 1)
  cat(sprintf("%-22s %6d rows  %3d cols  %5s%% missing\n",
              tbl_name, nrow(df), ncol(df), pct_missing))
})

# ------------------------------------------------------------
# 5. Join helpers -- attach related-table context onto any table
#    that carries the matching foreign key, without repeating the
#    same left_join() everywhere it's needed later.
# ------------------------------------------------------------

# join_with_sample(): attach columns from `samples` onto any table
# that already has a sample_id column (antibio_suscepts, streaks, etc.)
join_with_sample <- function(child_df) {
  sample_info <- tep_data$samples %>%
    # institution_name already exists on every child table (we renamed
    # it there earlier), so drop it here rather than bringing in a
    # second, identical copy.
    select(-institution_name) %>%
    # Prefix every OTHER column with "sample_". rename_with()'s second
    # argument is a tidyselect expression -- "-id" means "every column
    # except id" -- so id is left alone (it's about to become the join
    # key) while things like date/nickname/sample_type become
    # sample_date/sample_nickname/sample_sample_type. That guarantees
    # nothing here can ever collide with the child table's own columns,
    # even ones that happen to share a name but mean something different.
    rename_with(~ paste0("sample_", .x), -id)

  child_df %>%
    left_join(sample_info, by = c("sample_id" = "id"))
}

# Quick test -- run these two lines by hand in the console (not part of
# the automatic pipeline) to confirm the join behaves correctly:
#   combined <- join_with_sample(tep_data$antibio_suscepts)
#   nrow(combined) == nrow(tep_data$antibio_suscepts)  # should be TRUE
#   names(combined)                                    # new sample_* columns

# join_with_institution(): attach columns from `institutions` onto any
# table that already has an institution_name column (basically all of
# them). Unlike sample_id/id, institution_name is a text match rather
# than a numeric foreign key -- worth spot-checking once with:
#   anti_join(tep_data$samples, tep_data$institutions, by = "institution_name")
# which should return zero rows if every name lines up cleanly.
join_with_institution <- function(child_df) {
  institution_info <- tep_data$institutions %>%
    # id is that table's own internal row number -- nothing else refers
    # to it, so it's safe to drop (unlike institution_name, which is the
    # actual join key and has to stay).
    select(-id) %>%
    # Prefix everything EXCEPT the join key, same idea as
    # join_with_sample: country_name becomes institution_country_name,
    # so it's unambiguous which table it came from and it can never
    # collide with a same-named column elsewhere.
    rename_with(~ paste0("institution_", .x), -institution_name)

  child_df %>%
    left_join(institution_info, by = "institution_name")
}

# ------------------------------------------------------------
# 7. build_display_table() -- a "dressed up" version of any table,
#    ready to show in the app. It checks which join keys a table
#    actually has, and attaches sample_* / institution_* context
#    accordingly. Since both joins are many-to-one (many streaks
#    per sample, many samples per institution), this only ADDS
#    columns -- it never multiplies rows, so row counts stay exactly
#    what they were in the original table.
# ------------------------------------------------------------
build_display_table <- function(tbl_name) {
  df <- tep_data[[tbl_name]]

  if ("sample_id" %in% names(df)) {
    df <- df %>% join_with_sample()
  }
  if ("institution_name" %in% names(df)) {
    df <- df %>% join_with_institution()
  }

  df
}

# Try it out -- compare the original to the "dressed up" version:
#   View(tep_data$isolates)
#   View(build_display_table("isolates"))

# ------------------------------------------------------------
# 6. Build ready-to-use combined views.
#    The app COULD call join_with_sample()/join_with_institution()
#    itself every time it draws a table, but doing that join once
#    here, right after cleaning, means the app just displays an
#    already-joined table -- simpler code over there, and the join
#    only happens once instead of every time someone opens the app.
# ------------------------------------------------------------

# Chains both helpers: sample context, then institution context
# (institution_name survives the first join, so the second one still
# has something to match against).
join_with_context <- function(child_df) {
  child_df %>%
    join_with_sample() %>%
    join_with_institution()
}

# Only tables that actually have a sample_id column can go through
# join_with_sample() -- institutions and samples themselves are the
# "parent" tables here, not children, so they're left out of this list.
# map_lgl() checks each table in tep_data and keeps just the TRUE ones
# (the "_lgl" means it expects each check to return one TRUE/FALSE).
combinable_tables <- names(tep_data)[
  map_lgl(tep_data, ~ "sample_id" %in% names(.x))
]

tep_data_combined <- tep_data[combinable_tables] %>%
  map(join_with_context)

# e.g. tep_data_combined$antibio_suscepts now has the antibiotic
# results AND the sample's site/environment info AND the institution's
# country, all in one table, ready for the app to display directly.

# ------------------------------------------------------------
# 8. Save everything as .rds files in the "Data" folder.
#    An .rds file holds ONE R object, so we write one file per table
#    (rather than one file for the whole list of tables):
#      Data/isolates.rds            <- individual table
#      Data/isolates_combined.rds   <- same table + sample/institution info
#    The "_combined" ending is how the app can tell the two groups
#    apart for its "Individual" and "Combined" selector sections.
#
#    Unlike a .csv, an .rds keeps every column's type and missing
#    values (NA) exactly as they are in R, so nothing is re-guessed
#    when the table is read back in.
#
#    TO LOAD THEM BACK (e.g. in the app), read a table by name:
#      readRDS("Data/isolates.rds")
#      readRDS("Data/isolates_combined.rds")
#
#    NOTE: Data/data_dictionary.csv (the variable descriptions shown in
#    the app) is a hand-written file, NOT made by this script. It stays
#    a .csv so it can be edited in Excel.
# ------------------------------------------------------------
dir.create("Data", showWarnings = FALSE)

# Writes every table in a named list to Data/<name><suffix>.rds.
# iwalk() hands us each table (df) and its name (tbl_name) in turn.
write_table_list <- function(tbl_list, suffix = "") {
  iwalk(tbl_list, function(df, tbl_name) {
    saveRDS(df, file.path("Data", paste0(tbl_name, suffix, ".rds")))
  })
}

write_table_list(tep_data)                         # individual tables
write_table_list(tep_data_combined, "_combined")   # combined tables

message("\nDone. Saved ", length(tep_data), " individual and ",
        length(tep_data_combined), " combined .rds files to the Data folder.")
