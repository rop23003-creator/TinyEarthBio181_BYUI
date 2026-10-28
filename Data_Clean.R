# [Data_Clean.R] =====================================================================================================================================
# PURPOSE:
#   Reads the raw XLSX exports, fixes known data-quality issues, and saves each cleaned table as its own .rds file in the "Data" folder.
#   The app script (Bio181L_App_TE) loads these files with read_rds() so no cleaning is needed every time the app starts.
#   An .rds file stores the table exactly as R holds it, so column types (numbers, Dates, TRUE/FALSE) come back unchanged.
#
# BEFORE YOU RUN THIS:
#   1. Open this project through its .Rproj file instead of the script itself, making RStudio's "working directory" the project folder automatically
#   so the relative file paths below work correctly.
#   2. Run Data_Pull.R in the project first so the Data_Raw folder holds the latest files.
#   3. Install these packages once, if you haven't already: install.packages("tidyverse") [readxl comes with the tidyverse install.]
# ====================================================================================================================================================

library(tidyverse)  # dplyr, stringr, purrr, readr (type_convert()), and lubridate (ymd() for dates)
library(readxl)     # reads .xlsx files into R

# [1. List every table we have and where its file is located] ========================================================================================
# All files are held inside the Data_Raw folder. A named vector like this means the rest of the script can loop over `table_files` instead of
# repeating the same code 14 separate times.
# ====================================================================================================================================================
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
  # aa_screen_extracts = "Data_Raw/aa-screen-extracts.xlsx",  # It cannot be downloaded yet; this line is kept so it can be cleaned later.
)
missing_files <- table_files[!file.exists(table_files)]       # Confirms every raw file in table_files really exists before reading anything else.
if (length(missing_files) > 0) {
  stop("These raw files were not found:\n  ",
       paste(missing_files, collapse = "\n  "),
       "\nCheck that (1) the project was opened through its .Rproj file ",
       "and (2) the files are in the Data_Raw folder.",
       call. = FALSE)                                         # call. = FALSE keeps the error message short and clean.
}

# [2. Cleaning Setup] ================================================================================================================================
# Nothing is cleaned here yet. This section only defines the two cleaning functions that section 3a then runs on every table: tidy_names() fixes the
# column names and clean_table() fixes the values.
# ====================================================================================================================================================

  # [Column Name Cleaning] ===========================================================================================================================
  # tidy_names() turns every column name into lower-case snake_case (e.g. "Sample pH" -> "sample_ph").
  # ==================================================================================================================================================
tidy_names <- function(data_table) {                          # Makes every column name tidy snake_case so names look the same in every table.
  new_names <- names(data_table) %>%
    str_trim() %>%                                            # Removes stray spaces at each end of the name.
    str_to_lower() %>%                                        # Makes all letters lower case.
    str_replace_all("[^a-z0-9]+", "_") %>%                    # Converts all spaces, dashes, and symbols into an underscore.
    str_replace_all("^_+|_+$", "")                            # Removes underscores at each end of the name.
  new_names[new_names == ""] <- "unnamed_column"              # An empty name would break later steps of the cleaning process.
  names(data_table) <- make.unique(new_names, sep = "_")      # Duplicate names get numbered suffixes so data does not get overwritten.
  data_table
}

  # [Table Cleaning] =================================================================================================================================
  # clean_table() tidies the names, blanks, dates, and column types of one table, and removes repeated IDs from isolates and samples.
  # ==================================================================================================================================================
clean_table <- function(data_table, table_name) {             # table_name names the table in warnings and picks out isolates/samples below.
  data_table <- data_table %>%
    tidy_names() %>%
    mutate(across(where(is.character),                        # str_squish() trims both ends of the text and turns line breaks and repeated
                  ~ na_if(str_squish(.x), ""))) %>%           # spaces into one space. Anything left empty becomes NA.
    rename(any_of(c(institution_name = "name"))) %>%          # Renames "name" to "institution_name" for clarity.
    select(-starts_with("num_"))                              # Drops the count columns with "num_" to remove clutter.
  # -- Date Cleaning --
  if ("date" %in% names(data_table)) {                        # Converts date text into R's Date class so dates are correctly sorted and plotted.
    had_value <- !is.na(data_table$date)
    data_table <- data_table %>%
      mutate(date = if (inherits(date, c("Date", "POSIXt"))) {
        as_date(date)                                         # as_date() drops any time-of-day part.
      } else {
        suppressWarnings(ymd(date))                           # ymd() reads the digits in year-month-day order regardless of the separator used.
      })
    n_failed <- sum(had_value & is.na(data_table$date))
    if (n_failed > 0) {
      warning(table_name, ": ", n_failed, " date value(s) could not be read as year-month-day and are now NA.", call. = FALSE)
    }
  }
  # -- Column Type Conversion --
  data_table <- suppressMessages(type_convert(data_table))    # Now that blanks are NA, text columns holding only numbers become numeric.
  # -- Repeated IDs in isolates and samples --
  if (table_name %in% c("isolates", "samples")) {             # Joins match on these tables' IDs, so each ID must be unique.
    is_dup  <- duplicated(data_table$id) & !is.na(data_table$id)
    dup_ids <- unique(data_table$id[is_dup])                  # The IDs listed more than once while ignoring blank IDs.
    if (length(dup_ids) > 0) {                                # Keeps the first row of each, with a warning message.
      warning(table_name, ": ", length(dup_ids), " id(s) appeared more than once; kept the first row of each: ",
              paste(dup_ids, collapse = ", "), call. = FALSE)
      data_table <- filter(data_table, !is_dup)
    }
  }
  data_table
}

# [3a. Read and clean every file] ====================================================================================================================
# Reads each raw file in turn and runs clean_table() on it. The result, tepd_data_tables, is a named list holding one cleaned table per file.
# ====================================================================================================================================================
tepd_data_tables <- imap(table_files, function(path, table_name) {
  message("Reading ", table_name, " ...")                     # Walks through table_files and gets file path and name to print progress.
  read_excel(path, guess_max = 1e5) %>%                       # Raise guess_max past default so the code can scan the whole sheet.
    clean_table(table_name)
})

# [3b. Remove "Legacy" records] ======================================================================================================================
# Older records imported into Tiny Earth are marked by a nickname that starts with "Legacy" (e.g. "Legacy Soil #5859", "Legacy Isolate #31"). We drop
# those rows from every table, plus any row that belongs to a legacy sample or isolate even if its own nickname doesn't say so -- that way no table is
# left pointing at a parent that was removed.
# ====================================================================================================================================================
is_legacy <- function(nickname) {                             # TRUE for a nickname starting with "Legacy"; FALSE for any other or a blank one.
  coalesce(str_detect(nickname, regex("^legacy\\b", ignore_case = TRUE)), FALSE)
}

  # [Find legacy samples, then legacy isolates] ======================================================================================================
  # An isolate is legacy if its own nickname says so OR it came from a legacy sample.
  # ==================================================================================================================================================
legacy_sample_ids <- tepd_data_tables$samples %>%
  filter(is_legacy(nickname)) %>%
  pull(id)
legacy_isolate_ids <- tepd_data_tables$isolates %>%
  filter(is_legacy(nickname) | sample_id %in% legacy_sample_ids) %>%
  pull(id)

  # [Remove legacy rows from all tables] =============================================================================================================
  # A row is dropped if its own nickname is legacy, or if it points to a legacy sample or isolate.
  # ==================================================================================================================================================
tepd_data_tables <- imap(tepd_data_tables, function(data_table, table_name) {
  drop <- rep(FALSE, nrow(data_table))
  if ("nickname" %in% names(data_table))   drop <- drop | is_legacy(data_table$nickname)
  if ("sample_id" %in% names(data_table))  drop <- drop | data_table$sample_id %in% legacy_sample_ids
  if ("isolate_id" %in% names(data_table)) drop <- drop | data_table$isolate_id %in% legacy_isolate_ids
  if (any(drop)) {
    message("Removed ", sum(drop), " legacy row(s) from ", table_name)
  }
  filter(data_table, !drop)
})

  # [Remove institutions with nothing left] ==========================================================================================================
  # An institution whose samples were all legacy has no data left in any table, so it is removed too.
  # ==================================================================================================================================================
n_inst_before <- nrow(tepd_data_tables$institutions)
tepd_data_tables$institutions <- tepd_data_tables$institutions %>%
  filter(institution_name %in% tepd_data_tables$samples$institution_name)
n_inst_removed <- n_inst_before - nrow(tepd_data_tables$institutions)
if (n_inst_removed > 0) {
  message("Removed ", n_inst_removed, " institution(s) with no remaining samples")
}

# [3c. Fix known data-entry problems] ================================================================================================================
# These came from checking every column for blanks, zeros, and inconsistent text. Each fix is listed separately so it is easy to see -- or undo --
# what was changed.
# ====================================================================================================================================================

  # [Fix 1: zeros that are really "not measured"] ====================================================================================================
  # Outdoor air can't have 0% humidity, and an antibiotic disk with a concentration of 0 holds no antibiotic, so it isn't a real test. A
  # sample_temperature of exactly 0.0 looks like a field left at its default -- EXCEPT for winter (Dec-Mar) samples from the cold_sites below, where
  # frozen soil really does sit at 0 C, so those zeros are kept. Turning the rest into NA means the app leaves them out of analyses instead of
  # treating them as real values. NOTE: almost all kept zeros are from BYU-Idaho (Jan and Mar samples); if they turn out to be defaults after all,
  # delete the "& !winter_cold" part below.
  # ==================================================================================================================================================
cold_sites <- c("Brigham Young University–Idaho", "University of Wisconsin–Madison", "Madison College",
                "University of St. Thomas", "The College of St. Scholastica")

tepd_data_tables$samples <- tepd_data_tables$samples %>%
  mutate(winter_cold        = coalesce(institution_name %in% cold_sites & month(date) %in% c(12, 1:3), FALSE),
         air_humidity       = na_if(air_humidity, 0),
         sample_temperature = if_else(sample_temperature %in% 0 & !winter_cold, NA, sample_temperature)) %>%
  select(-winter_cold)

tepd_data_tables$antibio_suscepts <- tepd_data_tables$antibio_suscepts %>%
  mutate(concentration = na_if(concentration, 0))

  # [Fix 2: one spelling for "none"] =================================================================================================================
  # In columns like additives, "none" is a real answer ("no additive used"), so it stays -- but "None" and "none" would show up as two categories, so
  # every text column gets one lower-case spelling here. (Fix 13 later capitalizes it to "None".)
  # ==================================================================================================================================================
tepd_data_tables <- map(tepd_data_tables, function(data_table) {
  data_table %>% mutate(across(where(is.character),
                       ~ if_else(str_to_lower(.x) %in% "none", "none", .x)))
})

  # [Fix 3: one name per test in other_tests$test_type] ==============================================================================================
  # Students typed the same test many ways ("KOH", "KOH test", "KOH Test"). First we make a simple "key" from each name: lower-case, dashes to spaces,
  # and a trailing " agar" / " test" removed (so "Citrate Agar Test" and "citrate" both become "citrate"). Then the lookup below gives each key its
  # one official name. Any key NOT in the lookup keeps the name as the student entered it -- add a line here to fix a new one. coalesce() gives the
  # official name if the key is in the lookup, otherwise the original spelling.
  # ==================================================================================================================================================
test_type_names <- c(
  "anaerobic"                         = "Anaerobic Test",
  "bcp glucose"                       = "BCP Glucose Test",
  "bgp glucose"                       = "BCP Glucose Test",  # Typo.
  "bromocresol purple (bcp)"          = "BCP Glucose Test",
  "blood"                             = "Blood Agar Test",
  "casein hydrolysis"                 = "Casein Hydrolysis Test",
  "catalase"                          = "Catalase Test",
  "cetramide"                         = "Cetrimide Agar Test",  # Typo.
  "cetrimide"                         = "Cetrimide Agar Test",
  "citrate"                           = "Citrate Test",
  "citrate permease"                  = "Citrate Test",
  "cytochrome c oxidase"              = "Oxidase Test",
  "oxidase"                           = "Oxidase Test",
  "denitrification"                   = "Denitrification Test",
  "emb"                               = "EMB Agar Test",
  "emb agar growth"                   = "EMB Agar Test",
  "emb lactose fermentation"          = "EMB Agar Test",
  "fermentation"                      = "Fermentation Test",
  "florescence"                       = "Fluorescence Test",  # Typo.
  "gelatinase"                        = "Gelatinase Test",
  "glucose fermentation"              = "Glucose Fermentation Test",
  "koh"                               = "KOH Test",
  "macconkey"                         = "MacConkey Agar Test",
  "mannitol fermentation"             = "Mannitol Fermentation Test",
  "mannitol salt"                     = "Mannitol Salt Agar Test",
  "mannitol salt agar growth"         = "Mannitol Salt Agar Test",
  "mannitol salt agar growth and mannitol fermentation" = "Mannitol Salt Agar Test",
  "msa"                               = "Mannitol Salt Agar Test",
  "motility"                          = "Motility Test",
  "nitrate"                           = "Nitrate Reduction Test",
  "nitrate reduction"                 = "Nitrate Reduction Test",
  "pea"                               = "PEA Agar Test",
  "starch hydrolysis"                 = "Starch Hydrolysis Test",
  "sulfide, indole, and motility"     = "Sulfide, Indole, and Motility Test",
  "triple sugar iron"                 = "Triple Sugar Iron Test",
  "urease"                            = "Urease Test"
)

tepd_data_tables$other_tests <- tepd_data_tables$other_tests %>%
  mutate(
    test_key  = test_type %>%
      str_to_lower() %>%
      str_replace_all("-", " ") %>%
      str_squish() %>%                                        # Collapses doubled spaces (e.g. "KOH - test" -> "koh test").
      str_remove("( agar)?( test)?$"),
    test_type = coalesce(unname(test_type_names[test_key]), test_type)
  ) %>%
  select(-test_key)

  # [Fix 4: impossible or mis-entered sample values] =================================================================================================
  #   air_temperature  At the cold_sites (see Fix 1), winter (Dec-Mar) readings sit far below 0 C, yet a few are above 10 (13-32) -- a Fahrenheit
  #                 reading typed into the Celsius box. Those are converted to C (25 F -> -3.9 C), which lines them up with the rest. A row whose
  #                 sample_temperature is room temperature (15 C or more) was measured indoors, so its air reading is left as entered.
  #   lat           A latitude must be between -90 and 90. One sample has 43081360 -- really 43.08136 with the decimal point lost. We move the decimal
  #                 back so 2 digits stay in front (43081360 -> 43.08136); anything still impossible -> NA.
  #   lng           A longitude outside -180..180 can't be fixed safely -> NA.
  #   air_humidity  A value between 0 and 1 (e.g. 0.45) was entered as a fraction, so it is converted to a percent (45).
  #   depth         Depth is "how far below the surface", so a negative depth (e.g. -3) is stored as positive.
  # ==================================================================================================================================================
tepd_data_tables$samples <- tepd_data_tables$samples %>%
  mutate(
    air_temperature = if_else(institution_name %in% cold_sites & month(date) %in% c(12, 1:3) & air_temperature > 10 &
                                !coalesce(sample_temperature >= 15, FALSE),
                              round((air_temperature - 32) * 5 / 9, 1), air_temperature),
    lat = if_else(abs(lat) > 90, lat / 10^(floor(log10(abs(lat))) - 1), lat),
    lat = if_else(abs(lat) > 90, NA_real_, lat),
    lng = if_else(abs(lng) > 180, NA_real_, lng),
    air_humidity = if_else(air_humidity > 0 & air_humidity <= 1,
                           air_humidity * 100, air_humidity),
    depth = abs(depth)
  )

  # [Fix 5: known misspellings that split one group into two] ========================================================================================
  # Two misspellings are corrected wherever they appear: "Staphylococus" -> "Staphylococcus" (test_bacteria) and "Erthromycin" -> "Erythromycin"
  # (antibiotics). In eukaryotics, the two organism names below were typed ~15 different ways each, so any name starting like "S. scl..." or
  # "Macroph..." gets the one correct name. CAUTION: every name starting with "S." is caught, so a future "S. cerevisiae" would be wrongly renamed.
  # Names written out in full, like "Salvia hispanica" or "Saccharomyces cerevisiae", are not affected.
  # ==================================================================================================================================================
tepd_data_tables <- map(tepd_data_tables, function(data_table) {
  data_table %>%                                              # any_of() skips tables that don't have the column.
    mutate(across(any_of("test_bacteria"), ~ str_replace(.x, "Staphylococus", "Staphylococcus")),
           across(any_of("antibiotics"),   ~ str_replace(.x, "Erthromycin", "Erythromycin")))
})

tepd_data_tables$eukaryotics <- tepd_data_tables$eukaryotics %>%
  mutate(eukaryotic_organism = case_when(
    str_detect(str_to_lower(eukaryotic_organism), "^(s\\.|scl|scer|sel)") ~ "Sclerotinia sclerotiorum",
    str_detect(str_to_lower(eukaryotic_organism), "^(m\\.|macroph)")      ~ "Macrophomina phaseolina",
    TRUE ~ eukaryotic_organism
  ))

  # [Fix 6: one spelling for values that differ only by capitals or spaces] ==========================================================================
  # "Loam" / "loam" or "Control" / "control" would show up as two groups. unify_case() finds every group of values that match once lower-cased, and
  # gives the whole group its MOST COMMON spelling. Nicknames and DNA sequences are skipped -- those are labels, not categories.
  # ==================================================================================================================================================
unify_case <- function(x) {
  key <- str_to_lower(str_squish(x))
  spellings <- tibble(x, key) %>%
    filter(!is.na(x)) %>%
    count(key, x, sort = TRUE) %>%                            # Puts the most common spelling first...
    distinct(key, .keep_all = TRUE)                           # ...and keeps just that one per key.
  spellings$x[match(key, spellings$key)]
}

tepd_data_tables <- map(tepd_data_tables, function(data_table) {
  data_table %>% mutate(across(where(is.character) & !any_of(c("nickname", "fasta_text")),
                       unify_case))
})

  # [Fix 7: rows exported twice] =====================================================================================================================
  # The website export sometimes repeats a record on several identical rows (same id, same everything), so the extra copies are removed. Rows that
  # match in every column EXCEPT id are kept: those are separate records -- usually a test repeated on the same day with the same result
  # (a replicate) -- and removing them would change the counts.
  # ==================================================================================================================================================
tepd_data_tables <- imap(tepd_data_tables, function(data_table, table_name) {
  is_dup <- duplicated(data_table)
  if (any(is_dup)) {
    message("Removed ", sum(is_dup), " exact copy row(s) from ", table_name)
  }
  filter(data_table, !is_dup)
})

  # [Fix 8: the same test giving opposite answers] ===================================================================================================
  # Some isolates were tested more than once and recorded BOTH results (e.g. "Yes" and "No" against the same bacterium). We can't know which one is
  # right, so every row in that group is marked inconclusive rather than letting one result be picked by chance. The values are compared in lower
  # case, so the check keeps working if the website changes their capitals.
  # ==================================================================================================================================================
tepd_data_tables$aa_screen_isolates <- tepd_data_tables$aa_screen_isolates %>%
  group_by(isolate_id, test_bacteria) %>%
  mutate(conflict = all(c("yes", "no") %in% str_to_lower(antibiotic_activity))) %>%
  ungroup()
message("Marked ", sum(tepd_data_tables$aa_screen_isolates$conflict),
        " conflicting aa_screen_isolates row(s) as Inconclusive")
tepd_data_tables$aa_screen_isolates <- tepd_data_tables$aa_screen_isolates %>%
  mutate(antibiotic_activity = if_else(conflict, "Inconclusive", antibiotic_activity)) %>%
  select(-conflict)

tepd_data_tables$gram_stains <- tepd_data_tables$gram_stains %>%
  group_by(isolate_id) %>%
  mutate(conflict = all(c("gram positive", "gram negative") %in% str_to_lower(result))) %>%
  ungroup()
message("Marked ", sum(tepd_data_tables$gram_stains$conflict),
        " conflicting gram_stains row(s) as Inconclusive")
tepd_data_tables$gram_stains <- tepd_data_tables$gram_stains %>%
  mutate(result = if_else(conflict, "Inconclusive", result)) %>%
  select(-conflict)

  # [Fix 9: 16S rRNA genus results] ==================================================================================================================
  # "No NCBI BLAST hits" means NO genus was found, and "Other" names no genus either, so both become NA instead of looking like one more kind of
  # bacterium. An isolate given two different genera is set to NA too (same reason as Fix 8). Finally, genus_confident flags each identification:
  # TRUE when the DNA match (percent_identity) is at least 90%, FALSE below that, NA if there is no genus or no match % -- filter on it to keep only
  # trustworthy genus calls.
  # ==================================================================================================================================================
tepd_data_tables$rrna_analyses <- tepd_data_tables$rrna_analyses %>%
  mutate(genus = if_else(genus %in% c("No NCBI BLAST hits", "Other"), NA, genus)) %>%
  group_by(isolate_id) %>%
  mutate(genus = if (n_distinct(genus, na.rm = TRUE) > 1) NA_character_ else genus) %>%
  ungroup() %>%
  mutate(genus_confident = if_else(is.na(genus), NA, percent_identity >= 90))

  # [Fix 10: one wording per result in other_tests$results] ==========================================================================================
  # Students typed the same answer ~200 ways ("+", "Postive", "Catalase positive", "negative; colorless", ...). Each result is replaced by one label,
  # matched by keyword. Blood Agar results are a type of hemolysis rather than positive/negative, so they get Alpha / Beta / Gamma hemolysis.
  # MacConkey Agar results often name both the Gram reaction and lactose fermentation ("Gram negative, lactose fermenting"), so for that test the
  # result is the LACTOSE reading: Positive = ferments lactose (pink/red colonies), Negative = does not; an answer that only names the Gram reaction
  # becomes "Other". Results that say both positive and negative (e.g. "Fermentation positive, gas production negative") or neither (e.g. "Purple
  # tube") become "Other". The checks run top to bottom, and the first one that matches wins. A row with no result at all holds no data, so it is
  # removed.
  # ==================================================================================================================================================
standardize_result <- function(results, test_type) {
  x       <- str_to_lower(results)
  blood   <- test_type %in% "Blood Agar Test"
  mac     <- test_type %in% "MacConkey Agar Test"
  pos     <- str_detect(x, paste0("pos|postive|^\\++$|\\(\\+\\)|, \\+$|\\byes\\b|produces|is produced|(?<!no )bubbl|^motile?$|",
                                  "ferments|fermenting|has lactose"))
  neg     <- str_detect(x, "neg|\\(-\\)|^-$|, -$|\\bno\\b|\\bnot\\b|non-|\\bnone\\b|colorless")
  mac_pos <- str_detect(x, "lactose|ferment|pink|\\bred\\b")
  mac_neg <- str_detect(x, "no lactose|not (a )?(lactose )?ferment|does not produce lactose|lactose production negative|non-?lactose")
  case_when(
    is.na(x)                                                   ~ NA_character_,
    blood & str_detect(x, "alpha.*beta|no/partial")            ~ "Other",            # Two answers at once.
    blood & str_detect(x, "alpha|partial")                     ~ "Alpha hemolysis",
    blood & str_detect(x, "beta|complete|^b hemo")             ~ "Beta hemolysis",
    blood & str_detect(x, "gamma|no hemoly")                   ~ "Gamma hemolysis",
    str_detect(x, "inconclusive|\\+/-|n/a")                    ~ "Inconclusive",
    str_detect(x, "no growth|did not grow|no specific growth") ~ "No growth",
    mac & mac_neg                                              ~ "Negative",         # Does not ferment lactose.
    mac & mac_pos                                              ~ "Positive",         # Ferments lactose.
    mac & str_detect(x, "gram")                                ~ "Other",            # Gram reaction only, no lactose reading.
    pos & !neg                                                ~ "Positive",
    neg & !pos                                                 ~ "Negative",
    TRUE                                                       ~ "Other"
  )
}

n_tests_before <- nrow(tepd_data_tables$other_tests)
tepd_data_tables$other_tests <- tepd_data_tables$other_tests %>%
  mutate(results = standardize_result(results, test_type)) %>%
  filter(!is.na(results))
message("Removed ", n_tests_before - nrow(tepd_data_tables$other_tests), " other_tests row(s) with no result")

  # [Fix 11: Excel rounding leftovers and impossible numbers] ========================================================================================
  # Excel stored some numbers slightly off (26.6000003815 instead of 26.6, 9.98999977 instead of 10). Rounding to 1 decimal place puts them back.
  # Then, values that can't be real become NA:
  #   temperature  No plate is incubated at 0 C or below, or at 45 C and above. A 48 is usually the duration (48 hours) typed in the wrong box.
  #   cfug         1.4e13 colonies per gram is 50,000x the next highest count -- a typo.
  #   duration     An extract incubated 1000 hours (6 weeks) is 3x the next longest -- a typo.
  # ==================================================================================================================================================
tepd_data_tables <- map(tepd_data_tables, function(data_table) {
  data_table %>%
    mutate(across(any_of(c("temperature", "duration", "media_used_concentration")), ~ round(.x, 1)),
           across(any_of("temperature"), ~ if_else(.x <= 0 | .x >= 45, NA, .x)))
})

tepd_data_tables$dilutions <- tepd_data_tables$dilutions %>%
  mutate(cfug = if_else(cfug > 1e12, NA, cfug))

tepd_data_tables$extracts <- tepd_data_tables$extracts %>%
  mutate(duration = if_else(duration >= 1000, NA, duration))

  # [Fix 12: library activity counts repeated on every isolate row] ==================================================================================
  # In aa_screen_libraries, a library screened with several isolates repeats its activity_* counts on each isolate's row, so adding up the counts
  # would count the same library several times. The counts are kept on the first row of each library and test bacterium, and set to NA on the rest.
  # ==================================================================================================================================================
tepd_data_tables$aa_screen_libraries <- tepd_data_tables$aa_screen_libraries %>%
  group_by(id, test_bacteria) %>%
  mutate(across(starts_with("activity_"), ~ if_else(row_number() == 1, .x, NA))) %>%
  ungroup()

  # [Fix 13: one casing style for category values] ===================================================================================================
  # Some columns were written in Title Case ("Isolate Did Not Grow") and others in lower case ("gram positive", "none"). The columns below are put in
  # Sentence case -- first letter capital, the rest lower case ("Isolate did not grow", "Gram positive", "None", "Off-white, pale yellow"). Names
  # with their own capitals (species, media, institutions, test types) and dilution_buffer (acronyms like PBS) are left as written.
  # ==================================================================================================================================================
sentence_case_cols <- c("additives", "antibiotic_activity", "drying_agent_used", "eukaryotic_inhibition", "isolate_form", "result", "soil_type",
                        "morphology_color", "morphology_edge", "morphology_elevation", "morphology_surface", "morphology_whole_colony")

tepd_data_tables <- map(tepd_data_tables, function(data_table) {
  data_table %>% mutate(across(any_of(sentence_case_cols), str_to_sentence))
})

  # [Fix 14: drop empty and low-value columns] =======================================================================================================
  # Some columns (e.g. gram_stains$cell_shape, rrna_analyses$primers_used) were never filled in by anyone -- not even in the raw files -- so any
  # column with no values at all is removed. The columns in drop_columns are removed too: they are mostly empty, almost always the same value, or
  # free-text notes that can't be analyzed, so they only clutter the app. "all" applies to every table that has the column. Delete a name from the
  # list to bring that column back. A row that becomes an exact copy of another once its columns are dropped (e.g. two library_collections rows that
  # differed only by isolate_id) is removed too.
  # expected_empty lists the columns that are already known to be empty. If any OTHER column turns up empty (e.g. after a new Data_Pull.R), it is
  # still dropped, but with a warning, so a column the app relies on can't disappear without notice. A column in expected_empty that gets filled in
  # later is kept automatically.
  # ==================================================================================================================================================
drop_columns <- list(
  all                 = c("media_mod", "additive_concentration",           # 79-98% empty in every table that has them
                          "experimental_design", "protocol",               # free-text notes
                          "test_date"),                                    # 36-41% empty and usually differs from date; date is kept
  samples             = c("sample_type",                                   # 98% Soil; the rest are soil textures (see soil_type)
                          "country",                                       # 98% United States; institutions has the country
                          "site_description"),                             # free-text notes
  extracts            = c("resin_used", "volume_of_liquid_culture",        # 88-94% empty; resin_used holds only 0s
                          "solid_or_liquid_culture",                       # 32 of 33 Solid
                          "solvent_used_for_extraction",                   # 31 of 33 ethyl acetate
                          "temperature"),                                  # 91% 28 C
  dilutions           = c("morphology_color", "morphology_edge", "morphology_elevation",
                          "morphology_surface", "morphology_whole_colony", # 86-87% empty; colony morphology is recorded in streaks
                          "media_used_concentration"),                     # 90% 100
  antibio_suscepts    = "additives",                                       # 98% none
  aa_screen_libraries = "additives",                                       # 94% none
  eukaryotics         = c("additives",                                     # 98% none
                          "results"),                                      # 75% empty free text; eukaryotic_inhibition holds the answer
  library_collections = c("additives",                                     # 96% none
                          "isolate_id"),                                   # 98% empty (2 rows)
  gram_stains         = "cell_size",                                       # 91% empty
  rrna_analyses       = c("fasta_text",                                    # raw DNA sequence, 65% empty
                          "query_length"),                                 # 55% empty
  institutions        = "id"                                               # nothing refers to it
)

expected_empty <- list(
  library_collections = c("morphology_color", "morphology_edge", "morphology_elevation",
                          "morphology_surface", "morphology_whole_colony"),
  streaks             = c("nickname", "additives"),
  gram_stains         = "cell_shape",
  rrna_analyses       = c("primers_used", "polyamerase_used", "query_coverage"),
  extracts            = "mass_of_crude_extract"                            # only legacy extracts had a mass
)

tepd_data_tables <- imap(tepd_data_tables, function(data_table, table_name) {
  empty_cols <- names(select(data_table, where(~ all(is.na(.x)))))
  listed     <- c(drop_columns$all, drop_columns[[table_name]], expected_empty[[table_name]])
  surprise   <- setdiff(empty_cols, listed)
  if (length(surprise) > 0) {
    warning(table_name, ": column(s) with no values were dropped: ", paste(surprise, collapse = ", "),
            ". Check the raw file, or add them to expected_empty.", call. = FALSE)
  }
  drop_cols  <- intersect(c(drop_columns$all, drop_columns[[table_name]]), names(data_table)) %>%
    union(empty_cols)
  if (length(drop_cols) > 0) {
    message("Dropped empty or low-value column(s) from ", table_name, ": ",
            paste(drop_cols, collapse = ", "))
  }
  data_table <- data_table %>% select(-all_of(drop_cols))
  n_copies   <- sum(duplicated(data_table))
  if (n_copies > 0) {
    message("Removed ", n_copies, " exact copy row(s) from ", table_name)
  }
  distinct(data_table)
})

# [4a. Join helpers] =================================================================================================================================
# Attach related-table context onto any table that carries the matching foreign key, without repeating the same left_join() everywhere it's needed
# later.
#
# HOW THE TABLES LINK (from the Tiny Earth schema diagram):
#   institutions --(institution_name)--> samples
#   samples  --(sample_id)--> isolates, dilutions,
#                             library_collections, aa_screen_libraries
#   isolates --(isolate_id)--> streaks, gram_stains, other_tests,
#                              rrna_analyses, antibio_suscepts,
#                              eukaryotics, aa_screen_isolates, extracts
# dilutions and aa_screen_libraries also carry an isolate_id, so they get isolate info as well (library_collections' isolate_id is dropped in Fix 14).
# So any table can be traced up the chain: isolate -> sample -> institution.
# ====================================================================================================================================================

  # [join_parent()] ==================================================================================================================================
  # The ONE function that does the work for the three helpers below, so the naming rules live in a single place.
  #   child_df   the table we are adding columns TO
  #   parent_df  the table the new columns come FROM
  #   prefix     text put in front of the parent's columns (e.g. "sample_")
  #   child_key  the column in child_df used to match
  #   parent_key the matching column in parent_df (default "id")
  #   carry      "link" columns in the parent (e.g. sample_id inside isolates) that should keep their plain name. If the child already has its own
  #              copy it is dropped; if not, it is brought along so the NEXT join in the chain has a key.
  # ==================================================================================================================================================
join_parent <- function(child_df, parent_df, prefix, child_key,
                        parent_key = "id", carry = character(0)) {
  child_names <- names(child_df)

  carry      <- intersect(carry, names(parent_df))  # Only link columns the parent really has.
  carry_drop <- intersect(carry, child_names)       # The child already has its own copy.
  carry_keep <- setdiff(carry, child_names)         # The child lacks it, so bring it along.
  # -- Prefix the parent's columns --
  # Prefix every OTHER parent column so it cannot collide with a column of the child table. A name that ALREADY starts with the prefix (e.g.
  # sample_ph) is left alone, which avoids names like sample_sample_ph -- unless the child has a column with that exact name, in which case it is
  # prefixed anyway to stay unique.
  parent_df <- parent_df %>%
    select(-any_of(carry_drop)) %>%
    rename_with(
      ~ if_else(str_starts(.x, prefix) & !(.x %in% child_names),
                .x, paste0(prefix, .x)),
      -all_of(c(parent_key, carry_keep))
    )
  # -- Name-collision check --
  # A parent column like "depth" becomes "sample_depth" above, but if the child ALREADY has a "sample_depth" column, left_join() would quietly rename
  # both to sample_depth.x / sample_depth.y. Stop instead, so a new or changed table can't slip confusing names into the app.
  clash <- intersect(setdiff(names(parent_df), parent_key), child_names)
  if (length(clash) > 0) {
    stop("join_parent(): these columns would exist twice after joining: ",
         paste(clash, collapse = ", "), call. = FALSE)
  }
  # -- The join --
  # setNames(parent_key, child_key) builds c(child_key = parent_key), which is how left_join() is told "match these two differently named columns".
  # na_matches = "never" stops a blank (NA) key from matching a blank key in the parent -- by default left_join() treats NA = NA as a match, which
  # would attach some random parent row's info to every row missing its key.
  child_df %>%
    left_join(parent_df, by = setNames(parent_key, child_key),
              na_matches = "never")
}

  # [join_with_isolate()] ============================================================================================================================
  # Attach columns from `isolates` onto any table that has an isolate_id column (streaks, gram_stains, antibio_suscepts, etc.). The isolate's own
  # sample_id / dilution_id / institution_name are carried along unprefixed so the sample and institution joins can follow.
  # ==================================================================================================================================================
join_with_isolate <- function(child_df) {
  join_parent(child_df, tepd_data_tables$isolates, prefix = "isolate_",
              child_key = "isolate_id",
              carry = c("sample_id", "dilution_id", "institution_name"))
}

  # [join_with_sample()] =============================================================================================================================
  # Attach columns from `samples` onto any table that has a sample_id column. institution_name is carried (only if the table does not already have it)
  # so the institution join can follow.
  # ==================================================================================================================================================
join_with_sample <- function(child_df) {
  join_parent(child_df, tepd_data_tables$samples, prefix = "sample_",
              child_key = "sample_id", carry = "institution_name")
}

  # [join_with_institution()] ========================================================================================================================
  # Attach columns from `institutions` onto any table that has an institution_name column. Unlike sample_id/id, institution_name is a text match
  # rather than a numeric foreign key, so a misspelled name simply finds no match (see the spot-check in section 5's Manual checks).
  # ==================================================================================================================================================
join_with_institution <- function(child_df) {
  if (!"institution_name" %in% names(child_df)) {
    stop("join_with_institution(): table has no institution_name column.",
         call. = FALSE)
  }
  join_parent(child_df, tepd_data_tables$institutions, prefix = "institution_",
              child_key = "institution_name", parent_key = "institution_name")
}

# [4b. Build ready-to-use combined views] ============================================================================================================
# The app COULD call join_with_sample()/join_with_institution() itself every time it draws a table, but doing that join once here, right after
# cleaning, means the app just displays an already-joined table -- simpler code over there, and the join only happens once instead of every time
# someone opens the app. For example, tepd_combined_tables$antibio_suscepts has the antibiotic results AND the sample's site/environment info AND the
# institution's country, all in one table, ready for the app to display directly.
# ====================================================================================================================================================

  # [join_with_context()] ============================================================================================================================
  # Chains the helpers in the order the schema diagram shows: isolate context first, then sample context, then institution context. Each step only
  # runs if the table has the key it needs, so one function works for every table. The earlier steps carry along the keys that the later steps need
  # (sample_id, then institution_name).
  # ==================================================================================================================================================
join_with_context <- function(child_df) {
  if ("isolate_id" %in% names(child_df)) child_df <- join_with_isolate(child_df)
  if ("sample_id"  %in% names(child_df)) child_df <- join_with_sample(child_df)
  join_with_institution(child_df)
}

  # [Which tables get a combined version] ============================================================================================================
  # Every table except `institutions` (the top "parent" of the chain, with nothing above it to join) gets a combined version. We keep tables that have
  # at least one link column. any() asks "does it have ANY of these?", and setdiff() removes institutions from the result.
  # ==================================================================================================================================================
combinable_tables <- setdiff(
  names(tepd_data_tables)[
    map_lgl(tepd_data_tables, ~ any(c("isolate_id", "sample_id", "institution_name") %in% names(.x)))
  ],
  "institutions"
)

tepd_combined_tables <- tepd_data_tables[combinable_tables] %>%
  map(join_with_context)

# [4c. Row-count check on the joins] =================================================================================================================
# The joins above are meant to ADD columns only. If samples$id or institution_name ever contained a duplicate, a left_join() would quietly copy the
# matching rows and inflate the table, which would skew every statistic in the app. This section stops the script before anything is saved if that
# happens.
# ====================================================================================================================================================

  # [Step A] =========================================================================================================================================
  # The three join keys must be unique in their parent tables. anyDuplicated() returns 0 when there are no repeats, so "> 0" means at least one key
  # value appears more than once.
  # ==================================================================================================================================================
if (anyDuplicated(tepd_data_tables$samples$id) > 0) {
  stop("samples$id has duplicate values, so joins would add extra rows.",
       call. = FALSE)
}
if (anyDuplicated(tepd_data_tables$isolates$id) > 0) {
  stop("isolates$id has duplicate values, so joins would add extra rows.",
       call. = FALSE)
}
if (anyDuplicated(tepd_data_tables$institutions$institution_name) > 0) {
  stop("institutions$institution_name has duplicate values, so joins would add extra rows.",
       call. = FALSE)
}

  # [Step B] =========================================================================================================================================
  # Each combined table must have exactly as many rows as its original. imap_lgl() runs the comparison once per table and returns one TRUE/FALSE per
  # table, named by table.
  # ==================================================================================================================================================
rows_match <- imap_lgl(tepd_combined_tables, function(data_table, table_name) {
  nrow(data_table) == nrow(tepd_data_tables[[table_name]])
})
if (!all(rows_match)) {
  stop("Row count changed after joining for: ",
       paste(names(rows_match)[!rows_match], collapse = ", "),
       call. = FALSE)
}
message("Join check passed: every combined table kept its original row count.")

# [5. Finalize] ======================================================================================================================================
# Tags each column's type, saves every cleaned table to the "Data" folder, then prints a final report so you can check the results before using the
# app.
# ====================================================================================================================================================

  # [Tag each column's type] =========================================================================================================================
  # So R (and the app) treats each column correctly: ids and counts become whole numbers (integer), and category text becomes a factor (R's type for
  # categories). Nicknames stay plain text -- they are labels, not categories. Dates and TRUE/FALSE columns are already tagged. as_whole() leaves a
  # column as it is if any value has decimals, so a future 0.5 is never cut down to 0, or if a value is too big for an integer (over ~2.1 billion),
  # which is why cfug (colonies per gram, up to 1e12) stays a decimal number.
  # Factor categories are sorted alphabetically, except the columns in level_order, which get a meaningful order (e.g. Susceptible, Intermediate,
  # Resistant) so tables and graphs list them that way. A category not in the list is placed after the listed ones.
  # ==================================================================================================================================================
whole_number_cols <- c("duration", "media_used_concentration", "concentration", "number_of_isolates", "number_of_plates_used")

level_order <- list(
  antibiotic_activity   = c("Yes", "No", "Inconclusive", "Isolate did not grow"),
  eukaryotic_inhibition = c("Yes", "No", "Inconclusive"),
  result                = c("Gram positive", "Gram negative", "Inconclusive"),
  results               = c("Positive", "Negative", "Alpha hemolysis", "Beta hemolysis", "Gamma hemolysis", "No growth", "Inconclusive", "Other",
                            "Susceptible", "Intermediate", "Resistant")     # other_tests and antibio_suscepts share the name "results"
)

as_whole <- function(x) {
  fits <- all(x == round(x), na.rm = TRUE) && all(abs(x) <= .Machine$integer.max, na.rm = TRUE)
  if (fits) as.integer(x) else x
}

tag_types <- function(data_table) {
  data_table %>%
    mutate(across((matches("(^|_)id$") | any_of(whole_number_cols) | starts_with("activity_")) & where(is.numeric), as_whole),
           across(where(is.character) & !ends_with("nickname"), as.factor)) %>%
    mutate(across(any_of(names(level_order)),                 # intersect() keeps only the categories this table really has.
                  ~ fct_relevel(.x, intersect(level_order[[cur_column()]], levels(.x)))))
}
tepd_data_tables     <- map(tepd_data_tables, tag_types)
tepd_combined_tables <- map(tepd_combined_tables, tag_types)

  # [Save the tables] ================================================================================================================================
  # Every table is saved as an .rds file in the "Data" folder: individual tables as <name>.rds, combined tables as <name>_combined.rds.
  # ==================================================================================================================================================
dir.create("Data", showWarnings = FALSE)                      # Makes the Data folder if it isn't there already.

write_table_list <- function(tbl_list, suffix = "") {
  iwalk(tbl_list, function(data_table, table_name) {
    write_rds(data_table, file.path("Data", paste0(table_name, suffix, ".rds")), compress = "gz")
  })
}

write_table_list(tepd_data_tables)                            # Individual tables.
write_table_list(tepd_combined_tables, "_combined")           # Combined tables.

  # [Final Report] ===================================================================================================================================
  # Prints one line per cleaned table with its rows, columns, and % of empty cells, then how many files were saved. Look for a table that came out
  # with 0 rows, far fewer columns, or a much higher % missing than last time -- a sign that a fix, the legacy filter, or the new raw files went
  # wrong.
  # ==================================================================================================================================================
message("\nTable size and missing-data report:")
iwalk(tepd_data_tables, function(data_table, table_name) {
  pct_missing <- round(100 * sum(is.na(data_table)) / (nrow(data_table) * ncol(data_table)), 1)
  cat(sprintf("%-22s %6d rows  %3d cols  %5s%% missing\n",
              table_name, nrow(data_table), ncol(data_table), pct_missing))
})

message("\nDone. Saved ", length(tepd_data_tables), " individual and ",
        length(tepd_combined_tables), " combined .rds files to the Data folder.")

  # [Manual checks] ==================================================================================================================================
  # Not run automatically. After the script finishes, paste these lines into the console to spot-check the joins by hand:
  #   anti_join(tepd_data_tables$samples, tepd_data_tables$institutions, by = "institution_name")   # should return 0 rows (every name matches)
  #   combined <- join_with_context(tepd_data_tables$antibio_suscepts)
  #   nrow(combined) == nrow(tepd_data_tables$antibio_suscepts)                                       # should be TRUE
  #   names(combined)                                                                                 # new isolate_*, sample_*, institution_* columns
  # ==================================================================================================================================================
