# [Data_Clean.R] =====================================================================================================================================
# PURPOSE:
#   Reads the raw XLSX exports, fixes known data-quality issues, and saves each cleaned table as its own .rds file in the "Data" folder.
#   The app script (Bio181L_App_TE) loads these files with readRDS() so no cleaning is needed every time the app starts.
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
# All files are held inside the Data_Raw folder. A named vector like this means the rest of the script can loop over `table_files` instead of repeating
# the same code 14 separate times.
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
  # aa_screen_extracts = "Data_Raw/aa-screen-extracts.xlsx",  # Currently it cannot be downloaded so we have the code to pull it in for cleaning later.
)
missing_files <- table_files[!file.exists(table_files)]       # Confirms every raw file in table_files really exists before reading anything else.
if (length(missing_files) > 0) {
  stop("These raw files were not found:\n  ",
       paste(missing_files, collapse = "\n  "),
       "\nCheck that (1) the project was opened through its .Rproj file ",
       "and (2) the files are in the Data_Raw folder.",
       call. = FALSE)                                         # call. = FALSE keeps the error message short and clean.
}

# [2. General cleaning applied to every table] =======================================================================================================
# Writes the cleaning logic once, then applies it to every file in the loop below.
# ====================================================================================================================================================
tidy_names <- function(data_table) {                      # Makes every column name tidy snake_case so names look the same in every table.
  new_names <- names(data_table) %>%
    str_trim() %>%                                        # Removes stray spaces at each end of the name.
    str_to_lower() %>%                                    # Puts all letters in lower case format.
    str_replace_all("[^a-z0-9]+", "_") %>%                # Converts all spaces, dashes, and symbols into an underscore.
    str_replace_all("^_+|_+$", "")                        # Removes underscores at each end of the name.
  new_names[new_names == ""] <- "unnamed_column"          # An empty name would break later steps of the cleaning process.
  names(data_table) <- make.unique(new_names, sep = "_")  # Duplicate names get numerated suffixes so data does not get overwritten.
  data_table
}

clean_table <- function(data_table, tbl_name) {           # tbl_name names the table in warnings and picks out isolates/samples below
  # [First pass: names, blanks, and "name"] ==========================================================================================================
  # tidy_names()  Tidy the column names FIRST so every later step (such as renaming "name" below) can rely on lower-case snake_case names.
  # mutate()      The exported files use a single space " " for missing values instead of leaving the cell truly blank. That means R's is.na() misses
  #               them entirely. This line finds every text column, trims whitespace, and turns anything left empty into a real NA (R's built-in
  #               "missing value" marker).
  # rename()      In every table, the column called "name" is actually a lookup showing the *institution's* name -- not that row's own label (the
  #               row's own label lives in "nickname"). We rename it here so it's unambiguous every time you see it later. any_of() just means "rename
  #               it if this column exists" so the script won't error on a table that lacks it.
  # ==================================================================================================================================================
  data_table <- data_table %>%
    tidy_names() %>%
    mutate(across(where(is.character), ~ na_if(str_trim(.x), ""))) %>%
    rename(any_of(c(institution_name = "name")))

  # [Dates] ==========================================================================================================================================
  # If this table has a "date" column, parse it from text (e.g. "2024/09/18") into a real R Date. ymd() reads the digits in year-month-day order
  # regardless of the separator used, so it copes fine if the source ever switches between "/" and "-". If read_excel() already read the column as
  # real dates, as_date() just drops any time-of-day part instead of re-parsing it as text. ymd() turns anything it can't read into NA with only an
  # easy-to-miss warning, so we count those failures ourselves and report them clearly.
  # ==================================================================================================================================================
  if ("date" %in% names(data_table)) {
    had_value <- !is.na(data_table$date)
    data_table <- data_table %>%
      mutate(date = if (inherits(date, c("Date", "POSIXt"))) {
        as_date(date)
      } else {
        suppressWarnings(ymd(date))
      })
    n_failed <- sum(had_value & is.na(data_table$date))
    if (n_failed > 0) {
      warning(tbl_name, ": ", n_failed, " date value(s) could not be read ",
              "as year-month-day and are now NA.", call. = FALSE)
    }
  }

  # [Column types] ===================================================================================================================================
  # Columns like sample_ph, air_temperature, or duration are genuinely numeric, but because some cells came in as a blank space, R read the WHOLE
  # column as text (a column can only have one type, so one non-numeric cell forces the rest to text too). Now that those blanks are real NAs,
  # type_convert() re-guesses a sensible type -- numeric, logical, or left as text -- for every remaining character column, the same way it would when
  # reading a fresh CSV.
  # ==================================================================================================================================================
  data_table <- suppressMessages(type_convert(data_table))

  # [Count columns: num_] ============================================================================================================================
  # Columns named "num_something" (e.g. num_streak, num_extract) are rollup COUNTS -- "how many streaks does this isolate have?" -- not measurements.
  # A blank cell there means that count is genuinely zero, not "we don't know." So for just these columns, we convert to a real number and fill any
  # remaining blanks with 0 instead of leaving NA. coalesce(x, 0) means "use x if it has a value, otherwise use 0." But a cell that HAD something
  # non-numeric in it (e.g. "n/a" or "3?") would also become NA and then 0, hiding a typo as a real zero count -- so we warn about those cells before
  # filling.
  # ==================================================================================================================================================
  num_cols <- str_subset(names(data_table), "^num_")
  for (col in num_cols) {
    as_num <- suppressWarnings(as.numeric(data_table[[col]]))
    n_bad  <- sum(!is.na(data_table[[col]]) & is.na(as_num))
    if (n_bad > 0) {
      warning(tbl_name, "$", col, ": ", n_bad, " value(s) were not numbers ",
              "and were set to 0.", call. = FALSE)
    }
    data_table[[col]] <- coalesce(as_num, 0)
  }

  # [Repeated ids in isolates and samples] ===========================================================================================================
  # In isolates and samples -- the two tables the joins look up by "id" -- each id should appear only once, but the export sometimes lists the same
  # record twice (e.g. an isolate linked to two dilutions, with only dilution_id differing). A repeated id there would make the joins below copy rows,
  # so we keep the FIRST row for each id and warn which ids were collapsed -- fix those records at the source when you can. Rows with a blank (NA) id
  # are all kept, since NA isn't a real repeated value. NOT applied to other tables: in antibio_suscepts, other_tests, etc. a repeated id is real data
  # (one row per antibiotic or test performed).
  # ==================================================================================================================================================
  if (tbl_name %in% c("isolates", "samples")) {
    is_dup  <- duplicated(data_table$id) & !is.na(data_table$id)
    dup_ids <- unique(data_table$id[is_dup])
    if (length(dup_ids) > 0) {
      warning(tbl_name, ": ", length(dup_ids), " id(s) appeared more than ",
              "once; kept the first row of each: ",
              paste(dup_ids, collapse = ", "), call. = FALSE)
      data_table <- data_table[!is_dup, ]
    }
  }
  data_table
}

# [3a. Read + clean every file, all at once] =========================================================================================================
# imap() walks through table_files, and for each entry it gets BOTH the file path (path) and that entry's name (tbl_name, e.g. "isolates"), so we
# can print progress as we go.
# ====================================================================================================================================================
tep_data <- imap(table_files, function(path, tbl_name) {
  message("Reading ", tbl_name, " ...")
  # [guess_max] ======================================================================================================================================
  # By default, read_excel() only looks at the first 1,000 rows to guess each column's type. A column that's mostly blank in that window gets guessed
  # as "logical" -- then real values further down the sheet get silently turned into NA because they don't fit that guess. Since our tables run up to
  # ~24,000 rows, we raise guess_max well past that so it scans the WHOLE sheet before deciding each column's type.
  # ==================================================================================================================================================
  read_excel(path, guess_max = 1e5) %>% clean_table(tbl_name)
})

# [What tep_data looks like now] =====================================================================================================================
# tep_data is now a named list of tibbles (data frames), e.g.
#   tep_data$isolates
#   tep_data$samples
# Explore any one of them in the RStudio Viewer with:
#   View(tep_data$isolates)
# ====================================================================================================================================================

# [3b. Remove "Legacy" records] ======================================================================================================================
# Older records imported into Tiny Earth are marked by a nickname that starts with "Legacy" (e.g. "Legacy Soil #5859", "Legacy Isolate #31"). We drop
# those rows from EVERY table, plus any row that belongs to a legacy sample or isolate even if its own nickname doesn't say so -- that way no table is
# left pointing at a parent that was removed. The raw files in Data_Raw are not touched. Set drop_legacy to FALSE to keep legacy records instead.
# ====================================================================================================================================================
drop_legacy <- TRUE

if (drop_legacy) {
  # [is_legacy()] ====================================================================================================================================
  # TRUE for a nickname starting with "Legacy" (any capitalization). A blank (NA) nickname counts as FALSE, so filter() won't drop it.
  # ==================================================================================================================================================
  is_legacy <- function(nickname) {
    coalesce(str_detect(nickname, regex("^legacy\\b", ignore_case = TRUE)), FALSE)
  }

  # [Find legacy samples, then legacy isolates] ======================================================================================================
  # An isolate is legacy if its own nickname says so OR it came from a legacy sample.
  # ==================================================================================================================================================
  legacy_sample_ids <- tep_data$samples %>%
    filter(is_legacy(nickname)) %>%
    pull(id)
  legacy_isolate_ids <- tep_data$isolates %>%
    filter(is_legacy(nickname) | sample_id %in% legacy_sample_ids) %>%
    pull(id)

  tep_data <- imap(tep_data, function(data_table, tbl_name) {
    # [Mark rows to drop] ============================================================================================================================
    # Start with "keep every row", then rule rows out one check at a time -- each check only runs if the table has that column.
    # ================================================================================================================================================
    drop <- rep(FALSE, nrow(data_table))
    if ("nickname" %in% names(data_table))   drop <- drop | is_legacy(data_table$nickname)
    if ("sample_id" %in% names(data_table))  drop <- drop | data_table$sample_id %in% legacy_sample_ids
    if ("isolate_id" %in% names(data_table)) drop <- drop | data_table$isolate_id %in% legacy_isolate_ids

    if (any(drop)) {
      message("Removed ", sum(drop), " legacy row(s) from ", tbl_name)
    }
    data_table[!drop, ]
  })

  # [Institutions with nothing left] =================================================================================================================
  # With legacy samples gone, some institutions have no samples left. Keep only institutions that still appear in the samples table, so the app
  # doesn't list schools with nothing to show.
  # ==================================================================================================================================================
  n_before <- nrow(tep_data$institutions)
  tep_data$institutions <- tep_data$institutions %>%
    filter(institution_name %in% tep_data$samples$institution_name)
  n_gone <- n_before - nrow(tep_data$institutions)
  if (n_gone > 0) {
    message("Removed ", n_gone, " institution(s) with no remaining samples")
  }
}

# [3c. Fix known data-entry problems] ================================================================================================================
# These came from checking every column for blanks, zeros, and inconsistent text. Each fix is listed separately so it is easy to see -- or undo --
# what was changed.
# ====================================================================================================================================================

# [Fix 1: zeros that are really "not measured"] ======================================================================================================
# Outdoor air can't have 0% humidity, and a sample_temperature of exactly 0.0 (most of them from BYU-Idaho) looks like a field left at its default
# rather than a real reading. Turning them into NA means the app leaves them out of analyses instead of treating them as real values. NOTE: this
# applies to every school, so a genuine reading of 0 degrees C is removed too.
# ====================================================================================================================================================
tep_data$samples <- tep_data$samples %>%
  mutate(air_humidity       = na_if(air_humidity, 0),
         sample_temperature = na_if(sample_temperature, 0))

# [Fix 2: one spelling for "none"] ===================================================================================================================
# In additives/media_mod, "none" is a real answer ("no additive used"), so it stays -- but "None" and "none" would show up as two categories.
# ====================================================================================================================================================
tep_data <- map(tep_data, function(data_table) {
  data_table %>% mutate(across(where(is.character),
                       ~ if_else(str_to_lower(.x) %in% "none", "none", .x)))
})

# [Fix 3: one name per test in other_tests$test_type] ================================================================================================
# Students typed the same test many ways ("KOH", "KOH test", "KOH Test"). First we make a simple "key" from each name: lower-case, dashes to spaces,
# and a trailing " agar" / " test" removed (so "Citrate Agar Test" and "citrate" both become "citrate"). Then the lookup below gives each key its one
# official name. Any key NOT in the lookup keeps the name as the student entered it -- add a line here to fix a new one. coalesce() gives the official
# name if the key is in the lookup, otherwise the original spelling.
# ====================================================================================================================================================
test_type_names <- c(
  "anaerobic"                         = "Anaerobic Test",
  "bcp glucose"                       = "BCP Glucose Test",
  "bgp glucose"                       = "BCP Glucose Test",  # typo
  "bromocresol purple (bcp)"          = "BCP Glucose Test",
  "blood"                             = "Blood Agar Test",
  "casein hydrolysis"                 = "Casein Hydrolysis Test",
  "catalase"                          = "Catalase Test",
  "cetramide"                         = "Cetrimide Agar Test",  # typo
  "cetrimide"                         = "Cetrimide Agar Test",
  "citrate"                           = "Citrate Test",
  "citrate permease"                  = "Citrate Test",
  "cytochrome c oxidase"              = "Oxidase Test",
  "oxidase"                           = "Oxidase Test",
  "denitrification"                   = "Denitrification Test",
  "emb"                               = "EMB Agar Test",
  "emb agar growth"                   = "EMB Agar Test",
  "emb lactose fermentation"          = "EMB Agar Test",
  "florescence"                       = "Fluorescence Test",  # typo
  "gelatinase"                        = "Gelatinase Test",
  "koh"                               = "KOH Test",
  "macconkey"                         = "MacConkey Agar Test",
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

tep_data$other_tests <- tep_data$other_tests %>%
  mutate(
    test_key  = test_type %>%
      str_to_lower() %>%
      str_replace_all("-", " ") %>%
      str_squish() %>%                          # collapse doubled spaces
      str_remove("( agar)?( test)?$"),
    test_type = coalesce(unname(test_type_names[test_key]), test_type)
  ) %>%
  select(-test_key)

# [Fix 4: impossible or mis-entered sample values] ===================================================================================================
#   lat           A latitude must be between -90 and 90. One sample has 43081360 -- really 43.08136 with the decimal point lost. We move the decimal
#                 back so 2 digits stay in front (43081360 -> 43.08136); anything still impossible -> NA.
#   lng           A longitude outside -180..180 can't be fixed safely -> NA.
#   air_humidity  A value between 0 and 1 (e.g. 0.45) was entered as a fraction, so it is converted to a percent (45).
#   depth         Depth is "how far below the surface", so a negative depth (e.g. -3) is stored as positive.
#   country       A blank country is filled with the institution's country.
# ====================================================================================================================================================
inst_country <- setNames(tep_data$institutions$country_name,
                         tep_data$institutions$institution_name)

tep_data$samples <- tep_data$samples %>%
  mutate(
    lat = if_else(abs(lat) > 90, lat / 10^(floor(log10(abs(lat))) - 1), lat),
    lat = if_else(abs(lat) > 90, NA_real_, lat),
    lng = if_else(abs(lng) > 180, NA_real_, lng),
    air_humidity = if_else(air_humidity > 0 & air_humidity <= 1,
                           air_humidity * 100, air_humidity),
    depth   = abs(depth),
    country = coalesce(country, unname(inst_country[institution_name]))
  )

# [Fix 5: known misspellings that split one group into two] ==========================================================================================
# Two misspellings are corrected wherever they appear: "Staphylococus" -> "Staphylococcus" (test_bacteria) and "Erthromycin" -> "Erythromycin"
# (antibiotics). In eukaryotics, the two organism names below were typed ~15 different ways each, so any name starting like "S. scl..." or
# "Macroph..." gets the one correct name. CAUTION: every name starting with "S." is caught, so a future "S. cerevisiae" would be wrongly renamed.
# Names written out in full, like "Salvia hispanica" or "Saccharomyces cerevisiae", are not affected.
# ====================================================================================================================================================
tep_data <- map(tep_data, function(data_table) {
  if ("test_bacteria" %in% names(data_table)) {
    data_table$test_bacteria <- str_replace(data_table$test_bacteria, "Staphylococus", "Staphylococcus")
  }
  if ("antibiotics" %in% names(data_table)) {
    data_table$antibiotics <- str_replace(data_table$antibiotics, "Erthromycin", "Erythromycin")
  }
  data_table
})

tep_data$eukaryotics <- tep_data$eukaryotics %>%
  mutate(eukaryotic_organism = case_when(
    str_detect(str_to_lower(eukaryotic_organism), "^(s\\.|scl|scer|sel)") ~ "Sclerotinia sclerotiorum",
    str_detect(str_to_lower(eukaryotic_organism), "^(m\\.|macroph)")      ~ "Macrophomina phaseolina",
    TRUE ~ eukaryotic_organism
  ))

# [Fix 6: one spelling for values that differ only by capitals or spaces] ============================================================================
# "Loam" / "loam" or "Control" / "control" would show up as two groups. unify_case() finds every group of values that match once lower-cased, and
# gives the whole group its MOST COMMON spelling. Nicknames and DNA sequences are skipped -- those are labels, not categories.
# ====================================================================================================================================================
unify_case <- function(x) {
  key <- str_to_lower(str_squish(x))
  spellings <- tibble(x, key) %>%
    filter(!is.na(x)) %>%
    count(key, x, sort = TRUE) %>%          # most common spelling first
    distinct(key, .keep_all = TRUE)         # keep just that one per key
  spellings$x[match(key, spellings$key)]
}

tep_data <- map(tep_data, function(data_table) {
  data_table %>% mutate(across(where(is.character) & !any_of(c("nickname", "fasta_text")),
                       unify_case))
})

# [Fix 7: rows entered twice] ========================================================================================================================
# A row that matches another row in EVERY column except its id was almost certainly submitted twice, so the extra copy is removed. In samples,
# isolates, and dilutions, a copy is only removed if no other table points to its id -- otherwise that table's rows would lose their link.
# ====================================================================================================================================================
referenced_ids <- list(
  samples   = unlist(map(tep_data, ~ .x[["sample_id"]])),
  isolates  = unlist(map(tep_data, ~ .x[["isolate_id"]])),
  dilutions = unlist(map(tep_data, ~ .x[["dilution_id"]]))
)

tep_data <- imap(tep_data, function(data_table, tbl_name) {
  is_dup <- duplicated(select(data_table, -any_of("id")))
  if (tbl_name %in% names(referenced_ids)) {
    is_dup <- is_dup & !(data_table$id %in% referenced_ids[[tbl_name]])
  }
  if (any(is_dup)) {
    message("Removed ", sum(is_dup), " row(s) entered twice from ", tbl_name)
  }
  data_table[!is_dup, ]
})

# [Fix 8: the same test giving opposite answers] =====================================================================================================
# Some isolates were tested more than once and recorded BOTH results (e.g. "Yes" and "No" against the same bacterium). We can't know which one is
# right, so every row in that group is marked inconclusive rather than letting one result be picked by chance.
# ====================================================================================================================================================
tep_data$aa_screen_isolates <- tep_data$aa_screen_isolates %>%
  group_by(isolate_id, test_bacteria) %>%
  mutate(conflict = all(c("Yes", "No") %in% antibiotic_activity)) %>%
  ungroup()
message("Marked ", sum(tep_data$aa_screen_isolates$conflict),
        " conflicting aa_screen_isolates row(s) as Inconclusive")
tep_data$aa_screen_isolates <- tep_data$aa_screen_isolates %>%
  mutate(antibiotic_activity = if_else(conflict, "Inconclusive", antibiotic_activity)) %>%
  select(-conflict)

tep_data$gram_stains <- tep_data$gram_stains %>%
  group_by(isolate_id) %>%
  mutate(conflict = all(c("gram positive", "gram negative") %in% result)) %>%
  ungroup()
message("Marked ", sum(tep_data$gram_stains$conflict),
        " conflicting gram_stains row(s) as inconclusive")
tep_data$gram_stains <- tep_data$gram_stains %>%
  mutate(result = if_else(conflict, "inconclusive", result)) %>%
  select(-conflict)

# [Fix 9: 16S rRNA genus results] ====================================================================================================================
# "No NCBI BLAST hits" means NO genus was found, so it becomes NA instead of looking like one more kind of bacterium. An isolate given two different
# genera is set to NA too (same reason as Fix 8). Finally, genus_confident flags each identification: TRUE when the DNA match (percent_identity) is at
# least 90%, FALSE below that, NA if there is no genus or no match % -- filter on it to keep only trustworthy genus calls.
# ====================================================================================================================================================
tep_data$rrna_analyses <- tep_data$rrna_analyses %>%
  mutate(genus = na_if(genus, "No NCBI BLAST hits")) %>%
  group_by(isolate_id) %>%
  mutate(genus = if (n_distinct(genus, na.rm = TRUE) > 1) NA_character_ else genus) %>%
  ungroup() %>%
  mutate(genus_confident = if_else(is.na(genus), NA, percent_identity >= 90))

# [Fix 10: recount the "num_" columns] ===============================================================================================================
# Tiny Earth computed these counts over its WHOLE database -- including the legacy records removed above -- so they no longer match our tables. Each
# count is rebuilt from the rows we actually have. The lookup says which table each count comes from; a num_ column whose table we don't have (e.g.
# num_aa_screen_extract) can't be recounted, so it is dropped.
# ====================================================================================================================================================
num_sources <- list(
  isolates = c(num_streak             = "streaks",
               num_aa_screen_isolate  = "aa_screen_isolates",
               num_gram_stain_test    = "gram_stains",
               num_other_test         = "other_tests",
               num_rrna_analysis      = "rrna_analyses",
               num_antibio_suscept    = "antibio_suscepts",
               num_eukaryotic         = "eukaryotics",
               num_extract            = "extracts",
               num_library_collection = "library_collections",
               num_aa_screen_library  = "aa_screen_libraries"),
  samples  = c(num_dilution           = "dilutions",
               num_library_collection = "library_collections",
               num_aa_screen_library  = "aa_screen_libraries",
               num_isolate            = "isolates"),
  institutions = c(num_sample  = "samples",
                   num_isolate = "isolates")
)
count_keys <- list(isolates     = c("id", "isolate_id"),  # (own key, child's key) per parent
                   samples      = c("id", "sample_id"),
                   institutions = c("institution_name", "institution_name"))

tep_data <- imap(tep_data, function(data_table, tbl_name) {
  sources <- num_sources[[tbl_name]]
  for (col in intersect(names(sources), names(data_table))) {
    keys   <- count_keys[[tbl_name]]
    counts <- table(tep_data[[sources[[col]]]][[keys[2]]])  # rows per parent
    data_table[[col]] <- as.numeric(coalesce(as.integer(counts[as.character(data_table[[keys[1]]])]), 0L))
  }
  data_table %>% select(-any_of(setdiff(str_subset(names(data_table), "^num_"), names(sources))))
})

# [Fix 11: drop columns that are completely empty] ===================================================================================================
# Some columns (e.g. gram_stains$cell_shape, rrna_analyses$primers_used) were never filled in by anyone -- not even in the raw files. They only
# clutter the app, so any column with no values at all is removed.
# ====================================================================================================================================================
tep_data <- imap(tep_data, function(data_table, tbl_name) {
  empty_cols <- names(data_table)[map_lgl(data_table, ~ all(is.na(.x)))]
  if (length(empty_cols) > 0) {
    message("Dropped empty column(s) from ", tbl_name, ": ",
            paste(empty_cols, collapse = ", "))
  }
  data_table %>% select(-all_of(empty_cols))
})

# [4. Quick sanity check] ============================================================================================================================
# Print size + missingness for every table so you can eyeball whether anything looks off before trusting the data downstream.
# ====================================================================================================================================================
iwalk(tep_data, function(data_table, tbl_name) {
  pct_missing <- round(100 * sum(is.na(data_table)) / (nrow(data_table) * ncol(data_table)), 1)
  cat(sprintf("%-22s %6d rows  %3d cols  %5s%% missing\n",
              tbl_name, nrow(data_table), ncol(data_table), pct_missing))
})

# [5. Join helpers] ==================================================================================================================================
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
# library_collections and aa_screen_libraries also carry an isolate_id, so they get isolate info as well.
# So any table can be traced up the chain: isolate -> sample -> institution.
# ====================================================================================================================================================

# [join_parent()] ====================================================================================================================================
# The ONE function that does the work for the three helpers below, so the naming rules live in a single place.
#   child_df   the table we are adding columns TO
#   parent_df  the table the new columns come FROM
#   prefix     text put in front of the parent's columns (e.g. "sample_")
#   child_key  the column in child_df used to match
#   parent_key the matching column in parent_df (default "id")
#   carry      "link" columns in the parent (e.g. sample_id inside isolates) that should keep their plain name. If the child already has its own copy
#              it is dropped; if not, it is brought along so the NEXT join in the chain has a key.
#   drop       parent columns to throw away (not needed in the child)
# ====================================================================================================================================================
join_parent <- function(child_df, parent_df, prefix, child_key,
                        parent_key = "id", carry = character(0),
                        drop = character(0)) {
  child_names <- names(child_df)

  carry      <- intersect(carry, names(parent_df))  # only link columns the parent really has
  carry_drop <- intersect(carry, child_names)       # child already has its own copy
  carry_keep <- setdiff(carry, child_names)         # child lacks it -> bring it along

  # [Prefix the parent's columns] ====================================================================================================================
  # Prefix every OTHER parent column so it cannot collide with a column of the child table. A name that ALREADY starts with the prefix (e.g.
  # sample_type) is left alone, which avoids names like sample_sample_type -- unless the child has a column with that exact name, in which case it is
  # prefixed anyway to stay unique.
  # ==================================================================================================================================================
  parent_df <- parent_df %>%
    select(-any_of(c(drop, carry_drop))) %>%
    rename_with(
      ~ if_else(str_starts(.x, prefix) & !(.x %in% child_names),
                .x, paste0(prefix, .x)),
      -all_of(c(parent_key, carry_keep))
    )

  # [Name-collision check] ===========================================================================================================================
  # A parent column like "type" becomes "sample_type" above, but if the child ALREADY has a "sample_type" column, left_join() would quietly rename
  # both to sample_type.x / sample_type.y. Stop instead, so a new or changed table can't slip confusing names into the app.
  # ==================================================================================================================================================
  clash <- intersect(setdiff(names(parent_df), parent_key), child_names)
  if (length(clash) > 0) {
    stop("join_parent(): these columns would exist twice after joining: ",
         paste(clash, collapse = ", "), call. = FALSE)
  }

  # [The join] =======================================================================================================================================
  # setNames(parent_key, child_key) builds c(child_key = parent_key), which is how left_join() is told "match these two differently named columns".
  # na_matches = "never" stops a blank (NA) key from matching a blank key in the parent -- by default left_join() treats NA = NA as a match, which
  # would attach some random parent row's info to every row missing its key.
  # ==================================================================================================================================================
  child_df %>%
    left_join(parent_df, by = setNames(parent_key, child_key),
              na_matches = "never")
}

# [join_with_isolate()] ==============================================================================================================================
# Attach columns from `isolates` onto any table that has an isolate_id column (streaks, gram_stains, antibio_suscepts, etc.). The isolate's own
# sample_id / dilution_id / institution_name are carried along unprefixed so the sample and institution joins can follow.
# ====================================================================================================================================================
join_with_isolate <- function(child_df) {
  join_parent(child_df, tep_data$isolates, prefix = "isolate_",
              child_key = "isolate_id",
              carry = c("sample_id", "dilution_id", "institution_name"))
}

# [join_with_sample()] ===============================================================================================================================
# Attach columns from `samples` onto any table that has a sample_id column. institution_name is carried (only if the table does not already have it)
# so the institution join can follow.
# ====================================================================================================================================================
join_with_sample <- function(child_df) {
  join_parent(child_df, tep_data$samples, prefix = "sample_",
              child_key = "sample_id", carry = "institution_name")
}

# [join_with_institution()] ==========================================================================================================================
# Attach columns from `institutions` onto any table that has an institution_name column. Unlike sample_id/id, institution_name is a text match rather
# than a numeric foreign key -- worth spot-checking once with:
#   anti_join(tep_data$samples, tep_data$institutions, by = "institution_name")
# which should return zero rows if every name lines up cleanly. The institutions table's own "id" is dropped: nothing else refers to it.
# ====================================================================================================================================================
join_with_institution <- function(child_df) {
  if (!"institution_name" %in% names(child_df)) {
    stop("join_with_institution(): table has no institution_name column.",
         call. = FALSE)
  }
  join_parent(child_df, tep_data$institutions, prefix = "institution_",
              child_key = "institution_name", parent_key = "institution_name",
              drop = "id")
}

# [6. Build ready-to-use combined views] =============================================================================================================
# The app COULD call join_with_sample()/join_with_institution() itself every time it draws a table, but doing that join once here, right after
# cleaning, means the app just displays an already-joined table -- simpler code over there, and the join only happens once instead of every time
# someone opens the app. For example, tep_data_combined$antibio_suscepts has the antibiotic results AND the sample's site/environment info AND the
# institution's country, all in one table, ready for the app to display directly.
# ====================================================================================================================================================

# [join_with_context()] ==============================================================================================================================
# Chains the helpers in the order the schema diagram shows: isolate context first, then sample context, then institution context. Each step only runs
# if the table has the key it needs, so one function works for every table. The earlier steps carry along the keys that the later steps need
# (sample_id, then institution_name).
# ====================================================================================================================================================
join_with_context <- function(child_df) {
  if ("isolate_id" %in% names(child_df)) child_df <- join_with_isolate(child_df)
  if ("sample_id"  %in% names(child_df)) child_df <- join_with_sample(child_df)
  join_with_institution(child_df)
}

# [Which tables get a combined version] ==============================================================================================================
# Every table except `institutions` (the top "parent" of the chain, with nothing above it to join) gets a combined version. We keep tables that have
# at least one link column. any() asks "does it have ANY of these?", and setdiff() removes institutions from the result.
# ====================================================================================================================================================
combinable_tables <- setdiff(
  names(tep_data)[
    map_lgl(tep_data, ~ any(c("isolate_id", "sample_id", "institution_name") %in% names(.x)))
  ],
  "institutions"
)

tep_data_combined <- tep_data[combinable_tables] %>%
  map(join_with_context)

# [Quick test] =======================================================================================================================================
# Run these lines by hand in the console (not part of the automatic pipeline) to confirm a join behaves correctly:
#   combined <- join_with_context(tep_data$antibio_suscepts)
#   nrow(combined) == nrow(tep_data$antibio_suscepts)  # should be TRUE
#   names(combined)        # new isolate_*, sample_*, institution_* columns
# ====================================================================================================================================================

# [7. Row-count check on the joins] ==================================================================================================================
# The joins above are meant to ADD columns only. If samples$id or institution_name ever contained a duplicate, a left_join() would quietly copy the
# matching rows and inflate the table, which would skew every statistic in the app. This section stops the script before anything is saved if that
# happens.
# ====================================================================================================================================================

# [Step A] ===========================================================================================================================================
# The three join keys must be unique in their parent tables. anyDuplicated() returns 0 when there are no repeats, so "> 0" means at least one key
# value appears more than once.
# ====================================================================================================================================================
if (anyDuplicated(tep_data$samples$id) > 0) {
  stop("samples$id has duplicate values, so joins would add extra rows.",
       call. = FALSE)
}
if (anyDuplicated(tep_data$isolates$id) > 0) {
  stop("isolates$id has duplicate values, so joins would add extra rows.",
       call. = FALSE)
}
if (anyDuplicated(tep_data$institutions$institution_name) > 0) {
  stop("institutions$institution_name has duplicate values, so joins would add extra rows.",
       call. = FALSE)
}

# [Step B] ===========================================================================================================================================
# Each combined table must have exactly as many rows as its original. imap_lgl() runs the comparison once per table and returns one TRUE/FALSE per
# table, named by table.
# ====================================================================================================================================================
rows_match <- imap_lgl(tep_data_combined, function(data_table, tbl_name) {
  nrow(data_table) == nrow(tep_data[[tbl_name]])
})
if (!all(rows_match)) {
  stop("Row count changed after joining for: ",
       paste(names(rows_match)[!rows_match], collapse = ", "),
       call. = FALSE)
}
message("Join check passed: every combined table kept its original row count.")

# [8. Save everything as .rds files in the "Data" folder] ============================================================================================
# An .rds file holds ONE R object, so we write one file per table (rather than one file for the whole list of tables):
#   Data/isolates.rds            <- individual table
#   Data/isolates_combined.rds   <- same table + sample/institution info
# The "_combined" ending is how the app can tell the two groups apart for its "Individual" and "Combined" selector sections.
#
# Unlike a .csv, an .rds keeps every column's type and missing values (NA) exactly as they are in R, so nothing is re-guessed when the table is read
# back in.
#
# TO LOAD THEM BACK (e.g. in the app), read a table by name:
#   readRDS("Data/isolates.rds")
#   readRDS("Data/isolates_combined.rds")
#
# NOTE: Data/data_dictionary.csv (the variable descriptions shown in the app) is a hand-written file, NOT made by this script. It stays a .csv so it
# can be edited in Excel.
# ====================================================================================================================================================
dir.create("Data", showWarnings = FALSE)

# [write_table_list()] ===============================================================================================================================
# Writes every table in a named list to Data/<name><suffix>.rds. iwalk() hands us each table (data_table) and its name (tbl_name) in turn.
# ====================================================================================================================================================
write_table_list <- function(tbl_list, suffix = "") {
  iwalk(tbl_list, function(data_table, tbl_name) {
    saveRDS(data_table, file.path("Data", paste0(tbl_name, suffix, ".rds")))
  })
}

write_table_list(tep_data)                         # individual tables
write_table_list(tep_data_combined, "_combined")   # combined tables

message("\nDone. Saved ", length(tep_data), " individual and ",
        length(tep_data_combined), " combined .rds files to the Data folder.")
