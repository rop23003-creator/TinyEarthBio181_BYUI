# [Data_Pull.R] ======================================================================================================================================
# PURPOSE
#   Downloads the latest raw Excel (.xlsx) export of every table on the Tiny Earth Public Database website (https://publicdata.tinyearth.wisc.edu/)
#   into the "Data_Raw" folder. Run this FIRST, then run Data_Clean.R to turn those raw files into the cleaned .rds files the app uses.
#
#   Data_Raw always holds the files exactly as the website gives them -- nothing here edits the data.
#
# HOW THE WEBSITE'S DOWNLOAD WORKS
#   Each table has its own page (e.g. .../isolates/) with a "Download All (Excel)" button. Behind that button are two values written into the page:
#     report_id  which table's spreadsheet to build
#     nonce      a short-lived security code the site requires
#   Clicking the button sends both to the site's download address, which answers with the .xlsx file. This script does the same thing for every table:
#   open the page, read the two values, and ask for the file.
#
# BEFORE YOU RUN THIS
#   1. Open this project through its .Rproj file, so "Data_Raw" means the folder inside this project.
#   2. Install these packages once, if you haven't already:
#        install.packages(c("tidyverse", "httr2"))
#      (readxl comes with the tidyverse install.)
# ====================================================================================================================================================

library(tidyverse)  # stringr (find report_id/nonce), purrr (repeat downloads), %>% pipe, and more
library(readxl)     # opens each downloaded file to check it's a real spreadsheet
library(httr2)      # sends requests to the website and saves the replies

base_url <- "https://publicdata.tinyearth.wisc.edu"
raw_dir  <- "Data_Raw"

# [1. The tables to download] ========================================================================================================================
# Each name is the table's web address on the site (e.g. .../isolates/) AND the name its file is saved under (e.g. Data_Raw/isolates.xlsx) -- the same
# names Data_Clean.R reads. If the site adds a table, add its page name here.
# ====================================================================================================================================================
tables <- c(
  "institutions",
  "samples",
  "dilutions",
  "library-collections",
  "aa-screen-libraries",
  "isolates",
  "streaks",
  "aa-screen-isolates",
  "gram-stains",
  "other-tests",
  "rrna-analyses",
  "antibio-suscepts",
  "eukaryotics",
  "extracts",
  "aa-screen-extracts"
)

dir.create(raw_dir, showWarnings = FALSE)  # make Data_Raw if it isn't there yet

# [2. A function that downloads ONE table] ===========================================================================================================
# The new file is first saved to a temporary spot and checked. Only if it is a real, readable Excel file does it replace the old copy in Data_Raw --
# so a failed download never leaves you with a broken or half-finished file.
# ====================================================================================================================================================
pull_table <- function(slug) {
  message("Downloading ", slug, " ...")

  jar <- tempfile(fileext = ".txt")  # "cookie jar": both requests then look like one visitor

  # [Step A] =========================================================================================================================================
  # Open the table's page and read its HTML as text.
  # ==================================================================================================================================================
  page <- request(paste0(base_url, "/", slug, "/")) %>%
    req_user_agent("BYU-Idaho Bio 181 Tiny Earth data pull (R httr2)") %>%
    req_cookie_preserve(jar) %>%
    req_perform() %>%
    resp_body_string()

  # [Step B] =========================================================================================================================================
  # Find the two values the download button uses. str_match() returns the full match in column 1 and the part inside ( ) in column 2, so [, 2] is just
  # the value we want.
  # ==================================================================================================================================================
  report_id <- str_match(page, 'data-report_id="(\\d+)"')[, 2]
  nonce     <- str_match(page, paste0('reportbuilder_', report_id,
                                      '\\s*=\\s*\\{[^}]*"nonce":"([^"]+)"'))[, 2]
  if (is.na(report_id) || is.na(nonce)) {
    stop("could not find the download button on the ", slug,
         " page -- the website may have changed.", call. = FALSE)
  }

  # [Step C] =========================================================================================================================================
  # Ask for the spreadsheet, the same way the button does, and save the reply to a temporary file.
  # ==================================================================================================================================================
  tmp <- tempfile(fileext = ".xlsx")
  request(paste0(base_url, "/wp-admin/admin-ajax.php")) %>%
    req_user_agent("BYU-Idaho Bio 181 Tiny Earth data pull (R httr2)") %>%
    req_cookie_preserve(jar) %>%
    req_body_form(
      action               = "report_builder_download_report",
      nonce                = nonce,
      `wdtReportConfig[id]` = report_id,
      wdtDownloadToken     = format(as.numeric(Sys.time()) * 1000, scientific = FALSE),
      downloadType         = "download"
    ) %>%
    req_timeout(300) %>%  # big tables can take a while to build, so allow up to 5 minutes
    req_perform(path = tmp)

  # [Step D] =========================================================================================================================================
  # Check the file before trusting it. Every .xlsx file begins with the two letters "PK" (it's a zip file inside), so anything else -- like an error
  # page -- is rejected. Then make sure readxl can actually open it. col_types = "text" reads every cell as plain text -- we only need a row count
  # here, and it skips readxl's type-guessing (and its pile of warnings). Data_Clean.R works out the real column types later.
  # ==================================================================================================================================================
  first_bytes <- readBin(tmp, "raw", n = 2)
  if (!identical(first_bytes, charToRaw("PK"))) {
    stop("the website did not send an Excel file for ", slug, call. = FALSE)
  }
  n_rows <- nrow(read_excel(tmp, col_types = "text"))

  # [Step E] =========================================================================================================================================
  # The file is good -- put it in Data_Raw, replacing the old one.
  # ==================================================================================================================================================
  file.copy(tmp, file.path(raw_dir, paste0(slug, ".xlsx")), overwrite = TRUE)
  message("  saved ", slug, ".xlsx (", n_rows, " rows)")
  n_rows
}

# [3. Download every table] ==========================================================================================================================
# tryCatch() means "if one table fails, print why and keep going" instead of stopping the whole script. A failed table keeps its old file in Data_Raw.
# ====================================================================================================================================================
rows_pulled <- map_dbl(tables, function(slug) {
  n <- tryCatch(pull_table(slug), error = function(e) {
    message("  FAILED: ", conditionMessage(e))
    NA
  })
  Sys.sleep(1)  # wait one second between tables so we don't flood the website
  n
})
names(rows_pulled) <- tables

# [4. Summary] =======================================================================================================================================
# Report how many tables downloaded, and name any that failed.
# ====================================================================================================================================================
failed <- names(rows_pulled)[is.na(rows_pulled)]

message("\nDownloaded ", sum(!is.na(rows_pulled)), " of ", length(tables),
        " tables into ", raw_dir, ".")
if (length(failed) > 0) {
  warning("These tables could not be downloaded (their old files, if any, ",
          "were kept): ", paste(failed, collapse = ", "),
          "\nTry again later, or download them by hand from the website.",
          call. = FALSE)
} else {
  message("Next step: run Data_Clean.R.")
}
