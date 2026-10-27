# [Data_Pull.R] ======================================================================================================================================
# PURPOSE:
#   Downloads the latest raw Excel (.xlsx) export of every table on the Tiny Earth Public Database website (https://publicdata.tinyearth.wisc.edu/)
#   into the "Data_Raw" folder. Run this FIRST, then run Data_Clean.R to turn those raw files into the cleaned .rds files the app uses.
#
#   Data_Raw always holds the files exactly as the website gives them -- nothing here edits the data.
#
# HOW THE WEBSITE'S DOWNLOAD WORKS:
#   Each table has its own page (e.g. .../isolates/) with a "Download All (Excel)" button. Behind that button are two values written into the page:
#     report_id  which table's spreadsheet to build
#     nonce      a short-lived security code the site requires
#   Clicking the button sends both to the site's download address, which answers with the .xlsx file. This script does the same thing for every table:
#   open the page, read the two values, and ask for the file.
#
# BEFORE YOU RUN THIS:
#   1. Open this project through its .Rproj file, so "Data_Raw" means the folder inside this project.
#   2. Install these packages once, if you haven't already: install.packages(c("tidyverse", "httr2")) [readxl comes with the tidyverse install.]
# ====================================================================================================================================================

library(tidyverse)  # stringr (find report_id/nonce); purrr (repeat downloads)
library(readxl)     # Opens each downloaded file to check it's a real spreadsheet.
library(httr2)      # Sends requests to the website and saves the replies.

# [1. Setup: website address, Data_Raw folder, and table list] =======================================================================================
# Each name is the table's web address on the site (e.g. .../isolates/) AND the name its file is saved under (e.g. Data_Raw/isolates.xlsx) -- the same
# names Data_Clean.R reads.
# ====================================================================================================================================================
base_url     <- "https://publicdata.tinyearth.wisc.edu"
data_raw_dir <- "Data_Raw"
dir.create(data_raw_dir, showWarnings = FALSE) # Make the Data_Raw folder if it isn't there already.
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

# [2. How each table is downloaded] ==================================================================================================================
# A new file is first saved to a temporary spot and checked. Only if the file is a readable XLSX file does it replace the old copy in Data_Raw.
# This makes it so a failed download never leaves you with an unusable file.
# ====================================================================================================================================================
pull_table <- function(table_name) {
  message("Downloading ", table_name, " ...")

  jar <- tempfile(fileext = ".txt")  # Cookie "jar": it's a temp file holding the site's cookies, so both requests look like one visitor.

  # [Step 1/5] =======================================================================================================================================
  # Open the table's page and read its HTML as text. req_user_agent() labels our requests (here and in Step 3) so the site's admins can see in their
  # logs who is downloading and why. It does not change the data we get.
  # ==================================================================================================================================================
  page <- request(paste0(base_url, "/", table_name, "/")) %>%
    req_user_agent("BYU-Idaho Bio 181 Tiny Earth data pull (R httr2)") %>%
    req_cookie_preserve(jar) %>%
    req_perform() %>%
    resp_body_string()

  # [Step 2/5] =======================================================================================================================================
  # Find the two values the download button uses. str_match() returns the full match in column 1 and the part inside ( ) in column 2, so [, 2] is just
  # the value we want.
  # ==================================================================================================================================================
  report_id <- str_match(page, 'data-report_id="(\\d+)"')[, 2]
  nonce <- str_match(page, paste0('reportbuilder_', report_id, '\\s*=\\s*\\{[^}]*"nonce":"([^"]+)"'))[, 2]
  if (is.na(report_id) || is.na(nonce)) {
    stop("could not find the download button on the ", table_name, " page -- the website may have changed.", call. = FALSE)
  }

  # [Step 3/5] =======================================================================================================================================
  # Ask for the spreadsheet, the same way the button does, and save the reply to a temporary file.
  # ==================================================================================================================================================
  tmp <- tempfile(fileext = ".xlsx")
  request(paste0(base_url, "/wp-admin/admin-ajax.php")) %>%
    req_user_agent("BYU-Idaho Bio 181 Tiny Earth data pull (R httr2)") %>%
    req_cookie_preserve(jar) %>%
    req_body_form(action = "report_builder_download_report",
                  nonce = nonce,
                  `wdtReportConfig[id]` = report_id,
                  wdtDownloadToken = format(as.numeric(Sys.time()) * 1000, scientific = FALSE),
                  downloadType = "download") %>%
    req_timeout(300) %>%
    req_perform(path = tmp)

  # [Step 4/5] =======================================================================================================================================
  # Check the file before trusting it. Every .xlsx file begins with the two letters "PK" (it's a zip file inside), so anything else is rejected.
  # Then make sure readxl can actually open it. col_types = "text" reads every cell as plain text and it skips readxl's type-guessing.
  # ==================================================================================================================================================
  xlsx_zip_id <- readBin(tmp, "raw", n = 2)
  if (!identical(xlsx_zip_id, charToRaw("PK"))) {
    stop("the website did not send an Excel file for ", table_name, call. = FALSE)
  }
  n_rows <- nrow(read_excel(tmp, col_types = "text"))

  # [Step 5/5] =======================================================================================================================================
  # Put the file in Data_Raw and replace the old one.
  # ==================================================================================================================================================
  file.copy(tmp, file.path(data_raw_dir, paste0(table_name, ".xlsx")), overwrite = TRUE)
  message("  saved ", table_name, ".xlsx (", n_rows, " rows)")
  n_rows
}

# [3. Downloads every table] =========================================================================================================================
# tryCatch() means "if one table fails, print why and keep going" instead of stopping the whole script. A failed table keeps its old file in Data_Raw.
# ====================================================================================================================================================
rows_pulled <- map_dbl(tables, function(table_name) {
  n <- tryCatch(pull_table(table_name), error = function(e) {
    message("  FAILED: ", conditionMessage(e))
    NA
  })
  Sys.sleep(1)  # wait one second between tables so we don't flood the website
  n
})
names(rows_pulled) <- tables

# [4. Download Report] ===============================================================================================================================
# Reports how many tables were downloaded, and name any that failed.
# ====================================================================================================================================================
failed <- names(rows_pulled)[is.na(rows_pulled)]
message("\nDownloaded ", sum(!is.na(rows_pulled)), " of ", length(tables), " tables into ", data_raw_dir, ".")
if (length(failed) > 0) {
  warning("These tables could not be downloaded (their old files, if any, ",
          "were kept): ", paste(failed, collapse = ", "),
          "\nTry again later, or download them by hand from the website.",
          call. = FALSE)
} else {
  message("Next step: run Data_Clean.R.")
}
