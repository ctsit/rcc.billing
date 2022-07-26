library(dplyr)

# create test data for clean_processed_payment_data_from_email

input <- tribble(
  ~study_name,
  ~crc_number,
  ~ids_number,
  ~ocr_number,
  ~invoice_number,
  ~amount_paid,
  ~je_number,
  ~je_posting_date,
  ~ctsi_study_id,
  # record 1
  "Your Cool Study",
  1234,
  "",
  "OCR9001",
  "20220513-01",
  "$100.00",
  "CTSI060822",
  "6/10/2022",
  2929,
  # record 2
  "YANCS",
  NA,
  "IDS-9999",
  "OCR9002",
  "20220515-01",
  "$100.00",
  "CTSI060822",
  "6/10/2022",
  3030
)


output <- tribble(
  ~name_of_service,
  ~crc_number,
  ~ids_number,
  ~ocr_number,
  ~invoice_number,
  ~amount_paid,
  ~je_number,
  ~je_posting_date,
  ~ctsi_study_id,
  # record 1
  "Your Cool Study",
  1234,
  as.character(NA),
  "OCR9001",
  "20220513-01",
  100,
  "CTSI060822",
  lubridate::ymd("2022-06-10"),
  2929,
  # record 2
  "YANCS",
  as.numeric(NA),
  "IDS-9999",
  "OCR9002",
  "20220515-01",
  100,
  "CTSI060822",
  lubridate::ymd("2022-06-10"),
  3030
)

clean_processed_payment_data_from_email_test_data <- list(
  input = input,
  output = output
)

usethis::use_data(clean_processed_payment_data_from_email_test_data, overwrite = TRUE)
