library(devtools)
load_all("lib/bibliographica/")


get_aleph_row_type <- function(aleph_row) {
  marc_field_number <- substr(aleph_row, 11, 13)
  return(marc_field_number)
}


check_aleph_row_type <- function(aleph_row, marc_field_code) {
  row_type <- get_aleph_row_type(aleph_row)
  return(row_type == marc_field_code)
}


get_aleph_row_nos_of_type <- function(aleph_data, marc_field_code) {
  row_nos <- which(check_aleph_row_type(aleph_data, "260"))
  return(row_nos)
}


get_aleph_row_content_split <- function(aleph_row) {
  aleph_row_content <- substr(aleph_row, 19, nchar(aleph_row))
  aleph_row_content_split <- strsplit(aleph_row_content, "\\$\\$")[[1]]
  return(aleph_row_content_split)
}


combine_aleph_row_split <- function(aleph_row_content_split) {
  aleph_row_new <- paste(aleph_row_content_split, collapse = "$$")
  return(aleph_row_new)
}


get_aleph_row_260_content_new <- function(aleph_row) {
  aleph_row_content_split <- get_aleph_row_content_split(aleph_row)
  
  pubyear_subfield_n <- which(substr(aleph_row_content_split, 1, 1) == "c")[1]
  pubyear_subfield_content <- substr(aleph_row_content_split[pubyear_subfield_n],
                                     2,
                                     nchar(aleph_row_content_split[pubyear_subfield_n]))
  pubyear_subfield_content_unified <- polish_years(as.character(pubyear_subfield_content))
  
  if (is.na(pubyear_subfield_content_unified$till)) {
    pubyear_new_content <- as.character(pubyear_subfield_content_unified$from)
  } else {
    pubyear_new_content <- 
      paste(pubyear_subfield_content_unified$from,
            pubyear_subfield_content_unified$till,
            sep = "-")
  }
  
  pubyear_new_content <- paste0("c", pubyear_new_content)
  aleph_row_content_split[pubyear_subfield_n] <-
    pubyear_new_content
  
  aleph_row_content_new <- combine_aleph_row_split(aleph_row_content_split)
  aleph_row_new <- paste0(substr(aleph_row, 1, 18), aleph_row_content_new)
  return(aleph_row_new)
}

# only updates year now
update_aleph_data_field_260 <- function(aleph_data) {
  row_nos <- get_aleph_row_nos_of_type(aleph_data, "260")
  rows_to_update <- aleph_data[row_nos]
  rows_new_content <- lapply(rows_to_update, get_aleph_row_260_content_new)
  aleph_data[row_nos] <- rows_new_content
  return(aleph_data)
}
