
source("enrich_aleph_functions.R")
source("enrich_aleph_options.R")

# if input_arg == "stdin", takes data from there
aleph_data <- scan(input_arg, what = "character", sep = "\n")
aleph_data_new <- update_aleph_data_field_260(aleph_data)
aleph_data_new_ul <- unlist(aleph_data_new)

# if output_arg == "stdout", pipes data there
if (output_arg != "stdout") {
  out_file <- file(output_arg)
  writeLines(aleph_data_new_ul, out_file)
  close(out_file)
} else {
  cat(aleph_data_new_ul, sep = "\n")
}

# stdin/out usage:
# cat input/hawking.alephseq | Rscript enrich_aleph.R > hawking_enriched.alephseq

# input file / output file usage:
# edit enrich_aleph_options.R lines 3&4 with file locations.
