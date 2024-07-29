generate_single_report <-
function(catalogue, bootstraps, bootstraps_experimental, similarity, tally, dataset, parquet_path = NULL, sample_information = NULL) {
  expo_file <- catalogue
  bootstrap_file <- bootstraps
  bootstraps_experimental_file <- bootstraps_experimental
  tally_file <- tally
  similarity_file <- similarity
  parquet_folder <- parquet_path
  sample_info_file <- sample_information

  split_catalogue <- stringr::str_split(catalogue, "\\.")[[1]]
  sample_of_interest_cat <- split_catalogue[2]

  split_bootstraps <- stringr::str_split(bootstraps, "\\.")[[1]]
  sample_of_interest_boot <- split_catalogue[2]

  split_tally <- stringr::str_split(tally, "\\.")[[1]]
  sample_of_interest_tally <- split_catalogue[2]

  if (sample_of_interest_cat != sample_of_interest_boot ||
      sample_of_interest_cat != sample_of_interest_tally ||
      sample_of_interest_boot != sample_of_interest_tally) {
    stop("Input files do not come from the same sample")
  }

  sample_of_interest <- sample_of_interest_cat
  pattern_type <- c("SBS96", "DBS78", "ID83")
  if (grepl(pattern_type[1], expo_file)) {
    sig_type <- "SBS96"
  } else if (grepl(pattern_type[2], expo_file)) {
    sig_type <- "DBS78"
  } else if (grepl(pattern_type[3], expo_file)) {
    sig_type <- "ID83"
  } else {
    stop("Input files do not have a mutation type in the filename")
  }

  try(fs::dir_create(paste0('/results/', sample_of_interest)))
  rmarkdown::render(
    input = '/vignettes/SignatureAnalysis_Full.Rmd',
    output_format = 'html_document',
    output_file = paste0('/results/', sample_of_interest, '/MutationalSignatureAnalysis_', sample_of_interest, '_', sig_type, '.html'),
    params = list(
      sample_of_interest = sample_of_interest,
      sig_type = sig_type,
      sig_description = dataset,
      catalogue = expo_file,
      tally = tally_file,
      bootstraps = bootstrap_file,
      experimental_bootstraps = bootstraps_experimental_file,
      sample_similarity = similarity_file,
      parquet_file_path = parquet_folder,
      sample_information = sample_info_file
    )
  )
}
