generate_summary_layer <-
function(catalogue, bootstraps, tally, dataset,
                     catalogue2, bootstraps2, tally2, dataset2,
                     catalogue3, bootstraps3, tally3, dataset3) {

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

  try(fs::dir_create(paste0('/results/', sample_of_interest)))
  rmarkdown::render(
    input = '/vignettes/SignatureAnalysis_Summary.Rmd',
    output_format = 'html_document',
    output_file = paste0('/results/', sample_of_interest, '/MutationalSignatureAnalysis_', sample_of_interest, '_Summary.html'),
    params = list(
      sample_of_interest = sample_of_interest,
      cos_threshold = 0.9,
      sig_description_sbs96 = dataset,
      sig_description_dbs78 = dataset2,
      sig_description_id83 = dataset3,
      catalogue_sbs96 = catalogue,
      tally_sbs96 = tally,
      bootstraps_sbs96 = bootstraps,
      catalogue_dbs78 = catalogue2,
      tally_dbs78 = tally2,
      bootstraps_dbs78 = bootstraps2,
      catalogue_id83 = catalogue3,
      tally_id83 = tally3,
      bootstraps_id83 = bootstraps3
    )
  )
}
