#' Generate Single Report
#'
#' This function auto-generates the signature report.
#'
#' @param catalogue .expo file path which contains the optimal contributions of signatures in the sample
#' @param bootstraps .bootstrap_summary file path which contains the optimal bootstrap statistics for each signature
#' @param bootstraps_experimenal .expo_bootstraps file path which contain the experimental bootstrap statistics for each signature
#' @param tally .tally file path which contain the decomposition of mutations in the sample
#' @param dataset the COSMIC signature dataset being used
#' @param parquet_path a path to the folder of a parquet file which describes the signature models fitted to each sample in the database (optional parameter)
#' @param sample_information a .metadata.tsv. file which contains sample information (optional parameter)
#' @returns full signature html report
#' @export
generate_single_report <- function(catalogue, bootstraps, bootstraps_experimental, similarity, tally, dataset, parquet_path = NULL, sample_information = NULL) {
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

#' Generate Summary Layer
#'
#' This function auto-generates a summary layer. Note: Please input
#' the files in this order: (SBS96.expo file, SBS96.bootstrap file, SBS96.tally file, SBS96.dataset,
#' DBS78.expo file, DBS78.bootstrap file, DBS78.tally file, DBS78.dataset, ID83.expo file,
#' ID83.bootstrap file, ID83.tally file, ID83.dataset).
#'
#' @param catalogue SBS96 .expo file path which contains the optimal contributions of signatures in the sample
#' @param bootstraps SBS96 .bootstraps file path which contains the optimal bootstrap statistics for each signature
#' @param tally SBS96 .tally file path which contain the decompositions of the mutations
#' @param dataset the COSMIC signature dataset being used for SBS96
#' @param catalogue2 DBS78 .expo file path which conntains the optimal contributions of signatures in the sample
#' @param bootstraps2 DBS78 .bootstraps file path which contains the optimal bootstrap statistics for each signature
#' @param tally2 DBS78 .tally file path which contain the decompositions of the mutations
#' @param dataset2 the COSMIC signature dataset being used for DBS78
#' @param catalogue3 ID83 .expo file path which conntains the optimal contributions of signatures in the sample
#' @param bootstraps3 ID83 .bootstraps file path which contains the optimal bootstrap statistics for each signature
#' @param tally3 ID83 .tally file path which contain the decompositions of the mutations
#' @param dataset3 the COSMIC signature dataset being used for ID83
#' @returns full signature html reports
#' @export
generate_summary_layer <- function(catalogue, bootstraps, tally, dataset,
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

#' Sigstory
#'
#' This function auto-generates signature reports and a summary layer. Note: Please input
#' the files in this order: (SBS96.expo file, SBS96.bootstrap file, SBS96.tally file, SBS96.dataset, SBS96 parquet path,
#' DBS78.expo file, DBS78.bootstrap file, DBS78.tally file, DBS78.dataset, DBS78 parquet path, ID83.expo file,
#' ID83.bootstrap file, ID83.tally file, ID83.dataset, ID83 parquet path).
#'
#' @param catalogue SBS96 .expo file path which contains the optimal contributions of signatures in the sample
#' @param bootstraps SBS96 .bootstraps file path which contains the optimal bootstrap statistics for each signature
#' @param tally SBS96 .tally file path which contain the decompositions of the mutations
#' @param dataset the COSMIC signature dataset being used for SBS96
#' @param paruqet_path the path to a paruqet database for SBS96
#' @param catalogue2 DBS78 .expo file path which conntains the optimal contributions of signatures in the sample
#' @param bootstraps2 DBS78 .bootstraps file path which contains the optimal bootstrap statistics for each signature
#' @param tally2 DBS78 .tally file path which contain the decompositions of the mutations
#' @param dataset2 the COSMIC signature dataset being used for DBS78
#' @param paruqet_path2 the path to a paruqet database for DBS78
#' @param catalogue3 ID83 .expo file path which conntains the optimal contributions of signatures in the sample
#' @param bootstraps3 ID83 .bootstraps file path which contains the optimal bootstrap statistics for each signature
#' @param tally3 ID83 .tally file path which contain the decompositions of the mutations
#' @param dataset3 the COSMIC signature dataset being used for ID83
#' @param paruqet_path3 the path to a paruqet database for ID83
#' @param sample_information a .metadata.tsv. file which contains sample information
#' @returns full signature html reports
#' @export
sigstory <- function(catalogue, bootstraps, bootstraps_experimental, similarity, tally, dataset, parquet_path = NULL,
                     catalogue2 = NULL, bootstraps2 = NULL, bootstraps_experimental2 = NULL, similarity2 = NULL, tally2 = NULL, dataset2 = NULL, parquet_path2 = NULL,
                     catalogue3 = NULL, bootstraps3 = NULL, bootstraps_experimental3 = NULL, similarity3 = NULL, tally3 = NULL, dataset3 = NULL, parquet_path3 = NULL,
                     sample_information = NULL) {

  ######################
  ### Error Checking ###
  ######################
  if (!is.null(catalogue2) && (catalogue == catalogue2 ||
                               (!is.null(catalogue3) && catalogue2 == catalogue3) ||
                               (!is.null(catalogue3) && catalogue == catalogue3))) {
    stop("Input catalogue files are the same")
  }

  if (!is.null(bootstraps2) && (bootstraps == bootstraps2 ||
                                (!is.null(bootstraps3) && bootstraps2 == bootstraps3) ||
                                (!is.null(bootstraps3) && bootstraps == bootstraps3))) {
    stop("Input bootstraps files are the same")
  }

  if (!is.null(bootstraps_experimental2) && (bootstraps_experimental == bootstraps_experimental2 ||
                           (!is.null(bootstraps_experimental3) && bootstraps_experimental == bootstraps_experimental3) ||
                           (!is.null(bootstraps_experimental3) && bootstraps_experimental == bootstraps_experimental3))) {
    stop("Input experimental bootstrap files are the same")
  }

  if (!is.null(similarity2) && (similarity == similarity2 ||
                           (!is.null(similarity3) && similarity2 == similarity3) ||
                           (!is.null(similarity3) && similarity == similarity3))) {
    stop("Input tally files are the same")
  }

  if (!is.null(dataset2) && (dataset == dataset2 ||
                                (!is.null(dataset3) && dataset2 == dataset3) ||
                                (!is.null(dataset3) && dataset == dataset3))) {
    stop("Input COSMIC datasets files are the same")
  }

  #######################
  ### Report Creation ###
  #######################
  if (is.null(catalogue2) || is.null(bootstraps2) || is.null(tally2) || is.null(dataset2) ||
      is.null(catalogue3) || is.null(bootstraps3) || is.null(tally3) || is.null(dataset3)) {
    # Generate Single Report
    expo_file <- catalogue
    bootstrap_file <- bootstraps
    bootstraps_experimental_file <- bootstraps_experimental
    tally_file <- tally
    sample_file <- sample_information

    generate_single_report(expo_file, bootstrap_file, bootstraps_experimental_file, similarity, tally_file, dataset, parquet_path, sample_file)

  } else if (!is.null(catalogue2) && !is.null(bootstraps2) && !is.null(tally2) && !is.null(dataset2) &&
              !is.null(catalogue3) && !is.null(bootstraps3) && !is.null(tally3) && !is.null(dataset3)) {
    sample_file <- sample_information

    # Create The Individual Reports
    expo_file <- catalogue
    bootstrap_file <- bootstraps
    bootstraps_experimental_file <- bootstraps_experimental
    tally_file <- tally
    generate_single_report(expo_file, bootstrap_file, bootstraps_experimental_file, similarity, tally_file, dataset, parquet_path, sample_file)

    expo_file2 <- catalogue2
    bootstrap_file2 <- bootstraps2
    bootstraps_experimental_file2 <- bootstraps_experimental2
    tally_file2 <- tally2
    generate_single_report(expo_file2, bootstrap_file2, bootstraps_experimental_file2, similarity2, tally_file2, dataset2, parquet_path2, sample_file)

    expo_file3 <- catalogue3
    bootstrap_file3 <- bootstraps3
    bootstraps_experimental_file3 <- bootstraps_experimental3
    tally_file3 <- tally3
    generate_single_report(expo_file3, bootstrap_file3, bootstraps_experimental_file3, similarity3, tally_file3, dataset3, parquet_path3, sample_file)

    # Summary Layer
    generate_summary_layer(catalogue, bootstraps, tally, dataset,
                           catalogue2, bootstraps2, tally2, dataset2,
                           catalogue3, bootstraps3, tally3, dataset3)

  } else {
    stop("No files were created, ensure no NULL values are being passed in")
  }
}



