sigstory <-
function(catalogue, bootstraps, bootstraps_experimental, similarity, tally, dataset, parquet_path = NULL,
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
