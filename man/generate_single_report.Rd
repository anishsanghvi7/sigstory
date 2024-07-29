\name{generate_single_report}
\alias{generate_single_report}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
A Capitalized Title (ideally limited to 65 characters)
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
generate_single_report(catalogue, bootstraps, bootstraps_experimental, similarity, tally, dataset, parquet_path = NULL, sample_information = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{catalogue}{
%%     ~~Describe \code{catalogue} here~~
}
  \item{bootstraps}{
%%     ~~Describe \code{bootstraps} here~~
}
  \item{bootstraps_experimental}{
%%     ~~Describe \code{bootstraps_experimental} here~~
}
  \item{similarity}{
%%     ~~Describe \code{similarity} here~~
}
  \item{tally}{
%%     ~~Describe \code{tally} here~~
}
  \item{dataset}{
%%     ~~Describe \code{dataset} here~~
}
  \item{parquet_path}{
%%     ~~Describe \code{parquet_path} here~~
}
  \item{sample_information}{
%%     ~~Describe \code{sample_information} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or standard data sets, see data().

## The function is currently defined as
function (catalogue, bootstraps, bootstraps_experimental, similarity, 
    tally, dataset, parquet_path = NULL, sample_information = NULL) 
{
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
    }
    else if (grepl(pattern_type[2], expo_file)) {
        sig_type <- "DBS78"
    }
    else if (grepl(pattern_type[3], expo_file)) {
        sig_type <- "ID83"
    }
    else {
        stop("Input files do not have a mutation type in the filename")
    }
    try(fs::dir_create(paste0("/results/", sample_of_interest)))
    rmarkdown::render(input = "/vignettes/SignatureAnalysis_Full.Rmd", 
        output_format = "html_document", output_file = paste0("/results/", 
            sample_of_interest, "/MutationalSignatureAnalysis_", 
            sample_of_interest, "_", sig_type, ".html"), params = list(sample_of_interest = sample_of_interest, 
            sig_type = sig_type, sig_description = dataset, catalogue = expo_file, 
            tally = tally_file, bootstraps = bootstrap_file, 
            experimental_bootstraps = bootstraps_experimental_file, 
            sample_similarity = similarity_file, parquet_file_path = parquet_folder, 
            sample_information = sample_info_file))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
