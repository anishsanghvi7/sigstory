\name{generate_summary_layer}
\alias{generate_summary_layer}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
A Capitalized Title (ideally limited to 65 characters)
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
generate_summary_layer(catalogue, bootstraps, tally, dataset, catalogue2, bootstraps2, tally2, dataset2, catalogue3, bootstraps3, tally3, dataset3)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{catalogue}{
%%     ~~Describe \code{catalogue} here~~
}
  \item{bootstraps}{
%%     ~~Describe \code{bootstraps} here~~
}
  \item{tally}{
%%     ~~Describe \code{tally} here~~
}
  \item{dataset}{
%%     ~~Describe \code{dataset} here~~
}
  \item{catalogue2}{
%%     ~~Describe \code{catalogue2} here~~
}
  \item{bootstraps2}{
%%     ~~Describe \code{bootstraps2} here~~
}
  \item{tally2}{
%%     ~~Describe \code{tally2} here~~
}
  \item{dataset2}{
%%     ~~Describe \code{dataset2} here~~
}
  \item{catalogue3}{
%%     ~~Describe \code{catalogue3} here~~
}
  \item{bootstraps3}{
%%     ~~Describe \code{bootstraps3} here~~
}
  \item{tally3}{
%%     ~~Describe \code{tally3} here~~
}
  \item{dataset3}{
%%     ~~Describe \code{dataset3} here~~
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
function (catalogue, bootstraps, tally, dataset, catalogue2, 
    bootstraps2, tally2, dataset2, catalogue3, bootstraps3, tally3, 
    dataset3) 
{
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
    try(fs::dir_create(paste0("/results/", sample_of_interest)))
    rmarkdown::render(input = "/vignettes/SignatureAnalysis_Summary.Rmd", 
        output_format = "html_document", output_file = paste0("/results/", 
            sample_of_interest, "/MutationalSignatureAnalysis_", 
            sample_of_interest, "_Summary.html"), params = list(sample_of_interest = sample_of_interest, 
            cos_threshold = 0.9, sig_description_sbs96 = dataset, 
            sig_description_dbs78 = dataset2, sig_description_id83 = dataset3, 
            catalogue_sbs96 = catalogue, tally_sbs96 = tally, 
            bootstraps_sbs96 = bootstraps, catalogue_dbs78 = catalogue2, 
            tally_dbs78 = tally2, bootstraps_dbs78 = bootstraps2, 
            catalogue_id83 = catalogue3, tally_id83 = tally3, 
            bootstraps_id83 = bootstraps3))
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
