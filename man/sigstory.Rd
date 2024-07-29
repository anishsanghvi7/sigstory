\name{sigstory}
\alias{sigstory}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
A Capitalized Title (ideally limited to 65 characters)
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
sigstory(catalogue, bootstraps, bootstraps_experimental, similarity, tally, dataset, parquet_path = NULL, catalogue2 = NULL, bootstraps2 = NULL, bootstraps_experimental2 = NULL, similarity2 = NULL, tally2 = NULL, dataset2 = NULL, parquet_path2 = NULL, catalogue3 = NULL, bootstraps3 = NULL, bootstraps_experimental3 = NULL, similarity3 = NULL, tally3 = NULL, dataset3 = NULL, parquet_path3 = NULL, sample_information = NULL)
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
  \item{catalogue2}{
%%     ~~Describe \code{catalogue2} here~~
}
  \item{bootstraps2}{
%%     ~~Describe \code{bootstraps2} here~~
}
  \item{bootstraps_experimental2}{
%%     ~~Describe \code{bootstraps_experimental2} here~~
}
  \item{similarity2}{
%%     ~~Describe \code{similarity2} here~~
}
  \item{tally2}{
%%     ~~Describe \code{tally2} here~~
}
  \item{dataset2}{
%%     ~~Describe \code{dataset2} here~~
}
  \item{parquet_path2}{
%%     ~~Describe \code{parquet_path2} here~~
}
  \item{catalogue3}{
%%     ~~Describe \code{catalogue3} here~~
}
  \item{bootstraps3}{
%%     ~~Describe \code{bootstraps3} here~~
}
  \item{bootstraps_experimental3}{
%%     ~~Describe \code{bootstraps_experimental3} here~~
}
  \item{similarity3}{
%%     ~~Describe \code{similarity3} here~~
}
  \item{tally3}{
%%     ~~Describe \code{tally3} here~~
}
  \item{dataset3}{
%%     ~~Describe \code{dataset3} here~~
}
  \item{parquet_path3}{
%%     ~~Describe \code{parquet_path3} here~~
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
    tally, dataset, parquet_path = NULL, catalogue2 = NULL, bootstraps2 = NULL, 
    bootstraps_experimental2 = NULL, similarity2 = NULL, tally2 = NULL, 
    dataset2 = NULL, parquet_path2 = NULL, catalogue3 = NULL, 
    bootstraps3 = NULL, bootstraps_experimental3 = NULL, similarity3 = NULL, 
    tally3 = NULL, dataset3 = NULL, parquet_path3 = NULL, sample_information = NULL) 
{
    if (!is.null(catalogue2) && (catalogue == catalogue2 || (!is.null(catalogue3) && 
        catalogue2 == catalogue3) || (!is.null(catalogue3) && 
        catalogue == catalogue3))) {
        stop("Input catalogue files are the same")
    }
    if (!is.null(bootstraps2) && (bootstraps == bootstraps2 || 
        (!is.null(bootstraps3) && bootstraps2 == bootstraps3) || 
        (!is.null(bootstraps3) && bootstraps == bootstraps3))) {
        stop("Input bootstraps files are the same")
    }
    if (!is.null(bootstraps_experimental2) && (bootstraps_experimental == 
        bootstraps_experimental2 || (!is.null(bootstraps_experimental3) && 
        bootstraps_experimental == bootstraps_experimental3) || 
        (!is.null(bootstraps_experimental3) && bootstraps_experimental == 
            bootstraps_experimental3))) {
        stop("Input experimental bootstrap files are the same")
    }
    if (!is.null(similarity2) && (similarity == similarity2 || 
        (!is.null(similarity3) && similarity2 == similarity3) || 
        (!is.null(similarity3) && similarity == similarity3))) {
        stop("Input tally files are the same")
    }
    if (!is.null(dataset2) && (dataset == dataset2 || (!is.null(dataset3) && 
        dataset2 == dataset3) || (!is.null(dataset3) && dataset == 
        dataset3))) {
        stop("Input COSMIC datasets files are the same")
    }
    if (is.null(catalogue2) || is.null(bootstraps2) || is.null(tally2) || 
        is.null(dataset2) || is.null(catalogue3) || is.null(bootstraps3) || 
        is.null(tally3) || is.null(dataset3)) {
        expo_file <- catalogue
        bootstrap_file <- bootstraps
        bootstraps_experimental_file <- bootstraps_experimental
        tally_file <- tally
        sample_file <- sample_information
        generate_single_report(expo_file, bootstrap_file, bootstraps_experimental_file, 
            similarity, tally_file, dataset, parquet_path, sample_file)
    }
    else if (!is.null(catalogue2) && !is.null(bootstraps2) && 
        !is.null(tally2) && !is.null(dataset2) && !is.null(catalogue3) && 
        !is.null(bootstraps3) && !is.null(tally3) && !is.null(dataset3)) {
        sample_file <- sample_information
        expo_file <- catalogue
        bootstrap_file <- bootstraps
        bootstraps_experimental_file <- bootstraps_experimental
        tally_file <- tally
        generate_single_report(expo_file, bootstrap_file, bootstraps_experimental_file, 
            similarity, tally_file, dataset, parquet_path, sample_file)
        expo_file2 <- catalogue2
        bootstrap_file2 <- bootstraps2
        bootstraps_experimental_file2 <- bootstraps_experimental2
        tally_file2 <- tally2
        generate_single_report(expo_file2, bootstrap_file2, bootstraps_experimental_file2, 
            similarity2, tally_file2, dataset2, parquet_path2, 
            sample_file)
        expo_file3 <- catalogue3
        bootstrap_file3 <- bootstraps3
        bootstraps_experimental_file3 <- bootstraps_experimental3
        tally_file3 <- tally3
        generate_single_report(expo_file3, bootstrap_file3, bootstraps_experimental_file3, 
            similarity3, tally_file3, dataset3, parquet_path3, 
            sample_file)
        generate_summary_layer(catalogue, bootstraps, tally, 
            dataset, catalogue2, bootstraps2, tally2, dataset2, 
            catalogue3, bootstraps3, tally3, dataset3)
    }
    else {
        stop("No files were created, ensure no NULL values are being passed in")
    }
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
