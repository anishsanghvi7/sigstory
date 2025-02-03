
<!-- README.md is generated from README.Rmd. Please edit that file -->

# sigstory

**sigstory** creates HTML Reports which visualise mutational signature
profiles in samples.

The genomes of cancer samples bear characteristic scars which accrue over time, reflecting tumour history, the
mechanisms that drove its formation and each encounter with genotoxic therapies. Over the last decade the study of
these mutational signatures has advanced rapidly, progressing far beyond the identification of mutagenic processes
based exclusively on the patterns of single base substitutions (SBS). We now leverage doublets, indels, copy number
changes and structural rearrangements to gain deeper insights into cancer mutagenesis than ever possible using SBS
alone. 

These advancements bring considerable challenges. A modern mutational signature analysis describes at least
5 different classes of mutational signatures per sample which need to be integrated and concisely presented for
efficient review. There is a notable lack of tools capable of performing this integration effectively. Additionally, as
mutational signature analyses become a standard component of translational research programs, the high false
positive rates of conventional approaches used in pure-research applications become increasingly problematic. 

To address these challenges, we present SigStory, a tool for streamlining the interpretation of modern mutational
signature analysis. SigStory summarises comprehensive signature analyses in convenient HTML reports that include
interactive visualisations of a sample‚ the sample's mutation catalogue, signature contributions and similarity to other
reference cancer samples. These reports concisely describe the contributions of all 5 common signature classes and
provide clear warnings when signature models fall below quality control thresholds. SigStory bridges the gap between
complex mutational data and clear, concise insights, fostering quicker and more accurate discoveries.

View project poster <a href='https://www.canva.com/design/DAGTNplV6Gk/RukOJ56pAp4sHLs8JTubEA/edit?utm_content=DAGTNplV6Gk&utm_campaign=designshare&utm_medium=link2&utm_source=sharebutton' target='_blank'>here</a>


## Installation

You can install the development version of **sigstory** like so:

``` r
if (!require("devtools", quietly = TRUE)) {
    install.packages("devtools")
}
devtools::install_github("anishsanghvi7/sigstory")
```

## Quick Start

### Storing Variables

``` r
library(sigstory)

###############################
## Assign variables to files ##
###############################
# Note: change outdir to any value starting from the base directory of your system
# (e.g. /Users/...) otherwise it will create a 'results' folder in the R-Package
# Library folder.
outdir <- 'results'
sample_information <- system.file("sample_metadata.csv", package = "sigstory")
dimensionality_reduction_overall <- system.file("tsne_metadata_overall.csv", package = "sigstory")

# SBS96
exposure <- system.file("SBS96_fit.TCGA-CA-6717-01.hg19.expo.csv", package = "sigstory")
bootstraps <- system.file("SBS96_fit.TCGA-CA-6717-01.hg19.bootstrap_summary.csv", package = "sigstory")
bootstraps_experimental <- system.file("SBS96_fit.TCGA-CA-6717-01.hg19.expo_bootstraps.csv", package = "sigstory")
tally <- system.file("SBS96_catalogue.TCGA-CA-6717-01.hg19.tally.csv", package = "sigstory")
similarity <- system.file("SBS96_comparison.TCGA-CA-6717-01.hg19.similarity.csv", package = "sigstory")
dataset <- 'COSMIC_v3.4_SBS_GRCh38'
dimensionality_reduction <- system.file("tsne_metadata_SBS96.csv", package = "sigstory")
parquet_path <- system.file("class=SBS96", package = "sigstory")

# DBS78
exposure2 <- system.file("DBS78_fit.TCGA-CA-6717-01.hg19.expo.csv", package = "sigstory")
bootstraps2 <- system.file("DBS78_fit.TCGA-CA-6717-01.hg19.bootstrap_summary.csv", package = "sigstory")
bootstraps_experimental2 <- system.file("DBS78_fit.TCGA-CA-6717-01.hg19.expo_bootstraps.csv", package = "sigstory")
tally2 <- system.file("DBS78_catalogue.TCGA-CA-6717-01.hg19.tally.csv", package = "sigstory")
similarity2 <- system.file("DBS78_comparison.TCGA-CA-6717-01.hg19.similarity.csv", package = "sigstory")
dataset2 <- 'COSMIC_v3.4_DBS_GRCh38'
dimensionality_reduction2 <- system.file("tsne_metadata_DBS78.csv", package = "sigstory")
parquet_path2 <- system.file("class=DBS78", package = "sigstory")

# ID83
exposure3 <- system.file("ID83_fit.TCGA-CA-6717-01.hg19.expo.csv", package = "sigstory")
bootstraps3 <- system.file("ID83_fit.TCGA-CA-6717-01.hg19.bootstrap_summary.csv", package = "sigstory")
bootstraps_experimental3 <- system.file("ID83_fit.TCGA-CA-6717-01.hg19.expo_bootstraps.csv", package = "sigstory")
tally3 <- system.file("ID83_catalogue.TCGA-CA-6717-01.hg19.tally.csv", package = "sigstory")
similarity3 <- system.file("ID83_comparison.TCGA-CA-6717-01.hg19.similarity.csv", package = "sigstory")
dataset3 <- 'COSMIC_v3.4_ID_GRCh37'
dimensionality_reduction3 <- system.file("tsne_metadata_ID83.csv", package = "sigstory")
parquet_path3 <- system.file("class=ID83", package = "sigstory")
```

### Minimal Report Generation

Using the least amount of parameters in sigstory you can make one report. By running this function with these parameters, it will
be the same as running the ```generate_single_report()``` function with the same parameters.

```r
sigstory(outdir, exposure, bootstraps, bootstraps_experimental, tally, dataset)
```

### Full Report Generation 

To create reports for all mutation types (SBS96, DBS78, ID83) and a summary layer, ensure that the values for:

- exposure, exposure2, exposure3
- bootstraps, bootstraps2, bootstraps3
- bootstraps_experimental, bootstraps_experimental2, bootstraps_experimental3
- tally, tally2, tally3
- dataset, dataset2, dataset3 (available datasets can be found here: https://cancer.sanger.ac.uk/signatures/)

are not ```NULL``` values. If these values are ```NULL``` then only a single report will be generated. If you do not have all the available files for other parameters (like similarity, dimensiionality_reduction and 
parquet_path) simply insert a ```NULL``` value in place of them.

```r
sigstory(outdir,
    exposure, bootstraps, bootstraps_experimental, tally, dataset, similarity, dimensionality_reduction, parquet_path,
    exposure2, bootstraps2, bootstraps_experimental2, tally2, dataset2, similarity2, dimensionality_reduction2, parquet_path2,
    exposure3, bootstraps3, bootstraps_experimental3, tally3, dataset3, similarity3, dimensionality_reduction3, parquet_path3,
    sample_information, dimensionality_reduction_overall)
```
