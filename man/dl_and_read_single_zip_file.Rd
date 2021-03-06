% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/util.R
\name{dl_and_read_single_zip_file}
\alias{dl_and_read_single_zip_file}
\title{Download, reads and organizes a single zip file}
\usage{
dl_and_read_single_zip_file(type_data, year_in, quarter_in, cache_folder,
  do_cache)
}
\arguments{
\item{type_data}{Type of dataset ('yearly' or 'quarterly')}

\item{year_in}{The year}

\item{quarter_in}{The quarter (e.g. 'Q1')}

\item{cache_folder}{Path of cache folder to save all memoise files and downloaded zip files}

\item{do_cache}{Use memoise cache system? TRUE (default) or FALSE}
}
\value{
A dataframe with the clean data from zip file
}
\description{
After downloading the file, this function will read its contents and organize it in a dataframe.
}
\examples{

\dontrun{

df <- dl_and_read_single_zip_file(type_data = 'yearly',
                                  year_in = 2018,
                                  quarter_in = 'Q1',
                                  cache_folder = 'GetEdgarData-cache',
                                  do_cache = TRUE)
}
}
