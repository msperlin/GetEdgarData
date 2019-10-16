#' Scrapes available datasets from the SEC website
#'
#' This function find the available zip files from the SEC website <https://www.sec.gov/files/dera/data/financial-statement-data-sets/>,
#' returning a dataframe with the information about files.
#'
#' @return A dataframe with files information
#' @export
#'
#' @examples
#'
#' \dontrun{
#' df_files <- get_available_periods()
#' }
get_available_files <- function() {

  my_url <- 'https://www.sec.gov/dera/data/financial-statement-data-sets.html'

  available_files <- xml2::read_html(my_url) %>%
    rvest::html_nodes(".associated-data-distribution table tbody tr td a") %>%
    rvest::html_attr('href')

  base_url <- 'https://www.sec.gov/files/dera/data/financial-statement-data-sets/'

  # fix check msgs (no visible binding)
  filename <- NULL

  df_files <- dplyr::tibble(filename = basename(available_files),
                            url = paste0(base_url, filename),
                            year = as.numeric(stringr::str_sub(filename, 1, 4)),
                            quarter = stringr::str_to_upper(
                              stringr::str_sub(filename, 5, 6)))

  return(df_files)

}

#' Download, reads and organizes a single zip file
#'
#' After downloading the file, this function will read its contents and organize it in a dataframe.
#'
#' @inheritParams get_edgar_fin_data
#' @param year_in The year
#' @param quarter_in The quarter (e.g. 'Q1')
#'
#' @return A dataframe with the clean data from zip file
#' @export
#'
#' @examples
#'
#' \dontrun{
#'
#' df <- dl_and_read_single_zip_file(type_data = 'yearly',
#'                                   year_in = 2018,
#'                                   quarter_in = 'Q1',
#'                                   cache_folder = 'GetEdgarData-cache',
#'                                   do_cache = TRUE)
#'}
dl_and_read_single_zip_file <- function(type_data,
                                        year_in,
                                        quarter_in,
                                        cache_folder,
                                        do_cache) {

  message('\t', year_in, '/', quarter_in)

  if (do_cache) {
    f_out <- file.path(cache_folder, 'cache files',
                       paste0('edgar_cache_',
                              year_in, '-', quarter_in, '-',
                              type_data, '.rds' ))

    if (!dir.exists(dirname(f_out))) {
      dir.create(dirname(f_out))
    }

    if (file.exists(f_out)) {
      l_out <- readr::read_rds(f_out)

      message('\t- Found cache file')

      return(l_out)

    }
  }

  download_single_zip_file(year_in = year_in,
                           quarter_in = quarter_in,
                           cache_folder = cache_folder)

  f_in <- file.path(cache_folder, 'zip files',
                    paste0('edgar-files-',
                           year_in, '-', quarter_in, '.zip') )

  my_dir <- file.path(tempdir(), basename(f_in) )
  utils::unzip(f_in,
               exdir =  my_dir)

  message('\t- Parsing .txt files')

  df_num <- get_single_raw_table(path_to_files = my_dir,
                                 type_table = 'num',
                                 cache_folder = cache_folder)

  df_sub <- get_single_raw_table(path_to_files = my_dir,
                                 type_table = 'sub',
                                 cache_folder = cache_folder)

  #df.pre <- get_single_raw_table(path_to_files = my_dir,
  #                               type_table = 'pre',
  #                               cache_folder = cache_folder)

  df_final_tab <- parse_final_table(df_num, df_sub, #df.pre,
                                    type_data = type_data,
                                    year_in = year_in)


  l_out <- list(tab = df_final_tab,
                sub = df_sub)

  if (do_cache) {
    message('\t- Saving cache file')
    readr::write_rds(l_out, path =  f_out )
  }

  return(l_out)

}


#' Downloads a single zip file from SEC
#'
#' Downloads a single file from <https://www.sec.gov/files/dera/data/financial-statement-data-sets/>.
#'
#' @inheritParams dl_and_read_single_zip_file
#'
#' @return TRUE, if sucessful
#' @export
#'
#' @examples
#' \dontrun{
#' flag <- download_single_zip_file(year_in = 2018,
#'                                  quarter_in = 'Q1',
#'                                  cache_folder = 'GetEdgarData-cache')
#' }
download_single_zip_file <- function(year_in,
                                     quarter_in,
                                     cache_folder = 'GetEdgarData-cache') {

  base_url <- 'https://www.sec.gov/files/dera/data/financial-statement-data-sets/'
  url_dl <- paste0(base_url, year_in,
                   stringr::str_to_lower(quarter_in), '.zip')

  f_out <- file.path(cache_folder, 'zip files',
                     paste0('edgar-files-',
                            year_in, '-', quarter_in, '.zip') )

  if (file.exists(f_out)) {
    message('\t- Zip file already exists at cache folder')
    return(TRUE)
  } else {
    message('\t- Downloading zip file from SEC', appendLF = FALSE)
  }

  if (!dir.exists(dirname(f_out))) dir.create(dirname(f_out), recursive = TRUE)

  utils::download.file(url_dl,
                       destfile = f_out, quiet = TRUE)

  message('\tOK')
  message('\t\t- file available at ', f_out)

  return(TRUE)

}
