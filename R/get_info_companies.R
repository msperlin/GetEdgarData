#' Get information about available companies
#'
#' This functino will read the zip files and get information about available companies.
#'
#' @param years Years to find data
#' @param type_form The type of SEC form ('10-K' or '10-Q')
#' @inheritParams get_edgar_fin_data
#'
#' @return A dataframe with information about companies
#' @export
#'
#' @examples
#'
#' \dontrun{
#' df_info <- get_info_companies()
#' }
get_info_companies <- function(years = 2009:2018,
                               type_data = 'yearly',
                               type_form  = '10-K',
                               do_cache = TRUE,
                               cache_folder = 'GetEdgarData-cache') {

  # fix check msgs (no visible binding)
  year <- quarter <- sic_code <- form <- NULL

  possible_cases <- c('yearly', 'quarterly')
  if (!(type_data %in% possible_cases) ) {
    stop('Argument type_data should be one of ',
         paste0(possible_cases, collapse = ', '))
  }

  possible_cases <- c('10-K', '10-Q')
  if (!(type_form %in% possible_cases) ) {
    stop('Argument type_form should be one of ',
         paste0(possible_cases, collapse = ', '))
  }

  message('Fetching ', type_data, ' data:')

  df_files <- get_available_files()

  if (type_data == 'yearly') my_quarters <- 'Q4'
  if (type_data == 'quarterly') my_quarters <- c('Q1', 'Q2', 'Q3', 'Q4')

  df_files_temp <- df_files %>%
    dplyr::filter(year %in% years,
                  quarter %in% my_quarters)

  if (nrow(df_files_temp) == 0) {
    stop('Cant find selected years and quarters in edgar website.. ')
  }

  l_args <- list(year_in = df_files_temp$year,
                 quarter_in = df_files_temp$quarter,
                 cache_folder = cache_folder,
                 do_cache = do_cache,
                 type_data = rep(list(type_data), nrow(df_files_temp)))

  l_out <- purrr::pmap(.l = l_args,
                       .f = dl_and_read_single_zip_file)

  df <- dplyr::bind_rows(purrr::map(l_out, 2))

  df_sic <- get_sic_codes(cache_folder)

  df_info <- df %>%
    mutate(sic_code = as.numeric(sic_code)) %>%
    left_join(df_sic, by = 'sic_code') %>%
    filter(form == type_form) %>%
    unique()

  return(df_info)


}

#' Get SIC codes from SIC
#'
#' Reads and caches SIC codes from SEC (<https://www.sec.gov/info/edgar/siccodes.htm>)
#'
#' @inheritParams get_edgar_fin_data
#'
#' @return A dataframe with sic codes
#' @export
#'
#' @examples
#'
#' \dontrun{
#' df_sic <- get_sic_codes(cache_folder = tempdir())
#' }
#'
get_sic_codes <- function(cache_folder) {

  f_sic <- file.path(cache_folder, 'sic_tab.csv')

  if (file.exists(f_sic)) {

    df_sic <- readr::read_csv(f_sic,
                              col_types = readr::cols())

    return(df_sic)

  }

  # if local file is not available
  my_url <- 'https://www.sec.gov/info/edgar/siccodes.htm'

  df_sic <- xml2::read_html(my_url) %>%
    rvest::html_table(fill = TRUE)

  df_sic <- df_sic[[1]][ , c(1,3)]
  #df_sic <- df_sic[4:nrow(df_sic), ]
  names(df_sic) <- c('sic_code', 'sic_desc')

  # save it
  readr::write_csv(df_sic, f_sic)

  return(df_sic)

}

