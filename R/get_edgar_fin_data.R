#' Imports data from Edgar (SEC/US)
#'
#' Every company trades in the US must deliver its financial statements to the SEC.
#' Edgar is the system that organizes such information and allows user to query the documents.
#'
#' This function downloads data from zip files available at SEC/DERA (<https://www.sec.gov/dera/data>).
#'
#' @param companies Name of companies (see get_info_companies() for querying available names)
#' @param years Years of datasets (numeric vector)
#' @param type_data Type of dataset ('yearly' or 'quarterly')
#' @param consolidated Consolidade financial statements? TRUE (default) or FALSE
#' @param do_cache Use memoise cache system? TRUE (default) or FALSE
#' @param cache_folder Path of cache folder to save all memoise files and downloaded zip files
#'
#' @return A dataframe with financial statements
#' @export
#'
#' @import dplyr
#' @examples
#'
#' \dontrun{
#'
#' my.company <- 'APPLE INC'
#' my.years <- 2016:2018
#' my.type <- 'yearly'
#'
#' out <- get_edgar_fin_data(companies = my.company,
#'                          years = my.years,
#'                          type.data = my.type,
#'                          consolidated = TRUE)
#' }
get_edgar_fin_data <- function(companies,
                               years = seq(2009, lubridate::year(Sys.Date())),
                               type_data = 'yearly',
                               consolidated = TRUE,
                               do_cache = TRUE,
                               cache_folder = 'GetEdgarData-cache') {

  possible_cases <- c('yearly', 'quarterly')
  if (!(type_data %in% possible_cases) ) {
    stop('Argument type.data should be one of ',
         paste0(possible_cases, collapse = ', '))
  }

  # fix check msgs (no visible binding)
  year <- quarter <- current_name  <- coreg <- NULL

  message('Fetching ', type_data, ' data for ', length(companies), ' companies:')

  df_files <- get_available_files()

  if (type_data == 'yearly') my_quarters = 'Q4'
  if (type_data == 'quarterly') my_quarters = c('Q1', 'Q2', 'Q3', 'Q4')

  df_files_temp <- df_files %>%
    filter(year %in% years,
           quarter %in% my_quarters)

  if (nrow(df_files_temp) == 0) {
    stop('Cant find selected years and quarters in edgar website.. ')
  }

  l_args <- list(year_in = df_files_temp$year,
                 quarter_in = df_files_temp$quarter,
                 do_cache = do_cache,
                 cache_folder = cache_folder,
                 type_data = rep(list(type_data), nrow(df_files_temp)))

  # get data
  l_out <- purrr::pmap(.l = l_args,
                       .f = dl_and_read_single_zip_file)

  # organize it and filter companies
  df_sic <- get_sic_codes(cache_folder)

  # bind all data and fix it
  df <- dplyr::bind_rows(purrr::map(l_out, 1)) %>%
    filter(current_name %in% companies,
           dplyr::if_else(rep(consolidated, n()),
                          is.na(coreg),
                          !is.na(coreg)) ) %>%
    left_join(df_sic, by = 'sic_code')


  return(df)

}

