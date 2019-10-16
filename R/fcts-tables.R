#' Imports a single table from unzipped file
#'
#' After unzipping the file from Edgar, this function will read a particular type of table (num, sub or pre).
#'
#' @param path_to_files Path to the folder where zip file was decompressed
#' @param type_table The type of table ('num', 'sub', 'pre')
#' @inheritParams get_edgar_fin_data
#'
#' @return A dataframe with the requested data
#' @export
#'
#' @examples
#'
#' \dontrun{
#' df <- get_single_raw_table(path_to_files = tempdir(),
#'                            'num',
#'                            'GetEdgar-Cache')
#' }
get_single_raw_table <- function(path_to_files = tempdir(),
                                 type_table,
                                 cache_folder) {

  message('\t\t- Reading "', type_table, '" table')

  file_to_read <- file.path(path_to_files,
                            paste0(type_table, '.txt') )


  suppressWarnings({
    df_out <- readr::read_delim(file_to_read, delim = '\t',
                                col_types = readr::cols(),
                                progress = FALSE, quote = '"')
  })

  df_out <- switch(type_table,
                   'num' = parse_num_table(df_out),
                   'sub' = parse_sub_table(df_out),
                   'pre' = parse_sub_table(df_out))

  return(df_out)

}

#' Organizes a "sub" table from unzipped file
#'
#' @param df.in Raw dataframe from .txt file
#'
#' @return An organized dataframe
#' @export
#'
#' @examples
#'
#' \dontrun{
#' df_sub <- parse_sub_table(raw_sub_df)
#' }
parse_sub_table <- function(df.in) {

  # fix check msgs (no visible binding)
  name <- former <- changed <- sic <- NULL
  countryba <- stprba <- cityba <- cik <- adsh <- NULL
  form <- fy <- fp <- sic_code <- NULL

  df_sub <- df.in %>%
    select(current_name = name,
           former_name = former,
           change_date_name = changed,
           sic_code = sic,
           country = countryba,
           state = stprba,
           city = cityba,
           cik,
           id_file = adsh,
           form,
           year = fy,
           quarter = fp)

  df_sub <- df_sub %>%
    mutate(sic_code = as.numeric(sic_code))

  # leave quarterly for FY and Q* (some quarterly data are at FY)

  return(df_sub)

}

#' Organizes a "num" table from unzipped file
#'
#' @param df.in Raw dataframe from .txt file
#'
#' @return A dataframe
#' @export
#'
#' @import dplyr
#' @examples
#'
#' \dontrun{
#' df_num <- parse_num_table(raw_num_df)
#' }
parse_num_table <- function(df.in) {

  #glimpse(df.in)

  # fix check msgs (no visible binding)
  adsh <- tag <- version <- ddate <- NULL
  uom <- value <- qtrs <- coreg <- ref_date <-  NULL

  df_num <- df.in %>%
    select(id.file = adsh,
           tag,
           version,
           ref_date = ddate,
           unit_ref = uom,
           value_ref = value,
           qtrs,
           coreg) %>%
    mutate(ref_date = as.Date(as.character(ref_date), '%Y%m%d'))

  df_num <- unique(df_num)

  return(df_num)

}


#' Imports "tag" table
#'
#' @inheritParams get_single_raw_table
#'
#' @return A dataframe
#' @export
#'
#' @examples
#' \dontrun{
#' df_raw_tag <- get_tag_table(path_to_files = tempdir())
#' }
get_tag_table <- function(path_to_files = tempdir()) {

  file_to_read <- file.path(path_to_files, 'tag.txt')
  df_tag <- readr::read_delim(file_to_read, delim = '\t',
                              col_types = readr::cols() )
  return(df_tag)
}

#' Imports "pre" table
#'
#' @inheritParams get_single_raw_table
#'
#' @return A dataframe
#'
#' @export
#'
#' @examples
#'
#' \dontrun{
#' df_raw_pre <- get_pre_table(path_to_files = tempdir())
#' }
#'
get_pre_table <- function(path_to_files = tempdir()) {

  file_to_read <- file.path(path_to_files, 'pre.txt')
  df_pre <- readr::read_delim(file_to_read, delim = '\t',
                       col_types = readr::cols() )

  return(df_pre)
}


#' Parses final table from zip file
#'
#' @param df_num Dataframe with numerical data
#' @param df_sub Dataframe with sub table
#' @inheritParams get_edgar_fin_data
#' @inheritParams dl_and_read_single_zip_file
#'
#' @return A dataframe with the final data
#' @import dplyr
#' @export
#'
#' @examples
#' \dontrun{
#' df_final <- parse_final_table(df_num, df_sub, 'quarterly', 2018)
#' }
parse_final_table <- function(df_num, df_sub, #df.pre,
                              type_data,
                              year_in) {

  message('\t\t- Parsing final table')

  # fix check msgs (no visible binding)
  country <- state <- city <- sic_code <- ref_date <- NULL
  qtrs <- quarter <- NULL

  # filter info in df_sub
  df_sub <- df_sub %>%
    select(-country, -state, -city) %>%
    mutate(sic_code = as.numeric(sic_code))

  df_tab <- inner_join(df_sub, df_num, by = 'id.file') %>%
    filter(lubridate::year(ref_date) == year_in)

  if (type_data == 'yearly') {

    df_tab <- df_tab %>%
      filter(qtrs == 4,
             quarter == 'FY')

  } else if (type_data == 'quarterly') {
    # leave FY for Q data (final 4Q is at 10-K)
    df_tab <- df_tab %>%
      filter(qtrs == 1)
  }

  return(df_tab)
}
