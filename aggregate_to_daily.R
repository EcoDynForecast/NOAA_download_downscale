aggregate_to_daily <- function(data){
  if("NOAA.member" %in% colnames(data)){
    daily.data <- data %>%
      dplyr::mutate(date = date(timestamp)) %>%
      group_by(NOAA.member, date) %>%
      dplyr::summarize_all("mean", na.rm = FALSE) %>%
      ungroup() %>%
      select(-timestamp)
  }else{
    daily.data <- data %>%
      dplyr::mutate(date = date(timestamp)) %>%
      group_by(date) %>%
      dplyr::summarize_all("mean", na.rm = FALSE) %>%
      ungroup() %>%
      select(-timestamp)
  }
  return(daily.data)
}