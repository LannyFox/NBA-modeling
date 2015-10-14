#' this script has the function to score a season

score_season <- function(season=NBA2015, model_func=glm_uw){
  first_score_date <- as.Date(median(as.numeric(season$Date)),origin='1970-01-01')
  daily_preds<- data.table()
  for (date in season[Date>first_score_date,unique(Date)]){
    daily_preds <- rbindlist(list(daily_preds,model_func(date=date,season=season)))
  }
  return(daily_preds)
}
