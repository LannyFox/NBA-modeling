library('data.table',quietly = T)
library('ROCR')

load('season_data/NBA2015.Rdata')

#unweighted glm test
glm_uw <- function(date, season=NBA2015){
  training <- season[Date<date]
  fit <- glm(Home.Win ~ Home + prev.day.H + prev.count.3.H + Visitor + prev.day.V + prev.count.3.V,data=training,family='binomial')
  pred <- game.data[Date==date]
  pred$Prediction <- predict(fit,pred,type='response')
  pred <- pred[,.(Date,Prediction,Home.Win,Home,Home.Pts,Visitor,Vis.Pts)]
  return(pred)
}

score_season <- function(season=NBA2015, model_func=glm_uw){
  first_score_date <- as.Date(median(as.numeric(season$Date)),origin='1970-01-01')
  daily_preds<- data.table()
  for (date in season[Date>first_score_date,unique(Date)]){
    daily_preds <- rbindlist(list(daily_preds,model_func(date=date,season=season)))
  }
  return(daily_preds)
}

tst2 <- score_season()
