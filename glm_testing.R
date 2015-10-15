library('data.table')
library('ROCR')

load('season_data/NBA2015.Rdata')
source('score_season.R')

#unweighted glm test
glm_uw <- function(date, season=NBA2015){
  training <- season[Date<date]
  fit <- glm(Home.Win ~ Home + prev.day.H + prev.count.3.H + Visitor + prev.day.V + prev.count.3.V,data=training,family='binomial')
  pred <- season[Date==date]
  pred$Prediction <- predict(fit,pred,type='response')
  pred <- pred[,.(Date,Prediction,Home.Win,Home,Home.Pts,Visitor,Vis.Pts)]
  return(pred)
}

glm_noprev_uw <- function(date, season=NBA2015){
  training <- season[Date<date]
  fit <- glm(Home.Win ~ Home + Visitor,data=training,family='binomial')
  pred <- season[Date==date]
  pred$Prediction <- predict(fit,pred,type='response')
  pred <- pred[,.(Date,Prediction,Home.Win,Home,Home.Pts,Visitor,Vis.Pts)]
  return(pred)
}


uw_glm <- score_season(model_func=glm_uw)
uw_noprev <- score_season(model_func=glm_noprev_uw)
