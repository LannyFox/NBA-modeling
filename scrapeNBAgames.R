library('XML')
library('data.table')

scrapeNBAgames <- function(Season){
  url <- paste0('http://www.basketball-reference.com/leagues/NBA_',Season,'_games.html?lid=header_seasons')

  dat <- data.table(readHTMLTable(url,stringsAsFactors=F)[[1]][,c(1,4:7)])
  N <- nrow(dat)
  setnames(dat,old=c('Visitor/Neutral','Home/Neutral'),c('Visitor','Home'))
  dat[,'Date':=as.Date(substr(dat$Date,6,30),format="%b %d, %Y")]
  dat[,'Home.Pts':=as.numeric(PTS)][,'Vis.Pts':=as.numeric(PTS.1)][,'Home.Win':=1*(Home.Pts>Vis.Pts)]
  dat[,c('PTS','PTS.1'):=NULL]
  setkeyv(dat,c("Date","Home","Visitor"))
  dat2 <- dat[rep(1:N,N),.(Date,Visitor,Home)];setnames(dat2,c("P.Date",'P.Visitor','P.Home'))
  datPanel <- data.table(cbind(dat[rep(1:N,each=N),.(Date,Visitor,Home)],dat2))
  
  ###Possible predictors:
  # Game previous day y/n
  # Count games previous 5 days
  # count games previous 3 days
  # count games previous 2 days
  # OT game previous day y/n
  # OT game last 2 days y/n
  # OT game last 3 days y/n
  panelCollapse <- datPanel[,.(prev.day.H=sum((P.Home==Home|P.Visitor==Home)&P.Date==(Date-1)),
                               prev.count.3.H=sum((P.Home==Home|P.Visitor==Home)&P.Date>=(Date-3)&P.Date<Date), 
                               
                               prev.day.V = sum((P.Visitor==Visitor|P.Home==Visitor)&P.Date==(Date-1)),
                               prev.count.3.V = sum((P.Visitor==Visitor|P.Home==Visitor)&P.Date>=(Date-3)&P.Date<Date)
                               ),keyby=c('Date','Home','Visitor')]

  
  
  merged <- dat[panelCollapse][,c('prev.count.3.V','prev.count.3.H'):=list(factor(prev.count.3.V),factor(prev.count.3.H))]
  ret <- cbind(Season,merged)
  return(ret)
}

if (F){
for (year in 2005:2015){
  assign(paste0('NBA',year),scrapeNBAgames(year))
  save(list=paste0('NBA',year),file=paste0('season_data/NBA',year,'.Rdata'),row.names=F)
  print(year)
}
}





