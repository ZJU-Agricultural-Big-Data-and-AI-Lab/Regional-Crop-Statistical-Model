libs <- c("data.table", "dplyr")
lapply(libs, require, character.only = TRUE)

df <- fread('../data/County records.csv')

order_dis <- unique(df$District)

district_coef <- data.table()
district_r2 <- data.table()

# Construct MLR over districts under three temporal resolution, including phase, month, and season.
for (dis in order_dis){
  sector <- df[District==dis,]
  
  phase.lm <- lm(de_Yield~GP1.KDD+GP1.GDD+GP1.PCPN+
                          GP2.KDD+GP2.GDD+GP2.PCPN+
                          GP3.KDD+GP3.GDD+GP3.PCPN+
                          GP4.KDD+GP4.GDD+GP4.PCPN, data=sector)

  month.lm <-lm(de_Yield~KDD_Jun+GDD_Jun+PCPN_Jun+
                         KDD_Jul+GDD_Jul+PCPN_Jul+
                         KDD_Aug+GDD_Aug+PCPN_Aug+
                         KDD_Sep+GDD_Sep+PCPN_Sep, data=sector)
          
  season.lm <- lm(de_Yield~season.GDD+season.KDD+season.PCPN, data=sector)
  
  coef_phase <- summary(phase.lm)$coefficients%>%as.data.table(keep.rownames = TRUE)
  coef_month <- summary(month.lm)$coefficients%>%as.data.table(keep.rownames = TRUE)
  coef_season <- summary(season.lm)$coefficients%>%as.data.table(keep.rownames = TRUE)
  
  coef_phase$scale <- 'Phase'
  coef_month$scale <- 'Month'
  coef_season$scale <- 'Season'
  coef <- rbind(coef_phase,coef_month,coef_season)
  coef$District <- dis
  
  r2_phase <- data.table(r2=summary(phase.lm)$r.squared,
                         adj.r2=summary(phase.lm)$adj.r.squared,
                         District = dis,scale='Phase')
  r2_month <- data.table(r2=summary(month.lm)$r.squared,
                         adj.r2=summary(month.lm)$adj.r.squared,
                         District = dis,scale='Month')
  r2_season <- data.table(r2=summary(season.lm)$r.squared,
                          adj.r2=summary(season.lm)$adj.r.squared,
                          District = dis,scale='Season')
  r2 <- rbind(r2_phase,r2_month,r2_season)
  
  district_coef <- rbind(district_coef,coef)
  district_r2 <- rbind(district_r2,r2)
}

# Summarize the results
tapply(district_r2$adj.r2, district_r2$scale, summary)
tapply(district_r2$r2, district_r2$scale, summary)

tapply(district_coef[scale == 'Phase' & `Pr(>|t|)`<0.05]$Estimate, 
       district_coef[scale == 'Phase' & `Pr(>|t|)`<0.05]$rn, summary)


# Statistics over states
KDD <- c('GP1.KDD','GP2.KDD','GP3.KDD','GP4.KDD')
GDD <- c('GP1.GDD','GP2.GDD','GP3.GDD','GP4.GDD')
PCPN <- c('GP1.PCPN','GP2.PCPN','GP3.PCPN','GP4.PCPN')
factors <- c(KDD,GDD,PCPN)
coef_phase <- district_coef[scale == 'Phase' & rn != '(Intercept)' & `Pr(>|t|)`<0.05]
coef_phase[,c("State", "district") := tstrsplit(District, ",", fixed=TRUE)]

mean_st <- coef_phase[,lapply(.SD,mean),by=c('State','rn'),.SD=c('Estimate')]
st_coef <- dcast(mean_st,State~rn,value.var='Estimate')

mean_cornbelt <- coef_phase[,lapply(.SD,mean),by=c('rn'),.SD=c('Estimate')]

