source('Tamar_Biodiversity_Functions.R')


##### ALA Records ####

#initialize variables
  unique_records.b = list()
  days.b = list()
  
  yrs = 2015:2020
  ny = length(yrs)
  for(i in 1:ny){
    dat.bris = read.csv( paste('ALA_records_', yrs[i],'.csv', sep ='') )
    unique_records.b[[i]] = Create1stRecords(dat.bris)
    min.date = min(unique_records.b[[i]]$eventDate)
    days.b[[i]] = (unique_records.b[[i]]$eventDate - min.date)/(60*60*24)
  }
  
  
##### ALA Records all years brisbane####

  #list of unique records for 2015-2020
  unique_records.b.ay = list()
  days.b.ay = list()
  
  yrs = 2015:2020
  ny = length(yrs)
  dat.bris.ay = read.csv( paste('ALA_records_', yrs[1],'.csv', sep ='') )
  
  for(i in 2:ny){
    dat.new.year = read.csv( paste('ALA_records_', yrs[i],'.csv', sep ='') )
    dat.bris.ay = rbind(dat.bris.ay, dat.new.year)
  }
  
  unique_records.b.ay = Create1stRecords(dat.bris.ay)
  min.date.ay = min(unique_records.b.ay$eventDate)
  days.b.ay = (unique_records.b.ay$eventDate - min.date.ay)/(60*60*24)
  

#### House Records ####
  dat.house = read.csv('House_records.csv')
  unique_records.h = Create1stRecords(dat.house)
  min.date.h = min(unique_records.h$eventDate)
  days.h = (unique_records.h$eventDate - min.date.h)/(60*60*24)

#### QLD Records ####
  dat.QLD = read.csv('ALA_QLD_records.csv')
  unique_records.q = Create1stRecords(dat.QLD)
  min.date.q = min(unique_records.q$eventDate)
  days.q = (unique_records.q$eventDate - min.date.q)/(60*60*24)
  
  
#### Plot Accumulation Curves with QLD ####
  lw=2
  
  #queensland curve
  plot(days.q, cumsum(unique_records.q$tick), type='l', 
       lty=2, col=2, lwd = lw,
       xlab = 'Days', ylab = 'Species')  
  
  #brisbane 2020 curve
  lines(days.b[[ny]], cumsum(unique_records.b[[ny]]$tick), type='l', lty=3, col=3)
    
  #brisbane curves
  for(i in 1:(ny-1)){
    lines(days.b[[i]], cumsum(unique_records.b[[i]]$tick), type='l', col = ny-i+3, lty = ny-i+3)
  }
  
  #house curve
  house.acum.curv = cumsum(unique_records.h$tick)
  lines(days.h, cumsum(unique_records.h$tick), 
        type='l', col=1, lty=1, lwd = lw)

  
  legend("topleft", c('QLD 2020', paste('Brisbane', as.character(rev(yrs))), 'House 2020'), 
         col = c( 2:(ny+2), 1), lty = c( 2:(ny+2), 1), lwd = c(lw, rep(1,ny),lw)
         )
  
  #significant dates to check against data
    ease.lockdown = ( as.numeric( as.POSIXct(strptime("2020-05-15", "%Y-%m-%d") ) ) - min.date)/(60*60*24)
    end.uni.lockdown = ( as.numeric( as.POSIXct(strptime("2020-07-10", "%Y-%m-%d") ) ) - min.date)/(60*60*24)
    bump = ( as.numeric( as.POSIXct(strptime("2020-11-12", "%Y-%m-%d") ) ) - min.date)/(60*60*24)
    
    #abline(v=ease.lockdown, lty=2, col='orange', lwd=2)
    #abline(v=end.uni.lockdown, lty=3, col='green', lwd=2)
    #abline(v=bump, lty=4, col='purple', lwd=2)
    
    plot(days.b.ay/365, cumsum(unique_records.b.ay$tick), type='l', 
         lty=2, col=2, lwd = lw, main = "Brisbane Species Accumulation Curve",
         xlab = 'Years', ylab = 'Species') 
    
    ######
    
    
    
    
    
    
    
    #### Plot Brisbane Curves 
    #### use lim 1 for lockdown, lim 2 for year
    lw=4
    
    #significant dates to check against data
    ease.lockdown = ( as.numeric( as.POSIXct(strptime("2020-05-15", "%Y-%m-%d") ) ) - min.date)/(60*60*24)
    end.uni.lockdown = ( as.numeric( as.POSIXct(strptime("2020-07-10", "%Y-%m-%d") ) ) - min.date)/(60*60*24)
    bump = ( as.numeric( as.POSIXct(strptime("2020-11-12", "%Y-%m-%d") ) ) - min.date)/(60*60*24)
    
    xlim1 = c(0, end.uni.lockdown)
    xlim2 = c(0,365)
    
    ylim1 = c(0,1500)
    ylim2 = c(0,2500)
    par(oma=rep(0,4), mar = c(5,5,2,2))

    #brisbane 2020 curve
    plot(days.b[[ny]], cumsum(unique_records.b[[ny]]$tick), 
         xlim = xlim2,
         ylim = ylim2,
         type='l', lty=2, col=3, lwd = lw,
         xaxs="i", yaxs="i",
         xlab = 'Days', ylab = 'Species', cex.axis = 1.5, cex.lab=2)  
    
    #brisbane curves
    for(i in 1:(ny-1)){
      lines(days.b[[i]], cumsum(unique_records.b[[i]]$tick), 
            type='l', col = 'grey', lty = 3, lwd = lw)
    }
    
    #house curve
    house.acum.curv = cumsum(unique_records.h$tick)
    lines(days.h, cumsum(unique_records.h$tick), 
          type='l', col=1, lty=1, lwd = lw)
    
    
    legend("topleft", c('Brisbane 2020', 'Brisbane 2015 - 2019', 'House 2020'), 
           col = c('green','grey','black'), lty = c( 2, 3, 1), lwd = rep(lw,3), bty = 'n', cex=1.3
    )
    

#species at end of year
    tot.year = tail(which(days.h==365),1)
    tot.lockdown = tail(which(days.h==end.uni.lockdown),1)
