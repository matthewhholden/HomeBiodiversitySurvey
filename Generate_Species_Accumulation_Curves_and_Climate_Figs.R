source('Unique_Record_Function.R')


##### ALA Records ####
print("Starting Brisbane tallies")
#initialize variables
  unique_records.b = list()
  days.b = list()
  
  yrs = 2015:2020
  ny = length(yrs)
  for(i in 1:ny){
    print(yrs[i])
    dat.bris = read.csv( paste('ALA_records_', yrs[i],'.csv', sep ='') )
    unique_records.b[[i]] = Create1stRecords(dat.bris)
    min.date = min(unique_records.b[[i]]$eventDate)
    days.b[[i]] = (unique_records.b[[i]]$eventDate - min.date)/(60*60*24)
  }

#### House Records ####
  print("Starting house tally")
  dat.house = read.csv('House_records_final.csv')
  unique_records.h = Create1stRecords(dat.house)
  min.date.h = min(unique_records.h$eventDate)
  days.h = (unique_records.h$eventDate - min.date.h)/(60*60*24)


    
#########################################
#### Rain and Temperature Data
#########################################
    
    #### load data
    dat = read.csv('Brisbane daily rainfall & temp data 2020-21.csv', header=FALSE)
    names(dat) = c('date','temp.min','temp.max')
    
    
    #### data cleaning
    dat = dat[-356,] #remove extra row with column labels
    dat$temp.max[7]=dat$temp.max[6] #add missing value based on nearby weather stations which say day Jan 6 and 7 had similar max temp
    dat$temp.min = as.numeric(dat$temp.min)
    
    ####
    l=length(dat$temp.min)
    ind.start = which(dat$date=="29/03/2020")
    ind.end = which(dat$date=="29/03/2021")
    tmin = dat$temp.min[-(c(1:ind.start,(ind.end+1):l))]
    tmax = dat$temp.max[-(c(1:ind.start,(ind.end+1):l))]
    days = 1:length(tmin)
    
    ####
    dat.rain = read.csv('rain.csv', header=FALSE)
    names(dat.rain) = c('date','rain')
    dat.rain[is.na(dat.rain$rain),2] = 0
    rain = dat.rain$rain[-(c(1:ind.start,(ind.end+1):l))]

    
    
##########################################
#### taxa specific accumulation curves
########################################## 
    

    ############### Taxa
    
    #index of all records of a particular species
    ind.lep = which(unique_records.h$order=="Lepidoptera")
    ind.dip = which(unique_records.h$order=="Diptera")
    ind.pla = which(unique_records.h$kingdom=="Plantae")
    ind.col = which(unique_records.h$order=="Coleoptera")
    ind.hym = which(unique_records.h$order=="Hymenoptera")
    ind.hem = which(unique_records.h$order=="Hemiptera")
    ind.ara = which(unique_records.h$class=="Arachnida")
    ind.ave = which(unique_records.h$class=="Aves")
    ind.np = which(unique_records.h$kingdom !="Plantae")
    ind.fg = which(unique_records.h$kingdom =="Fungi")
    ind.m = which(unique_records.h$class =="Mammalia")
    
    
    house.acum.curv.l = cumsum(unique_records.h$tick[ind.lep])
    house.acum.curv.d = cumsum(unique_records.h$tick[ind.dip])
    house.acum.curv.p = cumsum(unique_records.h$tick[ind.pla])
    house.acum.curv.c = cumsum(unique_records.h$tick[ind.col])
    house.acum.curv.h = cumsum(unique_records.h$tick[ind.hym])
    house.acum.curv.he = cumsum(unique_records.h$tick[ind.hem])
    house.acum.curv.a = cumsum(unique_records.h$tick[ind.ara])
    house.acum.curv.av = cumsum(unique_records.h$tick[ind.ave])
    house.acum.curv.np = cumsum(unique_records.h$tick[ind.np])
    house.acum.curv.fg = cumsum(unique_records.h$tick[ind.fg])
    
    days.h.l = (unique_records.h$eventDate[ind.lep] - min.date.h)/(60*60*24)
    days.h.d = (unique_records.h$eventDate[ind.dip] - min.date.h)/(60*60*24)
    days.h.p = (unique_records.h$eventDate[ind.pla] - min.date.h)/(60*60*24)
    days.h.c = (unique_records.h$eventDate[ind.col] - min.date.h)/(60*60*24)
    days.h.h = (unique_records.h$eventDate[ind.hym] - min.date.h)/(60*60*24)
    days.h.he = (unique_records.h$eventDate[ind.hem] - min.date.h)/(60*60*24)
    days.h.a = (unique_records.h$eventDate[ind.ara] - min.date.h)/(60*60*24)
    days.h.av = (unique_records.h$eventDate[ind.ave] - min.date.h)/(60*60*24)
    days.h.np = (unique_records.h$eventDate[ind.np] - min.date.h)/(60*60*24)
    days.h.fg = (unique_records.h$eventDate[ind.fg] - min.date.h)/(60*60*24)


##########################################
#### Plot combined Fig 
########################################## 

# png(file = 'Accumulation_Curves_and_climate.png', 
#     height = 4.6, width = 7, units='in', res=4800)
    
pdf(file = 'Accumulation_Curves_and_climate.pdf', 
    height = 4.6, width = 7)    
ca = 1.1
cl = 1.4
clg = 0.92
lw = 2

par(mfrow = c(2,2), mar = c(0.5,4.4,0.8,0.0), oma = c(2.7,0,0.15,0.15))

xlim2 = c(0,365)
ylim2 = c(0,2650)

#brisbane 2020 curve
plot(days.b[[ny]], cumsum(unique_records.b[[ny]]$tick), 
     xlim = xlim2,
     ylim = ylim2,
     type='l', lty=2, col=3, lwd = lw,
     xaxs="i", yaxs="i", xaxt='n',
     xlab = '', ylab = '', cex.axis = ca, cex.lab=cl)  
mtext(side = 2, outer=FALSE, "Species", cex.lab = cl, cex = cl, padj = -2.5)
#brisbane curves
for(i in 1:(ny-1)){
  lines(days.b[[i]], cumsum(unique_records.b[[i]]$tick), 
        type='l', col = 'grey', lty = 3, lwd = lw)
}
#house curve
text(17,2435,"a)", cex=cl)
house.acum.curv = cumsum(unique_records.h$tick)
lines(days.h, cumsum(unique_records.h$tick), 
      type='l', col=1, lty=1, lwd = lw)
legend(45,2720, c( 'Brisbane 2015 - 2019','Brisbane 2020', 'House 2020'), 
       col = c('grey','green','black'), lty = c( 3, 2, 1), lwd = rep(lw,3), 
       bty = 'n', cex=clg
)


plot(days.h.d, house.acum.curv.d, 
     xlim = c(0,365), ylim = c(0,111),
     type='l', lty=2, col=2, lwd = lw,
     xaxs="i", yaxs="i", xaxt='n',
     xlab = '', ylab = '', cex.axis = ca, cex.lab=cl) 
text(17,102,"b)", cex=cl)
mtext(side = 2, outer=FALSE, "Species", cex.lab = cl, cex = cl, padj = -2.5)
lines(days.h.c, house.acum.curv.c, col=3, lty=3, lwd = lw)
lines(days.h.h, house.acum.curv.h, col=4, lty=4, lwd = lw)
lines(days.h.he, house.acum.curv.he, col=5, lty=5, lwd = lw)
lines(days.h.a, house.acum.curv.a, col=6, lty=6, lwd = lw)
lines(days.h.av, house.acum.curv.av, col=7, lty=7, lwd = lw)

legend(200, 58.5, c('Diptera', 'Coleoptera', 'Hymenoptera',
                        'Hemiptera', 'Arachnida', 'Aves'), 
       col = 2:7, lty = 2:7, lwd = rep(lw,6), bty = 'n', cex=.99*clg
)    

plot(days, tmin, type ='b',
     ylim = c( .96*min(as.numeric(dat$temp.min)), 1.04*max(as.numeric(dat$temp.max))),
     col = 'blue', pch = 25, bg = 'blue', cex=.6,
     cex.axis = ca, cex.lab=cl,
     xaxs="i", yaxs="i",
     ylab = '', xlab = 'Days')
points(days, tmax, type ='b', col = 'red', pch=24, bg='red', cex=.6)
mtext(side = 2, outer=FALSE, "Daily temperature (°C)", cex.lab = cl, cex = cl, padj = -2.5)
text(17, 34.6,"c)", cex=cl)

plot(1:365, rain, type='h',     
     cex.axis = ca, cex.lab=cl,
     xaxs="i", yaxs="i",
     ylab = '', xlab = 'Days')
text(17,116,"d)", cex=cl)

mtext(side = 2, outer=FALSE, "Daily rainfall (mm)", cex.lab = cl, cex = cl, padj = -2.5)
mtext(side = 1, outer=TRUE, "Days", cex.lab = cl, cex = cl, padj = 1.5)
dev.off()
