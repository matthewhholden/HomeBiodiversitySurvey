
Create1stRecords = function(dat){
  #takes in a dataset of observation records from ALA, sorted by date, must contain columns named
  #kingdom, phylum, class, order, family, genus, species, eventDate
  #outputs a data set including only the first observation of a "species"
  #where a "species" is a unique leaf of the phylogenic tree. In other words
  #a record only IDed to genus is considered a unique species if there are no
  #records IDed to species in the data set that also belong to that genus.
  
#### species
  
  #reomve duplicate records
    dat.nd = dat[!duplicated(dat[c('kingdom', 'phylum', 'class', 'order', 'family', 'genus', 'species')]), ]
    print(paste('There are ', sum( duplicated( dat[c('kingdom', 'phylum', 'class', 'order', 'family', 'genus', 'species')])), ' confirmed duplicates'))
    print(head(dat[duplicated(dat[c('kingdom', 'phylum', 'class', 'order', 'family', 'genus', 'species')]), ], 10 ))
    
  #find potential duplicate records, that cannot be certain to be a diff species, including 
    #(row 1) those not marked to species, with duplicate genus
    #(row 2) those not marked to genus, with duplicate family
    #(row 3) those not marked to family, with duplicate order
    #(row 4) those not marked to order, with duplicate class
    #(row 5) those not marked to class, with duplicate phylum
    #(row 6) those not marked to phylum, with duplicate kingdom
    potential.duplicate = 
      ( dat.nd$species=='' & dat.nd$genus!='' & duplicated(dat.nd$genus) ) |
      ( dat.nd$species=='' & dat.nd$genus=='' & dat.nd$family!='' & duplicated(dat.nd$family) ) |
      ( dat.nd$species=='' & dat.nd$genus=='' & dat.nd$family=='' & dat.nd$order!='' & duplicated(dat.nd$order) ) |
      ( dat.nd$species=='' & dat.nd$genus=='' & dat.nd$family=='' & dat.nd$order=='' & dat.nd$class!='' & duplicated(dat.nd$class) ) |
      ( dat.nd$species=='' & dat.nd$genus=='' & dat.nd$family=='' & dat.nd$order=='' & dat.nd$class=='' & dat.nd$phylum!='' & duplicated(dat.nd$phylum) ) |
      ( dat.nd$species=='' & dat.nd$genus=='' & dat.nd$family=='' & dat.nd$order=='' & dat.nd$class=='' & dat.nd$phylum=='' & dat.nd$kingdom!=''& duplicated(dat.nd$kingdom) )
    
  #remove potential duplicate records
    dat.npd = dat.nd[!potential.duplicate,]
    print( head( dat.nd[potential.duplicate,] , 10 ))
    print( paste("There are potentially, ",  sum(potential.duplicate)," more unconfirmed duplicates" ))
    #dat.npd =dat.nd (toggle off to check if different when not removing "potential" duplicates)
    

  #sort data from first record to last record by date  
    dat.npd$tick = 1
    sorted.unique.records = dat.npd[order(dat.npd$eventDate),]

  return(sorted.unique.records)
}
