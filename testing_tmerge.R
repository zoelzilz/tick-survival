# dicking around with Survival Package, from the literature

head(cgd0)

## learning to use tmerge, which seems a lot like a bunch of tools in tidyverse

# call 1 - "sets a time range for each subject yo be from 0 to last observation, set explicitly with tstop
newcgd <- tmerge(data1 = cgd0[,1:13], #picks the first dataset, which is the first 13 rows of cgd
                 data2 = cgd0, # i guess the second dataset can be the same data?
                 id = id, # merging on the id column?
                 tstop=futime) # sets time range for each individual til... event?

# call 2 - could have been part of the above, tmerge processes events sequentially
newcgd <- tmerge(newcgd, # new one
                 cgd0, #old one
                 id = id,
                 infect = event(etime1), infect = event(etime2), infect = event(etime3),infect = event(etime4), infect=event(etime5),infect=event(etime6), infect=event(etime7)) # does this add a new column called infect?



############################
# using the JASA data set might be better, looks more like ours
head(jasa)
# tx.date = transplant date // fu.date = last day of follow up // fu.stat = dead or alive at that date // 

# need to add an identifier (we already have this)
jasa$subject <- 1:nrow(jasa) #fun to play with base R - add a new col called subject

tdata <- with(jasa, data.frame(subject = subject, # make a new dataframe with data from jasa
                              futime = pmax(.5, fu.date - accept.dt), # adds new col of total time in experiment? our data has this
                              txtime = ifelse(tx.date == fu.date, #creating a col of time after transplant
                                              (tx.date - accept.dt) -.5, #if the date of transplant = last date (death) then subtract acceptance date and half a day
                                              (tx.date = accept.dt)), # otherwise, just subtract acceptance date
                              fustat = fustat # keep this variable the same
                              ))

xdata <- tmerge(jasa, tdata, id = subject, #merging on subject
                death = event(futime, fustat), # death is the event fustat (0/1) we are adding at time futime
                transplant = tdc(txtime), # transplant is the time dependent variable, defaults to 0 before time txtime and 1 after, i think, but can also be a continuous variable x (see 3.4: PBC data in literature)
                trt = tdc(txtime),
                options = list(idname = "subject")
                )


## ok well wtf lets  try it with our data
attach(tickmclimate)

tmc <- tmerge(tickmclimate, tickmclimate, id = tickID,
              death = event(days_elpsd, death_t_f))
head(tmc)
View(tmc)

#ok so this works how it's supposed to but we need to get the appropriate temp data in there




#################################################### using two datasets, one with repeated measures
view(pbc)
temp <- subset(pbc, id <= 312, select = c(id:sex, stage))
pbc2 <- tmerge(temp, temp, id = id , death = event(time, status)) #set range... what does this mean
# tmerge is adding tstart (0) and tstop using time (# of days between registration and death, which = our days_elapsd)
# tick18_2 <-tmerge(tick18, tick18, id = tickID, death = event (days_elapsd, death_t_f))

# now incorporate other data
pbc3 <- tmerge(pbc2, pbcseq, id = id, ascites = tdc(day, ascites), bili = tdc(day, ascites), albumin = tdc(day, albumin)) # adds a bunch of tdc's ie time dependent covariates
View(pbc3)
# tick18_3 <- tmerge(tick18_2, microclimate) .... dang, doesnt work perfectly because we have no ID to tmerge on, ID for the tick data is different (more) than for the climate data.

# we can just make our final dataset mimic what pbc3 looks like:
## we need to add a tstart and tstop for each one