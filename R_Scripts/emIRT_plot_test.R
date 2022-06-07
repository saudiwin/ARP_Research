# Test the plotting package on each of the emIRT functions
# Use the examples from the help files


source('emIRT_graph.R')

# binIRT



data(s109)

rc <- convertRC(s109)
p <- makePriors(rc$n, rc$m, 1)
s <- getStarts(rc$n, rc$m, 1)

lout <- binIRT(.rc = rc,
               .starts = s,
               .priors = p,
               .control = {
                 list(threads = 1,
                      verbose = FALSE,
                      thresh = 1e-6)})

plot(lout,rc_data=rc)

# Add CIs

lout <- boot_emIRT(lout, .data = rc, .starts = s, .priors = p,
                       .control = list(threads = 1, verbose = FALSE, thresh = 1e-06), Ntrials=10, verbose=2)


plot(lout,rc_data=rc)

#show only republicans

plot(lout,rc_data=rc,subset_name='R')

#show only most liberal/most conservative senators.

plot(lout,rc_data=rc,subset_name=c('SESSIONS (R AL)','BOXER (D CA)'),subset_type='individual')

#adjust position of names for senators

plot(lout,rc_data=rc,hjust_bottom=-1,hjust_top=2)

# Function produces a ggplot object, so it can be further modified

outobj <- plot(lout,rc_data=rc)

outobj <- outobj + geom_vline(xintercept=0)

plot(outobj)

#hierIRT

data(dwnom)

lout <- hierIRT(.data = dwnom$data.in,
                .starts = dwnom$cur,
                .priors = dwnom$priors,
                .control = {list(
                  threads = 2,
                  verbose = TRUE,
                  thresh = 1e-4,
                  maxit=200,
                  checkfreq=1)})

# Note that with this data in which different legislators have multiple observations, ggplot2
# will put all observations for each legislator on the same row
plot(lout,legis.names=dwnom$legis$name,parties=dwnom$nomres$party)
plot(lout,legis.names=dwnom$legis$name,parties=dwnom$nomres$party,subset_name='HATCH      ',subset_type='individual')

#networkIRT

data(ustweet)

lout <- networkIRT(.y = ustweet$data,
                   .starts = ustweet$starts,
                   .priors = ustweet$priors,
                   .control = {list(verbose = TRUE,
                                    maxit = 100,
                                    convtype = 2,
                                    thresh = 1e-6,
                                    threads = 1)},
                   .anchor_item = 43)

plot(lout,legis.names=colnames(ustweet$data))

#ordIRT

data(AsahiTodai)


out.varinf <- ordIRT(.rc = AsahiTodai$dat.all, .starts = AsahiTodai$start.values,
                     .priors = AsahiTodai$priors, .D = 1,
                     .control = {list(verbose = TRUE,
                                      thresh = 1e-6, maxit = 500)})


plot(out.varinf)

#poisIRT

data(manifesto)

lout <- poisIRT(.rc = manifesto$data.manif,
                i = 0:(ncol(manifesto$data.manif)-1),
                NI=ncol(manifesto$data.manif),
                .starts = manifesto$starts.manif,
                .priors = manifesto$priors.manif,
                .control = {list(
                  threads = 1,
                  verbose = TRUE,
                  thresh = 1e-6,
                  maxit=1000)})

plot(lout,legis.names=colnames(manifesto$data.manif))

#dynIRT
#dynIRT requires different code to loop over the T time points and produce a facet.grid
#dynIRT objects do not inherit from emIRT objects, so the function has to be called explicitly

data("mq_data")

lout <- dynIRT(.data = mq_data$data.mq,
               .starts = mq_data$cur.mq,
               .priors = mq_data$priors.mq,
               .control = {list(
                 threads = 1,
                 verbose = TRUE,
                 thresh = 1e-6,
                 maxit=500)})

lout <- boot_emIRT(lout,Ntrials = 10,.data = mq_data$data.mq,
                   .starts = mq_data$cur.mq,
                   .priors = mq_data$priors.mq,
                   .control = {list(
                     threads = 1,
                     verbose = TRUE,
                     thresh = 1e-6,
                     maxit=500)})

# Function can only plot max six facets of time points. It will by default select 6 equally-spaced time points 

outobj <- plot.emIRT(lout,legis.names=row.names(mq_data$data.mq$rc),timelabels=as.character(1937:2013))

outobj <- outobj + geom_vline(xintercept=0)

plot(outobj)

# You can add time point labels and specify particular time points to plot

plot.emIRT(lout,legis.names=row.names(mq_data$data.mq$rc),timelabels=as.character(1937:2013),timepoints=c('1937','1938',
                                                                                                       '1939','1940',
                                                                                                       '1941','1942',
                                                                                                       '1943','1944'))

# Also select a judge to see what that judge has done over time

plot.emIRT(lout,legis.names=row.names(mq_data$data.mq$rc),timelabels=as.character(1937:2013),
           timepoints=as.character(1994:2004),
           subset_name=c('Rehnquist','Breyer'),
           subset_type='individual')
