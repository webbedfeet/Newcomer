library(haven)
library(dplyr)
library(ggplot2)
library(tibble)
dat <- read_dta('merged_ATI_LPC_CPC_may_5.dta')
for(i in 1:ncol(dat)){
  if(is(dat[[i]],'Date')) dat[[i]] = as.Date(as.character(dat[[i]]))
  if(is.numeric(dat[[i]])) dat[[i]] = as.numeric(dat[[i]])
  if(is.integer(dat[[i]])) dat[[i]] = as.numeric(dat[[i]])
}

# Bland-Altman Analysis

x <- dat$balometerintake
y <- dat$intakevolume

blah = tibble(x,y)
blah = blah[complete.cases(blah),]
blah <- blah %>% mutate(M = y-x,
                        A = (y+x)/2,
                        M2 = (y-x)/x)
plot(M2~A, data=blah)
SD = sd(blah$M2)
Mean = mean(blah$M2)
abline(h=Mean)
abline(h = c(Mean+2*SD, Mean - 2*SD), lty=2)

ggplot(blah, aes(x=A, y = M2))+geom_point()+
  geom_hline(yintercept = c(Mean, Mean - 2*SD, Mean + 2*SD),
             linetype = c(1,2,2))+
  labs(x = 'Average', y = 'Proportional difference')+
  theme_bw()

# Find observation right after filter change

bl <- plyr::ddply(dat, ~unitnumber, function(m){
  if(!is.na(unique(m$unit_number_informationfilter))){
    tst = ifelse(m$date>m$unit_number_informationfilter,1,0)
    rnd = min(m$roundnumber[tst==1])
    return(data.frame(filterchange = unique(m$unit_number_informationfilter), freshround = rnd))
  } else {
    return(data.frame(filterchange=NA, freshround=NA))
  }
})

bl2 = bl %>% dplyr::filter(!is.na(freshround), freshround<Inf)
freshdat = list()
for(i in 1:nrow(bl2)){
  freshdat[[i]] <- dat[dat$unitnumber==bl2[i,'unitnumber'] & dat$roundnumber == bl2[i,'freshround'],]
}
freshdat = do.call(rbind, freshdat)
freshdat <- plyr::ddply(freshdat, ~unitnumber, function(m) m[m$date==min(m$date),])

dirtydat = list()
for (i in 1:nrow(freshdat)){
  dirtydat[[i]] <- dat[dat$unitnumber==freshdat[i,'unitnumber'] & dat$roundnumber == freshdat[i,'roundnumber']-1,]
}
dirtydat = do.call(rbind, dirtydat)
freshdat2 = freshdat[freshdat$unitnumber %in% dirtydat$unitnumber,]

## Create indicator for which rows are the first test post filter change.



# keep unit 51 round 7 location BUILDING 10/11TH FLOOR
#

library(reshape2)
dat_expt = dat %>% dplyr::filter(unitnumber!=2, unitnumber!=3)
blah = dat_expt %>% select(ends_with('velocity'),mfg)
blah = blah[,-1]
blah2 = melt(blah, id.vars = c('mfg','middlecentervelocity'))
ggplot(blah2, aes(x=middlecentervelocity, y=value))+geom_point()+geom_smooth()+geom_abline(color='red')+facet_wrap(~variable, nrow=2)

ggplot(blah2,
       aes(x=(middlecentervelocity+value)/2, y=(value-middlecentervelocity)))+
  geom_point()+
  geom_smooth()+
  geom_hline(yintercept=0, color='red')+
  facet_wrap(~variable, nrow=2)+
  labs(x='Average velocity', y = 'Difference from middlecenter')

# Capture effectiveness

ce <- dat_expt %>% select(starts_with('effect'), -effectless90, roundnumber, unitnumber)
ce1 <- ce %>% select(-roundnumber, -unitnumber) %>% mutate_each(funs(ifelse(.>99.97,1,0)))
no_of_cells_pass <- apply(ce1[,2:10],1,sum)

cutoff = list()
for(cr in c(90,95, 97.5, 99, 99.5, 99.7, 99.9, 99.95, 99.97)){
  cutoff = c(cutoff, c(cr, 100*mean(ce$effect>=cr, na.rm=T)))
}

cutoff = data.frame(matrix(do.call(c, cutoff), ncol=2, byrow=T))
names(cutoff) <- c('Threshold','Percent_passing')
ggplot(cutoff, aes(Threshold, Percent_passing))+
  geom_line()+
  labs(y='Percent passing')

passing = matrix(NA, nrow=84, ncol=9)
units = sort(unique(dat_expt$unitnumber))
for(i in 1:nrow(dat_expt)){
  unit_index = match(dat_expt$unitnumber[i], units)
  passing[unit_index, dat_expt$roundnumber[i]] <- ifelse(dat_expt$effect[i]>99.97, 1, 0)
}

## How many sectors can fail
## How much can they fail by
## Which ones can fail

n <- dat_expt %>% select(starts_with('effectivenessquad')) %>% names() %>% sort()
corrs <- dat_expt[,n] %>% cor(use='pair') %>% round(digit=2)
image(corrs)

dat_effective <- dat_expt %>% select(starts_with('effectiveness'))
dat_effective <- dat_effective[,sort(names(dat_effective))]

meet_criterion <- function(x, crit=99.97){
  x %>% mutate_each(funs(ifelse(.>crit, 1, 0)))
}

crit_levels = c(90,95, 97.5, 99, 99.5, 99.7, 99.9, 99.95, 99.97)
out = list()
for(cr in crit_levels){
  cr_dat <- meet_criterion(dat_effective, crit=cr)
  bl = melt(cr_dat, id.var=1)
  out[[as.character(cr)]] <- bl %>% dplyr::filter(effectiveness==1) %>% group_by(variable) %>%
    summarise(prop_fail = 100*mean(1-value, na.rm=T))
}

blah = plyr::ldply(out) %>% mutate(crit = as.numeric(.id))
ggplot(blah, aes(x=crit, y = prop_fail))+geom_line()+
  facet_wrap(~variable, nrow=3)+
  labs(x = 'Failure criterion', y = "Proportion of time sector fails when overall passes")

cr_dat <- meet_criterion(dat_effective, crit=99.9)
number_fail <- 9-apply(cr_dat[,-1],1, sum,na.rm=T)
table(number_fail[cr_dat[,1]==1])
apply(cr_dat[cr_dat[,1]==1 & number_fail==3,],1, function(x) which(x==0))-1

library(tidyr)
library(reshape2)
dat_effective <- mutate(dat_effective, ind_effect = ifelse(effectiveness>=99.97,1,0))
howmuch_pass <- dat_effective %>% dplyr::filter(ind_effect==1) %>%
  summarise_each(funs(min = min(., na.rm=T)),
                 contains('quad')) %>% melt %>%
  mutate(quad = gsub('_min','',variable),
         min = value) %>%
  select(-variable,-value)

dat_effective$pos <- apply(dat_effective[,2:10], 1, function(x) ifelse(any(x<=0),0,1))
d <- dat_effective %>% mutate(id = 1:nrow(.)) %>% dplyr::filter(ind_effect==0, pos==1)
rng <- data.frame(t(apply(d[,2:10],1,range))); names(rng) <- c('min','max')
rng$id <- 1:nrow(rng)

plt <- ggplot(rng, aes(x=id, ymin=min, ymax=max))+geom_linerange()+
  geom_hline(yintercept=99.97, linetype=2)
plt
plt+ylim(99.5,100)
plt+ylim(99.9,100)

bl <- dat_effective %>% filter(effectiveness >= 99.97) %>%
  summarise_each(funs(round(100*mean(.>99.97 , na.rm=T),2)), -effectiveness) %>%
  matrix(nrow=3, byrow=T)
bl <- dat_effective %>% filter(effectiveness >= 99.97) %>%
  summarise_each(funs(round(min(. , na.rm=T),2)), -effectiveness) %>%
  matrix(nrow=3, byrow=T)


ind <- apply(dat_effective[,2:10], 1, function(x) all(x>0))
dat_effective <- dat_effective[ind,]

apply(subset(dat_effective, ind_effect==0)[,2:10],1,range)


d2 <- d %>% filter(effectiveness > 99.5 & effectiveness < 99.97)
quad_summary <- function(d2) {
  out <- d2 %>% summarise_each(funs(n = n(),mean = mean(., na.rm=T), median = median(., na.rm=T),
                             min = min(., na.rm=T), max = max(., na.rm=T),
                             Q25 = quantile(.,.25,na.rm=T),
                             Q10 = quantile(.,.1,na.rm=T)), contains('quad'))
  out %>% melt %>% separate(variable,c('quad','stat')) %>%
    mutate(value = round(value,2)) %>% spread(stat,value)
}

quad_pass <- function(d2){
  out <- d2 %>% summarise_each(funs(mean = mean(. > 99.97, na.rm=T)), contains('quad'))
  out %>% melt %>% mutate(value=round(100*value,2)) %>%
    separate(variable,c('quad','stat')) %>% spread(stat,value)
}

out <- quad_summary(d %>% filter(effectiveness > 99.5))
out2 <- quad_summary(d %>% filter(effectiveness > 99.9))

obs_summ <- function(d){
  x <- t(apply(d[,2:10],1, function(x) c(range(x), mean(x), median(x),
                                         quantile(x,0.3),
                                         quantile(x, 0.2),
                                         quantile(x, 0.1))))
  x <- data.frame(x); names(x) <- c('min','max','mean','median','Q3', 'Q2','Q1')
  x <- cbind(x, data.frame('quad5' = d[,6], 'ind'=1:nrow(x)))
  names(x)[8] = 'quad5'
  return(x)
}

o9995 <- obs_summ(d %>% filter(effectiveness>99.95))
o9990 <- obs_summ(d %>% filter(effectiveness>99.9))
o_bad <- obs_summ(d %>% filter(effectiveness<=99.9))


plot_range_effectiveness <- function(d){
  require(ggplot2)
  to_plot <- obs_summ(d)
  plt <- ggplot(to_plot)+
    geom_point(aes(x=ind,y=quad5))+
    geom_linerange(aes(x=ind, ymin=min, ymax=max))+
    geom_hline(yintercept=99.97, linetype=2)+
    labs(x='',y='Effectiveness across sectors')
  return(plt)
}

p1 <- plot_range_effectiveness(d %>% filter(effectiveness > 99.95))
p2 <- plot_range_effectiveness(d %>% filter(effectiveness > 99.9))
p3 <- plot_range_effectiveness(d %>% filter(effectiveness > 99.5))
p4 <- plot_range_effectiveness(d %>% filter(effectiveness > 99.0))

# Two-by-two characteristics
# Accuracy = (TP+TN)/(P+N)
# TPR (= recall = sensitivity) TP/P
# FPR (= fallout = 1- specificity) FP/N

measures_2x2 <- function(d){
  d1 <- as.matrix(d)
  acc <- sum(diag(d1))/sum(d)
  tpr <- d1[2,2]/sum(d1[2,])
  fpr <- d1[1,2]/sum(d1[1,])
  ppv <- d1[2,2]/sum(d1[,2])
  npv <- d1[1,1]/sum(d1[,1])
  return(c(Accuracy = 100*acc, TPR = 100*tpr, FPR = 100*fpr,
           TNR = 100*(1-fpr), PPV = 100*ppv, NPV = 100*npv))
}

measures_2x2(table(dat_effective$effectiveness >= 99.97, dat_effective$effectivenessquad5>=99.97))

number_positive <- apply(meet_criterion(dat_effective) %>% select(contains('quad')),1,sum)

oc <- data.frame(rbind(
measures_2x2(table(dat_effective$effectiveness>=99.97, number_positive>=5)),
measures_2x2(table(dat_effective$effectiveness>=99.97, number_positive>=6)),
measures_2x2(table(dat_effective$effectiveness>=99.97, number_positive>=7)),
measures_2x2(table(dat_effective$effectiveness>=99.97, number_positive>=8)),
measures_2x2(table(dat_effective$effectiveness>=99.97, number_positive>=9))
))
oc <- cbind('Min_pos' = 5:9, oc)

measures_2x2(table(
  dat_effective$ind_effect==1,
  obs_summ(dat_effective)$min > 99.9))

library(ROCR)

p1 <- prediction(obs_summ(dat_effective)$min/101, dat_effective$ind_effect)
perf=performance(p1, 'tpr','fpr')
plot(perf)
opt_cut <- perf@alpha.values[[1]][which.max(perf@y.values[[1]]-perf@x.values[[1]])]
performance(p1,'auc')

measures_2x2(table(
  dat_effective$ind_effect==1,
  obs_summ(dat_effective)$min > opt_cut*100))

measures_2x2(table(
  dat_effective$ind_effect==1,
  obs_summ(dat_effective)$min > opt_cut*100 & dat_effective$effectivenessquad5 > 99.97))

measures_2x2(table(
  dat_effective$ind_effect==1,
  obs_summ(dat_effective)$min > opt_cut*100 & dat_effective$effectivenessquad5>99.97 & number_positive>=7))

## Combined test
tst1 <- obs_summ(dat_effective)$min > opt_cut*100 & dat_effective$effectivenessquad5>=99.97 & number_positive >=7
table(tst1, dat_effective$ind_effect)
measures_2x2(table(dat_effective$ind_effect,tst1))

d_test <- dat_effective %>% filter(effectivenessquad5>=99.97)
d_test$num_pos <- number_positive[dat_effective$effectivenessquad5>=99.97]

measures_2x2(table(
  d_test$ind_effect==1,
  obs_summ(d_test)$min > opt_cut*100))

d_test2 <- d_test[obs_summ(d_test)$min > opt_cut*100,]
measures_2x2(table(
  d_test2$ind_effect==1,
  d_test2$num_pos >=7
))

D <- cbind(dat_effective, num_pos=number_positive)
D$min <- apply(D[,2:10],1,min)
D <- D %>% mutate(ind1=ifelse(effectivenessquad5>=99.97, 1, 0),
                  ind2 = ifelse(min > 98.83,1,0),
                  ind3 = ifelse(num_pos >=7,1,0))
tst = D$ind1
tst[D$ind1==1 & D$ind2==0] <- 0
tst[D$ind1==1 & D$ind2==1 & D$ind3==0] <- 0
measures_2x2(table(D$ind_effect,tst))

## How different is the center sector?

# Are the differences across sectors due to random measurement error?
max_distr = table(apply(D[,2:10],1,order)[9,])
chisq.test(max_distr, p=rep(1/9,9)) # pval < 10E-15

# Is the center sector just better?

x1 = D[,6]
x2 = apply(D[,c(2:5,7:10)],1,mean)
x3 = apply(D[,c(2,4,8,10)],1,mean)
x4 = apply(D[,c(2,4,8,10)],1,max)

t.test(x1,x2,paired=T) # pval 8E-4
t.test(x1,x3,paired=T) # pval 4E-5
t.test(x1,x4,paired=T)

D <- as.data.frame(D)
tests <- list()
for(i in c(2:5,7:10)){
  tests[[i]] <- t.test( D[,i], D[,6],paired=T, conf.int=T)
}
library(broom)
out <- plyr::ldply(tests, tidy) %>% select(estimate, p.value, conf.low, conf.high) %>%
  mutate_at(funs(formatC(., digits=3, format='f')),
            .cols=c('estimate', 'conf.low','conf.high')) %>%
  mutate(p.value = format.pval(p.value, digits=2, eps=0.001)) %>%
  # mutate(p.value = format.pval(p.value, digits=3, eps=0.001)) %>%
  mutate(sector = c(1:4,6:9)) %>%
  select(sector, estimate:conf.high)



######################################################################
library(ggplot2)
library(reshape2)
MAplot <- function(a,b,...){
  d <- data.frame(a,b)
  plt <- ggplot(d, aes(x=(a+b)/2,y=(b-a)))+
    geom_point()+geom_smooth()+
    geom_hline(yintercept = 0)+theme_minimal()
  return(plt)
}
nm = grep('velocity$',names(dat),value=T)
nm <- nm[-1]
veldata <- dat[,nm]
veldata2 <- melt(veldata, id.vars = 'middlecentervelocity')
veldata2$variable <- factor(veldata2$variable,
                           levels = c('upperleftvelocity','uppercentervelocity',
                                      'upperrightvelocity','middleleftvelocity',
                                    'middlerightvelocity',
                                      'lowerleftvelocity','lowercentervelocity','lowerrightvelocity'))

veldata3 <- melt(veldata)
veldata3$middlecenter <- rep(veldata$middlecentervelocity,9)
veldata3$variable <- factor(veldata3$variable,
                            levels = c('upperleftvelocity','uppercentervelocity',
                                       'upperrightvelocity','middleleftvelocity',
                                       'middlecentervelocity','middlerightvelocity',
                                       'lowerleftvelocity','lowercentervelocity',
                                       'lowerrightvelocity'))

ggplot(veldata3, aes(x = (middlecenter+value)/2, y = value- middlecenter))+
  geom_point(shape=20)+geom_smooth()+
  geom_hline(yintercept=0)+theme_minimal()+
  facet_wrap(~variable, nrow=3)+
  labs(x='Average velocity',y='Difference from center sector')

# Updated graph based on new data -----------------------------------------

newdata = readxl::read_excel('Book1.xlsx')
newdata2 <- reshape2::melt(newdata)
newdata2$middlecenter <- rep(newdata$middlecentervelocity,9)
newdata2$variable <- factor(newdata2$variable,
                            levels = c('upperleftvelocity','uppercentervelocity',
                                       'upperrightvelocity','middleleftvelocity',
                                       'middlecentervelocity','middlerightvelocity',
                                       'lowerleftvelocity','lowercentervelocity',
                                       'lowerrightvelocity'))
ggplot(newdata2, aes(x= (middlecenter+value)/2, y = value - middlecenter))+
  geom_point(shape=20)+geom_smooth()+
  geom_hline(yintercept=0)+ theme_minimal()+
  facet_wrap(~variable, nrow=3)+
  labs(x = 'Average velocity', y = 'Difference from center sector')
