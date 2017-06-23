library(haven)
library(tidyverse)
dat <- read_dta('master jan 6 2017.dta')

# is.Date <- function(x){
#   is(x, 'Date')
# }
# for(i in 1:ncol(dat)){
#   if(is(dat[[i]],'Date')) dat[[i]] = as.Date(as.character(dat[[i]]))
#   if(is.numeric(dat[[i]])) dat[[i]] = as.numeric(dat[[i]])
#   if(is.integer(dat[[i]])) dat[[i]] = as.numeric(dat[[i]])
# }


study_dat <- dat %>%
  filter(study3==1) %>%
  filter(!(unitnumber %in% c(2,3))) %>%
  select(unitnumber, effect, matches('^effect[1-9]'))

study_dat %>% filter(effect >= 99.97) %>% summarise_each(funs(mins=min(., na.rm=T), perc = mean(.>=99.97)*100, n_perc = sum(.>=99.97)), matches('^effect[1-9]')) %>% gather(variable, value) %>% separate(variable, c('effect', 'fn')) %>% spread(effect,value)

bl = study_dat %>% filter(effect>=99.97) %>% select(matches('^effect[1-9]'))
cumsum(table(9-apply(bl>=99.97,1,sum)))

study_dat %>% filter(effect >=99.95, effect <99.97) %>% summarise_each(funs(mins=min(., na.rm=T), perc = mean(.>=99.97)*100, n_perc = sum(.>=99.97)), matches('^effect[1-9]')) %>% gather(variable, value) %>% separate(variable, c('effect', 'fn')) %>% spread(effect,value)

study_dat %>% filter(effect >=99.9, effect <99.97) %>% summarise_each(funs(mins=min(., na.rm=T), perc = mean(.>=99.97)*100, n_perc = sum(.>=99.97)), matches('^effect[1-9]')) %>% gather(variable, value) %>% separate(variable, c('effect', 'fn')) %>% spread(effect,value)

study_dat %>% filter(effect >=99, effect <99.9) %>% summarise_each(funs(mins=min(., na.rm=T), perc = mean(.>=99.97)*100, n_perc = sum(.>=99.97)), matches('^effect[1-9]')) %>% gather(variable, value) %>% separate(variable, c('effect', 'fn')) %>% spread(effect,value)


bl <- study_dat %>% filter(effect >= 99.95, effect < 99.97) %>% select(matches('^effect[1-9]'))
cumsum(table(9-apply(bl>=99.97,1,sum)))
bl <- study_dat %>% filter(effect >= 99.9, effect < 99.97) %>% select(matches('^effect[1-9]'))
cumsum(table(9-apply(bl>=99.97,1,sum)))
bl <- study_dat %>% filter(effect >= 99, effect < 99.9) %>% select(matches('^effect[1-9]'))
cumsum(table(9-apply(bl>=99.97,1,sum)))


effects = study_dat %>% select(matches('^effect[1-9]'))
blah=as_data_frame(apply(t(apply(as_data_frame(-effects),1, rank, ties.method='min')),2, table))
blah$rank = 1:9
blah %>% gather(variable, value, -rank) %>% ggplot(aes(factor(variable), value))+geom_bar(stat='identity', aes(fill=factor(rank)))

ind <- apply(effects,1, function(x) all(x>95))

study_dat %>% filter(ind) %>%  select(matches('^effect[1-9]')) %>%
  gather(sector, value, -effect5) %>%
  bind_rows(data.frame(effect5 = study_dat[ind,]$effect5,
                       sector = 'effect5',
                       value = study_dat[ind,]$effect5)) %>%
  ggplot(aes(x=(effect5+value)/2, y = value-effect5))+
  geom_point()+ geom_smooth(se=F)+
  facet_wrap(~sector, nrow=3)


## How many sectors can fail
## How much can they fail by
## Which ones can fail

meet_criterion <- function(x, crit=99.97){
  x %>% mutate_each(funs(ifelse(.>crit, 1, 0)))
}

# crit_levels = c(90,95, 97.5, 99, 99.5, 99.7, 99.9, 99.95, 99.97)
# out = list()
# for(cr in crit_levels){
#   cr_dat <- meet_criterion(effects, crit=cr)
#   bl = reshape2::melt(cr_dat, id.var=1)
#   out[[as.character(cr)]] <- bl %>% dplyr::filter(effect >= 99.97) %>% group_by(variable) %>%
#     summarise(prop_fail = 100*mean(1-value, na.rm=T))
# }
#
# blah = plyr::ldply(out) %>% mutate(crit = as.numeric(.id))
# ggplot(blah, aes(x=crit, y = prop_fail))+geom_line()+
#   facet_wrap(~variable, nrow=3)+
#   labs(x = 'Failure criterion', y = "Proportion of time sector fails when overall passes")
#
# cr_dat <- meet_criterion(dat_effective, crit=99.9)
# number_fail <- 9-apply(cr_dat[,-1],1, sum,na.rm=T)
# table(number_fail[cr_dat[,1]==1])
# apply(cr_dat[cr_dat[,1]==1 & number_fail==3,],1, function(x) which(x==0))-1

library(tidyverse)
study_dat<- mutate(study_dat, ind_effect = ifelse(effect>=99.97,1,0))
howmuch_pass <-study_dat %>% dplyr::filter(ind_effect==1) %>%
  summarise_each(funs(min = min(., na.rm=T)),
                 matches('^effect[1-9]')) %>%
  gather(variable, value) %>%
  separate(variable, c('sector','fn')) %>%
  select(-fn)


obs_summ <- function(d){
  x <- t(apply(d[,3:11],1, function(x) c(range(x), mean(x), median(x),
                                         quantile(x,0.3),
                                         quantile(x, 0.2),
                                         quantile(x, 0.1))))
  x <- data.frame(x); names(x) <- c('min','max','mean','median','Q3', 'Q2','Q1')
  x <- cbind(x, data.frame('quad5' = d[,7], 'ind'=1:nrow(x)))
  names(x)[8] = 'quad5'
  return(x)
}

o9995 <- obs_summ(study_dat %>% filter(effect>99.95))
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

p1 <- plot_range_effectiveness(study_dat %>% filter(effect >= 99.95 & effect< 99.97))
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

number_positive <- apply(meet_criterion(effects) ,1,sum)

oc <- data.frame(rbind(
measures_2x2(table(study_dat$effect >=99.97, number_positive>=5)),
measures_2x2(table(study_dat$effect>=99.97, number_positive>=6)),
measures_2x2(table(study_dat$effect>=99.97, number_positive>=7)),
measures_2x2(table(study_dat$effect>=99.97, number_positive>=8)),
measures_2x2(table(study_dat$effect>=99.97, number_positive>=9))
))
oc <- cbind('Min_pos' = 5:9, oc)

measures_2x2(table(
  dat_effective$ind_effect==1,
  obs_summ(dat_effective)$min > 99.9))

library(ROCR)

p1 <- prediction(obs_summ(study_dat)$min/101, study_dat$ind_effect)
perf=performance(p1, 'tpr','fpr')
plot(perf)
opt_cut <- perf@alpha.values[[1]][which.max(perf@y.values[[1]]-perf@x.values[[1]])] # 98.85
performance(p1,'auc')

measures_2x2(table(
  study_dat$ind_effect==1,
  obs_summ(study_dat)$min > .9983*100))

PPV <- rep(0,1001)
NPV <- rep(0,1001)
for(i in 1:1002){
  if(i %% 100 == 0) print(i)
  m = measures_2x2(table(
    study_dat$ind_effect==1,
    obs_summ(study_dat)$min > opt_cut*100))
  PPV[i] <- m['PPV']
  NPV[i] <- m['NPV']
}

measures_2x2(table(
  study_dat$ind_effect==1,
  obs_summ(dat_effective)$min > opt_cut*100 & dat_effective$effectivenessquad5 > 99.97))

measures_2x2(table(
  dat_effective$ind_effect==1,
  obs_summ(dat_effective)$min > opt_cut*100 & dat_effective$effectivenessquad5>99.97 & number_positive>=7))

## Combined test
mins = obs_summ(study_dat)$min
tst = ifelse(study_dat$effect5 >= 99.97, 1, 0)
tst <- ifelse(tst==1 & mins > 0.998*100, 1,0)
tst <- ifelse(tst==1 & number_positive >=7, 1,0)
tst <- ifelse(tst)
tst1 <- obs_summ(study_dat)$min > .995*100 &
  study_dat$effect5 >= 99.97# & number_positive >=7
table(tst1, study_dat$ind_effect)
measures_2x2(table(study_dat$ind_effect,tst))

d_test <- dat_effective %>% filter(effectivenessquad5>=99.97)
d_test$num_pos <- number_positive[dat_effective$effectivenessquad5>=99.97]

measures_2x2(table(
 study_dat$ind_effect==1,
  obs_summ(study_dat)$min > .998*100 & study_dat$effect5>99.97))

measures_2x2(table(study_dat$ind_effect, number_positive>6))

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
