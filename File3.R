## Loking at reliability

library(haven)
library(tidyverse)

dat <- read_dta('master jan 6 2017.dta')
dat_study <- dat %>% filter(mfg==6, study3==1) %>%
  mutate(middlecentervolume = middlecentervelocity * 22.5 * 22.5 / 144.0)


dat0 = dat_study %>% filter(post==0) %>%
  select(unitnumber, intakevolume, middlecentervolume, exhaustvolume) %>%
  gather(variable, value, -unitnumber)
dat1 <- dat_study %>% filter(post==1) %>%
  select(unitnumber, balometerintake, balometerexhaust) %>%
  gather(variable, value, -unitnumber)

d <- bind_rows(dat0,dat1)
d %>% nest(-variable) %>%
  mutate(mods = map(data, ~lme(value ~ 1, data=., random = ~1))) -> blah2

d %>% nest(-variable) %>%
  mutate(mods = map(data, ~lmer(value ~ (1|unitnumber), data=.)),
         results = map(mods, ~tidy(.)),
         ci = map(mods, ~as.data.frame(confint(.,2)))) -> blah

d %>% nest(-variable) %>%
  mutate(mods = map(data, ~lmer(value ~ (1|unitnumber), data=.)),
         results = map(mods, ~tidy(.))) %>%
  select(variable, results) %>%
  unnest() %>%
  filter(term=='sd_Observation.Residual') %>% select(variable, estimate) %>%
  left_join(blah %>% select(variable, ci) %>% unnest())


blah %>%
  mutate(results = map(results, ~filter(., term=='sd_Observation.Residual') %>% select(estimate))) %>%
  select(variable, results, ci) %>%
  unnest() -> blah2
blah2 %>% mutate(ci = sprintf('%6.2f (%6.2f, %6.2f)', estimate, `2.5 %`, `97.5 %`))

ggplot(dat0, aes(x=date, y=intakevolume))+geom_point()+geom_smooth()
ggplot(dat1, aes(x=date, y=balometerintake))+geom_point()+geom_smooth()
summary(dat0$date)
summary(dat1$date)

d <- bind_rows(
  dat0 %>% select(date, unitnumber, intakevolume) %>%
    mutate(variable='intakevolume') %>% rename(value=intakevolume),
  dat1 %>% select(date, unitnumber, balometerintake) %>%
    mutate(variable='balometer') %>% rename(value=balometerintake)
)

common_units <- intersect(dat0$unitnumber, dat1$unitnumber)
d1 <- d %>% filter(unitnumber %in% common_units)

ggplot(d, aes(, y = value, group=variable, color=variable))+geom_boxplot()



# For paper ---------------------------------------------------------------

dat %>% filter(study3==1, post==1) %>% count(mfg)
dat %>% filter(study3==1, post==1) %>%
  group_by(mfg) %>%
  summarise('Mean' = mean(balometerexhaust, na.rm=T),
            'SD' = sd(balometerexhaust, na.rm=T)) %>%
  mutate(out = sprintf('%.1f (%.1f)', Mean, SD))
dat %>% filter(study3==1, post==0) %>%
  group_by(mfg) %>%
  summarise('Mean' = mean(intakevolume, na.rm=T),
            'SD' = sd(intakevolume, na.rm=T)) %>%
  mutate(out = sprintf('%.1f (%.1f)', Mean, SD))
dat %>% filter(study3==1, post==0) %>% group_by(mfg) %>%
  summarise('Mean' = mean(exhaustvolume, na.rm=T),
            'SD' = sd(exhaustvolume, na.rm=T)) %>%
  mutate(out = sprintf('%.1f (%.1f)', Mean, SD))

dat %>% filter(study3==1, post==0) %>%
  select(mfg, intakevolume, exhaustvolume) %>%
  gather(variable, value, -mfg) %>%
  mutate(claimed = ifelse(mfg==6, 1975, NA),
         claimed = ifelse(mfg==7, 1000, claimed),
         claimed = ifelse(mfg==9, 2100, claimed)) %>%
  group_by(mfg) %>%
  summarise(M = mean((claimed - value)/claimed*100, na.rm=T),
            S=sd((claimed - value)/claimed*100, na.rm=T)) %>%
  mutate(out = sprintf('%.1f (%.1f)', M, S))

dat %>% filter(study3==1, post==1) %>%
  select(mfg, balometerintake, balometerexhaust) %>%
  gather(variable, value, -mfg) %>%
  mutate(claimed = ifelse(mfg==6, 1975, NA),
         claimed = ifelse(mfg==5, 1900, claimed),
         claimed = ifelse(mfg==9, 2100, claimed)) %>%
  group_by(mfg) %>%
  summarise(M = mean((claimed - value)/claimed*100, na.rm=T),
            S=sd((claimed - value)/claimed*100, na.rm=T)) %>%
  mutate(out = sprintf('%.1f (%.1f)', M, S))



library(forcats)
dat_plt <- rbind(dat0,dat1)
dat_plt <- dat_plt %>%
  filter(variable != 'middlecentervolume') %>%
  mutate(variable = as_factor(variable),
                              variable = fct_recode(variable,
                                                    'Intake (anemometer)' = 'intakevolume', 'Intake (balometer)' = 'balometerintake',
                                                    'Exhaust (anemometer)' = 'exhaustvolume', 'Exhaust (balometer)' = 'balometerexhaust'))
library(cowplot)
plt <- ggplot(dat_plt, aes(x=variable, y=value))+
  geom_boxplot(coef = Inf)+
  geom_hline(yintercept = 1975, linetype=2, color='red')+
  labs(x='', y='CFM')+
  coord_flip()+
  theme(axis.title.x = element_text(size=10),
        axis.text = element_text(size=11),
        axis.ticks = element_blank())

plt2 <- dat_plt %>% group_by(variable) %>%
  summarise('Mean' = mean(value, na.rm=T)) %>%
  ggplot(aes(x=0, y=variable))+
  geom_text(aes(label = sprintf('%.1f', Mean)), size=3)
plt2 <- plt2 +
  labs(x='Mean')+
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_text(size=10),
        axis.line = element_blank())

plt3 <- dat_plt %>% group_by(variable) %>%
  summarise('SD'=sd(value, na.rm=T)) %>%
  ggplot(aes(x=0, y=variable))+
  geom_text(aes(label=sprintf('%.1f',SD)), size=3)
plt3 <- plt3+
  labs(x='Std. Dev.')+
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_text(size=10),
        axis.line = element_blank())

plot_grid(plt,plt2, plt3, nrow=1, rel_widths = c(5,1,1),
          align = 'h')
