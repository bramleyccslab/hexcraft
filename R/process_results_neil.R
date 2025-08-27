library(tidyverse)
library(ggplot2)

rm(list=ls())

load('for_neil.rdata')

exclude<-c(87, 100, 107)
problems<-data.frame(id=c('train1','train2','train3','train4','train5','train6','train7','train8','train9',
                          'dabone1','dabone2','dabone3','hazard1','hazard2','hazard3','dinopaw1','dinopaw2','dinopaw3'),
                     challenge=c('A', 'ASW', 'Z', 'X', 'ZX', 'XD', 'ZKKK', 'ZF', 'ASSR',
                                 'XKX','ZXKXW','XKXWWXKXSSR',
                                 'ZSA', 'ZSARK', 'ZSAKZSARKX',
                                 'ZXD', 'ZXDWR', 'ZXDESZXDSR'))

# var tutorial = ['A', 'ASW', 'Z', 'X', 'ZX', 'XD', 'ZKKK', 'ZF', 'ASSR'];
# var dabone = ['XKX','ZXKXW','XKXWWXKXSSR'];
# var hazard = ['ZSA', 'ZSARK', 'ZSAKZSARKX'];
# var dinopaw = ['ZXD', 'ZXDWR', 'ZXDESZXDSR'];

df.sw<-data.frame(upi = sapply(df_fixed$demographics, '[[', 1), 
                  gender = sapply(df_fixed$demographics, '[[', 2),
                  age =sapply(df_fixed$demographics, '[[', 3),
                  condition = NA,
                  training_completed = NA,
                  tests_completed = NA,
                  train1=NA,
                  train2=NA,
                  train3=NA,
                  train4=NA,
                  train5=NA,
                  train6=NA,
                  train7=NA,
                  train8=NA,
                  train9=NA,
                  dabone1=NA,
                  dabone2=NA,
                  dabone3=NA,
                  hazard1=NA,
                  hazard2=NA,
                  hazard3=NA,
                  dinopaw1=NA,
                  dinopaw2=NA,
                  dinopaw3=NA)

# Also need to create a dataframe with a row for every problem for every participant
df.tw<-data.frame()
ls.sw<-list()

i<-1
for (i in 1:nrow(df.sw))
{
  ls.sw[[i]]<-list()
  if (!i%in%exclude)
  {
    this_results<-df_fixed$level[[i]]
    completed<-rep(NA, nrow(problems))
    
    df.sw$condition[i]<-problems$id[which(this_results[which(sapply(this_results, '[[', 'trial')==10)][[1]]$challenge==problems$challenge)]
    
    for (j in 1:nrow(problems))
    {
      ix<-which(sapply(this_results, '[[', 'challenge')==problems$challenge[j])
      completed[j]<-any(sapply(this_results[ix], '[[', 'result'))
      if (completed[j])
      {
        df.sw[[ problems$id[j] ]][i]<-length(ix)
      }
      
      ls.sw[[i]][[j]]<-df_fixed$timestamps_fixed[[i]][ix]
    }
    
    tmp<-data.frame(do.call(rbind.data.frame, ls.sw[[i]][c(11:12,14:25, 17:18)]))#GOT HERE
    df.sw$actions[i]<-nrow(tmp)
      
    df.sw$training_completed[i]<-sum(completed[1:9])
    df.sw$tests_completed[i]<-sum(completed[10:18])
    df.sw$condition_problem_passed[i]<-!is.na(df.sw[[df.sw$condition[i]]][i])
    
  }
  
}

df.sw<-df.sw %>%filter(condition_problem_passed)


c(mean(!is.na(df.sw$dabone1)),
  sum(!is.na(df.sw$dabone2)),
  sum(!is.na(df.sw$dabone3)),
  sum(!is.na(df.sw$hazard1)),
  sum(!is.na(df.sw$hazard2)),
  sum(!is.na(df.sw$hazard3)),
  sum(!is.na(df.sw$dinopaw1)),
  sum(!is.na(df.sw$dinopaw2)),
  sum(!is.na(df.sw$dinopaw3)))

df.sw %>% group_by(condition) %>% summarise_all('mean', na.rm=T)

# CREATE TRIALWISE DATAFRAME
df.tw <- df.sw %>% gather(problem, attempts, dabone1:dinopaw3) %>% 
  mutate(problem = factor(problem, levels = c("train1",   "train2",   "train3",   "train4",   "train5",   "train6",   "train7",   "train8", "train9", 
                                              "dinopaw1", "dabone1", "hazard1", "dinopaw2", "dabone2", "hazard2", "dinopaw3", "dabone3", "hazard3")),
         condition = factor(condition, levels = c('dinopaw1','dabone1','hazard1'), labels = c('dinopaw', 'dabone','hazard')),
         correct = !is.na(attempts)) %>% 
  arrange(upi, condition)

df.tw<-df.tw %>% filter(!(condition=='dabone' & problem=='hazard1') & !(condition=='dabone' & problem=='dinopaw1') &
                          !(condition=='hazard' & problem=='dabone1') & !(condition=='hazard' & problem=='dinopaw1') &
                          !(condition=='dinopaw' & problem=='dabone1') & !(condition=='dinopaw' & problem=='hazard1') )

# Should just count how many T for each problem, condition combo
ggplot(df.tw %>% filter(condition!='NA' & !problem%in%c('dabone1','hazard1','dinopaw1')), aes(as.numeric(correct), x=problem)) +
  geom_bar(stat='identity') +
  facet_wrap(~ condition) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

# WHAT WE WERE TRYING TO PLOT
df.tw %>% group_by(condition, problem)%>% summarise(sc=sum(correct))

n_runs<-10000

df.sim<-data.frame(mixhit=rep(NA, n_runs),
                   mix2hit=rep(NA, n_runs),
                   dabonehit=rep(NA, n_runs),
                   hazardhit=rep(NA, n_runs),
                   dinopawhit=rep(NA, n_runs),
                   gs=rep(NA, n_runs),
                   n=rep(NA, n_runs))
for (n in 1:n_runs)
{
  gs<-12
  ts<-list()
  ts$mix<-c(sample(which(df.sw$condition=='dabone1'), floor(gs/3)),
         sample(which(df.sw$condition=='hazard1'), floor(gs/3)),
         sample(which(df.sw$condition=='dinopaw1'), floor(gs/3)))
  ts$mix2<-c(sample(which(df.sw$condition=='hazard1'), floor(gs/2)),
            sample(which(df.sw$condition=='dinopaw1'), floor(gs/2)))
  ts$dabone<-sample(which(df.sw$condition=='dabone1'), gs)
  ts$hazard<-sample(which(df.sw$condition=='hazard1'), gs)
  ts$dinopaw<-sample(which(df.sw$condition=='dinopaw1'), gs)
  
  ts$mixhit<-any(!is.na(df.sw$dabone2[ts$mix]))+any(!is.na(df.sw$dabone3[ts$mix]))+
    any(!is.na(df.sw$hazard2[ts$mix]))+any(!is.na(df.sw$hazard3[ts$mix]))+
    any(!is.na(df.sw$dinopaw2[ts$mix]))+any(!is.na(df.sw$dinopaw3[ts$mix]))
  ts$mix2hit<-any(!is.na(df.sw$dabone2[ts$mix2]))+any(!is.na(df.sw$dabone3[ts$mix2]))+
    any(!is.na(df.sw$hazard2[ts$mix2]))+any(!is.na(df.sw$hazard3[ts$mix2]))+
    any(!is.na(df.sw$dinopaw2[ts$mix2]))+any(!is.na(df.sw$dinopaw3[ts$mix2]))
  ts$dabonehit<-any(!is.na(df.sw$dabone2[ts$dabone]))+any(!is.na(df.sw$dabone3[ts$dabone]))+
    any(!is.na(df.sw$hazard2[ts$dabone]))+any(!is.na(df.sw$hazard3[ts$dabone]))+
    any(!is.na(df.sw$dinopaw2[ts$dabone]))+any(!is.na(df.sw$dinopaw3[ts$dabone]))
  ts$hazardhit<-any(!is.na(df.sw$dabone2[ts$hazard]))+any(!is.na(df.sw$dabone3[ts$hazard]))+
    any(!is.na(df.sw$hazard2[ts$hazard]))+any(!is.na(df.sw$hazard3[ts$hazard]))+
    any(!is.na(df.sw$dinopaw2[ts$hazard]))+any(!is.na(df.sw$dinopaw3[ts$hazard]))
  ts$dinopawhit<-any(!is.na(df.sw$dabone2[ts$dinopaw]))+any(!is.na(df.sw$dabone3[ts$dinopaw]))+
    any(!is.na(df.sw$hazard2[ts$dinopaw]))+any(!is.na(df.sw$hazard3[ts$dinopaw]))+
    any(!is.na(df.sw$dinopaw2[ts$dinopaw]))+any(!is.na(df.sw$dinopaw3[ts$dinopaw]))
  
  df.sim$mixhit[n]<-ts$mixhit
  df.sim$mix2hit[n]<-ts$mix2hit
  df.sim$dabonehit[n]<-ts$dabonehit
  df.sim$hazardhit[n]<-ts$hazardhit
  df.sim$dinopawhit[n]<-ts$dinopawhit
}

df.sim.l<-gather(df.sim, key, val, mixhit:dinopawhit)
ggplot(df.sim.l, aes(y=val, x=key, fill=key)) +
  geom_violin() +
  stat_summary(geom='point', fun.y = 'mean')
