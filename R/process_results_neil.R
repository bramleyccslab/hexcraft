library(tidyverse)
library(ggplot2)
library(gridExtra) #For plotting lots of things at once

rm(list=ls())

load('./dat/results_for_neil.rdata')

exclude<-c(87, 100, 107)
problems<-data.frame(id=c('train1','train2','train3','train4','train5','train6','train7','train8','train9',
                          'dabone1','dabone2','dabone3','hazard1','hazard2','hazard3','dinopaw1','dinopaw2','dinopaw3'),
                     challenge=c('A', 'ASW', 'Z', 'X', 'ZX', 'XD', 'ZKKK', 'ZF', 'ASSR',
                                 'XKX','ZXKXW','XKXWWXKXSSR',
                                 'ZSA', 'ZSARK', 'ZSAKZSARKX',
                                 'ZXD', 'ZXDWR', 'ZXDESZXDSR')) 
# THIS IS THE CHRONOLOGICAL ORDER THEY APPEAR IN THE READ IN


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
                  dinopaw1=NA,
                  dabone1=NA,
                  hazard1=NA,
                  dinopaw2=NA,
                  dabone2=NA,
                  hazard2=NA,
                  dinopaw3=NA,
                  dabone3=NA,
                  hazard3=NA,
                  attempts_train1=NA,
                  attempts_train2=NA,
                  attempts_train3=NA,
                  attempts_train4=NA,
                  attempts_train5=NA,
                  attempts_train6=NA,
                  attempts_train7=NA,
                  attempts_train8=NA,
                  attempts_train9=NA,
                  attempts_dinopaw1=NA,
                  attempts_dabone1=NA,
                  attempts_hazard1=NA,
                  attempts_dinopaw2=NA,
                  attempts_dabone2=NA,
                  attempts_hazard2=NA,
                  attempts_dinopaw3=NA,
                  attempts_dabone3=NA,
                  attempts_hazard3=NA,
                  exclude = 1:118 %in% exclude)

# Also need to create a dataframe with a row for every problem for every participant
df.tw<-data.frame()
ls.sw<-ls.sw.all<-ls.whi<-list()

i<-1
for (i in 1:nrow(df.sw))
{
  ls.sw[[i]]<-ls.sw.all[[i]]<-ls.whi[[i]]<-list()
  if (!i%in%exclude)
  {
    this_results<-df_fixed$level[[i]]
    completed<-rep(NA, nrow(problems))
    
    df.sw$condition[i]<-problems$id[which(this_results[which(sapply(this_results, '[[', 'trial')==10)][[1]]$challenge==problems$challenge)]
    
    for (j in 1:nrow(problems))
    {
      ix<-which(sapply(this_results, '[[', 'challenge') == problems$challenge[j])
      completed[j]<-any(sapply(this_results[ix], '[[', 'result'))
      if (length(ix)>0)
      {
        ls.whi[[i]][[j]]<-which(sapply(this_results[ix], '[[', 'result'))[1]
      } else {
        ls.whi[[i]][[j]]<-NA
      }
      df.sw[[problems$id[j]]][i]<-completed[j]
      df.sw[[paste0('attempts_', problems$id[j]) ]][i]<-length(ix)
      
      ls.sw[[i]][[j]]<-df_fixed$timestamps_fixed[[i]][ix]
      ls.sw.all[[i]][[j]] <-data.frame(do.call(rbind.data.frame, df_fixed$timestamps_fixed[[i]][ix]))
    }
    
    # tmp<-ls.sw[[i]][c(11:12,14:15, 17:18)]
    # tmp2<-data.frame(do.call(rbind.data.frame, ls.sw[[i]]))#GOT HERE
    # df.sw$actions[i]<-nrow(tmp2)
      
    df.sw$training_completed[i]<-sum(completed[1:9])
    df.sw$tests_completed[i]<-sum(completed[10:18])
    df.sw$condition_problem_passed[i]<-df.sw[[df.sw$condition[i]]][i]
    
  }
  
}

# EXCLUSIONS
ls.sw<-ls.sw[df.sw$condition_problem_passed & !df.sw$exclude]
ls.sw.all<-ls.sw.all[df.sw$condition_problem_passed & !df.sw$exclude]
ls.whi<-ls.whi[df.sw$condition_problem_passed & !df.sw$exclude]
df.sw <- filter(df.sw, condition_problem_passed & !exclude)

# REMOVE DUPLICATE ATTEMPTS:
for (i in 1:length(ls.sw))
{
  for (j in 1:length(ls.sw[[i]]))
  {
    timestamps<-c()
    if (length(ls.sw[[i]][[j]])>1)
    {
      for (k in 1:length(ls.sw[[i]][[j]]) )
      {
        timestamps<-c(timestamps, ls.sw[[i]][[j]][[k]]$interval[1])
      }
      is_unique<-rep(T, length(timestamps))
      for (l in 1:(length(timestamps)-1))
      {
        is_unique[l+1]<-!timestamps[l] %in% timestamps[l+1:length(timestamps)]
      }
      cat(i,j,k, is_unique, '\n')
      
      ls.sw[[i]][[j]]<-ls.sw[[i]][[j]][is_unique]
      
      df.tw$attempts[df.tw$upi==df.sw$upi[i] & df.tw$problem==problems$id[j]]<-sum(is_unique)

      ls.sw.all[[i]][[j]] <-data.frame(do.call(rbind.data.frame, ls.sw[[i]][[j]]))
    }
    
  }
}


#
c(mean(df.sw$dabone1),
  sum(df.sw$dabone2),
  sum(df.sw$dabone3),
  sum(df.sw$hazard1),
  sum(df.sw$hazard2),
  sum(df.sw$hazard3),
  sum(df.sw$dinopaw1),
  sum(df.sw$dinopaw2),
  sum(df.sw$dinopaw3))

df.sw %>% group_by(condition) %>% summarise_all('mean', na.rm=T)

# CREATE TRIALWISE DATAFRAME
tmp<-df.sw %>% gather(problem, attempts, attempts_dinopaw1:attempts_hazard3)
df.tw <- df.sw %>% gather(problem, correct, dinopaw1:hazard3) %>% 
  mutate(attempts_official = tmp$attempts,
         attempts_timestamps = NA,
         problem = factor(problem, levels = c("dinopaw1", "dabone1", "hazard1",
                                              "dinopaw2", "dabone2", "hazard2",
                                              "dinopaw3", "dabone3", "hazard3")),
         condition = factor(condition, levels = c('dinopaw1','dabone1','hazard1'),
                            labels = c('dinopaw', 'dabone','hazard'))) %>%
  select(c(1:6, 34:39)) %>%
  arrange(upi, condition)
df.tw$locks<-''
for (t in 1:nrow(df.tw))
{
  i<-which(df.sw$upi==df.tw$upi[t])
  j<-which(problems$id==df.tw$problem[t])
  df.tw$actions[t]<-paste0(ls.sw.all[[i]][[j]]$key, collapse = '')
  df.tw$locks[t]<-paste0(rep('0', nchar(df.tw$actions[t])), collapse = '')
  
  # These are the true breakpoints
  six<-sapply(ls.sw[[i]][[j]], nrow)
  
  df.tw$attempts_timestamp[t]<-length(six)
  
  if (length(six)>0)
  {
    for (s in 1:length(six))
    {
      
      str_sub(df.tw$locks[t], sum(six[1:s]), sum(six[1:s]))<-'1'
    }
  }


  # for (k in 1:length(ls.sw.all[[i]][[j]]))
  # {
  #   ls.sw.all[[i]][[j]][[k]]
  # }
  
}

save(file='./dat/results_processed_neil.rdata', df.sw, df.tw, ls.sw, ls.sw.all, ls.whi)



# REMOVE THE ROWS FOR THE MANIPULATION PROBLEM TO FOCUS JUST ON THE TEST PROBLEM
df.tw<-df.tw %>% filter(!(condition=='dabone' & problem=='hazard1') & !(condition=='dabone' & problem=='dinopaw1') &
                          !(condition=='hazard' & problem=='dabone1') & !(condition=='hazard' & problem=='dinopaw1') &
                          !(condition=='dinopaw' & problem=='dabone1') & !(condition=='dinopaw' & problem=='hazard1') )



# Should just count how many T for each problem, condition combo
ggplot(df.tw %>% filter(condition!='NA' & !problem%in%c('dinopaw1','dabone1','hazard1')), aes(as.numeric(correct), x=problem)) +
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
  gs<-6
  ts<-list()
  ts$mix<-c(sample(which(df.sw$condition=='dabone1'), floor(gs/3)),
         sample(which(df.sw$condition=='hazard1'), floor(gs/3)),
         sample(which(df.sw$condition=='dinopaw1'), floor(gs/3)))
  ts$mix2<-c(sample(which(df.sw$condition=='hazard1'), floor(gs/2)),
            sample(which(df.sw$condition=='dinopaw1'), floor(gs/2)))
  ts$dabone<-sample(which(df.sw$condition=='dabone1'), gs)
  ts$hazard<-sample(which(df.sw$condition=='hazard1'), gs)
  ts$dinopaw<-sample(which(df.sw$condition=='dinopaw1'), gs)
  
  ts$mixhit<-any(df.sw$dabone2[ts$mix])+any(df.sw$dabone3[ts$mix])+
    any(df.sw$hazard2[ts$mix])+any(df.sw$hazard3[ts$mix])+
    any(df.sw$dinopaw2[ts$mix])+any(df.sw$dinopaw3[ts$mix])
  ts$mix2hit<-any(df.sw$dabone2[ts$mix2])+any(df.sw$dabone3[ts$mix2])+
    any(df.sw$hazard2[ts$mix2])+any(df.sw$hazard3[ts$mix2])+
    any(df.sw$dinopaw2[ts$mix2])+any(df.sw$dinopaw3[ts$mix2])
  ts$dabonehit<-any(df.sw$dabone2[ts$dabone])+any(df.sw$dabone3[ts$dabone])+
    any(df.sw$hazard2[ts$dabone])+any(df.sw$hazard3[ts$dabone])+
    any(df.sw$dinopaw2[ts$dabone])+any(df.sw$dinopaw3[ts$dabone])
  ts$hazardhit<-any(df.sw$dabone2[ts$hazard])+any(df.sw$dabone3[ts$hazard])+
    any(df.sw$hazard2[ts$hazard])+any(df.sw$hazard3[ts$hazard])+
    any(df.sw$dinopaw2[ts$hazard])+any(df.sw$dinopaw3[ts$hazard])
  ts$dinopawhit<-any(df.sw$dabone2[ts$dinopaw])+any(df.sw$dabone3[ts$dinopaw])+
    any(df.sw$hazard2[ts$dinopaw])+any(df.sw$hazard3[ts$dinopaw])+
    any(df.sw$dinopaw2[ts$dinopaw])+any(df.sw$dinopaw3[ts$dinopaw])
  
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
