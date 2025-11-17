library(RMySQL)
library(rjson)
library(tidyverse)

rm(list=ls())

#ssh -L 1111:127.0.0.1:3306 nbramley@eco.ppls.ed.ac.uk
# Password:your_eco_password

mydb = dbConnect(RMySQL::MySQL(),
                 user='eco_mdb_root',
                 password='KZrQupBmNL',
                 dbname='eco_mdb1',
                 host='127.0.0.1',
                 port=1111,
                 table='hexcraft_exp2')


dbListTables(mydb)
df.raw<-dbReadTable(mydb,"hexcraft_exp2")

df.raw<-df.raw[c(12:21),]

vec<-rep(NA, nrow(df.raw))
vecF<-rep(F, nrow(df.raw))
vec0<-rep(0, nrow(df.raw))
df.sw<-data.frame(p_ix=vec,
                  prolific_id=vec,
                  condition=vec,
                  gender=vec,
                  age=vec,
                  ins_time=vec,tut_time=vec,trial_time=vec,total_time=vec,
                  comprehension_score=vec, tutorial_tries=vec, tutorial_keys = vec,
                  c1t1=vecF,c1t2=vecF,c1t3=vecF,
                  c2t1=vecF,c2t2=vecF,c2t3=vecF,
                  test1=vec,test2=vec,test3=vec,test4=vec,test5=vec,
                  c1t1_tries=vec0,c1t2_tries=vec0,c1t3_tries=vec0,
                  c2t1_tries=vec0,c2t2_tries=vec0,c2t3_tries=vec0,
                  test1_tries=vec, test2_tries=vec,test3_tries=vec,test4_tries=vec,test5_tries=vec,
                  total=vec,
                  m_steps=vec,m_steps_successes=vec, m_steps_failures=vec,
                  n_add=vec,n_delete=vec,n_corner=vec,n_bar=vec,n_move=vec,n_rotate=vec,n_flip=vec,n_reflect=vec,
                  comments=vec)

df.tw<-data.frame(p_ix=c(),
                  prolific_id=c(),
                  condition=c(),
                  trial=c(),
                  attempt=c(),
                  actions=c(),
                  success=c())

ls.states<-list()

for (ppt in 1:nrow(df.raw))
{
  ls.p<-fromJSON(df.raw$results[[ppt]])
  df.sw$p_ix[ppt]<-ppt
  df.sw$prolific_id[ppt]<-ls.p$subjectwise$prolific_id
  df.sw$condition[ppt]<-ls.p$subjectwise$condition
  df.sw$gender[ppt]<-ls.p$subjectwise$gender
  df.sw$age[ppt]<-ls.p$subjectwise$age
  
  df.sw$ins_time[ppt]<-round((ls.p$subjectwise$timings$tutorial-ls.p$subjectwise$timings$start)/(1000*60), digits = 2)
  df.sw$tut_time[ppt]<-round((ls.p$subjectwise$timings$test-ls.p$subjectwise$timings$tutorial)/(1000*60), digits = 2)
  df.sw$trial_time[ppt]<-round((ls.p$subjectwise$timings$end-ls.p$subjectwise$timings$test)/(1000*60), digits = 2)
  df.sw$total_time[ppt]<-round((ls.p$subjectwise$timings$end-ls.p$subjectwise$timings$start)/(1000*60), digits = 2)
  
  df.sw$comprehension_score[ppt]<-sum(ls.p$subjectwise$comprehension)
  
  for (trial in 1:8)
  {
    if (trial<4)
    {
      df.sw[[paste0('c', ls.p$subjectwise$condition+1, 't',trial ,sep='')]][ppt]<-any(sapply(ls.p$trials[[trial]], '[[', 'result'))
      df.sw[[paste0('c', ls.p$subjectwise$condition+1, 't',trial,'_tries',sep='')]][ppt]<-length(ls.p$trials[[trial]])
    } else {
      df.sw[[paste0('test',trial-3 ,sep='')]][ppt]<-any(sapply(ls.p$trials[[trial]], '[[', 'result'))
      df.sw[[paste0('test',trial-3,'_tries',sep='')]][ppt]<-length(ls.p$trials[[trial]])
    }

    
    for (attempt in 1:length(ls.p$trials[[trial]]))
    {
      success<-F
      if (!is.null(ls.p$trials[[trial]][[attempt]]$result))
      {
        success<-ls.p$trials[[trial]][[attempt]]$result
      }
      
      df.tw<-rbind(df.tw,
                   data.frame(p_ix=ppt,
                              prolific_id=ls.p$subjectwise$prolific_id,
                              condition=ls.p$subjectwise$condition,
                              trial=trial,
                              attempt=attempt,
                              actions=paste0(ls.p$trials[[trial]][[attempt]]$actions, collapse=','),
                              keycodes=paste0(sapply(ls.p$trials[[trial]][[attempt]]$keys, '[[', 'key'), collapse=''),
                              success=success))
    }
    
  }
  df.sw$comments[ppt]<-ls.p$subjectwise$comments
  df.sw$n_add[ppt]<-str_count(paste0(df.tw$keycodes[df.tw$p_ix==df.sw$p_ix[ppt]], collapse=''),'A')
  df.sw$n_delete[ppt]<-str_count(paste0(df.tw$keycodes[df.tw$p_ix==df.sw$p_ix[ppt]], collapse=''),'D')
  df.sw$n_corner[ppt]<-str_count(paste0(df.tw$keycodes[df.tw$p_ix==df.sw$p_ix[ppt]], collapse=''),'Z')
  df.sw$n_bar[ppt]<-str_count(paste0(df.tw$keycodes[df.tw$p_ix==df.sw$p_ix[ppt]], collapse=''),'X')
  df.sw$n_move[ppt]<-str_count(paste0(df.tw$keycodes[df.tw$p_ix==df.sw$p_ix[ppt]], collapse=''),'W')+
    str_count(paste0(df.tw$keycodes[df.tw$p_ix==df.sw$p_ix[ppt]], collapse=''),'E')+
    str_count(paste0(df.tw$keycodes[df.tw$p_ix==df.sw$p_ix[ppt]], collapse=''),'S')
  df.sw$n_rotate[ppt]<-str_count(paste0(df.tw$keycodes[df.tw$p_ix==df.sw$p_ix[ppt]], collapse=''),'R')
  df.sw$n_flip[ppt]<-str_count(paste0(df.tw$keycodes[df.tw$p_ix==df.sw$p_ix[ppt]], collapse=''),'F')
  df.sw$n_reflect[ppt]<-str_count(paste0(df.tw$keycodes[df.tw$p_ix==df.sw$p_ix[ppt]], collapse=''),'K')
  df.sw$m_steps[ppt]<-mean(nchar(paste0(df.tw$keycodes[df.tw$p_ix==df.sw$p_ix[ppt]])))
  
  df.sw$m_steps_successes[ppt]<-mean(nchar(paste0(df.tw$keycodes[df.tw$p_ix==df.sw$p_ix[ppt]]))[df.tw$success[df.tw$p_ix==df.sw$p_ix[ppt]]])
  df.sw$m_steps_failures[ppt]<-mean(nchar(paste0(df.tw$keycodes[df.tw$p_ix==df.sw$p_ix[ppt]]))[!df.tw$success[df.tw$p_ix==df.sw$p_ix[ppt]]])
  
  
  # And let's extract the larger and less important items:
  ls.states[[ppt]]<-list()
  for (trial in 1:length(ls.p$state_history_tests))
  {
    ls.states[[ppt]][[trial]]<-list()
    for (attempt in 1:length(ls.p$state_history_tests[[trial]]))
    {
      ls.states[[ppt]][[trial]][[attempt]]<-list()
      for (step in 1:length(ls.p$state_history_tests[[trial]][[attempt]]))
      {
        ls.states[[ppt]][[trial]][[attempt]][[step]]<-matrix(unlist(ls.p$state_history_tests[[trial]][[attempt]][[step]]), ncol=9)
      }
    }
  }
  
  this_ppt_tutorial_tries<-this_ppt_tutorial_keys<-rep(NA, length(ls.p$tutorials))
  for (tutorial in 1:length(ls.p$tutorials))
  {
    this_ppt_tutorial_tries[tutorial]<-length(ls.p$tutorials[[tutorial]])
    this_ppt_tutorial_keys[tutorial]<-''
    for (attempt in 1:length(ls.p$tutorials[[tutorial]]))
    {
      this_ppt_tutorial_keys[tutorial]<-paste0(this_ppt_tutorial_keys[tutorial], paste0(sapply(ls.p$tutorials[[tutorial]][[attempt]]$keys, '[[', 'key'), collapse=''))
    }
  }
  df.sw$tutorial_tries[ppt]<-paste0(this_ppt_tutorial_tries, collapse=',')
  df.sw$tutorial_keys[ppt]<-paste0(nchar(this_ppt_tutorial_keys), collapse=',')
}


df.sw<- df.sw %>% mutate(total = c1t1+c1t2+c1t3+c2t1+c2t2+c2t3+test1+test2+test3+test4+test5,
                         bonus = total*0.2,
                         condition = factor(condition, levels = 0:1, labels = c('train_reflect','train_flower')))
# training group 1:
#   A WWR∠R∠R
# AWA WWR∠R∠R
# AWAWA WWR∠R∠R
# training group 2:
#   XR∠RD
# XR∠RD SSW
# XR∠R∠D ESES
# tests:
#   Z∠∠ WWR∠R∠R
# Z∠∠D WWR∠R∠R
# XR∠RD WR
# XR∠RD EES R∠R
# XR∠RD WWR∠R∠R

df.tw<-df.tw %>% mutate(condition = factor(condition, levels = 0:1, labels = c('train1_reflect','train2_flower')))

df.tw$trial

# A clunky way to get the trial type in the right order for condition 2
df.tw$trial_type<-df.tw$trial
ix4<-which(df.tw$condition=='train2_flower' & df.tw$trial==4)
ix5<-which(df.tw$condition=='train2_flower' & df.tw$trial==5)
ix6<-which(df.tw$condition=='train2_flower' & df.tw$trial==6)
ix7<-which(df.tw$condition=='train2_flower' & df.tw$trial==7)
df.tw$trial_type[ix4]<-6
df.tw$trial_type[ix5]<-7
df.tw$trial_type[ix6]<-4
df.tw$trial_type[ix7]<-5

df.tw<-df.tw %>% mutate(trial_type = case_match(trial, 1~'train_rr1',2~'train_rr2',3~'train_rr3',
                                         4~'test_rr1',5~'test_rr2',6~'test_f1',7~'test_f2',8~'test_both'),
                 trial_solution = case_match(trial, 1~'AWWRKRKR',2~'AWAWWRKRKR',3~'AWAWAWWRKRKR',
                                     4~'ZKKWWRKRKR',5~'ZKKDWWRKRKR',6~'ZXDRWR',7~'ZXDREESRKR',8~'ZXDRWWRKRKR'),
                 train_test = factor(trial<4, levels = c(T,F), labels = c('train','test')))

df.tw$trial_solution[df.tw$condition=='train2_flower' & df.tw$trial==1]<-'ZXDR'
df.tw$trial_solution[df.tw$condition=='train2_flower' & df.tw$trial==2]<-'ZXDRWWE'
df.tw$trial_solution[df.tw$condition=='train2_flower' & df.tw$trial==3]<-'ZXDRWWF'

df.tw$trial_type[df.tw$condition=='train2_flower' & df.tw$trial==1]<-'ZXDR'
df.tw$trial_type[df.tw$condition=='train2_flower' & df.tw$trial==2]<-'ZXDRWWE'
df.tw$trial_type[df.tw$condition=='train2_flower' & df.tw$trial==3]<-'ZXDRWWF'


df.sw
df.tw


save(file='../dat/exp2.rdata', df.sw, df.tw)

df.sw %>% group_by(condition) %>% summarise(mean(total))

df.l <- df.sw %>% gather(test, correct, test1:test5)

df.l<-df.sw %>% group_by(condition) %>% summarise(t1=sum(test1),
                                                  t2=sum(test2),
                                                  t3=sum(test3),
                                                  t4=sum(test4),
                                                  t5=sum(test5), n=n()) %>%
  mutate(t1=t1/n, t2=t2/n, t3=t3/n, t4=t4/n,t5=t5/n) %>%
  gather(test, accuracy, t1:t5)

ggplot(df.l, aes(y=accuracy, x=test, fill=condition)) +
  geom_bar(stat='identity', position = position_dodge())

