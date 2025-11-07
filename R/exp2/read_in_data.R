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

df.raw<-df.raw[-c(1:3),] #Remove test data points from Neil and Haozhe

# df.raw<-df.raw[2,]
vec<-rep(NA, nrow(df.raw))
df.sw<-data.frame(p_ix=vec,
                  prolific_id=vec,
                  condition=vec,
                  gender=vec,
                  age=vec,
                  ins_time=vec,tut_time=vec,trial_time=vec,total_time=vec,
                  comprehension_score=vec, tutorial_attempts=vec, tutorial_keys = vec,
                  t1_solved=vec,t2_solved=vec,t3_solved=vec,t4_solved=vec,
                  t5_solved=vec,t6_solved=vec,t7_solved=vec,t8_solved=vec,
                  t1_attempts=vec,t2_attempts=vec,t3_attempts=vec,t4_attempts=vec,
                  t5_attempts=vec,t6_attempts=vec,t7_attempts=vec,t8_attempts=vec,
                  total_solved=vec,
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
    df.sw[[paste0('t',trial,'_solved',sep='')]][ppt]<-any(sapply(ls.p$trials[[trial]], '[[', 'result'))
    df.sw[[paste0('t',trial,'_attempts',sep='')]][ppt]<-length(ls.p$trials[[trial]])
    
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
 
 this_ppt_tutorial_attempts<-this_ppt_tutorial_keys<-rep(NA, length(ls.p$tutorials))
 for (tutorial in 1:length(ls.p$tutorials))
 {
    this_ppt_tutorial_attempts[tutorial]<-length(ls.p$tutorials[[tutorial]])
    this_ppt_tutorial_keys[tutorial]<-''
    for (attempt in 1:length(ls.p$tutorials[[tutorial]]))
    {
      this_ppt_tutorial_keys[tutorial]<-paste0(this_ppt_tutorial_keys[tutorial], paste0(sapply(ls.p$tutorials[[tutorial]][[attempt]]$keys, '[[', 'key'), collapse=''))
    }
 }
 df.sw$tutorial_attempts[ppt]<-paste0(this_ppt_tutorial_attempts, collapse=',')
 df.sw$tutorial_keys[ppt]<-paste0(nchar(this_ppt_tutorial_keys), collapse=',')
}

df.sw<- df.sw %>% mutate(total_solved = t1_solved+t2_solved+t3_solved+t4_solved+t5_solved+t6_solved+t7_solved+t8_solved,
                         bonus = total_solved*0.2)

df.sw
df.tw


save(file='../dat/exp2.rdata', df.sw, df.tw)


for (ppt in 1:nrow(df.raw))
{
ls.p[[ppt]]
}