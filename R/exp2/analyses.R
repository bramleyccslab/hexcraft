library(tidyverse)
library(lme4)
library(jsonlite)

rm(list=ls())

load(file='../dat/exp2.rdata', verbose = T)

df.sw %>% group_by(condition) %>% summarise(mean(total))


# Accuracy by test position

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
ggsave('accuracy_by_test_position.pdf', width = 5, height = 5)



# df.l <- df.sw %>% gather(test, correct, test_rr1:test_both)


# Accuracy by test type!
df.sw <- df.sw %>% mutate(train_score = c1t1+c1t2+c1t3+c2t1+c2t2+c2t3)
df.l<-df.sw %>% group_by(condition) %>% summarise(rr1=sum(test_rr1),
                                                  rr2=sum(test_rr2),
                                                  f1=sum(test_f1),
                                                  f2=sum(test_f2),
                                                  both=sum(test_both), n=n()) %>%
  mutate(rr1=rr1/n, rr2=rr2/n, f1=f1/n, f2=f2/n,both=both/n) %>%
  gather(test_type, accuracy, rr1:both) %>%
  mutate(test_type = factor(test_type, levels = c('rr1','rr2','f1','f2','both'),
                            labels = c('Reflect 1','Reflect 2', 'Flower 1', 'Flower 2', 'Both')))


ggplot(df.l, aes(y=accuracy, x=test_type, fill=condition)) +
  geom_bar(stat='identity', position = position_dodge()) +
  labs(x='Test problem', y='Proportion solved', fill='Condition') +
  scale_fill_manual(values = c('#E38300', '#497059')) +
  theme_bw() +
  theme(legend.position = 'top')

ggsave('accuracy_by_test_type.pdf', width = 6, height = 5)

df.tmp<-df.sw %>% group_by(condition, train_score) %>% summarise(rr1=sum(test_rr1),
                                                                 rr2=sum(test_rr2),
                                                                 f1=sum(test_f1),
                                                                 f2=sum(test_f2),
                                                                 both=sum(test_both), n=n()) %>%
  mutate(rr1=rr1/n, rr2=rr2/n, f1=f1/n, f2=f2/n,both=both/n) %>%
  gather(test_type, accuracy, rr1:both) %>%
  mutate(test_type = factor(test_type, levels = c('rr1','rr2','f1','f2','both'),
                            labels = c('Reflect 1','Reflect 2', 'Flower 1', 'Flower 2', 'Both')))


ggplot(df.tmp, aes(y=accuracy, x=test_type, fill=condition)) +
  geom_bar(stat='identity', position = position_dodge()) +
  labs(x='Test problem', y='Proportion solved', fill='Condition') +
  scale_fill_manual(values = c('#E38300', '#497059')) +
  facet_wrap(. ~ train_score, ncol=1) +
  theme_bw() +
  theme(legend.position = 'top')

ggsave('accuracy_by_test_type_train_score.pdf', width = 6, height = 5)

ggplot(df.tmp %>% group_by(condition, train_score) %>% summarise(n=n[1]), aes(x=train_score, y=n, fill=condition)) +
  geom_bar(stat = 'identity', position = position_dodge()) +
  scale_fill_manual(values = c('#E38300', '#497059')) +
  labs(fill = 'Condition',y='N', x='Number of training tasks completed') +
  theme_bw()+
  theme(legend.position = 'none')
ggsave('training_performance.pdf', width = 4, height = 4)

df.test <- df.tw %>% filter(train_test == 'test') %>% group_by(p_ix, trial_type) %>%
  summarise(prolific_id = prolific_id[1], condition = condition[1], trial = trial[1],
            attempts = max(attempt), success = any(success), trial_solution = trial_solution[1]) %>%
  mutate(trial_type = factor(trial_type, levels = c('test_rr1','test_rr2','test_f1','test_f2','test_both'),
                             labels = c('Reflect 1','Reflect 2', 'Flower 1', 'Flower 2', 'Both')))

df.train <- df.tw %>% filter(train_test == 'train') %>% group_by(p_ix, trial_type) %>%
  summarise(prolific_id = prolific_id[1], condition = condition[1], trial = trial[1],
            attempts = max(attempt), success = any(success),trial_solution = trial_solution[1]) 
# %>%
# mutate(trial_type = factor(trial_type, levels = c('test_rr1','test_rr2','test_f1','test_f2','test_both'),
# labels = c('Reflect 1','Reflect 2', 'Flower 1', 'Flower 2', 'Both')))

# Check the accuracies match between df.tw and df.test
df.test %>% group_by(condition, trial_type) %>% summarise(mean(success))
df.test %>% group_by(condition, trial) %>% summarise(mean(success))

ggplot(df.test %>% filter(success==T), aes(y=attempts, x=trial_type, fill=condition)) +
  stat_summary(fun='mean', geom='bar', position = position_dodge()) +
  stat_summary(fun.data = mean_se, geom = "errorbar", position = position_dodge(0.9), width = 0.2) +
  scale_fill_manual(values = c('#E38300', '#497059')) +
  theme_bw()
ggsave('attempts_before_success_test_type.pdf', width = 7, height = 5)

df.strain<-df.train %>% group_by(trial_type) %>% summarise(accuracy = mean(success))

ggplot(df.train, aes(success, x=trial_type, fill=condition)) +
  geom_bar()


stat_summary(fun='mean', geom='bar', position = position_dodge()) +
  stat_summary(fun.data = mean_se, geom = "errorbar", position = position_dodge(0.9), width = 0.2) +
  scale_fill_manual(values = c('#E38300', '#497059')) +
  theme_bw()


df.sw$comments

df.test$ttf<-factor(df.test$trial_type, levels = c('Both', 'Reflect 1','Reflect 2', 'Flower 1', 'Flower 2'))


# Basic stats
m0<-glmer(success ~ condition + ttf + (1|prolific_id),
          data = df.test, family = 'binomial')
summary(m0)
m1<-glmer(success ~ condition * ttf + (1|prolific_id),
          data = df.test, family = 'binomial')
summary(m1)

# tmp<-df.sw %>% group_by(condition) %>% summarise(rr1=sum(test_rr1),rr2=sum(test_rr2),f1=sum(test_f1),f2=sum(test_f2),bot=sum(test_both))
# counts<-matrix(unlist(tmp), nrow=2)[,2:6]
# chisq.test(counts)





# STRING LEVEL ANALYSES



solutions<-list(r1=(df.tw %>% filter(success, trial_type=='test_rr1'))$keycodes,
                r2=(df.tw %>% filter(success, trial_type=='test_rr2'))$keycodes,
                f1=(df.tw %>% filter(success, trial_type=='test_f1'))$keycodes,
                f2=(df.tw %>% filter(success, trial_type=='test_f2'))$keycodes,
                both=(df.tw %>% filter(success, trial_type=='test_both'))$keycodes)
solutions_sorted<-solutions
for ( i in 1:5)
{
  solutions_sorted[[i]]<-solutions_sorted[[i]][sort(nchar(solutions_sorted[[i]]), index.return = T)$ix]
  solutions_sorted[[i]]<-str_replace_all(str_replace_all(str_replace_all(str_replace_all(solutions_sorted[[i]], 'K', 'O'), 'Z', 'C'), 'X','B'), 'L', '')
}
solutions_sorted


solutions_condition<-list(reflect = list(r1=(df.tw %>% filter(success, condition=='train1_reflect', trial_type=='test_rr1'))$keycodes,
                                         r2=(df.tw %>% filter(success, condition=='train1_reflect', trial_type=='test_rr2'))$keycodes,
                                         f1=(df.tw %>% filter(success, condition=='train1_reflect', trial_type=='test_f1'))$keycodes,
                                         f2=(df.tw %>% filter(success, condition=='train1_reflect', trial_type=='test_f2'))$keycodes,
                                         both=(df.tw %>% filter(success, condition=='train1_reflect', trial_type=='test_both'))$keycodes),
                          flower = list(r1=(df.tw %>% filter(success, condition=='train2_flower', trial_type=='test_rr1'))$keycodes,
                                        r2=(df.tw %>% filter(success, condition=='train2_flower', trial_type=='test_rr2'))$keycodes,
                                        f1=(df.tw %>% filter(success, condition=='train2_flower', trial_type=='test_f1'))$keycodes,
                                        f2=(df.tw %>% filter(success, condition=='train2_flower', trial_type=='test_f2'))$keycodes,
                                        both=(df.tw %>% filter(success, condition=='train2_flower', trial_type=='test_both'))$keycodes))
solutions_sorted_condition<-solutions_condition
for (i in 1:5)
{
  solutions_sorted_condition[[1]][[i]]<-solutions_sorted_condition[[1]][[i]][sort(nchar(solutions_sorted_condition[[1]][[i]]), index.return = T)$ix]
  solutions_sorted_condition[[1]][[i]]<-str_replace_all(str_replace_all(str_replace_all(str_replace_all(solutions_sorted_condition[[1]][[i]], 'K', 'O'), 'Z', 'C'), 'X','B'), 'L', '')
  solutions_sorted_condition[[2]][[i]]<-solutions_sorted_condition[[2]][[i]][sort(nchar(solutions_sorted_condition[[2]][[i]]), index.return = T)$ix]
  solutions_sorted_condition[[2]][[i]]<-str_replace_all(str_replace_all(str_replace_all(str_replace_all(solutions_sorted_condition[[2]][[i]], 'K', 'O'), 'Z', 'C'), 'X','B'), 'L', '')
}

i<-5
cat(paste0(solutions_sorted_condition[[1]][[i]], collapse='\n'))
cat(paste0(solutions_sorted_condition[[2]][[i]], collapse='\n'))


solutions_sorted_highlighted_condition<-solutions_sorted_condition
for (i in 1:5)
{
  solutions_sorted_highlighted_condition[[1]][[i]]<-str_replace_all(solutions_sorted_condition[[1]][[i]], 'ROROR', '[ROROR]')
  solutions_sorted_highlighted_condition[[2]][[i]]<-str_replace_all(solutions_sorted_condition[[2]][[i]],, 'ROROR', '[ROROR]')
  solutions_sorted_highlighted_condition[[1]][[i]]<-str_replace_all(solutions_sorted_condition[[1]][[i]], 'CBDR', '[CBDR]')
  solutions_sorted_highlighted_condition[[2]][[i]]<-str_replace_all(solutions_sorted_condition[[2]][[i]],, 'CBDR', '[CBDR]')
}
i<-3
cat(paste0(solutions_sorted_highlighted_condition[[1]][[i]], collapse='\n'))


library(PTXQC)

wc<-list(list(),list())
bc<-list()

df.wc<-data.frame(task=c(), condition = c(), p1=c(), p2=c(), lcs=c(), len=c())
df.bc<-data.frame(task=c(),  p1=c(), p2=c(), lcs=c(), len=c())
# What is the average longest common substring between pairs of solutions within condition vs between conditions
for (task in 1:5)
{
  wc[[1]][[task]]<-matrix(NA, length(solutions_sorted_condition[[1]][[task]]),length(solutions_sorted_condition[[1]][[task]]))
  wc[[2]][[task]]<-matrix(NA, length(solutions_sorted_condition[[2]][[task]]),length(solutions_sorted_condition[[2]][[task]]))
  bc[[task]]<-matrix(NA, length(solutions_sorted_condition[[1]][[task]]),length(solutions_sorted_condition[[2]][[task]]))
  # WITHIN CONDITION
  for (i in 1:length(solutions_sorted_condition[[1]][[task]]))
  {
    for (j in 1:length(solutions_sorted_condition[[1]][[task]]))
    {
      if (i!=j)
      {
        wc[[1]][[task]][i,j]<-LCSn(c(solutions_sorted_condition[[1]][[task]][i],solutions_sorted_condition[[1]][[task]][j]),  min_LCS_length = 0)
        df.wc<-rbind(df.wc, data.frame(task=task, condition = 'reflect', p1=i, p2=j, lcs = wc[[1]][[task]][i,j], len = nchar(wc[[1]][[task]][i,j])))
      }
      
    }
  }
  
  for (i in 1:length(solutions_sorted_condition[[2]][[task]]))
  {
    for (j in 1:length(solutions_sorted_condition[[2]][[task]]))
    {
      if (i!=j)
      {
        wc[[2]][[task]][i,j]<-LCSn(c(solutions_sorted_condition[[2]][[task]][i],solutions_sorted_condition[[2]][[task]][j]),  min_LCS_length = 0)
        df.wc<-rbind(df.wc, data.frame(task=task, condition = 'reflect', p1=i, p2=j, lcs = wc[[2]][[task]][i,j], len = nchar(wc[[2]][[task]][i,j])))
      }
      
    }
  }
  
  # BETWEEN CONDITIONS
  for (i in 1:length(solutions_sorted_condition[[1]][[task]]))
  {
    for (j in 1:length(solutions_sorted_condition[[2]][[task]]))
    {
      bc[[task]][i,j]<-LCSn(c(solutions_sorted_condition[[1]][[task]][i],solutions_sorted_condition[[2]][[task]][j]),  min_LCS_length = 0)
      df.bc<-rbind(df.bc, data.frame(task=task,  p1=i, p2=j, lcs = bc[[task]][i,j], len = nchar(bc[[task]][i,j])))
    }
  }
  
  
  # mean(nchar(wc[[1]][[1]]), na.rm=T), mean(nchar(wc[[1]][[1]]), na.rm=T), mean(nchar(wc[[1]][[1]]), na.rm=T), mean(nchar(wc[[1]][[1]]), na.rm=T)
}

# Within the same learning condition
df.wc %>% group_by(task) %>% summarise(ml = mean(len), med=median(len), sdl = sd(len))
#  Between learning conditions
df.bc %>% group_by(task) %>% summarise(ml = mean(len), med=median(len), sdl = sd(len))

# Almost the same!

libs_haozhe<- fromJSON(txt="../../json/learned_grammars.json")

libs_proc<-c()
for (i in 1:length(libs_haozhe))
{
  tmp<-names(fromJSON(libs_haozhe[[i]]))
  tmp2<-substr(tmp, 6, nchar(tmp))
  for (j in 1:length(tmp2))
  {
    tmp2[j]<-str_replace_all(tmp2[j], "'","")
    tmp2[j]<-str_replace_all(tmp2[j], " ","")
  }
  if (length(tmp2)>10)
  {
    libs_proc[i]<-paste0(tmp2[11:length(tmp2)], collapse = '; ')
  } else 
  {
    libs_proc[i]<-''
  }

}

trl<-libs_proc[df.sw$condition=='train_reflect']
trl<-str_replace_all(str_replace_all(str_replace_all(str_replace_all(trl, 'k', 'o'), 'z', 'c'), 'x','b'), 'l', '')
trl[sort(nchar(trl), index.return = T)$ix]
tfl<-libs_proc[df.sw$condition=='train_flower']
tfl<-str_replace_all(str_replace_all(str_replace_all(str_replace_all(tfl, 'k', 'o'), 'z', 'c'), 'x','b'), 'l', '')
tfl[sort(nchar(tfl), index.return = T)$ix]

cat(paste0(trl[sort(nchar(trl), index.return = T)$ix], collapse = '\n'))
cat(paste0(tfl[sort(nchar(tfl), index.return = T)$ix], collapse = '\n'))

