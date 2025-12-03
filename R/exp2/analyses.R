library(tidyverse)

rm(list=ls())

load(file='../dat/exp2.rdata', verbose = T)

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
ggsave('accuracy_by_test_position.pdf', width = 5, height = 5)



# df.l <- df.sw %>% gather(test, correct, test_rr1:test_both)

df.l<-df.sw %>% group_by(condition) %>% summarise(rr1=sum(test_rr1),
                                                  rr2=sum(test_rr2),
                                                  f1=sum(test_f1),
                                                  f2=sum(test_f2),
                                                  both=sum(test_both), n=n()) %>%
  mutate(rr1=rr1/n, rr2=rr2/n, f1=f1/n, f2=f2/n,both=both/n) %>%
  gather(test_type, accuracy, rr1:both) %>%
  mutate(test_type = factor(test_type, levels = c('rr1','rr2','f1','f2','both')))

ggplot(df.l, aes(y=accuracy, x=test_type, fill=condition)) +
  geom_bar(stat='identity', position = position_dodge())
ggsave('accuracy_by_test_type.pdf', width = 5, height = 5)

df.sw$comments
