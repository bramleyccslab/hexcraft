# generate_test_sets
rm(list=ls())

library(tidyverse)
library(ggplot2)
library(gridExtra) #For plotting lots of things at once
library(ngram)

# source('hex_setup.R')
load('./dat/hexsetup.rdata', verbose = T)
load('./dat/results_processed_neil.rdata', verbose = T)
keymap<-c("A","D","X","Z","K","F","R","E","S","W","L")


stimuli<-list(dabone =  c('XKX','ZXKXW','XKXWWXKXSSR'), 
              hazard =  c('ZSA', 'ZSARK', 'ZSAKZSARKX'), 
              dinopaw = c('ZXD', 'ZXDWR', 'ZXDESZXDSR'))

problems<-data.frame(id=c('train1','train2','train3','train4','train5','train6','train7','train8','train9',
                          'dabone1','dabone2','dabone3','hazard1','hazard2','hazard3','dinopaw1','dinopaw2','dinopaw3'),
                     challenge=c('A', 'ASW', 'Z', 'X', 'ZX', 'XD', 'ZKKK', 'ZF', 'ASSR',
                                 'XKX','ZXKXW','XKXWWXKXSSR',
                                 'ZSA', 'ZSARK', 'ZSAKZSARKX',
                                 'ZXD', 'ZXDWR', 'ZXDESZXDSR')) 

test_problems<-c('dinopaw2','dabone2','hazard2','dinopaw3','dabone3','hazard3')
tp_ix<-c(11:12,14:15,17:18)
strings<-list()
for (i in 1:nrow(df.sw))
{
  # One participant's strings
  tmp<-sapply(unlist(ls.sw[[i]][tp_ix], recursive=F),  '[[', 'key')
  strings[[i]]<-sapply(tmp, concatenate)
}
string<-concatenate(sapply(strings, concatenate), collapse = '.')
phrasetables<-list()
for (i in 2:8)
{
  ng <- ngram(string, n=i)
  phrasetables[[i-1]]<-get.phrasetable(ng)
}

df.ngram<-rbind(cbind(n=2, position=1:20, phrasetables[[1]][1:20,]),
      cbind(n=3, position=1:20, phrasetables[[2]][1:20,]),
      cbind(n=4, position=1:20, phrasetables[[3]][1:20,]),
      cbind(n=5, position=1:20, phrasetables[[4]][1:20,]),
      cbind(n=6, position=1:20, phrasetables[[5]][1:20,]),
      cbind(n=7, position=1:20, phrasetables[[6]][1:20,]),
      cbind(n=8, position=1:20, phrasetables[[7]][1:20,]))

for (nn in 2:8)
{
  df<-df.ngram %>% filter(n==nn) %>% mutate(ngrams = factor(ngrams, levels = ngrams))
  ggplot(df , aes(x=ngrams, y=freq)) +
    geom_bar(stat='identity') +
    theme(axis.text.x = element_text(angle=90, vjust=.5, hjust=1)) +
    labs(y='Frequency', x=paste0(c(nn, '-grams'), collapse=''))
  
  ggsave(paste0(c('./plot/ngrams_', nn, '.pdf'), collapse=''), width = 6, height =4)
  
}

removeDuplicates<-function(s)
{
  if (length(s) < 2)
  {
    out<-s
  } else if (s[1] != s[2])
  {
    out<- c(s[1], removeDuplicates(s[2:length(s)]))
  } else
  {
    out<-removeDuplicates(s[2:length(s)])   
  }
  out
}


tmp<-strsplit(string, split=' ')[[1]]

sort(summary(factor(tmp)))

tmp<-recode(tmp, E='M', S='M', W='M')


sstring<-c()
i<-0
while (i<((length(tmp)/1000)-1))
{
  sstring<-c(sstring, removeDuplicates(tmp[((i*1000)+1):((i*1000)+1000)]))
  i<-i+1
}
sstring<-c(sstring, removeDuplicates(tmp[41001:length(tmp)]))
sstring<-concatenate(sstring)

sphrasetables<-list()
for (i in 2:8)
{
  ng <- ngram(sstring, n=i)
  sphrasetables[[i-1]]<-get.phrasetable(ng)
}


df.sngram<-rbind(cbind(n=2, position=1:20, sphrasetables[[1]][1:20,]),
                cbind(n=3, position=1:20, sphrasetables[[2]][1:20,]),
                cbind(n=4, position=1:20, sphrasetables[[3]][1:20,]),
                cbind(n=5, position=1:20, sphrasetables[[4]][1:20,]),
                cbind(n=6, position=1:20, sphrasetables[[5]][1:20,]),
                cbind(n=7, position=1:20, sphrasetables[[6]][1:20,]),
                cbind(n=8, position=1:20, sphrasetables[[7]][1:20,]))

for (nn in 2:8)
{
  df<-df.sngram %>% filter(n==nn) %>% mutate(ngrams = factor(ngrams, levels = ngrams))
  ggplot(df , aes(x=ngrams, y=freq)) +
    geom_bar(stat='identity') +
    theme(axis.text.x = element_text(angle=90, vjust=.5, hjust=1)) +
    labs(y='Frequency', x=paste0(c(nn, '-grams'), collapse=''))
  
  ggsave(paste0(c('./plot/sngrams_', nn, '.pdf'), collapse=''), width = 6, height =4)
  
}

