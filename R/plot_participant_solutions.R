# generate_test_sets
rm(list=ls())

library(tidyverse)
library(ggplot2)
library(gridExtra) #For plotting lots of things at once

# source('hex_setup.R')
load('./dat/hexsetup.rdata', verbose = T)
load('./dat/results_processed_neil.rdata', verbose = T)
keymap<-c("A","D","X","Z","K","F","R","E","S","W","L")

f$End<-function(ts)
{
  ts$active<-0
  ts
}

problems<-data.frame(id=c('train1','train2','train3','train4','train5','train6','train7','train8','train9',
                          'dabone1','dabone2','dabone3','hazard1','hazard2','hazard3','dinopaw1','dinopaw2','dinopaw3'),
                     challenge=c('A', 'ASW', 'Z', 'X', 'ZX', 'XD', 'ZKKK', 'ZF', 'ASSR',
                                 'XKX','ZXKXW','XKXWWXKXSSR',
                                 'ZSA', 'ZSARK', 'ZSAKZSARKX',
                                 'ZXD', 'ZXDWR', 'ZXDESZXDSR')) 
  
test_problems<-c('dinopaw2','dabone2','hazard2','dinopaw3','dabone3','hazard3')
p<-t<-1

for (p in 1:nrow(df.sw))
{
  for (t in 1:length(test_problems))
  {
    p_save<-list()
    ix<-df.tw$upi==df.sw$upi[p] & df.tw$problem==test_problems[t]
    string<-df.tw$actions[ix]
    actions<-strsplit(string, "")[[1]]
    starts<-strsplit(df.tw$starts[ix])
    state<-empty_state %>% mutate(target = 0, active=0)
    
    # for (a in 1:length(actions))
    # {
    #   state<-f[[which(keymap==actions[a])]](state)
    # }
    # 
    for (a in 1:length(actions))
    {
      #If it is an impotent Enter (where they continue without clearing after), do nothing.
      if (!(which(keymap==actions[a])==11 & starts[a]=='0'))
      {
        state<-f[[which(keymap==actions[a])]](state)
      }
      
      state$combination<-0
      state$combination[state$active==0 & state$target==1]<-1
      state$combination[state$active==1 & state$target==0]<-2
      state$combination[state$active==1 & state$target==1]<-3
      board_polygons$combination<-factor(rep(state$combination, each = 6), levels = 0:3, labels = c('empty','missed','wrong','correct'))
      board_polygons$target<-factor(rep(state$target, each = 6))

      p_save[[a]]<-ggplot(board_polygons, aes(x_pos, y_pos)) + 
        geom_polygon(aes(group = id, fill=combination), colour = 'lightgray') +
        scale_y_reverse() +
        scale_fill_manual(values = c('white','lightgreen','black','black'), drop = F) +
        theme(legend.position = 'none',
              axis.title.x=element_blank(),
              axis.text.x=element_blank(),
              axis.ticks.x=element_blank(),
              axis.title.y=element_blank(),
              axis.text.y=element_blank(),
              axis.ticks.y=element_blank())
    }
      p_plot<-grid.arrange(grobs = p_save, nrow = round(sqrt(a)),
                           top = paste0('Problem: ', df.tw$problem[ix], ' Participant: ', df.tw$upi[ix], ' Solved: ', df.tw$correct[ix],
                                        '\nAttempts: ', df.tw$attempts[ix], ' Actions: ', df.tw$actions[ix], collapse = '_'))
      
      ggsave(p_plot, width = 10, height = 10,
             filename=paste0('./plot/participants/',
                             df.tw$problem[ix], '_', df.tw$upi[ix], '_', df.tw$correct[ix], '.pdf', collapse = ''))
    }
    
  }
}
