# generate_test_sets
rm(list=ls())

library(tidyverse)
library(ggplot2)
library(gridExtra) #For plotting lots of things at once

# source('hex_setup.R')
load('./dat/hexsetup.rdata', verbose = T)

keymap<-c("A","D","X","Z","K","F","R","E","S","W")

stimuli<-list(dabone=list('XKX','ZXKXW','XKXKSXKXR'),
              hazard=list('ZSA','ZSAZSAR','ZSAEEZSASWXEK'),
              dinopaw=list('XZD','XZDSRKR','ZKSSXZDRKK'),
              triangle=list('ASAEA','ASAEAEASAEAK','ASAEASASAEAEASAEA'))
s<-t<-a<-1

for (s in 1:length(stimuli))
{
  for (t in 1:length(stimuli[[s]]))
  {
    string<-stimuli[[s]][[t]]
    actions<-strsplit(string, "")[[1]]
    state<-empty_state %>% mutate(target = 0, active=0)
    
    for (a in 1:length(actions))
    {
      state<-f[[which(keymap==actions[a])]](state)
    }

    # Paste the final state to the board-map and plot it out to look at what happened
    board_polygons$state<-factor(rep(state$active, each = 6))


    p_save<-ggplot(board_polygons, aes(x_pos, y_pos)) +
      geom_polygon(aes(group = id, fill=state), colour = 'black') +
      # geom_text(data = empty_state, aes(x = x_pos, y = y_pos, label = id), size = 3) +
      scale_y_reverse() +
      scale_fill_manual(values = c('white','lightgreen')) +
      theme(legend.position = 'none')
    
    ggsave(p_save, width = 5.5, height = 5,
           filename=paste0('./plot/exp1_stimuli/',
                           names(stimuli[s]), '_',t,
                           c('easy','medium','hard')[t], '.pdf'))
    
    state$target <- state$active
    state$active <-0
    
    for (a in 1:length(actions))
    {
      state<-f[[which(keymap==actions[a])]](state)
      
      state$combination<-0
      state$combination[state$active==0 & state$target==1]<-1
      state$combination[state$active==1 & state$target==0]<-2
      state$combination[state$active==1 & state$target==1]<-3
      board_polygons$combination<-factor(rep(state$combination, each = 6), levels = 0:3, labels = c('empty','missed','wrong','correct'))
      board_polygons$target<-factor(rep(state$target, each = 6))

      p_save<-ggplot(board_polygons, aes(x_pos, y_pos)) + 
        geom_polygon(aes(group = id, fill=combination), colour = 'lightgray') +
        scale_y_reverse() +
        scale_fill_manual(values = c('white','lightgreen','black','black'), drop = F) +
        theme(legend.position = 'none')
      
      ggsave(p_save, width = 5.5, height = 5,
             filename=paste0('./plot/exp1_stimuli/',
                             names(stimuli[s]), '_',t,
                             c('easy','medium','hard')[t], '_', a, '.pdf'))
    }
    
  }
}
