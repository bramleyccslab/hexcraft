# generate_test_sets
rm(list=ls())

library(tidyverse)
library(ggplot2)
library(gridExtra) #For plotting lots of things at once

# source('hex_setup.R')
load('./dat/hexsetup.rdata', verbose = T)

custom_cache = c("AddUnit")
ccache = TRUE


# ##################################################
# Generate a bunch of test sequences without caching

set.seed(678654) #changed for random generation

depth<-3
N<-100
basic_targets_d3<-matrix(NA, nrow=61, ncol=N)
basic_targets_d3_solutions<-list()


for (i in 1:100)
{
  # Basic functionality is to chain these together, calculating a loss and keeping a history
  state<-empty_state %>% select(-x_pos, -y_pos) %>% mutate(target = 0, active=0)
  
  # Generate a random sequence (in which every step actually does something)
  d<-1
  a_states<-matrix(NA, nrow=nrow(state), ncol=depth)#vector(mode = "list", length = depth)
  actions<-rep(NA, length = depth)
  while (d<(depth+1))
  {
    action<-sample(length(f), 1)
    tmp_state<-f[[action]](state)
    loop_check<-F
    
    if (d==1)
    {
      if (all(tmp_state$active==0))
      {
        cat(i,d, 'ineffective first step\n')
        loop_check<-T
      }
    }
    

    if (d>1)
    {
      for (lc in 1:(d-1))
      {
        if(all(tmp_state$active==a_states[,lc]))
        {
          loop_check<-T
          cat(i, d, lc, 'loop detected\n')
        }
      }
    }
    
    
    if (loop_check==F)
    {
      state<-tmp_state
      a_states[,d]<-state$active
      actions[d]<-names(f)[action]
      d<-d+1
    }
  }
  
  basic_targets_d3[,i]<-state$active
  basic_targets_d3_solutions[[i]]<-actions
}

save(file='./dat/basic_targets_d3.rdata', basic_targets_d3, basic_targets_d3_solutions)

# Mega-plot time
p<-fb<-list()
for (i in 1:N)
{
  fb[[i]]<-board_polygons
  fb[[i]]$state<-factor(rep(basic_targets_d3[,i], each = 6))
  p[[i]]<-ggplot(fb[[i]], aes(x_pos, y_pos)) + 
    geom_polygon(aes(group = id, fill=state), colour = 'black') +
    scale_fill_manual(values = c('white','yellow')) +
    ggtitle(paste0(basic_targets_d3_solutions[[i]], collapse=' '))
  
}

p_save<-grid.arrange(grobs = p, nrow = round(sqrt(N)))
ggsave(p_save, filename='./plot/targets_d3.pdf', width = 50, height = 40, limitsize = F)


# Now for some recursive targets
set.seed(1988)
depth<-3
N<-9
recursive_targets_d3<-list()
recursive_targets_d3_solutions<-list()


for (cache in 1:N)
{
  recursive_targets_d3[[cache]]<-matrix(NA, nrow=61, ncol=10)
  recursive_targets_d3_solutions[[cache]]<-list()
  
  for (example in 1:N)
  {
    # Basic functionality is to chain these together, calculating a loss and keeping a history
    state<-empty_state %>% select(-x_pos, -y_pos) %>% mutate(target = 0, active=0)
    
    # Generate a random sequence (in which every step actually does something)
    d<-1
    a_states<-matrix(NA, nrow=nrow(state), ncol=depth)#vector(mode = "list", length = depth)
    actions<-list()
    
    
    while (d<(depth+1))
    {
      if (runif(1)>0.5)
      {
        action<-names(f)[sample(length(f), 1)]
        tmp_state<-f[[action]](state)
        cat('cache', cache, ' example', example, 'primitive\n')
      } 
      else {
        tmp_state <- apply_cache(state, basic_targets_d3_solutions[[cache]])
        
        cat('cache', cache, ' example', example, 'library\n')
      }
      
       
      # Checks the state actually changed (reduces redundancies a lot)
      loop_check<-F
      
      if (d==1)
      {
        if (all(tmp_state$active==0))
        {
          cat(cache, example, d, 'ineffective first step\n')
          loop_check<-T
        }
      }
      
      if (d>1)
      {
        for (lc in 1:(d-1))
        {
          if(all(tmp_state$active==a_states[,lc]))
          {
            loop_check<-T
            cat(cache, example, d, lc, 'loop detected\n')
          }
        }
      }
      
      
      if (loop_check==F)
      {
        state<-tmp_state
        a_states[,d]<-state$active
        actions[[d]]<-action
        d<-d+1
      }
    }
    recursive_targets_d3[[cache]][,example]<-state$active
    recursive_targets_d3_solutions[[cache]][[example]]<-actions
  }
  
}



# Mega-plot time
for (cache in 1:N)
{
  p<-fb<-list()
  
  tmp<-board_polygons
  tmp$state<- factor(rep(basic_targets_d3[,cache], each = 6))
  p[[1]]<-ggplot(tmp, aes(x_pos, y_pos)) + 
    geom_polygon(aes(group = id, fill=state), colour = 'black') +
    scale_fill_manual(values = c('white','blue')) +
    ggtitle('Cached primitive')
  
  for (example in 1:N)
  {
    fb[[example]]<-board_polygons
    fb[[example]]$state<-factor(rep(recursive_targets_d3[[cache]][,example], each = 6))
    p[[example+1]]<-ggplot(fb[[example]], aes(x_pos, y_pos)) + 
      geom_polygon(aes(group = id, fill=state), colour = 'black') +
      scale_fill_manual(values = c('white','yellow')) +
      ggtitle(paste0(recursive_targets_d3_solutions[[cache]][[example]], collapse=' '))
  }
  str(p)
  p_save<-grid.arrange(grobs = p, nrow = 2,
                       top = paste0('Cache sequence: ', paste0(basic_targets_d3_solutions[[cache]], collapse = ' ')))
  ggsave(p_save, filename=paste0('./plot/recursive_targets', cache, '.pdf'), width = 30, height = 10, limitsize = F)
}


save(file='./dat/recursive_targets_d3.rdata', basic_targets_d3, basic_targets_d3_solutions)


# Very deep recursion
set.seed(1988)
set.seed(1989)
library<-trace<-list(1,2,3,4,5,6,7,8,9,10)
for (d in 1:10)
{
  ix<-sample(1:length(library), 2, replace = T)

  library[[length(library)+1]]<-unlist(library[ix])
  
  trace[[length(trace)+1]]<-list(trace[[ix[1]]], trace[[ix[2]]])
}


state<-empty_state %>% select(-x_pos, -y_pos) %>% mutate(target = 0, active=0)
ix <- which(sapply(library, length) == max(sapply(library, length)))[1]
a_states<-matrix(NA, nrow=nrow(state), ncol=length(library[[ix]]))#vector(mode = "list", length = depth)

# todo make it a character string using letters and stop at 26 entries (so they are always a single string digit)?
test_string<-unlist(library[sample(1:length(library), 15, replace = T)])
potential_appearances<-list()
for (i in 1:length(library))
{
  potential_appearances[[i]]<-grep(library[i], test_string)
}
potential_appearances


# dt <- FromListSimple(trace[[ix]])
# dt


# Recursive sequence
for (i in 1:length(library[[ix]]))
{
  state<-f[[library[[ix]][i]]](state)
  a_states[,i]<-state$active
}
fb<-p<-list()
for (example in 1:length(library[[ix]]))
{
  fb[[example]]<-board_polygons
  fb[[example]]$state<-factor(rep(a_states[,example], each = 6))
  p[[example]]<-ggplot(fb[[example]], aes(x_pos, y_pos)) + 
    geom_polygon(aes(group = id, fill=state), colour = 'black') +
    scale_fill_manual(values = c('white','yellow')) +
    ggtitle(paste0('Step', example, ': ', names(f)[library[[ix]][example]], collapse=' '))
}

p_save<-grid.arrange(grobs = p, nrow = round(sqrt(length(library[[ix]]))),
                     top = paste0('Deep recursion example: ', paste0(library[[ix]], collapse = '_')))
ggsave(p_save, filename=paste0('./plot/deep_recursion', paste0(head(library[[ix]]), collapse = '_'), 'etc.pdf'), width = 20, height = 15, limitsize = F)

save(file='./dat/training_case.rdata', library, trace, ix)
