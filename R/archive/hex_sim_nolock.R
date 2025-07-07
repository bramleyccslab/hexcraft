rm(list=ls())

library(tidyverse)
library(ggplot2)
library(gridExtra) #For plotting lots of things at once

# source('hex_setup.R')
  
##########
#START
##########

# Basic functionality is to chain these together, calculating a loss and keeping a history
state<-empty_board %>% select(-x_pos, -y_pos) %>% mutate(target = 0, active=0)

# Generate a random sequence
depth<-10
a_states<-matrix(NA, nrow=nrow(state), ncol=depth)#vector(mode = "list", length = depth)
actions<-rep(NA, length = depth)

for (d in 1:depth)
{
  if (d==1)
  {
    action<-sample(c(1,3,4), 1)
  } else {
    action<-sample(length(f), 1)
  }

  state<-f[[action]](state)
  
  a_states[,d]<-state$active
  actions[d]<-names(f)[action]
}

colSums(a_states)
actions

# Append the state to the board and plot it
full_board$state<-factor(rep(state$active, each = 6))
ggplot(full_board, aes(x_pos, y_pos)) + 
  geom_polygon(aes(group = id, fill=state), colour = 'black') +
  scale_fill_manual(values = c('white','yellow'))

a_states


target<-state$active



##################
# Greedy solver
##################

state<-empty_board %>% select(-x_pos, -y_pos) %>% mutate(target = 0, active=0, locked=0)
state$target<-target

full_points<-sum(state$target)

full_board$target<-factor(rep(state$target, each = 6))
ggplot(full_board, aes(x_pos, y_pos)) + 
  geom_polygon(aes(group = id, fill=target), colour = 'black') +
  scale_fill_manual(values = c('white','lightyellow'))

timeout<-100#10 #Try the whole search process 10 times
n_samples<-500#10 #On each step generate 10 candidate sequences of length 1:max_depth
max_depth<-10#10 #Progressively increase the number of steps generated (so you have a short-path-first search)

a_states<-list()#vector(mode = "list", length = depth)
actions<-list()
scores<-rep(NA, length = timeout)

for (t in 1:timeout)
{
  # Track how well you are doing
  current_score<-sum(state$active==1 & state$target==1) - sum(state$active==1 & state$target==0) - length(unlist(actions))*0.1
  
  ps<-list()
  score<-rep(NA, n_samples)
  
  progress<-F #Breaks the search loop when you find something that improves on what you have (greedyness)
  seq_length<-1 #Reset the sequence length each time
  
  # This is a greedy search
  #It starts with 1-step options and increases until something improves the fitness
  while (T)
  {
    # Create a set of n_samples potential next action(sequences)
    prospective_actions<-list() #Keep they hypothetical play-forwards in this list
    for (samp in 1:n_samples)
    {
      #Each step is a function from the library [TO ADD, ALLOW CACHING]
      # prospective_actions[[samp]]<-sample(length(f), seq_length, replace = T)
      # prospective_actions[[samp]]<-rep(NA, seq_length)
      for (step in 1:seq_length) #OR samp:seq_length if we wanna reuse the failed trajectories from the previous depth?
      {
        
        if (step==1)
        {
          prospective_actions[[samp]]<-c(sample(length(f), 1, replace = T))
          # Apply the first action in the sequence
          ps[[samp]]<-f[[prospective_actions[[samp]][step]]](state)
        } else {
          prospective_actions[[samp]][step]<-sample(length(f), 1, replace = T)
          # Apply subsequent actions in the sequence
          ps[[samp]]<-f[[prospective_actions[[samp]][step]]](ps[[samp]])
        }
        #THERE'S AN OPPORTUNITY HERE TO CHECK THE SCORE ALONG THE PATH AND TAKE BEST/SHORTEST, SHOULD ADD FOR PERFORMANCE LATER?
      }
      # How many points would I have if I stopped there?
      score[samp]<-sum(ps[[samp]]$active==1 & ps[[samp]]$target==1) - sum(ps[[samp]]$active==1 & ps[[samp]]$target==0) - length(unlist(actions))*0.1 - seq_length*0.1
    }
    greedy_choice_ix<-which(score==max(score))
    cat('t',t, 'seq_length', seq_length, 'max score', max(score), 'current score', current_score, '\n')
    
    # sample from the best options if there are more than 1
    if (length(greedy_choice_ix)>1)
    {
      greedy_choice_ix<-sample(greedy_choice_ix, 1)
    }
    
    # If this gets me anywhere, then do it
    if (max(score)>current_score)
    {
      state<-ps[[greedy_choice_ix]]
      a_states[[length(a_states)+1]]<-state$active
      actions[[length(actions)+1]]<-names(f)[prospective_actions[[greedy_choice_ix]]]
      scores[t]<-score[greedy_choice_ix]
      break
      
    } else if (seq_length>max_depth) {
      #If i have timed out and failed in this loop-through then reset the search to greedy again
      # a_states[[length(a_states)+1]]<-state$active
      # actions[[length(actions)+1]]<-''
      scores[t]<-score[greedy_choice_ix]
      cat('search failed')
      break
    } else {
      #Otherwise increase depth by 1 and keep trying
      seq_length<-seq_length+1
    }
    
  }# Progress
  
  if (scores[t]==full_points)
  {
    print('done!')
    break
  }
}

# state<-f$Lock(state)

cat('Got:', sum(state$active==1 & state$target==1), ' of ', sum(state$target),
    'with ', sum(state$active==1 & state$target==0), 'mistakes, with ', length(unlist(actions)),
    'actions, for a score of', sum(state$active==1 & state$target==1) - sum(state$active==1 & state$target==0) - length(unlist(actions))*0.1)

state$combination<-0
state$combination[state$active==0 & target==1]<-1
state$combination[state$active==1 & target==0]<-2
state$combination[state$active==1 & target==1]<-3

full_board$combination<-factor(rep(state$combination, each = 6), levels = 0:3, labels = c('empty','missed','wrong','correct'))
full_board$state<-factor(rep(state$active, each = 6), levels = 0:1, labels = c('inactive','active'))

# Fix colour scheme
ggplot(full_board, aes(x_pos, y_pos)) + 
  geom_polygon(aes(group = id, fill=combination, colour=state)) +
  scale_colour_manual(values = c('gray','black'), drop = F) +
  scale_fill_manual(values = c('white','yellow','pink','lightgreen'), drop = F)



# ##################################################
# Generate a bunch of test sequences without caching
depth<-5
N<-100
basic_targets_d5<-matrix(NA, nrow=61, ncol=N)
basic_targets_d5_solutions<-list()


for (i in 1:100)
{
  # Basic functionality is to chain these together, calculating a loss and keeping a history
  state<-empty_board %>% select(-x_pos, -y_pos) %>% mutate(target = 0, active=0)
  
  # Generate a random sequence (in which every step actually does something)
  d<-1
  a_states<-matrix(NA, nrow=nrow(state), ncol=depth)#vector(mode = "list", length = depth)
  actions<-rep(NA, length = depth)
  while (d<(depth+1))
  {
    action<-sample(length(f), 1)
    tmp<-f[[action]](state)
    if (!all(tmp$active==state$active))
    {
      state<-tmp
      a_states[,d]<-state$active
      actions[d]<-names(f)[action]
      d<-d+1
    }
  }
  basic_targets_d5[,i]<-state$active
  basic_targets_d5_solutions[[i]]<-actions
}

# Mega-plot time
p<-fb<-list()
for (i in 1:N)
{
  fb[[i]]<-full_board
  fb[[i]]$state<-factor(rep(basic_targets_d5[,i], each = 6))
  p[[i]]<-ggplot(fb[[i]], aes(x_pos, y_pos)) + 
    geom_polygon(aes(group = id, fill=state), colour = 'black') +
    scale_fill_manual(values = c('white','yellow')) +
    ggtitle(paste0(basic_targets_d5_solutions[[i]], collapse=' '))

}

p_save<-grid.arrange(grobs = p, nrow = round(sqrt(N)))
ggsave(p_save, filename='targets.pdf', width = 50, height = 40, limitsize = F)
