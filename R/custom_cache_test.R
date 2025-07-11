# generate_test_sets
rm(list=ls())

library(tidyverse)
library(ggplot2)
library(gridExtra) #For plotting lots of things at once

# source('hex_setup.R')
load('./dat/hexsetup.rdata', verbose = T)

custom_cache = c("AddBar", "AddCorner", "RemoveUnit")  #triangle
#custom_cache = c("AddUnit", "ShiftSE", "AddUnit", "ShiftNE", "AddUnit")  #triangle
#custom_cache = c("AddCorner", "ShiftSE", "AddUnit") #hazard
#custom_cache = c("AddBar", "RotateClockwise", "AddBar")  #dabone

ccache_only = FALSE  
ccache = TRUE



# ##################################################
# Generate a bunch of test sequences without caching

set.seed(27122000) #changed for random generation

depth <- 7
N <- if (ccache) 1 else 9  

recursive_targets_d3 <- list()
recursive_targets_d3_solutions <- list()

for (cache in 1:N) {
  recursive_targets_d3[[cache]] <- matrix(NA, nrow = 61, ncol = 10)
  recursive_targets_d3_solutions[[cache]] <- list()
  
  for (example in 1:10) {
    state <- empty_state %>% select(-x_pos, -y_pos) %>% mutate(target = 0, active = 0)
    d <- 1
    a_states <- matrix(NA, nrow = nrow(state), ncol = depth)
    actions <- list()
    
    max_attempts = 50
    attempts = 0
    
    while (d < (depth + 1) && attempts < max_attempts) {
      attempts = attempts + 1
      
      use_cache =  ccache_only || runif(1) < 0.5
      
      if (use_cache) {
        tmp_state <- apply_cache(state, custom_cache)
        action <- paste0("Cache")
        cat('cache', cache, 'example', example, 'USING CUSTOM CACHE\n')
      } else {
        action <- names(f)[sample(length(f), 1)]
        tmp_state <- f[[action]](state)
        cat('cache', cache, 'example', example, 'primitive\n')
      }
      
      loop_check <- FALSE
      
      if (d == 1 && all(tmp_state$active == 0)) {
        loop_check <- TRUE
      }
      
      if (d > 1) {
        for (lc in 1:(d - 1)) {
          if (all(tmp_state$active == a_states[, lc])) {
            loop_check <- TRUE
            break
          }
        }
      }
      
      if (!loop_check) {
        state <- tmp_state
        a_states[, d] <- state$active
        actions[[d]] <- action
        d <- d + 1
        attempts <- 0  
      }
    }
    
    
    recursive_targets_d3[[cache]][, example] <- state$active
    recursive_targets_d3_solutions[[cache]][[example]] <- actions
  }
}




# Mega-plot time
for (cache in 1:N) {
  p <- fb <- list()
  
  tmp <- board_polygons
  tmp$state <- factor(rep(if (ccache) {
    apply_cache(empty_state %>% mutate(active = 0), custom_cache)$active
  } else {
    basic_targets_d3[, cache]
  }, each = 6))
  
  p[[1]] <- ggplot(tmp, aes(x_pos, y_pos)) +
    geom_polygon(aes(group = id, fill = state), colour = 'black') +
    scale_fill_manual(values = c('white', 'blue')) +
    scale_y_reverse() +
    ggtitle(if (ccache) "Custom Cache Primitive" else "Cached primitive")
  
  for (example in 1:10) {
    fb[[example]] <- board_polygons
    fb[[example]]$state <- factor(rep(recursive_targets_d3[[cache]][, example], each = 6))
    p[[example + 1]] <- ggplot(fb[[example]], aes(x_pos, y_pos)) +
      geom_polygon(aes(group = id, fill = state), colour = 'black') +
      scale_fill_manual(values = c('white', 'yellow')) +
      ggtitle(paste0(recursive_targets_d3_solutions[[cache]][[example]], collapse = ' '))
  }
  
  top_title <- if (ccache) {
    paste0("Custom Cache: ", paste(custom_cache, collapse = ", "))
  } else {
    paste0("Cache sequence: ", paste(basic_targets_d3_solutions[[cache]], collapse = " "))
  }
  
  p_save <- grid.arrange(grobs = p, nrow = 2, top = top_title)
  
  filename <- if (ccache) {
    paste0('./plot/recursive_targets_custom_', paste(custom_cache, collapse = "_"), '.pdf')
  } else {
    paste0('./plot/recursive_targets', cache, '.pdf')
  }
  
  ggsave(p_save, filename = filename, width = 30, height = 10, limitsize = FALSE)
}



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
