rm(list=ls())

library(tidyverse)
library(ggplot2)

#########
# SETUP
#########

N <- 4 #Hexagon radius (excluding centre, other stuff should scale with this)



# action_displays = ['W','A','D','F', '&#8629 ','&#8736',
#                        'E','R','S','Z','X','C']
# action_keys = c('W','A','D','F','L','K',
#                 'E','R','S','Z','X','C')
# if (cache)
# {
#   primitive_keycodes = c(87, 65, 68, 70, 13, 32)
#   cachable_keycodes = c(69,82,83,90, 88, 67)
#   
#   # action_keys =     ['W','A','D','F','L','K']
#   action_keycodes = c(87, 65, 68, 70, 13, 32)#76, 75
#   # action_displays = ['W','A','D','Z', '&#8629','&#8736']
#   
#   console.log(cachable_keycodes)
#   
# } else {
#   primitive_keycodes = c(87, 65, 68, 70, 13, 32,
#                          69,82,83,90, 88, 67)
#   
#   action_keycodes = c(87, 65, 68, 70, 13, 32,
#                       69,82,83,90, 88, 67)#76, 75
#   
# }


# Core HEX functions

#Cube-coordinate directional vectors
cdv <- list(NE=c(1, -1, 0),
            E=c(1, 0, -1),
            SE=c(0, 1, -1),
            SW=c(-1, 1, 0),
            W=c(-1, 0, +1),
            NW=c(0, -1, +1))
#Convert cube coordinates q and r to xy
cube_to_oddr<-function(q,r)
{
  col = q + (r - bitwAnd(r,1)) / 2
  row = r
  data.frame(x_id=col, y_id=row)
}
# And back again
oddr_to_cube <- function(x,y)
{
  q = x - (y - bitwAnd(y,1)) / 2
  r = y
  s = -q-r
  data.frame(q=q, r=r, s=s)
}

# Make the board/state
make_hex_lattice = function(N, dist=1) {
  nx<-ny<-2*N+1
  origin<-c(-N,-N)
  offset<-c(0, rep(c(0.5,0), N))
  
  loc <- data.frame(x_pos = sort(c(rep(seq(from=0, by=dist, length.out=nx),each=ceiling(ny/2)),
                                            rep(seq(from=dist/2, by=dist, length.out=nx),
                                                each=floor(ny/2)))) + origin[1],
                          y_pos = rep(c(seq(from=0, by = dist*sqrt(3), length.out=ceiling(ny/2)),
                                           seq(from=dist*sqrt(3)/2, by=dist*sqrt(3),
                                               length.out=floor(ny/2))) + origin[2], times=nx))
  
  loc<-loc %>% mutate(x = rep(-N:N, each=ny),
                                  y = rep(c(seq(-N,N, by=2),seq((-N+1),N, by=2)), nx))
  
  loc <- cbind(loc, oddr_to_cube(loc$x, loc$y))
  
  loc<-loc %>% arrange(y, x) %>%
    filter(abs(q)<5 & abs(r)<5 & abs(s)<5) %>% mutate(id = 1:n()) %>%
  select(id, x, y, q,r,s, x_pos, y_pos)

  return(loc)
}

# Extend state description to include all polygon vertices
make_hex_coords  = function(loc) {
  # Separate x and y coordinates
  lx = loc$x_pos # x-coordinates
  ly = loc$y_pos # y-coordinates      
  # Plot hexagonal lattice as filled hexagons
  hex_x = data.frame(loc, cbind(lx + 0, lx + 0.5, lx + 0.5, lx + 0, lx - 0.5, lx - 0.5))
  hex_y = data.frame(loc, cbind(ly - 1/(sqrt(3)), ly - 1/(2*sqrt(3)), ly + 1/(2*sqrt(3)), ly + 1/(sqrt(3)), ly + 1/(2*sqrt(3)), ly - 1/(2*sqrt(3))))
  
  #Get your coordinates in long format with an id
  hexdat <- hex_x %>% gather(vertice, x_pos, X1:X6) %>% arrange(id, vertice)
  hexdat_y <-  hex_y %>% gather(vertice, y_pos, X1:X6) %>% arrange(id, vertice)
    #Merge them into the same dataframe
  hexdat$y_pos <- hexdat_y$y_pos

  return(hexdat)
}


# Create the board
empty_board<-make_hex_lattice(N)
full_board<-make_hex_coords(empty_board)

# Amortise the directional transformation vectors
# ################################################
cdv_amort<-list()
for (dir in 1:6)
{
  cdv_amort[[dir]]<-data.frame(from=1:nrow(empty_board), to = rep(NA, nrow(empty_board)))
  for (i in 1:nrow(empty_board))
  {
    tmp<-which(empty_board$q==empty_board$q[i]+cdv[[dir]][1] &
         empty_board$r==empty_board$r[i]+cdv[[dir]][2] &
         empty_board$s==empty_board$s[i]+cdv[[dir]][3])
    if (any(tmp))
    {
      cdv_amort[[dir]]$to[i]<-tmp
    }

  }
  cdv_amort[[dir]]<-cdv_amort[[dir]][!is.na(cdv_amort[[dir]]$to),] #Remove the indices that go off the board
}

rot_amort<-data.frame(from=1:nrow(empty_board), to = rep(NA, nrow(empty_board)))
for (i in 1:nrow(empty_board))
{
  tmp<-which(empty_board$q==-empty_board$q[i] &
               empty_board$r==-empty_board$r[i] &
               empty_board$s==-empty_board$s[i])
  if (any(tmp))
  {
    rot_amort$to[i]<-tmp
  }
}

flip_amort<-list()
for (axis in 1:3)
{
  flip_amort[[axis]]<-data.frame(from=1:nrow(empty_board), to = rep(NA, nrow(empty_board)))
  
  for (i in 1:nrow(empty_board))
  {
    if (axis==1)
    {
    tmp<-which(empty_board$q==empty_board$q[i] &
                 empty_board$r==empty_board$s[i] &
                 empty_board$s==empty_board$r[i])
    }
    
    if (axis==2)
    {
      tmp<-which(empty_board$q==empty_board$s[i] &
                   empty_board$r==empty_board$r[i] &
                   empty_board$s==empty_board$q[i])
    }
    
    if (axis==3)
    {
      tmp<-which(empty_board$q==empty_board$r[i] &
                   empty_board$r==empty_board$q[i] &
                   empty_board$s==empty_board$s[i])
    }
    
    if (any(tmp))
    {
      flip_amort[[axis]]$to[i]<-tmp
    }
  }
}

###################
# Create action set
# ##############
f<-list() 

f$AddUnit<-function(ts)
{
  #Add unit in centre
  # this_state[N+1,N+1] <- 1
  # ts$active[ts$x==0, ts$y==0] <- 1
  ts$active[31]<-1 #Amortised for speed!
  ts
}

f$RemoveUnit<-function(ts)
{
  #Add unit in center
  # ts$active[ts$x==0, ts$y==0] <- 0
  ts$active[31]<-0
  ts
}

f$AddBar<-function(ts)
{
  #Add horizontal bar
  ts$active[c(30, 31, 32)] <- 1
  #ts$x==-1 & ts$y==0
  #ts$x==0 & ts$y==0
  ts
}

f$AddCorner<-function(ts)
{
  #Add horizontal bar
  ts$active[c(24,31,32)] <- 1#ts$x==1 & ts$y==-1
  ts
}

f$RotateClockwise<-function(ts)
{
  os<-ts
  ts$active<-0
  ts$active[rot_amort$to]<-os$active[rot_amort$from]
  ts
}

f$Flip<-function(ts, axis=1)
{
  os<-ts
  ts$active<-0
  ts$active[flip_amort$to]<-os$active[flip_amort$from]
  ts
}


f$ShiftNE<-function(ts, dir = 1)
{
  os<-ts
  ts$active<-0
  ts$active[cdv_amort[[dir]]$to]<-os$active[cdv_amort[[dir]]$from]
  ts
}

f$ShiftE<-function(ts, dir = 2)
{
  os<-ts
  ts$active<-0
  ts$active[cdv_amort[[dir]]$to]<-os$active[cdv_amort[[dir]]$from]
  ts
}

f$ShiftNW<-function(ts, dir = 3)
{
  os<-ts
  ts$active<-0
  ts$active[cdv_amort[[dir]]$to]<-os$active[cdv_amort[[dir]]$from]
  ts
}

f$ShiftSE<-function(ts, dir = 4)
{
  os<-ts
  ts$active<-0
  ts$active[cdv_amort[[dir]]$to]<-os$active[cdv_amort[[dir]]$from]
  ts
}

f$ShiftW<-function(ts, dir = 5)
{
  os<-ts
  ts$active<-0
  ts$active[cdv_amort[[dir]]$to]<-os$active[cdv_amort[[dir]]$from]
  ts
}

f$ShiftNW<-function(ts, dir = 6)
{
  os<-ts
  ts$active<-0
  ts$active[cdv_amort[[dir]]$to]<-os$active[cdv_amort[[dir]]$from]
  ts
}

f$Lock<-function(ts)
{
  # Special function because it shifts the active state to the locked state for which we get the scored loss
  # Should I change this since we want every trial to come with a loss even if not yet locked?
  # I guess we could pull the scoring out and make the loss not dependent on things being locked
  # or make it that locking gives points and clears the hexes, such that the loss of the active state is just about the un-solved hexes
  ts$locked<-pmax(ts$locked, ts$active)
  ts$active<-0
  ts
}



# 
# Action <- function(keycode, this_state=state, real=T, midcache = F)
# {
#   # Basically a wrapper that executes a function, writes it to the results and triggers visualisation update...
#   if (keycode==76 | keycode==13){this_state<-Lock(this_state, real)}
#   if (keycode==75 | keycode==32){this_state<-RotateClockwise(this_state, real)}
# 
#   if (keycode==87){this_state<-Shift('W', this_state, real)}#w
# 
#   if (keycode==65){this_state<-AddUnit(this_state, real)}#a
#   if (keycode==68){this_state<-RemoveUnit(this_state, real)}#SW
#   if (keycode==70){this_state<-Flip(axis=1, this_state, real)}
# 
# 
#   if (cache)# & real
#   {
#       if (keycode==69){this_state<-UseCache(1, this_state, real)}#Shift('E', this_state, real)}#e->EAST
#       if (keycode==82){this_state<-UseCache(2, this_state, real)}#Shift('NE', this_state, real)}#r->NW
#       if (keycode==83){this_state<-UseCache(3, this_state, real)}#AddCorner(this_state, real)}#s
#       if (keycode==90){this_state<-UseCache(4, this_state, real)}#AddBar(this_state, real)}#z
#       if (keycode==88){this_state<-UseCache(5, this_state, real)}#Shift('SW', this_state, real)}#x
#       if (keycode==67){this_state<-UseCache(6, this_state, real)}#Shift('SE', this_state, real)}#c
#   } else {
#       if (keycode==69){this_state<-Shift('E', this_state, real)}
#       if (keycode==82){this_state<-Shift('NW', this_state, real)}
#       if (keycode==83){this_state<-Shift('SW', this_state, real)}
#       if (keycode==90){this_state<-AddCorner(this_state, real)}
#       if (keycode==88){this_state<-AddBar(this_state, real)}
#       if (keycode==67){this_state<-Shift('SE', this_state, real)}
#   }
# 
#   if (real & !midcache & (keycode%in%primitive_keycodes | keycode%in%c(81,75,76)))
#   {
#       cat('got here')
#       actions[length(actions)+1]<-keycode
#       
#       results$action_history[length(results$action_history)+1]<-actions
#       results$state_history[length(results$tate_history)+1]<-this_state
# 
#       #Update the visual (player's) state
#       Update()
#   }
#   
#   this_state
# }
#   
  
#   
# Update <-function() {
#   # Draws the hex board....  Will need to integrate with R plotting
#   
#   # Loop over the raw xy locations
#   for (y in 1:10)
#   {
#     for (x in seq(y%%2+1, 18, by=2))
#     {
#       #Convert to cube coordinate
#       these_cube_coords = oddr_to_cube(floor((x-2*N)/2),y-N)
#       # oddr_to_cube(floor(prx[x]/2),pry[y])
#       
#       #If we're inside the hexagon
#           if(abs(these_cube_coords[1])<5 & abs(these_cube_coords[2])<5 & abs(these_cube_coords[3])<5)
#           {
#               #If the square is already locked in...
#               if (locked_state[these_cube_coords[1]+N, these_cube_coords[2]+N]==1)
#               {
#                   #And if the locked in state is correct
#                   if (target[these_cube_coords[1]+N, these_cube_coords[2]+N]==1)
#                   {
#                       print('draw strong green')
#                       # d.draw(x, y, null, null, "#090") #Strong Green
#                   } else {
#                       #Otherwise
#                       print('draw red')
#                       # d.draw(x, y, null, null, "#900")#Red
#                   }
# 
#               } else {
#                   #If state is not already not locked in but is part of the target
#                   if (target[these_cube_coords[1]+N, these_cube_coords[2]+N]==1)
#                   {
#                     print('draw light green')
#                       # d.draw(x, y, null, null, "#9e9") #Light green
#                   } else {
#                       #Otherwise if unoccupied, non target and not locked in it is white
#                     print('draw white')
#                      # d.draw(x, y, null, null, "white")
#                   }
#               }
#               
#               # If the state is actively occupied
#               if (state[these_cube_coords[1]+N, these_cube_coords[2]+N]==1)
#               {
#                   #And part of the pattern
#                   if (target[these_cube_coords[1]+N, these_cube_coords[2]+N]==1)
#                   {
#                       print('draw green, black dot?')
#                       # d.draw(x, y, "•", "black", "#595")#"#595"
#                   } else {
#                     print('draw gray, black dot?')
#                     # d.draw(x, y, "•", "black",  "#999")
#                   }
#               } 
#           }
#       }
#   }
# 
#     # UpdateStringVis()
# }
# 


UseCache<-function(n, this_state, this_real)
{    
  #Read the cached pattern TODO KEEP THIS NOT IN THE STRING
  cache_pattern <- library[n]#Array.from($('#cache' + n).text())
  cat('before expanding:', cache_pattern)
  
  #Get rid of any nesting before the display process begins
  
  while (cachable_keycodes[1] %in% cache_pattern |
         cachable_keycodes[2] %in% cache_pattern | 
         cachable_keycodes[3] %in% cache_pattern | 
         cachable_keycodes[4] %in% cache_pattern | 
         cachable_keycodes[5] %in% cache_pattern | 
         cachable_keycodes[6] %in% cache_pattern ) {
    
    for (i in 1:length(cache_pattern))
    {
      for (j in 1:length(cachable_keycodes))
      {
        if (cache_pattern[i]==cachable_keycodes[j])
        {
          if (i==1)
          {
            cache_pattern<-c(library[[j]], cache_pattern[2:length(cache_pattern)])
          } else if (i==length(cache_pattern))
          {
            cache_pattern<-c(cache_pattern[1:(i-1)], library[[j]])
          } else {
            cache_pattern<-c(cache_pattern[1:(i-1)], library[[j]], cache_pattern[(i+1):length(cache_pattern)])
          }

          #MAY NEED TO CHECK IF ITS FIRST OR LAST
          # cache_pattern.splice(i, 1)
          # insertArrayAt(cache_pattern, i, library[j])
          break
        }
      }
    }
  }
  #How long is the fully expanded string?
    cpl = length(cache_pattern)
  
  #Feed the string to the Action function with short timeouts so you can see it play out
  cat('after expanding:', cache_pattern)
  
  
  for (i in 1:length(cache_pattern))
  {
    Action(cache_pattern[i], this_state=this_state, real=this_real, midcache = T)
  }
  if (this_real)
  {
    Update()
    actions[length(actions)+1]<-cachable_keycodes[n]
    # UpdateStringVis()
  }
}  
  
##########
#START
##########
  
# REMOVE THE GENERATION/SOLUTION DISTINCTION, WE NEED TO BE ABLE TO GENERATE PROCEDURALLY AND COMPARE WITH A TARGET BUT CAN STORE THESE DIFFERENTLY
#TODO: GET THE BASIC OPERATIONS WORKING PROPERLY BOTH AS GENERATION AND FOR SOLUTION

# Basic functionality is to chain these together, calculting a loss and keeping a history


state<-empty_board %>% select(-x_pos, -y_pos) %>% mutate(target = 0, active=0, locked=0)

# Generate a random sequence
depth<-100
a_states<-l_states<-matrix(NA, nrow=nrow(state), ncol=depth)#vector(mode = "list", length = depth)
actions<-rep(NA, length = depth)

for (d in 1:depth)
{
  action<-sample(length(f), 1)
  state<-f[[action]](state)
  
  a_states[,d]<-state$active
  l_states[,d]<-state$locked
  actions[d]<-names(f)[action]
}
state<-f$Lock(state)

colSums(a_states)
actions

# Append the state to the board and plot it
full_board$state<-factor(rep(state$locked, each = 6))
ggplot(full_board, aes(x_pos, y_pos)) + 
  geom_polygon(aes(group = id, fill=state), colour = 'black') +
  scale_fill_manual(values = c('white','lightgreen'))


target<-state$locked

# Depth 1 planner
state<-empty_board %>% select(-x_pos, -y_pos) %>% mutate(target = 0, active=0, locked=0)
state$target<-target

full_point<-sum(state$target)
full_board$target<-factor(rep(state$target, each = 6))
ggplot(full_board, aes(x_pos, y_pos)) + 
  geom_polygon(aes(group = id, fill=target), colour = 'black') +
  scale_fill_manual(values = c('white','lightyellow'))

depth<-100
a_states<-l_states<-matrix(NA, nrow=nrow(state), ncol=depth)#vector(mode = "list", length = depth)
actions<-scores<-rep(NA, length = depth)
for (d in 1:depth)
{
  ps<-list()
  score<-rep(NA, length(f))
  for (pa in 1:length(f))
  {
    ps[[pa]]<-f[[pa]](state)
    # How many points would I have if I did this
    score[pa]<-sum(ps[[pa]]$active==1 & ps[[pa]]$target==1 & ps[[pa]]$locked==0)*0.1 +
               -sum(ps[[pa]]$active==1 & ps[[pa]]$target==0)*0.1 + 
                sum(ps[[pa]]$locked==1 & ps[[pa]]$target==1) + 
                -sum(ps[[pa]]$locked==1 & ps[[pa]]$target==0)
  }
  greedy_choice_ix<-which(score==max(score))
  
  if (length(greedy_choice_ix)>1)
  {
    greedy_choice_ix<-sample(greedy_choice_ix, 1)
  }
  
  state<-f[[greedy_choice_ix]](state)
  
  a_states[,d]<-state$active
  l_states[,d]<-state$locked
  actions[d]<-names(f)[greedy_choice_ix]
  scores[d]<-score
}
# state<-f$Lock(state)

cat('Got:', sum(state$locked==1 & state$target==1), ' of ', sum(state$target),
    'with ', sum(state$locked==1 & state$target==0), 'mistakes')

state$combination<-0
state$combination[state$locked==0 & target==1]<-1
state$combination[state$locked==1 & target==0]<-2
state$combination[state$locked==1 & target==1]<-3

full_board$combination<-factor(rep(state$combination, each = 6), levels = 0:3, labels = c('empty','missed','wrong','correct'))
full_board$state<-factor(rep(state$active, each = 6), levels = 0:1, labels = c('inactive','active'))
# Fix colour scheme
ggplot(full_board, aes(x_pos, y_pos)) + 
  geom_polygon(aes(group = id, fill=combination, colour=state)) +
  scale_colour_manual(values = c('gray','black'), drop = F) +
  scale_fill_manual(values = c('white','yellow','pink','lightgreen'), drop = F)

# Could add an approach reward for if active state could get 'closer' to remaining targets, sum of squared errors or something... but getting off track ehre, maybe remove lock!


# Lock is playing havok because it gets stuck with a lot of active stuff it can't bin
# What might fix this problem? remove lock altogether?
# Make Lock expensive but the rewards for locking more valuable than active state?
# Just rely on learned sequences?

# Let's try and do away with lock altogther and see what happens?

#' if (mode=='procedural')
#' {
#'   target <- ProceduralPattern(gen_depth)
#' } else {
#'   target <- ChallengePattern(pattern)
#'   # console.log(pattern)
#'   #'djdjdwuuuedjdjdwnnedjdjdwkkk' -previously
#'   #fxfxfswrwrwrtfxfxfscctfxfxfseeet
#'   #xkxkxdrrrlxkxkxdsslxkxkxdeeel -- e.g. three circles XKXKXDRRRLXKXKXDSSLXKXKXDEEEL
#'   #zfzsslzfzccclzfzrrrlzfzkeelzfzkkwwwlzfzkkkkeerrl circle of bones
#' }

# console.log(gen_depth, 'mode', mode, ' ', pattern)
# results$target <- target #Store the target pattern
# 
# if (cache)
# {
#   results$library_history <-library
# }

#VISUALISATION

#Plot the hexagons onto it
# for (y in 1:(2*N+1))
# {
#   for (x in seq(y%%2+1, 2*(2*N+1), by=2))
#   {
#     cat('xy', x, y, 'centered', prx[x], pry[y], oddr_to_cube(floor(prx[x]/2),pry[y]), '\n')
#         #console.log('xy', x, y, 'centered', prx[x], pry[y], oddr_to_cube(floor(prx[x]/2),pry[y]))
#         # Note the coordinates, every odd row is offset to the right doubling the number of x coordinates needed
#         
#     these_cube_coords = oddr_to_cube(floor((x-2*N)/2),y-N)
#     
#     #If we're inside the hexagon
#     if(abs(these_cube_coords[1])<5 & abs(these_cube_coords[2])<5 & abs(these_cube_coords[3])<5)
#     {
#       #Uncomment to view coordinates
#       # raw_xy_string = x.toString()+','+y.toString()
#       # xy_string = (x-2*N).toString()+','+(y-N).toString()
#       qrs_string = paset0(these_cube_coords[1], these_cube_coords[2], sep=',')#why do this?+','+these_cube_coords[2].toString()
#       
#       if (target[these_cube_coords[1]+N, these_cube_coords[2]+N]==1)
#       {
#         # d.draw(x, y, null, null, "#9e9")
#         print('draw green')
#       } else {
#         # d.draw(x, y, null, null, "#fff")
#         print('draw white')
#       }
#       
#     }
#         
#   }
# }
  
#SETS UP INPUT input.
# window.addEventListener("keypress", function(e) {
#     #Workaround to avoid spacebar scrolling to bottom of page
#     if (e.which == 32 | e.which == 13) {
#         e.preventDefault()
#     }
#     code = e.charCode
# 
#     if (code>=97 && code<=122)
#     {
#         code = code-32#Hack to convert lower case keycodes to upper case
#     }
#     ch = String.fromCharCode(code)
#     console.log(ch)
#     console.log("Key char is: " + ch + " Code is: " + code)
#     Action(code)
# })

#SETS UP CACHING ACTIONS (INCLUDING SOME OF THE CHECKING LOGIC)
# if (cache)
# {
#     #Functionality for using caches
#     $(".cache-btn").click(function(){
#         tmp = this.id
#         console.log('clicked', this.id, tmp.charAt(tmp.length-1))
#         which_cache = Number(tmp.charAt(tmp.length-1))
# 
#         # console.log('hihi', actions, which_cache, cachable_keycodes,
#             # cachable_keycodes[which_cache], actions.includes(cachable_keycodes[which_cache]))
# 
#         if (!actions.includes(cachable_keycodes[which_cache]))
#         {
#             library[which_cache] = _.cloneDeep(actions)
#             actions = [cachable_keycodes[which_cache]]
# 
#             UpdateCacheVis(which_cache)
#             UpdateStringVis()
#             results.library_history.push(_.cloneDeep(library))
#             results.library_update_at.push(results.action_history.length)
#         } else {
#             alert('Cannot cache a sequence that contains itself!')
#         }
#     })
# 
#     for (let i=0 i<library.length i++)
#     {
#         UpdateCacheVis(i)
#     }
# }

  
          