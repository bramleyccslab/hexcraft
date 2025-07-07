rm(list=ls())

library(tidyverse)
library(ggplot2)

#########
# SETUP
#########

cache<-F#Mode of entire task...
mid_cache = F #Marker for behaviour when in the middle of caching something...?

N <- 4 #Hexagon radius (excluding centre, other stuff should scale with this)
state <- locked_state<-empty_state <- matrix(0, nrow=2*N+1, ncol=2*N+1)
# target_lock_tmp <-target <-locked_state <- old_state <- 


spillage <- 0#tracks false positives in solutions
gen_depth <- 20 #How deep to generate random example
mode <- 'procedural' #'challenge'

cdv <- list(NE=c(1, -1, 0),
            E=c(1, 0, -1),
            SE=c(0, 1, -1),
            SW=c(-1, 1, 0),
            W=c(-1, 0, +1),
            NW=c(0, -1, +1))#Cube-coordinate directional vectors

actions <- c()#[] #Store the sequence of actions the user performs

library <- list(c(32, 32, 32, 87, 32, 32, 32), #East
                c(32,32,32,32,32,87,32), #NE
                c(32,87,32,32,32,32,32),#SW
                c(65, 87, 65, 69, 82, 69, 65, 83),#Corner65, 87, 65, 69, 82, 65, 83]
                c(65, 82, 65, 83, 69, 83, 69, 65, 82))#Bar65, 87, 82, 65, 83, 83, 69, 69, 65, 87, 82

#Store the action sequences they cache
results <- list(target=c(), performance=c(), action_history=c(), state_history=c(), library_history=c(), library_update_at=c(0))

pattern <- action_keys <-action_keycodes <- primitive_keycodes <-display_array <-cachable_keycodes <- c()


# action_displays = ['W','A','D','F', '&#8629 ','&#8736',
#                        'E','R','S','Z','X','C']
action_keys = c('W','A','D','F','L','K',
                'E','R','S','Z','X','C')

if (cache)
{
  primitive_keycodes = c(87, 65, 68, 70, 13, 32)
  cachable_keycodes = c(69,82,83,90, 88, 67)
  
  # action_keys =     ['W','A','D','F','L','K']
  action_keycodes = c(87, 65, 68, 70, 13, 32)#76, 75
  # action_displays = ['W','A','D','Z', '&#8629','&#8736']
  
  console.log(cachable_keycodes)
  
} else {
  primitive_keycodes = c(87, 65, 68, 70, 13, 32,
                         69,82,83,90, 88, 67)
  
  action_keycodes = c(87, 65, 68, 70, 13, 32,
                      69,82,83,90, 88, 67)#76, 75
  
}



# Core HEX functions
cube_to_oddr<-function(q,r)
{
  col = q + (r - bitwAnd(r,1)) / 2
  row = r
  data.frame(x_id=col, y_id=row)
}

oddr_to_cube <- function(x,y)
{
  q = x - (y - bitwAnd(y,1)) / 2
  r = y
  s = -q-r
  data.frame(q=q, r=r, s=s)
}

# function oddr_to_axial(hex):
#   var q = hex.col - (hex.row - (hex.row&1)) / 2
# var r = hex.row
# return Hex(q, r)


##########
# PLOTTING
##########

MakeHexLattice = function(N, dist=1) {
  nx<-ny<-2*N+1
  origin<-c(-N,-N)
  offset<-c(0, rep(c(0.5,0), N))
  
  locations <- data.frame(x_abs = sort(c(rep(seq(from=0, by=dist, length.out=nx),each=ceiling(ny/2)),
                                            rep(seq(from=dist/2, by=dist, length.out=nx),
                                                each=floor(ny/2)))) + origin[1],
                          y_abs = rep(c(seq(from=0, by = dist*sqrt(3), length.out=ceiling(ny/2)),
                                           seq(from=dist*sqrt(3)/2, by=dist*sqrt(3),
                                               length.out=floor(ny/2))) + origin[2], times=nx))
  
  locations<-locations %>% mutate(x = rep(-N:N, each=ny),
                                  y = rep(c(seq(-N,N, by=2),seq((-N+1),N, by=2)), nx))
  
  locations <- cbind(locations, oddr_to_cube(locations$x, locations$y))
  
  locations<-locations %>% arrange(y, x) %>%
    mutate(id = 1:nrow(locations), in_bound = abs(q)<5 & abs(r)<5 & abs(s)<5) %>% 
  select(id, x, y, q,r,s, in_bound, x_abs, y_abs)

  return(locations)
}

MakeHexCoords  = function(loc) {
  # Separate x and y coordinates
  lx = loc$x_abs # x-coordinates
  ly = loc$y_abs # y-coordinates      
  # Plot hexagonal lattice as filled hexagons
  hex_x = data.frame(loc, cbind(lx + 0, lx + 0.5, lx + 0.5, lx + 0, lx - 0.5, lx - 0.5))
  hex_y = data.frame(loc, cbind(ly - 1/(sqrt(3)), ly - 1/(2*sqrt(3)), ly + 1/(2*sqrt(3)), ly + 1/(sqrt(3)), ly + 1/(2*sqrt(3)), ly - 1/(2*sqrt(3))))
  
  #Get your coordinates in long format with an id
  hexdat <- hex_x %>% gather(vertice, x_abs, X1:X6) %>% arrange(id, vertice)
  hexdat_y <-  hex_y %>% gather(vertice, y_abs, X1:X6) %>% arrange(id, vertice)
    #Merge them into the same dataframe
  hexdat$y_abs <- hexdat_y$y_abs

  return(hexdat)
}


# Create the board
loc<-MakeHexLattice(N)
board<-MakeHexCoords(loc)

# Create action set

f<-list() #Let's keep the functions in a list

f$AddUnit<-function(this_state)
{
  #Add unit in centre
  this_state[N+1,N+1] <- 1
  
  this_state
}

f$RemoveUnit<-function(this_state)
{
  #Add unit in centre
  this_state[N+1,N+1] <- 0
  
  this_state
}

f$AddBar<-function(this_state)
{
  #Add horizontal bar
  this_state[N+1,N] <- 1
  this_state[N+1,N+1] <- 1
  this_state[N+1,N+2] <- 1
  
  this_state
}

f$AddCorner<-function(this_state)
{
  #Add horizontal bar
  this_state[N+2,N+1] <- 1
  this_state[N+1,N+1] <- 1
  this_state[N,N+2] <- 1
  
  this_state
}


f$Shift<-function(this_state, dir='E')
{
  old_state<-state
  
  sv = cdv[[dir]]#Shift vector
  #Loop over all the axial coordinates in the hexagon
  for (q in (-N+1):N)
  {
    for (r in (-N+1):N)
    {
      #Calculate s
      s <- -q-r
      
      #Calculate updated position
      qprime = q+sv[1]
      rprime = r+sv[2]
      sprime = s+sv[3]
      # console.log('qrs', q,r,s, 'qrsprime', qprime, rprime,sprime)
      #If new position is still on the map
      if (abs(qprime)<5 & abs(rprime)<5 & abs(sprime)<5)
      {
        #Write into the new position
        this_state[qprime+N, rprime+N] = old_state[q+N, r+N]#WILL NEED TO CHECK INDEXING
      }
      
      #Zero the cells left behind
      if (sv[1]!=0 & q==(-N*sv[1]))
      {
        # console.log('q zeroing', q, r)
        this_state[q+N, r+N]=0
      }
      if (sv[2]!=0 & r==(-N*sv[2]))
      {
        # console.log('r zeroing', q, r)
        this_state[q+N, r+N]=0
      }
      if (sv[2]!=0 & s==(-N*sv[2]))
      {
        # console.log('s zeroing', q, r)
        this_state[q+N, r+N]=0
      }
    }
  }
  
  this_state
}



f$RotateClockwise<-function(this_state)
{
  old_state<-state
  for (q in (-N+1):N)
  {
    for (r in (-N+1):N)
    {
      s=-q-r
      #120 degree rotations
      # q_rot= r
      # r_rot = s
      # s_rot = q
      
      #60 degree rotations
      #xx, yy, zz = -yy, -zz, -xx
      q_rot= -r
      r_rot = -s
      s_rot = -q
      if (abs(q)<=N & abs(r)<=N & abs(s)<=N)
      {
        #Stops things spinning out of the  hex and growing the array dims?
        this_state[q_rot+N, r_rot+N] = old_state[q+N, r+N] 
      }
      
    }
  }
  this_state
}

f$Flip<-function(this_state, axis=1)
{
  old_state<-state
  
  for (q in (-N+1):N)
  {
    for (r in (-N+1):N)
    {
      s=-q-r
      if (axis==1)
      {
        q_flip=q
        r_flip=s
        s_flip=r
        
      } else if (axis==2)
      {
        q_flip=s
        r_flip=r
        s_flip=q
      } else if (axis==3)
      {
        q_flip=r
        r_flip=q
        s_flip=s
      }
      
      if (abs(q)<=N & abs(r)<=N & abs(s)<=N)
      {
        #Stops things spinning out of the  hex and growing the array dims?
        this_state[q_flip+N, r_flip+N] <- old_state[q+N, r+N] 
      }
      
    }
  }
  this_state
}


Lock<-function(this_state, locked_state, real=T)
{
  # Special function because it shifts the active state to the locked state for which we get the scored loss
  # Should I change this since we want every trial to come with a loss even if not yet locked?
  # I guess we could pull the scoring out and make the loss not dependent on things being locked
  # or make it that locking gives points and clears the hexes, such that the loss of the active state is just about the un-solved hexes
  match <- T
  spillage <- 0
  
  for (q in (-N+1):N)
  {
    for (r in (-N+1):N)
    {
      s = -q-r
      if (this_state[q+N, r+N]==1)
      {
        # Main functionality of real, to remove?
        if (real)
        {
          locked_state[q+N, r+N]=1
        } else {
          target_lock_tmp[q+N, r+N]=1
        }
        
        this_state[q+N, r+N]=0
      }
      
      if (real & abs(q)<=N & abs(r)<=N & abs(s)<=N)
      {
        if (locked_state[q+N, r+N]==0 & target[q+N, r+N]==1)
        {
          cat('nonmatch', q,r)
          match = F 
        }
        
        if (locked_state[q+N, r+N]==1 & target[q+N, r+N]==0)
        {
          spillage<-spillage+1
        }
      }
    }
  }
  
  if (real)
  {
    if (match)
    {
      print('task completed!')
      # save_data()
    }
  }
  
}




Action <- function(keycode, this_state=state, real=T, midcache = F)
{
  # Basically a wrapper that executes a function, writes it to the results and triggers visualisation update...
  if (keycode==76 | keycode==13){this_state<-Lock(this_state, real)}
  if (keycode==75 | keycode==32){this_state<-RotateClockwise(this_state, real)}

  if (keycode==87){this_state<-Shift('W', this_state, real)}#w

  if (keycode==65){this_state<-AddUnit(this_state, real)}#a
  if (keycode==68){this_state<-RemoveUnit(this_state, real)}#SW
  if (keycode==70){this_state<-Flip(axis=1, this_state, real)}


  if (cache)# & real
  {
      if (keycode==69){this_state<-UseCache(1, this_state, real)}#Shift('E', this_state, real)}#e->EAST
      if (keycode==82){this_state<-UseCache(2, this_state, real)}#Shift('NE', this_state, real)}#r->NW
      if (keycode==83){this_state<-UseCache(3, this_state, real)}#AddCorner(this_state, real)}#s
      if (keycode==90){this_state<-UseCache(4, this_state, real)}#AddBar(this_state, real)}#z
      if (keycode==88){this_state<-UseCache(5, this_state, real)}#Shift('SW', this_state, real)}#x
      if (keycode==67){this_state<-UseCache(6, this_state, real)}#Shift('SE', this_state, real)}#c
  } else {
      if (keycode==69){this_state<-Shift('E', this_state, real)}
      if (keycode==82){this_state<-Shift('NW', this_state, real)}
      if (keycode==83){this_state<-Shift('SW', this_state, real)}
      if (keycode==90){this_state<-AddCorner(this_state, real)}
      if (keycode==88){this_state<-AddBar(this_state, real)}
      if (keycode==67){this_state<-Shift('SE', this_state, real)}
  }

  if (real & !midcache & (keycode%in%primitive_keycodes | keycode%in%c(81,75,76)))
  {
      cat('got here')
      actions[length(actions)+1]<-keycode
      
      results$action_history[length(results$action_history)+1]<-actions
      results$state_history[length(results$tate_history)+1]<-this_state

      #Update the visual (player's) state
      Update()
  }
  
  this_state
}
  
  
  
Update <-function() {
  # Draws the hex board....  Will need to integrate with R plotting
  
  # Loop over the raw xy locations
  for (y in 1:10)
  {
    for (x in seq(y%%2+1, 18, by=2))
    {
      #Convert to cube coordinate
      these_cube_coords = oddr_to_cube(floor((x-2*N)/2),y-N)
      # oddr_to_cube(floor(prx[x]/2),pry[y])
      
      #If we're inside the hexagon
          if(abs(these_cube_coords[1])<5 & abs(these_cube_coords[2])<5 & abs(these_cube_coords[3])<5)
          {
              #If the square is already locked in...
              if (locked_state[these_cube_coords[1]+N, these_cube_coords[2]+N]==1)
              {
                  #And if the locked in state is correct
                  if (target[these_cube_coords[1]+N, these_cube_coords[2]+N]==1)
                  {
                      print('draw strong green')
                      # d.draw(x, y, null, null, "#090") #Strong Green
                  } else {
                      #Otherwise
                      print('draw red')
                      # d.draw(x, y, null, null, "#900")#Red
                  }

              } else {
                  #If state is not already not locked in but is part of the target
                  if (target[these_cube_coords[1]+N, these_cube_coords[2]+N]==1)
                  {
                    print('draw light green')
                      # d.draw(x, y, null, null, "#9e9") #Light green
                  } else {
                      #Otherwise if unoccupied, non target and not locked in it is white
                    print('draw white')
                     # d.draw(x, y, null, null, "white")
                  }
              }
              
              # If the state is actively occupied
              if (state[these_cube_coords[1]+N, these_cube_coords[2]+N]==1)
              {
                  #And part of the pattern
                  if (target[these_cube_coords[1]+N, these_cube_coords[2]+N]==1)
                  {
                      print('draw green, black dot?')
                      # d.draw(x, y, "•", "black", "#595")#"#595"
                  } else {
                    print('draw gray, black dot?')
                    # d.draw(x, y, "•", "black",  "#999")
                  }
              } 
          }
      }
  }

    # UpdateStringVis()
}


      
RandomPattern<-function()
{
  target_pattern <-state
  
  for (x in 1:(2*N+1))
  {
    for (y in 1:(2*N+1))
    {
      these_cube_coords = oddr_to_cube(floor((x-2*N)/2),y-N)
      #oddr_to_cube(floor(prx[x]/2),pry[y])
      if (abs(these_cube_coords[1])<=N & abs(these_cube_coords[2])<=N & abs(these_cube_coords[3])<=N)
      {
        if (runif()>.5)
        {
          target_pattern[x,y]=1
        }
      }
    }  
  }
  target_pattern
}

      
ChallengePattern<-function(sequence)
{
  target_pattern <- state
  
  # seq_arr = Array.from(sequence) //Is this a charsplit?
  seq_arr<-strsplit(sequence, '')
  for (step in 1:length(sequence))
  {
    this_key = seq_arr[step]#.shift()
    tmp = which(action_keys==this_key)
    cat('keycode', this_key, tmp, c(primitive_keycodes,cachable_keycodes))
    
    Action(c(primitive_keycodes,cachable_keycodes)[tmp], target_pattern, F)
  }
  
  for (q in (-N+1):N)
  {
    for (r in (-N+1):N)
    {
      if (target_lock_tmp[q+N, r+N]==1)
      {
        target_pattern[q+N, r+N]=1
      } 
    }
  }
  
  target_pattern
}


ProceduralPattern<-function(depth)
{
  target_pattern <- state
  generation_procedure <-c()
  
  for (step in 1:depth)
  {
    this_key = c(primitive_keycodes,cachable_keycodes)[sample(length(c(primitive_keycodes,cachable_keycodes)), 1)]#ROT.RNG.getItem(action_keycodes.concat(cachable_keycodes))
    #Remove the cachable keycode concatenation to use prior
    generation_procedure[length(generation_procedure)+1]<-this_key
    target_pattern<-Action(this_key, target_pattern, F)
  }
  
  for (q in (-N+1):N)
  {
    for (r in (-N+1):N)
    {
      if (target_lock_tmp[q+N, r+N]==1)
      {
        target_pattern[q+N, r+N]=1
      } 
    }
  }
  cat('gen proc', generation_procedure, c(action_keycodes, primitive_keycodes))
  tmp_key_array = c()
  for (i in 1:length(generation_procedure))
  {
    tmp= which(c(action_keycodes.concat, cachable_keycodes)==generation_procedure[i])#Might need to restrict to first element if returns more than one?
    tmp_key_array[length(tmp_key_array)+1]<-action_keys[tmp]
    # console.log(tmp_key_array, tmp)
  }
  pattern = paste0(tmp_key_array)#BUT THIS IS NEVER RETURNED?
  
  target_pattern
}
      
      

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

depth<-10

state_history<-vector(mode = "list", length = depth)
action_history<-rep(NA, length = depth)

for (i in 1:depth)
{
  action<-sample(length(f), 1)
  state<-f[[action]](state)
  
  state_history[[i]]<-state
  action_history[i]<-action
}


# Append the state to the board and plot it
board$state<-factor(rep(c(state), each = 6))
board$state[board$in_bound==F]<-NA

ggplot(board, aes(x_abs, y_abs)) + 
  geom_polygon(aes(group = id, fill=state, colour = vertice)) +
  scale_fill_manual(values = c('white','black'))


if (mode=='procedural')
{
  target <- ProceduralPattern(gen_depth)
} else {
  target <- ChallengePattern(pattern)
  # console.log(pattern)
  #'djdjdwuuuedjdjdwnnedjdjdwkkk' -previously
  #fxfxfswrwrwrtfxfxfscctfxfxfseeet
  #xkxkxdrrrlxkxkxdsslxkxkxdeeel -- e.g. three circles XKXKXDRRRLXKXKXDSSLXKXKXDEEEL
  #zfzsslzfzccclzfzrrrlzfzkeelzfzkkwwwlzfzkkkkeerrl circle of bones
}

# console.log(gen_depth, 'mode', mode, ' ', pattern)
results$target <- target #Store the target pattern

if (cache)
{
  results$library_history <-library
}

#VISUALISATION

#Plot the hexagons onto it
for (y in 1:(2*N+1))
{
  for (x in seq(y%%2+1, 2*(2*N+1), by=2))
  {
    cat('xy', x, y, 'centered', prx[x], pry[y], oddr_to_cube(floor(prx[x]/2),pry[y]), '\n')
        #console.log('xy', x, y, 'centered', prx[x], pry[y], oddr_to_cube(floor(prx[x]/2),pry[y]))
        # Note the coordinates, every odd row is offset to the right doubling the number of x coordinates needed
        
    these_cube_coords = oddr_to_cube(floor((x-2*N)/2),y-N)
    
    #If we're inside the hexagon
    if(abs(these_cube_coords[1])<5 & abs(these_cube_coords[2])<5 & abs(these_cube_coords[3])<5)
    {
      #Uncomment to view coordinates
      # raw_xy_string = x.toString()+','+y.toString()
      # xy_string = (x-2*N).toString()+','+(y-N).toString()
      qrs_string = paset0(these_cube_coords[1], these_cube_coords[2], sep=',')#why do this?+','+these_cube_coords[2].toString()
      
      if (target[these_cube_coords[1]+N, these_cube_coords[2]+N]==1)
      {
        # d.draw(x, y, null, null, "#9e9")
        print('draw green')
      } else {
        # d.draw(x, y, null, null, "#fff")
        print('draw white')
      }
      
    }
        
  }
}
  
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

  
          