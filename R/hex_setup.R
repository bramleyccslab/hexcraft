#########
# SETUP
#########
library(tidyverse)

N <- 4 #Hexagon radius (excluding centre, other stuff should scale with this)

#Cube-coordinate directional vectors
direction_vectors <- list(NE=c(1, -1, 0),
                          E =c(1, 0, -1),
                          SE=c(0, 1, -1),
                          SW=c(-1, 1, 0),
                          W =c(-1, 0, +1),
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
  
  loc<-loc %>% arrange(desc(y), x) %>%   #this was changed to desc because it was flipped upside-down
    filter(abs(q)<5 & abs(r)<5 & abs(s)<5) %>% mutate(id = 1:n()) %>%
  select(id, x, y, q,r,s, x_pos, y_pos)

  return(loc)
}

# Extend state description to include all polygonal vertices for plotting
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
empty_state<-make_hex_lattice(N)
#  Create the full set of polygonal coordinates for drawing the board
board_polygons<-make_hex_coords(empty_state)

# "Amortise" (i.e. precalculate) the basic transformations
# so you don't need to loop through calcualting them at runtime
# ################################################

amort<-list(vectors = list(), rotations = list(), flips = list())
for (dir in 1:6)
{
  amort$vectors[[dir]]<-data.frame(from=1:nrow(empty_state), to = rep(NA, nrow(empty_state)))
  for (i in 1:nrow(empty_state))
  {
    tmp<-which(empty_state$q==empty_state$q[i]+direction_vectors[[dir]][1] &
         empty_state$r==empty_state$r[i]+direction_vectors[[dir]][2] &
         empty_state$s==empty_state$s[i]+direction_vectors[[dir]][3])
    if (any(tmp))
    {
      amort$vectors[[dir]]$to[i]<-tmp
    }

  }
  amort$vectors[[dir]]<-amort$vectors[[dir]][!is.na(amort$vectors[[dir]]$to),] #Remove the indices that go off the board
}

amort$rotations[[1]]<-data.frame(from=1:nrow(empty_state), to = rep(NA, nrow(empty_state)))
for (i in 1:nrow(empty_state))
{
  tmp<-which(empty_state$q==-empty_state$r[i] &
               empty_state$r==-empty_state$s[i] &
               empty_state$s==-empty_state$q[i])
  if (any(tmp))
  {
    amort$rotations[[1]]$to[i]<-tmp
  }
}

amort$flips<-list()
for (axis in 1:3)
{
  amort$flips[[axis]]<-data.frame(from=1:nrow(empty_state), to = rep(NA, nrow(empty_state)))
  
  for (i in 1:nrow(empty_state))
  {
    if (axis==1)
    {
    tmp<-which(empty_state$q==empty_state$q[i] &
                 empty_state$r==empty_state$s[i] &
                 empty_state$s==empty_state$r[i])
    }
    
    if (axis==2)
    {
      tmp<-which(empty_state$q==empty_state$s[i] &
                   empty_state$r==empty_state$r[i] &
                   empty_state$s==empty_state$q[i])
    }
    
    if (axis==3)
    {
      tmp<-which(empty_state$q==empty_state$r[i] &
                   empty_state$r==empty_state$q[i] &
                   empty_state$s==empty_state$s[i])
    }
    
    if (any(tmp))
    {
      amort$flips[[axis]]$to[i]<-tmp
    }
  }
}

###################
# Create action set
###################

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
  #Remove unit in center
  # ts$active[ts$x==0, ts$y==0] <- 0
  ts$active[31]<-0
  ts
}

f$AddBar<-function(ts)
{
  #Add horizontal bar
  ts$active[c(39, 31, 23)] <- 1
  #ts$x==-1 & ts$y==0
  #ts$x==0 & ts$y==0
  ts
}

f$AddCorner<-function(ts)
{
  #Add horizontal bar
  ts$active[c(22,31,32)] <- 1#ts$x==1 & ts$y==-1
  ts
}

f$RotateClockwise<-function(ts)
{
  os<-ts
  ts$active<-0
  ts$active[amort$rotations[[1]]$to]<-os$active[amort$rotations[[1]]$from]
  ts
}

f$Flip<-function(ts, axis=1)
{
  os<-ts
  ts$active<-0
  ts$active[amort$flips[[axis]]$to]<-os$active[amort$flips[[axis]]$from]
  ts
}

f$Reflect<-function(ts, axis=1)
{
  os<-ts
  ts$active<-0  
  ts$active[amort$flips[[axis]]$to]<-os$active[amort$flips[[axis]]$from]
  ts$active<-as.numeric(ts$active | os$active)
  ts
}

# f$Invert<-function(ts, radius = 2)
# {
#   ts$active[abs(ts$q)<radius & abs(ts$r)<radius & abs(ts$s)<radius]<-1-ts$active[abs(ts$q)<radius & abs(ts$r)<radius & abs(ts$s)<radius]
#   ts
# }

f$ShiftNE<-function(ts, dir = 1)
{
  os<-ts
  ts$active<-0
  ts$active[amort$vectors[[dir]]$to]<-os$active[amort$vectors[[dir]]$from]
  ts
}


f$ShiftSE<-function(ts, dir = 3) #changed this to SE
{
  os<-ts 
  ts$active<-0
  ts$active[amort$vectors[[dir]]$to]<-os$active[amort$vectors[[dir]]$from] 
  ts
}


f$ShiftW<-function(ts, dir = 5)
{
  os<-ts
  ts$active<-0
  ts$active[amort$vectors[[dir]]$to]<-os$active[amort$vectors[[dir]]$from]
  ts
}

apply_cache<-function(ts, cache){
  for (c in 1: length(cache))
  {
    ts<-f[[cache[c]]](ts)
  }
  ts
}

save(file='./dat/hexsetup.rdata', f, empty_state, board_polygons, amort,
     cube_to_oddr, oddr_to_cube, make_hex_coords, make_hex_lattice, apply_cache)

direction_vectors

# f$ShiftE<-function(ts, dir = 2)
# {
#   os<-ts
#   ts$active<-0
#   ts$active[amort$vectors[[dir]]$to]<-os$active[amort$vectors[[dir]]$from]
#   ts
# }
# f$ShiftSE<-function(ts, dir = 4)
# {
#   os<-ts
#   ts$active<-0
#   ts$active[amort$vectors[[dir]]$to]<-os$active[amort$vectors[[dir]]$from]
#   ts
# }
# f$ShiftNW<-function(ts, dir = 6)
# {
#   os<-ts
#   ts$active<-0
#   ts$active[amort$vectors[[dir]]$to]<-os$active[amort$vectors[[dir]]$from]
#   ts
# }
# f$Lock<-function(ts)
# {
#   # Special function because it shifts the active state to the locked state for which we get the scored loss
#   # Should I change this since we want every trial to come with a loss even if not yet locked?
#   # I guess we could pull the scoring out and make the loss not dependent on things being locked
#   # or make it that locking gives points and clears the hexes, such that the loss of the active state is just about the un-solved hexes
#   ts$locked<-pmax(ts$locked, ts$active)
#   ts$active<-0
#   ts
# }



# UseCache<-function(n, this_state, this_real)
# {    
#   #Read the cached pattern TODO KEEP THIS NOT IN THE STRING
#   cache_pattern <- library[n]#Array.from($('#cache' + n).text())
#   cat('before expanding:', cache_pattern)
#   
#   #Get rid of any nesting before the display process begins
#   
#   while (cachable_keycodes[1] %in% cache_pattern |
#          cachable_keycodes[2] %in% cache_pattern | 
#          cachable_keycodes[3] %in% cache_pattern | 
#          cachable_keycodes[4] %in% cache_pattern | 
#          cachable_keycodes[5] %in% cache_pattern | 
#          cachable_keycodes[6] %in% cache_pattern ) {
#     
#     for (i in 1:length(cache_pattern))
#     {
#       for (j in 1:length(cachable_keycodes))
#       {
#         if (cache_pattern[i]==cachable_keycodes[j])
#         {
#           if (i==1)
#           {
#             cache_pattern<-c(library[[j]], cache_pattern[2:length(cache_pattern)])
#           } else if (i==length(cache_pattern))
#           {
#             cache_pattern<-c(cache_pattern[1:(i-1)], library[[j]])
#           } else {
#             cache_pattern<-c(cache_pattern[1:(i-1)], library[[j]], cache_pattern[(i+1):length(cache_pattern)])
#           }
# 
#           #MAY NEED TO CHECK IF ITS FIRST OR LAST
#           # cache_pattern.splice(i, 1)
#           # insertArrayAt(cache_pattern, i, library[j])
#           break
#         }
#       }
#     }
#   }
#   #How long is the fully expanded string?
#     cpl = length(cache_pattern)
#   
#   #Feed the string to the Action function with short timeouts so you can see it play out
#   cat('after expanding:', cache_pattern)
#   
#   
#   for (i in 1:length(cache_pattern))
#   {
#     Action(cache_pattern[i], this_state=this_state, real=this_real, midcache = T)
#   }
#   if (this_real)
#   {
#     Update()
#     actions[length(actions)+1]<-cachable_keycodes[n]
#     # UpdateStringVis()
#   }
# }  
  
