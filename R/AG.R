# generate_test_sets
rm(list=ls())

library(tidyverse)
library(ggplot2)
library(gridExtra) #For plotting lots of things at once

# source('hex_setup.R')
load('./dat/hexsetup.rdata', verbose = T)
load('./dat/training_case.rdata', verbose = T)

#FROM JOHNSON GRIFFITHS & GOLDWATER

W<-c(1:11) #Terminals
N<-c(S) #Nonterminals
R<-c('S->(A,S)', 'S->A')#Productions??
theta<-c(.5,.5)#Parameters

C<- c() #Adaptor vector


m<-NA #Number of clusters in sequence
n_k<-c()#number of times cluster l appears in z
z<-c()#clustered sequence
a<- 0.5#First parameter of process (in [0,1])
b<- 0.5#Second parameter of proces (in [0, inf])
  
# e.g. 
z<-c(1,1,1,1,2,2,2,3,3,1)
# z<-c(3,3,3,3,2,2,2,1,1,3)
m<-length(unique(z))
n<-summary(factor(z))

j_a<-c()
for (i in 1:length(n))
{
  tmp<-c()
  for (j in 1:(n[i]-1))
  {
    tmp[j]<-j-a
  }
  j_a[i]<-prod(tmp)
}
p_z<-(prod( (a * (n-1) + b) * j_a ) ) / prod(0:(length(z)-1)+b)
p_z


# Questions for Chris:

# algo for calculating likelihood of string given a library?




install.packages("stringr")
library(stringr)

# Define the grammar rules
grammar_rules <- list(
  "S" = c("NP VP"),
  "VP" = c("V NP", "eats"),
  "NP" = c("Det N", "John", "apple"),
  "Det" = c("the", "a"),
  "N" = c("man", "apple"),
  "V" = c("eats")
)

# Function to recursively apply grammar rules and find parses
find_parses <- function(input, start_symbol, grammar_rules, depth = 0) {
  if (depth > 10) return(list(input))  # Limit the recursion depth
  
  if (!(start_symbol %in% names(grammar_rules))) {
    return(ifelse(start_symbol == input, list(input), list()))
  }
  
  options <- grammar_rules[[start_symbol]]
  all_combinations <- list()
  for (option in options) {
    elements <- str_split(option, " ")[[1]]
    if (length(elements) == 1) {
      matches <- find_parses(input, elements[1], grammar_rules, depth + 1)
      if (length(matches[[1]]) > 0) {
        all_combinations <- c(all_combinations, list(paste(start_symbol, "->", option)))
      }
    } else {
      first_part <- find_parses(str_sub(input, 1, nchar(input)/2), elements[1], grammar_rules, depth + 1)
      second_part <- find_parses(str_sub(input, nchar(input)/2 + 1, nchar(input)), elements[2], grammar_rules, depth + 1)
      if (length(first_part[[1]]) > 0 && length(second_part[[1]]) > 0) {
        combined <- paste(paste(first_part, collapse=" "), paste(second_part, collapse=" "))
        all_combinations <- c(all_combinations, list(paste(start_symbol, "->", combined)))
      }
    }
  }
  return(all_combinations)
}

# Define the input
input_string <- "John eats apple"

# Find possible parses
possible_parses <- find_parses(input_string, "S", grammar_rules)

# Print the parses
print(possible_parses)
