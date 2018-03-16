
library(shiny)
library(highcharter)
library(shinythemes)

randomWalk = function(n, u) {
  s = rep(1, n + 1)
  
  for (i in 1:n) {
    s[i+1] = s[i] * sample(c(u, 1/u), 1)
  }
  
  s
}