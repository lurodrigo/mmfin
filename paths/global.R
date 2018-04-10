
library(shiny)
library(highcharter)
library(shinythemes)
library(tidyverse)
library(magrittr)
library(glue)
library(DiagrammeR)

computeVs = function(N, u, r, strike) {
  d = 1/u
  p = (1+r - d)/(u-d)
  q = (u - (1+r))/(u-d)
  v = matrix(rep(0, (N + 1)*(N + 1)), nrow = N + 1)
  
  for (k in 0:N) {
    v[N + 1, k + 1] = max(u^(N-k) * d^k - strike, 0)
  }
  
  for (n in (N-1):0) {
    for (k in 0:n) {
      v[n + 1, k + 1] = (p*v[n+2, k+1] + q*v[n+2, k+2])/(1 + r)
    }
  }
  
  v
}

graphSpec = function(N, u, r, strike, digitos) {
  v = computeVs(N, u, r, strike)
  
  nodes = expand.grid(k = 0:N, n = 0:N) %>%
    filter(k <= n) %>%
    select(n, k)
  
  edges = nodes %>%
    filter(n < N) %>%
    mutate(n2 = n + 1, k2 = k, label = 'H')
    
  edges = bind_rows(edges, edges %>% mutate(k2 = k2 + 1, label = 'T')) %>%
    arrange(n, k, desc(k2))
  
  for (i in 1:nrow(nodes)) {
    nodes$label[i] = sprintf(glue('%.{digitos}f'), v[nodes$n[i]+1, nodes$k[i]+1])
  }
  
  glue("
    digraph gr {{
      graph[rankdir = LR]
      node[fontname = Times, fixedsize = true, shape = rectangle]
      edge[fontname = Times]
       {node_labels}
       {edge_labels}
      graph [nodesep = 0.1]
    }}
  ", 
    node_labels = nodes %>% glue_data("{n}.{k}[label='{label}']") %>% paste0(collapse = "\n"),
    edge_labels = edges %>% glue_data("{n}.{k}->{n2}.{k2}[label='{label}']") %>% paste0(collapse = "\n")
  )
}

getPath = function(v, tosses) {
  x = integer(nrow(v))
  x[1] = v[1, 1]
  k = 1
  for (i in 2:length(x)) {
    if (toupper(str_sub(tosses, i, i)) == 'T') {
      k = k + 1
    } 
    x[i] = v[i, k]
  }
  x
}