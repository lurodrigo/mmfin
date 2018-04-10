
library(shiny)
library(highcharter)
library(shinythemes)
library(tidyverse)
library(glue)
library(DiagrammeR)

computeVs = function(N, u, r, strike) {
  d = 1/u
  p = (1+r - d)/(u-d)
  q = (u - (1+r))/(u-d)
  v = matrix(rep(0, (N + 1)*(N + 1)), nrow = N + 1)
  
  for (k in 0:N) {
    v[N + 1, k + 1] = max(choose(N, k) * u^k * d^(N-k) - strike, 0)
  }
  
  for (n in (N-1):0) {
    for (k in 0:n) {
      v[n + 1, k + 1] = (p*v[n+2, k+2] + q*v[n+2, k+1])/(1 + r)
    }
  }
  
  v
}

graphSpec = function(N, u, r, strike) {
  v = computeVs(N, u, r, strike)
  
  nodes = expand.grid(k = 0:N, n = 0:N) %>%
    filter(k <= n) %>%
    select(n, k)
  
  edges = nodes %>%
    filter(n < N) %>%
    mutate(n2 = n + 1, k2 = k)
    
  edges = bind_rows(edges, edges %>% mutate(k2 = k2 + 1)) %>%
    arrange(n, k, desc(k2))
  
  glue("
    digraph gr {{
      graph[rankdir = LR]
      node[shape = circle]
       {node_labels}
       {nodes};
       {edges};
      graph [nodesep = 0.1]
    }}
  ", 
    nodes = nodes %>% glue_data("{n}.{k}") %>% paste0(collapse = ";"),
    edges = edges %>% glue_data("{n}.{k}->{n2}.{k2}") %>% paste0(collapse = ";"),
    node_labels = nodes %>% glue_data("{n}.{k}[label='{v[n+1][k+1]}']") %>% paste0(collapse = "\n")
  )
}