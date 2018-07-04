
library(tidyverse)
library(Rsolnp)

# número de ativos (manter <= 5)
n_assets = 5

# número de carteiras
n_portfolios = 10000

# taxa de retorno livre de risco
rf = 6.5 # selic anual

mercado = TRUE

if (mercado) {
  stocks = c("PETR4.SA", "ITSA4.SA", "VALE3.SA", "BBDC4.SA", "ABEV3.SA")[1:n_assets]
  
  tb = map_dfc(stocks, function(stock) {
    returns = getSymbols(stock, from = "2016-07-01", to = "2018-06-30", auto.assign = FALSE)[, 4]
    data.frame((returns/lag(returns) - 1)[2:length(returns)])
  })
  
  r = rowMeans(tb)
  
} else {
  randomCovMatrix = function(n) {
    p = qr.Q(qr(matrix(rnorm(n^2), n)))
    crossprod(p, p* 3 * runif(n))
  }
  
  # retornos aleatórios com distribuição normal
  r = rnorm(n_assets, 10, 3)
  Sigma = randomCovMatrix(n_assets)
}

# gera matriz aleatória onde cada linha indica uma carteira e cada coluna 
# indice o peso de um ativo
mat_portfolios = matrix(runif(n_assets * n_portfolios), nrow = n_portfolios)
mat_portfolios = mat_portfolios / rowSums(mat_portfolios)

tb_portfolios = data.frame(
  return = mat_portfolios %*% r,
  risk = sqrt(diag(mat_portfolios %*% Sigma %*% t(mat_portfolios)))
) %>%
  mutate(sharpe = (return - rf)/risk)

# limites para o gráfico, só para assegurar que as partes interessantes 
# serão plotadas
xlim = c(0, max(tb_portfolios$risk + .1))
ylim = c(rf - .5, max(tb_portfolios$return + .5))


x0 = rep(1/n_assets, n_assets)
minRiskForReturn = function(return) {
  # minimiza w^t Sigma w (variância da carteira) sujeito a soma dos pesos = 1,
  # retorno = um dado retorno
  solnp(x0, function(w) t(w) %*% Sigma %*% w, 
    eqfun = function(w) c(sum(w), r %*% w),
    eqB = c(1, return))
}

hyperbola = data.frame(return = seq(ylim[1], ylim[2], length.out = 100)) %>%
  mutate(
    risk = sqrt(map_dbl(return, ~ tail(minRiskForReturn(.)$values, 1))),
    sharpe = (return - rf)/risk
  )

# gradiente da reta de alocação de capital = maior índice de sharpe
max_sharpe = max(hyperbola$sharpe)

# plot do gráfico
ggplot(tb_portfolios, aes(x = risk, y = return, color = sharpe)) + 
  geom_point(size = 1) +
  scale_color_gradient(low = "red", high = "green") + 
  xlab("Risco") + ylab("Retorno esperado") + theme_minimal() +
  geom_abline(slope = max_sharpe, intercept = rf, size = 1) +
  coord_cartesian(xlim = xlim, ylim = ylim) +
  geom_path(data = hyperbola, aes(x = risk, y = return), size = 1) +
  geom_hline(yintercept = 0) + geom_vline(xintercept = 0) +
  annotate("text", x = 0, y = 0)
