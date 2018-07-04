
library(tidyverse)
library(Rsolnp)
library(quantmod)

printf = function(...) cat(sprintf(...))

# número de ativos (manter <= 5)
n_assets = 5

# número de carteiras
n_portfolios = 10000

# taxa de retorno livre de risco
rf = 0.065 # selic anual

mercado = TRUE

if (mercado) {
  stocks = c("PETR4.SA", "ITSA4.SA", "VALE3.SA", "BBDC4.SA", "ABEV3.SA")[1:n_assets]
  
  tb = map_dfc(stocks, function(stock) {
    returns = getSymbols(stock, from = "2016-07-01", to = "2018-06-30", auto.assign = FALSE)[, 4]
    data.frame((returns/lag(returns) - 1)[2:length(returns)])
  })
  
  r = colMeans(tb) * 252
  Sigma = matrix(rep(0, n_assets * n_assets), nrow = n_assets)
  
  for (i in 1:n_assets) {
    for (j in 1:n_assets) {
      Sigma[i, j] = cov(tb[, i], tb[, j]) * 252^2
    }
  }
  
} else {
  stocks = c("AAA", "BBB", "CCC", "DDD", "EEE")[1:n_assets]
  randomCovMatrix = function(n) {
    p = qr.Q(qr(matrix(rnorm(n^2), n)))
    crossprod(p, p * runif(n))
  }
  
  # retornos aleatórios com distribuição normal
  r = rf + 0.05*rnorm(n_assets)
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
xlim = c(0, max(tb_portfolios$risk + .001))
ylim = c(rf - .005, max(tb_portfolios$return + .005))

x0 = rep(1/n_assets, n_assets)
minRiskForReturn = function(return) {
  # minimiza w^t Sigma w (variância da carteira) sujeito a soma dos pesos = 1,
  # retorno = um dado retorno
  solnp(x0, function(w) t(w) %*% Sigma %*% w, 
    eqfun = function(w) c(sum(w), r %*% w),
    eqB = c(1, return),
    ineqfun = function(w) w,
    ineqLB = rep(0, n_assets),
    ineqUB = rep(1.01, n_assets)
  )
}

hyperbola = data.frame(return = seq(ylim[1], ylim[2], length.out = 100)) %>%
  mutate(
    risk = sqrt(map_dbl(return, ~ tail(minRiskForReturn(.)$values, 1))),
    sharpe = (return - rf)/risk
  )

# gradiente da reta de alocação de capital = maior índice de sharpe
max_sharpe = max(hyperbola$sharpe)
imax = which.max(hyperbola$sharpe)
optimal = minRiskForReturn(hyperbola$return[imax])

# Escreve as informações do problema e da careita tangente:
rownames(Sigma) <- colnames(Sigma) <- names(r) <- names(optimal$par) <- stocks
printf("Vetor de retornos: \n")
print(r)
printf("Matriz de covariância:\n")
print(Sigma)

printf("Carteira tangente:\nPesos:\n")
print(optimal$par)

printf("Risco: %.4f\tRetorno: %.4f\tÍndice de Sharpe: %.4f\n", 
       hyperbola$risk[imax], hyperbola$return[imax], hyperbola$sharpe[imax])

# plot do gráfico
ggplot(tb_portfolios, aes(x = risk, y = return, color = sharpe)) + 
  geom_point(size = 1) +
  scale_color_gradient(low = "red", high = "green", guide = guide_legend(title = "Índice de Sharpe")) + 
  xlab("Risco") + ylab("Retorno esperado") + theme_minimal() +
  geom_abline(slope = max_sharpe, intercept = rf, size = 1) +
  coord_cartesian(xlim = xlim, ylim = ylim) +
  geom_path(data = hyperbola, aes(x = risk, y = return), size = 1) +
  geom_hline(yintercept = 0) + geom_vline(xintercept = 0)
