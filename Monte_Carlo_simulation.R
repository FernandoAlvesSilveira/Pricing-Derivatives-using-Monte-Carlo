#Pricing derivatives (European option) using Monte Carlo Simulation
#Precificando derivativos (Op��es europ�ias) usando simula��es de Monte Carlo

"""Options, Futures, and Other Derivatives, 9th Edition,
ISBN 978-0-133-45631-8, by John C. Hull, published by Pearson Education, 2015. """

# Price / pre�o do ativo
S <- 50

#Strike / limite
K <- 50

#Volatility / Volatilidade 
sig <- 0.3

#Maturity / Vencimento ou Tempo de maturidade
T <- 0.5
  
#Risk free rate / taxa livre de risco
R <- 0.05
  

#Simulations / simula��es
n_simulation <- 1000

#Matrix of prices / matriz de pre�os
prices_simulated <- matrix(0, nrow = 1, ncol = n_simulation)


for (simulation in 1:n_simulation) {
  #Chapter 21 equation 17 / Equa��o 17 do cap�tulo 21
  value_adjusted <- S*exp((R - sig*sig/2)*T + sig * rnorm(365)*sqrt(T))
  derivative_prices <- exp(-R*T)*max(value_adjusted - K)
  prices_simulated[,simulation] <- derivative_prices
}


plot(prices_simulated[1:n_simulation])
lines(lowess(prices_simulated[1:n_simulation]))

#Mean of simulated prices / Retorno dos pre�os m�dios estimados
print(mean(prices_simulated))




