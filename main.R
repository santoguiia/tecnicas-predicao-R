# Amostra 
base <- read.csv2("apartamentos_floripa.csv") 
set.seed(22121998) # COLOQUE O SEU DIA MÊS E ANO DE NASCIMENTO NA SET.SEED 
base1 = base[sample(nrow(base), 500),] 
