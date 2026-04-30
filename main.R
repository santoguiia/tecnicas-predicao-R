# ============================================================
# EXERCÍCIO 2 — Regressão Linear Múltipla
# Variável dependente: VALOR (em R$ mil)
# Arquivo de dados: base1_exportada.csv
# ============================================================

# ---- 1. IMPORTAÇÃO E PREPARO DOS DADOS --------====----------
base1 <- read.csv2("base1_exportada.csv")

# Converter Valor de R$ para R$ mil (variável dependente em milhares)
base1$Valor <- base1$Valor / 1000

# Definir fatores com categorias de referência explícitas
# Referência Acabamento: "Ruim" | Referência Local: "Centro"
base1$Acabamento <- factor(base1$Acabamento,
                           levels = c("Ruim", "Bom", "Excelente"))
base1$Local      <- factor(base1$Local,
                           levels = c("Centro", "Continente",
                                      "Leste", "Norte", "Sul"))

# Verificação da estrutura
str(base1)
summary(base1)


# ---- 2. MODELO COMPLETO (todas as variáveis) ----------------
# Inclui: Area, Idade, Energia, Acabamento, Local
modelo1 <- lm(Valor ~ Area + Idade + Energia + Acabamento + Local,
              data = base1)
summary(modelo1)
# Resultado: Energia não é significativa (p = 0.569 > 0.05)


# ---- 3. TESTE ESTATÍSTICO — significância geral (F) ---------
# F-statistic e p-value já presentes no summary()
# Também via ANOVA:
anova(modelo1)


# ---- 4. MODELO FINAL — sem Energia (variável não significativa) ----
modelo2 <- lm(Valor ~ Area + Idade + Acabamento + Local, data = base1)
summary(modelo2)
# Todas as variáveis restantes são significativas (p < 0.001)


# ---- 5. COEFICIENTE DE DETERMINAÇÃO AJUSTADO (R² ajustado) --
cat("R² Ajustado:", round(summary(modelo2)$adj.r.squared, 4), "\n")
# Interpreta-se: ~83,1% da variação de Valor é explicada pelo modelo


# ---- 6. IMPORTÂNCIA RELATIVA (%SS) ---------------------------
# Utiliza Soma de Quadrados sequencial (Tipo I)
an <- anova(modelo2)
SS_total <- sum(an$`Sum Sq`)
imp_pct <- round(an$`Sum Sq` / SS_total * 100, 2)
names(imp_pct) <- c("Area", "Idade", "Acabamento", "Local", "Residuos")
print(imp_pct)

# Gráfico de barras — Importância Relativa
# Objetivo: mostrar a contribuição percentual de cada variável no modelo
 do relatório LaTeX
png("importancia_relativa.png", width = 900, height = 600, res = 120)
cores <- c("#2E86AB", "#A23B72", "#F18F01", "#C73E1D")
imp_vars <- imp_pct[1:4]  # apenas variáveis (sem resíduos)
bp <- barplot(imp_vars,
              col    = cores,
              ylim   = c(0, 30),
              main   = "Importância Relativa das Variáveis (%SS)",
              ylab   = "Contribuição (%)",
              xlab   = "Variável",
              border = "white",
              las    = 1)
text(bp, imp_vars + 0.8, paste0(imp_vars, "%"), cex = 0.95, font = 2)
dev.off()


# ---- 7. ANÁLISE DE RESÍDUOS — 4 gráficos diagnósticos -------
# Objetivo: verificar pressupostos do modelo de regressão
 do relatório LaTeX
png("residuos_4graficos.png", width = 1400, height = 1100, res = 120)
par(mfrow = c(2, 2))

# Gráfico 1 — Resíduos vs Valores Ajustados
# Verifica homocedasticidade e linearidade
plot(modelo2, which = 1, main = "Resíduos vs Valores Ajustados")

# Gráfico 2 — Q-Q Normal dos Resíduos
# Verifica normalidade dos resíduos
plot(modelo2, which = 2, main = "Q-Q Normal dos Resíduos")

# Gráfico 3 — Scale-Location (Spread-Location)
# Verifica homocedasticidade (raiz dos resíduos padronizados)
plot(modelo2, which = 3, main = "Scale-Location")

# Gráfico 4 — Resíduos vs Alavancagem (Cook's Distance)
# Identifica observações influentes
plot(modelo2, which = 5, main = "Resíduos vs Alavancagem")

dev.off()


# ---- GRÁFICOS EXPLORATÓRIOS ADICIONAIS ----------------------

# Scatter: Área vs Valor
# Objetivo: ilustrar a relação linear positiva entre área e valor

png("scatter_area_valor.png", width = 900, height = 600, res = 120)
plot(base1$Area, base1$Valor,
     main = "Área Privativa vs Valor do Imóvel",
     xlab = "Área (m²)",
     ylab = "Valor (R$ mil)",
     pch  = 19,
     col  = rgb(0.2, 0.5, 0.8, 0.5),
     cex  = 0.7)
abline(lm(Valor ~ Area, data = base1), col = "red", lwd = 2)
dev.off()

# Boxplot: Acabamento vs Valor
# Objetivo: comparar distribuição do valor por nível de acabamento

png("boxplot_acabamento.png", width = 900, height = 600, res = 120)
boxplot(Valor ~ Acabamento, data = base1,
        col    = c("#F4A261", "#2A9D8F", "#E76F51"),
        main   = "Valor por Tipo de Acabamento",
        xlab   = "Acabamento",
        ylab   = "Valor (R$ mil)",
        border = "gray30")
dev.off()

# Boxplot: Local vs Valor
# Objetivo: comparar distribuição do valor por localização

png("boxplot_local.png", width = 900, height = 600, res = 120)
boxplot(Valor ~ Local, data = base1,
        col    = c("#264653", "#2A9D8F", "#E9C46A", "#F4A261", "#E76F51"),
        main   = "Valor por Localização do Imóvel",
        xlab   = "Localização",
        ylab   = "Valor (R$ mil)",
        border = "gray30",
        las    = 1)
dev.off()

cat("\n=== Análise concluída. Gráficos salvos. ===\n")