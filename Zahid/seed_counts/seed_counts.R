library(plyr)
library(dplyr)
library(multcomp)
rm(list = ls())
setwd('~/Documents/R/Numerical_Ecology/Zahid/seed_counts/')
load('seed_counts.RData')

# Tukey HSD ---------------------------------------------------------------
iter_formula <- as.formula(
    paste0(paste0(colnames(df)[-1], collapse = ' + '), ' ~ Samples')
)
result_aov <- aov(formula = iter_formula, data = df)
result_tukeyhsd <- TukeyHSD(result_aov, conf.level = .95)
plot(result_tukeyhsd)
print(result_tukeyhsd)

# Test per Variable -------------------------------------------------------
for (iter_name in colnames(df)[-1]) {
    iter_formula <- as.formula(paste0(iter_name, '~Samples'))
    iter_aov <- aov(formula = iter_formula, data = df)
    # 
    cat('\n\n', iter_name)
    cat('\n################################################################\n')
    TukeyHSD(x = aov(formula = iter_formula, data = df)) %>% print()
    oneway.test(formula = iter_formula, data = df) %>% print()
    t.test(x = df[[iter_name]], g = df$Samples, 
           p.adjust.method = "BH", pool.sd = FALSE) %>% print()
    wilcox.test(df[[iter_name]], g = df$Samples) %>% print()
    cat('\n################################################################\n')
}
