
if (1) {
    library(plyr)
    library(dplyr)
    df_Abundancy_Genus <- clipr::read_clip_tbl() 
    # 
    df_Abundancy_Genus <- 
        df_Abundancy_Genus[, colnames(df_Abundancy_Genus) != 'rank']
    seq_DI <- grep(pattern = 'DI.', colnames(df_Abundancy_Genus), fixed = T)
    seq_Mock <- grep(pattern = 'Mock.', colnames(df_Abundancy_Genus), fixed = T)
    seq_AeMBR <- grep(pattern = 'AeMBR.', colnames(df_Abundancy_Genus), fixed = T)
    seq_AnMBR <- grep(pattern = 'AnMBR.', colnames(df_Abundancy_Genus), fixed = T)
}

# 
df_result <- alply(
    .data = df_Abundancy_Genus, .margins = 1, .progress = 'time',
    .fun = function(
        iter_Abundancy_Genus,
        seq_group_name = c('seq_DI', 'seq_Mock', 'seq_AeMBR', 'seq_AnMBR')
    ) {
        temp_index <- combn(1:4, 2)
        # 
        temp_colnames <- 
            apply(X = temp_index, MARGIN = 2,
                  FUN = function(iter_seq_index) {
                      temp <- paste0('t (', seq_group_name[iter_seq_index[1]],
                                     ' ~ ', seq_group_name[iter_seq_index[2]], ')')
                      gsub(pattern = 'seq_', replacement = '', x = temp)
                  }
            )
        temp_colnames <- c('ANOVA', temp_colnames)
        # 
        temp_pval <- 
            apply(X = temp_index, MARGIN = 2, 
                  FUN = function(
                      iter_seq_index
                  ) {
                      t.test(
                          iter_Abundancy_Genus[
                              eval(as.symbol(seq_group_name[iter_seq_index[1]]))
                              ],
                          iter_Abundancy_Genus[
                              eval(as.symbol(seq_group_name[iter_seq_index[2]]))
                              ]
                      )$p.value
                  })
        # 
        df_temp <- data.frame(
            group = factor(c(sub('\\d+', 'DI', seq_DI), 
                             sub('\\d+', 'Mock', seq_Mock),
                             sub('\\d+', 'AeMBR', seq_AeMBR), 
                             sub('\\d+', 'AnMBR', seq_AnMBR))),
            Abundancy = as.numeric(iter_Abundancy_Genus[c(seq_DI, seq_Mock, seq_AeMBR, seq_AnMBR)])
        )
        temp_pval <- c(
            summary(aov(formula = Abundancy ~ group, data = df_temp))[[1]]$`Pr(>F)`[1],
            temp_pval
        )
        # 
        names(temp_pval) <- temp_colnames
        # 
        return(temp_pval)
    }
)

# 
df_result <- do.call(rbind, df_result)
df_result[is.nan(df_result)] <- NA
rownames(df_result) <- df_Abundancy_Genus$name
clipr::write_clip(df_result, na = 'NA')
