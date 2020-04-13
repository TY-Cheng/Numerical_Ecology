
library(plyr)
library(tidyverse)
# setwd('/Users/chengh/Desktop/Tomoso_project')
setwd('~/Documents/R/Numerical_Ecology/Hong/')
rm(list = ls())
df_Abundancy_Genus <- readr::read_csv('total.csv')

if (1) {
    # for manual clip importation
    # df_Abundancy_Genus <- clipr::read_clip_tbl() 
    # drop the 'rank' column
    df_Abundancy_Genus <- df_Abundancy_Genus %>% select(-rank)
    # list_group_colname: save column names in each group to this list
    # (?i) to ignore case: '(?i)control' for both 'CONTROL' and 'control'.
    # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
    # In this case, we have four groups: 'control', 'bacteriophage', 'uv', 'mix'
    # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
    list_group_colname <- list(
        seq_Control = str_subset(string = colnames(df_Abundancy_Genus), 
                                 pattern = '(?i)control'),
        seq_Bacteriophage = str_subset(string = colnames(df_Abundancy_Genus),
                                       pattern = '(?i)bacteriophage'),
        seq_UV = str_subset(string = colnames(df_Abundancy_Genus),
                            pattern = '(?i)UV'),
        seq_Mix = str_subset(string = colnames(df_Abundancy_Genus),
                             pattern = '(?i)Mix')
    )
    # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
    # Initialize the Abundancy ~ Group table, for later ANOVA tests
    # the groups are ranked & sorted, to keep the order with 'list_group_colname'
    # !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    # The levels set here must be in accordance with the 'list_group_colname' above
    # !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
    df_anova <- data.frame(
        group = str_extract(
            string = colnames(df_Abundancy_Genus), 
            pattern = '((?i)Bacteriophage)|((?i)Control)|((?i)Mix)|((?i)UV)'
        ) %>% 
            factor(levels = c('Control', 'Bacteriophage', 'UV', 'Mix'), ordered = T) %>% 
            sort() %>% 
            na.omit()
    )
}

if (1) {
    # 
    df_result <- alply(
        .data = df_Abundancy_Genus,
        .margins = 1, .progress = 'time',
        .fun = function(
            iter_Abundancy_Genus,
            seq_group_name = names(list_group_colname)
        ) {
            # show all possible combinations of selecting 2 groups
            # eg, select 2 from 3 groups could be: (1,2), (1,3), (2,3)
            temp_index <- combn(1:length(list_group_colname), 2)
            # Prepare the colnames for the final result table
            temp_colnames <- 
                apply(X = temp_index, MARGIN = 2,
                      FUN = function(iter_seq_index) {
                          temp <- paste0('t (', seq_group_name[iter_seq_index[1]],
                                         ' ~ ', seq_group_name[iter_seq_index[2]], ')')
                          gsub(pattern = 'seq_', replacement = '', x = temp)
                      }
                )
            temp_colnames <- c('ANOVA', temp_colnames)
            # Calculate the pvalues of t tests
            temp_pval <- 
                apply(
                    X = temp_index, MARGIN = 2, 
                    FUN = function(
                        iter_seq_index
                    ) {
                        t.test(
                            as.numeric(iter_Abundancy_Genus[
                                list_group_colname[[seq_group_name[iter_seq_index[1]]]]
                                ]),
                            as.numeric(iter_Abundancy_Genus[
                                list_group_colname[[seq_group_name[iter_seq_index[2]]]]
                                ])
                        )$p.value
                    })
            # Initialize the Abundancy ~ Group table, for ANOVA tests
            df_temp <- df_anova
            df_temp['Abundancy'] <- as.numeric(
                iter_Abundancy_Genus[unlist(list_group_colname)]
            )
            # include the p value from the ANOVA test
            temp_pval <- c(
                summary(aov(formula = Abundancy ~ group, 
                            data = df_temp))[[1]]$`Pr(>F)`[1],
                temp_pval
            )
            # rename the columns as previously prepared
            names(temp_pval) <- temp_colnames
            # 
            return(temp_pval)
        }
    )
    df_result <- do.call(rbind, df_result)
}

if (1) {
    # set nan as NA
    df_result[is.nan(df_result)] <- NA
    # rename the rows
    rownames(df_result) <- df_Abundancy_Genus$name
    # Exportation to a csv file
    write.csv(x = df_result, file = 'df_result.csv')
    # for manual clip exportation
    clipr::write_clip(df_result, na = 'NA')
}
