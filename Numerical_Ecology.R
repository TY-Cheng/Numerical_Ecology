# Initialization ----------------------------------------------------------
library(dplyr)
library(ggplot2)
library(runjags)
setwd("/Users/chengt/OneDrive/MBR_Yasmeen")
load('MBR_Yasmeen.RData')
# 
# To avoid warning 
# In runjags.summaries(fullmcmclist = mcmc, thinnedmcmclist = thinnedmcmc,  :
# An unexpected error occured while calculating the mode
runjags.options(silent.jags = F, silent.runjags = F,
                mode.continuous = T,
                inits.warning = T, rng.warning = T, summary.warning = T,
                blockcombine.warning = T, blockignore.warning = T,
                tempdir.warning = T, nodata.warning = T)

# Exploration -------------------------------------------------------------
colnames(Species_Yasmeen)
# category:     1:7     ;
# sample:       8:25    ;
# Species_Yasmeen <- arrange(
#     Species_Yasmeen, Phylum, Class, Order, Family, Genus, Species)
# Species_Yasmeen <- filter(Species_Yasmeen, !is.na(.data$Species))
# Species_Yasmeen <- Species_Yasmeen[Species_Yasmeen$Kingdom!='Unassigned', ]
# Species_Yasmeen <- Species_Yasmeen[Species_Yasmeen$Phylum!='p__', ]
# Species_Yasmeen <- Species_Yasmeen[Species_Yasmeen$Class!='c__', ]
# 
if(0){
    # now, no NA among KPCOFGS
    # no p__ in Phylum
    # no c__ in Class
    sort(unique(Species_Yasmeen$Kingdom))
    sort(unique(Species_Yasmeen$Phylum))
    100 * colSums(filter(Species_Yasmeen, .data$Phylum == 'p__')[8:25])/
        colSums(Species_Yasmeen[8:25])
    sort(unique(Species_Yasmeen$Class))
    100 * colSums(filter(Species_Yasmeen, .data$Class == 'c__')[8:25])/
        colSums(Species_Yasmeen[8:25])
    # 
    sort(unique(Species_Yasmeen$Order))
    100 * colSums(filter(Species_Yasmeen, .data$Order == 'o__')[8:25])/
        colSums(Species_Yasmeen[8:25])
    sort(unique(Species_Yasmeen$Family))
    100 * colSums(filter(Species_Yasmeen, .data$Family == 'f__')[8:25])/
        colSums(Species_Yasmeen[8:25])
    sort(unique(Species_Yasmeen$Genus))
    100 * colSums(filter(Species_Yasmeen, .data$Genus == 'g__')[8:25])/
        colSums(Species_Yasmeen[8:25])
    sort(unique(Species_Yasmeen$Species))
    100 * colSums(filter(Species_Yasmeen, .data$Species == 's__')[8:25])/
        colSums(Species_Yasmeen[8:25])
}
# 

# Thresholding ------------------------------------------------------------
Sample_Sum_PCOFGS <- function(
    df = Species_Yasmeen, by = 'Phylum', threshold = .01) {
    df_colsum <- data.frame()
    iter_seq <- unique(df[,by])
    for (iter_by in iter_seq) {
        subdf <- df[df[by]==iter_by, ]
        # print(iter_by)
        # print(colSums(subdf[8:25]))
        row_new <- colSums(subdf[8:25])
        df_colsum <- rbind(df_colsum, 
                           row_new)
    }
    df_colsum <- cbind(iter_seq, df_colsum)
    dimnames(df_colsum) <- list(
        iter_seq,
        c(by, colnames(df)[8:25])
    )
    # Thresholding
    COLSUM_Final <- colSums(df_colsum[2:19])
    Other_rowindex <- rowSums((df_colsum[, 2:19]/COLSUM_Final) <= threshold)>17
    Other <- colSums(df_colsum[Other_rowindex, 2:19])
    df_colsum <- rbind(
        df_colsum[!Other_rowindex, 2:19],
        Other
    )
    iter_seq <- c(names(Other_rowindex[!Other_rowindex]), 'Other')
    df_colsum <- cbind(iter_seq, df_colsum)
    dimnames(df_colsum) <- list(
        iter_seq,
        c(by, colnames(df)[8:25])
    )
    
    return(df_colsum)
}

# Sample_Col_Sum ----------------------------------------------------------
# cols = Kingdom, Phylum, Class, Order, Family, Genus, Species, 
# Ref_MQ181108_121, Ref_MQ181108_122, Ref_MQ181108_123, 
# A1_MQ181108_133, A1_MQ181108_134, A1_MQ181108_135, 
# B1_MQ181108_130, B1_MQ181108_131, B1_MQ181108_132, 
# B1_MQ181108_136, B1_MQ181108_137, B1_MQ181108_138, 
# B2_MQ181221_114, B2_MQ181221_115, B2_MQ181221_116, 
# A2_MQ181221_117, A2_MQ181221_118, A2_MQ181221_119



# Stacked BarPlot ---------------------------------------------------------
# May plot for Phylum, Class, Order, Family level
dim(Sample_Sum_PCOFGS(df = Species_Yasmeen, by = 'Phylum', threshold = .008))
dim(Sample_Sum_PCOFGS(df = Species_Yasmeen, by = 'Class', threshold = .0188))
dim(Sample_Sum_PCOFGS(df = Species_Yasmeen, by = 'Order', threshold = .03))
dim(Sample_Sum_PCOFGS(df = Species_Yasmeen, by = 'Family', threshold = .05))
if(0){
    for (iter_i in 1:4) {
        cluster_by <- c('Phylum', 'Class', 'Order', 'Family')[iter_i]
        threshold <- c(.008, .0188, .03, .05)[iter_i]
        # 
        df <- Sample_Sum_PCOFGS(df = Species_Yasmeen, 
                                by = cluster_by, 
                                threshold = threshold)
        df <- data.frame(
            index = gsub("^.*_", "", rownames(df)),
            Ref = rowMeans(select(df, starts_with(('Ref')))),
            A1 = rowMeans(select(df, starts_with(('A1')))),
            B1 = rowMeans(select(df, starts_with(('B1')))),
            B2 = rowMeans(select(df, starts_with(('B2')))),
            A2 = rowMeans(select(df, starts_with(('A2'))))
        )
        # df$index <- factor(df$index, 
        #                    levels = sort(levels(df$index), decreasing = T))
        dfm <- reshape2::melt(df, id.var = c('index'))
        # 
        assign(paste0('p_',cluster_by),
               ggplot(dfm, aes(x = variable,
                               y = value, 
                               fill = index)) + 
                   geom_bar(stat = "identity", position = "fill") +
                   scale_y_continuous(labels = scales::percent_format(),
                                      breaks = seq(0, 1, .2),
                                      minor_breaks = seq(0, 1, .1)) +
                   scale_fill_brewer(palette = "Set3") +
                   labs(fill = paste0('(', LETTERS[iter_i], ') ', cluster_by)) +
                   # ggtitle(paste0('(', LETTERS[iter_i], ') ', cluster_by)) +
                   xlab('Condition') + ylab('Proportion') +
                   theme_bw() +
                   theme(axis.text.x = element_text(angle = 45, hjust = 1),
                         legend.position = 'top',
                         legend.title = element_text(face = "bold")
                         # legend.title='element_blank()'
                   ) +
                   coord_flip())
    }
    if(0){
        p_Phylum
        ggsave('a_BarPlot_Phylum.eps', device = 'eps', 
               width = 9, height = 6)
        p_Class
        ggsave('b_BarPlot_Class.eps', device = 'eps',
               width = 9, height = 6)
        p_Order
        ggsave('c_BarPlot_Order.eps', device = 'eps',
               width = 9, height = 6)
        p_Family
        ggsave('d_BarPlot_Family.eps', device = 'eps',
               width = 9, height = 6)
        # four together
        gridExtra::grid.arrange(p_Phylum, p_Class, 
                                p_Order, p_Family, ncol = 2)
    }
}



# Bayesian Modeling --------------------------------------------------------
# Multinomial for species, Normal for pollutant removal
if(1){
    if(0){
        clipr::write_clip(t(
            Sample_Sum_PCOFGS(df = Species_Yasmeen, 
                              by = 'Phylum', 
                              threshold = .04)))
        Phylum_WQ_io <- clipr::read_clip_tbl()
    }
    colnames(Phylum_WQ_io)
    N <- nrow(Phylum_WQ_io)
    J_1 <- length(4:7)
    J_2 <- length(8:11)
    K <- length(13:19)
    colnames(Phylum_WQ_io)[4:(4+J_1-1)]                     # WQ_in, J_1
    colnames(Phylum_WQ_io)[(4+J_1):(4+J_1+J_2-1)]           # WQ_out, J_2
    colnames(Phylum_WQ_io)[(4+J_1+J_2):(4+J_1+J_2+K-1)]     # Phylum, K
    colnames(Phylum_WQ_io)[4+J_1+J_2+K]                     # Total
    
    # K = 7 #buckets, size = Total #balls, N = 18 #times    !!!
    # WQ_in & WQ_out have J_1 = 4 & J_2 = 5 # covariates    !!!
}

# Bayesian
# 1. WQ_in & Phylum, Simple -------------------------------------------------
if(1){
    # 
    df_mn_in_Phylum <- cbind(
        1:(N*K),
        Phylum_WQ_io[rep(1:N, each = K), 4:(4+J_1-1)],
        c(t(as.matrix(Phylum_WQ_io[, (4+J_1+J_2):(4+J_1+J_2+K-1)]))),
        rep(Phylum_WQ_io[, (4+J_1+J_2+K)], each = K),
        rep(1:K, N),
        rep(1:N, each = K)
    )
    dimnames(df_mn_in_Phylum) <- list(
        paste(rep(rownames(Phylum_WQ_io), each = K),
              rep(colnames(Phylum_WQ_io)[(4+J_1+J_2):(4+J_1+J_2+K-1)], N), 
              sep = '_'),
        c('Index', 
          'COD_in', 'DO_in', 'TN_in', 'TP_in',
          'Count_seq', 'Total_seq', 'k_seq', 'n_seq')
    )
    # 
    model_mn_in_Phylum <- 
        "model{
        for(i in 1:N){
            Count_seq[(K * i - K + 1):(K * i)] ~ 
                dmulti(lambda[(K * i - K + 1):(K * i)], 
                        Total_seq[K * i])
        }
        for(i in 1:NK){
            # log(lambda[i]) <- inprod(WQ_in[i,], 
                                # beta_in[k_seq[i],])
            log(lambda[i]) <- inprod(WQ_in[i,],
                                beta_in_center[k_seq[i],])
        }
        for(i in 1:K) {
            for(j in 1:J_1){
                beta_in[i,j] ~ dnorm(0, nu_1)
                beta_in_center[i,j] <- beta_in[i,j] - mean(beta_in[i,])
            }
        }
        nu_1 ~ dgamma(1,1)
    }"
    # 
    set.seed(2019)
    result_mn_in_Phylum <- run.jags(
        model = model_mn_in_Phylum,
        data = list(
            NK = N * K,
            N = K,
            K = K,
            J_1 = J_1,
            Count_seq = df_mn_in_Phylum$Count_seq,
            Total_seq = df_mn_in_Phylum$Total_seq,
            WQ_in = as.matrix(
                df_mn_in_Phylum[, c("COD_in", "DO_in",
                                    "TN_in", "TP_in")]
            ),
            k_seq = df_mn_in_Phylum$k_seq
        ),
        monitor = c("beta_in_center"),
        # monitor = c("beta_in"),
        sample = 10000,
        thin = 1000,
        n.chains = 2,
        method = 'parallel'
    )
    # 
    summary(result_mn_in_Phylum)
    # plot(result_mn_in_Phylum, layout = c(2*2, 2*2))
}

# 2. Parallel_Coordinates to Phylum from WQ_in, Simple ------------------------
# 1) to Phylum from WQ_in, simple
# # # # # # # # # # # # # # # # # # # # # # #
# Contribution to Phylum from WQ_in, simple # 
# # # # # # # # # # # # # # # # # # # # # # #
if(1){
    set.seed(2019)
    # melt
    dfm <- data.frame(
        index = rownames(result_mn_in_Phylum$mcmc[[1]]), 
        result_mn_in_Phylum$mcmc[[1]])
    # Phylum Sequence, length K
    p_seq <- gsub("^.*_", "", 
                  colnames(Phylum_WQ_io)[(4+J_1+J_2):(4+J_1+J_2+K-1)])
    # WQ_in, length J_1
    j_1_seq <- colnames(Phylum_WQ_io)[4:(4+J_1-1)]
    colnames(dfm)[1:(1+K*J_1)] <- c('index', 
                                    paste(rep(p_seq, J_1), 
                                          rep(j_1_seq, each = K), 
                                          sep = '_'))
    # 
    # Without "Deinococcus.Thermus"
    # dfm <- select(dfm, - starts_with(p_seq[4]))
    # Sort by Phylum
    if(1){dfm <- dfm[,sort(colnames(dfm))]}   
    # 
    # subsample, for plotting
    dfm <- dfm[sample(NROW(dfm), size = NROW(dfm)/100), ]
    dfm <- reshape2::melt(dfm, id.var = 'index')
    # 
    ggplot(data = dfm, 
           aes(x = variable, y = value, group = index)) +
        geom_line(alpha = .1) + 
        geom_point(alpha = .1) +
        ggtitle('From_WQin_To_Phylum_Simple, with Deinococcus.Thermus') +
        # ggtitle('From_WQin_To_Phylum_Simple, without Deinococcus.Thermus') +
        xlab('Parameters') +
        ylab('Estimated Values') +
        theme_bw() +
        theme(axis.text.x = element_text(angle = 60, hjust = 1))
    # scale_y_continuous(
    #     # limits = c(-2,2), 
    #     breaks = seq(-3, 3, .5), 
    #     minor_breaks = seq(-2, 2, .1))
    if(1){
        ggsave('From_WQin_To_Phylum_Simple, with Deinococcus.Thermus.svg',
               # ggsave('From_WQin_To_Phylum_Simple, without Deinococcus.Thermus.svg',
               device = 'svg', width = 16, height = 9)
    }
    
}


# Bayesian
# 3. WQ_in & Phylum & WQ_out, Complete --------------------------------------
if(1){
    df_mn_in_Phylum_out <- cbind(
        1:(N*K),
        Phylum_WQ_io[rep(1:N, each = K), 4:(4+J_1-1)],
        Phylum_WQ_io[rep(1:N, each = K), (4+J_1):(4+J_1+J_2-1)],
        c(t(as.matrix(Phylum_WQ_io[, (4+J_1+J_2):(4+J_1+J_2+K-1)]))),
        rep(Phylum_WQ_io[, (4+J_1+J_2+K)], each = K),
        rep(1:K, N),
        rep(1:N, each = K)
    )
    dimnames(df_mn_in_Phylum_out) <- list(
        paste(rep(rownames(Phylum_WQ_io), each = K),
              rep(colnames(Phylum_WQ_io)[(4+J_1+J_2):(4+J_1+J_2+K-1)], N), 
              sep = '_'),
        c('Index', 
          'COD_in', 'DO_in', 'TN_in', 'TP_in',
          'COD_out', 'DO_out', 'TN_out', 'TP_out',
          'Count_seq', 'Total_seq', 'k_seq', 'n_seq')
    )
    # 
    model_mn_in_Phylum_out <- 
        "model{
        for(i in 1:N){
            Count_seq[(K * i - K + 1):(K * i)] ~ 
                dmulti(lambda[(K * i - K + 1):(K * i)], 
                        Total_seq[K * i])
            for(j in 1:J_2){
                WQ_out[K * i, j] ~ dnorm(
                    WQ_in[K * i, j] - inprod(
                                        Count_seq[(K * i - K + 1):(K * i)],
                                        beta_out[,j]),
                    .001)
            }
        }
        for(i in 1:NK){
            # log(lambda[i]) <- inprod(WQ_in[i,], 
                                # beta_in[k_seq[i],])
            log(lambda[i]) <- inprod(WQ_in[i,],
                                beta_in_center[k_seq[i],])
        }
        for(i in 1:K) {
            for(j in 1:J_1){
                beta_in[i,j] ~ dnorm(0, nu_1)
                beta_in_center[i,j] <- beta_in[i,j] - mean(beta_in[i,])
            }
            for(j in 1:J_2){
                beta_out[i,j] ~ dnorm(0, nu_2)
            }
        }
        nu_1 ~ dgamma(1,1)
        nu_2 ~ dgamma(1,1)
    }"
    # 
    set.seed(2019)
    result_mn_in_Phylum_out <- run.jags(
        model = model_mn_in_Phylum_out,
        data = list(
            NK = N * K,
            N = K,
            K = K,
            J_1 = J_1,
            J_2 = J_2,
            Count_seq = df_mn_in_Phylum_out$Count_seq,
            Total_seq = df_mn_in_Phylum_out$Total_seq,
            WQ_in = as.matrix(
                df_mn_in_Phylum_out[, c("COD_in", "DO_in",
                                        "TN_in", "TP_in")]
            ),
            WQ_out = as.matrix(
                df_mn_in_Phylum_out[, c("COD_out", "DO_out",
                                        "TN_in", "TP_in")]
            ),
            k_seq = df_mn_in_Phylum_out$k_seq
        ),
        monitor = c("beta_in_center", "beta_out"),
        # monitor = c("beta_in"),
        sample = 10000,
        thin = 1000,
        n.chains = 2,
        method = 'parallel'
    )
    # 
    summary(result_mn_in_Phylum_out)
    # plot(result_mn_in_Phylum_out, layout = c(2*2, 2*2))
}

# 4. Parallel_Coordinates, Complete --------------------------------
# 1) to WQ_out from Phylum, Complete
# 2) to Phylum from WQ_in, Complete
if(0){
    # # # # # # # # # # # # # # # # # # # # # # # # # 
    # Contribution to WQ_out from Phylum, complete  # 
    # # # # # # # # # # # # # # # # # # # # # # # # # 
    # 
    # melt
    dfm <- data.frame(
        index = rownames(result_mn_in_Phylum_out$mcmc[[1]]), 
        result_mn_in_Phylum_out$mcmc[[1]])
    # Only look at beta_out
    dfm <- cbind(dfm$index, select(dfm, starts_with('beta_out')))
    # Phylum Sequence, length K
    p_seq <- gsub("^.*_", "", 
                  colnames(Phylum_WQ_io)[(4+J_1+J_2):(4+J_1+J_2+K-1)])
    # WQ_in, length J_1
    j_1_seq <- colnames(Phylum_WQ_io)[4:(4+J_1-1)]
    j_2_seq <- colnames(Phylum_WQ_io)[(4+J_1):(4+J_1+J_2-1)]
    colnames(dfm)[1:(1+K*J_1)] <- c('index', 
                                    paste(rep(p_seq, J_1), 
                                          rep(j_2_seq, each = K), 
                                          sep = '_'))
    # Ignore "Deinococcus.Thermus"
    # dfm <- select(dfm, - starts_with(p_seq[4]))
    # Sort by Phylum
    if(0){dfm <- dfm[,sort(colnames(dfm))]}    
    # subsample, for plotting
    dfm <- dfm[sample(NROW(dfm), size = NROW(dfm)/100), ]
    dfm <- reshape2::melt(dfm, id.var = 'index')
    # 
    ggplot(data = dfm, 
           aes(x = variable, y = value, group = index)) +
        geom_line(alpha = .1) + 
        geom_point(alpha = .1) +
        ggtitle('From_WQout_To_Phylum, with Deinococcus.Thermus') +
        # ggtitle('From_WQin_To_Phylum, without Deinococcus.Thermus') +
        xlab('Parameters') +
        ylab('Estimated Values') +
        theme_bw() +
        theme(axis.text.x = element_text(angle = 60, hjust = 1))
    # scale_y_continuous(
    #     limits = c(-2,2), 
    #     breaks = seq(-3, 3, .5), minor_breaks = seq(-2, 2, .1))
    if(0){
        ggsave('From_Phylum_To_WQ_out_Complete, with Deinococcus.Thermus.svg',
               device = 'svg', width = 16, height = 9)
    }
}
if(0){
    # # # # # # # # # # # # # # # # # # # # # # # #  
    # Contribution to Phylum from WQ_in, complete # 
    # # # # # # # # # # # # # # # # # # # # # # # #
    # 
    # melt
    dfm <- data.frame(
        index = rownames(result_mn_in_Phylum_out$mcmc[[1]]), 
        result_mn_in_Phylum_out$mcmc[[1]])
    # Only look at beta_out
    dfm <- cbind(dfm$index, select(dfm, starts_with('beta_in')))
    # Phylum Sequence, length K
    p_seq <- gsub("^.*_", "", 
                  colnames(Phylum_WQ_io)[(4+J_1+J_2):(4+J_1+J_2+K-1)])
    # WQ_in, length J_1
    j_1_seq <- colnames(Phylum_WQ_io)[4:(4+J_1-1)]
    j_2_seq <- colnames(Phylum_WQ_io)[(4+J_1):(4+J_1+J_2-1)]
    colnames(dfm)[1:(1+K*J_1)] <- c('index', 
                                    paste(rep(p_seq, J_1), 
                                          rep(j_1_seq, each = K), 
                                          sep = '_'))
    # Ignore "Deinococcus.Thermus"
    # dfm <- select(dfm, - starts_with(p_seq[4]))
    # Sort by Phylum
    if(1){dfm <- dfm[,sort(colnames(dfm))]}    
    # subsample, for plotting
    dfm <- dfm[sample(NROW(dfm), size = NROW(dfm)/100), ]
    dfm <- reshape2::melt(dfm, id.var = 'index')
    # 
    ggplot(data = dfm, 
           aes(x = variable, y = value, group = index)) +
        geom_line(alpha = .1) + 
        geom_point(alpha = .1) +
        ggtitle('From_WQin_To_Phylum_Complete, with Deinococcus.Thermus') +
        # ggtitle('From_WQin_To_Phylum_Complete, without Deinococcus.Thermus') +
        xlab('Parameters') +
        ylab('Estimated Values') +
        theme_bw() +
        theme(axis.text.x = element_text(angle = 60, hjust = 1))
    # scale_y_continuous(
    #     limits = c(-2,2), 
    #     breaks = seq(-3, 3, .5), minor_breaks = seq(-2, 2, .1))
    if(0){
        ggsave('From_WQin_To_Phylum_Complete, with Deinococcus.Thermus.svg',
               # ggsave('From_WQin_To_Phylum_Complete, without Deinococcus.Thermus.svg',
               device = 'svg', width = 16, height = 9)
    }
}


# MVLM --------------------------------------------------------------------
if(0){
    library(MVLM)
    colnames(Phylum_WQ_io)
    
    Y <- Phylum_WQ_io[,c("COD_out", "DO_out", "TN_out", "TP_out")]
    Y <- as.matrix(Y)
    X <- Phylum_WQ_io[,c("COD_in", "DO_in", "TN_in", "TP_in",
                         "p__Actinobacteria", "p__Bacteroidetes", 
                         "p__Chlorobi", "p__Deinococcus.Thermus", 
                         "p__Planctomycetes", "p__Proteobacteria", 
                         "Other"
    )]
    mvlm.res <- mvlm(Y~., data = X)
    summary(mvlm.res)
    mvlm.res$y.rsq #is proportion (1st row as relative, others are absolute)
    
}

# MDMR --------------------------------------------------------------------
if(0){
    library(MDMR)
    Y <- Phylum_WQ_io[,c("COD_out", "DO_out", "TN_out", "TP_out")]
    Y <- as.matrix(Y)
    X <- Phylum_WQ_io[,c("COD_in", "DO_in", "TN_in", "TP_in",
                         "p__Actinobacteria", "p__Bacteroidetes", 
                         "p__Chlorobi", "p__Deinococcus.Thermus", 
                         "p__Planctomycetes", "p__Proteobacteria"
                         # "Other"
    )]
    X <- as.matrix(X)
    # Compute distance matrix
    D <- dist(Y, method = "minkowski", p = 3)
    # Conduct MDMR
    mdmr.res <- mdmr(X = X, D = D)
    # Check results
    summary(mdmr.res)
    
}


# PLSR ---------------------------------------------------------------------
if(1){
    library(pls)
    Y <- Phylum_WQ_io[,c("COD_out", "DO_out", "TN_out", "TP_out")]
    Y <- as.matrix(Y)
    X <- Phylum_WQ_io[,c("COD_in", "DO_in", "TN_in", "TP_in",
                         "p__Actinobacteria", "p__Bacteroidetes", 
                         "p__Chlorobi", "p__Deinococcus.Thermus", 
                         "p__Planctomycetes", "p__Proteobacteria",
                         "Other"
    )]
    X <- as.matrix(X)
    colnames(X) <- paste0('X.',colnames(X))
    colnames(Y) <- paste0('Y.',colnames(Y))
    rownames(X) <- rownames(Y) <- 
        c("Ref_123", "Ref_121", "Ref_122", 
          "A1_133", "A1_134", "A1_135", 
          "B1_130", "B1_131", "B1_132", 
          "B1_136", "B1_137", "B1_138", 
          "B2_114", "B2_115", "B2_116", 
          "A2_117", "A2_118", "A2_119")
    
    df <- data.frame(cbind(X,Y))
    # 
    fit_plsr <- plsr(Y ~ X, data = df, scale = T)
    plot(fit_plsr, asp = 1, line = T)
    # number of components with cross-validation
    fit_plsr_cv <- crossval(fit_plsr, scale = T, segments = 10)
    # selectNcomp(fit_plsr_cv, method = "onesigma", plot = TRUE)
    # selectNcomp(fit_plsr_cv, method = "randomization", plot = TRUE)
    plot(MSEP(fit_plsr_cv), legendpos = 'topright')
    summary(fit_plsr_cv, what = 'validation')
    # select ncomp = 6
    ncomp <- 6
    fit_plsr <- plsr(Y ~ X, data = df, ncomp = ncomp)
    # prediction vs. measured plot
    plot(fit_plsr_cv, ncomp = ncomp, asp = 1, line = T)
    fit_plsr_cv$fitted.values[,,ncomp]
    Y
    plot(fit_plsr_cv$fitted.values[,,ncomp], Y,
         asp = 1, xlim = c(-5,25), ylim = c(-5,25),
         xlab = 'Predicted Value',
         ylab = 'Measured Value')
    abline(a=0, b=1)
    # diagnosis
    plot(fit_plsr_cv, 'biplot', which = 'x')
    plot(fit_plsr_cv, 'biplot', which = 'y')
    plot(fit_plsr_cv, 'biplot', which = 'scores')
    plot(fit_plsr_cv, 'biplot', which = 'loadings')
    # 
    plot(fit_plsr_cv, plottype = "scores", comps = 1:ncomp)
    fit_plsr_cv$scores
    fit_plsr_cv$loadings
    fit_plsr_cv$Yscores
    fit_plsr_cv$Yloadings
    fit_plsr_cv$projection
    # 
}

