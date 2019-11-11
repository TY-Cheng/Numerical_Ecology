# Yasmeen MBR Data
# Importation -------------------------------------------------------------
library(readr)
library(scales)


library(stringr)
library(stringi)


library(adespatial)
library(dendextend)
library(clipr)
library(vegan)

setwd("~/OneDrive/MBR_Yasmeen")


# source('~/OneDrive/MBR_Yasmeen/coldiss.R')

# Read Raw Version

load('MBR_Yasmeen.RData')

if(0){
    MBR_Yasmeen <- data.frame(read_csv(
        "~/OneDrive/MBR_Yasmeen/MBR_Metadata_030219.csv", 
        col_types = cols(Comments = col_skip()),
        na = "NA"))
    MBR_Yasmeen$Date <- as.integer(MBR_Yasmeen$Date)
    MBR_Yasmeen$Condition <- factor(MBR_Yasmeen$Condition)
    MBR_Yasmeen$SteadyStateIndicator <- factor(MBR_Yasmeen$SteadyStateIndicator)
    # MBR_Yasmeen <- MBR_Yasmeen[MBR_Yasmeen$Condition!=4,]
    sapply(MBR_Yasmeen, class)
}




# Pairs Panel --------------------------------------------------------------
if(1){
    source('/Users/chengt/Documents/R/MBR_Ecology/panelutils.R')
    # Steady State Only, w/ wo condition 6
    df <- MBR_Yasmeen[which(MBR_Yasmeen$SteadyStateIndicator==1),]
    df <- df[which(df$Condition!=6),]
    # my_cols <- RColorBrewer::brewer.pal(n = nlevels(df$Condition),
    #                                     name = 'Dark2')
    my_cols <- 1:length(levels(df$Condition))
    my_cols <- my_cols[df$Condition]
    # 
    # Sort Columns
    df <- df[,c(
        # "Date", "Condition", 
        "COD_in", "NH4N_in", "NO3N_in", "PO4P_in", "DO_in",
        # "MLSS", 
        "MLVSS",
        "COD_out", "NH4N_out", "NO3N_out", "PO4P_out", "DO_out"
    )]
    
    
    par(oma = rep(0,4), mar = rep(0,4), cex = 2)
    
    {
        # svg(filename = 'pairs_panel_simplified.svg', width = 16, height = 12)
        pairs(x = df,
              upper.panel = panel.cor,
              diag.panel = panel.hist,
              lower.panel = panel.smooth,
              cex.labels = 2, font.labels = 2, 
              # cex.cor = 1.5,
              cex = 1.4,
              gap = .5, oma = rep(2,4))
        # dev.off()
    }
    
}


# RadViz ------------------------------------------------------------------
if(0){
    library(Radviz)
    set.seed(2019)
    df <- MBR_Yasmeen[which(MBR_Yasmeen$SteadyStateIndicator==1),]
    df <- df[which(df$Condition!=6),]
    IN <- df[,c("COD_in", "PO4P_in", "NH4N_in", "NO3N_in", "DO_in")]
    OUT <- df[,c("COD_out", "PO4P_out", "NH4N_out", "NO3N_out", "DO_out")]
    colnames(IN) <- colnames(OUT) <- str_replace(
        string = colnames(IN), pattern = '_in', replacement = '')
    
    mat <- rbind(IN, OUT)
    mat <- apply(mat,2,do.L,fun=function(x) quantile(x,c(0.1,0.9)))
    IN_scaled <- head(x = mat, n = nrow(IN))
    attr(IN_scaled, "row.names") <- attr(IN, "row.names")
    OUT_scaled <- tail(x = mat, n = nrow(OUT))
    attr(OUT_scaled, "row.names") <- attr(OUT, "row.names")
    
    
    rm(IN, OUT, mat)
    
    # Defining the anchors
    ct.S <- make.S(colnames(IN_scaled))
    # Optimize the position of anchors, by similarity matrix
    ct.sim <- cosine(as.matrix(rbind(IN_scaled,OUT_scaled)))
    in.da(ct.S, ct.sim)
    rv.da(ct.S,ct.sim)
    optim.ct <- do.optim(ct.S,ct.sim,iter=100,n=1000)
    ct.S <- make.S(tail(optim.ct$best,1)[[1]])
    
    # Projection & Plotting
    my_cols <- RColorBrewer::brewer.pal(n = nlevels(df$Condition),
                                        name = 'Dark2')
    # my_cols <- 1:length(levels(df$Condition))
    my_cols <- my_cols[df$Condition]
    
    # svg(filename = 'RadViz.svg', width = 8, height = 8)
    {
        par(oma = rep(0,4), mar = rep(0,4), cex = 1.4)
        ct.rv <- do.radviz(IN_scaled,ct.S)
        plot(ct.rv, point.color = my_cols, cex = 1.5, point.shape = 2)
        ct.rv <- do.radviz(OUT_scaled,ct.S)
        plot(ct.rv, point.color = my_cols, cex = 1.5, point.shape = 0, add = T)
        legend("topright", 
               legend = c(paste0(c('Ref', 'A1', 'B1', 'B2', 'A2'), '_in'),
                          paste0(c('Ref', 'A1', 'B1', 'B2', 'A2'), '_out')),
               pch = rep(c(2,0), each = 5),
               col = rep(unique(my_cols), 2),
               cex = .9, bty = 'n')
    }
    # dev.off()
    
    # rm(ct.S, ct.sim, optim.ct, ct.rv)
    # rm(IN_scaled, OUT_scaled, my_cols)
}


# Parallel Coordinates ----------------------------------------------------
library(ggplot2)

if(1){
    df <- MBR_Yasmeen[which(MBR_Yasmeen$SteadyStateIndicator==1),]
    df <- df[which(df$Condition!=6),]
    # df$COD_in_log <- log10(df$COD_in)
    # df$NH4N_in_log <- log10(df$NH4N_in)
    df <- df[,c("Condition",
                "COD_in", "PO4P_in", "NH4N_in", "NO3N_in","DO_in", 
                "MLSS", "MLVSS", 
                "COD_out", "PO4P_out", "NH4N_out", "NO3N_out", "DO_out")]
    
    df_scaled <- df[,2:ncol(df)]
    df_scaled <- data.frame(apply(df_scaled, 2, scale, center = T))
    df_scaled$NO3N_in <- 0
    df_scaled$index <- attr(df, "row.names")
    df_scaled$Condition <- factor(df$Condition)
    levels(df_scaled$Condition) <- c('Ref', 'A1', 'B1', 'B2', 'A2')
    # melt
    dfm <- reshape2::melt(df_scaled, id.var = c("index", "Condition"))
    
    # svg(filename = 'ParallelCoordinates.svg', width = 9, height = 6)
    
    par(cex = 1.2)
    p <- ggplot(data = dfm, 
                aes(x = variable, y = value, 
                    group = index, color = Condition)) +
        geom_line(alpha = .3, aes(color = Condition)) + 
        geom_point(alpha = .3) +
        xlab('Variables') +
        ylab('Normalized Value') +
        theme_bw() +
        theme(legend.position = "top", legend.direction = "horizontal")
    p
    
    # GGally::ggparcoord(data = df, 
    #                    columns = 2:ncol(df), 
    #                    groupColumn='Condition',alphaLines = .5)+
    #     xlab('')+ ylab('')+theme_bw()
    
    # dev.off()
    
    rm(df_scaled, dfm)
    rm(p)
}

# MVLM --------------------------------------------------------------------
library(MVLM)
if(1){
    # Steady State Only, w/ wo condition 6
    df <- MBR_Yasmeen[which(MBR_Yasmeen$SteadyStateIndicator==1),]
    df <- df[which(df$Condition!=6),]
    # Sort Columns
    df <- df[,c(
        "Date", "Condition", 
        "COD_in", "PO4P_in", "NH4N_in", "NO3N_in", "DO_in",
        "MLSS", "MLVSS",
        "COD_out", "PO4P_out", "NH4N_out", "NO3N_out", "DO_out"
    )]
    # df$Nutrients_in
    df$Nutrients_in <- df$NH4N_in + df$NO3N_in + df$PO4P_in
    
    
    Y <- df[,c("COD_out", "PO4P_out", "NH4N_out", "NO3N_out", "DO_out")]
    Y <- as.matrix(Y)
    # X <- df[,c("COD_in", "Nutrients_in", "DO_in", "MLVSS")]
    X <- df[,c("COD_in", "Nutrients_in", "DO_in")]
    mvlm.res <- mvlm(Y~.^2, data = X)
    summary(mvlm.res)
    mvlm.res$y.rsq #is proportion (1st row as relative, others are absolute)
    
    mvlm.res$beta.hat
    # write.csv(mvlm.res$beta.hat, file = 'beta.csv')
    
    Res <- residuals(mvlm.res)
    psych::describe(Res)
    apply(na.omit(Res), MARGIN = 2, norm, type = '2')/nrow(Res)
    
    # rm(Y, X, Res)
    # rm(mvlm.res)
}


# Sensitivity Analysis of MVLM --------------------------------------------
X_predict <- 
    rbind(
        expand.grid(
            COD_in = seq(from = 390, to = 410, by = 2),
            Nutrients_in = seq(from = 32, to = 37, by = .5),
            DO_in = seq(from = 0, to = .5, by = .1)
        ),
        expand.grid(
            COD_in = seq(from = 290, to = 310, by = 2),
            Nutrients_in = seq(from = 23, to = 28, by = .5),
            DO_in = seq(from = 3.2, to = 3.7, by = .1)
        ),
        expand.grid(
            COD_in = seq(from = 190, to = 210, by = 2),
            Nutrients_in = seq(from = 15, to = 20, by = .5),
            DO_in = seq(from = 6.5, to = 7, by = .1)
        ),
        expand.grid(
            COD_in = seq(from = 90, to = 110, by = 2),
            Nutrients_in = seq(from = 15, to = 20, by = .5),
            DO_in = seq(from = 6.5, to = 7, by = .1)
        ),
        expand.grid(
            COD_in = seq(from = 150, to = 170, by = 2),
            Nutrients_in = seq(from = 23, to = 28, by = .5),
            DO_in = seq(from = 3.2, to = 3.7, by = .1)
        )
    )

Y_predict <- predict(mvlm.res, X_predict)

X_predict[
    seq_len(nrow(Y_predict))[rowSums(Y_predict<0)>0],
]
Y_predict[rowSums(Y_predict<0)>0,]
# RDA ---------------------------------------------------------------------
if (0) {
    df <- MBR_Yasmeen_imputed[,c(
        "COD_in", "PO4P_in", "NH4N_in", "NO3N_in", "DO_in",
        "MLVSS",
        "COD_out", "PO4P_out", "NH4N_out", "NO3N_out", "DO_out",
        "Condition"
    )]
    Y <- df[,c("COD_out", "PO4P_out", "NH4N_out", "NO3N_out", "DO_out")]
    Y <- as.matrix(Y)
    X <- df[,c("COD_in", "PO4P_in", "NH4N_in", "DO_in",
               "MLVSS")]
    # CCA 82%
    if(0){
        summary(cca(Y~.^3,X), axes = F)
        plot(cca(Y~.,X), scaling = 2)
    }
    # RDA 88%
    rda.MBR <- rda(Y~.,X)
    if(0){
        plot(rda.MBR, scaling = 2, 
             xlim = c(-2, 2), ylim= c(-3, 3),
             # 都是output，拿lc（obs）
             display = c('sp', 'lc'),
             type = 'text')
        plot(rda.MBR, scaling = 2, 
             xlim = c(-3, 3), ylim= c(-3, 3),
             # 都是input，拿wa（obs）
             display = c('wa', 'cn'),
             type = 'text')
    }
    
    {
        rda.MBR.3 <- rda(Y~.^3,X)
        # summary(rda.MBR.3, axes = F)
        # svg('RDA.svg', width = 8, height = 8)
        par(cex = 1.1, oma= c(.5,.5,.5,.5))
        # combined (arrows only)
        proportion <- round(rda.MBR.3$CCA$eig/
                                sum(rda.MBR.3$CCA$eig, rda.MBR.3$CA$eig)*100, 
                            digits = 2)
        plot(rda.MBR, scaling = 2, 
             xlim = c(-5, 5), ylim= c(-5, 5),
             display = c('sp', 'cn'),
             # 即rda.MBR$CCA$biplot (input, ‘sp’)
             # 与rda.MBR$CCA$v (output, 'cn') 
             xlab = paste('RDA1 (', proportion[1], '%)', sep = ''),
             ylab = paste('RDA2 (', proportion[2], '%)', sep = ''),
             type = 'text')
        # labels(rda.MBR$CCA$eig/sum(rda.MBR$CCA$eig))
        my_cols <- df[rownames(df),]$Condition
        my_cols <- alpha(my_cols, alpha = .6)
        #wa input
        points(rda.MBR$CCA$wa[,1:2]*10, 
               col = my_cols, pch = 2, cex = .7)
        #lc output
        points(rda.MBR$CCA$u[,1:2]*10, 
               col = my_cols, pch = 6, cex = .7)
        # legends
        lv <- levels(factor(df[rownames(df),]$Condition))
        legend('topright', 
               legend = c(paste(lv, '_in',sep = ''), 
                          paste(lv, '_out',sep = '')),
               col = c(alpha(lv, alpha = .6),
                       alpha(lv, alpha = .6)),
               pch = c(rep(2,length(lv)),
                       rep(6,length(lv))),
               bty = 'n',
               cex = 1.1)
        title('Redundancy Analysis')
        # dev.off()
        rm(rda.MBR.3)
    }
}




# Stats of Imputed DF -----------------------------------------------------
if(0){
    MBR_Yasmeen_imputed <- data.frame(
        read_csv(
            "MBR_Yasmeen_imputed.csv", 
            col_types = cols(Condition = col_integer(), 
                             SteadyStateIndicator = col_integer()))
    )
    MBR_Yasmeen_imputed$Condition <- factor(MBR_Yasmeen_imputed$Condition)
    
    df <- MBR_Yasmeen_imputed[,-c(1,17,18)]
    for (i in levels(df$Condition)) {
        print(i)
        print(psych::describe(x = df[df$Condition == i, ], 
                              type = 1,
                              quant = c(.25, .75)
        )
        )
    }
}


# Transformation & Distance -----------------------------------------------
# Color plots of a dissimilarity matrix, without and with ordering
if(0){
    df <- MBR_Yasmeen[,-c(7,8,10,13)]
    df <- t(df)
    dist_chi2 <- dist.ldc(df, method = 'chisquare')
    coldiss(dist_chi2, nc = ncol(df), diag = T)
    mtext(text = 'Chi_2', line = 1, adj = .45, cex = 3)
}

if(0){
    # svg(filename = 'Distance.svg', width = 16, height = 9)
    df <- MBR_Yasmeen[,-c(6,7,8,10,13)]
    # df <- t(df)
    dist_eucl <- dist(scale(df), method = 'euclidean')
    coldiss(dist_eucl, nc = ncol(df), diag = T)
    mtext(text = 'Euclidian', line = 1, adj = .45, cex = 3)
    which(MBR_Yasmeen$Condition ==  '5')
    # dev.off()
}


# Hierarchical Clustering -------------------------------------------------
# hierarchical clustering
if(0){
    
    ch.average <- hclust(d = dist_eucl, method = 'average')
    plot(ch.average)
    ch.mcquitty <- hclust(d = dist_eucl, method = 'mcquitty')
    plot(ch.mcquitty)
    
    ch.centroid <- hclust(d = dist_eucl, method = 'centroid')
    plot(ch.centroid)
    ch.median <- hclust(d = dist_eucl, method = 'median')
    plot(ch.median)
    
    
    # plot the Shepard-like diagrams cor接近1越好
    ch.mcquitty.coph <- cophenetic(ch.mcquitty)
    cor(dist_eucl, ch.mcquitty.coph)
    plot(dist_eucl, ch.mcquitty.coph,
         asp = 1, 
         main = round(cor(dist_eucl, ch.mcquitty.coph), digits = 3))
    abline(0,1)
    lines(lowess(dist_eucl, ch.mcquitty.coph), col='red')
    
    
    ch.median.coph <- cophenetic(ch.median)
    cor(dist_eucl, ch.median.coph)
    plot(dist_eucl, ch.median.coph,
         asp = 1, 
         main = round(cor(dist_eucl, ch.median.coph), digits = 3))
    abline(0,1)
    lines(lowess(dist_eucl, ch.median.coph), col='red')
    # Gower distance 越小越好
    (gow.dist.mcquitty <- sum((dist_eucl - ch.mcquitty.coph)^2))
    (gow.dist.median <- sum((dist_eucl - ch.median.coph)^2))
    
    
    
    # Where to cut (fusion level values)
    plot(ch.mcquitty$height, nrow(MBR_Yasmeen):2, 
         type = 's',
         main = 'Fusion levels - Euclidean - Mcquitty',
         ylab = 'k (number of clusters)',
         xlab = 'h (node height)',
         col = 'gray'
    )
    text(ch.mcquitty$height, nrow(MBR_Yasmeen):2, nrow(MBR_Yasmeen):2,
         col = 'red', cex = .8)
    
    plot(ch.median$height, nrow(MBR_Yasmeen):2, 
         type = 's',
         main = 'Fusion levels - Euclidean - Median',
         ylab = 'k (number of clusters)',
         xlab = 'h (node height)',
         col = 'gray'
    )
    text(ch.median$height, nrow(MBR_Yasmeen):2, nrow(MBR_Yasmeen):2,
         col = 'red', cex = .8)
    
    # 观察觉得取6个cluster, cutree
    k <- 6
    ch.mcquitty.g <- cutree(ch.mcquitty, k = k)
    ch.median.g <- cutree(ch.median, k = k)
    table(ch.mcquitty.g, ch.median.g)
    
    # 对比
    tanglegram(
        untangle(dendlist(
            as.dendrogram(ch.mcquitty),
            as.dendrogram(ch.median)
        )),
        sort = T,
        main_left = 'mcquitty',
        main_right = 'median'
    )
    
    
    # Multiscale Bootstrap Resampling；注意transpose！
    # AU: approximately unbiased; BP: bootstrap probability
    # 红色很robust，蓝色还行
    
    ch.pv <- pvclust::pvclust(scale(t(df)),
                              method.hclust = 'ward.D2',
                              method.dist = 'euclidean',
                              parallel = T)
    plot(ch.pv)
    pvrect2(ch.pv, alpha = .99, pv = 'au', border = 2)
    lines(ch.pv)
    pvrect2(ch.pv, alpha = .91, pv = 'au', border = 4)
    
    
    plot(ch.pv$hclust$height, nrow(MBR_Yasmeen):2, 
         type = 's',
         main = 'Fusion levels - Euclidean - pv',
         ylab = 'k (number of clusters)',
         xlab = 'h (node height)',
         col = 'gray'
    )
    text(ch.pv$hclust$height, nrow(MBR_Yasmeen):2, nrow(MBR_Yasmeen):2,
         col = 'red', cex = .8)
    
    # Silhouette
    Si <- numeric(nrow(df))
    for (k in 2:(nrow(df) - 1)){
        sil <- silhouette(x = cutree(tree = ch.pv$hclust, k = k), 
                          dist = dist_eucl)
        Si[k] <- summary(sil)$avg.width
    }
    k.best <- which.max(Si)
    plot(
        1:nrow(df),
        Si,
        type = "h",
        main = "Silhouette-optimal number of clusters",
        xlab = "k (number of clusters)",
        ylab = "Average silhouette width"
    )
    axis(
        1,
        k.best,
        paste("optimum", k.best, sep = "\n"),
        col = "red",
        font = 2,
        col.axis = "red"
    )
    points(k.best,
           max(Si),
           pch = 16,
           col = "red",
           cex = 1.5
    )
    
    
}
