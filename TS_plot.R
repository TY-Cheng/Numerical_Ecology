setwd("/Users/chengt/Documents/R/MBR_Ecology")
load('MBR_Yasmeen.RData')
library(tidyverse)
library(reshape2)
if (1) {
    # wo condition 6
    df <- MBR_Yasmeen[which(MBR_Yasmeen$Condition!=6),]
    df$Condition <- factor(df$Condition)
    df$TN_in <- df$NH4N_in + df$NO3N_in
    df$TN_out <- df$NH4N_out + df$NO3N_out
    df$TP_in <- df$PO4P_in
    df$TP_out <- df$PO4P_out
    # Sort Columns
    df <- df[,c(
        "Date", "Condition", 
        "DO_in", "TN_in", "TP_in", "COD_in",
        "DO_out", "TN_out", "TP_out", "COD_out",
        "NH4N_in", "NH4N_out"
    )]
    rectangle <- data.frame()
    for (i in levels(df$Condition)) {
        rectangle[as.numeric(i),1] <- min(df[df$Condition==i,]$Date)
        rectangle[as.numeric(i),2] <- max(df[df$Condition==i,]$Date)
    }
    rectangle$Condition <- c('Ref', 'A1', 'B1', 'B2', 'A2')
    rectangle$position <- (rectangle$V1+rectangle$V2)/2
    rectangle$colour <- alpha(RColorBrewer::brewer.pal(
        n = 8, name = 'Set1'), alpha = .3)[c(1,2,3,5,6)]
    # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
    # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
    shape_in <- 2
    shape_out <- 0
    shape_efficiency <- 1
    color_in <- RColorBrewer::brewer.pal(8, 'Paired')[2]
    color_out <- RColorBrewer::brewer.pal(8, 'Paired')[4]
    color_efficiency <- RColorBrewer::brewer.pal(8, 'Paired')[6]
    # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
    # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
}



# TS COD ------------------------------------------------------------------
if (1) {
    # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
    svg(filename = 'TimeSeries_COD.svg', width = 12, height = 7)
    par(cex = 1,
        # oma = c(3, 2, 4, 2),
        mar = c(3.5, 3.5, 4, 3.5),
        # mgp = c(.5, .5, 0),
        xpd = NA)
    # Rectangle
    y <- df$COD_in
    plot(x = df$Date, y = y, cex = 1,
         xlim = range(df$Date), ylim = c(0, 1000),
         xaxt = 'n', yaxt = 'n', type = 'n',
         main = '', xlab = '', ylab = '', cex.axis = 1)
    axis(side = 1, at = c(1,10,23,39,53,67), labels = c(1,10,23,39,53,67),
         col = 'black', col.axis = 'black', lwd = 2.5, cex.axis = 1)
    for (i in 1:nrow(rectangle)) {
        rect(xleft = rectangle[i, 'V1'], xright = rectangle[i, 'V2'],
             ybottom = -40,  ytop = 1040,
             col = rectangle$colour[i], density = NA)
    }
    
    # Lines
    axis(side = 2, at = seq(0, 500, 50), labels = seq(0, 500, 50),
         col = 'black', col.axis = 'black', lwd = 2.5, 
         cex.axis = 1)
    lines(x = df$Date, y = y, type = 'o', cex = 1.1, lwd = 1.3,
          col = color_in,
          pch = shape_in)
    y <- df$COD_out
    lines(x = df$Date, y = y, type = 'o', cex = 1.1, lwd = 1.3,
          col = color_out,
          pch = shape_out)
    # Removal Efficiency
    par(new = T)
    plot(x = df$Date, y = y, cex = 1,
         xlim = range(df$Date), ylim = c(-100, 100),
         xaxt = 'n', yaxt = 'n', type = 'n',
         main = '', xlab = '', ylab = '', cex.axis = 1)
    axis(side = 4, at = seq(0, 100, 10), labels = seq(0, 100, 10),
         col = 'black', col.axis = 'black', 
         lwd = 2.5, 
         cex.axis = 1)
    # 
    y <- (1-(df$COD_out/df$COD_in))*100
    lines(x = df$Date, y = y, type = 'o', cex = 1.1, lwd = 1.3,
          col = color_efficiency,
          pch = shape_efficiency)
    # 
    abline(v = c(1,10,23,39,53,67), 
           h = c(seq(-100, 0, 20), seq(0, 100, 20)),
           col = 'gray', lty = 5, lwd = 1, xpd = F)
    # abline(h = 0, lty = 2, xpd = F)
    mtext(text = rectangle$Condition, side = 3, line = .5,
          adj = c(.08, .24, .45, .67, .88),
          col = 'black', cex = 1.4)
    
    title('(A)', cex.main = 1.3)
    mtext(text = 'Time (day)', side = 1, line = 2, adj = .5, cex = 1.7)
    mtext(text = 'COD removal efficiency (%)', 
          side = 4, line = 2.2, adj = 1.1, col = 'black', cex = 1.6)
    mtext(text = 'COD concentration (mg/L)', 
          side = 2, line = 2, adj = 0, col = 'black', cex = 1.6)
    shape::Arrows(x0 = c(5, 5, 60, 60), 
                  y0 = c(-70, -70, 60, 60), 
                  x1 = c(3, 5, 60, 62), 
                  y1 = c(-70, -85, 45, 60),
                  arr.type = 'triangle', lwd = 1)
    # Legend add in the first
    # Points legend
    legend(x = 38, y = 10,
           # 'right',
           # inset = c(.03),
           legend = c('Removal efficiency',
                      'Influent concentration',
                      'Effluent concentration'),
           col = c(color_efficiency, 
                   color_in,
                   color_out),
           pch = c(shape_efficiency, shape_in, shape_out),
           lwd = 1.3,
           # bty = 'n',
           bg = 'white',
           cex = 1.8)
    # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
    dev.off()
}



# TS TN -------------------------------------------------------------------
if (1) {
    # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
    svg(filename = 'TimeSeries_TN.svg', width = 12, height = 7)
    par(cex = 1,
        # oma = c(3, 2, 4, 2),
        mar = c(3.5, 3.5, 4, 3.5),
        # mgp = c(.5, .5, 0),
        xpd = NA)
    # Rectangle
    y <- df$TN_in
    plot(x = df$Date, y = y, cex = 1,
         xlim = range(df$Date), ylim = c(0, 100),
         xaxt = 'n', yaxt = 'n', type = 'n',
         main = '', xlab = '', ylab = '', cex.axis = 1)
    axis(side = 1, at = c(1,10,23,39,53,67), labels = c(1,10,23,39,53,67),
         col = 'black', col.axis = 'black', lwd = 2.5, cex.axis = 1)
    for (i in 1:nrow(rectangle)) {
        rect(xleft = rectangle[i, 'V1'], xright = rectangle[i, 'V2'],
             ybottom = -4,  ytop = 104,
             col = rectangle$colour[i], density = NA)
    }
    
    # Lines
    axis(side = 2, at = seq(0, 50, 5), labels = seq(0, 50, 5),
         col = 'black', col.axis = 'black', lwd = 2.5, 
         cex.axis = 1)
    lines(x = df$Date, y = y, type = 'o', cex = 1.1, lwd = 1.3,
          col = color_in,
          pch = shape_in)
    y <- df$TN_out
    lines(x = df$Date, y = y, type = 'o', cex = 1.1, lwd = 1.3,
          col = color_out,
          pch = shape_out)
    # Removal Efficiency
    par(new = T)
    plot(x = df$Date, y = y, cex = 1,
         xlim = range(df$Date), ylim = c(-100, 100),
         xaxt = 'n', yaxt = 'n', type = 'n',
         main = '', xlab = '', ylab = '', cex.axis = 1)
    axis(side = 4, at = seq(0, 100, 10), labels = seq(0, 100, 10),
         col = 'black', col.axis = 'black', 
         lwd = 2.5, 
         cex.axis = 1)
    # 
    y <- (1-(df$TN_out/df$TN_in))*100
    lines(x = df$Date, y = y, type = 'o', cex = 1.1, lwd = 1.3,
          col = color_efficiency,
          pch = shape_efficiency)
    # 
    abline(v = c(1,10,23,39,53,67), 
           h = c(seq(-100, 0, 20), seq(0, 100, 20)),
           col = 'gray', lty = 5, lwd = 1, xpd = F)
    # abline(h = 0, lty = 2, xpd = F)
    mtext(text = rectangle$Condition, side = 3, line = .5,
          adj = c(.08, .24, .45, .67, .88),
          col = 'black', 
          cex = 1.4)
    
    title('(B)', cex.main = 1.3)
    mtext(text = 'Time (day)', side = 1, line = 2, adj = .5, cex = 1.7)
    mtext(text = 'TN removal efficiency (%)', 
          side = 4, line = 2.2, adj = 1, col = 'black', cex = 1.6)
    mtext(text = 'TN concentration (mg/L)', 
          side = 2, line = 2, adj = 0, col = 'black', cex = 1.6)
    shape::Arrows(x0 = c(5, 5, 60, 60), 
                  y0 = c(-70, -70, 60, 60), 
                  x1 = c(3, 5, 60, 62), 
                  y1 = c(-70, -85, 45, 60),
                  arr.type = 'triangle', lwd = 1)
    # Legend add in the first
    # Points legend
    legend(x = 38, y = 10,
           # 'right',
           # inset = c(.03),
           legend = c('Removal efficiency',
                      'Influent concentration',
                      'Effluent concentration'),
           col = c(color_efficiency, 
                   color_in,
                   color_out),
           pch = c(shape_efficiency, shape_in, shape_out),
           lwd = 1.3,
           # bty = 'n',
           bg = 'white',
           cex = 1.8)
    # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
    dev.off()
}




# TS TP -------------------------------------------------------------------
if (1) {
    # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
    svg(filename = 'TimeSeries_TP.svg', width = 12, height = 7)
    par(cex = 1,
        # oma = c(3, 2, 4, 2),
        mar = c(3.5, 3.5, 4, 3.5),
        # mgp = c(.5, .5, 0),
        xpd = NA)
    # Rectangle
    y <- df$TP_in
    plot(x = df$Date, y = y, cex = 1,
         xlim = range(df$Date), ylim = c(0, 20),
         xaxt = 'n', yaxt = 'n', type = 'n',
         main = '', xlab = '', ylab = '', cex.axis = 1)
    axis(side = 1, at = c(1,10,23,39,53,67), labels = c(1,10,23,39,53,67),
         col = 'black', col.axis = 'black', lwd = 2.5, cex.axis = 1)
    for (i in 1:nrow(rectangle)) {
        rect(xleft = rectangle[i, 'V1'], xright = rectangle[i, 'V2'],
             ybottom = -.8,  ytop = 20.7,
             col = rectangle$colour[i], density = NA)
    }
    
    # Lines
    axis(side = 2, at = seq(0, 10, 1), labels = seq(0, 10, 1),
         col = 'black', col.axis = 'black', lwd = 2.5, 
         cex.axis = 1)
    lines(x = df$Date, y = y, type = 'o', cex = 1.1, lwd = 1.3,
          col = color_in,
          pch = shape_in)
    y <- df$TP_out
    lines(x = df$Date, y = y, type = 'o', cex = 1.1, lwd = 1.3,
          col = color_out,
          pch = shape_out)
    # Removal Efficiency
    par(new = T)
    plot(x = df$Date, y = y, cex = 1,
         xlim = range(df$Date), ylim = c(-300, 100),
         xaxt = 'n', yaxt = 'n', type = 'n',
         main = '', xlab = '', ylab = '', cex.axis = 1)
    axis(side = 4, at = seq(-100, 100, 20), labels = seq(-100, 100, 20),
         col = 'black', col.axis = 'black', lwd = 2.5, 
         cex.axis = 1)
    # 
    y <- (1-(df$TP_out/df$TP_in))*100
    lines(x = df$Date, y = y, type = 'o', cex = 1.1, lwd = 1.3,
          col = color_efficiency,
          pch = shape_efficiency)
    # 
    abline(v = c(1,10,23,39,53,67), 
           h = c(seq(-300, -100, 40), seq(-100, 100, 40)),
           col = 'gray', lty = 5, lwd = 1, xpd = F)
    # abline(h = 0, lty = 2, xpd = F)
    mtext(text = rectangle$Condition, side = 3, line = .5,
          adj = c(.08, .24, .45, .67, .88),
          col = 'black', cex = 1.4)
    
    title('(C)', cex.main = 1.3)
    mtext(text = 'Time (day)', side = 1, line = 2, adj = .5, cex = 1.7)
    mtext(text = 'TP removal efficiency (%)', 
          side = 4, line = 2.2, adj = 1, col = 'black', cex = 1.6)
    mtext(text = 'TP concentration (mg/L)', 
          side = 2, line = 2, adj = 0, col = 'black', cex = 1.6)
    shape::Arrows(x0 = c(5, 5, 60, 60), 
                  y0 = c(-240, -240, 0, 0), 
                  x1 = c(3, 5, 60, 62), 
                  y1 = c(-240, -270, -30, 0),
                  arr.type = 'triangle', lwd = 1)
    # Legend add in the first
    # Points legend
    legend(x = 38, y = -80,
           # 'right',
           # inset = c(.03),
           legend = c('Removal efficiency',
                      'Influent concentration',
                      'Effluent concentration'),
           col = c(color_efficiency, 
                   color_in,
                   color_out),
           pch = c(shape_efficiency, shape_in, shape_out),
           lwd = 1.3,
           # bty = 'n',
           bg = 'white',
           cex = 1.8)
    # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
    dev.off()
}



# TS NH4N -----------------------------------------------------------------
if (1) {
    # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
    svg(filename = 'TimeSeries_NH4N.svg', width = 12, height = 7)
    par(cex = 1,
        # oma = c(3, 2, 4, 2),
        mar = c(3.5, 3.5, 4, 3.5),
        # mgp = c(.5, .5, 0),
        xpd = NA)
    # Rectangle
    y <- df$NH4N_in
    plot(x = df$Date, y = y, cex = 1,
         xlim = range(df$Date), ylim = c(0, 100),
         xaxt = 'n', yaxt = 'n', type = 'n',
         main = '', xlab = '', ylab = '', cex.axis = 1)
    axis(side = 1, at = c(1,10,23,39,53,67), labels = c(1,10,23,39,53,67),
         col = 'black', col.axis = 'black', lwd = 2.5, cex.axis = 1)
    for (i in 1:nrow(rectangle)) {
        rect(xleft = rectangle[i, 'V1'], xright = rectangle[i, 'V2'],
             ybottom = -4,  ytop = 104,
             col = rectangle$colour[i], density = NA)
    }
    
    # Lines
    axis(side = 2, at = seq(0, 50, 5), labels = seq(0, 50, 5),
         col = 'black', col.axis = 'black', lwd = 2.5, cex.axis = .8)
    lines(x = df$Date, y = y, type = 'o', cex = 1.1, lwd = 1.3,
          col = color_in,
          pch = shape_in)
    y <- df$NH4N_out
    lines(x = df$Date, y = y, type = 'o', cex = 1.1, lwd = 1.3,
          col = color_out,
          pch = shape_out)
    # Removal Efficiency
    par(new = T)
    plot(x = df$Date, y = y, cex = 1,
         xlim = range(df$Date), ylim = c(-100, 100),
         xaxt = 'n', yaxt = 'n', type = 'n',
         main = '', xlab = '', ylab = '', cex.axis = 1)
    axis(side = 4, at = seq(0, 100, 10), labels = seq(0, 100, 10),
         col = 'black', col.axis = 'black', 
         lwd = 2.5, 
         cex.axis = 1)
    # 
    y <- (1-(df$NH4N_out/df$NH4N_in))*100
    lines(x = df$Date, y = y, type = 'o', cex = 1.1, lwd = 1.3,
          col = color_efficiency,
          pch = shape_efficiency)
    # 
    abline(v = c(1,10,23,39,53,67), 
           h = c(seq(-100, 0, 20), seq(0, 100, 20)),
           col = 'gray', lty = 5, lwd = 1, xpd = F)
    # abline(h = 0, lty = 2, xpd = F)
    mtext(text = rectangle$Condition, side = 3, line = .5,
          adj = c(.08, .24, .45, .67, .88),
          col = 'black', cex = 1.4)
    
    title('(D)', cex.main = 1.3)
    mtext(text = 'Time (day)', side = 1, line = 2, adj = .5, cex = 1.7)
    mtext(text = 'NH4N removal efficiency (%)', 
          side = 4, line = 2.2, adj = 1.1, col = 'black', cex = 1.6)
    mtext(text = 'NH4N concentration (mg/L)', 
          side = 2, line = 2, adj = 0, col = 'black', cex = 1.6)
    shape::Arrows(x0 = c(5, 5, 60, 60), 
                  y0 = c(-80, -80, 50, 50), 
                  x1 = c(3, 5, 60, 62), 
                  y1 = c(-80, -95, 35, 50),
                  arr.type = 'triangle', lwd = 1)
    # Legend add in the first
    # Points legend
    legend(x = 38, y = 10,
           # 'right',
           # inset = c(.03),
           legend = c('Removal efficiency',
                      'Influent concentration',
                      'Effluent concentration'),
           col = c(color_efficiency, 
                   color_in,
                   color_out),
           pch = c(shape_efficiency, shape_in, shape_out),
           lwd = 1.3,
           # bty = 'n',
           bg = 'white',
           cex = 1.8)
    # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
    dev.off()
}



# Time Series -------------------------------------------------------------
if(0){
    {
        library(tidyverse)
        library(reshape2)
        # wo condition 6
        # df <- MBR_Yasmeen[which(MBR_Yasmeen$SteadyStateIndicator==1),]
        df <- MBR_Yasmeen[which(MBR_Yasmeen$Condition!=6),]
        df$Condition <- factor(df$Condition)
        # df$COD_in_log <- log(df$COD_in)
        df$TN_in <- df$NH4N_in + df$NO3N_in
        df$TN_out <- df$NH4N_out + df$NH4N_out
        df$TP_in <- df$PO4P_in
        df$TP_out <- df$PO4P_out
        # Sort Columns
        df <- df[,c(
            "Date", "Condition",
            # "COD_in_log",
            "DO_in", "TN_in", "TP_in", "COD_in",
            # "MLVSS",
            "DO_out", "TN_out", "TP_out", "COD_out"
        )]
        rectangle <- data.frame()
        for (i in levels(df$Condition)) {
            rectangle[as.numeric(i),1] <- min(df[df$Condition==i,]$Date)
            rectangle[as.numeric(i),2] <- max(df[df$Condition==i,]$Date)
        }
        rectangle$Condition <- c('Ref', 'A1', 'B1', 'B2', 'A2')
        rectangle$position <- (rectangle$V1+rectangle$V2)/2
        
        dfm <- melt(df[,-2], id.vars = 'Date', variable.name = 'Parameter')
        rm(i)
    }
    
    {
        # Check colnames matching
        print(levels(dfm$Parameter))
        
        # Color brewing
        colourlevel <- RColorBrewer::brewer.pal(n = 8, name = 'Dark2')
        colourlevel <- colourlevel[c(1,2,3,
                                     1,2,3)]
        # Shape forming
        shapelevel <- c(0,0,0,
                        2,2,2)
    }
    
    # ggplot
    ggplot(data = dfm) +
        geom_rect(
            data = rectangle,
            mapping = aes(xmin = V1, xmax = V2, fill = Condition),
            ymin = -Inf, ymax = Inf, alpha = 0.2
        ) +
        scale_fill_manual(values = RColorBrewer::brewer.pal(5, name = 'Dark2')) +
        geom_vline(
            aes(xintercept = V1),
            data = rectangle,
            colour = "grey50", alpha = 0.5
        ) +
        geom_text(
            aes(x = position,
                y = 35,
                label = Condition),
            data = rectangle,
            size = 5, vjust = 0, hjust = .3
        ) +
        #
        geom_point(mapping = aes(x = Date, y = value,
                                 color = Parameter,
                                 shape = Parameter)) +
        geom_line(mapping = aes(x = Date, y = value, color = Parameter)) +
        #
        scale_shape_manual(values = shapelevel)+
        scale_color_manual(values = colourlevel)+
        # scale_size_manual(values=c(2,3,4))+
        labs(y = 'Parameter') +
        scale_x_continuous(name = 'Day',
                           breaks = c(rectangle$V1, 67),
                           labels = c(rectangle$V1, 67)) +
        guides(fill = guide_legend(order=1)) +
        theme_bw()
    # ggsave('TimeSeries.svg', width = 10, height = 6)
}



# TS, Traditional ---------------------------------------------------------
if(0){
    {
        library(tidyverse)
        library(reshape2)
        # wo condition 6
        df <- MBR_Yasmeen[which(MBR_Yasmeen$Condition!=6),]
        df$Condition <- factor(df$Condition)
        df$TN_in <- df$NH4N_in + df$NO3N_in
        df$TN_out <- df$NH4N_out + df$NO3N_out
        df$TP_in <- df$PO4P_in
        df$TP_out <- df$PO4P_out
        # Sort Columns
        df <- df[,c(
            "Date", "Condition", 
            "DO_in", "TN_in", "TP_in", "COD_in",
            "DO_out", "TN_out", "TP_out", "COD_out"
        )]
        rectangle <- data.frame()
        for (i in levels(df$Condition)) {
            rectangle[as.numeric(i),1] <- min(df[df$Condition==i,]$Date)
            rectangle[as.numeric(i),2] <- max(df[df$Condition==i,]$Date)
        }
        rectangle$Condition <- c('Ref', 'A1', 'B1', 'B2', 'A2')
        rectangle$position <- (rectangle$V1+rectangle$V2)/2
        rectangle$colour <- alpha(RColorBrewer::brewer.pal(
            n = 5, name = 'Dark2'), alpha = .3)
        
        # 
        lower <- c("TN_in", "TP_in",
                   "COD_out", "TN_out", "TP_out")
        upper <- c('COD_in')
        # Color & Shape
        colourlevel_lower <- RColorBrewer::brewer.pal(n = 5, name = 'Dark2')
        colourlevel_lower <- colourlevel_lower[c(3,4,
                                                 1,3,4)]
        colourlevel_upper <- colourlevel_lower[3]
        names(colourlevel_lower) <- lower
        # 
        shapelevel_lower <- c(2,2,
                              0,0,0)
        names(shapelevel_lower) <- lower
        shapelevel_upper <- 2
    }
    # 
    # Plot
    # svg(filename = 'TimeSeries_1.svg', width = 16, height = 9)
    {
        par(cex= 1.5,
            oma = c(2, .5, 0, 4), mar = c(1,1,1,1), 
            mgp = c(.5, .5, 0), xpd = NA)
        # Lower
        plot(x = df$Date, y = df$DO_in, cex = 1.2,
             xlim = c(0, 70), ylim = c(0, 50),
             xaxt = 'n', yaxt = 'n', type = 'n',
             main = '', xlab = '', ylab = '', cex.axis = .7)
        axis(side = 2, at = seq(0, 40, 5), labels = seq(0, 40, 5),
             col = 'black', col.axis = 'black', lwd = 2.5, cex.axis = .6)
        # Rectangle
        for (i in 1:nrow(rectangle)) {
            rect(xleft = rectangle[i, 'V1'], xright = rectangle[i, 'V2'],
                 ybottom = -2,  ytop = 52,
                 col = rectangle$colour[i], density = NA)
        }
        mtext(text = rectangle$Condition, side = 1, line = 0,
              
              adj = c(.09, .24, .44, .65, .84),
              col = 'black', cex = 1.6)
        # Lower Lines
        for (vrbl in lower) {
            lines(x = df$Date, y = df[,vrbl], 
                  type = 'o',
                  pch = shapelevel_lower[lower==vrbl],
                  col = colourlevel_lower[lower==vrbl],
                  cex = .6)
        }
        # Upper
        par(new = T)
        plot(x = df$Date, y = df$DO_in, cex = 1.2,
             xlim = c(0, 70), ylim = c(-2000, 500),
             xaxt = 'n', yaxt = 'n', type = 'n',
             main = '', xlab = '', ylab = '', cex.axis = .7)
        axis(side = 4, at = seq(0, 500, 100), labels = seq(0, 500, 100),
             col = colourlevel_upper, col.axis = colourlevel_upper, 
             lwd = 2.5, cex.axis = .6)
        lines(x = df$Date, y = df[,upper], type = 'o',
              col = colourlevel_upper,
              pch = shapelevel_upper,
              cex = .6)
        # 
        title(xlab = 'Day', line = 1.5)
        axis(side = 1, at = c(1,10,23,39,53,67), labels = c(1,10,23,39,53,67),
             col = 'black', col.axis = 'black', lwd = 2.5, cex.axis = .7)
        abline(v = c(1,10,23,39,53,67), 
               h = c(seq(-2000, 0, length.out = 17), seq(0, 500, 100)),
               col = 'gray', lty = 2, lwd = 1, xpd = F)
        # Rectangle legend
        legend('bottomright',
               inset = c(-.075,0),
               legend = rectangle$Condition,
               col = rectangle$colour,
               pch = 15,
               bty = 'n', cex = 1
        )
        # Points legend
        legend('right', 
               inset = c(-.1, 0),
               legend = c(upper, lower),
               col = c(colourlevel_upper, colourlevel_lower), 
               pch = c(shapelevel_upper, shapelevel_lower),
               bty = 'n', cex = .8)
        # 
    }
    # dev.off()
    rm(rectangle, i, vrbl,
       lower, upper,
       colourlevel_lower, colourlevel_upper,
       shapelevel_lower, shapelevel_upper)
    
}
# abbreviate('variable')


# TS, three column with efficiency ----------------------------------------
if(1){
    library(tidyverse)
    library(reshape2)
    # wo condition 6
    df <- MBR_Yasmeen[which(MBR_Yasmeen$Condition!=6),]
    df$Condition <- factor(df$Condition)
    df$TN_in <- df$NH4N_in + df$NO3N_in
    df$TN_out <- df$NH4N_out + df$NO3N_out
    df$TP_in <- df$PO4P_in
    df$TP_out <- df$PO4P_out
    # Sort Columns
    df <- df[,c(
        "Date", "Condition", 
        "DO_in", "TN_in", "TP_in", "COD_in",
        "DO_out", "TN_out", "TP_out", "COD_out"
    )]
    rectangle <- data.frame()
    for (i in levels(df$Condition)) {
        rectangle[as.numeric(i),1] <- min(df[df$Condition==i,]$Date)
        rectangle[as.numeric(i),2] <- max(df[df$Condition==i,]$Date)
    }
    rectangle$Condition <- c('Ref', 'A1', 'B1', 'B2', 'A2')
    rectangle$position <- (rectangle$V1+rectangle$V2)/2
    rectangle$colour <- alpha(RColorBrewer::brewer.pal(
        n = 5, name = 'Dark2'), alpha = .3)
    
    
    
    ###
    # svg(filename = 'TimeSeries_2.svg', width = 10/1.6, height = 15/1.6)
    {
        par(cex= 1,
            oma = c(1, 1, .5, 1), mar = c(2.5,2,4,2),
            mgp = c(.5, .5, 0), xpd = NA)
        # mfrow = c(3,1))
        shape_in <- 2
        shape_out <- 0
        shape_efficiency <- 1
        
        # Top, COD
        {
            # Rectangle
            y <- df$COD_in
            plot(x = df$Date, y = y, cex = 1,
                 xlim = range(df$Date), ylim = c(0, 1000),
                 xaxt = 'n', yaxt = 'n', type = 'n',
                 main = '', xlab = '', ylab = '', cex.axis = 1)
            axis(side = 1, at = c(1,10,23,39,53,67), labels = c(1,10,23,39,53,67),
                 col = 'black', col.axis = 'black', lwd = 2.5, cex.axis = 1)
            for (i in 1:nrow(rectangle)) {
                rect(xleft = rectangle[i, 'V1'], xright = rectangle[i, 'V2'],
                     ybottom = -40,  ytop = 1040,
                     col = rectangle$colour[i], density = NA)
            }
            
            # Lines
            axis(side = 2, at = seq(0, 500, 50), labels = seq(0, 500, 50),
                 col = 'black', col.axis = 'black', lwd = 2.5, cex.axis = .8)
            lines(x = df$Date, y = y, type = 'o', cex = 1,
                  pch = shape_in, col = 1)
            y <- df$COD_out
            lines(x = df$Date, y = y, type = 'o', cex = 1,
                  pch = shape_out, col = 1)
            # Removal Efficiency
            par(new = T)
            plot(x = df$Date, y = y, cex = 1,
                 xlim = range(df$Date), ylim = c(-100, 100),
                 xaxt = 'n', yaxt = 'n', type = 'n',
                 main = '', xlab = '', ylab = '', cex.axis = 1)
            axis(side = 4, at = seq(0, 100, 10), labels = seq(0, 100, 10),
                 col = 'black', col.axis = 'black', 
                 lwd = 2.5, cex.axis = .8)
            y <- (1-(df$COD_out/df$COD_in))*100
            lines(x = df$Date, y = y, type = 'o', cex = 1,
                  pch = shape_efficiency, col = 1)
            # 
            abline(v = c(1,10,23,39,53,67), 
                   h = c(seq(-100, 0, 20), seq(0, 100, 20)),
                   col = 'gray', lty = 2, lwd = 1, xpd = F)
            mtext(text = rectangle$Condition, side = 3, line = .5,
                  adj = c(.08, .24, .45, .67, .88),
                  col = 'black', cex = .9)
            
            title('(A)')
            mtext(text = 'Time (day)', side = 1, line = 1.5, adj = .5, cex = .9)
            mtext(text = 'COD removal (%)', 
                  side = 4, line = 1.5, adj = .82, col = 'black', cex = .8)
            mtext(text = 'COD (mg/L)', 
                  side = 2, line = 1.5, adj = .25, col = 'black', cex = .8)
        }
        
        # Legend add in the first
        # Points legend
        legend(x = 52.3, y = 50,
               # 'right',
               # inset = c(.03),
               
               legend = c('Influent', 'Effluent', 'Removal efficiency'),
               # col = c(colourlevel_upper, colourlevel_lower),
               pch = c(shape_in, shape_out, shape_efficiency),
               bty = 'n', cex = 1)
        
        
        # Middle, TN
        {
            # Rectangle
            y <- df$TN_in
            plot(x = df$Date, y = y, cex = 1,
                 xlim = range(df$Date), ylim = c(0, 100),
                 xaxt = 'n', yaxt = 'n', type = 'n',
                 main = '', xlab = '', ylab = '', cex.axis = 1)
            axis(side = 1, at = c(1,10,23,39,53,67), labels = c(1,10,23,39,53,67),
                 col = 'black', col.axis = 'black', lwd = 2.5, cex.axis = 1)
            for (i in 1:nrow(rectangle)) {
                rect(xleft = rectangle[i, 'V1'], xright = rectangle[i, 'V2'],
                     ybottom = -4,  ytop = 104,
                     col = rectangle$colour[i], density = NA)
            }
            
            # Lines
            axis(side = 2, at = seq(0, 50, 5), labels = seq(0, 50, 5),
                 col = 'black', col.axis = 'black', lwd = 2.5, cex.axis = .8)
            lines(x = df$Date, y = y, type = 'o', cex = 1,
                  pch = shape_in, col = 1)
            
            y <- df$TN_out
            lines(x = df$Date, y = y, type = 'o', cex = 1,
                  pch = shape_out, col = 1)
            # Removal Efficiency
            par(new = T)
            plot(x = df$Date, y = y, cex = 1,
                 xlim = range(df$Date), ylim = c(-100, 100),
                 xaxt = 'n', yaxt = 'n', type = 'n',
                 main = '', xlab = '', ylab = '', cex.axis = 1)
            axis(side = 4, at = seq(0, 100, 10), labels = seq(0, 100, 10),
                 col = 'black', col.axis = 'black', 
                 lwd = 2.5, cex.axis = .8)
            
            y <- (1-(df$TN_out/df$TN_in))*100
            lines(x = df$Date, y = y, type = 'o', cex = 1,
                  pch = shape_efficiency, col = 1)
            # 
            abline(v = c(1,10,23,39,53,67), 
                   h = c(seq(-100, 0, 20), seq(0, 100, 20)),
                   col = 'gray', lty = 2, lwd = 1, xpd = F)
            mtext(text = rectangle$Condition, side = 1, line = .5,
                  adj = c(.08, .24, .45, .67, .88),
                  col = 'black', cex = .9)
            
            title('(B)')
            mtext(text = 'Time (day)', side = 1, line = 1.5, adj = .5, cex = .9)
            mtext(text = 'TN removal (%)', 
                  side = 4, line = 1.5, adj = .8, col = 'black', cex = .8)
            mtext(text = 'TN (mg/L)', 
                  side = 2, line = 1.5, adj = .25, col = 'black', cex = .8)
        }
        
        
        # Bottom, TP
        {
            # Rectangle
            y <- df$TP_in
            plot(x = df$Date, y = y, cex = 1,
                 xlim = range(df$Date), ylim = c(0, 20),
                 xaxt = 'n', yaxt = 'n', type = 'n',
                 main = '', xlab = '', ylab = '', cex.axis = 1)
            axis(side = 1, at = c(1,10,23,39,53,67), labels = c(1,10,23,39,53,67),
                 col = 'black', col.axis = 'black', lwd = 2.5, cex.axis = 1)
            for (i in 1:nrow(rectangle)) {
                rect(xleft = rectangle[i, 'V1'], xright = rectangle[i, 'V2'],
                     ybottom = 0,  ytop = 20.6,
                     col = rectangle$colour[i], density = NA)
            }
            
            # Lines
            axis(side = 2, at = seq(0, 10, 1), labels = seq(0, 10, 1),
                 col = 'black', col.axis = 'black', lwd = 2.5, cex.axis = .8)
            lines(x = df$Date, y = y, type = 'o', cex = 1,
                  pch = shape_in, col = 1)
            
            y <- df$TP_out
            lines(x = df$Date, y = y, type = 'o', cex = 1,
                  pch = shape_out, col = 1)
            # Removal Efficiency
            par(new = T)
            plot(x = df$Date, y = y, cex = 1,
                 xlim = range(df$Date), ylim = c(-300, 100),
                 xaxt = 'n', yaxt = 'n', type = 'n',
                 main = '', xlab = '', ylab = '', cex.axis = 1)
            axis(side = 4, at = seq(-100, 100, 20), labels = seq(-100, 100, 20),
                 col = 'black', col.axis = 'black', 
                 lwd = 2.5, cex.axis = .8)
            
            y <- (1-(df$TP_out/df$TP_in))*100
            lines(x = df$Date, y = y, type = 'o', cex = 1,
                  pch = shape_efficiency, col = 1)
            # 
            abline(v = c(1,10,23,39,53,67), 
                   h = c(seq(-300, -100, 40), seq(-100, 100, 40)),
                   col = 'gray', lty = 2, lwd = 1, xpd = F)
            mtext(text = rectangle$Condition, side = 1, line = .5,
                  adj = c(.08, .24, .45, .67, .88),
                  col = 'black', cex = .9)
            
            title('(C)')
            mtext(text = 'Time (day)', side = 1, line = 1.5, adj = .5, cex = .9)
            mtext(text = 'TP removal (%)', 
                  side = 4, line = 1.5, adj = .8, col = 'black', cex = .8)
            mtext(text = 'TP (mg/L)', 
                  side = 2, line = 1.5, adj = .25, col = 'black', cex = .8)
        }
        
        rm(y, shape_in, shape_out, shape_efficiency)
    }
    # dev.off()
}
