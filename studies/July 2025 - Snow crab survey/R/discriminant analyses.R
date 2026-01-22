library(gulf.data)
library(gulf.graphics)
library(gulf.spatial)

x <- read.csv("studies/July 2025 - Snow crab survey/data/raw/Merus sampling 2025.csv")
names(x) <- tolower(names(x))
names(x) <- gsub("[.]+", ".", names(x))
names(x) <- gsub("[.]$", "", names(x))
names(x) <- gsub("x.day.tow.number", "tow.number", names(x))
names(x) <- gsub("stations", "tow.id", names(x)) 
x <- x[!is.na(x$merus.length.mm), ]
x$comment <- gsub("Barnicales took out", "Barnacles removed", x$comment)
x$julian <- julian(date(x))
x$new <- x$shell.condition %in% 1:2

# Attach biological data:
b <- read.scsbio(2025)
ix <- match(x[c("tow.id", "crab.number")], b[c("tow.id", "crab.number")])
b$maturity <- is.mature(b)
x$tow.number <- b$tow.number[ix]
x$carapace.width <- b$carapace.width[ix]
x$maturity <- b$maturity[ix]
x$shell.condition <- b$shell.condition[ix]

# Attach colorimeter data:
z <- read.csv("C:/Users/SuretteTJ/Desktop/github/gulf.data/inst/extdata/scs.colorimeter.2025.csv")
ix <- match(x[c("tow.id", "crab.number")], z[c("tow.id", "crab.number")])
x$colour.L <- z$colour.L[ix]
x$colour.a <- z$colour.a[ix]
x$colour.b <- z$colour.b[ix]

# Corrections:
x$shell.weight.dry.g[which((x$tow.id == "GP304F") & (x$crab.number == 90))] <- 4.1781
x$shell.condition[which((x$tow.id == "GP213F") & (x$crab.number == 11))] <- 2

x$water.content <- 100*(1-x$muscle.weight.dry.g / x$muscle.weight.wet.g)
plot(x$water.content, type = "n")
ix <- x$shell.condition == 2
points(which(ix), x$water.content[which(ix)], pch = 21, bg = fade("yellow"), col = "grey50", lwd = 0.5)

ix <- x$shell.condition == 3
points(which(ix), x$water.content[which(ix)], pch = 21, bg = fade("red"), col = "grey50", lwd = 0.5)

ix <- x$shell.condition == 4
points(which(ix), x$water.content[which(ix)], pch = 21, bg = fade("blue"), col = "grey50", lwd = 0.5)

ix <- which(x$water.content > 84 & x$shell.condition %in% 3:5)

# Muscle water content analysis:
clg()
png(file = "studies/July 2025 - Snow crab survey/figures/Discriminant analysis - water content.png", res = 500, units = "in", height = 5, width = 7)
model <- mgcv::gam(new ~ water.content, data = x, family = binomial())
beta <- coef(model)

t <- table(round(x$water.content*2)/2, x$shell.condition)
t <- t / repvec(apply(t, 1, sum), ncol = ncol(t))
gbarplot(t, col = fade(c("white", "yellow", "red", "blue")), legend = FALSE, 
         xaxt = "n", yaxt = "n")
xx <- 100*seq(0.7, 0.95, by = 0.001)
pp <- predict(model, newdata = list(water.content = xx))
pp <- 1 / (1 + exp(-pp))
lines(xx, pp, lwd = 2, col = "grey30")
xp <- c((log(0.25 / 0.75) - beta[1]) / beta[2], 
        - beta[1] / beta[2], 
        (log(0.75 / 0.25) - beta[1]) / beta[2])
names(xp) <- c(0.25, 0.5, 0.75)
for (i in 1:length(xp)){
   lines(c(xp[i], xp[i]), c(0, as.numeric(names(xp)[i])), lty = "dashed")
   lines(c(par("usr")[1], xp[i]), rep(as.numeric(names(xp)[i]), 2), lty = "dashed")
   #mtext(as.numeric(names(xp)[i]), 2, 0.25, at = as.numeric(names(xp)[i]), col = "red")
   mtext(round(xp[i], 1), 1, 0.25, at = xp[i], col = "red", cex = 0.75)
}
axis(1, at = 100*seq(0.74, 1, by = 0.02))
axis(2, at = seq(0, 1, by = 0.25))
mtext("Muscle water content (%)", 1, 2.5, font = 2, cex = 1.25)
mtext("Proportion of new-shelled", 2, 2.5, font = 2, cex = 1.25)
legend("bottomright", 
       legend = paste0("Shell condition ", 1:4),
       pch = 22,
       pt.bg = fade(c("white", "yellow", "red", "blue")),
       bg = fade("white", 0.8),
       col = "grey50", pt.lwd = 0.5, pt.cex = 2, box.col = "grey50", box.lwd = 0.5)
box(col = "grey50")
dev.off()

table(x$shell.condition, x$water.content > xp[["0.5"]]) # Confusion matrix.

# Shell weight content analysis:
clg()
png(file = "studies/July 2025 - Snow crab survey/figures/Discriminant analysis - shell index.png", res = 500, units = "in", height = 5, width = 7)
x$shell.index <- 1000000 * x$shell.weight.dry.g / ((x$merus.length.mm * x$merus.height.mm)^(3/2))
model <- mgcv::gam(new ~ shell.index, data = x, family = binomial())
beta <- coef(model)

t <- table(round(x$shell.index*1.25)/1.25, x$shell.condition)
t <- t / repvec(apply(t, 1, sum), ncol = ncol(t))
gbarplot(t, col = fade(c("white", "yellow", "red", "blue")), legend = FALSE, 
         xaxt = "n", yaxt = "n")
xx <- seq(15, 35, by = 0.1)
pp <- predict(model, newdata = list(shell.index = xx))
pp <- 1 / (1 + exp(-pp))
lines(xx, pp, lwd = 2, col = "grey30")
xp <- c((log(0.25 / 0.75) - beta[1]) / beta[2], 
        - beta[1] / beta[2], 
        (log(0.75 / 0.25) - beta[1]) / beta[2])
names(xp) <- c(0.25, 0.5, 0.75)
for (i in 1:length(xp)){
   lines(c(xp[i], xp[i]), c(0, as.numeric(names(xp)[i])), lty = "dashed")
   lines(c(par("usr")[1], xp[i]), rep(as.numeric(names(xp)[i]), 2), lty = "dashed")
   #mtext(as.numeric(names(xp)[i]), 2, 0.25, at = as.numeric(names(xp)[i]), col = "red")
   mtext(round(xp[i], 1), 1, 0.25, at = xp[i], col = "red", cex = 0.75)
}
axis(1, at = seq(15, 35, by = 5))
axis(2, at = seq(0, 1, by = 0.25))
mtext("Shell weight index", 1, 2.5, font = 2, cex = 1.25)
mtext("Proportion of new-shelled", 2, 2.5, font = 2, cex = 1.25)
legend("topright", 
       legend = paste0("Shell condition ", 1:4),
       pch = 22,
       pt.bg = fade(c("white", "yellow", "red", "blue")),
       bg = fade("white", 0.8),
       col = "grey50", pt.lwd = 0.5, pt.cex = 2, box.col = "grey50", box.lwd = 0.5)
box(col = "grey50")
dev.off()

table(x$shell.condition, x$shell.index > xp[["0.5"]]) # Confusion matrix.


# Colorimeter b analysis:
clg()
png(file = "studies/July 2025 - Snow crab survey/figures/Discriminant analysis - colorimeter b.png", res = 500, units = "in", height = 5, width = 7)
model <- mgcv::gam(new ~ colour.b, data = x, family = binomial())
beta <- coef(model)

t <- table(round(x$colour.b*1.25)/1.25, x$shell.condition)
t <- t / repvec(apply(t, 1, sum), ncol = ncol(t))
gbarplot(t, col = fade(c("white", "yellow", "red", "blue")), legend = FALSE, 
         xaxt = "n", yaxt = "n")
xx <- seq(-2, 35, by = 0.1)
pp <- predict(model, newdata = list(colour.b = xx))
pp <- 1 / (1 + exp(-pp))
lines(xx, pp, lwd = 2, col = "grey30")
xp <- c(- beta[1] / beta[2])
names(xp) <- c(0.5)
for (i in 1:length(xp)){
   lines(c(xp[i], xp[i]), c(0, as.numeric(names(xp)[i])), lty = "dashed")
   lines(c(par("usr")[1], xp[i]), rep(as.numeric(names(xp)[i]), 2), lty = "dashed")
   #mtext(as.numeric(names(xp)[i]), 2, 0.25, at = as.numeric(names(xp)[i]), col = "red")
   mtext(round(xp[i], 2), 1, 0.25, at = xp[i], col = "red", cex = 0.75)
}
axis(1, at = seq(-5, 35, by = 5))
axis(2, at = seq(0, 1, by = 0.25))
mtext("Colorimeter (b)", 1, 2.5, font = 2, cex = 1.25)
mtext("Proportion of new-shelled", 2, 2.5, font = 2, cex = 1.25)
legend("topright", 
       legend = paste0("Shell condition ", 1:4),
       pch = 22,
       pt.bg = fade(c("white", "yellow", "red", "blue")),
       bg = fade("white", 0.8),
       col = "grey50", pt.lwd = 0.5, pt.cex = 2, box.col = "grey50", box.lwd = 0.5)

box(col = "grey50")
dev.off()

table(x$shell.condition, x$colour.b > xp[["0.5"]]) # Confusion matrix.

# Colorimeter L analysis:
clg()
png(file = "studies/July 2025 - Snow crab survey/figures/Discriminant analysis - colorimeter L.png", res = 500, units = "in", height = 5, width = 7)
model <- mgcv::gam(new ~ colour.L, data = x, family = binomial())
beta <- coef(model)

t <- table(round(x$colour.L*1.25)/1.25, x$shell.condition)
t <- t / repvec(apply(t, 1, sum), ncol = ncol(t))
gbarplot(t, col = fade(c("white", "yellow", "red", "blue")), legend = FALSE, 
         xaxt = "n", yaxt = "n")
xx <- seq(-2, 70, by = 0.1)
pp <- predict(model, newdata = list(colour.L = xx))
pp <- 1 / (1 + exp(-pp))
lines(xx, pp, lwd = 2, col = "grey30")
xp <- c(- beta[1] / beta[2])
names(xp) <- c(0.5)
for (i in 1:length(xp)){
   lines(c(xp[i], xp[i]), c(0, as.numeric(names(xp)[i])), lty = "dashed")
   lines(c(par("usr")[1], xp[i]), rep(as.numeric(names(xp)[i]), 2), lty = "dashed")
   #mtext(as.numeric(names(xp)[i]), 2, 0.25, at = as.numeric(names(xp)[i]), col = "red")
   mtext(round(xp[i], 2), 1, 0.25, at = xp[i], col = "red", cex = 0.75)
}
axis(1, at = seq(-5, 75, by = 5))
axis(2, at = seq(0, 1, by = 0.25))
mtext("Colorimeter (L)", 1, 2.5, font = 2, cex = 1.25)
mtext("Proportion of new-shelled", 2, 2.5, font = 2, cex = 1.25)
legend("topleft", 
       legend = paste0("Shell condition ", 1:4),
       pch = 22,
       pt.bg = fade(c("white", "yellow", "red", "blue")),
       bg = fade("white", 0.8),
       col = "grey50", pt.lwd = 0.5, pt.cex = 2, box.col = "grey50", box.lwd = 0.5)

box(col = "grey50")
dev.off()

table(x$shell.condition, x$colour.b > xp[["0.5"]]) # Confusion matrix.
