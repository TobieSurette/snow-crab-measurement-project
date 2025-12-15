library(gulf.data)
library(gulf.graphics)

x <- read.csv("studies/May 2024 - Area 12 Fishery/data/Completed Merus Sampling 2024.csv")
names(x) <- gsub("[.]+", ".", names(x))
x$shell.condition <- as.numeric(substr(x$shell.condition, 1, 1))
x <- x[!(is.na(x$hemolymph) & is.na(x$carapace.width)), ]

# Hemolymph plot:
png(file = "studies/May 2024 - Area 12 Fishery/figures/hemolymph.png", res = 500, units = "in", height = 7, width = 7)
plot(x$crab.number, x$hemolymph, xlim = c(0, 60), ylim = c(0, 7), xaxs = "i", yaxs = "i", type = "n", xlab = "", ylab = "")
grid()
ix <- which(x$shell.condition == 1)
points(x$crab.number[ix], x$hemolymph[ix], pch = 21, bg = fade("green2"), lwd = 0.5, col = "grey50", cex = 1.5)
ix <- which(x$shell.condition == 2)
points(x$crab.number[ix], x$hemolymph[ix], pch = 22, bg = fade("orange"), lwd = 0.5, col = "grey50", cex = 1.5)
ix <- which(x$shell.condition == 3)
points(x$crab.number[ix], x$hemolymph[ix], pch = 23, bg = fade("red"), lwd = 0.5, col = "grey50", cex = 1.5)
ix <- which(x$shell.condition == 4)
points(x$crab.number[ix], x$hemolymph[ix], pch = 24, bg = fade("blue"), lwd = 0.5, col = "grey50", cex = 1.5)
text(x$crab.number, x$hemolymph, x$shell.condition, pos = 1)
mtext("Crab number", 1, 2.5, cex = 1.25, font = 2)
mtext("Hemolymph", 2, 2.5, cex = 1.25, font = 2)

legend("bottomright", 
       legend = paste0("Shell condition ", 1:4), pt.cex = 1.5,
       pt.lwd = 0.5, col = "grey50", 
       pch = 21:24, pt.bg = fade(c("green2", "orange", "red", "blue")),
       box.col = "grey50", box.lwd = 0.5)
box(col = "grey50")
dev.off()

# Chela height morphometry:
png(file = "studies/May 2024 - Area 12 Fishery/figures/chela height.png", res = 500, units = "in", height = 7, width = 7)
plot(x$carapace.width, x$chela.height, xlim = c(90, 130), xaxs = "i", ylim = c(10, 40), yaxs = "i", type = "n", xlab = "", ylab = "")
grid()
# Maturity discrimant function:
# (-0.7889259*log(x$carapace.width[ix]) + 0.6144883*log(x$chela.height[ix]) + 1.7605142)
xx <- seq(80, 130, len = 1000)
lines(xx, exp(1.283874571 * log(xx) - 2.86500817), lty = "dashed", col = fade("red"), lwd = 2)
mtext("Carapace width (mm)", 1, 2.5, cex = 1.25, font = 2)
mtext("Chela height (mm)", 2, 2.5, cex = 1.25, font = 2)
b <- read.scsbio(2024)
points(b$carapace.width, b$chela.height, cex = 0.5, col = fade("grey"))
points(x$carapace.width, x$chela.height, cex = 1.25, pch = 21, bg = fade("red"), col = "grey50", lwd = 0.5)
box(col = "grey50")
x$maturity <- "mature"
x$maturity[x$chela.height < exp(1.283874571 * log(x$carapace.width) - 2.86500817)] <- "immature"
dev.off()

# Check merus morphometry:
plot(x$wet.merus.length.mm., x$wet.merus.height.mm.)
ix <- which(x$maturity == "immature")
points(x$wet.merus.length.mm.[ix], x$wet.merus.height.mm.[ix], pch = 21, bg = "green2", cex = 1.5)

plot(x$carapace.width, x$wet.merus.length.mm.)
ix <- which(x$maturity == "immature")
points(x$carapace.width[ix], x$wet.merus.length.mm.[ix], pch = 21, bg = "green2", cex = 1.5)

plot(x$carapace.width, x$wet.merus.height.mm.)
ix <- which(x$maturity == "immature")
points(x$carapace.width[ix], x$wet.merus.height.mm.[ix], pch = 21, bg = "green2", cex = 1.5)

# Chela color:
png(file = "studies/May 2024 - Area 12 Fishery/figures/chela color b vs a.png", res = 500, units = "in", height = 7, width = 7)
vars <- c("colorimeter.chela.a", "colorimeter.chela.b")
plot(x[, vars[1]], x[, vars[2]], xlab = "", ylab = "", type = "n")
grid()
cols <- fade(c("green2", "orange", "red", "blue"))
for (i in 1:4){
   ix <- which(x$shell.condition == i)
   points(x[ix, vars[1]], x[ix, vars[2]],  pch = 20+i, bg = cols[i], lwd = 0.5, col = "grey50", cex = 1.5)
}
mtext("Chela colorimeter (a)", 1, 2.5, cex = 1.25, font = 2)
mtext("Chela colorimeter (b)", 2, 2.5, cex = 1.25, font = 2)
legend("bottomright", 
       legend = paste0("Shell condition ", 1:4), pt.cex = 1.5,
       pt.lwd = 0.5, col = "grey50", 
       pch = 21:24, pt.bg = fade(c("green2", "orange", "red", "blue")),
       box.col = "grey50", box.lwd = 0.5)
box(col = "grey50")
dev.off()

# Chela color:
png(file = "studies/May 2024 - Area 12 Fishery/figures/chela color L vs b.png", res = 500, units = "in", height = 7, width = 7)
vars <- c("colorimeter.chela.b", "colorimeter.chela.L")
plot(x[, vars[1]], x[, vars[2]], xlab = "", ylab = "", type = "n")
grid()
cols <- fade(c("green2", "orange", "red", "blue"))
for (i in 1:4){
   ix <- which(x$shell.condition == i)
   points(x[ix, vars[1]], x[ix, vars[2]],  pch = 20+i, bg = cols[i], lwd = 0.5, col = "grey50", cex = 1.5)
}
mtext("Chela colorimeter (b)", 1, 2.5, cex = 1.25, font = 2)
mtext("Chela colorimeter (L)", 2, 2.5, cex = 1.25, font = 2)
legend("bottomright", 
       legend = paste0("Shell condition ", 1:4), pt.cex = 1.5,
       pt.lwd = 0.5, col = "grey50", 
       pch = 21:24, pt.bg = fade(c("green2", "orange", "red", "blue")),
       box.col = "grey50", box.lwd = 0.5)
box(col = "grey50")
dev.off()

# Merus color:
png(file = "studies/May 2024 - Area 12 Fishery/figures/merus color b vs a.png", res = 500, units = "in", height = 7, width = 7)
vars <- c("colorimeter.merus.a", "colorimeter.merus.b")
plot(x[, vars[1]], x[, vars[2]], xlab = "", ylab = "", type = "n")
grid()
cols <- fade(c("green2", "orange", "red", "blue"))
for (i in 1:4){
   ix <- which(x$shell.condition == i)
   points(x[ix, vars[1]], x[ix, vars[2]],  pch = 20+i, bg = cols[i], lwd = 0.5, col = "grey50", cex = 1.5)
}
mtext("Merus colorimeter (a)", 1, 2.5, cex = 1.25, font = 2)
mtext("Merus colorimeter (b)", 2, 2.5, cex = 1.25, font = 2)
legend("bottomright", 
       legend = paste0("Shell condition ", 1:4), pt.cex = 1.5,
       pt.lwd = 0.5, col = "grey50", 
       pch = 21:24, pt.bg = fade(c("green2", "orange", "red", "blue")),
       box.col = "grey50", box.lwd = 0.5)
box(col = "grey50")
dev.off()

# Chela color:
png(file = "studies/May 2024 - Area 12 Fishery/figures/merus color L vs b.png", res = 500, units = "in", height = 7, width = 7)
vars <- c("colorimeter.merus.b", "colorimeter.merus.L")
plot(x[, vars[1]], x[, vars[2]], xlab = "", ylab = "", type = "n")
grid()
cols <- fade(c("green2", "orange", "red", "blue"))
for (i in 1:4){
   ix <- which(x$shell.condition == i)
   points(x[ix, vars[1]], x[ix, vars[2]],  pch = 20+i, bg = cols[i], lwd = 0.5, col = "grey50", cex = 1.5)
}
mtext("Merus colorimeter (b)", 1, 2.5, cex = 1.25, font = 2)
mtext("Merus colorimeter (L)", 2, 2.5, cex = 1.25, font = 2)
legend("bottomright", 
       legend = paste0("Shell condition ", 1:4), pt.cex = 1.5,
       pt.lwd = 0.5, col = "grey50", 
       pch = 21:24, pt.bg = fade(c("green2", "orange", "red", "blue")),
       box.col = "grey50", box.lwd = 0.5)
box(col = "grey50")
dev.off()

# Dry muscle weight versus carapace width:
png(file = "studies/May 2024 - Area 12 Fishery/figures/dry muscle weight.png", res = 500, units = "in", height = 7, width = 7)
vars <- c("carapace.width", "dry.muscle.weight.g.")
plot(x[, vars[1]], x[, vars[2]], xlab = "", ylab = "", type = "n")
grid()
cols <- fade(c("green2", "orange", "red", "blue"))
for (i in 1:4){
   ix <- which(x$shell.condition == i)
   points(x[ix, vars[1]], x[ix, vars[2]],  pch = 20+i, bg = cols[i], lwd = 0.5, col = "grey50", cex = 1.5)
}
mtext("Carapace width (mm)", 1, 2.5, cex = 1.25, font = 2)
mtext("Dry muscle weight (g)", 2, 2.5, cex = 1.25, font = 2)
legend("bottomright", 
       legend = paste0("Shell condition ", 1:4), pt.cex = 1.5,
       pt.lwd = 0.5, col = "grey50", 
       pch = 21:24, pt.bg = fade(c("green2", "orange", "red", "blue")),
       box.col = "grey50", box.lwd = 0.5)
box(col = "grey50")
dev.off()

# Dry muscle weight versus merus length
png(file = "studies/May 2024 - Area 12 Fishery/figures/dry muscle weight vs merus length.png", res = 500, units = "in", height = 7, width = 7)
vars <- c("wet.merus.length.mm.", "dry.muscle.weight.g.")
plot(x[, vars[1]], x[, vars[2]], xlab = "", ylab = "", type = "n")
grid()
cols <- fade(c("green2", "orange", "red", "blue"))
for (i in 1:4){
   ix <- which(x$shell.condition == i)
   points(x[ix, vars[1]], x[ix, vars[2]],  pch = 20+i, bg = cols[i], lwd = 0.5, col = "grey50", cex = 1.5)
}
mtext("Merus length (mm)", 1, 2.5, cex = 1.25, font = 2)
mtext("Dry muscle weight (g)", 2, 2.5, cex = 1.25, font = 2)
legend("topleft", 
       legend = paste0("Shell condition ", 1:4), pt.cex = 1.5,
       pt.lwd = 0.5, col = "grey50", 
       pch = 21:24, pt.bg = fade(c("green2", "orange", "red", "blue")),
       box.col = "grey50", box.lwd = 0.5)
box(col = "grey50")
dev.off()

# Wet muscle weight:
png(file = "studies/May 2024 - Area 12 Fishery/figures/dry-wet muscle ratio.png", res = 500, units = "in", height = 7, width = 7)
vars <- c("carapace.width", "dry.muscle.weight.g.", "muscle.weight.wet.")
plot(x[, vars[1]], x[, vars[2]] / x[, vars[3]], xlab = "", ylab = "", type = "n")
grid()
cols <- fade(c("green2", "orange", "red", "blue"))
for (i in 1:4){
   ix <- which(x$shell.condition == i)
   points(x[ix, vars[1]], x[ix, vars[2]] / x[ix, vars[3]],  pch = 20+i, bg = cols[i], lwd = 0.5, col = "grey50", cex = 1.5)
}
mtext("Carapace width (mm)", 1, 2.5, cex = 1.25, font = 2)
mtext("Dry / Wet muscle weight ratio", 2, 2.5, cex = 1.25, font = 2)
legend("bottomleft", 
       legend = paste0("Shell condition ", 1:4), pt.cex = 1.5,
       pt.lwd = 0.5, col = "grey50", 
       pch = 21:24, pt.bg = fade(c("green2", "orange", "red", "blue")),
       box.col = "grey50", box.lwd = 0.5)
box(col = "grey50")
dev.off()

# Shell weight:
png(file = "studies/May 2024 - Area 12 Fishery/figures/shell weight.png", res = 500, units = "in", height = 7, width = 7)
vars <- c("wet.merus.length.mm.", "dry.empty.shell.weight.g.")
plot(x[, vars[1]], x[, vars[2]], xlab = "", ylab = "", type = "n")
grid()
cols <- fade(c("green2", "orange", "red", "blue"))
for (i in 1:4){
   ix <- which(x$shell.condition == i)
   points(x[ix, vars[1]], x[ix, vars[2]],  pch = 20+i, bg = cols[i], lwd = 0.5, col = "grey50", cex = 1.5)
}
mtext("Carapace width (mm)", 1, 2.5, cex = 1.25, font = 2)
mtext("Dry / Wet muscle weight ratio", 2, 2.5, cex = 1.25, font = 2)
legend("bottomleft", 
       legend = paste0("Shell condition ", 1:4), pt.cex = 1.5,
       pt.lwd = 0.5, col = "grey50", 
       pch = 21:24, pt.bg = fade(c("green2", "orange", "red", "blue")),
       box.col = "grey50", box.lwd = 0.5)
box(col = "grey50")
dev.off()

# Shell weight:
#png(file = "studies/May 2024 - Area 12 Fishery/figures/shell weight.png", res = 500, units = "in", height = 7, width = 7)
vars <- c("wet.merus.length.mm.", "dry.empty.shell.weight.g.")
plot(x$colorimeter.merus.b, x[, vars[2]] / x[, vars[1]], xlab = "", ylab = "", type = "n")
grid()
cols <- fade(c("green2", "orange", "red", "blue"))
for (i in 1:4){
   ix <- which(x$shell.condition == i)
   points(x$colorimeter.merus.b[ix], x[ix, vars[2]] / x[ix, vars[1]],  pch = 20+i, bg = cols[i], lwd = 0.5, col = "grey50", cex = 1.5)
}
mtext("Colorimeter merus (b)", 1, 2.5, cex = 1.25, font = 2)
mtext("Dry / Wet muscle weight ratio", 2, 2.5, cex = 1.25, font = 2)
legend("bottomright", 
       legend = paste0("Shell condition ", 1:4), pt.cex = 1.5,
       pt.lwd = 0.5, col = "grey50", 
       pch = 21:24, pt.bg = fade(c("green2", "orange", "red", "blue")),
       box.col = "grey50", box.lwd = 0.5)
box(col = "grey50")
#dev.off()

png(file = "studies/May 2024 - Area 12 Fishery/figures/double ratio.png", res = 500, units = "in", height = 7, width = 7)
vars <- c("dry.muscle.weight.g.", "muscle.weight.wet.")
plot(x$dry.empty.shell.weight.g./ x$wet.merus.length.mm., x[, vars[2]] / x[, vars[1]], xlab = "", ylab = "", type = "n")
grid()
cols <- fade(c("green2", "orange", "red", "blue"))
for (i in 1:4){
   ix <- which(x$shell.condition == i)
   points(x$dry.empty.shell.weight.g.[ix] / x$wet.merus.length.mm.[ix], 
          x[ix, vars[2]] / x[ix, vars[1]],  
          pch = 20+i, bg = cols[i], lwd = 0.5, col = "grey50", cex = 1.5)
}
mtext("Merus weight / length (g/mm)", 1, 2.5, cex = 1.25, font = 2)
mtext("Wet / Dry muscle weight ratio", 2, 2.5, cex = 1.25, font = 2)
legend("topright", 
       legend = paste0("Shell condition ", 1:4), pt.cex = 1.5,
       pt.lwd = 0.5, col = "grey50", 
       pch = 21:24, pt.bg = fade(c("green2", "orange", "red", "blue")),
       box.col = "grey50", box.lwd = 0.5)
box(col = "grey50")
dev.off()

# Shell weight:
#png(file = "studies/May 2024 - Area 12 Fishery/figures/shell weight.png", res = 500, units = "in", height = 7, width = 7)
vars <- c("wet.merus.length.mm.", "dry.empty.shell.weight.g.")
plot(x$carapace.width, (x[, vars[2]]^0.5) / x[, vars[1]], xlab = "", ylab = "", type = "n")
grid()
cols <- fade(c("green2", "orange", "red", "blue"))
for (i in 1:4){
   ix <- which(x$shell.condition == i)
   points(x$carapace.width[ix], (x[ix, vars[2]]^0.5) / x[ix, vars[1]],  pch = 20+i, bg = cols[i], lwd = 0.5, col = "grey50", cex = 1.5)
}
mtext("Carapace width (mm)", 1, 2.5, cex = 1.25, font = 2)
mtext("Merus length^0.5 / shell weight", 2, 2.5, cex = 1.25, font = 2)
legend("topleft", 
       legend = paste0("Shell condition ", 1:4), pt.cex = 1.5,
       pt.lwd = 0.5, col = "grey50", 
       pch = 21:24, pt.bg = fade(c("green2", "orange", "red", "blue")),
       box.col = "grey50", box.lwd = 0.5, bg = NA)
box(col = "grey50")
#dev.off()

vars <- c("wet.merus.length.mm.", "dry.empty.shell.weight.g.")
plot(x$muscle.weight.wet. / x$dry.muscle.weight.g., (x[, vars[2]]^0.5) / x[, vars[1]], xlab = "", ylab = "", type = "n")
grid()
cols <- fade(c("green2", "orange", "red", "blue"))
for (i in 1:4){
   ix <- which(x$shell.condition == i)
   points(x$muscle.weight.wet.[ix] / x$dry.muscle.weight.g.[ix], 
          (x[ix, vars[2]]^0.5) / x[ix, vars[1]],  pch = 20+i, bg = cols[i], lwd = 0.5, col = "grey50", cex = 1.5)
}
mtext("Muscle wet / dry ratio", 1, 2.5, cex = 1.25, font = 2)
mtext("Merus length^0.5 / shell weight", 2, 2.5, cex = 1.25, font = 2)
legend("topright", 
       legend = paste0("Shell condition ", 1:4), pt.cex = 1.5,
       pt.lwd = 0.5, col = "grey50", cex = 1.25,
       pch = 21:24, pt.bg = fade(c("green2", "orange", "red", "blue")),
       box.col = "grey50", box.lwd = 0.5, bg = NA)
box(col = "grey50")
