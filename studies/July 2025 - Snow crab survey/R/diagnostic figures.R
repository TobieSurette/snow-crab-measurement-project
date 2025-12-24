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
   
# Attach biological data:
b <- read.scsbio(2025)
ix <- match(x[c("tow.id", "crab.number")], b[c("tow.id", "crab.number")])
b$maturity <- is.mature(b)
x$tow.number <- b$tow.number[ix]
x$carapace.width <- b$carapace.width[ix]
x$maturity <- b$maturity[ix]
x$shell.condition <- b$shell.condition[ix]

# Add coordinates:
s <- read.scsset(2025, valid = 1)
ix <- match(x[c("tow.id")], s[c("tow.id")])
x$longitude <- lon(s)[ix]
x$latitude  <- lat(s)[ix]

# Attach colorimeter data:
z <- read.csv("C:/Users/SuretteTJ/Desktop/github/gulf.data/inst/extdata/scs.colorimeter.2025.csv")
ix <- match(x[c("tow.id", "crab.number")], z[c("tow.id", "crab.number")])
x$colour.L <- z$colour.L[ix]
x$colour.a <- z$colour.a[ix]
x$colour.b <- z$colour.b[ix]

# Muscle weight versus shell weight:
png(file = "studies/July 2025 - Snow crab survey/figures/double ratio.png", res = 500, units = "in", height = 5.75, width = 7)
xx <- x$shell.weight.dry.g / x$merus.length.mm
yy <- 100 * (x$muscle.weight.wet.g - x$muscle.weight.dry.g) / x$muscle.weight.wet.g
plot(xx, yy, ylim = c(76, 90), yaxs = "i", xlab = "", ylab = "", type = "n")
grid()
cols <- fade(c("green2", "orange", "red", "blue"))
for (i in 2:4){
   ix <- which(x$shell.condition == i)
   points(xx[ix], yy[ix],  
          pch = 20+i, bg = cols[i], lwd = 0.5, col = "grey50", cex = 1.25)
   #text(xx[ix], yy[ix],
        #x$julian[ix], pos = 1, cex = 0.65)
}
mtext("Merus shell weight / length (g/mm)", 1, 2.5, cex = 1.25, font = 2)
mtext("Muscle water content (%)", 2, 2.5, cex = 1.25, font = 2)
legend("topright", 
       legend = paste0("Shell condition ", 1:4), pt.cex = 1.5,
       pt.lwd = 0.5, col = "grey50", 
       pch = 21:24, pt.bg = fade(c("green2", "orange", "red", "blue")),
       box.col = "grey50", box.lwd = 0.5, bg = NA)
box(col = "grey50")
dev.off()

# Muscle weight versus colorimeter:
png(file = "studies/July 2025 - Snow crab survey/figures/muscle weight versus colour.png", res = 500, units = "in", height = 5.75, width = 7)
xx <- x$colour.b
yy <- 100 * (x$muscle.weight.wet.g - x$muscle.weight.dry.g) / x$muscle.weight.wet.g
plot(xx, yy, ylim = c(76, 90), yaxs = "i", xlab = "", ylab = "", type = "n")
grid()
cols <- fade(c("green2", "orange", "red", "blue"))
for (i in 2:4){
   ix <- which(x$shell.condition == i)
   points(x$colour.b[ix], yy[ix], 
          pch = 20+i, bg = cols[i], lwd = 0.5, col = "grey50", cex = 0.08 * sqrt(abs(x$julian[ix])))
   #text(x$colour.b[ix], x[ix, vars[2]] / x[ix, vars[1]], 
   #     x$julian[ix], pos = 1, cex = 0.65)
}
mtext("Colorimeter b", 1, 2.5, cex = 1.25, font = 2)
mtext("Muscle water content (%)", 2, 2.5, cex = 1.25, font = 2)
legend("topright", 
       legend = paste0("Shell condition ", 1:4), pt.cex = 1.5,
       pt.lwd = 0.5, col = "grey50", 
       pch = 21:24, pt.bg = fade(c("green2", "orange", "red", "blue")),
       box.col = "grey50", box.lwd = 0.5, bg = NA)
box(col = "grey50")
dev.off()

# Water content versus day:
png(file = paste0("studies/July 2025 - Snow crab survey/figures/water content versus day.png"),
    width = 7, height = 5.75, res = 500, units = "in")

xx <- x$julian
yy <- 100 * (x$muscle.weight.wet.g - x$muscle.weight.dry.g) / x$muscle.weight.wet.g
plot(xx, yy, xlab = "", ylab = "", type = "n")
grid()
cols <- fade(c("green2", "orange", "red", "blue"))
for (i in 2:4){
   ix <- which(x$shell.condition == i)
   points(xx[ix], yy[ix], 
          pch = 20+i, bg = cols[i], lwd = 0.5, col = "grey50", cex = 0.1 * sqrt(abs(x$julian[ix])))
   #text(x$colour.b[ix], x[ix, vars[2]] / x[ix, vars[1]], 
   #     x$julian[ix], pos = 1, cex = 0.65)
}
mtext("Julian day", 1, 2.5, font = 2, cex = 1.25)
mtext("Muscle water content (%)", 2, 2.5, font = 2, cex = 1.25)
box(col = "grey50")
dev.off()

# Spatial patterns:
clg()
shell.condition <- 4
png(file = paste0("studies/July 2025 - Snow crab survey/figures/water content deviations SC", shell.condition, ".png"),
    width = 7, height = 5.75, res = 500, units = "in")
map.new()
map("bathymetry")
points(x$longitude, x$latitude)

ix <- x$shell.condition == shell.condition
cex <- x$muscle.weight.dry.g / x$muscle.weight.wet.g
cex <- (cex - mean(cex[ix], na.rm = TRUE)) / sd(cex[ix], na.rm = TRUE)
points(x$longitude[ix & cex < 0], x$latitude[ix & cex < 0], 
       pch = 21, col = fade("grey50"), bg = fade("red"),
       cex = 2.5*sqrt(-cex[ix & cex < 0]))
points(x$longitude[ix & cex > 0], x$latitude[ix & cex > 0], 
       pch = 21, col = fade("grey50"), bg = fade("blue"),
       cex = 2.5*sqrt(cex[ix & cex > 0]))
map("coast", col = "papayawhip")

legend("bottomleft", 
       legend = paste0(c("More watery SC", "Less watery SC"), shell.condition),
       pch = 21, pt.bg = fade(c("red", "blue")), pt.lwd = 0.5, col = "grey50",
       pt.cex = 2.25,
       bg = fade("white"))
map.axis(1:2)
box(col = "grey50", lwd = 0.5)
dev.off()

