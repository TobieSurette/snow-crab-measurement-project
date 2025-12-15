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
png(file = "studies/July 2025 - Snow crab survey/figures/double ratio.png", res = 500, units = "in", height = 7, width = 7)
vars <- c("muscle.weight.dry.g", "muscle.weight.wet.g")
plot(x$shell.weight.dry.g / x$merus.length.mm, x[, vars[2]] / x[, vars[1]], xlab = "", ylab = "", type = "n")
grid()
cols <- fade(c("green2", "orange", "red", "blue"))
for (i in 2:4){
   ix <- which(x$shell.condition == i)
   points(x$shell.weight.dry.g[ix] / x$merus.length.mm[ix], x[ix, vars[2]] / x[ix, vars[1]],  
          pch = 20+i, bg = cols[i], lwd = 0.5, col = "grey50", cex = 0.4 * sqrt(abs(x$colour.b[ix])))
   text(x$shell.weight.dry.g[ix] / x$merus.length.mm[ix], x[ix, vars[2]] / x[ix, vars[1]],
        x$julian[ix], pos = 1, cex = 0.65)
}
mtext("Merus shell weight / length (g/mm)", 1, 2.5, cex = 1.25, font = 2)
mtext("Wet / Dry muscle weight ratio", 2, 2.5, cex = 1.25, font = 2)
legend("topright", 
       legend = paste0("Shell condition ", 1:4), pt.cex = 1.5,
       pt.lwd = 0.5, col = "grey50", 
       pch = 21:24, pt.bg = fade(c("green2", "orange", "red", "blue")),
       box.col = "grey50", box.lwd = 0.5, bg = NA)
box(col = "grey50")
dev.off()

# Muscle weight versus colorimeter:
png(file = "studies/July 2025 - Snow crab survey/figures/muscle weight versus colour.png", res = 500, units = "in", height = 7, width = 7)
vars <- c("muscle.weight.dry.g", "muscle.weight.wet.g")
plot(x$colour.b, x[, vars[2]] / x[, vars[1]], xlab = "", ylab = "", type = "n")
grid()
cols <- fade(c("green2", "orange", "red", "blue"))
for (i in 2:4){
   ix <- which(x$shell.condition == i)
   points(x$colour.b[ix], x[ix, vars[2]] / x[ix, vars[1]], 
          pch = 20+i, bg = cols[i], lwd = 0.5, col = "grey50", cex = 0.1 * sqrt(abs(x$julian[ix])))
   #text(x$colour.b[ix], x[ix, vars[2]] / x[ix, vars[1]], 
   #     x$julian[ix], pos = 1, cex = 0.65)
}
mtext("Colorimeter b", 1, 2.5, cex = 1.25, font = 2)
mtext("Wet / Dry muscle weight ratio", 2, 2.5, cex = 1.25, font = 2)
legend("topright", 
       legend = paste0("Shell condition ", 1:4), pt.cex = 1.5,
       pt.lwd = 0.5, col = "grey50", 
       pch = 21:24, pt.bg = fade(c("green2", "orange", "red", "blue")),
       box.col = "grey50", box.lwd = 0.5, bg = NA)
box(col = "grey50")
dev.off()


xx <- x$julian
yy <- x[, vars[2]] / x[, vars[1]]
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


# Spatial patterns:
map.new()
points(x$longitude, x$latitude)
ix <- x$shell.condition == 4
cex <- x$muscle.weight.dry.g / x$muscle.weight.wet.g
cex <- (cex - mean(cex, na.rm = TRUE)) / sd(cex, na.rm = TRUE)
points(x$longitude[ix & cex > 0], x$latitude[ix & cex > 0], 
       pch = 21, col = fade("grey50"), bg = fade("blue"),
       cex = 3*sqrt(cex[ix & cex > 0]))
points(x$longitude[ix & cex < 0], x$latitude[ix & cex < 0], 
       pch = 21, col = fade("grey50"), bg = fade("red"),
       cex = 3*sqrt(-cex[ix & cex < 0]))
map("coast")
box(col = "grey50", lwd = 0.5)

