library(gulf.data)

# Organize colorimeter data:
file <- data.frame(name = dir(path = "W:/Crab/Offshore Crab Common/Fishing Year 2024/Trawl Survey/colorimeter",
                              full.names = TRUE, include.dirs = FALSE))

file$full   <- file$name
file$name   <- unlist(lapply(strsplit(file$name, "/"), function(x) x[length(x)]))
file$tow.id <- toupper(unlist(lapply(strsplit(file$name, "[.]"), function(x) x[1])))

r <- NULL
for (i in 1:nrow(file)){
   print(i)
   tmp <- read.csv(file$full[i], header = FALSE)[, 1:6]
   tmp$file <- file$name[i]
   r <- rbind(r, tmp)
}
colnames(r) <- c("description", "color.x", "color.y", "color.z", "date", "time", "file")

# Date format:
r$date <- date(year = as.numeric(unlist(lapply(strsplit(r$date, "/"), function(x) x[3]))),
               month = as.numeric(unlist(lapply(strsplit(r$date, "/"), function(x) x[1]))),
               day = as.numeric(unlist(lapply(strsplit(r$date, "/"), function(x) x[2]))))
r$date <- as.character(r$date)

# Time format:
tmp <- data.frame(hour   = as.numeric(unlist(lapply(strsplit(r$time, ":"), function(x) x[1]))),
                  minute = as.numeric(unlist(lapply(strsplit(r$time, ":"), function(x) x[2]))),
                  second = as.numeric(unlist(lapply(strsplit(r$time, "[: ]"), function(x) x[3]))),
                  noon   = unlist(lapply(strsplit(r$time, "[: ]"), function(x) x[4])))

tmp$hour[tmp$hour < 11 & tmp$noon == "PM"] <- tmp$hour[tmp$hour < 11 & tmp$noon == "PM"] + 12
r$time <- gulf.utils::time(paste0(tmp$hour, ":", tmp$minute, ":", tmp$second))

# Format other variables:
r$tow.id <- toupper(unlist(lapply(strsplit(r$description, " +"), function(x) x[1])))
r$tow.id <- gsub("[!]", "1", r$tow.id)
r$crab.number <- as.numeric(toupper(unlist(lapply(strsplit(r$description, " +"), function(x) x[3]))))
r$shell.condition <- as.numeric(toupper(unlist(lapply(strsplit(r$description, " +"), function(x) x[5]))))

# Re-order variables:
r <- r[c("date", "time", "tow.id", "crab.number", "shell.condition", "color.x", "color.y", "color.z")]

# Check index matches:
b <- read.scsbio(2024)
ix <- match(r[c("tow.id", "crab.number")], b[c("tow.id", "crab.number")])
r$shell.condition[is.na(r$shell.condition)] <- b$shell.condition[ix[is.na(r$shell.condition)]]

# Convert XYZ colour space to Lab:
XYZ2Lab <- function(X, Y, Z, illuminant = "C"){
   # Define standard illuminant:
   if (illuminant == "D50") I <- c(Xn = 96.4212, Yn = 100, Zn = 82.5188)
   if (illuminant == "D65") I <- c(Xn = 95.0489, Yn = 100, Zn = 108.8840)
   if (illuminant == "C")   I <- c(Xn = 98.0700, Yn = 100, Zn = 118.22)
   
   f <- function(t){
      delta <- 6/29
      ix <- t > delta^3
      v <- rep(NA, length(t))
      v[ix]  <- t[ix]^(1/3)
      v[!ix] <- ((1/3) * t[!ix] * delta^(-2)) + (4 / 29)
      return(v)
   }
   
   # Convert to L*a*b*:
   v <- data.frame(L = 116 * f(Y / I[["Yn"]]) - 16,
                   a = 500 * (f(X / I[["Xn"]]) - f(Y / I[["Yn"]])),
                   b = 200 * (f(Y / I[["Yn"]]) - f(Z / I[["Zn"]])))
   
   # Output:
   return(v)
}

v <- XYZ2Lab(r$color.x, r$color.y, r$color.z)
names(v) <- paste0("color.", names(v))
r <- cbind(r, v)
r <- r[setdiff(names(r), c("color.x", "color.y", "color.z"))]

# Output to gulf.data:
write.csv(r, row.names = FALSE,
          file = "C:/Users/SuretteTJ/Desktop/github/gulf.data/inst/extdata/scs.colorimeter.2024.csv")

r <- cbind(r, b[ix, c("tow.number", "carapace.width", "chela.height", "missing.legs", "samplers")])
r <- r[which(r$carapace.width >= 90), ]

# Output to crab measurement project:
write.csv(r, row.names = FALSE,
          file = "C:/Users/SuretteTJ/Desktop/github/snow-crab-measurement-project/studies/July 2024 - Snow crab survey/data/scs.crab.2024.csv")

gbarplot(table(round(r$color.b), r$shell.condition), col = c("red", "yellow", "green", "blue", "purple"))
plot(r$color.L, r$color.b)
points(r$color.L[r$shell.condition %in% 1:2], r$color.b[r$shell.condition %in% 1:2], pch = 21, bg = "red")
points(r$color.L[r$shell.condition %in% 3], r$color.b[r$shell.condition %in% 3], pch = 21, bg = "green")
points(r$color.L[r$shell.condition %in% 4:5], r$color.b[r$shell.condition %in% 4:5], pch = 21, bg = "blue")

gbarplot(table(round(r$color.b[r$shell.condition %in% 1:2] * 2) / 2), col = fade("red"), grid = TRUE, xlim = c(-5, 30))
gbarplot(table(round(r$color.b[r$shell.condition %in% 3] * 2) / 2), col = fade("green"), add = TRUE)
gbarplot(table(round(r$color.b[r$shell.condition %in% 4:5]* 2) / 2), col = fade("blue"), add = TRUE)
box(col = "grey50")

t <- table(round(r$color.b * 2) / 2, r$shell.condition)
t <- t / repvec(apply(t, 1, sum), ncol = ncol(t))
png(file = "results/figures/colorimeter shell condition proportions.png",
    res = 500, units = "in", height = 5.5, width = 7)
gbarplot(t, col = fade(c("red", "yellow", "green", "blue", "purple")), 
         legend = FALSE, xlim = c(-3, 26))
legend("bottomleft", 
       pch = 22, cex = 1.5,
       legend = 1:5,
       pt.cex = 2.5, 
       pt.bg = fade(c("red", "yellow", "green", "blue", "purple")))
mtext("Color b*", 1, 2.5, cex = 1.25, font = 2)
mtext("Proportion", 2, 2.5, cex = 1.25, font = 2)
box(col = "grey50")
dev.off()
