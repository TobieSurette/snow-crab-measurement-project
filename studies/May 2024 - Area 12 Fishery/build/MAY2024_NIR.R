library(gulf.data)

study <- "MAY2024"
study.path <- "May 2024 - Area 12 Fishery"

files <- dir(path = paste0("C:/Users/SuretteTJ/Desktop/github/snow-crab-measurement-project/studies/", study.path, "/data/raw"), full.names = TRUE)

res <- NULL
for (j in 1:length(files)){
   print(files[j])
   x <- read.csv(files[j])
   
   names(x) <- gsub("[.]+", ".", tolower(names(x)))
   names(x) <- gsub("[.]$", "", names(x))
   names(x) <- gsub("[_]+", ".", names(x))
   
   fvars <- names(x)[grep("^[xX][0-9]+", names(x))]
   
   print(setdiff(names(x), fvars))
   
   ix <- seq(2, nrow(x), by = 2)
   
   x$crab.number <- x$x
   x <- gulf.utils::compress(x)
   
   for (i in 1:length(ix)){
      tmp <- data.frame(sample.name        = x$sample.name[ix[i]],
                        sample.class       = x$sample.class[ix[i]],
                        session.id         = x$session.id[ix[i]],
                        session            = x$session[ix[i]],
                        session.date.utc   = x$session.date.utc[ix[i]],
                        scan               = x$scan[ix[i]],
                        crab.number        = x$crab.number[ix[i]],
                        #scan.chela.merus  = ifelse(is.null(x$scan.chela.merus[ix[i]]), "", x$scan.chela.merus[ix[i]]),
                        scanner.serial     = x$scanner.serial[ix[i]],
                        scan.configuration = x$scan.configuration[ix[i]],
                        scan.temperature   = x$scan.temperature[ix[i]],
                        scan.humidity      = x$scan.humidity[ix[i]],
                        scan.pga           = x$scan.pga[ix[i]],
                        wavelength         = unlist(x[ix[i]-1, fvars, drop = TRUE]),
                        intensity          = unlist(x[ix[i], fvars, drop = TRUE]))
      
      if (is.null(res)) res <- tmp else res <- rbind(res, tmp)
   }
}

res$scan.chela.merus <- "merus"
#res$scan.chela.merus <- tolower(gsub(" ", "", res$scan.chela.merus))

res$date <- unlist(lapply(strsplit(res$session.date.utc, " "), function(x) x[1]))
res$time <- unlist(lapply(strsplit(res$session.date.utc, " "), function(x) x[2]))

names(res) <- gsub("sample.class", "size.class", names(res))
names(res) <- gsub('scan.chela.merus', 'body.part', names(res))
names(res) <- gsub('intensity', 'value', names(res))

# Organize variables:
remove <- c('session.id', 'sample.name', 'session', 'scan.configuration', 'scanner.serial', 'session.date.utc', 'scan.pga')
res  <- res[, setdiff(names(res), remove)]
rownames(res) <- NULL

res$measure   <- "intensity"
res$measure[res$value < 5] <- "absorption"

# Remove ad-hoc experiment:
#res <- res[res$crab.number <= 70, ]

# Re-order variables:
vars <- c("date", "time", "crab.number", "size.class", "body.part", "scan")
res  <- res[, c(vars, setdiff(names(res), vars))]

# Write reformatted data to file:
#write.csv(res, row.names = FALSE, file = paste0("studies/", study.path, "/data/", study, "_NIR.csv"))

# Write row-oriented version:
key <- c("date",  "time", "crab.number", "size.class", "scan", "body.part", "scan.temperature", "scan.humidity")
tmp <- unique(res[key])
wavelengths <- sort(unique(round(res$wavelength,1)))
tmp[, as.character(wavelengths)] <- NA
int <- tmp
abs <- tmp
rm(tmp)

ix <- match(res[key], int[key])
for (i in 1:nrow(int)){
   int[i, as.character(round(res[(ix == i) & (res$measure == "intensity"), "wavelength"],1))]  <-  res[(ix == i) & (res$measure == "intensity"), "value"]
   abs[i, as.character(round(res[(ix == i) & (res$measure == "absorption"), "wavelength"],1))] <-  res[(ix == i) & (res$measure == "absorption"), "value"]
}

# Write row-oriented files:
#write.csv(int, row.names = FALSE, file = paste0("studies/", study.path, "/data/", study, "_NIR_row_intensity.csv"))
#write.csv(abs, row.names = FALSE, file = paste0("studies/", study.path, "/data/", study, "_NIR_row_absorption.csv"))

# Absolute values:
fvars <- names(abs)[gsub("[0-9.]", "", names(abs)) == ""]
plot(range(as.numeric(wavelengths)), c(min(abs[fvars]), max(abs[fvars])), type = "n")
grid()
for (i in 1:nrow(abs)){
   if (abs$body.part[i] == "merus"){
      if (abs$crab.number[i] %in% c(23:60)) col <- gulf.graphics::fade("blue", alpha = 0.25)
      if (abs$crab.number[i] %in% c(1:22))  col <- gulf.graphics::fade("green2", alpha = 0.25)
      offset <- 0   
   }else{
      col <- "red"
      offset <- 1  
   } 
         
   lines(as.numeric(wavelengths), abs[i, fvars] + offset, col = col, lwd = 0.75)
}

# Intensity values:
fvars <- names(int)[gsub("[0-9.]", "", names(int)) == ""]
plot(range(as.numeric(wavelengths)), c(0, max(int[fvars])), 
     type = "n", xlab = "", ylab = "", xaxs = "i", yaxs = "i")
grid()
j <- 6
for (i in 1:nrow(int)){
   if (int$body.part[i] == "merus"){
      if (int$crab.number[i] %in% c(23:60)) col <- gulf.graphics::fade("blue", alpha = 0.25)
      if (int$crab.number[i] %in% setdiff(c(1:22), 16)) col <- gulf.graphics::fade("green2", alpha = 0.25)
      offset <- 0   
   }else{
      col <- "red"
      offset <- 1  
   } 
   
   if (!(int$crab.number[i] %in% j)){
      lines(as.numeric(wavelengths), int[i, fvars] + offset, col = col, lwd = 2)
   }
   
   #if (!is.na(col)){
   #   text(1277.5, int[i, "1277.5"], int$crab.number[i]) 
   #   text(1507, int[i, "1507"], int$crab.number[i]) 
   #} 
}
for (i in 1:nrow(int)){
   if (int$crab.number[i] %in% j){
      lines(as.numeric(wavelengths), int[i, fvars] + offset, col = gulf.graphics::fade("red", alpha = 0.5), lwd = 3)
      text(1277.5, int[i, "1277.5"], int$scan[i]) 
      text(1507, int[i, "1507"], int$scan[i]) 
   }
}
mtext("Wavelength (nm)", 1, 2.75, font = 2, cex = 1.25)
mtext("Intensity", 2, 2.75, font = 2, cex = 1.25)
box(col = "grey50")


