# Organize and rename crab photos:

# Find files:
path <- "C:/Users/SuretteTJ/Desktop/scs 2025 homeless data/PHOTOCRABE2025"
file <- data.frame(name = dir(path = path, full.names = TRUE, include.dirs = TRUE, recursive = TRUE, pattern = "*.JPG"))
file$full <- file$name

# File name corrections:
file$name <- unlist(lapply(strsplit(file$name, "/"), function(x) x[length(x)]))
file$name <- gsub("GPF ", "GP", file$name)
file$name <- gsub("FR1F", "FR1", file$name) 
file$name <- gsub(" +[Cc]rab +#", " crab #", file$name)
file$name <- gsub("[#] +", " ", file$name)
file$name <- gsub(" +", " ", file$name)
file$name <- gsub("GP092FR1", "GP092F", file$name) 

# Extract tow ID:
file$tow.id <- unlist(lapply(strsplit(file$name, " "), function(x) x[1]))

# Extract crab number:
tmp <- unlist(lapply(strsplit(file$name, "2025"), function(x) x[1]))
file$crab.number <- as.numeric(unlist(lapply(strsplit(tmp, " "), function(x) x[3])))

# Extract photo number:
file$item <- as.numeric(gsub("[.]JPG", "", unlist(lapply(strsplit(file$name, " "), function(x) x[length(x)]))))

s <- read.scsset(2025, valid = 1)
file$date <- s$date[match(file$tow.id, s$tow.id)]

r <- data.frame(date = file$date,
                tow.id = file$tow.id,
                crab.number = file$crab.number, 
                name = file$name,
                item = file$item)

tmp <- aggregate(r["item"], by = r[c("date", "tow.id", "crab.number")], unique)
tmp$length <- unlist(lapply(tmp[, 3], length))
tmp <- tmp[order(tmp$date), ]

tows <- c("GP105F", "GP287F", "GP342F", "GP346F", "GP020F", "GP052F", "GP108F", 
          "GP066F", "GP050F", "GP168F", "GP221F", "GP043F", "GP330F", "GP333F")
tmp[tmp$tow.id %in% tows, ]


crabs <- unique(file[c("tow.id", "crab.number")])
path <- "C:/Users/SuretteTJ/Desktop/github/snow-crab-measurement-project/studies/July 2025 - Snow crab survey/photos/"
for (i in 1:nrow(crabs)){
   print(i)
   ix <- which(file$tow.id == crabs$tow.id[i] & file$crab.number == crabs$crab.number[i])
   
   # Dorsal view:
   to <- paste0(path, paste0("SCS", 2025, "_", file$tow.id[ix[1]], "_crab_",  file$crab.number[ix[1]], "_dorsal.jpg"))
   if (!file.exists(to)) file.copy(file$full[ix[1]], to)
   
   # Ventral view:
   if (length(ix >= 2)){
      for (j in 2:length(ix)){
         to <- paste0(path, paste0("SCS", 2025, "_", file$tow.id[ix[j]], "_crab_",  file$crab.number[ix[j]], "_ventral"))
         if (length(ix) > 2) to <- paste0(to, "_", j-1, ".jpg") else to <- paste0(to, ".jpg")
         if (!file.exists(to))  file.copy(file$full[ix[j]], to)      
      }
   }
}

