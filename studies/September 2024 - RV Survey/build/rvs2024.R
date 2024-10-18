library(gulf.data)

x <- read.csv("studies/September 2024 - RV Survey/CAR2024400_collected_specimens.csv")
names(x) <- tolower(names(x))

# Parse date:
x$date <- unlist(lapply(strsplit(x$date.time..canada.atlantic., " "), function(x) x[1]))
x$date <- as.character(gulf.utils::date(year  = as.numeric(unlist(lapply(strsplit(x$date, "/"), function(x) x[3]))), 
                                        month = as.numeric(unlist(lapply(strsplit(x$date, "/"), function(x) x[1]))),
                                        day   = as.numeric(unlist(lapply(strsplit(x$date, "/"), function(x) x[2])))))

# Parse time:
x$time <- gulf.utils::time(unlist(lapply(strsplit(x$date.time..canada.atlantic., " "), function(x) x[2])))
x$time <- gsub(":00$", "", x$time)

x$carapace.width <- as.numeric(unlist(lapply(strsplit(x$length, " "), function(x) x[1])))
x$sex <- 1

# Parse other variables
x$all.observations <- tolower(gsub('"', "", x$all.observations))
fun <- function(x){
   ix <- grep("chelae height", x)
   if (length(ix) == 1) return(x[grep("chelae height", x)]) else return("")
} 
x$chela.height <- unlist(lapply(strsplit(x$all.observations, ", "), fun))
x$chela.height <- as.numeric(unlist(lapply(strsplit(x$chela.height, ":"), function(x) x[2])))

vars <- c("date", "time", "set", "station", "stratum", "specimen.id", "sex", "carapace.width", "chela.height")
x <- x[vars]

write.csv(x, file = "studies/September 2024 - RV Survey/rvs2024.csv", row.names = FALSE, col.names = TRUE)

