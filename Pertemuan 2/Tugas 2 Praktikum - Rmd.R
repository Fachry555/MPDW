install.packages("sf")
install.packages("sp")

library(sf)
library(sp)

# tes membuat objek sederhana
nc <- st_read(system.file("shape/nc.shp", package="sf"))
plot(nc["AREA"])

plot(clmfires, use.marks = FALSE, pch = ".") 
plot(hamster)

d <- st_read(system.file("shape/nc.shp", package = "sf"),
             quiet = TRUE)
mapview(d, zcol = "SID74")  # SID74 (jumlah kasus kanker)
