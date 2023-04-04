#08.11.2022 - Igor Salerno Filgueiras
#Global parameters-----
library(meta)

setwd("~/Artigos/Guido/forest_plot_stat/ev_as_death/TACC/")
data <- openxlsx::read.xlsx("../../table.xlsx", sheet = 1)
colnames(data)[c(1,4,5,6,7)] <- c("Name", "n.trt", "n.ctrl", "ev.trt", "ev.ctrl")
data$ev.ctrl <- as.numeric(data$ev.ctrl)

#Event as death----
data$ev.ctrl <- data$n.ctrl - data$ev.ctrl
data$ev.trt <- data$n.trt - data$ev.trt

df <- data[which(data$Study.Design != "Single arm"),]
df <- df[,c(1,4,5,6,7)]

##17 studies----
m.bin <- meta::metabin(ev.trt,n.trt,ev.ctrl,n.ctrl,
                       data = df,
                       studlab = paste(Name),
                       common = T, random = F,
                       method = "MH",sm = "OR",
                       allstudies = T, incr = "TACC") # Mantel Haenszel weighting
png('complete_17_studies_tacc_or.png', res = 300, width = 9, height = 5, units = "in")

meta::forest(m.bin, layout = "RevMan5",
             col.square = "Black",
             leftcols = c("studlab", "event.e", "n.e", "event.c", "n.c", "effect", "ci"),
             label.left = "Favours experimental", label.right = "Favours control",
             test.overall = T)
dev.off()

m.bin <- meta::metabin(ev.trt,n.trt,ev.ctrl,n.ctrl,
                       data = df,
                       studlab = paste(Name),
                       common = T, random = F,
                       method = 'MH',sm = "RR",
                       allstudies = T, incr = "TACC") # Mantel Haenszel weighting

png('complete_17_studies_tacc_rr.png', res = 300, width = 9, height = 5, units = "in")
meta::forest(m.bin, layout = "RevMan5",
             col.square = "Black",
             leftcols = c("studlab", "event.e", "n.e", "event.c", "n.c", "effect", "ci"),
             label.left = "Favours experimental", label.right = "Favours control",
             test.overall = T)
dev.off()

##all 24 studies----
df2 <- data[,c(1,4,5,6,7,12)]
#df2$type <- c(rep("Perinatal",), rep("",))
df2 <- df2[order(df2$Name),]

avg_ctrl <- df2$n.ctrl/df2$n.trt #median is 1 and mean is 1.638216
mean(avg_ctrl,na.rm = T)
median(avg_ctrl,na.rm = T)
avg_ev_ctrl <- df2$ev.ctrl/df2$n.ctrl #median 0.1764706 and mean 0.2585945
mean(avg_ev_ctrl,na.rm = T)
median(avg_ev_ctrl,na.rm = T)

df2$n.ctrl[is.na(df2$n.ctrl)] <- df2$n.trt[is.na(df2$n.ctrl)]
df2$ev.ctrl[is.na(df2$ev.ctrl)] <- round(df2$n.ctrl[is.na(df2$ev.ctrl)]*mean(avg_ev_ctrl, na.rm = T))

#or
m.bin <- meta::metabin(ev.trt,n.trt,ev.ctrl,n.ctrl,
                       data = df2,
                       studlab = paste(Name),
                       common = T, random = F,
                       method = 'MH',sm = "OR",
                       allstudies = T, incr = "TACC") # Mantel Haenszel weighting

png('all_24_tacc_or.png', res = 300, width = 9, height = 6.5, units = "in")
meta::forest(m.bin, layout = "RevMan5",
             col.square = "Black",
             leftcols = c("studlab", "event.e", "n.e", "event.c", "n.c", "effect", "ci"),
             label.left = "Favours experimental", label.right = "Favours control",
             test.overall = T)
dev.off()

#update or
mm <- update(m.bin, subgroup = Peri, print.subgroup.name = F)

png('all_24_byperi_tacc_or.png', res = 300, width = 9, height = 8, units = "in")
meta::forest(mm, layout = "RevMan5",
             col.square = "Black",
             leftcols = c("studlab", "event.e", "n.e", "event.c", "n.c", "effect", "ci"),
             label.left = "Favours experimental", label.right = "Favours control",
             test.overall = T)
dev.off()

#rr
m.bin <- meta::metabin(ev.trt,n.trt,ev.ctrl,n.ctrl,
                       data = df2,
                       studlab = paste(Name),
                       common = T, random = F,
                       method = 'MH',sm = "RR",
                       incr = "TACC", allstudies = T) # Mantel Haenszel weighting

png('all_24_tacc_rr.png', res = 300, width = 9, height = 6.5, units = "in")
meta::forest(m.bin, layout = "RevMan5",
             col.square = "Black",
             leftcols = c("studlab", "event.e", "n.e", "event.c", "n.c", "effect", "ci"),
             label.left = "Favours experimental", label.right = "Favours control",
             test.overall = T)
dev.off()

#update rr
mm <- update(m.bin, subgroup = Peri, print.subgroup.name = F)

png('all_24_byperi_tacc_rr.png', res = 300, width = 9, height = 8, units = "in")
meta::forest(mm, layout = "RevMan5",
             col.square = "Black",
             leftcols = c("studlab", "event.e", "n.e", "event.c", "n.c", "effect", "ci"),
             label.left = "Favours experimental", label.right = "Favours control",
             test.overall = T)
dev.off()


##Perinatal----
data2 <- openxlsx::read.xlsx("../../table.xlsx", sheet = 2)
colnames(data2)[c(1,4,5,6,7)] <- c("Name", "n.trt", "n.ctrl", "ev.trt", "ev.ctrl")
data2$ev.ctrl <- as.numeric(data2$ev.ctrl)

data2$ev.ctrl <- data2$n.ctrl - data2$ev.ctrl
data2$ev.trt <- data2$n.trt - data2$ev.trt

data3 <- data2[which(data2$Study.Design != "Single arm"),]

data3 <- data3[,c(1,4,5,6,7)]


m.bin <- meta::metabin(ev.trt,n.trt,ev.ctrl,n.ctrl,
                       data = data3,
                       studlab = paste(Name),
                       common = T, random = F,
                       method = 'MH',sm = "OR",
                       allstudies = T, incr = "TACC") # Mantel Haenszel weighting

png('perinatal_with_control_tacc_or.png', res = 300, width = 9, height = 4, units = "in")
meta::forest(m.bin, layout = "RevMan5",
             col.square = "Black",
             leftcols = c("studlab", "event.e", "n.e", "event.c", "n.c", "effect", "ci"),
             label.left = "Favours experimental", label.right = "Favours control",
             test.overall = T)
dev.off()

m.bin <- meta::metabin(ev.trt,n.trt,ev.ctrl,n.ctrl,
                       data = data3,
                       studlab = paste(Name),
                       common = T, random = F,
                       method = 'MH',sm = "RR",
                       allstudies = T, incr = "TACC") # Mantel Haenszel weighting

png('perinatal_with_control_tacc_rr.png', res = 300, width = 9, height = 4, units = "in")
meta::forest(m.bin, layout = "RevMan5",
             col.square = "Black",
             leftcols = c("studlab", "event.e", "n.e", "event.c", "n.c", "effect", "ci"),
             label.left = "Favours experimental", label.right = "Favours control",
             test.overall = T)
dev.off()

###Reconstructing controls----
data2 <- data2[order(data2$Name),]

avg_ctrl <- data2$n.ctrl/data2$n.trt #median is 1 and mean is 1.199224
mean(avg_ctrl,na.rm = T)
median(avg_ctrl,na.rm = T)
avg_ev_ctrl <- data2$ev.ctrl/data2$n.ctrl #median 0.1666667 and mean 0.2602011
mean(avg_ev_ctrl,na.rm = T)
median(avg_ev_ctrl,na.rm = T)

data2$n.ctrl[is.na(data2$n.ctrl)] <- data2$n.trt[is.na(data2$n.ctrl)]
data2$ev.ctrl[is.na(data2$ev.ctrl)] <- round(data2$n.ctrl[is.na(data2$ev.ctrl)]*mean(avg_ev_ctrl, na.rm = T))

m.bin <- meta::metabin(ev.trt,n.trt,ev.ctrl,n.ctrl,
                       data = data2,
                       studlab = paste(Name),
                       common = T, random = F,
                       method = 'MH',sm = "OR",
                       allstudies = T, incr = "TACC") # Mantel Haenszel weighting

png('perinatal_all_tacc_or.png', res = 300, width = 9, height = 5.5, units = "in")
meta::forest(m.bin, layout = "RevMan5",
             col.square = "Black",
             leftcols = c("studlab", "event.e", "n.e", "event.c", "n.c", "effect", "ci"),
             label.left = "Favours experimental", label.right = "Favours control",
             test.overall = T)
dev.off()

m.bin <- meta::metabin(ev.trt,n.trt,ev.ctrl,n.ctrl,
                       data = data2,
                       studlab = paste(Name),
                       common = T, random = F,
                       method = 'MH',sm = "RR",
                       allstudies = T, incr = "TACC") # Mantel Haenszel weighting

png('perinatal_all_tacc_rr.png', res = 300, width = 9, height = 5.5, units = "in")
meta::forest(m.bin, layout = "RevMan5",
             col.square = "Black",
             leftcols = c("studlab", "event.e", "n.e", "event.c", "n.c", "effect", "ci"),
             label.left = "Favours experimental", label.right = "Favours control",
             test.overall = T)
dev.off()

