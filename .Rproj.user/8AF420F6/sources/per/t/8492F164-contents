#install.packages("tidyverse")
library(readxl)
library(tidyverse)
library(ggplot2)
#==============================================================================
#Import main data in nbastats
file_path <- "nbaSheet.xlsx"
nbastats <- data.frame(read_excel(file_path))
#==============================================================================

#將原始資料依照球員位置分隔出
Pos_Playernum <- table(nbastats$Pos)
C <- nbastats[nbastats$Pos == "C",]
PF <- nbastats[nbastats$Pos == "PF",]
SF <- nbastats[nbastats$Pos == "SF",]
SG <- nbastats[nbastats$Pos == "SG",]
PG <- nbastats[nbastats$Pos == "PG",]
#==============================================================================

#將每個位置球員的年齡區間數量總和以十歲為區間 20~29 30~39 40~49 以此類推
for(i in 1: nrow(C)){
  C$Age[i] <- round(C$Age[i]/10) * 10
}
barplot(table(C$Age), col = "lightgreen", main = "The age of C Players")

for(i in 1: nrow(PF)){
  PF$Age[i] <- round(PF$Age[i]/10) * 10
}
barplot(table(PF$Age), col = "lightgreen", main = "The age of PF Players")

for(i in 1: nrow(SF)){
  SF$Age[i] <- round(SF$Age[i]/10) * 10
}
barplot(table(SF$Age), col = "lightgreen", main = "The age of SF Players")

for(i in 1: nrow(SG)){
  SG$Age[i] <- round(SG$Age[i]/10) * 10
}
barplot(table(SG$Age), col = "lightgreen", main = "The age of SG Players")

for(i in 1: nrow(PG)){
  PG$Age[i] <- round(PG$Age[i]/10) * 10
}
barplot(table(PG$Age), col = "lightgreen", main = "The age of PG Players")
#==============================================================================

#建立表格 用來比對各位置的各個資料
Pos <- c("C", "PF", "SF", "SG", "PG")
avr_age <- c(mean(C$Age), mean(PF$Age), mean(SF$Age), mean(SG$Age), mean(PG$Age))
avr_x2p <- c(mean(C$X2P), mean(PF$X2P), mean(SF$X2P), mean(SG$X2P), mean(PG$X2P))
avr_x3p <- c(mean(C$X3P), mean(PF$X3P), mean(SF$X3P), mean(SG$X3P), mean(PG$X3P))
avr_ft <- c(mean(C$FT), mean(PF$FT), mean(SF$FT), mean(SG$FT), mean(PG$FT))
df <- data.frame(Pos, avr_age, avr_x2p, avr_x3p, avr_ft)


df$Age20_29 <- c(sum(C$Age == 20), sum(PF$Age == 20), sum(SF$Age == 20), sum(SG$Age == 20), sum(PG$Age == 20))
df$Age30_39 <- c(sum(C$Age == 30), sum(PF$Age == 30), sum(SF$Age == 30), sum(SG$Age == 30), sum(PG$Age == 30))
df$Age40_49 <- c(sum(C$Age == 40), sum(PF$Age == 40), sum(SF$Age == 40), sum(SG$Age == 40), sum(PG$Age == 40))

#製作表格
barplot(cbind(Age20_29,Age30_39,Age40_49) ~ Pos, data = df,
        col = c("lightgreen", "green", "darkgreen"),
        beside = FALSE,
        horiz = TRUE,
        names.arg = df$Pos,
        axisnames = TRUE,
        ylab = "",
        xlab = "數量",
        legend.text = TRUE,
        )
#==============================================================================

#製作表格
barplot(cbind(avr_x2p, avr_x3p, avr_ft) ~ Pos, data = df,
        col = c("#003366", "#0099CC", "#66CCFF"),
        beside = FALSE,
        horiz = TRUE,
        names.arg = df$Pos,
        axisnames = TRUE,
        ylab = "",
        xlab = "平均球數",
        legend.text = TRUE,
)
#==============================================================================
