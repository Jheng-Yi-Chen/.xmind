
##################################################
#               n file recode                    #
##################################################

##################################################

library(dplyr)
library(stringr)

##################################################

# P5A[is.na(P5A)] <- ""
# P5B[is.na(P5B)] <- ""
# P6A[is.na(P6A)] <- ""
# P6B[is.na(P6B)] <- ""

P5A <- select(P5A, -grep("x", names(P5A)))
P5B <- select(P5B, -grep("x", names(P5B)))
P6A <- select(P6A, -grep("x", names(P6A)))
P6B <- select(P6B, -grep("x", names(P6B)))

P5A <- P5A[-c(1, 2), ]
P5B <- P5B[-c(1, 2), ]
P6A <- P6A[-c(1, 2), ]
P6B <- P6B[-c(1, 2), ]

##################################################
# P6A$P6Q5

# # P6A$P6Q5_11 <- as.numeric(as.character(P6A$P6Q5_11))
# P6A$P6Q5_11 # 308 is "|"
# class(P6A$P6Q5_11)
# where <-  nchar(as.vector(P6A$P6Q5_11)) > 1
# where
# # P6A$P6Q5_11 <- as.vector(P6A$P6Q5_11)
# P6A$P6Q5_11[where] <- "11"
# P6A$P6Q5_11
# P6A$P6Q5_11[P6A$P6Q5_11 == "｜"] <- "11"
# P6A$P6Q5_11

P6A$P6Q5_11[nchar(P6A$P6Q5_11) > 0] <- "11"
P6A$P6Q5_11

# where <- P6A$P6Q5_3 == "2"
# P6A$P6Q5_3[where] <- "3"
# 
# where <- P6A$P6Q5_4 == "3"
# P6A$P6Q5_4[where] <- "4"
# 
# where <- P6A$P6Q5_5 == "4"
# P6A$P6Q5_5[where] <- "5"
# 
# where <- P6A$P6Q5_6 == "5"
# P6A$P6Q5_6[where] <- "6"

P6A$P6Q5_3[P6A$P6Q5_3 == "2"] <- "3"
P6A$P6Q5_4[P6A$P6Q5_4 == "3"] <- "4"
P6A$P6Q5_5[P6A$P6Q5_5 == "4"] <- "5"
P6A$P6Q5_6[P6A$P6Q5_6 == "5"] <- "6"
P6A$P6Q5_7[P6A$P6Q5_7 == "6"] <- "7"
P6A$P6Q5_8[P6A$P6Q5_8 == "7"] <- "8"
P6A$P6Q5_9[P6A$P6Q5_9 == "8"] <- "9"
P6A$P6Q5_10[P6A$P6Q5_10 == "9"] <- "10"

# for (x in P6A$P6Q5_11) {
#   x <-  nchar(as.vector(P6A$P6Q5_11)) > 1
#     if (x == TRUE) {
#       gsub(x, 11, P6A$P6Q5_11)
#   }
# }

# P6A$P6Q5_11 %>% 
#   as.numeric() %>% 
#   nchar() > 1 %>% 
#   recode(11)

# P6A$P6Q5_11[which(nchar(as.vector(P6A$P6Q5_11)) > 1)] <- "11"

# nchar(as.vector(P6A$P6Q5_11))

P6A$P6Q5 <- paste(P6A$P6Q5_1, P6A$P6Q5_3, P6A$P6Q5_4, P6A$P6Q5_5, P6A$P6Q5_6, P6A$P6Q5_7, P6A$P6Q5_8, P6A$P6Q5_9, P6A$P6Q5_10, P6A$P6Q5_11, sep = ",")
# P6A$P6Q5 <- str_c(P6A$P6Q5_1, P6A$P6Q5_3, P6A$P6Q5_4, P6A$P6Q5_5, P6A$P6Q5_6, P6A$P6Q5_7, P6A$P6Q5_8, P6A$P6Q5_9, P6A$P6Q5_10, P6A$P6Q5_11, sep = "")
P6A$P6Q5 <- gsub(",,", ",", P6A$P6Q5)
# P6A$P6Q5 <- gsub(",3,", "3,", P6A$P6Q5)
# P6A$P6Q5 <- gsub(",4,", "4,", P6A$P6Q5)
# P6A$P6Q5 <- gsub(",5,", "5,", P6A$P6Q5)
# P6A$P6Q5 <- gsub(",7,", "7,", P6A$P6Q5)
# P6A$P6Q5 <- gsub(",0,", ",0", P6A$P6Q5)
# P6A$P6Q5 <- gsub(",1,", ",1", P6A$P6Q5)
# P6A$P6Q5 <- gsub(",4,", ",4", P6A$P6Q5)
P6A$P6Q5
class(P6A$P6Q5)
# str_count(P6A$P6Q5)
# P6A$P6Q5 <- strsplit(P6A$P6Q5, ",")
# P6A$P6Q5 <- grep(",,", "", P6A$P6Q5)

# P6A$P6Q5[str_sub(string = P6A$P6Q5, start = 1, end = 1) == ","]
# P6A$P6Q5[str_sub(string = P6A$P6Q5, start = nchar(P6A$P6Q5), end = nchar(P6A$P6Q5)) == ","]

P6A$P6Q5[str_sub(string = P6A$P6Q5, start = 1, end = 1) == ","] <- str_replace(string =  P6A$P6Q5[str_sub(string = P6A$P6Q5, start = 1, end = 1) == ","], pattern = "^\\,", replacement = "")
P6A$P6Q5[str_sub(string = P6A$P6Q5, start = nchar(P6A$P6Q5), end = nchar(P6A$P6Q5)) == ","] <- 
  str_replace(string =  P6A$P6Q5[str_sub(string = P6A$P6Q5, start = nchar(P6A$P6Q5), end = nchar(P6A$P6Q5)) == ","], pattern = "\\,$", replacement = "")

##################################################
# P6B$P6Q5

P6B$P6Q5_11; class(P6B$P6Q5_11)
P6B$P6Q5_11[nchar(P6B$P6Q5_11) > 0] <- "11"
P6B$P6Q5_3[P6B$P6Q5_3 == "2"] <- "3"
P6B$P6Q5_4[P6B$P6Q5_4 == "3"] <- "4"
P6B$P6Q5_5[P6B$P6Q5_5 == "4"] <- "5"
P6B$P6Q5_6[P6B$P6Q5_6 == "5"] <- "6"
P6A$P6Q5_7[P6A$P6Q5_7 == "6"] <- "7"
P6B$P6Q5_8[P6B$P6Q5_8 == "7"] <- "8"
P6B$P6Q5_9[P6B$P6Q5_9 == "8"] <- "9"
P6B$P6Q5_10[P6B$P6Q5_10 == "9"] <- "10"

P6B$P6Q5 <- paste(P6B$P6Q5_1, P6B$P6Q5_3, P6B$P6Q5_4, P6B$P6Q5_5, P6B$P6Q5_6, P6B$P6Q5_7, P6B$P6Q5_8, P6B$P6Q5_9, P6B$P6Q5_10, P6B$P6Q5_11, sep = ",")
P6B$P6Q5

P6B$P6Q5 <- gsub(",,", ",", P6B$P6Q5); P6B$P6Q5 <- gsub(",,", ",", P6B$P6Q5)
P6B$P6Q5
class(P6B$P6Q5)

P6B$P6Q5[str_sub(string = P6B$P6Q5, start = 1, end = 1) == ","] <- str_replace(string =  P6B$P6Q5[str_sub(string = P6B$P6Q5, start = 1, end = 1) == ","], pattern = "^\\,", replacement = "")
P6B$P6Q5[str_sub(string = P6B$P6Q5, start = nchar(P6B$P6Q5), end = nchar(P6B$P6Q5)) == ","] <- 
  str_replace(string =  P6B$P6Q5[str_sub(string = P6B$P6Q5, start = nchar(P6B$P6Q5), end = nchar(P6B$P6Q5)) == ","], pattern = "\\,$", replacement = "")

P6B$P6Q5

##################################################
# P6A$P6Q12

P6A$P6Q12 <- paste(P6A$P6Q12_1, P6A$P6Q12_2, P6A$P6Q12_3, P6A$P6Q12_4, P6A$P6Q12_5, P6A$P6Q12_6, P6A$P6Q12_7, P6A$P6Q12_8, P6A$P6Q12_9, P6A$P6Q12_10, P6A$P6Q12_99, sep = ",")
P6A$P6Q12
P6A$P6Q12 <- gsub(",,", ",", P6A$P6Q12)
P6A$P6Q12

P6A$P6Q12[str_sub(string = P6A$P6Q12, start = 1, end = 1) == ","] <- str_replace(string =  P6A$P6Q12[str_sub(string = P6A$P6Q12, start = 1, end = 1) == ","], pattern = "^\\,", replacement = "")
P6A$P6Q12[str_sub(string = P6A$P6Q12, start = nchar(P6A$P6Q12), end = nchar(P6A$P6Q12)) == ","] <- 
  str_replace(string =  P6A$P6Q12[str_sub(string = P6A$P6Q12, start = nchar(P6A$P6Q12), end = nchar(P6A$P6Q12)) == ","], pattern = "\\,$", replacement = "")

P6A$P6Q12

##################################################
# P6B$P6Q12

P6B$P6Q12 <- paste(P6B$P6Q12_1, P6B$P6Q12_2, P6B$P6Q12_3, P6B$P6Q12_4, P6B$P6Q12_5, P6B$P6Q12_6, P6B$P6Q12_7, P6B$P6Q12_8, P6B$P6Q12_9, P6B$P6Q12_10, P6B$P6Q12_99, sep = ",")

P6B$P6Q12 <- gsub(",,", ",", P6B$P6Q12)
P6B$P6Q12

P6B$P6Q12[str_sub(string = P6B$P6Q12, start = 1, end = 1) == ","] <- str_replace(string =  P6B$P6Q12[str_sub(string = P6B$P6Q12, start = 1, end = 1) == ","], pattern = "^\\,", replacement = "")
P6B$P6Q12[str_sub(string = P6B$P6Q12, start = nchar(P6B$P6Q12), end = nchar(P6B$P6Q12)) == ","] <- 
  str_replace(string =  P6B$P6Q12[str_sub(string = P6B$P6Q12, start = nchar(P6B$P6Q12), end = nchar(P6B$P6Q12)) == ","], pattern = "\\,$", replacement = "")

P6B$P6Q12

##################################################
# P6A$P6Q13

P6A$P6Q13_99[P6A$P6Q13_99 == "1"] <- "99"
P6A$P6Q13_1[P6A$P6Q13_1 == "2"] <- "1"
P6A$P6Q13_2[P6A$P6Q13_2 == "3"] <- "2"
P6A$P6Q13_3[P6A$P6Q13_3 == "4"] <- "3"
P6A$P6Q13_4[P6A$P6Q13_4 == "5"] <- "4"
P6A$P6Q13_5[P6A$P6Q13_5 == "6"] <- "5"
P6A$P6Q13_6[P6A$P6Q13_6 == "7"] <- "6"
P6A$P6Q13_7[P6A$P6Q13_7 == "8"] <- "7"
P6A$P6Q13_8[P6A$P6Q13_8 == "9"] <- "8"
P6A$P6Q13_9[P6A$P6Q13_9 == "10"] <- "9"
P6A$P6Q13_10[P6A$P6Q13_10 == "11"] <- "10"
P6A$P6Q13_98[nchar(P6A$P6Q13_98) > 0] <- "98"

P6A$P6Q13 <- paste(P6A$P6Q13_1, P6A$P6Q13_2, P6A$P6Q13_3, P6A$P6Q13_4, P6A$P6Q13_5, P6A$P6Q13_6, P6A$P6Q13_7, P6A$P6Q13_8, P6A$P6Q13_9, P6A$P6Q13_10, P6A$P6Q13_98, P6A$P6Q13_99, sep = ",")
P6A$P6Q13
P6A$P6Q13 <- gsub(",,", ",", P6A$P6Q13); P6A$P6Q13

P6A$P6Q13[str_sub(string = P6A$P6Q13, start = 1, end = 1) == ","] <- str_replace(string =  P6A$P6Q13[str_sub(string = P6A$P6Q13, start = 1, end = 1) == ","], pattern = "^\\,", replacement = "")
P6A$P6Q13[str_sub(string = P6A$P6Q13, start = nchar(P6A$P6Q13), end = nchar(P6A$P6Q13)) == ","] <- 
  str_replace(string =  P6A$P6Q13[str_sub(string = P6A$P6Q13, start = nchar(P6A$P6Q13), end = nchar(P6A$P6Q13)) == ","], pattern = "\\,$", replacement = "")

P6A$P6Q13

##################################################
# P6B$P6Q13

P6B$P6Q13_99[P6B$P6Q13_99 == "1"] <- "99"
P6B$P6Q13_1[P6B$P6Q13_1 == "2"] <- "1"
P6B$P6Q13_2[P6B$P6Q13_2 == "3"] <- "2"
P6B$P6Q13_3[P6B$P6Q13_3 == "4"] <- "3"
P6B$P6Q13_4[P6B$P6Q13_4 == "5"] <- "4"
P6B$P6Q13_5[P6B$P6Q13_5 == "6"] <- "5"
P6B$P6Q13_6[P6B$P6Q13_6 == "7"] <- "6"
P6B$P6Q13_7[P6B$P6Q13_7 == "8"] <- "7"
P6B$P6Q13_8[P6B$P6Q13_8 == "9"] <- "8"
P6B$P6Q13_9[P6B$P6Q13_9 == "10"] <- "9"
P6B$P6Q13_10[P6B$P6Q13_10 == "11"] <- "10"
P6B$P6Q13_98[nchar(P6B$P6Q13_98) > 0] <- "98"

P6B$P6Q13 <- paste(P6B$P6Q13_1, P6B$P6Q13_2, P6B$P6Q13_3, P6B$P6Q13_4, P6B$P6Q13_5, P6B$P6Q13_6, P6B$P6Q13_7, P6B$P6Q13_8, P6B$P6Q13_9, P6B$P6Q13_10, P6B$P6Q13_98, P6B$P6Q13_99, sep = ",")
P6B$P6Q13
P6B$P6Q13 <- gsub(",,", ",", P6B$P6Q13); P6B$P6Q13

##################################################
# P6A$P6Q14

P6A$P6Q14_99[P6A$P6Q14_99 == "1"] <- "99"
P6A$P6Q14_1[P6A$P6Q14_1 == "2"] <- "1"
P6A$P6Q14_2[P6A$P6Q14_2 == "3"] <- "2"
P6A$P6Q14_3[P6A$P6Q14_3 == "4"] <- "3"
P6A$P6Q14_4[P6A$P6Q14_4 == "5"] <- "4"
P6A$P6Q14_5[P6A$P6Q14_5 == "6"] <- "5"
P6A$P6Q14_6[P6A$P6Q14_6 == "7"] <- "6"
P6A$P6Q14_7[P6A$P6Q14_7 == "8"] <- "7"
P6A$P6Q14_8[P6A$P6Q14_8 == "9"] <- "8"
P6A$P6Q14_9[P6A$P6Q14_9 == "10"] <- "9"
P6A$P6Q14_10[P6A$P6Q14_10 == "11"] <- "10"
P6A$P6Q14_98[nchar(P6A$P6Q14_98) > 0] <- "98"

P6A$P6Q14 <- paste(P6A$P6Q14_1, P6A$P6Q14_2, P6A$P6Q14_3, P6A$P6Q14_4, P6A$P6Q14_5, P6A$P6Q14_6, P6A$P6Q14_7, P6A$P6Q14_8, P6A$P6Q14_9, P6A$P6Q14_10, P6A$P6Q14_98, P6A$P6Q14_99, sep = ",")
P6A$P6Q14 <- gsub(",,", ",", P6A$P6Q14); P6A$P6Q14

##################################################
# P6B$P6Q14

P6B$P6Q14_99[P6B$P6Q14_99 == "1"] <- "99"
P6B$P6Q14_1[P6B$P6Q14_1 == "2"] <- "1"
P6B$P6Q14_2[P6B$P6Q14_2 == "3"] <- "2"
P6B$P6Q14_3[P6B$P6Q14_3 == "4"] <- "3"
P6B$P6Q14_4[P6B$P6Q14_4 == "5"] <- "4"
P6B$P6Q14_5[P6B$P6Q14_5 == "6"] <- "5"
P6B$P6Q14_6[P6B$P6Q14_6 == "7"] <- "6"
P6B$P6Q14_7[P6B$P6Q14_7 == "8"] <- "7"
P6B$P6Q14_8[P6B$P6Q14_8 == "9"] <- "8"
P6B$P6Q14_9[P6B$P6Q14_9 == "10"] <- "9"
P6B$P6Q14_10[P6B$P6Q14_10 == "11"] <- "10"
P6B$P6Q14_98[nchar(P6B$P6Q14_98) > 0] <- "98"

P6B$P6Q14 <- paste(P6B$P6Q14_1, P6B$P6Q14_2, P6B$P6Q14_3, P6B$P6Q14_4, P6B$P6Q14_5, P6B$P6Q14_6, P6B$P6Q14_7, P6B$P6Q14_8, P6B$P6Q14_9, P6B$P6Q14_10, P6B$P6Q14_98, P6B$P6Q14_99, sep = ",")
P6B$P6Q14 <- gsub(",,", ",", P6B$P6Q14); P6B$P6Q14

##################################################
# P6A$P6Q15

P6A$P6Q15_99[P6A$P6Q15_99 == "1"] <- "99"
P6A$P6Q15_1[P6A$P6Q15_1 == "2"] <- "1"
P6A$P6Q15_2[P6A$P6Q15_2 == "3"] <- "2"
P6A$P6Q15_3[P6A$P6Q15_3 == "4"] <- "3"
P6A$P6Q15_4[P6A$P6Q15_4 == "5"] <- "4"
P6A$P6Q15_5[P6A$P6Q15_5 == "6"] <- "5"
P6A$P6Q15_6[P6A$P6Q15_6 == "7"] <- "6"
P6A$P6Q15_7[P6A$P6Q15_7 == "8"] <- "7"
P6A$P6Q15_8[P6A$P6Q15_8 == "9"] <- "8"
P6A$P6Q15_9[P6A$P6Q15_9 == "10"] <- "9"
P6A$P6Q15_10[P6A$P6Q15_10 == "11"] <- "10"
P6A$P6Q15_98[nchar(P6A$P6Q15_98) > 0] <- "98"

P6A$P6Q15 <- paste(P6A$P6Q15_1, P6A$P6Q15_2, P6A$P6Q15_3, P6A$P6Q15_4, P6A$P6Q15_5, P6A$P6Q15_6, P6A$P6Q15_7, P6A$P6Q15_8, P6A$P6Q15_9, P6A$P6Q15_10, P6A$P6Q15_98, P6A$P6Q15_99, sep = ",")
P6A$P6Q15 <- gsub(",,", ",", P6A$P6Q15); P6A$P6Q15

##################################################
# P6B$P6Q15

P6B$P6Q15_99[P6B$P6Q15_99 == "1"] <- "99"
P6B$P6Q15_1[P6B$P6Q15_1 == "2"] <- "1"
P6B$P6Q15_2[P6B$P6Q15_2 == "3"] <- "2"
P6B$P6Q15_3[P6B$P6Q15_3 == "4"] <- "3"
P6B$P6Q15_4[P6B$P6Q15_4 == "5"] <- "4"
P6B$P6Q15_5[P6B$P6Q15_5 == "6"] <- "5"
P6B$P6Q15_6[P6B$P6Q15_6 == "7"] <- "6"
P6B$P6Q15_7[P6B$P6Q15_7 == "8"] <- "7"
P6B$P6Q15_8[P6B$P6Q15_8 == "9"] <- "8"
P6B$P6Q15_9[P6B$P6Q15_9 == "10"] <- "9"
P6B$P6Q15_10[P6B$P6Q15_10 == "11"] <- "10"
P6B$P6Q15_98[nchar(P6B$P6Q15_98) > 0] <- "98"

P6B$P6Q15 <- paste(P6B$P6Q15_1, P6B$P6Q15_2, P6B$P6Q15_3, P6B$P6Q15_4, P6B$P6Q15_5, P6B$P6Q15_6, P6B$P6Q15_7, P6B$P6Q15_8, P6B$P6Q15_9, P6B$P6Q15_10, P6B$P6Q15_98, P6B$P6Q15_99, sep = ",")
P6B$P6Q15 <- gsub(",,", ",", P6B$P6Q15); P6B$P6Q15

##################################################
# P6A$P6Q16

# for(x in table) {
#   function() {
#     table_function <- tablb()
#   }; x <- seq(1:12)
# }
  
table(P6A$P6Q16_1)
table(P6A$P6Q16_2)
table(P6A$P6Q16_3)
table(P6A$P6Q16_4)
table(P6A$P6Q16_5)
table(P6A$P6Q16_6)
table(P6A$P6Q16_7)
table(P6A$P6Q16_8)
table(P6A$P6Q16_9)
table(P6A$P6Q16_10)
table(P6A$P6Q16_11)
table(P6A$P6Q16_12)

P6A$P6Q16_12[nchar(P6A$P6Q16_12) > 0] <- "12"

P6A$P6Q16 <- paste(P6A$P6Q16_1, P6A$P6Q16_2, P6A$P6Q16_3, P6A$P6Q16_4, P6A$P6Q16_5, P6A$P6Q16_6, P6A$P6Q16_7, P6A$P6Q16_8, P6A$P6Q16_9, P6A$P6Q16_10, P6A$P6Q16_11, P6A$P6Q16_12, sep = ",")
P6A$P6Q16 <- gsub(",,", ",", P6A$P6Q16); P6A$P6Q16

##################################################
# P6B$P6Q16

table(P6B$P6Q16_1)
table(P6B$P6Q16_2)
table(P6B$P6Q16_3)
table(P6B$P6Q16_4)
table(P6B$P6Q16_5)
table(P6B$P6Q16_6)
table(P6B$P6Q16_7)
table(P6B$P6Q16_8)
table(P6B$P6Q16_9)
table(P6B$P6Q16_10)
table(P6B$P6Q16_11)
table(P6B$P6Q16_12)

P6B$P6Q16 <- paste(P6B$P6Q16_1, P6B$P6Q16_2, P6B$P6Q16_3, P6B$P6Q16_4, P6B$P6Q16_5, P6B$P6Q16_6, P6B$P6Q16_7, P6B$P6Q16_8, P6B$P6Q16_9, P6B$P6Q16_10, P6B$P6Q16_11, P6B$P6Q16_12, sep = ",")
P6B$P6Q16 <- gsub(",,", ",", P6B$P6Q16); P6B$P6Q16

##################################################
# P6A$P6Q17

P6A$P6Q17_11[P6A$P6Q17_11 == "1"] <- "11"
P6A$P6Q17_12[P6A$P6Q17_12 == "2"] <- "12"
P6A$P6Q17_13[P6A$P6Q17_13 == "3"] <- "13"
P6A$P6Q17_14[P6A$P6Q17_14 == "4"] <- "14"
P6A$P6Q17_15[P6A$P6Q17_15 == "5"] <- "15"
P6A$P6Q17_16[P6A$P6Q17_16 == "6"] <- "16"
P6A$P6Q17_99[P6A$P6Q17_99 == "7"] <- "99"

P6A$P6Q17 <- paste(P6A$P6Q17_11, P6A$P6Q17_12, P6A$P6Q17_13, P6A$P6Q17_14, P6A$P6Q17_15, P6A$P6Q17_16, P6A$P6Q17_99, sep = ",")
P6A$P6Q17 <- gsub(",,", ",", P6A$P6Q17); P6A$P6Q17

##################################################
# P6B$P6Q17

P6B$P6Q17_11[P6B$P6Q17_11 == "1"] <- "11"
P6B$P6Q17_12[P6B$P6Q17_12 == "2"] <- "12"
P6B$P6Q17_13[P6B$P6Q17_13 == "3"] <- "13"
P6B$P6Q17_14[P6B$P6Q17_14 == "4"] <- "14"
P6B$P6Q17_15[P6B$P6Q17_15 == "5"] <- "15"
P6B$P6Q17_16[P6B$P6Q17_16 == "6"] <- "16"
P6B$P6Q17_99[P6B$P6Q17_99 == "7"] <- "99"

P6B$P6Q17 <- paste(P6B$P6Q17_11, P6B$P6Q17_12, P6B$P6Q17_13, P6B$P6Q17_14, P6B$P6Q17_15, P6B$P6Q17_16, P6B$P6Q17_99, sep = ",")
P6B$P6Q17 <- gsub(",,", ",", P6B$P6Q17); P6B$P6Q17

##################################################
# P6A$P6Q18

table(P6A$P6Q18_1)
table(P6A$P6Q18_2)
table(P6A$P6Q18_3)
table(P6A$P6Q18_4)
table(P6A$P6Q18_5)
table(P6A$P6Q18_6)
table(P6A$P6Q18_7)

P6A$P6Q18 <- paste(P6A$P6Q18_1, P6A$P6Q18_2, P6A$P6Q18_3, P6A$P6Q18_4, P6A$P6Q18_5, P6A$P6Q18_6, P6A$P6Q18_7, sep = ",")
P6A$P6Q18 <- gsub(",,", ",", P6A$P6Q18); P6A$P6Q18

##################################################
# P6B$P6Q18

table(P6B$P6Q18_1)
table(P6B$P6Q18_2)
table(P6B$P6Q18_3)
table(P6B$P6Q18_4)
table(P6B$P6Q18_5)
table(P6B$P6Q18_6)
table(P6B$P6Q18_7)

P6B$P6Q18 <- paste(P6B$P6Q18_1, P6B$P6Q18_2, P6B$P6Q18_3, P6B$P6Q18_4, P6B$P6Q18_5, P6B$P6Q18_6, P6B$P6Q18_7, sep = ",")
P6B$P6Q18 <- gsub(",,", ",", P6B$P6Q18); P6B$P6Q18

##################################################
# P5A$ID5
# P5B$ID5
# P6A$ID6
# P6B$ID6

P5A$ID5 <- P5A$MemberID5
P5B$ID5 <- P5B$MemberID5
P6A$ID6 <- P6A$MemberID6
P6B$ID6 <- P6B$MemberID6

##################################################

P5A$Qtype5 <- 51
P5B$Qtype5 <- 52
P6A$Qtype6 <- 61
P6B$Qtype6 <- 62

##################################################
# P6A$P6Q2
# P6B$P6Q2

table(P6A$P6Q2)
table(P6A$P6Q2_3)
P6A$P6Q2[nchar(P6A$P6Q2_3) > 0] <- "3"; table(P6A$P6Q2)

table(P6B$P6Q2)
table(P6B$P6Q2_3)
P6B$P6Q2[nchar(P6B$P6Q2_3) > 0] <- "3"; table(P6B$P6Q2)

##################################################
# P6A$P6Q3
# P6B$P6Q3

table(P6A$P6Q3)
table(P6A$P6Q3_3)
P6A$P6Q3[nchar(P6A$P6Q3_3) > 0] <- "3"; table(P6A$P6Q3)

table(P6B$P6Q3)
table(P6B$P6Q3_3)
P6B$P6Q3[nchar(P6B$P6Q3_3) > 0] <- "3"; table(P6B$P6Q3)
# P6B$P6Q3[98]

##################################################
# P6A$P6Q1
# P6B$P6Q1

P6A$P6Q1_D
P6A$P6Q1_M
P6A$P6Q1_Y

P6B$P6Q1_D
P6B$P6Q1_M
P6B$P6Q1_Y

##################################################
# P6A$P6Q4_1
# P6A$P6Q4_2
# P6A$P6Q4_3

# sum(P6A$P6Q4_R_1 == "1")

P6A$P6Q4_1[P6A$P6Q4_R_1 == "1"] <- "1"
P6A$P6Q4_2[P6A$P6Q4_R_1 == "2"] <- "1"
P6A$P6Q4_3[P6A$P6Q4_R_1 == "3"] <- "1"

P6A$P6Q4_1[P6A$P6Q4_R_2 == "1"] <- "2"
P6A$P6Q4_2[P6A$P6Q4_R_2 == "2"] <- "2"
P6A$P6Q4_3[P6A$P6Q4_R_2 == "3"] <- "2"

P6A$P6Q4_1[P6A$P6Q4_R_3 == "1"] <- "3"
P6A$P6Q4_2[P6A$P6Q4_R_3 == "2"] <- "3"
P6A$P6Q4_3[P6A$P6Q4_R_3 == "3"] <- "3"

P6A$P6Q4_1[P6A$P6Q4_R_4 == "1"] <- "4"
P6A$P6Q4_2[P6A$P6Q4_R_4 == "2"] <- "4"
P6A$P6Q4_3[P6A$P6Q4_R_4 == "3"] <- "4"

P6A$P6Q4_1[P6A$P6Q4_R_5 == "1"] <- "5"
P6A$P6Q4_2[P6A$P6Q4_R_5 == "2"] <- "5"
P6A$P6Q4_3[P6A$P6Q4_R_5 == "3"] <- "5"

P6A$P6Q4_1[P6A$P6Q4_R_6 == "1"] <- "6"
P6A$P6Q4_2[P6A$P6Q4_R_6 == "2"] <- "6"
P6A$P6Q4_3[P6A$P6Q4_R_6 == "3"] <- "6"

P6A$P6Q4_1[P6A$P6Q4_R_7 == "1"] <- "7"
P6A$P6Q4_2[P6A$P6Q4_R_7 == "2"] <- "7"
P6A$P6Q4_3[P6A$P6Q4_R_7 == "3"] <- "7"

P6A$P6Q4_1[P6A$P6Q4_R_8 == "1"] <- "8"
P6A$P6Q4_2[P6A$P6Q4_R_8 == "2"] <- "8"
P6A$P6Q4_3[P6A$P6Q4_R_8 == "3"] <- "8"

P6A$P6Q4_1[P6A$P6Q4_R_9 == "1"] <- "9"
P6A$P6Q4_2[P6A$P6Q4_R_9 == "2"] <- "9"
P6A$P6Q4_3[P6A$P6Q4_R_9 == "3"] <- "9"

P6A$P6Q4_1[P6A$P6Q4_R_10 == "1"] <- "10"
P6A$P6Q4_2[P6A$P6Q4_R_10 == "2"] <- "10"
P6A$P6Q4_3[P6A$P6Q4_R_10 == "3"] <- "10"

P6A$P6Q4_1[P6A$P6Q4_R_11 == "1"] <- "11"
P6A$P6Q4_2[P6A$P6Q4_R_11 == "2"] <- "11"
P6A$P6Q4_3[P6A$P6Q4_R_11 == "3"] <- "11"

P6A$P6Q4_1[P6A$P6Q4_R_12 == "1"] <- "12"
P6A$P6Q4_2[P6A$P6Q4_R_12 == "2"] <- "12"
P6A$P6Q4_3[P6A$P6Q4_R_12 == "3"] <- "12"

P6A$P6Q4_1[P6A$P6Q4_R_13 == "1"] <- "13"
P6A$P6Q4_2[P6A$P6Q4_R_13 == "2"] <- "13"
P6A$P6Q4_3[P6A$P6Q4_R_13 == "3"] <- "13"

P6A$P6Q4_1[P6A$P6Q4_R_14 == "1"] <- "14"
P6A$P6Q4_2[P6A$P6Q4_R_14 == "2"] <- "14"
P6A$P6Q4_3[P6A$P6Q4_R_14 == "3"] <- "14"

P6A$P6Q4_1[P6A$P6Q4_R_15 == "1"] <- "15"
P6A$P6Q4_2[P6A$P6Q4_R_15 == "2"] <- "15"
P6A$P6Q4_3[P6A$P6Q4_R_15 == "3"] <- "15"

P6A$P6Q4_1[P6A$P6Q4_R_16 == "1"] <- "16"
P6A$P6Q4_2[P6A$P6Q4_R_16 == "2"] <- "16"
P6A$P6Q4_3[P6A$P6Q4_R_16 == "3"] <- "16"

# P6A$P6Q4_1[P6A$ID6 == "15945"]
# P6A$P6Q4_2[P6A$ID6 == "15945"]
# P6A$P6Q4_3[P6A$ID6 == "15945"]
# 
# P6A$P6Q4_1[P6A$ID6 == "40613"]
# P6A$P6Q4_2[P6A$ID6 == "40613"]
# P6A$P6Q4_3[P6A$ID6 == "40613"]
# 
# P6A$P6Q4_1[P6A$ID6 == "12918"]
# P6A$P6Q4_2[P6A$ID6 == "12918"]
# P6A$P6Q4_3[P6A$ID6 == "12918"]

##################################################
# P6B$P6Q4_1
# P6B$P6Q4_2
# P6B$P6Q4_3

P6B$P6Q4_1[P6B$P6Q4_R_1 == "1"] <- "1"
P6B$P6Q4_2[P6B$P6Q4_R_1 == "2"] <- "1"
P6B$P6Q4_3[P6B$P6Q4_R_1 == "3"] <- "1"

P6B$P6Q4_1[P6B$P6Q4_R_2 == "1"] <- "2"
P6B$P6Q4_2[P6B$P6Q4_R_2 == "2"] <- "2"
P6B$P6Q4_3[P6B$P6Q4_R_2 == "3"] <- "2"

P6B$P6Q4_1[P6B$P6Q4_R_3 == "1"] <- "3"
P6B$P6Q4_2[P6B$P6Q4_R_3 == "2"] <- "3"
P6B$P6Q4_3[P6B$P6Q4_R_3 == "3"] <- "3"

P6B$P6Q4_1[P6B$P6Q4_R_4 == "1"] <- "4"
P6B$P6Q4_2[P6B$P6Q4_R_4 == "2"] <- "4"
P6B$P6Q4_3[P6B$P6Q4_R_4 == "3"] <- "4"

P6B$P6Q4_1[P6B$P6Q4_R_5 == "1"] <- "5"
P6B$P6Q4_2[P6B$P6Q4_R_5 == "2"] <- "5"
P6B$P6Q4_3[P6B$P6Q4_R_5 == "3"] <- "5"

P6B$P6Q4_1[P6B$P6Q4_R_6 == "1"] <- "6"
P6B$P6Q4_2[P6B$P6Q4_R_6 == "2"] <- "6"
P6B$P6Q4_3[P6B$P6Q4_R_6 == "3"] <- "6"

P6B$P6Q4_1[P6B$P6Q4_R_7 == "1"] <- "7"
P6B$P6Q4_2[P6B$P6Q4_R_7 == "2"] <- "7"
P6B$P6Q4_3[P6B$P6Q4_R_7 == "3"] <- "7"

P6B$P6Q4_1[P6B$P6Q4_R_8 == "1"] <- "8"
P6B$P6Q4_2[P6B$P6Q4_R_8 == "2"] <- "8"
P6B$P6Q4_3[P6B$P6Q4_R_8 == "3"] <- "8"

P6B$P6Q4_1[P6B$P6Q4_R_9 == "1"] <- "9"
P6B$P6Q4_2[P6B$P6Q4_R_9 == "2"] <- "9"
P6B$P6Q4_3[P6B$P6Q4_R_9 == "3"] <- "9"

P6B$P6Q4_1[P6B$P6Q4_R_10 == "1"] <- "10"
P6B$P6Q4_2[P6B$P6Q4_R_10 == "2"] <- "10"
P6B$P6Q4_3[P6B$P6Q4_R_10 == "3"] <- "10"

P6B$P6Q4_1[P6B$P6Q4_R_11 == "1"] <- "11"
P6B$P6Q4_2[P6B$P6Q4_R_11 == "2"] <- "11"
P6B$P6Q4_3[P6B$P6Q4_R_11 == "3"] <- "11"

P6B$P6Q4_1[P6B$P6Q4_R_12 == "1"] <- "12"
P6B$P6Q4_2[P6B$P6Q4_R_12 == "2"] <- "12"
P6B$P6Q4_3[P6B$P6Q4_R_12 == "3"] <- "12"

P6B$P6Q4_1[P6B$P6Q4_R_13 == "1"] <- "13"
P6B$P6Q4_2[P6B$P6Q4_R_13 == "2"] <- "13"
P6B$P6Q4_3[P6B$P6Q4_R_13 == "3"] <- "13"

P6B$P6Q4_1[P6B$P6Q4_R_14 == "1"] <- "14"
P6B$P6Q4_2[P6B$P6Q4_R_14 == "2"] <- "14"
P6B$P6Q4_3[P6B$P6Q4_R_14 == "3"] <- "14"

P6B$P6Q4_1[P6B$P6Q4_R_15 == "1"] <- "15"
P6B$P6Q4_2[P6B$P6Q4_R_15 == "2"] <- "15"
P6B$P6Q4_3[P6B$P6Q4_R_15 == "3"] <- "15"

P6B$P6Q4_1[P6B$P6Q4_R_16 == "1"] <- "16"
P6B$P6Q4_2[P6B$P6Q4_R_16 == "2"] <- "16"
P6B$P6Q4_3[P6B$P6Q4_R_16 == "3"] <- "16"

##################################################
# P6A$P6Q6_1
# P6A$P6Q6_2
# P6A$P6Q6_3

P6A$P6Q6_1[P6A$P6Q6_R_1 == "1"] <- "1"
P6A$P6Q6_2[P6A$P6Q6_R_1 == "2"] <- "1"
P6A$P6Q6_3[P6A$P6Q6_R_1 == "3"] <- "1"

P6A$P6Q6_1[P6A$P6Q6_R_2 == "1"] <- "2"
P6A$P6Q6_2[P6A$P6Q6_R_2 == "2"] <- "2"
P6A$P6Q6_3[P6A$P6Q6_R_2 == "3"] <- "2"

P6A$P6Q6_1[P6A$P6Q6_R_3 == "1"] <- "3"
P6A$P6Q6_2[P6A$P6Q6_R_3 == "2"] <- "3"
P6A$P6Q6_3[P6A$P6Q6_R_3 == "3"] <- "3"

P6A$P6Q6_1[P6A$P6Q6_R_4 == "1"] <- "4"
P6A$P6Q6_2[P6A$P6Q6_R_4 == "2"] <- "4"
P6A$P6Q6_3[P6A$P6Q6_R_4 == "3"] <- "4"

P6A$P6Q6_1[P6A$P6Q6_R_5 == "1"] <- "5"
P6A$P6Q6_2[P6A$P6Q6_R_5 == "2"] <- "5"
P6A$P6Q6_3[P6A$P6Q6_R_5 == "3"] <- "5"

P6A$P6Q6_1[P6A$P6Q6_R_6 == "1"] <- "6"
P6A$P6Q6_2[P6A$P6Q6_R_6 == "2"] <- "6"
P6A$P6Q6_3[P6A$P6Q6_R_6 == "3"] <- "6"

P6A$P6Q6_1[P6A$P6Q6_R_7 == "1"] <- "7"
P6A$P6Q6_2[P6A$P6Q6_R_7 == "2"] <- "7"
P6A$P6Q6_3[P6A$P6Q6_R_7 == "3"] <- "7"

P6A$P6Q6_1[P6A$P6Q6_R_8 == "1"] <- "8"
P6A$P6Q6_2[P6A$P6Q6_R_8 == "2"] <- "8"
P6A$P6Q6_3[P6A$P6Q6_R_8 == "3"] <- "8"

P6A$P6Q6_1[P6A$P6Q6_R_9 == "1"] <- "9"
P6A$P6Q6_2[P6A$P6Q6_R_9 == "2"] <- "9"
P6A$P6Q6_3[P6A$P6Q6_R_9 == "3"] <- "9"

P6A$P6Q6_1[P6A$P6Q6_R_10 == "1"] <- "10"
P6A$P6Q6_2[P6A$P6Q6_R_10 == "2"] <- "10"
P6A$P6Q6_3[P6A$P6Q6_R_10 == "3"] <- "10"

##################################################
# P6B$P6Q6_1
# P6B$P6Q6_2
# P6B$P6Q6_3

P6B$P6Q6_1[P6B$P6Q6_R_1 == "1"] <- "1"
P6B$P6Q6_2[P6B$P6Q6_R_1 == "2"] <- "1"
P6B$P6Q6_3[P6B$P6Q6_R_1 == "3"] <- "1"

P6B$P6Q6_1[P6B$P6Q6_R_2 == "1"] <- "2"
P6B$P6Q6_2[P6B$P6Q6_R_2 == "2"] <- "2"
P6B$P6Q6_3[P6B$P6Q6_R_2 == "3"] <- "2"

P6B$P6Q6_1[P6B$P6Q6_R_3 == "1"] <- "3"
P6B$P6Q6_2[P6B$P6Q6_R_3 == "2"] <- "3"
P6B$P6Q6_3[P6B$P6Q6_R_3 == "3"] <- "3"

P6B$P6Q6_1[P6B$P6Q6_R_4 == "1"] <- "4"
P6B$P6Q6_2[P6B$P6Q6_R_4 == "2"] <- "4"
P6B$P6Q6_3[P6B$P6Q6_R_4 == "3"] <- "4"

P6B$P6Q6_1[P6B$P6Q6_R_5 == "1"] <- "5"
P6B$P6Q6_2[P6B$P6Q6_R_5 == "2"] <- "5"
P6B$P6Q6_3[P6B$P6Q6_R_5 == "3"] <- "5"

P6B$P6Q6_1[P6B$P6Q6_R_6 == "1"] <- "6"
P6B$P6Q6_2[P6B$P6Q6_R_6 == "2"] <- "6"
P6B$P6Q6_3[P6B$P6Q6_R_6 == "3"] <- "6"

P6B$P6Q6_1[P6B$P6Q6_R_7 == "1"] <- "7"
P6B$P6Q6_2[P6B$P6Q6_R_7 == "2"] <- "7"
P6B$P6Q6_3[P6B$P6Q6_R_7 == "3"] <- "7"

P6B$P6Q6_1[P6B$P6Q6_R_8 == "1"] <- "8"
P6B$P6Q6_2[P6B$P6Q6_R_8 == "2"] <- "8"
P6B$P6Q6_3[P6B$P6Q6_R_8 == "3"] <- "8"

P6B$P6Q6_1[P6B$P6Q6_R_9 == "1"] <- "9"
P6B$P6Q6_2[P6B$P6Q6_R_9 == "2"] <- "9"
P6B$P6Q6_3[P6B$P6Q6_R_9 == "3"] <- "9"

P6B$P6Q6_1[P6B$P6Q6_R_10 == "1"] <- "10"
P6B$P6Q6_2[P6B$P6Q6_R_10 == "2"] <- "10"
P6B$P6Q6_3[P6B$P6Q6_R_10 == "3"] <- "10"

##################################################
# P6A$P6Q19_1
# P6A$P6Q19_2
# P6A$P6Q19_3
# P6A$P6Q19_4
# P6A$P6Q19_5
# P6A$P6Q19_7

P6A$P6Q19_1 <- P6A$P6Q19_Nestle
P6A$P6Q19_2 <- P6A$P6Q19_Centrum
P6A$P6Q19_3 <- P6A$P6Q19_Abbott
P6A$P6Q19_4 <- P6A$P6Q19_Mamacare
P6A$P6Q19_5 <- P6A$P6Q19_Blackmores
P6A$P6Q19_7 <- P6A$P6Q19_GNC

##################################################

P6B$P6Q19_1 <- P6B$P6Q19_Nestle
P6B$P6Q19_2 <- P6B$P6Q19_Centrum
P6B$P6Q19_3 <- P6B$P6Q19_Abbott
P6B$P6Q19_4 <- P6B$P6Q19_Mamacare
P6B$P6Q19_5 <- P6B$P6Q19_Blackmores
P6B$P6Q19_7 <- P6B$P6Q19_GNC

##################################################

P6A$P6Q20_11 <- P6A$P6Q20_NOW
P6A$P6Q20_12 <- P6A$P6Q20_Wyeth
P6A$P6Q20_13 <- P6A$P6Q20_Horeb
P6A$P6Q20_14 <- P6A$P6Q20_MJN
P6A$P6Q20_15 <- P6A$P6Q20_Nolbel
P6A$P6Q20_16 <- P6A$P6Q20_Mamacare

##################################################

P6B$P6Q20_11 <- P6B$P6Q20_NOW
P6B$P6Q20_12 <- P6B$P6Q20_Wyeth
P6B$P6Q20_13 <- P6B$P6Q20_Horeb
P6B$P6Q20_14 <- P6B$P6Q20_MJN
P6B$P6Q20_15 <- P6B$P6Q20_Nolbel
P6B$P6Q20_16 <- P6B$P6Q20_Mamacare

##################################################

P6A$P6Q21_1 <- P6A$P6Q21_MJN
P6A$P6Q21_2 <- P6A$P6Q21_P6Q21_Abbott
P6A$P6Q21_3 <- P6A$P6Q21_P6Q21_Meiji
P6A$P6Q21_4 <- P6A$P6Q21_P6Q21_Anmum
P6A$P6Q21_5 <- P6A$P6Q21_P6Q21_Quaker
P6A$P6Q21_6 <- P6A$P6Q21_P6Q21_Neoangelac

##################################################

P6B$P6Q21_1 <- P6B$P6Q21_MJN
P6B$P6Q21_2 <- P6B$P6Q21_P6Q21_Abbott
P6B$P6Q21_3 <- P6B$P6Q21_P6Q21_Meiji
P6B$P6Q21_4 <- P6B$P6Q21_P6Q21_Anmum
P6B$P6Q21_5 <- P6B$P6Q21_P6Q21_Quaker
P6B$P6Q21_6 <- P6B$P6Q21_P6Q21_Neoangelac

##################################################

P6A$P6Q22
P6B$P6Q22

##################################################

P6A$P6Q23
P6B$P6Q23

##################################################
# P6A$P6Q24_rank

P6A$P6Q24_rank1[P6A$P6Q24_Nestle == "1"] <- "1"
P6A$P6Q24_rank2[P6A$P6Q24_Nestle == "2"] <- "1"
P6A$P6Q24_rank3[P6A$P6Q24_Nestle == "3"] <- "1"
P6A$P6Q24_rank4[P6A$P6Q24_Nestle == "4"] <- "1"
P6A$P6Q24_rank5[P6A$P6Q24_Nestle == "5"] <- "1"
P6A$P6Q24_rank6[P6A$P6Q24_Nestle == "6"] <- "1"
P6A$P6Q24_rank7[P6A$P6Q24_Nestle == "7"] <- "1"

P6A$P6Q24_rank1[P6A$P6Q24_Wyeth == "1"] <- "2"
P6A$P6Q24_rank2[P6A$P6Q24_Wyeth == "2"] <- "2"
P6A$P6Q24_rank3[P6A$P6Q24_Wyeth == "3"] <- "2"
P6A$P6Q24_rank4[P6A$P6Q24_Wyeth == "4"] <- "2"
P6A$P6Q24_rank5[P6A$P6Q24_Wyeth == "5"] <- "2"
P6A$P6Q24_rank6[P6A$P6Q24_Wyeth == "6"] <- "2"
P6A$P6Q24_rank7[P6A$P6Q24_Wyeth == "7"] <- "2"

P6A$P6Q24_rank1[P6A$P6Q24_MJN == "1"] <- "3"
P6A$P6Q24_rank2[P6A$P6Q24_MJN == "2"] <- "3"
P6A$P6Q24_rank3[P6A$P6Q24_MJN == "3"] <- "3"
P6A$P6Q24_rank4[P6A$P6Q24_MJN == "4"] <- "3"
P6A$P6Q24_rank5[P6A$P6Q24_MJN == "5"] <- "3"
P6A$P6Q24_rank6[P6A$P6Q24_MJN == "6"] <- "3"
P6A$P6Q24_rank7[P6A$P6Q24_MJN == "7"] <- "3"

P6A$P6Q24_rank1[P6A$P6Q24_Abbott == "1"] <- "4"
P6A$P6Q24_rank2[P6A$P6Q24_Abbott == "2"] <- "4"
P6A$P6Q24_rank3[P6A$P6Q24_Abbott == "3"] <- "4"
P6A$P6Q24_rank4[P6A$P6Q24_Abbott == "4"] <- "4"
P6A$P6Q24_rank5[P6A$P6Q24_Abbott == "5"] <- "4"
P6A$P6Q24_rank6[P6A$P6Q24_Abbott == "6"] <- "4"
P6A$P6Q24_rank7[P6A$P6Q24_Abbott == "7"] <- "4"

P6A$P6Q24_rank1[P6A$P6Q24_Meiji == "1"] <- "5"
P6A$P6Q24_rank2[P6A$P6Q24_Meiji == "2"] <- "5"
P6A$P6Q24_rank3[P6A$P6Q24_Meiji == "3"] <- "5"
P6A$P6Q24_rank4[P6A$P6Q24_Meiji == "4"] <- "5"
P6A$P6Q24_rank5[P6A$P6Q24_Meiji == "5"] <- "5"
P6A$P6Q24_rank6[P6A$P6Q24_Meiji == "6"] <- "5"
P6A$P6Q24_rank7[P6A$P6Q24_Meiji == "7"] <- "5"

P6A$P6Q24_rank1[P6A$P6Q24_Quaker == "1"] <- "6"
P6A$P6Q24_rank2[P6A$P6Q24_Quaker == "2"] <- "6"
P6A$P6Q24_rank3[P6A$P6Q24_Quaker == "3"] <- "6"
P6A$P6Q24_rank4[P6A$P6Q24_Quaker == "4"] <- "6"
P6A$P6Q24_rank5[P6A$P6Q24_Quaker == "5"] <- "6"
P6A$P6Q24_rank6[P6A$P6Q24_Quaker == "6"] <- "6"
P6A$P6Q24_rank7[P6A$P6Q24_Quaker == "7"] <- "6"

P6A$P6Q24_rank1[P6A$P6Q24_Snow == "1"] <- "7"
P6A$P6Q24_rank2[P6A$P6Q24_Snow == "2"] <- "7"
P6A$P6Q24_rank3[P6A$P6Q24_Snow == "3"] <- "7"
P6A$P6Q24_rank4[P6A$P6Q24_Snow == "4"] <- "7"
P6A$P6Q24_rank5[P6A$P6Q24_Snow == "5"] <- "7"
P6A$P6Q24_rank6[P6A$P6Q24_Snow == "6"] <- "7"
P6A$P6Q24_rank7[P6A$P6Q24_Snow == "7"] <- "7"

# P6A[ , c("P6Q24_Nestle", "P6Q24_Wyeth", "P6Q24_MJN", "P6Q24_Abbott", "P6Q24_Meiji", "P6Q24_Quaker", "P6Q24_Snow")]
# P6A[order(P6A[, c("P6Q24_Nestle", "P6Q24_Wyeth", "P6Q24_MJN", "P6Q24_Abbott", "P6Q24_Meiji", "P6Q24_Quaker", "P6Q24_Snow")], decreasing = TRUE), ]
# A[order(A[,4],decreasing=T),]
# P6A %>% 
#   group_by(P6Q24_Nestle, P6Q24_Wyeth, P6Q24_MJN, P6Q24_Abbott, P6Q24_Meiji, P6Q24_Quaker, P6Q24_Snow) %>%
#   arrange() %>% 
#   # dplyr::filter(P6Q24_Nestle = 1 | P6Q24_Wyeth = 1 | P6Q24_MJN = 1 | P6Q24_Abbott = 1 | P6Q24_Meiji = 1 | P6Q24_Quaker = 1 | P6Q24_Snow = 1) %>% 
#   # dplyr::filter(%in% c("1")) %>% 
#   summarise(P6Q24_rank1, P6Q24_rank2, P6Q24_rank3, P6Q24_rank4, P6Q24_rank5, P6Q24_rank6, P6Q24_rank7) %>% 
#   head(10)

# P6AP6Q24_TABLE <- P6A[ , c("P6Q24_Nestle", "P6Q24_Wyeth", "P6Q24_MJN", "P6Q24_Abbott", "P6Q24_Meiji", "P6Q24_Quaker", "P6Q24_Snow")]

# P6AP6Q24_TABLE %>% 
#   apply(1, sort) %>% 
#   head(10)

# P6A %>% 
#   select(P6Q24_Nestle, P6Q24_Wyeth, P6Q24_MJN, P6Q24_Abbott, P6Q24_Meiji, P6Q24_Quaker, P6Q24_Snow) %>% 
#   as.data.frame() %>% 
#   apply(1, sort) %>% 
#   head(10) 

# P6A$P6Q24_order_table <- apply(P6A[ , c("P6Q24_Nestle", "P6Q24_Wyeth", "P6Q24_MJN", "P6Q24_Abbott", "P6Q24_Meiji", "P6Q24_Quaker", "P6Q24_Snow")], 1, order)

# P6A[ , c(P6A$P6Q24_Nestle, P6A$P6Q24_Wyeth, P6A$P6Q24_MJN, P6A$P6Q24_Abbott, P6A$P6Q24_Meiji, P6A$P6Q24_Quaker, P6A$P6Q24_Snow)]
# P6A[ , c("P6Q24_Nestle", "P6Q24_Wyeth", "P6Q24_MJN", "P6Q24_Abbott", "P6Q24_Meiji", "P6Q24_Quaker", "P6Q24_Snow")]

# P6A$P6Q24_rank1 <- apply(P6A[ , c("P6Q24_Nestle", "P6Q24_Wyeth", "P6Q24_MJN", "P6Q24_Abbott", "P6Q24_Meiji", "P6Q24_Quaker", "P6Q24_Snow")], 1, value == 1)

# P6A$P6Q24_rank1 <- P6A %>% 
#   summarise(P6A[ , c("P6Q24_Nestle", "P6Q24_Wyeth", "P6Q24_MJN", "P6Q24_Abbott", "P6Q24_Meiji", "P6Q24_Quaker", "P6Q24_Snow")])

# P6A[ , c(P6A$P6Q24_Nestle, P6A$P6Q24_Wyeth, P6A$P6Q24_MJN, P6A$P6Q24_Abbott, P6A$P6Q24_Meiji, P6A$P6Q24_Quaker, P6A$P6Q24_Snow)]

# function_P6Q24 <- function() {
#   P6A$P6Q24_rank1 <- apply(P6A[ , c("P6Q24_Nestle", "P6Q24_Wyeth", "P6Q24_MJN", "P6Q24_Abbott", "P6Q24_Meiji", "P6Q24_Quaker", "P6Q24_Snow")], 1, which(P6A[ , c("P6Q24_Nestle", "P6Q24_Wyeth", "P6Q24_MJN", "P6Q24_Abbott", "P6Q24_Meiji", "P6Q24_Quaker", "P6Q24_Snow")]) == 1),
# }

##################################################
# P6B$P6Q24_rank

P6B$P6Q24_rank1[P6B$P6Q24_Nestle == "1"] <- "1"
P6B$P6Q24_rank2[P6B$P6Q24_Nestle == "2"] <- "1"
P6B$P6Q24_rank3[P6B$P6Q24_Nestle == "3"] <- "1"
P6B$P6Q24_rank4[P6B$P6Q24_Nestle == "4"] <- "1"
P6B$P6Q24_rank5[P6B$P6Q24_Nestle == "5"] <- "1"
P6B$P6Q24_rank6[P6B$P6Q24_Nestle == "6"] <- "1"
P6B$P6Q24_rank7[P6B$P6Q24_Nestle == "7"] <- "1"

P6B$P6Q24_rank1[P6B$P6Q24_Wyeth == "1"] <- "2"
P6B$P6Q24_rank2[P6B$P6Q24_Wyeth == "2"] <- "2"
P6B$P6Q24_rank3[P6B$P6Q24_Wyeth == "3"] <- "2"
P6B$P6Q24_rank4[P6B$P6Q24_Wyeth == "4"] <- "2"
P6B$P6Q24_rank5[P6B$P6Q24_Wyeth == "5"] <- "2"
P6B$P6Q24_rank6[P6B$P6Q24_Wyeth == "6"] <- "2"
P6B$P6Q24_rank7[P6B$P6Q24_Wyeth == "7"] <- "2"

P6B$P6Q24_rank1[P6B$P6Q24_MJN == "1"] <- "3"
P6B$P6Q24_rank2[P6B$P6Q24_MJN == "2"] <- "3"
P6B$P6Q24_rank3[P6B$P6Q24_MJN == "3"] <- "3"
P6B$P6Q24_rank4[P6B$P6Q24_MJN == "4"] <- "3"
P6B$P6Q24_rank5[P6B$P6Q24_MJN == "5"] <- "3"
P6B$P6Q24_rank6[P6B$P6Q24_MJN == "6"] <- "3"
P6B$P6Q24_rank7[P6B$P6Q24_MJN == "7"] <- "3"

P6B$P6Q24_rank1[P6B$P6Q24_Abbott == "1"] <- "4"
P6B$P6Q24_rank2[P6B$P6Q24_Abbott == "2"] <- "4"
P6B$P6Q24_rank3[P6B$P6Q24_Abbott == "3"] <- "4"
P6B$P6Q24_rank4[P6B$P6Q24_Abbott == "4"] <- "4"
P6B$P6Q24_rank5[P6B$P6Q24_Abbott == "5"] <- "4"
P6B$P6Q24_rank6[P6B$P6Q24_Abbott == "6"] <- "4"
P6B$P6Q24_rank7[P6B$P6Q24_Abbott == "7"] <- "4"

P6B$P6Q24_rank1[P6B$P6Q24_Meiji == "1"] <- "5"
P6B$P6Q24_rank2[P6B$P6Q24_Meiji == "2"] <- "5"
P6B$P6Q24_rank3[P6B$P6Q24_Meiji == "3"] <- "5"
P6B$P6Q24_rank4[P6B$P6Q24_Meiji == "4"] <- "5"
P6B$P6Q24_rank5[P6B$P6Q24_Meiji == "5"] <- "5"
P6B$P6Q24_rank6[P6B$P6Q24_Meiji == "6"] <- "5"
P6B$P6Q24_rank7[P6B$P6Q24_Meiji == "7"] <- "5"

P6B$P6Q24_rank1[P6B$P6Q24_Quaker == "1"] <- "6"
P6B$P6Q24_rank2[P6B$P6Q24_Quaker == "2"] <- "6"
P6B$P6Q24_rank3[P6B$P6Q24_Quaker == "3"] <- "6"
P6B$P6Q24_rank4[P6B$P6Q24_Quaker == "4"] <- "6"
P6B$P6Q24_rank5[P6B$P6Q24_Quaker == "5"] <- "6"
P6B$P6Q24_rank6[P6B$P6Q24_Quaker == "6"] <- "6"
P6B$P6Q24_rank7[P6B$P6Q24_Quaker == "7"] <- "6"

##################################################

P6A$P6Q25
P6B$P6Q25

##################################################

P6A$P6Q26_Nestle
P6A$P6Q26_Wyeth
P6A$P6Q26_MJN
P6A$P6Q26_Abbott
P6A$P6Q26_Meiji
P6A$P6Q26_Quaker
P6A$P6Q26_Snow
P6A$P6Q26_Karihome
P6A$P6Q26_Babecare
P6A$P6Q26_Neoangelac

##################################################

P6B$P6Q26_Nestle
P6B$P6Q26_Wyeth
P6B$P6Q26_MJN
P6B$P6Q26_Abbott
P6B$P6Q26_Meiji
P6B$P6Q26_Quaker
P6B$P6Q26_Snow
P6B$P6Q26_Karihome
P6B$P6Q26_Babecare
P6B$P6Q26_Neoangelac

##################################################

P6A$P6Q27_rank1[P6A$P6Q27_Nestle == "1"] <- "1"
P6A$P6Q27_rank2[P6A$P6Q27_Nestle == "2"] <- "1"
P6A$P6Q27_rank3[P6A$P6Q27_Nestle == "3"] <- "1"
P6A$P6Q27_rank4[P6A$P6Q27_Nestle == "4"] <- "1"
P6A$P6Q27_rank5[P6A$P6Q27_Nestle == "5"] <- "1"
P6A$P6Q27_rank6[P6A$P6Q27_Nestle == "6"] <- "1"
P6A$P6Q27_rank7[P6A$P6Q27_Nestle == "7"] <- "1"
P6A$P6Q27_rank8[P6A$P6Q27_Nestle == "8"] <- "1"
P6A$P6Q27_rank9[P6A$P6Q27_Nestle == "9"] <- "1"
P6A$P6Q27_rank10[P6A$P6Q27_Nestle == "10"] <- "1"

P6A$P6Q27_rank1[P6A$P6Q27_Wyeth == "1"] <- "2"
P6A$P6Q27_rank2[P6A$P6Q27_Wyeth == "2"] <- "2"
P6A$P6Q27_rank3[P6A$P6Q27_Wyeth == "3"] <- "2"
P6A$P6Q27_rank4[P6A$P6Q27_Wyeth == "4"] <- "2"
P6A$P6Q27_rank5[P6A$P6Q27_Wyeth == "5"] <- "2"
P6A$P6Q27_rank6[P6A$P6Q27_Wyeth == "6"] <- "2"
P6A$P6Q27_rank7[P6A$P6Q27_Wyeth == "7"] <- "2"
P6A$P6Q27_rank8[P6A$P6Q27_Wyeth == "8"] <- "2"
P6A$P6Q27_rank9[P6A$P6Q27_Wyeth == "9"] <- "2"
P6A$P6Q27_rank10[P6A$P6Q27_Wyeth == "10"] <- "2"

P6A$P6Q27_rank1[P6A$P6Q27_MJN == "1"] <- "3"
P6A$P6Q27_rank2[P6A$P6Q27_MJN == "2"] <- "3"
P6A$P6Q27_rank3[P6A$P6Q27_MJN == "3"] <- "3"
P6A$P6Q27_rank4[P6A$P6Q27_MJN == "4"] <- "3"
P6A$P6Q27_rank5[P6A$P6Q27_MJN == "5"] <- "3"
P6A$P6Q27_rank6[P6A$P6Q27_MJN == "6"] <- "3"
P6A$P6Q27_rank7[P6A$P6Q27_MJN == "7"] <- "3"
P6A$P6Q27_rank8[P6A$P6Q27_MJN == "8"] <- "3"
P6A$P6Q27_rank9[P6A$P6Q27_MJN == "9"] <- "3"
P6A$P6Q27_rank10[P6A$P6Q27_MJN == "10"] <- "3"

P6A$P6Q27_rank1[P6A$P6Q27_Abbott == "1"] <- "4"
P6A$P6Q27_rank2[P6A$P6Q27_Abbott == "2"] <- "4"
P6A$P6Q27_rank3[P6A$P6Q27_Abbott == "3"] <- "4"
P6A$P6Q27_rank4[P6A$P6Q27_Abbott == "4"] <- "4"
P6A$P6Q27_rank5[P6A$P6Q27_Abbott == "5"] <- "4"
P6A$P6Q27_rank6[P6A$P6Q27_Abbott == "6"] <- "4"
P6A$P6Q27_rank7[P6A$P6Q27_Abbott == "7"] <- "4"
P6A$P6Q27_rank8[P6A$P6Q27_Abbott == "8"] <- "4"
P6A$P6Q27_rank9[P6A$P6Q27_Abbott == "9"] <- "4"
P6A$P6Q27_rank10[P6A$P6Q27_Abbott == "10"] <- "4"

P6A$P6Q27_rank1[P6A$P6Q27_Meiji == "1"] <- "5"
P6A$P6Q27_rank2[P6A$P6Q27_Meiji == "2"] <- "5"
P6A$P6Q27_rank3[P6A$P6Q27_Meiji == "3"] <- "5"
P6A$P6Q27_rank4[P6A$P6Q27_Meiji == "4"] <- "5"
P6A$P6Q27_rank5[P6A$P6Q27_Meiji == "5"] <- "5"
P6A$P6Q27_rank6[P6A$P6Q27_Meiji == "6"] <- "5"
P6A$P6Q27_rank7[P6A$P6Q27_Meiji == "7"] <- "5"
P6A$P6Q27_rank8[P6A$P6Q27_Meiji == "8"] <- "5"
P6A$P6Q27_rank9[P6A$P6Q27_Meiji == "9"] <- "5"
P6A$P6Q27_rank10[P6A$P6Q27_Meiji == "10"] <- "5"

P6A$P6Q27_rank1[P6A$P6Q27_Quaker == "1"] <- "6"
P6A$P6Q27_rank2[P6A$P6Q27_Quaker == "2"] <- "6"
P6A$P6Q27_rank3[P6A$P6Q27_Quaker == "3"] <- "6"
P6A$P6Q27_rank4[P6A$P6Q27_Quaker == "4"] <- "6"
P6A$P6Q27_rank5[P6A$P6Q27_Quaker == "5"] <- "6"
P6A$P6Q27_rank6[P6A$P6Q27_Quaker == "6"] <- "6"
P6A$P6Q27_rank7[P6A$P6Q27_Quaker == "7"] <- "6"
P6A$P6Q27_rank8[P6A$P6Q27_Quaker == "8"] <- "6"
P6A$P6Q27_rank9[P6A$P6Q27_Quaker == "9"] <- "6"
P6A$P6Q27_rank10[P6A$P6Q27_Quaker == "10"] <- "6"

P6A$P6Q27_rank1[P6A$P6Q27_Snow == "1"] <- "7"
P6A$P6Q27_rank2[P6A$P6Q27_Snow == "2"] <- "7"
P6A$P6Q27_rank3[P6A$P6Q27_Snow == "3"] <- "7"
P6A$P6Q27_rank4[P6A$P6Q27_Snow == "4"] <- "7"
P6A$P6Q27_rank5[P6A$P6Q27_Snow == "5"] <- "7"
P6A$P6Q27_rank6[P6A$P6Q27_Snow == "6"] <- "7"
P6A$P6Q27_rank7[P6A$P6Q27_Snow == "7"] <- "7"
P6A$P6Q27_rank8[P6A$P6Q27_Snow == "8"] <- "7"
P6A$P6Q27_rank9[P6A$P6Q27_Snow == "9"] <- "7"
P6A$P6Q27_rank10[P6A$P6Q27_Snow == "10"] <- "7"

P6A$P6Q27_rank1[P6A$P6Q27_Karihome == "1"] <- "8"
P6A$P6Q27_rank2[P6A$P6Q27_Karihome == "2"] <- "8"
P6A$P6Q27_rank3[P6A$P6Q27_Karihome == "3"] <- "8"
P6A$P6Q27_rank4[P6A$P6Q27_Karihome == "4"] <- "8"
P6A$P6Q27_rank5[P6A$P6Q27_Karihome == "5"] <- "8"
P6A$P6Q27_rank6[P6A$P6Q27_Karihome == "6"] <- "8"
P6A$P6Q27_rank7[P6A$P6Q27_Karihome == "7"] <- "8"
P6A$P6Q27_rank8[P6A$P6Q27_Karihome == "8"] <- "8"
P6A$P6Q27_rank9[P6A$P6Q27_Karihome == "9"] <- "8"
P6A$P6Q27_rank10[P6A$P6Q27_Karihome == "10"] <- "8"

P6A$P6Q27_rank1[P6A$P6Q27_Babecare == "1"] <- "9"
P6A$P6Q27_rank2[P6A$P6Q27_Babecare == "2"] <- "9"
P6A$P6Q27_rank3[P6A$P6Q27_Babecare == "3"] <- "9"
P6A$P6Q27_rank4[P6A$P6Q27_Babecare == "4"] <- "9"
P6A$P6Q27_rank5[P6A$P6Q27_Babecare == "5"] <- "9"
P6A$P6Q27_rank6[P6A$P6Q27_Babecare == "6"] <- "9"
P6A$P6Q27_rank7[P6A$P6Q27_Babecare == "7"] <- "9"
P6A$P6Q27_rank8[P6A$P6Q27_Babecare == "8"] <- "9"
P6A$P6Q27_rank9[P6A$P6Q27_Babecare == "9"] <- "9"
P6A$P6Q27_rank10[P6A$P6Q27_Babecare == "10"] <- "9"

P6A$P6Q27_rank1[P6A$P6Q27_Neoangelac == "1"] <- "10"
P6A$P6Q27_rank2[P6A$P6Q27_Neoangelac == "2"] <- "10"
P6A$P6Q27_rank3[P6A$P6Q27_Neoangelac == "3"] <- "10"
P6A$P6Q27_rank4[P6A$P6Q27_Neoangelac == "4"] <- "10"
P6A$P6Q27_rank5[P6A$P6Q27_Neoangelac == "5"] <- "10"
P6A$P6Q27_rank6[P6A$P6Q27_Neoangelac == "6"] <- "10"
P6A$P6Q27_rank7[P6A$P6Q27_Neoangelac == "7"] <- "10"
P6A$P6Q27_rank8[P6A$P6Q27_Neoangelac == "8"] <- "10"
P6A$P6Q27_rank9[P6A$P6Q27_Neoangelac == "9"] <- "10"
P6A$P6Q27_rank10[P6A$P6Q27_Neoangelac == "10"] <- "10"

##################################################

P6B$P6Q27_rank1[P6B$P6Q27_Nestle == "1"] <- "1"
P6B$P6Q27_rank2[P6B$P6Q27_Nestle == "2"] <- "1"
P6B$P6Q27_rank3[P6B$P6Q27_Nestle == "3"] <- "1"
P6B$P6Q27_rank4[P6B$P6Q27_Nestle == "4"] <- "1"
P6B$P6Q27_rank5[P6B$P6Q27_Nestle == "5"] <- "1"
P6B$P6Q27_rank6[P6B$P6Q27_Nestle == "6"] <- "1"
P6B$P6Q27_rank7[P6B$P6Q27_Nestle == "7"] <- "1"
P6B$P6Q27_rank8[P6B$P6Q27_Nestle == "8"] <- "1"
P6B$P6Q27_rank9[P6B$P6Q27_Nestle == "9"] <- "1"
P6B$P6Q27_rank10[P6B$P6Q27_Nestle == "10"] <- "1"

P6B$P6Q27_rank1[P6B$P6Q27_Wyeth == "1"] <- "2"
P6B$P6Q27_rank2[P6B$P6Q27_Wyeth == "2"] <- "2"
P6B$P6Q27_rank3[P6B$P6Q27_Wyeth == "3"] <- "2"
P6B$P6Q27_rank4[P6B$P6Q27_Wyeth == "4"] <- "2"
P6B$P6Q27_rank5[P6B$P6Q27_Wyeth == "5"] <- "2"
P6B$P6Q27_rank6[P6B$P6Q27_Wyeth == "6"] <- "2"
P6B$P6Q27_rank7[P6B$P6Q27_Wyeth == "7"] <- "2"
P6B$P6Q27_rank8[P6B$P6Q27_Wyeth == "8"] <- "2"
P6B$P6Q27_rank9[P6B$P6Q27_Wyeth == "9"] <- "2"
P6B$P6Q27_rank10[P6B$P6Q27_Wyeth == "10"] <- "2"

P6B$P6Q27_rank1[P6B$P6Q27_MJN == "1"] <- "3"
P6B$P6Q27_rank2[P6B$P6Q27_MJN == "2"] <- "3"
P6B$P6Q27_rank3[P6B$P6Q27_MJN == "3"] <- "3"
P6B$P6Q27_rank4[P6B$P6Q27_MJN == "4"] <- "3"
P6B$P6Q27_rank5[P6B$P6Q27_MJN == "5"] <- "3"
P6B$P6Q27_rank6[P6B$P6Q27_MJN == "6"] <- "3"
P6B$P6Q27_rank7[P6B$P6Q27_MJN == "7"] <- "3"
P6B$P6Q27_rank8[P6B$P6Q27_MJN == "8"] <- "3"
P6B$P6Q27_rank9[P6B$P6Q27_MJN == "9"] <- "3"
P6B$P6Q27_rank10[P6B$P6Q27_MJN == "10"] <- "3"

P6B$P6Q27_rank1[P6B$P6Q27_Abbott == "1"] <- "4"
P6B$P6Q27_rank2[P6B$P6Q27_Abbott == "2"] <- "4"
P6B$P6Q27_rank3[P6B$P6Q27_Abbott == "3"] <- "4"
P6B$P6Q27_rank4[P6B$P6Q27_Abbott == "4"] <- "4"
P6B$P6Q27_rank5[P6B$P6Q27_Abbott == "5"] <- "4"
P6B$P6Q27_rank6[P6B$P6Q27_Abbott == "6"] <- "4"
P6B$P6Q27_rank7[P6B$P6Q27_Abbott == "7"] <- "4"
P6B$P6Q27_rank8[P6B$P6Q27_Abbott == "8"] <- "4"
P6B$P6Q27_rank9[P6B$P6Q27_Abbott == "9"] <- "4"
P6B$P6Q27_rank10[P6B$P6Q27_Abbott == "10"] <- "4"

P6B$P6Q27_rank1[P6B$P6Q27_Meiji == "1"] <- "5"
P6B$P6Q27_rank2[P6B$P6Q27_Meiji == "2"] <- "5"
P6B$P6Q27_rank3[P6B$P6Q27_Meiji == "3"] <- "5"
P6B$P6Q27_rank4[P6B$P6Q27_Meiji == "4"] <- "5"
P6B$P6Q27_rank5[P6B$P6Q27_Meiji == "5"] <- "5"
P6B$P6Q27_rank6[P6B$P6Q27_Meiji == "6"] <- "5"
P6B$P6Q27_rank7[P6B$P6Q27_Meiji == "7"] <- "5"
P6B$P6Q27_rank8[P6B$P6Q27_Meiji == "8"] <- "5"
P6B$P6Q27_rank9[P6B$P6Q27_Meiji == "9"] <- "5"
P6B$P6Q27_rank10[P6B$P6Q27_Meiji == "10"] <- "5"

P6B$P6Q27_rank1[P6B$P6Q27_Quaker == "1"] <- "6"
P6B$P6Q27_rank2[P6B$P6Q27_Quaker == "2"] <- "6"
P6B$P6Q27_rank3[P6B$P6Q27_Quaker == "3"] <- "6"
P6B$P6Q27_rank4[P6B$P6Q27_Quaker == "4"] <- "6"
P6B$P6Q27_rank5[P6B$P6Q27_Quaker == "5"] <- "6"
P6B$P6Q27_rank6[P6B$P6Q27_Quaker == "6"] <- "6"
P6B$P6Q27_rank7[P6B$P6Q27_Quaker == "7"] <- "6"
P6B$P6Q27_rank8[P6B$P6Q27_Quaker == "8"] <- "6"
P6B$P6Q27_rank9[P6B$P6Q27_Quaker == "9"] <- "6"
P6B$P6Q27_rank10[P6B$P6Q27_Quaker == "10"] <- "6"

P6B$P6Q27_rank1[P6B$P6Q27_Snow == "1"] <- "7"
P6B$P6Q27_rank2[P6B$P6Q27_Snow == "2"] <- "7"
P6B$P6Q27_rank3[P6B$P6Q27_Snow == "3"] <- "7"
P6B$P6Q27_rank4[P6B$P6Q27_Snow == "4"] <- "7"
P6B$P6Q27_rank5[P6B$P6Q27_Snow == "5"] <- "7"
P6B$P6Q27_rank6[P6B$P6Q27_Snow == "6"] <- "7"
P6B$P6Q27_rank7[P6B$P6Q27_Snow == "7"] <- "7"
P6B$P6Q27_rank8[P6B$P6Q27_Snow == "8"] <- "7"
P6B$P6Q27_rank9[P6B$P6Q27_Snow == "9"] <- "7"
P6B$P6Q27_rank10[P6B$P6Q27_Snow == "10"] <- "7"

P6B$P6Q27_rank1[P6B$P6Q27_Karihome == "1"] <- "8"
P6B$P6Q27_rank2[P6B$P6Q27_Karihome == "2"] <- "8"
P6B$P6Q27_rank3[P6B$P6Q27_Karihome == "3"] <- "8"
P6B$P6Q27_rank4[P6B$P6Q27_Karihome == "4"] <- "8"
P6B$P6Q27_rank5[P6B$P6Q27_Karihome == "5"] <- "8"
P6B$P6Q27_rank6[P6B$P6Q27_Karihome == "6"] <- "8"
P6B$P6Q27_rank7[P6B$P6Q27_Karihome == "7"] <- "8"
P6B$P6Q27_rank8[P6B$P6Q27_Karihome == "8"] <- "8"
P6B$P6Q27_rank9[P6B$P6Q27_Karihome == "9"] <- "8"
P6B$P6Q27_rank10[P6B$P6Q27_Karihome == "10"] <- "8"

P6B$P6Q27_rank1[P6B$P6Q27_Babecare == "1"] <- "9"
P6B$P6Q27_rank2[P6B$P6Q27_Babecare == "2"] <- "9"
P6B$P6Q27_rank3[P6B$P6Q27_Babecare == "3"] <- "9"
P6B$P6Q27_rank4[P6B$P6Q27_Babecare == "4"] <- "9"
P6B$P6Q27_rank5[P6B$P6Q27_Babecare == "5"] <- "9"
P6B$P6Q27_rank6[P6B$P6Q27_Babecare == "6"] <- "9"
P6B$P6Q27_rank7[P6B$P6Q27_Babecare == "7"] <- "9"
P6B$P6Q27_rank8[P6B$P6Q27_Babecare == "8"] <- "9"
P6B$P6Q27_rank9[P6B$P6Q27_Babecare == "9"] <- "9"
P6B$P6Q27_rank10[P6B$P6Q27_Babecare == "10"] <- "9"

P6B$P6Q27_rank1[P6B$P6Q27_Neoangelac == "1"] <- "10"
P6B$P6Q27_rank2[P6B$P6Q27_Neoangelac == "2"] <- "10"
P6B$P6Q27_rank3[P6B$P6Q27_Neoangelac == "3"] <- "10"
P6B$P6Q27_rank4[P6B$P6Q27_Neoangelac == "4"] <- "10"
P6B$P6Q27_rank5[P6B$P6Q27_Neoangelac == "5"] <- "10"
P6B$P6Q27_rank6[P6B$P6Q27_Neoangelac == "6"] <- "10"
P6B$P6Q27_rank7[P6B$P6Q27_Neoangelac == "7"] <- "10"
P6B$P6Q27_rank8[P6B$P6Q27_Neoangelac == "8"] <- "10"
P6B$P6Q27_rank9[P6B$P6Q27_Neoangelac == "9"] <- "10"
P6B$P6Q27_rank10[P6B$P6Q27_Neoangelac == "10"] <- "10"

##################################################

P6A$P6Q28
P6A$P6Q28_98

P6A$P6Q28[nchar(P6A$P6Q28_98) > 0] <- "98"
table(P6A$P6Q28)

##################################################

P6B$P6Q28
P6B$P6Q28_98

P6B$P6Q28[nchar(P6B$P6Q28_98) > 0] <- "98"
table(P6B$P6Q28)

##################################################

P6A$P6Q29
P6A$P6Q29_98

P6A$P6Q29[nchar(P6A$P6Q29_98) > 0] <- "98"
table(P6A$P6Q29)

##################################################

P6B$P6Q29
P6B$P6Q29_98

P6B$P6Q29[nchar(P6B$P6Q29_98) > 0] <- "98"
table(P6B$P6Q29)

##################################################

P6A$P6Q30
P6A$P6Q30_7

P6A$P6Q30[nchar(P6A$P6Q30_7) > 0] <- "7"
table(P6A$P6Q30)

##################################################

P6B$P6Q30
P6B$P6Q30_7

P6B$P6Q30[nchar(P6B$P6Q30_7) > 0] <- "7"
table(P6B$P6Q30)

##################################################

P6A$P6Q31
P6A$P6Q31_10

P6A$P6Q31[nchar(P6A$P6Q31_10) > 0] <- "10"
table(P6A$P6Q31)

##################################################

P6B$P6Q31
P6B$P6Q31_10

P6B$P6Q31[nchar(P6B$P6Q31_10) > 0] <- "10"
table(P6B$P6Q31)

##################################################

P6A$Month6
P6B$Month6

P6A$GETENDDAY6
P6B$GETENDDAY6

P6A$MemberDay
P6B$MemberDay

P6Q27_1

table(P6A$P6Q24_Nestle)







