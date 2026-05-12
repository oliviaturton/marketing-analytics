install.packages("corrplot")
install.packages("gplots")
install.packages("nFactors")

brand.ratings <- read.csv("Final Assessment Datasets/2_chocolate_rating.csv", stringsAsFactors = TRUE)
head(brand.ratings)
summary(brand.ratings)
str(brand.ratings)

brand.sc <- brand.ratings
brand.sc[,6:14] <- scale(brand.ratings[,6:14])
summary(brand.sc)

cor(brand.sc[,6:14])
library(corrplot)
corrplot(cor(brand.sc[,6:14]))
corrplot(cor(brand.sc[,6:14]), order="hclust")

brand.mean <- aggregate(cbind(cocoa_butter,
                              cocoa_percent,
                              rating,
                              counts_of_ingredients,
                              vanilla,
                              organic,
                              salt,
                              sugar,
                              sweetener)~ brand,
                        data=brand.sc, 
                        mean)
brand.mean

rownames(brand.mean) <- brand.mean$brand
brand.mean <- brand.mean [, -1]
brand.mean

library(gplots)
heatmap.2(as.matrix(brand.mean),main = "Brand attributes",
          trace = "none", key = FALSE, dend = "none"
)

brand.pc<- princomp(brand.mean, cor = TRUE)
summary(brand.pc)

plot(brand.pc,type="l") 
loadings(brand.pc) 
brand.pc$scores 

biplot(brand.pc, main = "Brand positioning")
