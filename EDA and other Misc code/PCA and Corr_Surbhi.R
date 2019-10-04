library(ggbiplot)
library(devtools)
library(factoextra)
library(digest)
library(Hmisc)
library(corrplot)
library(RColorBrewer)


picca <- read.csv("~/20190928_Model2_FINAL.csv")

####Subset to remove row numbers
picca <- subset(picca, select = -c(1))

str(picca)

summary(picca)


####Subset numerical data for PCA
picca <- subset(picca, select = -c(1:3))


####PCA
res.pca <- prcomp(picca, scale = TRUE)

####Screeplot
fviz_eig(res.pca)

####Individual PCA chart
fviz_pca_ind(res.pca, col.ind="cos2", geom = "point",
             gradient.cols = c("white", "#2E9FDF", "#FC4E07" ))

####Individual PCA chart - with ellipse
fviz_pca_ind(res.pca, label="none", habillage=picca$Breached,
             addEllipses=TRUE, ellipse.level=0.95, palette = "Dark")

####Variable PCA chart
fviz_pca_var(res.pca,
             col.var = "contrib",
             gradient.cols = c("#00AFBB", "#2E9FDF", "#FC4E07"),
             repel = TRUE)


fviz_pca_biplot(res.pca, label = "var", habillage=picca$Breached,
                repel = TRUE,
                addEllipses=TRUE, ellipse.level=0.95,
                ggtheme = theme_minimal())

####PCA Eigen Values
eig.val <- get_eigenvalue(res.pca)
eig.val


####PCA Summary
summary(res.pca)


###Correlation

res2 <- rcorr(as.matrix(picca))
res2


flattenCorrMatrix <- function(cormat, pmat) {
  ut <- upper.tri(cormat)
  data.frame(
    row = rownames(cormat)[row(cormat)[ut]],
    column = rownames(cormat)[col(cormat)[ut]],
    cor  =(cormat)[ut],
    p = pmat[ut]
  )
}

flattenCorrMatrix(res2$r, res2$P)


corrplot(res2$r, type="upper", order="hclust", 
         p.mat = res2$P, sig.level = 0.01, insig = "blank")

####Correlation plot of significant correlations with correlation coefficients 
corrplot(res2$r, order="hclust", method = "number", tl.col = "dark blue", tl.cex = 0.8,
         p.mat = res2$P, sig.level = 0.01, insig = "blank", col = brewer.pal(n = 8, name = "RdBu"))



