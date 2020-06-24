#Packages R nécessaires pour l'ACP
library(factoextra)#visualisation graphique
library(FactoMineR)#ACP


#Chargement des données
donnees<-read.csv("data.csv")
donnees<-data.frame(donnees,row.names = donnees$Nation)#Nommage
donnees<-donnees[4:17]#on ne garde que les variables pertinentes

donnees.variables<-names(donnees.actifs)
donnees.pays<-rownames(donnees.actifs)

#Analyse par composantes principales

donnees.acp<-PCA(donnees,graph = FALSE,quali.sup = 13)
summary(donnees.acp)

donnees.acp$call

fviz_pca_var(donnees.acp, col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE
)#Représentation des variables construisant les axes de l'ACP

donnees.desc <- dimdesc(donnees.acp, axes = c(1,2), proba = 0.01)
#Test des variables significativement impliquées dans l'ACP
donnees.desc$Dim.1
donnees.desc$Dim.2

fviz_pca_ind (donnees.acp, col.ind = donnees$Culture, pointsize = "cos2", pointshape=21,fill.ind = donnees$Culture,
              repel = TRUE,addEllipses = TRUE)

fviz_pca_biplot(donnees.acp, 
                # Colueur de remplissage des individdus par groupes
                geom.ind = "point",
                pointshape = 21,
                pointsize = 2.5,
                fill.ind = donnees$Culture,
                col.ind = "black",
                repel = TRUE,        # Evite le chévauchement du texte
                # Colorer les variables par groupes
                col.var = "cos2",
                legend.title = list(fill = "Culture", color = "cos2"),
                gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07")
)


cor.test(donnees$SISE,donnees$GGI,method = "spearman")
plot(donnees$GGI,donnees$SISE)
