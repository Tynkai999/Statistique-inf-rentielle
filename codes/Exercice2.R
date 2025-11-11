#==========================================================================
#                           Exercice 2
#     Influence de l'alcool sur le temps de réaction    
#==========================================================================

# Les données
# Le vecteur sans_alcool
sans_alcool <- c(0.68, 0.64, 0.68, 0.82, 0.58, 0.80, 0.72, 0.65, 0.84, 0.73, 
                 0.65, 0.59, 0.78, 0.67, 0.65)


# Le vecteur avec_vecteur
avec_alcool <- c(0.73, 0.62, 0.66, 0.92, 0.68, 0.87, 0.77, 0.70, 0.88, 0.79, 
                 0.72, 0.60, 0.78, 0.66, 0.68)

ecdf_sans_alcool <- ecdf(sans_alcool)
ecdf_avec_alcool <- ecdf(avec_alcool)

ecdf_sans_alcool
ecdf_avec_alcool

#Tracer les fonctions de répartitions
#temps de reaction sans consommation d'alcool
plot(ecdf_sans_alcool, main = "Fonctions de répartition empirique", 
     col = "orange", lwd = 2)

#temps de consommation sans alcool
lines(ecdf_avec_alcool, ,col = "red", lwd = 2)

#afficher une legende
legend("bottomright", legend = c("Sans alcool", "Avec alcool"),
       col = c("orange", "red"), lwd = 2) 


# Vérification de la normalité
shapiro_sans_alcool <- shapiro.test(sans_alcool)
shapiro_avec_alcool <- shapiro.test(avec_alcool)

cat("Le taux de normalité pour le groupe sans alcool")
shapiro_sans_alcool

cat("Le taux de normalité pour le groupe avec alcool")
shapiro_avec_alcool

# Test d'égalité des variances
var_test <- var.test(sans_alcool, avec_alcool)
cat("Test dégalité des variance")
print(var_test)



# Test de student pour les échantillons indépendants
test_t <- t.test(avec_alcool, sans_alcool, var.equal = TRUE)
cat("Test de student pour les échantillons")
print(test_t)