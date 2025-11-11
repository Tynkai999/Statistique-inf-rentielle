#==========================================================================
#                           Exercice 3
#                 Analyse de données pour enfant
#==========================================================================



# Création des vecteurs

#vecteurs individus
Individus = c("Erika", "Célia", "Erik", "Eve", "Paul", "jean", "Adan", "Louis",
              "Jules", "Léo")

#vecteurs Poids
Poids = c(16, 14, 13.5, 15.4, 16.5, 16, 17, 14.8, 17, 16.7)

#vecteurs Taille
Taille = c(100.0, 97.0, 95.5, 101.0, 100.0, 98.5, 103.0, 98.0, 101.5, 100.0)

#vecteurs sexe
Sexe = c("F", "F", "G","F","G", "G", "G", "G", "G", "G")


# Vecteur An
An <- c(3, 3, 3, 4, 3, 4, 3, 3, 4, 3)
# length(An)
# Vecteur Mois

Mois <- c(5, 10, 5, 0, 8, 0, 11, 9, 1, 3)
# length(Mois)
# Calculer l'âge des individus

Age <- round(An + Mois/12, 1)
Age


#moyenne de la taille
moyenne_taille <- mean(Taille)
moyenne_poids <- mean(Poids)
moyenne_age <- mean(Age)

# Affichage des moyennes
cat("La moyenne des tailles : ", moyenne_taille, "cm\n")
cat("La moyenne des poids : ", moyenne_poids, "kg\n")
cat("La moyenne des âges : ", moyenne_age, "an(s)\n")


# Taille en mètre
taille_m <- Taille / 100

# Calcul de l'IMC
IMC_echantillon = round((Poids / (taille_m)^2), 2)
# IMC_echantillon

cat("Valeur de l'IMC: \n")
for (i in 1:length(Individus)) {
  cat(Individus[i], " : " ,IMC_echantillon[i], "kg/m²\n")
}

# Structure en dataframe

enfant_df <- data.frame(
  Individus = Individus, 
  Sexe = Sexe, 
  Poids = Poids, 
  Taille = Taille,
  Age_complet = Age, 
  IMC_echantillon = IMC_echantillon
)

enfant_df


# Installation de plotly()
install.packages("plotly")


# Coéfficient de corrélation entre le Poids et la taille
correlation <- cor(Taille, Poids)
cat("Le coéfficient de corrélation entre la taille et le poids est de : ", 
    round(correlation, 3))



# Création du plot
plot <- plot_ly(
  data = enfant_df, 
  x = ~Taille, 
  y = ~Poids, 
  color = ~Sexe, 
  colors = c('F' = 'pink', 'G' = 'blue'),
  type = 'scatter', 
  mode = 'markers',
  text = ~paste(
    "Enfant : ", Individus,
    "<br>Sexe : ", Sexe, 
    "<br>Taille : ", Taille, "cm",
    "<br>Poids : ", Poids, "kg"
  ), 
  hoverinfo = "text", 
  marker = list(
    size = 10,          
    opacity = 0.9, 
    line = list(width = 1)  
  )
)

# Ajout de la droite de régression
plot <- plot %>%
  add_trace(
    x = ~Taille, 
    y = ~fitted(lm(Poids ~ Taille, data = enfant_df)),
    type = 'scatter', 
    mode = 'lines', 
    name = "Régression linéaire", 
    line = list(color = "red", width = 3, dash = "solid"),
    showlegend = FALSE
  ) %>%
  layout(
    title = list(
      text = '<br><b>Relation Poids-Taille par Sexe</b></br>',
      x = 0.5,
      font = list(size = 18)
    ),
    xaxis = list(
      title = 'Taille (cm)',
      zeroline = FALSE,
      gridcolor = 'lightgrey'
    ),
    yaxis = list(
      title = 'Poids (kg)',
      zeroline = FALSE,
      gridcolor = 'lightgrey'
    ),
    plot_bgcolor = 'white',
    legend = list(x = 0.02, y = 0.98),
    annotations = list(
      list(
        x = 0.02, y = 0.02,
        xref = "paper", yref = "paper",
        text = paste("r =", round(correlation, 3)),
        showarrow = FALSE,
        font = list(size = 14, color = "black"),
        bgcolor = "lightyellow",
        bordercolor = "orange"
      )
    )
  )

# Affichage du graphique
plot

modele <- lm(Poids ~ Taille, data = enfant_df)
predictions <- fitted(modele)

cat("\n=== ANALYSE DU MODÈLE ===\n")
cat("Équation : Poids =", round(coef(modele)[1], 2), "+", round(coef(modele)[2], 2), "× Taille\n")

# Tableau comparatif réel vs prédit
comparaison <- data.frame(
  Individu = enfant_df$Individu,
  Taille = enfant_df$Taille,
  Poids_Reel = enfant_df$Poids,
  Poids_Predit = round(predictions, 2)
)
print(comparaison)