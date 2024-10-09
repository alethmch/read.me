# Paramètres
n_tables <- 5                 # Nombre total de tables
n_rows_per_table <- 6         # Nombre de rangées par table
n_pots_per_row <- 11          # Nombre de pots par rangée
n_genotypes <- 33             # Nombre de génotypes
n_per_genotype <- 10          # Chaque génotype doit apparaître 10 fois en tout (2 fois par table)
max_attempts <- 1000          # Nombre maximal de tentatives pour éviter les boucles infinies

# Définition des tables côte à côte
adjacent_tables <- list(c(1, 2), c(3, 4))  # Les tables 1-2 et 3-4 sont côte à côte

# Initialisation du tableau pour stocker les positions des génotypes
positions <- data.frame(
  genotype = integer(n_genotypes * n_per_genotype),  # Colonne pour les génotypes
  table = integer(n_genotypes * n_per_genotype),     # Colonne pour les tables
  row = integer(n_genotypes * n_per_genotype),       # Colonne pour les rangées
  pot = integer(n_genotypes * n_per_genotype)        # Colonne pour les positions dans les rangées
)

# Fonction qui vérifie les contraintes strictes
is_valid_position <- function(current_positions, new_position, genotype) {
  table <- new_position[1]
  row <- new_position[2]
  pot <- new_position[3]
  
  # Vérifier que les deux individus d'un même génotype ne sont pas dans le même rang sur la même table
  if (any(current_positions$genotype == genotype & current_positions$table == table & current_positions$row == row)) {
    return(FALSE)
  }
  
  return(TRUE)
}

# Fonction pour éviter au maximum les conflits horizontaux (rangée identique à travers le phytotron)
count_horizontal_conflicts <- function(current_positions, new_position, genotype) {
  row <- new_position[2]
  
  # Compter les individus du même génotype dans la même rangée (à travers toutes les tables)
  return(sum(current_positions$genotype == genotype & current_positions$row == row))
}

# Fonction pour éviter au maximum les conflits de position sur les tables côte à côte
count_adjacent_table_conflicts <- function(current_positions, new_position, genotype, adjacent_tables) {
  table <- new_position[1]
  pot <- new_position[3]
  
  # Vérifier les conflits de position entre les tables côte à côte
  conflicts <- 0
  for (pair in adjacent_tables) {
    if (table %in% pair) {
      other_table <- setdiff(pair, table)  # Obtenir la table adjacente
      # Vérifier s'il y a un individu du même génotype à la même position sur la table adjacente
      if (any(current_positions$genotype == genotype & current_positions$table == other_table & current_positions$pot == pot)) {
        conflicts <- conflicts + 1
      }
    }
  }
  
  return(conflicts)
}

# Attribution aléatoire des positions aux génotypes
set.seed(123)  # Fixer une graine pour la reproductibilité du tirage aléatoire

# Boucle sur chaque génotype
for (genotype in 1:n_genotypes) {
  assigned_positions <- data.frame(table = integer(0), row = integer(0), pot = integer(0))  # Positions assignées à ce génotype
  
  # Boucle pour attribuer 2 positions par table (5 tables x 2 = 10 positions)
  for (table in 1:n_tables) {
    assigned_to_table <- 0  # Compteur pour savoir combien de positions sont déjà assignées pour ce génotype sur cette table
    attempts <- 0  # Compteur du nombre d'essais
    
    while (assigned_to_table < 2 && attempts < max_attempts) {
      attempts <- attempts + 1
      
      # Tirer au sort une rangée et une position dans cette rangée
      new_position <- c(
        table,                        # La table courante
        sample(1:n_rows_per_table, 1), # Tirer une rangée au hasard
        sample(1:n_pots_per_row, 1)    # Tirer une position dans la rangée au hasard
      )
      
      # Vérifier les contraintes strictes
      if (is_valid_position(assigned_positions, new_position, genotype)) {
        # Calculer les conflits horizontaux et les conflits de position sur les tables adjacentes
        horizontal_conflicts <- count_horizontal_conflicts(assigned_positions, new_position, genotype)
        adjacent_conflicts <- count_adjacent_table_conflicts(assigned_positions, new_position, genotype, adjacent_tables)
        
        # Accepter la position si elle minimise les conflits, ou avec une probabilité d'acceptation
        if (horizontal_conflicts == 0 && adjacent_conflicts == 0) {
          # Ajouter la position si aucune conflit n'est trouvé
          assigned_positions <- rbind(assigned_positions, new_position)
          
          # Stocker cette position dans le tableau des positions globales
          idx <- (genotype - 1) * n_per_genotype + nrow(assigned_positions)
          positions[idx, ] <- c(genotype, new_position)
          
          assigned_to_table <- assigned_to_table + 1
        } else if (runif(1) > 0.2) {  # Probabilité d'accepter un conflit
          # Ajouter la position malgré les conflits avec une faible probabilité
          assigned_positions <- rbind(assigned_positions, new_position)
          
          idx <- (genotype - 1) * n_per_genotype + nrow(assigned_positions)
          positions[idx, ] <- c(genotype, new_position)
          
          assigned_to_table <- assigned_to_table + 1
        }
      }
    }
    
    # Si aucune position valide n'est trouvée après plusieurs essais
    if (assigned_to_table < 2) {
      cat("Aucune position valide trouvée pour le génotype", genotype, "dans la table", table, "après", max_attempts, "essais.\n")
      break  # Sortir de la boucle si aucune position n'a été trouvée
    }
  }
}

# Organiser les résultats par génotype
positions_list <- split(positions, positions$genotype)

# Afficher les résultats
print(positions_list)

