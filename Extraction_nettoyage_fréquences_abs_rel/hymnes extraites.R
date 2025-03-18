# Charger les packages nécessaires
library(xml2)
library(tidyverse)

# Définir le chemin du dossier contenant les fichiers XML
folder_path <- "C:/Users/33767/Desktop/cours master 2/Humanités Numériques/hymnes en xml"

# Lister les fichiers XML dans le dossier
xml_files <- list.files(folder_path, pattern = "\\.xml$", full.names = TRUE)

# Fonction pour extraire le titre et les vers d'un poème
extract_poem_data <- function(file) {
    doc <- read_xml(file)

    # Extraire le titre du poème
    titre <- xml_text(xml_find_first(doc, ".//head"))

    # Extraire les vers du poème (on teste plusieurs balises possibles)
    vers <- xml_text(xml_find_all(doc, ".//l | .//p | .//lg"))

    # Vérifier si on a bien extrait du texte
    if (length(vers) == 0) {
        warning(paste("⚠ Aucun vers extrait pour", basename(file)))
    }

    # Retourner un data frame
    data.frame(poeme = basename(file), titre = titre, texte = paste(vers, collapse = " "), stringsAsFactors = FALSE)
}

# Appliquer la fonction à tous les fichiers XML
all_poems <- map_dfr(xml_files, extract_poem_data)

# Vérifier si l'extraction a réussi
if (nrow(all_poems) == 0) {
    stop("🚨 Aucun texte valide n'a été extrait des fichiers XML. Vérifiez la structure des fichiers.")
}

# Afficher les premières lignes du tableau
print(head(all_poems, 5))

# Sauvegarder le tableau en CSV
write_csv(all_poems, file.path(folder_path, "poemes_extraits.csv"))
