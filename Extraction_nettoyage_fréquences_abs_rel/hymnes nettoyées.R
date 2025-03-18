# Charger les packages nécessaires
pacman::p_load(xml2, tidyverse, stringr, here, tm, dplyr)

# Définir le chemin du dossier contenant les fichiers XML
folder_path <- "C:/Users/33767/Desktop/cours master 2/Humanités Numériques/hymnes en xml"

# Lister tous les fichiers XML dans le dossier
xml_files <- list.files(folder_path, pattern = "\\.xml$", full.names = TRUE)

# Liste des stopwords en latin
stopwords_latin <- c("et", "in", "de", "ad", "per", "ex", "cum", "ut", "sed",
                     "non", "hic", "ille", "ego", "tu", "nos", "vos", "me", "te",
                     "sui", "noster", "vester", "suo", "quod", "qui", "quae", "quem")

# Fonction pour extraire et nettoyer le texte des fichiers XML
extract_clean_text <- function(file) {
    doc <- read_xml(file)

    # DEBUG : Vérifier la structure XML
    print(paste("📄 Traitement du fichier :", file))

    # Tester quelles balises existent
    all_tags <- xml_name(xml_children(doc))
    print(paste("Balises trouvées :", paste(unique(all_tags), collapse = ", ")))

    # Essayer de récupérer le texte des balises <l> ou autre
    text_lines <- xml_text(xml_find_all(doc, ".//l"))

    # Si les balises <l> ne renvoient rien, essayer avec <p> ou <div>
    if (length(text_lines) == 0) {
        text_lines <- xml_text(xml_find_all(doc, ".//p"))
    }
    if (length(text_lines) == 0) {
        text_lines <- xml_text(xml_find_all(doc, ".//div"))
    }

    # Afficher un exemple du texte brut récupéré
    if (length(text_lines) > 0) {
        print("✅ Texte trouvé !")
        print(head(text_lines, 5))  # Afficher les 5 premières lignes récupérées
    } else {
        print("⚠️ Aucun texte trouvé dans ce fichier.")
        return(NULL)
    }

    # Nettoyage du texte
    clean_text <- text_lines %>%
        tolower() %>%                          # Mettre en minuscules
        str_replace_all("[[:punct:]]", " ") %>% # Supprimer la ponctuation
        str_replace_all("[[:digit:]]", " ") %>% # Supprimer les chiffres
        str_squish()                            # Supprimer les espaces inutiles

    # Tokenisation (diviser en mots)
    tokens <- unlist(strsplit(clean_text, "\\s+"))

    # Supprimer les stopwords latins
    tokens <- tokens[!(tokens %in% stopwords_latin)]

    # Vérifier si des tokens restent après le nettoyage
    if (length(tokens) == 0) {
        print("⚠️ Après nettoyage, il ne reste plus de texte utilisable.")
        return(NULL)
    }

    # Retourner un dataframe propre
    data.frame(
        poeme = basename(file),
        token = tokens
    )
}

# Appliquer la fonction à tous les fichiers XML
all_poems_cleaned <- map_dfr(xml_files, extract_clean_text)

# Vérifier si des données ont été extraites
if (nrow(all_poems_cleaned) == 0) {
    stop("🚨 Aucun texte valide n'a été extrait des fichiers XML après nettoyage.")
}

# Sauvegarder les résultats dans un fichier CSV propre
write_csv(all_poems_cleaned, file.path(folder_path, "hymnes_nettoyes.csv"))

# Afficher un aperçu des données nettoyées
head(all_poems_cleaned, 20)
