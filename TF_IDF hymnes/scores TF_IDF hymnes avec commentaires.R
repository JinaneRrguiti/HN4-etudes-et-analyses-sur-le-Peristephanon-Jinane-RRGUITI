# 📌 CHARGEMENT DES PACKAGES
library(xml2)
library(tidyverse)
library(tidytext)
library(rmarkdown)

# 📌 CHEMIN DU DOSSIER CONTENANT LES FICHIERS XML (NOUVEAU)
folder_path <- file.path("C:", "Users", "33767", "Desktop", "cours master 2", "Humanités Numériques", "hn4", "hymnes en xml")

# 📌 LISTER LES FICHIERS XML
xml_files <- list.files(folder_path, pattern = "\\.xml$", full.names = TRUE)

# 📌 VÉRIFIER QUE DES FICHIERS XML EXISTENT
if (length(xml_files) == 0) {
    stop("🚨 Aucun fichier XML trouvé dans le dossier : ", folder_path)
}

# 📌 FONCTION D'EXTRACTION DU TEXTE PAR HYMNE
extract_poem_data <- function(file) {
    doc <- read_xml(file)

    # Vérifier que le fichier XML est bien lisible
    if (is.null(doc)) {
        warning(paste("⚠ Impossible de lire le fichier :", basename(file)))
        return(NULL)
    }

    # Extraire le titre du poème
    titre <- xml_text(xml_find_first(doc, ".//head"))
    if (is.na(titre) | titre == "") {
        titre <- "Titre inconnu"
    }

    # Extraire les vers du poème
    vers <- xml_text(xml_find_all(doc, ".//l | .//p | .//lg"))
    if (length(vers) == 0) {
        warning(paste("⚠ Aucun vers extrait dans", basename(file)))
        return(NULL)
    }

    # Retourner un data frame
    data.frame(poeme = basename(file), titre = titre, texte = paste(vers, collapse = " "), stringsAsFactors = FALSE)
}

# 📌 APPLIQUER L'EXTRACTION À TOUS LES FICHIERS XML
all_poems <- map_dfr(xml_files, extract_poem_data)

# 📌 VÉRIFIER SI L'EXTRACTION A FONCTIONNÉ
if (nrow(all_poems) == 0) {
    stop("🚨 Aucun texte valide n'a été extrait des fichiers XML.")
}

# 📌 NETTOYAGE DU TEXTE
clean_text <- function(text) {
    text <- tolower(text)  # Minuscule
    text <- gsub("[[:punct:]]", "", text)  # Supprime ponctuation
    text <- gsub("[0-9]", "", text)  # Supprime chiffres
    text <- str_split(text, "\\s+")  # Tokenisation
    return(unlist(text))
}

# 📌 APPLIQUER LE NETTOYAGE
all_poems_cleaned <- all_poems %>%
    mutate(tokens = map(texte, clean_text)) %>%
    unnest(tokens) %>%
    filter(tokens != "")

# 📌 STOPWORDS GRAMMATICAUX : EXCLURE LES MOTS NON LEXICAUX
stopwords_grammaticaux <- c(
    "et", "aut", "ac", "sed", "nec", "si", "cum", "non", "ut", "in", "ad", "per", "ex", "de", "sub", "pro", "sine",
    "nam", "enim", "quod", "quia", "atque", "quoque", "tamen", "post", "ante", "vero", "quidem", "dum", "nunc",
    "ergo", "itaque", "hinc", "igitur", "vel", "inter", "inde",
    "qui", "quae", "quod", "cuius", "cui", "quem", "quam", "quo", "quibus", "haec", "hoc", "hunc", "hanc", "hic",
    "ille", "illa", "illi", "ipse", "ipsa", "ipsum", "istud", "eius", "eorum", "meus", "tuus", "suus", "noster",
    "vester", "aliquis", "quis", "quid", "quicumque", "quilibet", "tibi", "quos", "his",
    "iam", "sic", "ubi", "nunc", "iamque", "numquam", "semper", "tamen", "quidem", "huc", "usque", "unde", "tunc",
    "est", "sunt", "esse", "erat", "erant", "fuit", "fuerunt", "fit", "fiunt", "fieri", "habere", "habet", "habeo",
    "dicere", "dixit", "facere", "fecit", "videre", "vidit", "venit", "ire", "ii", "eunt", "ire", "sit", "ait"
)

# 📌 FILTRER LES STOPWORDS ET CONSERVER LES TERMES DU LEXIQUE
tokens_filtered <- all_poems_cleaned %>%
    filter(!tokens %in% stopwords_grammaticaux, str_length(tokens) > 2) %>%
    count(poeme, tokens, sort = TRUE)

# 📌 CALCUL DU TF-IDF
tokens_tfidf <- tokens_filtered %>%
    bind_tf_idf(tokens, poeme, n) %>%
    arrange(poeme, desc(tf_idf))

# 📌 EXTRAIRE LES TOP 5 TERMES SPÉCIFIQUES PAR HYMNE
tokens_tfidf_top5 <- tokens_tfidf %>%
    group_by(poeme) %>%
    slice_max(order_by = tf_idf, n = 5) %>%
    ungroup()

# 📌 CRÉATION DU RAPPORT HTML COMMENTÉ
html_output <- file.path(folder_path, "analyse_TF-IDF.html")

sink(html_output)
cat("<html><head><title>Analyse TF-IDF des hymnes</title></head><body>")
cat("<h1>Analyse TF-IDF des hymnes</h1>")
cat("<p>Ce document présente les 5 mots les plus spécifiques de chaque hymne selon le score TF-IDF.</p>")

for (hymne in unique(tokens_tfidf_top5$poeme)) {
    data_hymne <- tokens_tfidf_top5 %>% filter(poeme == hymne)

    cat(paste("<h2>Hymne :", hymne, "</h2>"))
    cat("<ul>")

    for (i in 1:nrow(data_hymne)) {
        cat(paste("<li><b>", data_hymne$tokens[i], "</b> : Score TF-IDF =", round(data_hymne$tf_idf[i], 4), "</li>"))
    }

    cat("</ul>")

    # 📌 COMMENTAIRE EXPLICATIF
    cat("<p><b>Analyse :</b> Ces mots sont spécifiques à cet hymne car ils apparaissent fréquemment dans celui-ci mais sont rares dans les autres. Cela peut indiquer un **thème particulier** ou une **terminologie spécifique** propre à l'hymne.</p>")

    # Exemples d'interprétation
    cat("<p><b>Exemple d'analyse :</b> Si un hymne contient des termes comme 'martyr', 'sacrifice' ou 'souffrance', il est probable qu'il traite du thème du **martyre chrétien**.</p>")
    cat("<p>À l’inverse, si un hymne contient des termes comme 'baptême', 'fontaine', 'eau', alors il est sûrement centré sur le **rite du baptême**.</p>")

    cat("<p>En comparant les termes TF-IDF entre hymnes, on peut identifier les **spécificités lexicales** de chaque texte.</p>")
}

cat("</body></html>")
sink()

# 📌 MESSAGE FINAL
message("📊 ✅ Analyse terminée. Résultats enregistrés dans : ", html_output)
