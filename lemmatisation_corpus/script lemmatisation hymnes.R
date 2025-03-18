# 📌 CHARGEMENT DES PACKAGES
library(xml2)
library(tidyverse)
library(tidytext)
library(udpipe)
library(DT)
library(rmarkdown)

# 📌 CHEMIN DU DOSSIER CONTENANT LES FICHIERS XML
folder_path <- file.path("C:", "Users", "33767", "Desktop", "cours master 2", "Humanités Numériques", "hn4", "hymnes en xml")

# 📌 DOSSIER DE SORTIE POUR LES RÉSULTATS
output_folder <- file.path("C:", "Users", "33767", "Desktop", "cours master 2", "Humanités Numériques", "hn4", "lemmatisation_corpus")
dir.create(output_folder, showWarnings = FALSE, recursive = TRUE)

# 📌 LISTER LES FICHIERS XML
xml_files <- list.files(folder_path, pattern = "\\.xml$", full.names = TRUE)

# 📌 EXTRACTION DU TEXTE
extract_poem_data <- function(file) {
    doc <- read_xml(file)

    titre_node <- xml_find_first(doc, ".//head")
    titre <- if (!is.na(titre_node)) xml_text(titre_node) else "Titre inconnu"

    vers_nodes <- xml_find_all(doc, ".//l | .//p | .//lg")
    vers <- if (length(vers_nodes) > 0) xml_text(vers_nodes) else NULL

    if (is.null(vers) || length(vers) == 0) return(NULL)

    return(data.frame(poeme = basename(file), titre = titre, texte = paste(vers, collapse = " "), stringsAsFactors = FALSE))
}

# 📌 EXTRAIRE LES TEXTES DES 14 HYMNES
all_poems <- map_dfr(xml_files, extract_poem_data)

# 📌 NETTOYAGE DU TEXTE
clean_text <- function(text) {
    text <- tolower(text)
    text <- gsub("[[:punct:]]", "", text)
    text <- gsub("[0-9]", "", text)
    text <- str_replace_all(text, "\\s+", " ")
    return(text)
}

all_poems <- all_poems %>% mutate(texte = map_chr(texte, clean_text))

# 📌 CHARGEMENT DU MODÈLE UDPipe POUR LE LATIN
ud_model <- udpipe_download_model(language = "latin")  # Télécharge une seule fois
ud_model <- udpipe_load_model(ud_model$file_model)

# 📌 LEMMATISATION INDIVIDUELLE PAR HYMNE
lemmatisation_results <- map_dfr(all_poems$poeme, function(hymne) {
    text <- all_poems %>% filter(poeme == hymne) %>% pull(texte)
    annotation <- udpipe_annotate(ud_model, x = text) %>% as.data.frame()
    annotation$poeme <- hymne  # Ajoute le nom de l’hymne
    return(annotation)
})

# 📌 LISTE DES TERMES CIBLÉS (CORPS & MATIÈRE)
keywords <- c("membra", "artus", "corpus", "sanguis", "os", "cruor", "pectus", "manus", "caput",
              "ferro", "aqua", "igne", "lapis", "aurum", "argentum", "terra")

# 📌 FILTRAGE DES TERMES SPÉCIFIQUES AU CORPS ET À LA MATIÈRE
lemmatisation_filtered <- lemmatisation_results %>%
    filter(lemma %in% keywords) %>%
    select(poeme, token, lemma, upos)

# 📌 ENREGISTREMENT DU FICHIER CSV FILTRÉ
write.csv(lemmatisation_filtered, file.path(output_folder, "lemmatisation_corpus_filtre.csv"), row.names = FALSE)

# 📌 CRÉATION D'UN RAPPORT HTML
html_output <- file.path(output_folder, "Analyse_Lemmatisation_Corps_Matiere.html")
sink(html_output)

cat("<html><head><title>Analyse de Lemmatisation - Corps et Matière</title></head><body>")
cat("<h1>Lemmatisation des Hymnes de Prudence</h1>")
cat("<p>Cette analyse se concentre sur les termes liés au corps et aux éléments matériels.</p>")

# 📌 ANALYSE PAR HYMNE
for (hymne in unique(lemmatisation_filtered$poeme)) {
    data_hymne <- lemmatisation_filtered %>% filter(poeme == hymne)

    cat(paste("<h2>Hymne :", hymne, "</h2>"))
    cat("<table border='1'><tr><th>Mot original</th><th>Lemme</th><th>Catégorie grammaticale</th></tr>")

    for (i in 1:nrow(data_hymne)) {
        cat("<tr>")
        cat(paste0("<td>", data_hymne$token[i], "</td>"))
        cat(paste0("<td><b>", data_hymne$lemma[i], "</b></td>"))
        cat(paste0("<td>", data_hymne$upos[i], "</td>"))
        cat("</tr>")
    }
    cat("</table>")

    # 📌 COMMENTAIRES INTERPRÉTATIFS
    cat("<h3>Interprétation :</h3>")
    if ("manus" %in% data_hymne$lemma) {
        cat("<p><b>Manus :</b> La main dans les hymnes chrétiens peut symboliser la bénédiction, l’action ou le châtiment divin.</p>")
    }
    if ("caput" %in% data_hymne$lemma) {
        cat("<p><b>Caput :</b> La tête est souvent associée à l’autorité divine ou au sacrifice des martyrs.</p>")
    }
    if ("sanguis" %in% data_hymne$lemma) {
        cat("<p><b>Sanguis :</b> Le sang est le symbole du sacrifice et du rachat des péchés.</p>")
    }
    if ("ferro" %in% data_hymne$lemma) {
        cat("<p><b>Ferro :</b> Le fer peut évoquer les instruments de torture des martyrs.</p>")
    }
    if ("igne" %in% data_hymne$lemma) {
        cat("<p><b>Igne :</b> Le feu représente souvent la purification ou le supplice.</p>")
    }
}

cat("</body></html>")
sink()

# 📌 MESSAGE FINAL
message("📊 ✅ Lemmatisation terminée. Résultats disponibles dans : ", html_output)
