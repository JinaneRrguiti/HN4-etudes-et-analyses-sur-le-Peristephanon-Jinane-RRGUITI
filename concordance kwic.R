# 📌 CHARGEMENT DES PACKAGES
library(xml2)
library(tidyverse)
library(tidytext)
library(igraph)
library(ggraph)
library(wordcloud)
library(DT)
library(rmarkdown)
library(ggplot2)
library(gridExtra)

# 📌 CHEMIN DU DOSSIER CONTENANT LES FICHIERS XML
folder_path <- file.path("C:", "Users", "33767", "Desktop", "hymnes en xml2")

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

    data.frame(poeme = basename(file), titre = titre, texte = paste(vers, collapse = " "), stringsAsFactors = FALSE)
}

# 📌 EXTRAIRE LES TEXTES
all_poems <- map_dfr(xml_files, extract_poem_data)

# 📌 NETTOYAGE DU TEXTE
clean_text <- function(text) {
    text <- tolower(text)
    text <- gsub("[[:punct:]]", "", text)
    text <- gsub("[0-9]", "", text)
    text <- str_split(text, "\\s+")
    return(unlist(text))
}

# 📌 APPLICATION DU NETTOYAGE
all_poems_cleaned <- all_poems %>%
    mutate(tokens = map(texte, clean_text)) %>%
    unnest(tokens) %>%
    filter(tokens != "")

# 📌 LISTE DES TERMES CIBLÉS (CORPS & MATIÈRE)
keywords <- c("membra", "artus", "corpus", "sanguis", "os", "cruor", "pectus", "manus", "caput",
              "ferro", "aqua", "igne", "lapis", "aurum", "argentum", "terra")

# 📌 CRÉATION DU KWIC
kwic_results <- all_poems_cleaned %>%
    filter(tokens %in% keywords) %>%
    group_by(poeme) %>%
    mutate(contexte_gauche = lag(tokens, 2), contexte_droit = lead(tokens, 2)) %>%
    filter(!is.na(contexte_gauche), !is.na(contexte_droit)) %>%
    select(poeme, contexte_gauche, tokens, contexte_droit)

# 📌 LIMITATION À 30 ENTRÉES MAX PAR HYMNE
kwic_results <- kwic_results %>% group_by(poeme) %>% slice_head(n = 30) %>% ungroup()

# 📌 CRÉATION DU DOSSIER POUR LES VISUALISATIONS
output_folder <- file.path(folder_path, "visualisations")
dir.create(output_folder, showWarnings = FALSE)

# 📌 RÉSEAU LEXICAL DES CO-OCCURRENCES
cooc <- kwic_results %>%
    select(contexte_gauche, tokens, contexte_droit) %>%
    pivot_longer(cols = c(contexte_gauche, contexte_droit), names_to = "position", values_to = "mot") %>%
    count(tokens, mot, sort = TRUE) %>%
    filter(n > 1)

if (nrow(cooc) > 0) {
    graph <- graph_from_data_frame(cooc, directed = FALSE)

    plot_graph <- ggraph(graph, layout = "fr") +
        geom_edge_link(aes(edge_alpha = n), show.legend = FALSE) +
        geom_node_point(size = 5, color = "darkred") +
        geom_node_text(aes(label = name), repel = TRUE) +
        theme_void() +
        ggtitle("Réseau lexical des termes du corps et de la matière")

    ggsave(file.path(output_folder, "reseau_lexical.png"), plot_graph, width = 8, height = 6)
}

# 📌 WORDCLOUD DES TERMES ASSOCIÉS
word_freq <- cooc %>% group_by(mot) %>% summarise(freq = sum(n))

if (nrow(word_freq) > 0) {
    png(file.path(output_folder, "wordcloud.png"), width = 800, height = 600)
    wordcloud(words = word_freq$mot, freq = word_freq$freq, min.freq = 2, colors = brewer.pal(8, "Dark2"))
    dev.off()
}

# 📌 CRÉATION DU DOCUMENT HTML
html_output <- file.path(folder_path, "Analyse_KWIC_Corps_Matière.html")
sink(html_output)

cat("<html><head><title>Analyse KWIC - Vocabulaire du corps et de la matière</title></head><body>")
cat("<h1>Analyse KWIC</h1>")
cat("<p>Cette analyse explore comment les termes du corps et de la matière apparaissent dans leur contexte.</p>")

# 📌 AFFICHER LES VISUALISATIONS
cat("<h2>Réseau lexical des co-occurrences</h2>")
cat("<img src='visualisations/reseau_lexical.png' width='600px'>")

cat("<h2>Wordcloud des termes associés</h2>")
cat("<img src='visualisations/wordcloud.png' width='600px'>")

# 📌 ANALYSE PAR HYMNE
for (hymne in unique(kwic_results$poeme)) {
    data_hymne <- kwic_results %>% filter(poeme == hymne)

    cat(paste("<h2>Hymne :", hymne, "</h2>"))
    cat("<table border='1'><tr><th>Contexte gauche</th><th>Mot clé</th><th>Contexte droit</th></tr>")

    for (i in 1:nrow(data_hymne)) {
        cat("<tr>")
        cat(paste0("<td>", data_hymne$contexte_gauche[i], "</td>"))
        cat(paste0("<td><b>", data_hymne$tokens[i], "</b></td>"))
        cat(paste0("<td>", data_hymne$contexte_droit[i], "</td>"))
        cat("</tr>")
    }
    cat("</table>")

    # 📌 COMMENTAIRE SPÉCIFIQUE À L'HYMNE
    cat("<h3>Analyse de la symbolique des parties du corps</h3>")

    occurrences <- table(data_hymne$tokens)

    cat("<ul>")
    for (mot in names(occurrences)) {
        if (mot %in% c("manus", "caput", "corpus", "sanguis")) {
            cat(paste("<li><b>", mot, ":</b> mentionné", occurrences[[mot]], "fois."))
            if (mot == "manus") cat(" La main est un symbole de bénédiction et d'action dans la foi chrétienne.")
            if (mot == "caput") cat(" La tête symbolise l'autorité divine et le martyr des saints.")
            if (mot == "corpus") cat(" Le corps représente le sacrifice du Christ.")
            if (mot == "sanguis") cat(" Le sang est le symbole du martyre et du rachat des péchés.")
            cat("</li>")
        }
    }
    cat("</ul>")
}

cat("</body></html>")
sink()

# 📌 MESSAGE FINAL
message("📊 ✅ Analyse KWIC terminée avec tableau, réseau lexical et wordcloud. Résultats : ", html_output)
