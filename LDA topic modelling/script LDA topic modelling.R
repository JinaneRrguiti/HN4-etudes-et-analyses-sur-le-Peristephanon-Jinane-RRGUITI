# Chargement des packages nécessaires
pacman::p_load(here, scales, tidytext, tidyverse, tm, topicmodels, udpipe, xml2, knitr)

# Définition du chemin du dossier contenant les fichiers XML
folder_path <- "C:/Users/33767/Desktop/cours master 2/Humanités Numériques/hn4/hymnes en xml"
if (!dir.exists(folder_path)) {
    stop("Le dossier spécifié n'existe pas : ", folder_path)
}

# Création du dossier de sortie pour le rapport HTML, renommé "LDA topic modelling"
output_dir <- "C:/Users/33767/Desktop/cours master 2/Humanités Numériques/hn4/LDA topic modelling"
if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
}
html_output <- file.path(output_dir, "topics_report.html")

# Lecture et extraction des textes depuis les fichiers XML
extract_text <- function(file) {
    doc <- read_xml(file)
    text_nodes <- xml_find_all(doc, ".//l | .//p | .//lg")
    text <- paste(xml_text(text_nodes), collapse = " ")
    return(text)
}

xml_files <- list.files(folder_path, pattern = "\\.xml$", full.names = TRUE)
if (length(xml_files) == 0) {
    stop("Aucun fichier XML trouvé dans ", folder_path)
}

corpus_df <- tibble(
    doc_id = basename(xml_files),
    text   = map_chr(xml_files, extract_text)
)

# Nettoyage initial du texte
clean_text <- function(txt) {
    txt <- tolower(txt)
    txt <- gsub("[[:punct:]]", " ", txt)
    txt <- gsub("[0-9]", " ", txt)
    txt <- gsub("\\s+", " ", txt)
    txt
}
corpus_df <- corpus_df %>% mutate(text = map_chr(text, clean_text))

# Lemmatisation du corpus avec udpipe (pour le latin)
ud_model <- udpipe_download_model(language = "latin")
ud_model <- udpipe_load_model(ud_model$file_model)

annotation <- udpipe_annotate(ud_model, x = corpus_df$text, doc_id = corpus_df$doc_id)
anno_df <- as.data.frame(annotation)

# Reconstruction du texte lemmatisé par document (regroupement des lemmes)
lemmatized_df <- anno_df %>%
    group_by(doc_id) %>%
    summarise(lem_text = paste(lemma, collapse = " ")) %>%
    ungroup()
corpus_df <- left_join(corpus_df, lemmatized_df, by = "doc_id")

# Création du corpus tm à partir du texte lemmatisé
corpus <- Corpus(VectorSource(corpus_df$lem_text))
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeNumbers)

# Filtrage avec une liste étendue de stopwords en latin
latin_stopwords <- unique(c(
    # Prépositions, conjonctions, adverbes, etc.
    "a", "ab", "ac", "ad", "at", "ante", "circum", "contra", "de", "e", "ex",
    "et", "in", "inter", "intro", "per", "post", "pro", "sub", "super", "trans", "ut", "cum",
    "enim", "vero", "nam", "atque", "aut", "autem", "sed", "si", "tamen", "donec", "quidem",
    "vel", "velut", "modo", "nunc", "tam", "adeo",
    # Mots fréquents peu informatifs
    "est", "nec", "iam", "ait", "sibi", "quid", "quod", "quos", "his", "esse", "dum",
    "hic", "haec", "hoc", "ille", "illa", "illud", "iste", "ista", "istud", "ipse", "ipsa", "ipsum",
    "tibi", "sic", "quam", "quo", "hinc", "tunc", "quem", "qui", "tu",
    # Termes supplémentaires indésirables
    "neque", "non", "inde", "quis", "sum", "omnis", "jam", "sui", "suus",
    "noster", "ego", "nos", "tuus"
))
corpus <- tm_map(corpus, removeWords, latin_stopwords)
corpus <- tm_map(corpus, stripWhitespace)

# Création de la matrice document-terme (DTM)
dtm <- DocumentTermMatrix(corpus)
dtm <- removeSparseTerms(dtm, 0.98)
dtm <- dtm[rowSums(as.matrix(dtm)) > 0, ]
print(dim(dtm))

# Application du modèle LDA (Topic Modelling)
k <- 4  # Nombre de topics souhaités
set.seed(1234)
lda_model <- LDA(dtm, k = k, control = list(seed = 1234))

# Extraction des probabilités par topic (β)
# La valeur beta représente la probabilité qu’un mot apparaisse dans un topic donné.
topics <- tidy(lda_model, matrix = "beta")
if ("word" %in% names(topics)) {
    topics <- topics %>% rename(term = word)
}

# Sélection des 10 termes les plus représentatifs pour chaque topic
top_terms <- topics %>%
    group_by(topic) %>%
    top_n(10, beta) %>%
    ungroup() %>%
    arrange(topic, -beta)
print(top_terms)

# Génération d'un document HTML avec les résultats et commentaires interprétatifs
sink(html_output)
cat("<html><head><meta charset='utf-8'><title>Analyse des 4 Topics</title></head><body>\n")
cat("<h1>Analyse des 4 Topics extraits du corpus</h1>\n")
cat("<p>Le modèle LDA a été appliqué pour extraire 4 topics. Pour chaque topic, nous présentons un tableau des 10 termes les plus représentatifs et leurs valeurs <em>beta</em>.<br>")
cat("La valeur <strong>beta</strong> correspond à la probabilité qu'un mot apparaisse dans un topic donné. Un beta élevé indique que le mot est très caractéristique du topic.</p>\n")

# Fonction pour générer un commentaire spécifique pour chaque topic
generate_commentary <- function(topic_num) {
    if (topic_num == 1) {
        return("Topic 1 : Ce topic semble se concentrer sur des notions spirituelles et mystiques, avec des termes évoquant la transcendance et la quête du divin.")
    } else if (topic_num == 2) {
        return("Topic 2 : Ce topic est axé sur le corporel et la matière, suggérant des thèmes liés aux aspects physiques et matériels des hymnes.")
    } else if (topic_num == 3) {
        return("Topic 3 : Ce topic pourrait refléter des aspects structuraux et organisationnels du texte, liés aux formes littéraires ou aux rituels.")
    } else if (topic_num == 4) {
        return("Topic 4 : Ce topic semble aborder des thèmes émotionnels ou conflictuels, peut-être en lien avec le sacrifice, la souffrance ou la transformation.")
    } else {
        return("Veuillez analyser ces termes pour identifier le thème sous-jacent.")
    }
}

# Boucle sur chaque topic pour afficher son tableau et un commentaire interprétatif spécifique
for (i in sort(unique(top_terms$topic))) {
    cat(paste0("<h2>Topic ", i, "</h2>\n"))
    cat("<p>")
    cat(generate_commentary(i))
    cat("</p>\n")

    topic_table <- top_terms %>% filter(topic == i) %>% select(term, beta)
    table_html <- kable(topic_table, format = "html",
                        table.attr = "style='width:50%; border:1px solid black; margin-bottom:20px;'",
                        caption = paste("Top 10 termes pour le Topic", i))
    cat(table_html, "\n")
}
cat("</body></html>")
sink()

cat("Le rapport HTML a été généré ici : ", html_output, "\n")
