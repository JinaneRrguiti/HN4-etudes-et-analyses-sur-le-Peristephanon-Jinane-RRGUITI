# 📌 CHARGEMENT DES PACKAGES
library(xml2)
library(tidyverse)
library(tidytext)

# 📌 CHEMIN DU DOSSIER CONTENANT LES FICHIERS XML
folder_path <- "C:/Users/33767/Desktop/cours master 2/Humanités Numériques/hymnes en xml"

# 📌 LISTER LES FICHIERS XML
xml_files <- list.files(folder_path, pattern = "\\.xml$", full.names = TRUE)

# 📌 FONCTION D'EXTRACTION DU TEXTE PAR HYMNE
extract_poem_data <- function(file) {
    doc <- read_xml(file)
    titre <- xml_text(xml_find_first(doc, ".//head"))
    vers <- xml_text(xml_find_all(doc, ".//l | .//p | .//lg"))
    if (length(vers) == 0) {
        warning(paste("⚠ Aucun vers extrait pour", basename(file)))
        return(NULL)
    }
    data.frame(poeme = basename(file), titre = titre, texte = paste(vers, collapse = " "), stringsAsFactors = FALSE)
}

# 📌 EXTRACTION DES TEXTES
all_poems <- map_dfr(xml_files, extract_poem_data)
if (nrow(all_poems) == 0) stop("🚨 Aucun texte valide extrait des fichiers XML.")

# 📌 NETTOYAGE DU TEXTE
clean_text <- function(text) {
    text <- tolower(text)  # Minuscule
    text <- gsub("[[:punct:]]", "", text)  # Supprime ponctuation
    text <- gsub("[0-9]", "", text)  # Supprime chiffres
    text <- str_split(text, "\\s+")  # Tokenisation
    return(unlist(text))
}

all_poems_cleaned <- all_poems %>%
    mutate(tokens = map(texte, clean_text)) %>%
    unnest(tokens) %>%
    filter(tokens != "")

# 📌 STOPWORDS GRAMMATICAUX : EXCLURE LES MOTS NON LEXICAUX
stopwords_grammaticaux <- c("et", "aut", "ac", "sed", "nec", "si", "cum", "non", "ut", "in", "ad", "per", "ex", "de",
                            "sub", "pro", "sine", "nam", "enim", "quod", "quia", "atque", "quoque", "tamen", "post",
                            "ante", "vero", "quidem", "dum", "nunc", "ergo", "itaque", "hinc", "igitur", "vel",
                            "inter", "inde", "qui", "quae", "quod", "cuius", "cui", "quem", "quam", "quo", "quibus",
                            "haec", "hoc", "hunc", "hanc", "hic", "ille", "illa", "illi", "ipse", "ipsa", "ipsum",
                            "istud", "eius", "eorum", "meus", "tuus", "suus", "noster", "vester", "aliquis", "quis",
                            "quid", "quicumque", "quilibet", "tibi", "quos", "his", "iam", "sic", "ubi", "nunc",
                            "iamque", "numquam", "semper", "tamen", "quidem", "huc", "usque", "unde", "tunc",
                            "est", "sunt", "esse", "erat", "erant", "fuit", "fuerunt", "fit", "fiunt", "fieri",
                            "habere", "habet", "habeo", "dicere", "dixit", "facere", "fecit", "videre", "vidit",
                            "venit", "ire", "ii", "eunt", "ire", "sit", "ait")

# 📌 FILTRAGE DES STOPWORDS
tokens_filtered <- all_poems_cleaned %>%
    filter(!tokens %in% stopwords_grammaticaux, str_length(tokens) > 2) %>%
    count(poeme, tokens, sort = TRUE)

# 📌 CRÉATION DU DOSSIER POUR VISUALISATIONS
output_folder <- file.path(folder_path, "visualisations")
dir.create(output_folder, showWarnings = FALSE)

# 📌 ANALYSE DES FRÉQUENCES ABSOLUES (TOP 10)
tokens_absolute <- tokens_filtered %>%
    group_by(tokens) %>%
    summarise(n = sum(n)) %>%
    arrange(desc(n)) %>%
    slice_max(order_by = n, n = 10) %>%
    ungroup()

# 📌 VISUALISATION : 10 MOTS LES PLUS FRÉQUENTS (FRÉQUENCES ABSOLUES)
ggplot(tokens_absolute, aes(x = reorder(tokens, n), y = n)) +
    geom_col(fill = "blue") +
    coord_flip() +
    labs(
        title = "Top 10 mots les plus fréquents (Fréquences Absolues)",
        x = "Mots",
        y = "Fréquence Absolue"
    ) +
    theme_minimal()
ggsave(filename = file.path(output_folder, "frequence_absolue_top10.png"))

# 📌 ANALYSE DES FRÉQUENCES RELATIVES (TOP 5 PAR HYMNE)
tokens_frequence_relative <- tokens_filtered %>%
    group_by(poeme) %>%
    mutate(freq_relative = (n / sum(n)) * 100) %>%
    arrange(poeme, desc(freq_relative))

tokens_frequence_relative_top5 <- tokens_frequence_relative %>%
    group_by(poeme) %>%
    slice_max(order_by = freq_relative, n = 5) %>%
    ungroup()

# 📌 VISUALISATIONS DES FRÉQUENCES RELATIVES PAR HYMNE
for (hymne in unique(tokens_frequence_relative_top5$poeme)) {
    data_hymne <- tokens_frequence_relative_top5 %>% filter(poeme == hymne)

    plot <- ggplot(data_hymne, aes(x = reorder(tokens, freq_relative), y = freq_relative)) +
        geom_col(fill = "red") +
        coord_flip() +
        labs(
            title = paste("Top 5 mots fréquents -", hymne),
            x = "Mots",
            y = "Fréquence Relative (%)"
        ) +
        theme_minimal()

    ggsave(filename = file.path(output_folder, paste0(hymne, "_frequence_relative.png")), plot)
}

# 📌 CRÉATION DU RAPPORT HTML AVEC COMMENTAIRES EXPLICATIFS
rapport_path <- file.path(output_folder, "rapport_frequences.html")

rapport_content <- c(
    "<html><head><title>Analyse des Fréquences du Corpus</title></head><body>",
    "<h1>Analyse des Fréquences du Corpus</h1>",
    "<p><strong>Date :</strong> ", as.character(Sys.Date()), "</p>",
    "<h2>Fréquences Absolues</h2>",
    "<p>Les fréquences absolues montrent les mots les plus courants dans l'ensemble du corpus. Ces mots sont souvent des termes génériques liés aux thématiques des hymnes, comme <strong>sanctus</strong>, <strong>martyr</strong> ou <strong>gloria</strong>, qui apparaissent fréquemment dans tous les poèmes.</p>",
    "<h2>Fréquences Relatives</h2>",
    "<p>Les fréquences relatives permettent de voir quels mots dominent chaque hymne individuellement. Par exemple, si un hymne est dédié à un saint particulier, le nom du saint est souvent le mot dominant dans cet hymne.</p>",
    "<h2>Analyse</h2>",
    "<p>Comparativement, les fréquences absolues révèlent les thèmes récurrents dans l'ensemble du corpus, tandis que les fréquences relatives montrent des spécificités propres à chaque hymne.</p>",
    "</body></html>"
)

writeLines(rapport_content, rapport_path)

# 📌 MESSAGE FINAL
message("📊 ✅ Visualisations et rapport HTML générés dans : ", output_folder)
