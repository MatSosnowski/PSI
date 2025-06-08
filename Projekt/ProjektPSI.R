#' ---
#' title: "Raport PSI"
#' author: "Mateusz Sosnowski"
#' date:   "`r Sys.Date()`"
#' output:
#'   html_document:
#'     df_print: paged
#'     theme: spacelab      
#'     highlight: breezedark  
#'     toc: true            
#'     toc_depth: 3
#'     toc_float:
#'       collapsed: false
#'       smooth_scroll: true
#'     code_folding: show    
#'     number_sections: false 
#' ---

knitr::opts_chunk$set(
  message = FALSE,
  warning = FALSE
)


#' # Wymagane pakiety
# Wymagane pakiety ----
library(rvest)
library(stringr)
library(readr)
library(tm)           
library(SnowballC)    
library(cluster)      
library(wordcloud)    
library(factoextra)   
library(RColorBrewer) 
library(ggplot2)      
library(dplyr)        
library(ggrepel)      
library(DT)           

#' # Dane tekstowe
# Pobieranie plików tekstowych ----



#' # Lista przemówień

speeches <- list(
  "We_Shall_Fight_on_the_Beaches" = "https://www.winstonchurchill.org/resources/speeches/1940-the-finest-hour/we-shall-fight-on-the-beaches/",
  "Their_Finest_Hour" = "https://www.winstonchurchill.org/resources/speeches/1940-the-finest-hour/their-finest-hour/",
  "Blood_Toil_Tears_and_Sweat" = "https://www.winstonchurchill.org/resources/speeches/1940-the-finest-hour/blood-toil-tears-and-sweat/",
  "Four_Freedoms" = "https://teachingamericanhistory.org/document/the-four-freedoms/",
  "Day_of_Infamy" = "https://time.com/4593483/pearl-harbor-franklin-roosevelt-infamy-speech-attack/",
  "July_3_1941_Broadcast" = "https://www.marxists.org/reference/archive/stalin/works/1941/07/03.htm"
)

#' #  Funkcja do scrapowania tekstu przemówienia
scrape_speech <- function(url, file_name) {
  page <- read_html(url)
  
  content_nodes <- html_nodes(page, ".article-body p")
  
  if (length(content_nodes) == 0) {
    content_nodes <- html_nodes(page, "p")
  }
  
  text <- html_text(content_nodes, trim = TRUE)
  text <- str_trim(text)
  text <- text[text != ""]  
  
  write_lines(text, paste0(file_name, ".txt"))
}

# Przetwarzanie wszystkich podanych przemówień
for (name in names(speeches)) {
  url <- speeches[[name]]
  scrape_speech(url, name)
}

# Pliki txt zostały ręcznie ze zbędnych fragmentów nie będących przemówieniami


# Ładowanie plików

docs <- DirSource("textfolder")

# Utwórz korpus dokumentów tekstowych
corpus <- VCorpus(docs)

# Korpus
inspect(corpus)

corpus[[1]][[1]][1:6]

# Przetwarzanie i oczyszczanie tekstu ----


# Zapewnienie kodowania w całym korpusie
corpus <- tm_map(corpus, content_transformer(function(x) iconv(x, to = "UTF-8", sub = "byte")))


# Funkcja do zamiany znaków na spację
toSpace <- content_transformer(function (x, pattern) gsub(pattern, " ", x))

# linia pionowa
corpus <- tm_map(corpus, toSpace, "\\|")

# tabulatory
corpus <- tm_map(corpus, toSpace, "[ \t]{2,}")

# tylko ukośnik odwrotny (np. po http)
corpus <- tm_map(corpus, toSpace, "/")

# inne pozostałości
corpus <- tm_map(corpus, toSpace, "www")
corpus <- tm_map(corpus, toSpace, "~")
corpus <- tm_map(corpus, toSpace, "â€“")


# Sprawdzenie
corpus[[1]][[1]][1:6]


corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, removeWords, stopwords("english"))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, stripWhitespace)

inspect(corpus)


#' # Stemming 


# kopia korpus
corpus_copy <- corpus

# stemming w oryginalnym korpusie
corpus_stemmed <- tm_map(corpus, stemDocument)


# Sprawdzenie
corpus[[1]][[1]][1:6]
# Sprawdzenie
corpus_stemmed[[1]][[1]][1:6]


# Uzupełnienie rdzeni 

# funkcja pomocnicza: wykonuje stemCompletion linia po linii
complete_stems <- content_transformer(function(x, dict) {
  x <- unlist(strsplit(x, " "))                  # podziel na słowa
  x <- stemCompletion(x, dictionary = corpus_copy, type="longest") # uzupełnij rdzenie
  paste(x, collapse = " ")                       # połącz z powrotem w tekst
})

# stemCompletion do każdego dokumentu w korpusie
corpus_completed <- tm_map(corpus_stemmed, complete_stems, dict = corpus_copy)

# usuń NA
corpus_completed <- tm_map(corpus_completed, toSpace, "NA")
corpus_completed <- tm_map(corpus_completed, stripWhitespace)

# Sprawdzenie
corpus_completed[[1]][[1]][1]

# Porównaj:
corpus[[1]][[1]][1:6]
corpus_stemmed[[1]][[1]][1:6]

# Wybieram corpus_completed 

#' # Tokenizacja
# Macierze częstości TDM i DTM 

# a) Funkcja TermDocumentMatrix()

tdm <- TermDocumentMatrix(corpus_completed)
tdm
inspect(tdm)

tdm_m <- as.matrix(tdm)

tdm_m[1:6, 1:6]

# b) Funkcja DocumentTermMatrix() 
# dokumenty = wiersze, tokeny = kolumny
dtm <- DocumentTermMatrix(corpus_completed)
dtm
inspect(dtm)

dtm_m <- as.matrix(dtm)

dtm_m[1:6, 1:6]

#' #  2. Zliczanie częstości słów


# Można zliczyć same częstości słów w macierzach
# dla TDM i DTM da to identyczny rezultat
v <- sort(rowSums(tdm_m), decreasing = TRUE)
tdm_df <- data.frame(word = names(v), freq = v)
head(tdm_df, 10)

v2 <- sort(colSums(dtm_m), decreasing = TRUE)
dtm_df <- data.frame(word = names(v2), freq = v2)
head(dtm_df, 10)


# 3. Eksploracyjna analiza danych ----
# (Exploratory Data Analysis, EDA)


# Chmura słów (globalna)
wordcloud(words = tdm_df$word, freq = tdm_df$freq, min.freq = 7, 
          colors = brewer.pal(8, "Set2"))


#' # Klastrowanie

  
  # Dobór liczby klastrów
  # Metoda sylwetki (silhouette)
  fviz_nbclust(t(dtm_m), kmeans, method = "silhouette") +
  labs(title = "Dobór liczby klastrów", subtitle = "Metoda sylwetki")


#' # Funkcja klastrowania

perform_clustering <- function(k) {
  set.seed(123)
  
  # Klastrowanie
  klastrowanie <- kmeans(dtm_m, centers = k)
  
  # Nazwy dokumentów
  document_names <- names(corpus)
  
  # Ramka danych: dokumenty i klastry
  documents_clusters <- data.frame(
    Dokument = document_names,
    Klaster = as.factor(klastrowanie$cluster),  # jako faktor do wykresu
    stringsAsFactors = FALSE
  )
  
  # Informacje o klastrach
  cluster_info <- lapply(1:k, function(i) {
    cluster_docs_idx <- which(klastrowanie$cluster == i)
    cluster_docs <- dtm_m[cluster_docs_idx, , drop = FALSE]
    word_freq <- sort(colSums(cluster_docs), decreasing = TRUE)
    top_words <- paste(names(word_freq)[1:5], collapse = ", ")
    data.frame(
      Klaster = as.character(i),
      Liczba_dokumentów = length(cluster_docs_idx),
      Top_5_słów = top_words,
      stringsAsFactors = FALSE
    )
  })
  
  cluster_info_df <- do.call(rbind, cluster_info)
  
  # Połączenie danych
  documents_clusters_z_info <- left_join(documents_clusters, cluster_info_df, by = "Klaster")
  
  # Interaktywna tabela
  print(
    datatable(documents_clusters_z_info,
              caption = paste("Dokumenty i przypisanie do klastrów (k =", k, ")"),
              rownames = FALSE,
              options = list(pageLength = 10))
  )
  
  # Wizualizacja klastrów
  print(
    fviz_cluster(list(data = dtm_m, cluster = klastrowanie$cluster),
                 geom = "point",
                 main = paste("Wizualizacja klastrów dokumentów (k =", k, ")"))
  )
  
  # Chmury słów
  for (i in 1:k) {
    cluster_docs_idx <- which(klastrowanie$cluster == i)
    cluster_docs <- dtm_m[cluster_docs_idx, , drop = FALSE]
    word_freq <- colSums(cluster_docs)
    wordcloud(names(word_freq), freq = word_freq, 
              max.words = 15, colors = brewer.pal(8, "Dark2"))
    title(paste("Chmura słów - Klaster", i))
  }
  
  # Wykres słupkowy przypisania dokumentów
  print(
    ggplot(documents_clusters, aes(x = reorder(Dokument, Klaster), fill = Klaster)) +
      geom_bar(stat = "count", width = 0.7) +
      coord_flip() +
      labs(title = paste("Przypisanie dokumentów do klastrów (k =", k, ")"),
           x = "Dokument",
           y = "Liczba wystąpień (powinna wynosić 1)",
           fill = "Klaster") +
      theme_minimal(base_size = 13)
  )
}

#' #  Klastrowanie dla k=2

perform_clustering(2)

#' #  Klastrowanie dla k=3

perform_clustering(3)

#' #  Klastrowanie dla k=4

perform_clustering(4)
