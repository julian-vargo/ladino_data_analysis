# Tutorial: Analyzing Ladino Data in R
# Tutorial: Analizar Data de Ladino kon R
# Vargo, Julian (Oct 2025)
# Ladino/Judeo-Spanish Working Group. University of California, Berkeley
# Grupo de Lavoro en Ladino/Judeo-Spanish. Universita de Kalifornia, Berkeley.

#################################

# First things first, we must load our packages. If you already installed the packages, no need to do this.
# Primero, kale kargar los paketes. Si ya deskargatesh los paketes, no tenesh ke azer este paso.

# install.packages("ggplot")
# install.packages("dplyr")
# install.packages("lmtest")
# install.packages("readr")
# install.packages("stringr")
# install.packages("wordcloud")
library(ggplot2)
library(dplyr)
library(lmtest)
library(readr)
library(stringr)
library(wordcloud)

# Now, you must load the datasets for the tutorial. In Ladino I'll call them data table "dt", but in English they're often called dataframes.
# Agora, kale deskargar las tavlas de data del tutorial. Las yamo "dt" data-tavla
text_dt <- read_csv( "https://raw.githubusercontent.com/julian-vargo/ladino_data_analysis/refs/heads/main/data/el_amaneser.csv")
phonetic_dt <- read_csv("https://raw.githubusercontent.com/julian-vargo/ladino_data_analysis/refs/heads/main/data/schroeter_vargo_lsa_2026.csv")


## Chapter 1: Phonetic Analysis of Ladino Vowels
## Kapitolo 1: Analiz Fonetika de Vokales en Ladino

# Remove consonants and stress marking (1 and 0)
# Kitar las konsonantes i markadores de intonasyon (1 i 0)
vowels = c("A", "E", "I", "O", "U")
phonetic_dt <- phonetic_dt %>% mutate(
  phoneme = str_replace(phoneme, "1", ""),
  phoneme = str_replace(phoneme, "0", "")
) %>% filter(phoneme %in% vowels)

# Remove values that say --undefined-- or 99999
# Kitar las kadenas (variable de teksto) i los numeros ke no son definidos
phonetic_dt <- phonetic_dt %>% mutate(
  f1_50 = str_replace(f1_50, "--undefined--", ""),
  f1_50 = str_replace(f1_50, "99999", ""),
  f2_50 = str_replace(f2_50, "--undefined--", ""),
  f2_50 = str_replace(f2_50, "99999", "")
)

# Remove vowels greater than 2 standard deviations from each vocalic centroid
# Kitar las vokales ke tyenen valores ke son mas de dos desviasyones standards
phonetic_dt <- phonetic_dt %>% mutate(
  f1_50 = as.numeric(f1_50),
  f2_50 = as.numeric(f2_50)) %>% 
  group_by(phoneme, file_name) %>%
  mutate(
    f1_centroid = median(f1_50, na.rm = TRUE),
    f2_centroid = median(f2_50, na.rm = TRUE)) %>% 
  mutate(distance_from_centroid = sqrt((f1_50 - f1_centroid)^2 + (f2_50 - f2_centroid)^2)) %>%
  mutate(outlier_limit = 2 * sd(distance_from_centroid, na.rm = TRUE)) %>%
  filter(distance_from_centroid < outlier_limit) %>% ungroup()

# Normalize the formants around each speaker's vowel space center and z-normalize
# Normalizar los formantes sovre el sentro de kada favlante i poner a una eskala z-normalizada
phonetic_dt <- phonetic_dt %>% group_by(file_name) %>% mutate(
  f1_50_min = min(f1_50, na.rm = TRUE),
  f1_50_max = max(f1_50, na.rm = TRUE),
  f2_50_min = min(f2_50, na.rm = TRUE),
  f2_50_max = max(f2_50, na.rm = TRUE)) %>% 
  mutate(
  f1_center = (f1_50_max + f1_50_min)/2,
  f2_center = (f2_50_max + f2_50_min)/2) %>% 
  mutate(
  f1_dfc = f1_50 - f1_center,
  f2_dfc = f2_50 - f2_center,
  f1_sd = sd(f1_dfc, na.rm=T),
  f2_sd = sd(f2_dfc, na.rm=T)) %>%
  mutate(
  f1_z = f1_dfc / f1_sd,
  f2_z = f2_dfc / f2_sd) %>% ungroup()

# Create a set of average values for each vowel phoneme
# Kriar una data-tavla de valores promedias para kada fonema vokalika
centers <- phonetic_dt %>%
  group_by(phoneme) %>%
  summarise(
    f1_mean = mean(f1_z, na.rm = TRUE),
    f2_mean = mean(f2_z, na.rm = TRUE))

# Now, you will plot your first Ladino vowel chart!
# Agora, trazarash vuestro primer grafik de las vokales de Ladino!
vowel_graph <- ggplot(phonetic_dt, aes(x = f2_z, y = f1_z, color = phoneme)) +
  geom_point(alpha = 0.2) +
  stat_ellipse(aes(group = phoneme, fill = phoneme),
               geom = "polygon", alpha = 0.05, level = 0.5) + 
  geom_label(data = centers, aes(x = f2_mean, y = f1_mean, label = phoneme, fill = phoneme), 
             color = "white", fontface = "bold", alpha = 0.8) +
  scale_x_reverse() +
  scale_y_reverse() +
  theme_minimal() +
  labs(x = "F2 Midpoint", y = "F1 Midpoint", fill = "Phoneme", color = "Phoneme")

print(vowel_graph)


## Chapter 2: Textual Analysis of El Amaneser Newspaper
## Kapitolo 2 Analiz de Teksto del Jurnal El Amaneser

# Let's examine our dataset
# Mirar todos los biervos en el jurnal
print(text_dt)

# Let's set everything to lower-case
# Transformar las letras en miniskulas
text_dt <- text_dt %>%
  mutate(word = tolower(word))

# Create a dictionary of unique tokens
# Kriar un diksyonaryo/sozluk de los biervos unikos
dictionary <- unique(text_dt)

# Create a frequency column in our text dataframe
# Kriar una kolumna de frekuensias en muestra data-tavla de texto
frequencies <- text_dt %>%
  count(word, name = "frequency") %>%
  arrange(desc(frequency))

print(frequencies)

# Twenty most common words
#  Veinte biervos mas komunes
top_20 <- frequencies %>% 
  slice_max(order_by = frequency, n = 20)

ggplot(data=top_20, aes(x = reorder(word, frequency), y = frequency)) +
geom_col(fill = "navy") +
labs(
  title = "20 Most Frequent Words in Ladino",
  x = "Word",
  y = "Frequency") +
theme_minimal()

# Create a word cloud of the top 100 words in the newspaper
# Kriar un nuvle de biervos de los 100 biervos mas komunes del jurnal
wordcloud(words = frequencies$word,
          freq = frequencies$frequency,
          max.words = 100)


