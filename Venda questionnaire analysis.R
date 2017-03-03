# Analysis of questionnaire data for Predation by small mammalian carnivores in rural agro-ecosystems: An undervalued ecosystem service? Manuscript submitted to Ecosystem Services.

rm(list = ls())
library(dplyr)
library(ggplot2)
library(gmodels)

# Plot Fig. 3: Responses to question about seeing each species in each village
long <- read.csv("questionSummaryLong.csv")
long$species <- factor(long$species, levels = long$species[order(long$order)]) # orders species in intended order

p <- ggplot(long, aes(x=species, y=seenPercentage, fill=response)) +
  geom_bar(stat="identity") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5), legend.position="bottom", text = element_text(size=9), legend.margin=unit(0,"cm")) +
  xlab("Species") + ylab("Percentage of respondents") +
  labs(fill="Species seen") +
  facet_wrap( ~ village)
p
# Save plots
# ggsave(filename = "seen facet.png", plot = p, units = "mm", width = 90, height = 110, dpi = 500)
# ggsave(filename = "seen facet.eps", plot = p, units = "mm", width = 90, height = 110)


# Plot Fig. 4: Reasons why negative about each carnivore in each village
neg <- read.csv("posNeg.csv")
neg <- filter(neg, question == "negWhy", response != "N/A", response != "Kill pets ")
neg$species <- factor(neg$species, levels = neg$species[order(neg$orderOriginal)]) # orders species in intended order
library(ggplot2)
p <- ggplot(neg, aes(x=species, y=percent, fill=response)) +
  geom_bar(stat="identity") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5), legend.position="bottom", text = element_text(size=9), legend.margin=unit(0,"cm")) +
  # theme(legend.title = element_text(size = 8), legend.text = element_text(size = 7)) +
  xlab("Species") + ylab("Percentage of respondents") +
  labs(fill="Reason why negative") +
  facet_wrap( ~ village)
p
# Save plots
# ggsave(filename = "Reasons why negative.png", plot = p, units = "mm", width = 90, height = 110, dpi = 500)
# ggsave(filename = "Reasons why negative.eps", plot = p, units = "mm", width = 90, height = 110)

###

# Chi 2 tests for differences bewteen villages in frequency of responses to questionnaire

# Test for diffs in number of people that say that they have seen carnivore species
chi <- read.csv("chi.csv") # read data
unique(chi$species)
chi$response <- as.factor(chi$response) # formats necessary variables as factors
# Note: I pooled "No" and "Not sure" responses for these questions 
chi <- filter(chi, question == "seen")

# African civet
m <- filter(chi, species == "African civet"); m <- select(m, 4:5)
m <- data.matrix(m, rownames.force = NA)
library(gmodels)
CrossTable(m, fisher = TRUE, chisq = TRUE, expected = TRUE, sresid = TRUE, format = "SPSS")
# NS

# African wildcat
m <- filter(chi, species == "African wild cat"); m <- select(m, 4:5)
m <- data.matrix(m, rownames.force = NA)
CrossTable(m, fisher = TRUE, chisq = TRUE, expected = TRUE, sresid = TRUE, format = "SPSS")
# NS

# Black backed jackal
m <- filter(chi, species == "Black backed jackal"); m <- select(m, 4:5)
m <- data.matrix(m, rownames.force = NA)
CrossTable(m, fisher = TRUE, chisq = TRUE, expected = TRUE, sresid = TRUE, format = "SPSS")
# NS

# Banded mongoose
m <- filter(chi, species == "Banded mongoose"); m <- select(m, 4:5)
m <- data.matrix(m, rownames.force = NA)
CrossTable(m, fisher = TRUE, chisq = TRUE, expected = TRUE, sresid = TRUE, format = "SPSS")
# Sig

# Caracal
m <- filter(chi, species == "Caracal"); m <- select(m, 4:5)
m <- data.matrix(m, rownames.force = NA)
CrossTable(m, fisher = TRUE, chisq = TRUE, expected = TRUE, sresid = TRUE, format = "SPSS")
# NS

# Domestic cat
m <- filter(chi, species == "Domestic cat"); m <- select(m, 4:5)
m <- data.matrix(m, rownames.force = NA)
CrossTable(m, fisher = TRUE, chisq = TRUE, expected = TRUE, sresid = TRUE, format = "SPSS")
# NS

# Domestic dog
m <- filter(chi, species == "Domestic dog"); m <- select(m, 4:5)
m <- data.matrix(m, rownames.force = NA)
CrossTable(m, fisher = TRUE, chisq = TRUE, expected = TRUE, sresid = TRUE, format = "SPSS")
# NS

# Dwarf mongoose
m <- filter(chi, species == "Dwarf mongoose"); m <- select(m, 4:5)
m <- data.matrix(m, rownames.force = NA)
CrossTable(m, fisher = TRUE, chisq = TRUE, expected = TRUE, sresid = TRUE, format = "SPSS")
# NS

# Honey badger
m <- filter(chi, species == "Honey badger"); m <- select(m, 4:5)
m <- data.matrix(m, rownames.force = NA)
CrossTable(m, fisher = TRUE, chisq = TRUE, expected = TRUE, sresid = TRUE, format = "SPSS")
# NS

# Serval
m <- filter(chi, species == "Serval"); m <- select(m, 4:5)
m <- data.matrix(m, rownames.force = NA)
CrossTable(m, fisher = TRUE, chisq = TRUE, expected = TRUE, sresid = TRUE, format = "SPSS")
# NS

# Slender mongoose 
m <- filter(chi, species == "Slender mongoose"); m <- select(m, 4:5)
m <- data.matrix(m, rownames.force = NA)
CrossTable(m, fisher = TRUE, chisq = TRUE, expected = TRUE, sresid = TRUE, format = "SPSS")
# NS

# Striped polecat
m <- filter(chi, species == "Striped polecat"); m <- select(m, 4:5)
m <- data.matrix(m, rownames.force = NA)
CrossTable(m, fisher = TRUE, chisq = TRUE, expected = TRUE, sresid = TRUE, format = "SPSS")
# Sig

# Small spotted genet
m <- filter(chi, species == "Small spotted genet"); m <- select(m, 4:5)
m <- data.matrix(m, rownames.force = NA)
CrossTable(m, fisher = TRUE, chisq = TRUE, expected = TRUE, sresid = TRUE, format = "SPSS")
# Sig

# Water mongoose
m <- filter(chi, species == "Water mongoose"); m <- select(m, 4:5)
m <- data.matrix(m, rownames.force = NA)
CrossTable(m, fisher = TRUE, chisq = TRUE, expected = TRUE, sresid = TRUE, format = "SPSS")
# 

# White tailed mongoose
m <- filter(chi, species == "White tailed mongoose"); m <- select(m, 4:5)
m <- data.matrix(m, rownames.force = NA)
CrossTable(m, fisher = TRUE, chisq = TRUE, expected = TRUE, sresid = TRUE, format = "SPSS")
# 

# Yellow mongoose
m <- filter(chi, species == "Yellow mongoose"); m <- select(m, 4:5)
m <- data.matrix(m, rownames.force = NA)
CrossTable(m, fisher = TRUE, chisq = TRUE, expected = TRUE, sresid = TRUE, format = "SPSS")
# Sig

###

# Chi 2 test for difference between villages in number of people that say carnivores benefit the community
chi <- read.csv("chi.csv") # read data
chi$response <- as.factor(chi$response) # formats necessary variables as factors
# Note: I pooled "No" and "Not sure" responses for these questions 
chi <- filter(chi, question == "pos")

# Domestic cat
m <- filter(chi, species == "Domestic cat"); m <- select(m, 4:5)
m <- data.matrix(m, rownames.force = NA)
library(gmodels)
CrossTable(m, fisher = TRUE, chisq = TRUE, expected = TRUE, sresid = TRUE, format = "SPSS")
# Sig

# Domestic dog
m <- filter(chi, species == "Domestic dog"); m <- select(m, 4:5)
m <- data.matrix(m, rownames.force = NA)
CrossTable(m, fisher = TRUE, chisq = TRUE, expected = TRUE, sresid = TRUE, format = "SPSS")
# 
m

###

# Test for difs in number of people that say carnivores kill rodents
chi <- read.csv("chi.csv") # read data
unique(chi$species)
chi$response <- as.factor(chi$response) # formats necessary variables as factors
# Note: I pooled "No" and "Not sure" responses for these questions 
chi <- filter(chi, question == "killRodents")

# African civet
m <- filter(chi, species == "African civet"); m <- select(m, 4:5)
m <- data.matrix(m, rownames.force = NA)
library(gmodels)
CrossTable(m, fisher = TRUE, chisq = TRUE, expected = TRUE, sresid = TRUE, format = "SPSS")
# NS

# African wildcat
m <- filter(chi, species == "African wild cat"); m <- select(m, 4:5)
m <- data.matrix(m, rownames.force = NA)
CrossTable(m, fisher = TRUE, chisq = TRUE, expected = TRUE, sresid = TRUE, format = "SPSS")
# NS

# Black backed jackal
m <- filter(chi, species == "Black backed jackal"); m <- select(m, 4:5)
m <- data.matrix(m, rownames.force = NA)
CrossTable(m, fisher = TRUE, chisq = TRUE, expected = TRUE, sresid = TRUE, format = "SPSS")
# NS

# Banded mongoose
m <- filter(chi, species == "Banded mongoose"); m <- select(m, 4:5)
m <- data.matrix(m, rownames.force = NA)
CrossTable(m, fisher = TRUE, chisq = TRUE, expected = TRUE, sresid = TRUE, format = "SPSS")
# Sig

# Caracal
m <- filter(chi, species == "Caracal"); m <- select(m, 4:5)
m <- data.matrix(m, rownames.force = NA)
CrossTable(m, fisher = TRUE, chisq = TRUE, expected = TRUE, sresid = TRUE, format = "SPSS")
# NS

# Domestic cat
m <- filter(chi, species == "Domestic cat"); m <- select(m, 4:5)
m <- data.matrix(m, rownames.force = NA)
CrossTable(m, fisher = TRUE, chisq = TRUE, expected = TRUE, sresid = TRUE, format = "SPSS")
# NS

# Domestic dog
m <- filter(chi, species == "Domestic dog"); m <- select(m, 4:5)
m <- data.matrix(m, rownames.force = NA)
CrossTable(m, fisher = TRUE, chisq = TRUE, expected = TRUE, sresid = TRUE, format = "SPSS")
# NS

# Dwarf mongoose
m <- filter(chi, species == "Dwarf mongoose"); m <- select(m, 4:5)
m <- data.matrix(m, rownames.force = NA)
CrossTable(m, fisher = TRUE, chisq = TRUE, expected = TRUE, sresid = TRUE, format = "SPSS")
# NS

# Honey badger
m <- filter(chi, species == "Honey badger"); m <- select(m, 4:5)
m <- data.matrix(m, rownames.force = NA)
CrossTable(m, fisher = TRUE, chisq = TRUE, expected = TRUE, sresid = TRUE, format = "SPSS")
# NS

# Serval
m <- filter(chi, species == "Serval"); m <- select(m, 4:5)
m <- data.matrix(m, rownames.force = NA)
CrossTable(m, fisher = TRUE, chisq = TRUE, expected = TRUE, sresid = TRUE, format = "SPSS")
# NS

# Slender mongoose 
m <- filter(chi, species == "Slender mongoose"); m <- select(m, 4:5)
m <- data.matrix(m, rownames.force = NA)
CrossTable(m, fisher = TRUE, chisq = TRUE, expected = TRUE, sresid = TRUE, format = "SPSS")
# NS

# Striped polecat
m <- filter(chi, species == "Striped polecat"); m <- select(m, 4:5)
m <- data.matrix(m, rownames.force = NA)
CrossTable(m, fisher = TRUE, chisq = TRUE, expected = TRUE, sresid = TRUE, format = "SPSS")
# Sig

# Small spotted genet
m <- filter(chi, species == "Small spotted genet"); m <- select(m, 4:5)
m <- data.matrix(m, rownames.force = NA)
CrossTable(m, fisher = TRUE, chisq = TRUE, expected = TRUE, sresid = TRUE, format = "SPSS")
# Sig

# Water mongoose
m <- filter(chi, species == "Water mongoose"); m <- select(m, 4:5)
m <- data.matrix(m, rownames.force = NA)
CrossTable(m, fisher = TRUE, chisq = TRUE, expected = TRUE, sresid = TRUE, format = "SPSS")
# NS

# White tailed mongoose
m <- filter(chi, species == "White tailed mongoose"); m <- select(m, 4:5)
m <- data.matrix(m, rownames.force = NA)
CrossTable(m, fisher = TRUE, chisq = TRUE, expected = TRUE, sresid = TRUE, format = "SPSS")
# Sig

# Yellow mongoose
m <- filter(chi, species == "Yellow mongoose"); m <- select(m, 4:5)
m <- data.matrix(m, rownames.force = NA)
CrossTable(m, fisher = TRUE, chisq = TRUE, expected = TRUE, sresid = TRUE, format = "SPSS")
# NS

###

# Test for diffs in number of people that say that they have been negatively impacted by carnivores
chi <- read.csv("chi.csv") # read data
unique(chi$species)
chi$response <- as.factor(chi$response) # formats necessary variables as factors
# Note: I pooled "No" and "Not sure" responses for these questions 
chi <- filter(chi, question == "neg")

# African civet
m <- filter(chi, species == "African civet"); m <- select(m, 4:5)
m <- data.matrix(m, rownames.force = NA)
library(gmodels)
CrossTable(m, fisher = TRUE, chisq = TRUE, expected = TRUE, sresid = TRUE, format = "SPSS")
# no sig diff between villages in num respondents that say this species have impacts them negatively

# African wildcat
m <- filter(chi, species == "African wild cat"); m <- select(m, 4:5)
m <- data.matrix(m, rownames.force = NA)
CrossTable(m, fisher = TRUE, chisq = TRUE, expected = TRUE, sresid = TRUE, format = "SPSS")
# sig

# Black backed jackal
m <- filter(chi, species == "Black backed jackal"); m <- select(m, 4:5)
m <- data.matrix(m, rownames.force = NA)
CrossTable(m, fisher = TRUE, chisq = TRUE, expected = TRUE, sresid = TRUE, format = "SPSS")
# NS

# Banded mongoose
m <- filter(chi, species == "Banded mongoose"); m <- select(m, 4:5)
m <- data.matrix(m, rownames.force = NA)
CrossTable(m, fisher = TRUE, chisq = TRUE, expected = TRUE, sresid = TRUE, format = "SPSS")
# SD

# Caracal
m <- filter(chi, species == "Caracal"); m <- select(m, 4:5)
m <- data.matrix(m, rownames.force = NA)
CrossTable(m, fisher = TRUE, chisq = TRUE, expected = TRUE, sresid = TRUE, format = "SPSS")
# NS

# Domestic cat
m <- filter(chi, species == "Domestic cat"); m <- select(m, 4:5)
m <- data.matrix(m, rownames.force = NA)
CrossTable(m, fisher = TRUE, chisq = TRUE, expected = TRUE, sresid = TRUE, format = "SPSS")
# NS

# Domestic dog
m <- filter(chi, species == "Domestic dog"); m <- select(m, 4:5)
m <- data.matrix(m, rownames.force = NA)
CrossTable(m, fisher = TRUE, chisq = TRUE, expected = TRUE, sresid = TRUE, format = "SPSS")
# NS

# Dwarf mongoose
m <- filter(chi, species == "Dwarf mongoose"); m <- select(m, 4:5)
m <- data.matrix(m, rownames.force = NA)
CrossTable(m, fisher = TRUE, chisq = TRUE, expected = TRUE, sresid = TRUE, format = "SPSS")
# Sig

# Honey badger
m <- filter(chi, species == "Honey badger"); m <- select(m, 4:5)
m <- data.matrix(m, rownames.force = NA)
CrossTable(m, fisher = TRUE, chisq = TRUE, expected = TRUE, sresid = TRUE, format = "SPSS")
# NS

# Serval
m <- filter(chi, species == "Serval"); m <- select(m, 4:5)
m <- data.matrix(m, rownames.force = NA)
CrossTable(m, fisher = TRUE, chisq = TRUE, expected = TRUE, sresid = TRUE, format = "SPSS")
# NS

# Slender mongoose 
m <- filter(chi, species == "Slender mongoose"); m <- select(m, 4:5)
m <- data.matrix(m, rownames.force = NA)
CrossTable(m, fisher = TRUE, chisq = TRUE, expected = TRUE, sresid = TRUE, format = "SPSS")
# NS

# Striped polecat
m <- filter(chi, species == "Striped polecat"); m <- select(m, 4:5)
m <- data.matrix(m, rownames.force = NA)
CrossTable(m, fisher = TRUE, chisq = TRUE, expected = TRUE, sresid = TRUE, format = "SPSS")
# NS

# Small spotted genet
m <- filter(chi, species == "Small spotted genet"); m <- select(m, 4:5)
m <- data.matrix(m, rownames.force = NA)
CrossTable(m, fisher = TRUE, chisq = TRUE, expected = TRUE, sresid = TRUE, format = "SPSS")
# NS

# Water mongoose
m <- filter(chi, species == "Water mongoose"); m <- select(m, 4:5)
m <- data.matrix(m, rownames.force = NA)
CrossTable(m, fisher = TRUE, chisq = TRUE, expected = TRUE, sresid = TRUE, format = "SPSS")
# NS

# White tailed mongoose
m <- filter(chi, species == "White tailed mongoose"); m <- select(m, 4:5)
m <- data.matrix(m, rownames.force = NA)
CrossTable(m, fisher = TRUE, chisq = TRUE, expected = TRUE, sresid = TRUE, format = "SPSS")
# Sig

# Yellow mongoose
m <- filter(chi, species == "Yellow mongoose"); m <- select(m, 4:5)
m <- data.matrix(m, rownames.force = NA)
CrossTable(m, fisher = TRUE, chisq = TRUE, expected = TRUE, sresid = TRUE, format = "SPSS")
# NS

###

# Test for difs in number of people that say that people kill carnivore species
chi <- read.csv("chi.csv") # read data
unique(chi$species)
chi$response <- as.factor(chi$response) # formats necessary variables as factors
# Note: I pooled "No" and "Not sure" responses for these questions 
chi <- filter(chi, question == "killed")

# African wildcat
m <- filter(chi, species == "African wild cat"); m <- select(m, 4:5)
m <- data.matrix(m, rownames.force = NA)
CrossTable(m, fisher = TRUE, chisq = TRUE, expected = TRUE, sresid = TRUE, format = "SPSS")
# NS

# Dwarf mongoose
m <- filter(chi, species == "Dwarf mongoose"); m <- select(m, 4:5)
m <- data.matrix(m, rownames.force = NA)
CrossTable(m, fisher = TRUE, chisq = TRUE, expected = TRUE, sresid = TRUE, format = "SPSS")
# NS

# Slender mongoose 
m <- filter(chi, species == "Slender mongoose"); m <- select(m, 4:5)
m <- data.matrix(m, rownames.force = NA)
CrossTable(m, fisher = TRUE, chisq = TRUE, expected = TRUE, sresid = TRUE, format = "SPSS")
# Sig

# Yellow mongoose
m <- filter(chi, species == "Yellow mongoose"); m <- select(m, 4:5)
m <- data.matrix(m, rownames.force = NA)
CrossTable(m, fisher = TRUE, chisq = TRUE, expected = TRUE, sresid = TRUE, format = "SPSS")
# NS

###

# Chi 2 test for difference between villages in frequency of responses to "Do you have any chickens?"
count <- matrix (c(37,21,45,24)) # Matrix contents
dim(count) <- c(2,2) # defines the dimensions of the matrix
count
dimnames(count) = list(
  # names the matrix dimensions
  c("Doesn't keep chickens", "Keeps chickens"),
  # adds row names
  c("kaNdengeza", "vyeboom"))
# adds column names
count

barplot(count, beside = TRUE, legend = TRUE,
        col = c("grey", "white")) # plots frequencies

library(gmodels)
CrossTable(count, fisher = TRUE, chisq = TRUE, expected = TRUE, sresid = TRUE, format = "SPSS") # runs Chi 2
# No sig diff between villages in number of respondents that keep chickens

###

# Wilcox rank sum test for diff num chickens (excluding non-chicken owners) owned in 2 villages
chickens <- read.csv("chickensWide.csv")

# Plot distribution of number of chickens kept in each village
library(ggplot2)
qplot(chickens$chickensKaNdengeza) # Looks different to normal distribution
qplot(chickens$chickensVyeboom) # Looks different to normal distribution
# Use non-parametric statistics

wilcox.test (chickens$chickensKaNdengeza, chickens$chickensVyeboom, exact = TRUE, conf.int = TRUE)
# Significant difference in number chickens kept by respondents in the two villages