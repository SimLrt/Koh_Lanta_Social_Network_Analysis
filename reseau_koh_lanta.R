# Lance ce script depuis le dossier qui contient le CSV.
library(igraph)

donnees <- read.csv(
  "kohlanta_stats_aventuriers.csv",
  sep = ";",
  dec = ".",
  fileEncoding = "UTF-8",
  stringsAsFactors = FALSE
)

# Une URL identifie chaque fiche et evite de confondre les homonymes.
id <- if ("Id" %in% names(donnees)) donnees$Id else donnees$Url
saisons <- strsplit(donnees$Saisons, "\\s*\\|\\s*", perl = TRUE)
noms_saisons <- unique(unlist(saisons))

# Pour chaque saison, cree une paire entre tous ses participants.
listes_aretes <- lapply(noms_saisons, function(saison) {
  participants <- id[vapply(saisons, function(x) saison %in% x, logical(1))]
  if (length(participants) < 2) return(NULL)
  paires <- t(combn(participants, 2))
  data.frame(from = paires[, 1], to = paires[, 2], saison = saison)
})
aretes <- do.call(rbind, listes_aretes)

# Regroupe les saisons partagees sur un seul lien et calcule son poids.
aretes <- stats::aggregate(
  saison ~ from + to,
  data = aretes,
  FUN = function(x) paste(sort(unique(x)), collapse = " | ")
)
aretes$poids <- lengths(strsplit(aretes$saison, " \\| "))

noeuds <- data.frame(name = id, label = donnees$Nom)
reseau <- graph_from_data_frame(aretes, directed = FALSE, vertices = noeuds)

# Informations des participants conservees pour les tooltips du tableau de bord.
infos_participants <- donnees[match(V(reseau)$name, id), , drop = FALSE]
infos_participants$participations <- lengths(saisons)[match(V(reseau)$name, id)]

# Couleur = communaute; epaisseur = nombre de saisons partagees.
set.seed(1)
communaute <- cluster_louvain(reseau, weights = E(reseau)$poids)
couleurs <- hcl.colors(max(membership(communaute)), "Dark 3")
taille <- 4 + 5 * sqrt(degree(reseau) / max(degree(reseau)))

png("kohlanta_reseau.png", width = 1800, height = 1400, res = 160)
plot(
  reseau,
  layout = layout_with_fr(reseau, weights = E(reseau)$poids),
  vertex.label = NA,
  vertex.size = taille,
  vertex.color = couleurs[membership(communaute)],
  edge.width = 0.3 + log1p(E(reseau)$poids),
  edge.color = "#AAB2BD",
  main = "Koh-Lanta : participants des memes saisons"
)
dev.off()

write_graph(reseau, "kohlanta_reseau.graphml", format = "graphml")
write.csv(aretes, "kohlanta_aretes.csv", row.names = FALSE, fileEncoding = "UTF-8")
cat(vcount(reseau), "participants,", ecount(reseau), "liens. Fichiers PNG, GraphML et CSV crees.\n")
