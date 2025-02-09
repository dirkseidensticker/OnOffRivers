# Network of clay sourcing and preparation ----

# cf. notes taken during Kroon "Who's Afraid of Pots, Networks, and Probabilities? Probabilistic Approach to Ceramic Technology Reconceptualises Interactions between Funnel Beaker West and Corded Ware" (2024-06-14)
# !! cf. to be published in Sep. '24: Kroon, E., 2024. Serial Learners. Interactions between Funnel Beaker West and Corded Ware Communities in the Netherlands during the Third Millennium BCE from the Perspective of Ceramic Technology. Sidestone Press Dissertations. https://doi.org/10.59641/4e367hq

# steps:
# - build general network (cf. Roux)
#   - empirical sequences from case studies
#   - reference sequences from literature (cf. Potters' Communities DB with N. Nikis)
#   - radom reference sequenes (via Makarov chains)
# - compare sequences via "Time-to-Event Analysis"(?)
# - Wasserstein distance for change between groups

library(tidyr)
library(ggnetwork)
library(sna)

el <- data.table::fread("input/edgelist_clays_ethno.csv", encoding = "UTF-8")

e <- el %>%
  dplyr::group_by(FROM, TO) %>%
  dplyr::tally()

n <- e %>%
  network::network(
    .[,c("FROM", "TO")])

set.edge.attribute(n, "crossrefs", e$n)

ggnetwork(n)

ggplot(n, aes(
  x, y, xend = xend, yend = yend)) +
  geom_nodes() + 
  geom_edges(arrow = arrow(length=unit(0.30,"cm"), type = "closed")) + 
  geom_nodelabel_repel(aes(label = vertex.names))
