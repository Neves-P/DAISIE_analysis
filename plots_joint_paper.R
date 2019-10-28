set.seed(1)

tree1 <- DDD::dd_sim(pars = c(0.5, 0.4, Inf), ddmodel = 1, age = 5)
reconstructed_tree1 <- ape::plot.phylo(tree1$tes, show.tip.label = FALSE, edge.width = 3)
full_tree1 <- ape::plot.phylo(tree1$tas, show.tip.label = FALSE, edge.width = 3)

tree2 <- DDD::dd_sim(pars = c(0.5, 0.4, Inf), ddmodel = 1, age = 5)
reconstructed_tree2 <- ape::plot.phylo(tree2$tes, show.tip.label = FALSE, edge.width = 3)
full_tree2 <- ape::plot.phylo(tree2$tas, show.tip.label = FALSE, edge.width = 3)


tree3 <- DDD::dd_sim(pars = c(0.5, 0.4, Inf), ddmodel = 1, age = 5)
reconstructed_tree3 <- ape::plot.phylo(tree3$tes, show.tip.label = FALSE, edge.width = 3)
full_tree3 <- ape::plot.phylo(tree3$tas, show.tip.label = FALSE, edge.width = 3)
