make_tsne <- function(abun, meta, seed = 42){
  
  require(zCompositions, quietly = T)
  require(vegan, quietly = T)
  require(tidyverse, quietly = T)
  
  set.seed(seed)
  
  p <- abun %>%
    zCompositions::cmultRepl(method = "CZM", z.delete = FALSE) %>%
    vegan::decostand("clr") %>%
    dist %>%
    Rtsne::Rtsne(is_distance=TRUE,
                 perplexity = 4, theta = .5, 
                 pca = TRUE, max_iter = 10000) %>%
    .$Y %>% as_tibble() %>%
    cbind(tibble(sample_id = rownames(abun))) %>%
    relocate(sample_id) %>%
    left_join(meta, by = "sample_id") %>%
    ggplot(aes(x = V1, y = V2, fill = Surface)) +
    geom_point(size = 3, shape = 21) +
    stat_ellipse(aes(group = Surface_Type, color = Surface_Type)) +
    scale_color_viridis_d() +
    theme_bw() +
    #theme(#axis.text = element_blank(),
          #axis.title = element_blank(),
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank()) +
    xlab("t-SNE 1") + ylab("t-SNE 2")
  
  return(p)
}