setwd('C:/Users/caio.oliveira/OneDrive - Toradex AG/Documentos/unicamp/trab_3_got')

png('heatmap_full.png', width = 24, height = 24, units = 'cm', res = 120)
heatmap(asoiaf_x_full, Rowv = NA, Colv = NA, scale = 'none')
dev.off()

png('heatmap_reduzido.png', width = 24, height = 24, units = 'cm', res = 120)
heatmap(asoiaf_x, Rowv = NA, Colv = NA, scale = 'none')
dev.off()

pdf('network_full.pdf', width = 10, height = 7)
par(mar=c(0,0,0,0))
plot(network_full, vertex.size = log(deg_full, 2), vertex.label = NA, rescale = T, asp =0, axes =F,
     edge.width = log(E(network_full)$weight, 10), layout=layout_with_graphopt, 
     vertex.frame.color = '#543310', vertex.color = '#AF8F6F')
dev.off()

png('network_200.png', width = 32, height = 24, units = 'cm', res = 300)
par(mar=c(0,0,0,0))
plot(network, vertex.size = log(deg, 2), vertex.label = NA, rescale = T, asp =0, axes =F,
     edge.width = log(E(network)$weight, 7), layout=layout_with_graphopt)
dev.off()

png('hist_weight.png',width = 20, height = 14, units = 'cm', res = 80)
par(mar=c(2,2,2,2))
hist(got_edges$weight, breaks = 30, main = '', col = '#AF8F6F')
dev.off()

png('criterios.png', width = 15, height = 10, units = 'cm', res = 100)
cbind(1:5, w$icl, w$bic, w$loglik) %>% as.data.frame() %>% rename(Q=V1, ICL = V2, BIC = V3, Log_Ver = V4) %>% remove_rownames() %>% 
  pivot_longer(2:4, names_to = 'Critério', values_to = 'Valor') %>% ggplot(aes(x=Q,y=Valor))+
  geom_line(aes(group=Critério), color = '#AF8F6F', linewidth = 1)+
  geom_point(color='#543310')+
  facet_wrap(~Critério, nrow = 3, scales='free')+
  labs(y='')+
  theme_bw()
dev.off()
