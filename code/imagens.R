paleta = c('#fad68a','#963131','#800080','#257ca3','#1f5514','#543310')

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


pdf('clustered_network.pdf', width = 10, height = 7)
par(mar=c(0,0,0,0))
plot(network_full, vertex.size = log(deg_full, 2), vertex.label = NA, rescale = T, asp =0, axes =F,
     edge.width = log(E(network_full)$weight, 10), layout=layout_with_graphopt, 
     vertex.frame.color = '#543310', vertex.color = paleta[modelo_5q$clusters])
dev.off()

png('cluster_frequency.png', width = 16, height = 10, units = 'cm', res=120)
modelo_5q$clusters %>% as.factor() %>% as.data.frame() %>% rename(cluster='.') %>% ggplot(aes(x=cluster))+
  geom_bar(aes(fill=cluster), color = paleta[6],show.legend = F)+
  stat_count(aes(label = paste0(after_stat(count),' - ',round(after_stat(count)/796,2)*100,' %')), geom = 'label', size = 4)+
  scale_fill_manual(values = paleta)+
  labs(x='Cluster',y='Freq')+
  theme_bw()
dev.off()

pdf('connection_probability.pdf', width = 5.5, height = 3)
par(mar=c(2,2,1,5))
dimnames(modelo_5q$connections) = list(1:5,1:5)
modelo_5q$connections %>% plot(breaks = c(10^seq(-4,-1),0.5,1), col = c('#e9dbc6','#e8c48e','#c09061','#98694d','#553d3a'))
dev.off()

clustering_5q=rename(clustering_5q, cluster="modelo_5q$clusters")
d <- data.table(arrange(clustering_5q, desc(interactions)), key="cluster")
df = d[, head(.SD, 3), by=cluster]
df %>% mutate(char = str_replace(str_extract(char,'^[A-Z][a-z]+-[A-Z][a-z]+'),'-',' ')) %>% 
  mutate(Personagem_interacoes = paste(char, interactions, sep ='_')) %>% dplyr::select(1,4) %>% mutate(id = rep(1:3, 5)) %>% 
  pivot_wider(names_from = cluster, values_from = Personagem_interacoes, names_prefix = 'Cluster_') %>% dplyr::select(-1) %>% 
  xtable() %>% print(include.rownames=F)

pdf('distribuições_gamma.pdf', width=8,height=8)
par(mar=c(3,0.5,0.5,0.5), mfrow=c(5,5))
for(i in 1:5){
  for(j in 1:5){
    max = qgamma(0.95,modelo_5q$alpha[i,j],modelo_5q$beta[i,j])
    plot(function(x) dgamma(x,modelo_5q$alpha[i,j],modelo_5q$beta[i,j]),xlim = c(0,100),ylim=c(0,0.21), yaxt='n',xlab='', col = paleta[2+2*(i==j)])
    text(paste0('m= ',round(modelo_5q$alpha[i,j]/modelo_5q$beta[i,j],2),'\n var= ',round((modelo_5q$alpha[i,j]/modelo_5q$beta[i,j]^2),2)),
         x = 60, y = 0.1, col = paleta[3], cex = 1.4)
  }
}
dev.off()
