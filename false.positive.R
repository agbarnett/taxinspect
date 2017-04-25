# false.positive.R
# function for false positive rate
alpha_i = function(W_i, e_i){
  alpha_i = W_i / (1 + ((1-W_i)*e_i)) # false positive rate
  return(alpha_i)
}

# reproduce Figure 2 from paper
to.plot = expand.grid(W_i = seq(0, 1, length.out = 100), e_i=c(1, 10, 75))
to.plot$fp = alpha_i(W_i = to.plot$W_i, e_i = to.plot$e_i)
fig2 = ggplot(data=to.plot, aes(x=W_i, y=fp, col=factor(e_i)))+
  geom_line(size=1.2)+
  scale_color_manual('e', values=cbbPalette[1:3])+
  theme(legend.position=c(0.2,0.8))+
  ylab('False positive')
fig2
