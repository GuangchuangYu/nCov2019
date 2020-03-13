
require(nCov2019)
y <- load_nCov2019(lang = 'en', source='github')
d = y['global']


require(dplyr)
dd <- filter(d, time == time(y)) %>% 
    arrange(desc(cum_confirm)) 

dd = dd[1:40, ]
dd$country = factor(dd$country, levels=dd$country)

dd$angle = 1:40 * 360/40
require(ggplot2)
p <- ggplot(dd, aes(country, cum_confirm, fill=cum_confirm)) + 
    geom_col(width=1, color='grey90') + 
    geom_col(aes(y=I(5)), width=1, fill='grey90', alpha = .2) +       
    geom_col(aes(y=I(3)), width=1, fill='grey90', alpha = .2) +    
    geom_col(aes(y=I(2)), width=1, fill = "white") +
    scale_y_log10() + 
    scale_fill_gradientn(colors=c("darkgreen", "green", "orange", "firebrick","red"), trans="log") + 
    geom_text(aes(label=paste(country, cum_confirm, sep="\n"), 
                  y = cum_confirm *.8, angle=angle), 
            data=function(d) d[d$cum_confirm > 100,], 
            size=3, color = "white", fontface="bold", vjust=1)  + 
    geom_text(aes(label=paste0(cum_confirm, " cases ", country), 
                  y = max(cum_confirm) * 2, angle=angle+90), 
            data=function(d) d[d$cum_confirm < 100,], 
            size=3, vjust=0) + 
    coord_polar(direction=-1) + 
    theme_void() + 
    theme(legend.position="none") 

p1 = ggplotify::as.ggplot(p, scale=1.45, vjust=-.15, hjust=.02)

require(dplyr)
require(ggplot2)
require(shadowtext)
require(nCov2019)

d <- load_nCov2019()
dd <- d['global'] %>% 
  as_tibble %>%
  rename(confirm=cum_confirm) %>%
  filter(confirm > 100 & country != "China") %>%
  group_by(country) %>%
  mutate(days_since_100 = as.numeric(time - min(time))) %>%
  ungroup 
  

  

breaks=c(100, 200, 500, 1000, 2000, 5000, 10000, 20000, 50000, 100000)


p2 <- ggplot(dd, aes(days_since_100, confirm, color = country)) +
  geom_smooth(method='lm', aes(group=1),
              data = . %>% filter(!country %in% c("Japan", "Singapore")), 
              color='grey10', linetype='dashed') +
  geom_line(size = 0.8) +
  geom_point(pch = 21, size = 1) +
  scale_y_log10(expand = expansion(add = c(0,0.1)), 
                breaks = breaks, labels = breaks) +
  scale_x_continuous(expand = expansion(add = c(0,1))) +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid.minor = element_blank(),
    legend.position = "none",
    plot.margin = margin(3,20,3,3,"mm")
  ) +
  coord_cartesian(clip = "off") +
  geom_shadowtext(aes(label = paste0(" ",country)), hjust=0, vjust = 0, 
                  data = . %>% group_by(country) %>% top_n(1, days_since_100), 
                  bg.color = "white") +
  labs(x = "Number of days since 100th case", y = NULL, 
       title = "Confirmed COVID-19 cases",
       subtitle = time(d))


library(chinamap)

x <- get_nCov2019(lang = 'en')
cn = get_map_china()
cn$province <- trans_province(cn$province)
p3 <- plot(x, region = 'china', chinamap = cn,
     continuous_scale = FALSE,
     palette = 'Blues', font.size = 2) +
     theme_minimal(base_size=14)

require(cowplot)
pp <- plot_grid(p2, p3, ncol=1, labels=c("B", "C"), 
    rel_heights=c(.7, 1)) 
g <- plot_grid(p1, pp, ncol=2, rel_widths=c(1.2, 1), labels=c("A", "")) 
ggsave(g, filename = "nCov2019.png", width=16, height=11)

