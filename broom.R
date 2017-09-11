gg <- DataFiles %>% group_by(sce, sys) %>% ggplot() + geom_map(aes(map_id = FPU, fill = mean),colour="black", map = newobj) +
      expand_limits(x = newobj$long, y = newobj$lat)+
      geom_path(data = newobj, colour = "black", aes(long, lat, group = group), size = 0.5)+
      coord_map() + scale_fill_viridis() + 
      labs(title = paste( crops[c], " promedio de los rendimientos ponderados y agregados,sistema irrigado\n (2050-2020)" ,sep = ""))

plots <- DataFiles2 %>% group_by(sce, sys) %>% do(plot = ggplot() +
                                                  geom_map(., aes(map_id = FPU, fill = mean), colour = "black", map = newobj) +
                                                  expand_limits(x = newobj$long, y = newobj$lat)+
                                                  geom_path(data = newobj, colour = "black", aes(long, lat, group = group), size = 0.5)+
                                                  coord_map() + scale_fill_viridis())


plots <- small_pwt %>% group_by(country) %>% do(plot = ggplot(data = .) +
                                                      theme_tufte() +
                                                      geom_line(aes(y = avh, x = year)) +
                                                      ggtitle(unique(.$country)) +
                                                      ylab("Year") +
                                                      xlab("Average annual hours worked by persons engaged"))