# get_legis  <- anominate_arp_oned$legislators[[1]]
# get_legis_quants <- data.frame(t(apply(get_legis,2,function(x) quantile(x = x,c(0.05,0.5,0.95)))))
# names(get_legis_quants) <- c("5_Perc","Median","95_Perc")
# get_legis_quants$mean <- apply(get_legis,2,mean)
# get_legis_quants$legis.names <- row.names(get_legis_quants)
# get_legis_quants <- data.table(get_legis_quants,key="legis.names")
# get_legis_quants <- get_legis_quants[info_legis]
# get_legis_quants$Party <- factor(info_legis$Party,levels=c("Afek Tounes, le mouvement national et l'appel des tunisiens à l'étranger",
#                                                                  "Bloc Al Horra",
#                                                                  "Bloc Social-Démocrate",
#                                                                  "Front Populaire",
#                                                                  "Mouvement Ennahdha",
#                                                                  "Mouvement Nidaa Tounes",
#                                                                  "Union Patriotique Libre","Aucun bloc"),
#                                                             exclude=c("",NA),
#                                  labels=c("Afek","AlHorra","Social-Dém","FP","Nahda","Nidaa","UPL","Independent"))
# ggplot(data=get_legis_quants,aes(y=reorder(legis.names,mean),x=mean,colour=Party)) + geom_point() + my_theme + 
#   geom_text(aes(label=legis.names),check_overlap=TRUE,hjust=1.5) + 
#    theme(axis.text.y=element_blank()) + ylab("") + xlab("Ideology: Islamist v. Secularist") +
#   geom_errorbarh(aes(xmax=`95_Perc`,xmin=`5_Perc`)) + geom_vline(xintercept=0,alpha=0.5) + scale_colour_brewer(palette="Set1")
# 
# get_legis_1  <- anominate_arp_twod$legislators[[1]]
# get_legis_2  <- anominate_arp_twod$legislators[[2]]
# get_legis_quants_1 <- data.frame(t(apply(get_legis_1,2,function(x) quantile(x = x,c(0.05,0.5,0.95)))))
# get_legis_quants_2 <- data.frame(t(apply(get_legis_2,2,function(x) quantile(x = x,c(0.05,0.5,0.95)))))
# names(get_legis_quants_1) <- c("5_Perc","Median","95_Perc")
# names(get_legis_quants_2) <- c("5_Perc","Median","95_Perc")
# get_legis_quants_1$mean <- apply(get_legis_1,2,mean)
# get_legis_quants_2$mean <- apply(get_legis_2,2,mean)
# get_legis_quants_1$SD <- apply(get_legis_1,2,sd)
# get_legis_quants_2$SD <- apply(get_legis_2,2,sd)
# get_legis_quants_1$legis.names <- factor(row.names(get_legis_quants_1))
# names(get_legis_quants_1) <- paste0("Oned",names(get_legis_quants_1))
# names(get_legis_quants_2) <- paste0("Twod",names(get_legis_quants_2))
# combined_legis <- cbind(get_legis_quants_1,get_legis_quants_2)
# combined_legis$Party2 <- factor(combined_legis$Party,levels=c("Afek Tounes, le mouvement national et l'appel des tunisiens à l'étranger",
#                                                                "Bloc Al Horra",
#                                                                "Front Populaire",
#                                                                "Mouvement Ennahdha",
#                                                                "Mouvement Nidaa Tounes",
#                                                                "Union Patriotique Libre"),
#                                exclude=c("",NA,"Aucun bloc","Bloc Social-Démocrate"),
#                                labels=c("Afek","AlHorra","FP","Nahda","Nidaa","UPL"))
# combined_legis$Party <- factor(combined_legis$Party,levels=c("Afek Tounes, le mouvement national et l'appel des tunisiens à l'étranger",
#                                                               "Bloc Al Horra",
#                                                               "Front Populaire",
#                                                               "Bloc Social-Démocrate",
#                                                               "Mouvement Ennahdha",
#                                                               "Mouvement Nidaa Tounes",
#                                                               "Union Patriotique Libre"),
#                                 exclude=c("",NA,"Aucun bloc"),
#                                 labels=c("Afek","AlHorra","FP","Social-Dém","Nahda","Nidaa","UPL"))
# 
# # Phenomenal code from stack exchange to plot cool ellipses
# 
# ellipseFun <- function(center = c(0, 0), axes = c(1, 1), npoints = 101){
#   tt <- seq(0,2*pi, length.out = npoints)
#   xx <- center[1] + axes[1] * cos(tt)
#   yy <- center[2] + axes[2] * sin(tt)
#   return(data.frame(x = xx, y = yy))
# }
# 
# ellipse_data <- data.frame()
# for(k in levels(factor(combined_legis$Onedlegis.names))){
#   ellipse_data <- rbind(ellipse_data, cbind(as.data.frame(with(combined_legis[combined_legis$Onedlegis.names == k,], ellipseFun(center = c(Twodmean, Onedmean), axes = c(TwodSD, OnedSD), npoints = 101))),Onedlegis.names = k))
# }
# ellipse_data <- data.table(ellipse_data,key="Onedlegis.names")
# ellipse_data <- ellipse_data[combined_legis]
# ggplot(data=ellipse_data,aes(y=Onedmean,x=Twodmean)) + geom_polygon(aes(x = x, y = y, group = Onedlegis.names,fill=Party), alpha = .5) + 
#   my_theme + 
#   geom_text(aes(label=Onedlegis.names),check_overlap=TRUE) +
#   ylab("Ideology: Islamist v. Secularist") + xlab("Economic Policy: Left vs. Right") +# Now for the ellipse code
#   coord_fixed() + scale_fill_brewer(palette="Set1") +
#   geom_hline(yintercept=0,alpha=0.5) + geom_vline(xintercept = 0,alpha=0.5)
# 
# ggplot(data=combined_legis,aes(y=Onedmean,x=Twodmean)) + geom_point() + my_theme + 
#   geom_text(aes(label=Onedlegis.names),check_overlap=TRUE,hjust=0.5,vjust=1) +
#   ylab("Ideology: Islamist v. Secularist") + xlab("Economic Policy: Left vs. Right") +
# geom_errorbarh(aes(xmin=Twod5_Perc,xmax=Twod95_Perc),alpha=0.2) + geom_hline(yintercept=0,alpha=0.5) + geom_vline(xintercept = 0,alpha=0.5) +
#   stat_ellipse(geom="polygon",alpha=0.5,aes(fill=Party2)) + scale_colour_brewer(palette="Set1")
# 
# ggplot(data=combined_legis,aes(y=Onedmean,x=Twodmean,colour=Party)) + geom_point() + my_theme + 
#   geom_text(aes(label=Onedlegis.names),check_overlap=TRUE,hjust=0.5,vjust=1) +
#   ylab("Ideology: Islamist v. Secularist") + xlab("Economic Policy: Left vs. Right") +
#   geom_errorbarh(aes(xmin=Twod5_Perc,xmax=Twod95_Perc),alpha=0.2) + geom_hline(yintercept=0,alpha=0.5) + geom_vline(xintercept = 0,alpha=0.5) +
#   scale_colour_brewer(palette="Set1")
