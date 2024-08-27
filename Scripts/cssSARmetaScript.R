# call packages -----------------------------------------------------------

## Not run:
## dmetar must be installed from github

# if (!require("remotes")) {
#   install.packages("remotes")
# }
# remotes::install_github("MathiasHarrer/dmetar")
## End(**Not run**)

## package list
packages <- c(
  "ggplot2",
  "flextable",
  "officer",
  "extrafont",
  "dplyr",
  "metafor",
  "esc",
  "dmetar",
  "tidyverse",
  "boot",
  "rms",
  "plotly",
  "scatterplot3d",
  "qpcR",
  "MuMIn",
  "parallel",
  "openair"
)

## install or load packages
if (!require(install.load)) {
  install.packages("install.load")
}

install.load::install_load(packages)

eval(metafor:::.MuMIn)

devtools::install_github("ropensci/plotly")

# reconcile fonts ---------------------------------------------------------
remotes::install_version("Rttf2pt1", version = "1.3.8")
font_import(prompt = FALSE, pattern = "calibri")
fonts()
loadfonts(device = "win")
windowsFonts()

# data steps --------------------------------------------------------------
## create "notin" operator
"%notin%" <- Negate("%in%")

## define vector for wild fish
target.wild <- c(
  "ROSA",
  "JDAC",
  "YAKS",
  "JDAS",
  "AGCW",
  "AGWS",
  "EMCR",
  "EMWS"
)

## define vector for wild and hatchery stocks
target.tot <- c(
  "AGGR",
  "AGGA",
  "AGGB"
)

## set target grouping (should be one of the above: "target.wild" or "target.tot")
targ.grp <- "target.wild"

## read-in data
sar.meta.dat <- readRDS(
  "Data\\sarMeta.rds"
) %>%
  
  filter(
    css.grp
    %in%
      target.wild
  ) %>%
  
  ## add fields for adult returns and row id
  mutate(
    cases = (sar.est/100)*juv.pop,
    obs.id = 1:n(),
    dist_rkm = ifelse(
      css.grp == "EMCR" | css.grp == "EMWS",
      529,
      ifelse(
        css.grp == "AGCW" | css.grp == "AGWS",
        461,
        ifelse(
          css.grp == "ROSA" | css.grp == "YAKS",
          236,
          ifelse(
            css.grp == "JDAC" | css.grp == "JDAS",
            113,
            NA
          )
        )
      )
    )
  ) %>% 
  group_by(
    css.grp
  ) %>%
  mutate(
    es.id = seq_along(css.grp)
  )

# calculate summary effect sizes ------------------------------------------
sar.meta.es <- escalc (
  xi = cases, # adult returns
  ni = juv.pop, # juvenile population
  data = sar.meta.dat , # specify data (from above)
  measure = "PLO"  # data transformation (logit in this case)
)
#1--- data summary table
#a--- manipulate data
table.0.dat <- sar.meta.es %>% 
  dplyr::select(
    zone,
    css.grp,
    sar.reach,
    spp.code
  ) %>% 
  unique() %>% 
  mutate(
    zone=replace(zone, zone=="SNAK", "Snake R."),
    zone=replace(zone, zone=="MCOL", "Mid. Col. R"),
    zone=replace(zone, zone=="UCOL", "Upp. Col. R."),
    zone=replace(zone, zone=="UCOL", "Upp. Col. R."),
    spp.code = replace(spp.code, spp.code=="CH", "Chinook"),
    spp.code = replace(spp.code, spp.code=="ST", "steelhead")
    ) %>% 
  mutate(
    riv = c(
      rep("Snake R.",2),
      rep("Yakima R.",2),
      rep("John Day R.",2),
      rep("Entiat and Methow R.", 2)
    )
  ) %>% 
  mutate(
    n = c(
      nrow(filter(sar.meta.es, css.grp=="AGCW")),
      nrow(filter(sar.meta.es, css.grp=="AGWS")),
      nrow(filter(sar.meta.es, css.grp=="ROSA")),
      nrow(filter(sar.meta.es, css.grp=="YAKS")),
      nrow(filter(sar.meta.es, css.grp=="JDAC")),
      nrow(filter(sar.meta.es, css.grp=="JDAS")),
      nrow(filter(sar.meta.es, css.grp=="EMCR")),
      nrow(filter(sar.meta.es, css.grp=="EMWS"))
    )
  )

#b--- generate table
table.0 <- flextable(
  table.0.dat
) %>% 
  align(
    i = 1,
    j = 1,
    align = "center",
    part =  "header"
  ) %>%
  align(
    align = "center",
    part = "all"
  ) %>%
  set_header_labels(
    zone  = "Zone",
    css.grp = "CSS group ID",
    spp.code = "Species",
    riv = "Origin",
    sar.reach = "SAR est. reach",
    n = "No. estimates"
  ) %>%
  set_formatter(
    n = function(x) ifelse(is.na(x),"", formatC(x,digits = 0, format = "f", big.mark = ","))
  ) %>% 
  fontsize(
    size = 12,
    part = "all"
  ) %>%
  font(
    fontname = "Times New Roman",
    part = "all"
  ) %>%
  border_remove() %>%
  hline(
    i=1,
    part="header",
    border = fp_border(
      color="black",
      width = 1
    )
  ) %>%
  hline_top(
    part="header",
    border = fp_border(
      color="black",
      width = 2
    )
  ) %>%
  hline_bottom(
    part="body",
    border = fp_border(
      color="black",
      width = 2
    )
  ) %>%
  width(
    j = c(1,2,4,6), 
    1.1
  ) %>% 
  width(
    j = c(3),
    1.2
  ) %>%
  width(
    j = c(5),
    1.7
  ) %>% 
  print()

# specify models ----------------------------------------------------------
#1--- assess basic model structural components
#a--- assess random observation-level effects
#i--- model 1:
## intercept = TRUE;
## random effects = NA; 
## fixed effects = NA;
## var-cov matrix = unstructured
mod1 <- rma.mv(
  yi = yi, 
  V = vi,
  slab = css.grp,
  data = sar.meta.es,
  test = "t",
  dfs = "contain",
  method = "ML"
)

#ii--- model 2:
## intercept = TRUE; 
## random effect(s) = observation-level; 
## fixed effects = NA;
## var-cov matrix = unstructured
mod2 <- rma.mv(
  yi = yi, 
  V = vi,
  slab = css.grp,
  data = sar.meta.es,
  random = list(
    ~ 1 | obs.id
  ), 
  test = "t",
  dfs = "contain",
  method = "ML"
)

#iii--- model comparison
overdispFitStats <- fitstats(
  mod1,
  mod2
) %>%  
  t() %>% 
  as.data.frame() %>% 
  dplyr::select('logLik:',
                "deviance:",
                "AICc:"
  ) %>% 
  rename(
    "log likliehood" = 1,
    "Deviance" = 2,
    "AICc" = 3
  ) %>% 
  mutate(
    "Intercept" = c("TRUE", "TRUE"),
    "Random effects" = c("", "1|obs.id"),
    "Fixed effects" = c("",""),
    "Var-Cov structure adj." = c("","")
  ) %>% 
  arrange(
    AICc
  ) %>% 
  mutate(
    delta.AICc = round(
      akaike.weights(AICc)$deltaAIC,
      0
    ),
    AICc.weights = round(
      akaike.weights(AICc)$weights,
      2
    )
  ) %>%
  relocate(
    c(
      delta.AICc, 
      AICc.weights
    ),
    .after = AICc
  ) %>% 
  print()

#b--- assess random year effect
#i--- model 3:
## intercept = TRUE;
## random effects = observation-level, migration year; 
## fixed effects = NA;
## var-cov matrix = unstructured
mod3 <- rma.mv(
  yi = yi, 
  V = vi,
  slab = css.grp,
  data = sar.meta.es,
  random = list(
    ~ 1 | obs.id,
    ~ 1 | mig.yr
  ), 
  test = "t",
  dfs = "contain",
  method = "ML"
)

#ii--- model comparison
reMigYrFitStats <- fitstats(
  mod2,
  mod3
) %>%  
  t() %>% 
  as.data.frame() %>% 
  dplyr::select('logLik:',
                "deviance:",
                "AICc:"
  ) %>% 
  rename(
    "log likliehood" = 1,
    "Deviance" = 2,
    "AICc" = 3
  ) %>% 
  mutate(
    "Intercept" = c("TRUE", "TRUE"),
    "Random effects" = c("1|obs", "1|obs;1|mig.yr"),
    "Fixed effects" = c("",""),
    "Var-Cov structure adj." = c("","")
  ) %>% 
  arrange(
    AICc
  ) %>% 
  mutate(
    delta.AICc = round(
      akaike.weights(AICc)$deltaAIC,
      0
    ),
    AICc.weights = round(
      akaike.weights(AICc)$weights,
      2
    )
  ) %>%
  relocate(
    c(
      delta.AICc, 
      AICc.weights
    ),
    .after = AICc
  ) %>% 
  print()

#c--- assess random year effect vs crossed random year and stock effects
#i--- model 4:
## intercept = TRUE;
## random effects = observation-level, migration year, css group; 
## fixed effects = NA;
## var-cov matrix = unstructured

mod4 <- rma.mv(
  yi = yi, 
  V = vi,
  slab = css.grp,
  data = sar.meta.es,
  random = list(
    ~ 1 | obs.id,
    ~ 1 | mig.yr,
    ~ 1 | css.grp
  ),
  test = "t",
  dfs = "contain",
  method = "ML"
)

#ii--- model comparison
crossReMigYrFitStats <- fitstats(
  mod3,
  mod4
) %>%  
  t() %>% 
  as.data.frame() %>% 
  dplyr::select('logLik:',
                "deviance:",
                "AICc:"
  ) %>% 
  rename(
    "log likliehood" = 1,
    "Deviance" = 2,
    "AICc" = 3
  ) %>% 
  mutate(
    "Intercept" = c("TRUE", "TRUE"),
    "Random Effects" = c("1|obs;1|mig.yr", "1|obs;1|mig.yr;1|css.grp"),
    "Fixed Effects" = c("",""),
    "Var-Cov structure adj." = c("","")
  ) %>% 
  arrange(
    AICc
  ) %>% 
  mutate(
    delta.AICc = round(
      akaike.weights(AICc)$deltaAIC,
      0
    ),
    AICc.weights = round(
      akaike.weights(AICc)$weights,
      2
    )
  ) %>%
  relocate(
    c(
      delta.AICc, 
      AICc.weights
    ),
    .after = AICc
  ) %>% 
  print()


#e--- overall (structural) model comparison
globeStructFitStats <- fitstats(
  mod1,
  mod2,
  mod3,
  mod4
) %>%  
  t() %>% 
  as.data.frame() %>% 
  dplyr::select('logLik:',
                "deviance:",
                "AICc:"
  ) %>% 
  rename(
    log.likliehood = 1,
    deviance = 2,
    aicc = 3
  ) %>% 
  mutate(
    "Intercept" = c("+", "+","+","+"),
    "Fixed.Effects" = c("-","-","-","-"),
    "Random.Effects" = c("-","1|obs.","1|obs.; 1|mig. yr.","1|obs.; 1|mig. yr.; 1|CSS grp.")
    
  ) %>% 
  arrange(
    aicc
  ) %>% 
  mutate(
    delta.AICc = round(
      akaike.weights(aicc)$deltaAIC,
      0
    ),
    AICc.weights = round(
      akaike.weights(aicc)$weights,
      2
    )
  ) %>% 
  relocate(
    c(
      delta.AICc, 
      AICc.weights
    ),
    .after = aicc
  ) %>% 
  relocate(
    c(
      Intercept,
      Fixed.Effects,
      Random.Effects
      
    ),
    .before = log.likliehood
  ) %>%
  print()

#i--- create and output summary table
table.1 <- flextable(
  globeStructFitStats
) %>% 
  align(
    i = 1,
    j = 1,
    align = "center",
    part =  "header"
  ) %>%
  align(
    align = "center",
    part = "all"
  ) %>%
  set_header_labels(
    Intercept = "Intercept",
    Fixed.Effects = "Fixed Effects",
    Random.Effects = "Random Effects",
    Fixed.Effects = "Fixed Effects",
    log.likliehood = "log-liklihood",
    deviance = "Deviance",
    aicc = "AICc",
    delta.AICc = paste0("\u394","AICc"),
    AICc.weights = "AICc wt."
  ) %>%
  set_formatter(
    log.likliehood = function(x) ifelse(is.na(x),"", formatC(x,digits = 3, format = "f", big.mark = ",")),
    deviance = function(x) ifelse(is.na(x),"", formatC(x,digits = 3, format = "f", big.mark = ",")),
    aicc = function(x) ifelse(is.na(x),"", formatC(x,digits = 3, format = "f", big.mark = ",")),
    delta.AICc = function(x) ifelse(is.na(x),"", formatC(x,digits = 0, format = "f", big.mark = ",")),
    AICc.weights = function(x) ifelse(is.na(x),"", formatC(x,digits = 3, format = "f", big.mark = ","))
  ) %>% 
  fontsize(
    size = 12,
    part = "all"
  ) %>%
  font(
    fontname = "Times New Roman",
    part = "all"
  ) %>%
  border_remove() %>%
  hline(
    i=1,
    part="header",
    border = fp_border(
      color="black",
      width = 1
    )
  ) %>%
  hline_top(
    part="header",
    border = fp_border(
      color="black",
      width = 2
    )
  ) %>%
  hline_bottom(
    part="body",
    border = fp_border(
      color="black",
      width = 2
    )
  ) %>%
  width(
    j = 2, 
    1.1
  ) %>%
  width(
    j = 3, 
    2.1
  ) %>% 
  print()

#2--- hypothesis testing
#a--- multi-model inference
#i--- fit full model
## intercept = TRUE;
## random effects = observation-level, migration year, css group;
## var-cov matrix = unstructured
fullMod <- rma.mv(
  yi = yi, 
  V = vi,
  slab = css.grp,
  data = sar.meta.es,
  random = list(
    ~ 1 | mig.yr,
    ~ 1 | css.grp,
    ~ 1 | obs.id
  ),
  test = "t",
  dfs = "contain",
  method = "ML",
  mods = ~ factor(spp.code) + wtt + pitph
)

#ii--- conduct MMI
FEmmi <- dredge(
  fullMod, trace=2
) %>% 
  print()

#iii--- create and output summary table
table.2 <- flextable(
  as.data.frame(FEmmi)
) %>% 
  align(
    i = 1,
    j = 1,
    align = "center",
    part =  "header"
  ) %>%
  align(
    align = "center",
    part = "all"
  ) %>%
  set_header_labels(
    `(Intercept)` = "Intercept",
    `factor(spp.code)` = "Spp.",
    pitph = "PITPH",
    wtt = "WTT",
    df = "DF",
    logLik = "log-liklihood",
    AICc = "AICc",
    delta = paste0("\u394","AICc"),
    weight = "AICc wt."
  ) %>%
  set_formatter(
    pitph = function(x) ifelse(is.na(x),"", formatC(x,digits = 3, format = "f", big.mark = ",")),
    wtt = function(x) ifelse(is.na(x),"", formatC(x,digits = 3, format = "f", big.mark = ",")),
    df = function(x) ifelse(is.na(x),"", formatC(x,digits = 0, format = "f", big.mark = ",")),
    AICc = function(x) ifelse(is.na(x),"", formatC(x,digits = 3, format = "f", big.mark = ",")),
    delta = function(x) ifelse(is.na(x),"", formatC(x,digits = 0, format = "f", big.mark = ",")),
    weight = function(x) ifelse(is.na(x),"", formatC(x,digits = 3, format = "f", big.mark = ","))
  ) %>% 
  fontsize(
    size = 12,
    part = "all"
  ) %>%
  font(
    fontname = "Times New Roman",
    part = "all"
  ) %>%
  border_remove() %>%
  hline(
    i=1,
    part="header",
    border = fp_border(
      color="black",
      width = 1
    )
  ) %>%
  hline_top(
    part="header",
    border = fp_border(
      color="black",
      width = 2
    )
  ) %>%
  hline_bottom(
    part="body",
    border = fp_border(
      color="black",
      width = 2
    )
  ) %>% 
  print()

#b1--- plot parameter estimates from full model
#i--- manipulated data
sarMetaParam.dat <- data.frame(
  param.lbs = c("Species[steelhead]",
                "WTT",
                "PITPH"),
  param = fullMod$beta[2:4,],
  lcl = fullMod$ci.lb[-1],
  ucl = fullMod$ci.ub[-1]
)

#ii--- plot parameters
sarMetaParam.plotlot<-ggplot(data = sarMetaParam.dat, aes(x=param, y = param.lbs))+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(color = "black"),
        axis.title.y = element_text(face = "bold", size = 14,vjust = 1,color = "black"),
        axis.title.x = element_text(face = "bold", size = 14,vjust = -1,color = "black"),
        axis.text.x = element_text(face = "bold",size = 12,angle = 90,hjust = 1,vjust = 0.5,color = "black"),
        axis.text.y = element_text(face = "bold",size = 12,color = "black"),
        axis.ticks.length = unit(2,"mm"),
        legend.position = "none")+
  geom_vline(xintercept = 0,linetype = "dashed")+
  geom_errorbarh(aes(xmax=ucl,xmin=lcl),height = 0)+
  geom_point(size = 1)+
  geom_point(size = 4,shape = 1) +
  labs(y = "Parameter",x = "Estimate")+
  # geom_smooth(method = "gam",formula = y~s(x,k=3))
  # scale_y_continuous(limits=c(0,1.1),
  #                    breaks = seq(0,1.1,0.1),
  #                    labels = function(x) sprintf("%.1f", x))
  scale_y_discrete(limits = c("PITPH","WTT","Species[steelhead]")) + 
  scale_x_continuous(limits=c(-0.80,0.80),
                     breaks = seq(-0.80,0.80,0.2))
  print(sarMetaParam.plotlot)

#b--- assess variance inflation
#i---simulate VIF
sarMetaVIF <-
  vif.rma(
    fullMod,
    sim = TRUE,
    parallel = "snow",
    ncpus = detectCores(),
    table = TRUE,
    seed = 1234
  )

#ii--- manipulate simulation output
sarMetaVIF.sims <- rbind(
  data.frame(
    cov = "Species",
    vif = sarMetaVIF$sim[,1]
  ),
  
  data.frame(
    cov = "WTT",
    vif = sarMetaVIF$sim[,2]
  ),
  
  data.frame(
    cov = "PITPH",
    vif = sarMetaVIF$sim[,3]
  )
)

#iii--- plot VIF simulations
sarMetaVIF.plot <- ggplotly(
ggplot(data = sarMetaVIF.sims, aes(x = vif, fill = as.factor(cov), color = as.factor(cov))) +
  theme_bw()+
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(color = "black"),
        axis.title.y = element_text(face = "bold", size = 26,vjust = 1,margin = margin(t = 0, r = 10, b = 0, l = 0),family = "Calibri"),
        axis.title.x = element_text(face = "bold", size = 26,vjust = -1,margin = margin(t = 10, r = 0, b = 0, l = 0),family = "Calibri"),
        axis.text.x = element_text(face = "bold",size = 24,color="black", vjust=0.5,family = "Calibri"),
        axis.text.y = element_text(face = "bold",size = 24,color="black",family = "Calibri"),
        legend.title = element_text(face = "bold",size = 24,color="black",family = "Calibri"),
        legend.text=element_text(face = "bold",size = 24,color="black",family = "Calibri"),
        plot.margin = margin(0.5, 1, 0.5, 0.5, "cm"),
        axis.ticks.length = unit(0.25, "cm"))+
  labs(title ="Variance Inflation Factor", y = "Density", x = "VIF") +
  theme(plot.title = element_text(hjust = 0.5,size = 26,face = "bold",family = "Calibri")) +
  geom_density(alpha = 0.5) +
  geom_vline(xintercept = as.numeric(median(filter(sarMetaVIF.sims, cov=="Species")[,2])), color = "#0000FF")+
  geom_vline(xintercept = as.numeric(median(filter(sarMetaVIF.sims, cov=="WTT")[,2])), color = "#FF0000")+
  geom_vline(xintercept = as.numeric(median(filter(sarMetaVIF.sims, cov=="PITPH")[,2])), color = "grey")+
  # scale_color_manual(labels = c("test","WTT","PITPH")) +
  scale_fill_manual(name = "Covariates", values = c("Species" = "#0000FF", "WTT" = "#FF0000", "PITPH" = "grey")) +
  scale_color_manual(name = "Covariates", values = c("Species" = "#0000FF", "WTT" = "#FF0000", "PITPH" = "grey")) +
  theme(legend.position = "right") +
  labs(fill = "cov")+
  scale_y_continuous(expand = c(0,0)) +
  scale_x_continuous(limits=c(0.990,1.15), breaks = seq(0.990,1.15,0.025))
) %>% 
  print()

#c--- generate and summarize predictions
#i--- generate predictions
sarMeta.pred <- predict(
  fullMod,
  transf = transf.ilogit,
  addx = TRUE
) %>% 
  as.data.frame() %>% 
  mutate(
    zone = sar.meta.dat$zone,
    css.grp = sar.meta.dat$css.grp
  )

#iii--- generate 3D plot
axes.font <- list(
  gridcolor = "black",
  tickfont = 25,
  titlefont = 25,
  family = "Times New Roman",
  color = "red"
)

font.family <- "Calibri"
tick.size <- 16
aTitle.size <- 26

sarMeta.pred$zone[which(sarMeta.pred$zone == "SNAK")] <- 'Snake R.'
sarMeta.pred$zone[which(sarMeta.pred$zone == "MCOL")] <- 'Middle Columbia R.'
sarMeta.pred$zone[which(sarMeta.pred$zone == "UCOL")] <- 'Upper Columbia R.'
sarMeta.pred$zone <- as.factor(sarMeta.pred$zone)

sarMeta.3Dplot <- plot_ly(
  sarMeta.pred,
  x = ~ X.pitph, 
  y = ~ X.wtt,
  z = ~ pred,
  type = "scatter3d",
  mode="markers",
  color = ~ zone,
  colors = c(
    "#999999",
    "#E69F00", 
    "#56B4E9"
  )
) %>% 
  layout(
    scene = list(
      xaxis = list(
        title = 'PITPH',
        range = c(0,8),
        gridcolor = "black",
        backgroundcolor = "grey",
        tickfont = list(
          size = tick.size,
          family = font.family
        ),
        titlefont = list(
          size = aTitle.size,
          family = font.family
        )
      ),
      yaxis = list(
        title = 'WTT',
        range = c(0,30), 
        gridcolor = "black",
        tickfont = list(
          size = tick.size,
          family = font.family
        ),
        titlefont = list(
          size = aTitle.size,
          family = font.family
        )
      ),
      zaxis = list(
        title = 'Predicted SAR',
        range = c(0,0.035), 
        gridcolor = "black",
        tickfont = list(
          size = tick.size,
          family = font.family
        ),
        titlefont = list(
          size = aTitle.size,
          family = font.family
        )
      )
      
    )
  ) %>% 
  layout(
    legend = list(
      title = "Region",
      orientation = "v", y = 0.5, x = 0.87,
      font = list(
        size = 30,
        family = "Calibri"
      )
    )
  ) %>% 
  print()

#d--- develop forest plot
#i--- aggregate data
sar.meta.es.agg <- aggregate(
  sar.meta.es,
  cluster=css.grp,
  struct = "ID",
  addk=TRUE
)

#ii--- fit model with aggregate data
sar.meta.res <- rma(
  yi, 
  vi, 
  method="EE", 
  data = sar.meta.es.agg, 
  digits=5
)

#iii--- generate forest plot
forest(
  sar.meta.res,
  # sortvar = "css.grp",
  transf=transf.ilogit,
  mlab="Pooled Estimate",
  ilab=ki,
  width = 1,
  ilab.xpos=-0.10,
  digits = 3,
  xlab = "SAR",
  refline = NA,
  shade=TRUE,
  header=c("CSS Group", "SAR [95% CI]"),
  slab = sar.meta.es.agg$css.grp,
  fonts = c(rep("Calibri",3)),
  cex=1.4) %>% 
  print()

#4--- model verification
#i--- manipulate data
modVerif <- sarMeta.pred %>% 
  mutate(obs = sar.meta.es$sar.est/100,
         spp = sar.meta.dat$spp.code)

modVerif$spp[which(modVerif$spp == "CH")] <- 'Chinook'
modVerif$spp[which(modVerif$spp == "ST")] <- 'steelhead'

#iii--- model performance
#(a)--- estimate index of agreement
sarMeta.IOA <- modStats(
  modVerif,
  mod = "pred",
  obs = "obs",
  statistic = c("IOA")
  )

#(b)--- generate plot
sarMetaVerif.plot<- ggplotly(
  ggplot(data = modVerif, aes(x = pred, y = obs, fill = as.factor(zone), color = as.factor(spp))) +
    theme_bw()+
    theme(panel.border = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.line = element_line(color = "black"),
          axis.title.y = element_text(face = "bold", size = 24,vjust = 1,margin = margin(t = 0, r = 10, b = 0, l = 0),family = "Calibri"),
          axis.title.x = element_text(face = "bold", size = 24,vjust = -1,margin = margin(t = 10, r = 0, b = 0, l = 0),family = "Calibri"),
          axis.text.x = element_text(face = "bold",size = 24,color="black", vjust=0.5,family = "Calibri"),
          axis.text.y = element_text(face = "bold",size = 24,color="black",family = "Calibri"),
          legend.title = element_blank(),
          plot.margin = margin(0.5, 1, 0.5, 0.5, "cm"),
          legend.text=element_text(face = "bold",size = 18,color="black",family = "Calibri"),,
          axis.ticks.length = unit(0.15, "cm"))+
    labs(title ="", y = "Estimated", x = "Predicted") +
    theme(plot.title = element_text(hjust = 0.5,size = 16,face = "bold",family = "Calibri")) +
    geom_abline(intercept = 0, slope = 1)+
    geom_point(shape = 21,size = 4.5,stroke=0.5) +
    scale_color_manual(name = "",values = c(NA, "black")) +
    scale_fill_manual(name = "", values = c("#999999", "#E69F00", "#56B4E9")) +
    scale_y_continuous(limits=c(0,0.11),breaks = seq(0,0.11,0.02),labels = scales::percent, expand = c(0,0))+
    scale_x_continuous(limits=c(0,0.11),breaks = seq(0,0.11,0.02),labels = scales::percent, expand = c(0,0))
) %>% 
  add_annotations(x = 0.08, y = 0.06, paste("dr =",round(sarMeta.IOA[,2],2)), showarrow = F, font = list(size = 30, family = "Calibri")) %>% 
  print()
