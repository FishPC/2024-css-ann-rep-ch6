# source functions --------------------------------------------------------
source('Scripts\\Functions\\packFontHandler.R')

# call/install packages and reconcile fonts -------------------------------
## function can be modified @ Scripts\\Functions\\packHandler.R
packFontHandler()

# raw data steps/manipulation ---------------------------------------------
#1--- create 'notin' operator
# '%notin%' <- Negate('%in%')

#2--- define vector for wild fish
target.wild <- c(
  'ROSA',
  'JDAC',
  'YAKS',
  'JDAS',
  'AGCW',
  'AGWS',
  'EMCR',
  'EMWS'
)

#3--- define vector for wild and hatchery stocks
# target.tot <- c(
#   'AGGR',
#   'AGGA',
#   'AGGB'
# )

#4--- set target grouping (should be one of the above: 'target.wild' or 'target.tot')
targ.grp <- 'target.wild'

#4--- read external data
sar.meta.dat <- readRDS(
  'Data\\sarMeta.rds'
) |> 
  filter(
    css.grp
    %in%
      target.wild
  ) |> 
  ## add fields for adult returns and row id
  mutate(
    cases = (sar.est/100)*juv.pop,
    obs.id = 1:n(),
    dist_rkm = ifelse(
      css.grp == 'EMCR' | css.grp == 'EMWS',
      529,
      ifelse(
        css.grp == 'AGCW' | css.grp == 'AGWS',
        461,
        ifelse(
          css.grp == 'ROSA' | css.grp == 'YAKS',
          236,
          ifelse(
            css.grp == 'JDAC' | css.grp == 'JDAS',
            113,
            NA
          )
        )
      )
    )
  ) |> 
  group_by(
    css.grp
  ) |> 
  mutate(
    es.id = seq_along(css.grp)
  )

# calculate summary effect sizes ------------------------------------------
sar.meta.es <- escalc (
  xi = cases, # adult returns
  ni = juv.pop, # juvenile population
  data = sar.meta.dat , # specify data (from above)
  measure = 'PLO',  # data transformation (logit in this case)
  add = 1/2, # add constant to account for '0' cell entries
  to = 'none' # specify cell entries to apply constant
)

#1--- create summary table
#a--- manipulate data
table.1 <- sar.meta.es |> 
  dplyr::select(
    zone,
    # css.grp,
    sar.reach,
    spp.code
  ) |>  
  unique() |>  
  mutate(
    zone=replace(zone, zone=='SNAK', 'Snake R.'),
    zone=replace(zone, zone=='MCOL', 'Mid. Col. R'),
    zone=replace(zone, zone=='UCOL', 'Upp. Col. R.'),
    zone=replace(zone, zone=='UCOL', 'Upp. Col. R.'),
    spp.code = replace(spp.code, spp.code=='CH', 'Chinook'),
    spp.code = replace(spp.code, spp.code=='ST', 'steelhead')
  ) |>  
  mutate(
    riv = c(
      rep('Snake R.',2),
      rep('Yakima R.',2),
      rep('John Day R.',2),
      rep('Entiat and Methow R.', 2)
    )
  ) |>  
  mutate(
    n = c(
      nrow(filter(sar.meta.es, css.grp=='AGCW')),
      nrow(filter(sar.meta.es, css.grp=='AGWS')),
      nrow(filter(sar.meta.es, css.grp=='ROSA')),
      nrow(filter(sar.meta.es, css.grp=='YAKS')),
      nrow(filter(sar.meta.es, css.grp=='JDAC')),
      nrow(filter(sar.meta.es, css.grp=='JDAS')),
      nrow(filter(sar.meta.es, css.grp=='EMCR')),
      nrow(filter(sar.meta.es, css.grp=='EMWS'))
    )
  ) |>  
  #b--- generate table
  flextable() |>  
  align(
    i = 1,
    j = 1,
    align = 'center',
    part =  'header'
  ) |> 
  align(
    align = 'center',
    part = 'all'
  ) |> 
  set_header_labels(
    zone  = 'Zone',
    # css.grp = 'CSS group ID',
    spp.code = 'Species',
    riv = 'Origin',
    sar.reach = 'SAR est. reach',
    n = 'No. estimates'
  ) |> 
  set_formatter(
    n = function(x) ifelse(is.na(x),'', formatC(x,digits = 0, format = 'f', big.mark = ','))
  ) |>  
  fontsize(
    size = 12,
    part = 'all'
  ) |> 
  font(
    fontname = 'Times New Roman',
    part = 'all'
  ) |> 
  border_remove() |>
  hline(
    i=1,
    part='header',
    border = fp_border(
      color='black',
      width = 1
    )
  ) |> 
  hline_top(
    part='header',
    border = fp_border(
      color='black',
      width = 2
    )
  ) |> 
  hline_bottom(
    part='body',
    border = fp_border(
      color='black',
      width = 2
    )
  ) |> 
  width(
    j = c(
      1,
      3,
      5
    ), 
    1.1
  ) |>  
  width(
    j = c(
      2
    ),
    3.0
  ) |> 
  width(
    j = c(
      4
    ),
    1.7
  ) |>  
  mk_par( 
    j = 2,
    i = 1:2,
    value = as_paragraph(
      'Lwr. Granite',
      as_sub('juv.'),
      '\u2013',
      'Lwr. Granite',
      as_sub('ad.')
    )
  ) |> 
  mk_par( 
    j = 2,
    i = 3:4,
    value = as_paragraph(
      'McNary',
      as_sub('juv.'),
      '\u2013',
      'McNary',
      as_sub('ad.')
    )
  ) |>  
  mk_par( 
    j = 2,
    i = 5:6,
    value = as_paragraph(
      'John Day',
      as_sub('juv.'),
      '\u2013',
      'Bonn.',
      as_sub('ad.')
    )
  ) |> 
  mk_par( 
    j = 2,
    i = 7:8,
    value = as_paragraph(
      'Rocky Reach',
      as_sub('juv.'),
      '\u2013',
      'Rocky Reach',
      as_sub('ad.')
    )
  ) |>
  print()

# assess basic model structure --------------------------------------------
#1--- specify models
#a--- assess multi-level random effects model
#i--- model 1 (basic RE model) specs:
## intercept = TRUE;
## random effects = css.grp; 
## fixed effects = NA;
## var-cov matrix = unstructured
mod1 <- rma.mv(
  yi = yi, 
  V = vi,
  slab = css.grp,
  data = sar.meta.es,
  random = list(
    ~ 1 | css.grp
  ),
  test = 't',
  dfs = 'contain',
  method = 'ML'
)

#ii--- model 2 (multi-level model) specs:
## intercept = TRUE; 
## random effect(s) = obs.id within css.grp; 
## fixed effects = NA;
## var-cov matrix = unstructured
mod2 <- rma.mv(
  yi = yi, 
  V = vi,
  slab = css.grp,
  data = sar.meta.es,
  random = list(
    ~ 1 | css.grp/obs.id
  ), 
  test = 't',
  dfs = 'contain',
  method = 'ML'
)

#iii--- model comparison
multLevFitStats <- fitstats(
  mod1,
  mod2
) |>   
  t() |>  
  as.data.frame() |>  
  dplyr::select('logLik:',
                'deviance:',
                'AICc:'
  ) |>  
  rename(
    'log likliehood' = 1,
    'Deviance' = 2,
    'AICc' = 3
  ) |>  
  mutate(
    'Intercept' = c('TRUE', 'TRUE'),
    'Random effects' = c('1|css.grp', '1|css.grp/obs.id'),
    'Fixed effects' = c('',''),
    'Var-Cov structure adj.' = c('','')
  ) |>  
  arrange(
    AICc
  ) |>  
  mutate(
    delta.AICc = round(
      akaike.weights(AICc)$deltaAIC,
      0
    ),
    AICc.weights = round(
      akaike.weights(AICc)$weights,
      2
    )
  ) |> 
  relocate(
    c(
      delta.AICc, 
      AICc.weights
    ),
    .after = AICc
  ) |>  
  print()

#b--- assess random year effect
#i--- model 3 (mixed model) specs:
## intercept = TRUE;
## random effects = obs.id within css.grp, migration year; 
## fixed effects = NA;
## var-cov matrix = unstructured
mod3 <- rma.mv(
  yi = yi, 
  V = vi,
  slab = css.grp,
  data = sar.meta.es,
  random = list(
    ~ 1 | css.grp/obs.id,
    ~ 1 | mig.yr
  ), 
  test = 't',
  dfs = 'contain',
  method = 'ML'
)

#ii--- model comparison
reMigYrFitStats <- fitstats(
  mod2,
  mod3
) |>   
  t() |>  
  as.data.frame() |>  
  dplyr::select('logLik:',
                'deviance:',
                'AICc:'
  ) |>  
  rename(
    'log likliehood' = 1,
    'Deviance' = 2,
    'AICc' = 3
  ) |>  
  mutate(
    'Intercept' = c('TRUE', 'TRUE'),
    'Random effects' = c('1|css.grp/obs.id', '1|css.grp/obs.id;1|mig.yr'),
    'Fixed effects' = c('',''),
    'Var-Cov structure adj.' = c('','')
  ) |>  
  arrange(
    AICc
  ) |>  
  mutate(
    delta.AICc = round(
      akaike.weights(AICc)$deltaAIC,
      0
    ),
    AICc.weights = round(
      akaike.weights(AICc)$weights,
      2
    )
  ) |> 
  relocate(
    c(
      delta.AICc, 
      AICc.weights
    ),
    .after = AICc
  ) |>  
  print()

#2--- create table of overall (structural) model comparisons
#i--- manipulate data
table.2 <- fitstats(
  mod1,
  mod2,
  mod3
) |>   
  t() |>  
  as.data.frame() |>  
  dplyr::select('logLik:',
                'AICc:'
  ) |>  
  rename(
    log.likliehood = 1,
    aicc = 2
  ) |>  
  mutate(
    'Intercept' = c('+','+','+'),
    'Fixed.Effects' = c('-','-','-'),
    'Random.Effects' = c('1|CSS grp.','1|CSS grp./obs.','1|CSS grp./obs.; 1|mig. yr.')
  ) |> 
  arrange(
    aicc
  ) |>  
  mutate(
    delta.AICc = round(
      akaike.weights(aicc)$deltaAIC,
      0
    ),
    AICc.weights = round(
      akaike.weights(aicc)$weights,
      2
    ),
    no.param = arrange(
      AIC.rma(
        mod1,
              mod2,
              mod3
              ),
      AIC
    )$df
  ) |>  
  relocate(
    c(
      delta.AICc, 
      AICc.weights
    ),
    .after = aicc
  ) |>  
  relocate(
    c(
      Intercept,
      Fixed.Effects,
      Random.Effects,
      no.param
      
    ),
    .before = log.likliehood
  ) |> 
  #ii--- generate output table
  flextable() |>  
  align(
    i = 1,
    j = 1,
    align = 'center',
    part =  'header'
  ) |> 
  align(
    align = 'center',
    part = 'all'
  ) |> 
  set_header_labels(
    Intercept = 'Intercept',
    Fixed.Effects = 'Fixed Effects',
    Random.Effects = 'Random Effects',
    no.param = 'No. params.',
    log.likliehood = 'log-liklihood'
  ) |> 
  mk_par( 
    j = 6,
    i = 1,
    value = as_paragraph(
      'AIC',
      as_sub('c')
    ),
    part = 'header'
  ) |> 
  mk_par( 
    j = 7,
    i = 1,
    value = as_paragraph(
      paste0('\u394','AIC'),
      as_sub('c')
    ),
    part = 'header'
  ) |> 
  mk_par( 
    j =8,
    i = 1,
    value = as_paragraph(
      'AIC',
      as_sub('c'),
      ' wt.'
    ),
    part = 'header'
  ) |>
  set_formatter(
    log.likliehood = function(x) ifelse(is.na(x),'', formatC(x,digits = 3, format = 'f', big.mark = ',')),
    aicc = function(x) ifelse(is.na(x),'', formatC(x,digits = 1, format = 'f', big.mark = ',')),
    delta.AICc = function(x) ifelse(is.na(x),'', formatC(x,digits = 0, format = 'f', big.mark = ',')),
    AICc.weights = function(x) ifelse(is.na(x),'', formatC(x,digits = 2, format = 'f', big.mark = ','))
  ) |>  
  fontsize(
    size = 12,
    part = 'all'
  ) |> 
  font(
    fontname = 'Times New Roman',
    part = 'all'
  ) |> 
  border_remove() |> 
  hline(
    i=1,
    part='header',
    border = fp_border(
      color='black',
      width = 1
    )
  ) |> 
  hline_top(
    part='header',
    border = fp_border(
      color='black',
      width = 2
    )
  ) |> 
  hline_bottom(
    part='body',
    border = fp_border(
      color='black',
      width = 2
    )
  ) |> 
  width(
    j = 2, 
    1.1
  ) |> 
  width(
    j = 3, 
    2.1
  ) |> 
  width(
    j = 4, 
    1.0
  ) |> 
  print()

# hypothesis testing ------------------------------------------------------
#1--- multi-model inference
## helper functions necessary to coordinate metafor and MuMin
eval(metafor:::.MuMIn)

#a--- fit full model
## update best supported model from above, with moderators
fullMod <- update(
  mod3,
  ~ factor(spp.code) + wtt + pitph
)

#b--- estimate variance inflation factors
#i--- calculate VIF in closed-form and simulate props < vif
table.3 <- vif.rma(
  fullMod,
  sim = TRUE,
  parallel = "snow",
  ncpus = detectCores(),
  seed = 1234
) |> 
  #ii--- manipulated data
  as.data.frame() |> 
  mutate(
    mods = c(
      'Species',
      'WTT',
      'PITPH'
    )
  ) |> 
  dplyr::select(
    mods,
    vif,
    prop
  ) |>
  #iii--- generate table
  flextable() |> 
  align(
    i = 1,
    j = 1,
    align = 'center',
    part =  'header'
  ) |>
  align(
    align = 'center',
    part = 'all'
  ) |>
  set_header_labels(
    mods = 'Moderator',
    vif = 'VIF'
  ) |>
  mk_par( 
    j =3,
    i = 1,
    value = as_paragraph(
      'Prop.',
      as_sub('sims.'),
      ' < VIF'
    ),
    part = 'header'
  ) |>
  set_formatter(
    mods = function(x) ifelse(is.na(x),'', formatC(x,digits = 3, format = 'f')),
    vif = function(x) ifelse(is.na(x),'', formatC(x,digits = 3, format = 'f')),
    prop = function(x) ifelse(is.na(x),'', formatC(x,digits = 2, format = 'f'))
  ) |> 
  fontsize(
    size = 12,
    part = 'all'
  ) |>
  font(
    fontname = 'Times New Roman',
    part = 'all'
  ) |>
  border_remove() |>
  hline(
    i=1,
    part='header',
    border = fp_border(
      color='black',
      width = 1
    )
  ) |>
  hline_top(
    part='header',
    border = fp_border(
      color='black',
      width = 2
    )
  ) |>
  hline_bottom(
    part='body',
    border = fp_border(
      color='black',
      width = 2
    )
  ) |>
  autofit() |> 
  print()

#c--- run MMI
#i--- model selection
table.4 <- dredge(
  fullMod,
  trace=2
) |> 
  #ii--- manipulate output
  as.data.frame() |> 
  ### add-in estimates for Spp. (this has to be done manually after dredge)
  mutate(
    `factor(spp.code)` = c(
      update(
        fullMod,
        ~ factor(spp.code) + wtt + pitph
      )[1] |>  
        as.data.frame() |>  
        head(2) |>  
        tail(1) |>  
        as.numeric(),
      update(
        fullMod,
        ~ factor(spp.code) + wtt
      )[1] |>  
        as.data.frame() |>  
        head(2) |>  
        tail(1) |>  
        as.numeric(),
      NA,
      NA,
      NA,
      update(
        fullMod,
        ~ factor(spp.code) + pitph
      )[1] |>  
        as.data.frame() |>  
        head(2) |>  
        tail(1) |>  
        as.numeric(),
      NA,
      update(
        fullMod,
        ~ factor(spp.code)
      )[1] |>  
        as.data.frame() |>  
        head(2) |>  
        tail(1) |>  
        as.numeric()
    )
  ) |>  
  #iii--- generate output table
  flextable() |>  
  align(
    i = 1,
    j = 1,
    align = 'center',
    part =  'header'
  ) |> 
  align(
    align = 'center',
    part = 'all'
  ) |> 
  set_header_labels(
    `(Intercept)` = 'Intercept',
    `factor(spp.code)` = 'Spp.',
    pitph = 'PITPH',
    wtt = 'WTT',
    df = 'No. params.',
    logLik = 'log-liklihood'
  ) |>
  mk_par( 
    j = 7,
    i = 1,
    value = as_paragraph(
      'AIC',
      as_sub('c')
    ),
    part = 'header'
  ) |> 
  mk_par( 
    j = 8,
    i = 1,
    value = as_paragraph(
      paste0('\u394','AIC'),
      as_sub('c')
    ),
    part = 'header'
  ) |> 
  mk_par( 
    j =9,
    i = 1,
    value = as_paragraph(
      'AIC',
      as_sub('c'),
      ' wt.'
    ),
    part = 'header'
  ) |>
  set_formatter(
    `factor(spp.code)` = function(x) ifelse(is.na(x),'', formatC(x,digits = 3, format = 'f')),
    pitph = function(x) ifelse(is.na(x),'', formatC(x,digits = 3, format = 'f')),
    wtt = function(x) ifelse(is.na(x),'', formatC(x,digits = 3, format = 'f')),
    df = function(x) ifelse(is.na(x),'', formatC(x,digits = 0, format = 'f')),
    AICc = function(x) ifelse(is.na(x),'', formatC(x,digits = 1, format = 'f')),
    delta = function(x) ifelse(is.na(x),'', formatC(x,digits = 0, format = 'f')),
    weight = function(x) ifelse(is.na(x),'', formatC(x,digits = 2, format = 'f'))
  ) |>  
  fontsize(
    size = 12,
    part = 'all'
  ) |> 
  font(
    fontname = 'Times New Roman',
    part = 'all'
  ) |> 
  border_remove() |>
  hline(
    i=1,
    part='header',
    border = fp_border(
      color='black',
      width = 1
    )
  ) |> 
  hline_top(
    part='header',
    border = fp_border(
      color='black',
      width = 2
    )
  ) |> 
  hline_bottom(
    part='body',
    border = fp_border(
      color='black',
      width = 2
    )
  ) |>
  width(
    j = 5, 
    0.95
  ) |>
  print()

#d--- specify best model
## update full model based on model selection table
bestMod <- update(
  fullMod,
  ~ factor(spp.code) + wtt + pitph
)

#e--- plot model-averaged and best model coefficients and CIs
#i--- estimate model-averaged coefficients
avgMod <- dredge(
  bestMod,
  trace = 2
) |> 
  model.avg()

#ii--- data manipulation
sarMetaParam.plot <- bind_rows(
  coefTable(avgMod) |>
    as.data.frame() |> 
    dplyr::select(
      Estimate
    ) |> 
    tibble::rownames_to_column('param.lbs') |> 
    left_join(
      confint(
        avgMod, 
        full = TRUE
      ) |> 
        as.data.frame() |> 
        tibble::rownames_to_column('param.lbs'),
      by = 'param.lbs'
    ) |>  
    as.data.frame() |>  
    rename(
      lcl = 3,
      ucl = 4
    ) |>
    mutate(
      param.lbs = c(
        'Intercept',
        'Species[steelhead]',
        'PITPH',
        'WTT'
      )
    ) |> 
    tail(3) |> 
    mutate(
      selType = rep('average',3)
    ) |> 
    relocate(
      selType,
      .before = param.lbs
    ),
  data.frame(
    selType = rep('best',3),
    param.lbs = c("Species[steelhead]",
                  "WTT",
                  "PITPH"),
    param = bestMod$beta[2:4,],
    lcl = bestMod$ci.lb[-1],
    ucl = bestMod$ci.ub[-1]) |> 
    rename(
      Estimate = 3
    ) |> 
    `rownames<-`( NULL )
) |> 
  as.data.frame() |> 
  #iii--- generate plot
  ggplot(aes(x=Estimate, y = param.lbs, color = selType))+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(color = 'black'),
        axis.title.y = element_text(face = 'bold', size = 18,vjust = 1,color = 'black',family = 'Calibri'),
        axis.title.x = element_text(face = 'bold', size = 18,vjust = -1,color = 'black',family = 'Calibri'),
        axis.text.x = element_text(face = 'bold',size = 16,angle = 0,hjust = 0.5,vjust = 0.5,color = 'black',family = 'Calibri'),
        axis.text.y = element_text(face = 'bold',size = 16,color = 'black',family = 'Calibri'),
        axis.ticks.length = unit(2,'mm'),
        legend.position = 'right',
        legend.title = element_blank(),
        legend.text = element_text(size = 16,family = 'Calibri',face = 'bold'))+
  scale_colour_manual(name="Transplanted", values = c("best" = '#D55E00',"average" = "black")) +
  geom_vline(xintercept = 0,linetype = 'dashed') +
  geom_errorbarh(aes(xmax=ucl,xmin=lcl),height = 0, position = position_dodge(width = 0.2)) +
  geom_point(size = 1, position = position_dodge(width = 0.2)) +
  geom_point(size = 4,shape = 1, position = position_dodge(width = 0.2)) +
  labs(y = 'Parameter',x = 'Estimate') +
  scale_y_discrete(limits = c('WTT','PITPH','Species[steelhead]')) + 
  scale_x_continuous(limits=c(-0.80,0.80),
                     breaks = seq(-0.80,0.80,0.2))

#iv--- print figure for review
print(sarMetaParam.plot)

# variance components -----------------------------------------------------
#c--- variance component tables
#i--- full model (all moderators)
fullVC.mod <- bestMod

fullVC.tbl <- data.frame(
  sigNm = c(
    '\\sigma^2_1',
    '\\sigma^2_2',
    '\\sigma^2_3'
  ),
  sigEst = fullVC.mod$sigma2,
  sQRtsigEst = sqrt(fullVC.mod$sigma2),
  nLvls = fullVC.mod$s.nlevels,
  fac = fullVC.mod$s.names
) |>
  mutate(
    fac = c(
      'CSS grp.',
      'CSS grp./obs.',
      'mig. yr.'
    )
  ) |> 
  flextable() |>  
  align(
    i = 1,
    j = 1,
    align = 'center',
    part =  'header'
  ) |> 
  align(
    align = 'center',
    part = 'all'
  ) |> 
  set_header_labels(
    sigNm = 'Parameter',
    sigEst = 'Estimate',
    sQRtsigEst = 'Sqr. Rt. Estimate',
    nLvls = 'No. Levels',
    fac = 'Factor'
  ) |> 
  set_formatter(
    sigEst = function(x) ifelse(is.na(x),'', formatC(x,digits = 4, format = 'f')),
    sQRtsigEst = function(x) ifelse(is.na(x),'', formatC(x,digits = 4, format = 'f')),
    nLvls = function(x) ifelse(is.na(x),'', formatC(x,digits = 0, format = 'f'))
  ) |>  
  fontsize(
    size = 12,
    part = 'all'
  ) |> 
  font(
    fontname = 'Times New Roman',
    part = 'all'
  ) |> 
  border_remove() |>
  hline(
    i=1,
    part='header',
    border = fp_border(
      color='black',
      width = 1
    )
  ) |> 
  hline_top(
    part='header',
    border = fp_border(
      color='black',
      width = 2
    )
  ) |>
  hline_bottom(
    part='body',
    border = fp_border(
      color='black',
      width = 2
    )
  ) |>  
  mk_par(
    j = 'sigNm',
    value = as_paragraph(as_equation(sigNm))
  ) |> 
  autofit() |>
  set_caption(
    caption = as_paragraph(
      as_chunk(
        "Full model", 
        props = fp_text_default(
          bold = TRUE, 
          font.family = 'Times New Roman', 
          font.size = 14
          )
        )
    ),
    align_with_table = FALSE,
    word_stylename = "Table Caption",
    fp_p = fp_par(text.align = "left", padding = 3)
  ) |> 
  print()

#ii--- reduced model (no moderators)
redVC_nm.mod <- update(
  fullMod,
  ~ factor(spp.code)
)

redVC_nm.tbl <- data.frame(
  sigNm = c(
    '\\sigma^2_1',
    '\\sigma^2_2',
    '\\sigma^2_3'
  ),
  sigEst = redVC_nm.mod$sigma2,
  sQRtsigEst = sqrt(redVC_nm.mod$sigma2),
  nLvls = redVC_nm.mod$s.nlevels,
  fac = redVC_nm.mod$s.names
) |> 
  mutate(
    fac = c(
      'CSS grp.',
      'CSS grp./obs.',
      'mig. yr.'
    )
  ) |> 
  flextable() |>  
  align(
    i = 1,
    j = 1,
    align = 'center',
    part =  'header'
  ) |> 
  align(
    align = 'center',
    part = 'all'
  ) |> 
  set_header_labels(
    sigNm = 'Parameter',
    sigEst = 'Estimate',
    sQRtsigEst = 'Sqr. Rt. Estimate',
    nLvls = 'No. Levels',
    fac = 'Factor'
  ) |> 
  set_formatter(
    sigEst = function(x) ifelse(is.na(x),'', formatC(x,digits = 4, format = 'f')),
    sQRtsigEst = function(x) ifelse(is.na(x),'', formatC(x,digits = 4, format = 'f')),
    nLvls = function(x) ifelse(is.na(x),'', formatC(x,digits = 0, format = 'f'))
  ) |>  
  fontsize(
    size = 12,
    part = 'all'
  ) |> 
  font(
    fontname = 'Times New Roman',
    part = 'all'
  ) |> 
  border_remove() |> 
  hline(
    i=1,
    part='header',
    border = fp_border(
      color='black',
      width = 1
    )
  ) |> 
  hline_top(
    part='header',
    border = fp_border(
      color='black',
      width = 2
    )
  ) |> 
  hline_bottom(
    part='body',
    border = fp_border(
      color='black',
      width = 2
    )
  ) |>  
  mk_par(
    j = 'sigNm',
    value = as_paragraph(as_equation(sigNm))
  ) |>
  autofit() |> 
  set_caption(
    caption = as_paragraph(
      as_chunk(
        "Reduced model (mods. = spp.)", 
        props = fp_text_default(
          bold = TRUE, 
          font.family = 'Times New Roman', 
          font.size = 14
        )
      )
    ),
    align_with_table = FALSE,
    word_stylename = "Table Caption",
    fp_p = fp_par(text.align = "left", padding = 3)
  ) |> 
  print()

#ii--- reduced model (PITPH only)
redVC_pitph.mod <- update(
  fullMod,
  ~ factor(spp.code) + pitph
)

redVC_pitph.tbl <- data.frame(
  sigNm = c(
    '\\sigma^2_1',
    '\\sigma^2_2',
    '\\sigma^2_3'
  ),
  sigEst = redVC_pitph.mod$sigma2,
  sQRtsigEst = sqrt(redVC_pitph.mod$sigma2),
  nLvls = redVC_pitph.mod$s.nlevels,
  fac = redVC_pitph.mod$s.names
) |> 
  mutate(
    fac = c(
      'CSS grp.',
      'CSS grp./obs.',
      'mig. yr.'
    )
  ) |> 
  flextable() |>  
  align(
    i = 1,
    j = 1,
    align = 'center',
    part =  'header'
  ) |> 
  align(
    align = 'center',
    part = 'all'
  ) |> 
  set_header_labels(
    sigNm = 'Parameter',
    sigEst = 'Estimate',
    sQRtsigEst = 'Sqr. Rt. Estimate',
    nLvls = 'No. Levels',
    fac = 'Factor'
  ) |> 
  set_formatter(
    sigEst = function(x) ifelse(is.na(x),'', formatC(x,digits = 4, format = 'f')),
    sQRtsigEst = function(x) ifelse(is.na(x),'', formatC(x,digits = 4, format = 'f')),
    nLvls = function(x) ifelse(is.na(x),'', formatC(x,digits = 0, format = 'f'))
  ) |>  
  fontsize(
    size = 12,
    part = 'all'
  ) |> 
  font(
    fontname = 'Times New Roman',
    part = 'all'
  ) |> 
  border_remove() |> 
  hline(
    i=1,
    part='header',
    border = fp_border(
      color='black',
      width = 1
    )
  ) |> 
  hline_top(
    part='header',
    border = fp_border(
      color='black',
      width = 2
    )
  ) |> 
  hline_bottom(
    part='body',
    border = fp_border(
      color='black',
      width = 2
    )
  ) |>  
  mk_par(
    j = 'sigNm',
    value = as_paragraph(as_equation(sigNm))
  ) |> 
  autofit() |>
  set_caption(
    caption = as_paragraph(
      as_chunk(
        "Reduced model (mods. = spp., PITPH)", 
        props = fp_text_default(
          bold = TRUE, 
          font.family = 'Times New Roman', 
          font.size = 14
        )
      )
    ),
    align_with_table = FALSE,
    word_stylename = "Table Caption",
    fp_p = fp_par(text.align = "left", padding = 3)
  ) |>
  print()

#ii--- reduced model (WTT only)
redVC_wtt.mod <- update(
  fullMod,
  ~ factor(spp.code) + wtt
)

redVC_wtt.tbl <- data.frame(
  sigNm = c(
    '\\sigma^2_1',
    '\\sigma^2_2',
    '\\sigma^2_3'
  ),
  sigEst = redVC_wtt.mod$sigma2,
  sQRtsigEst = sqrt(redVC_wtt.mod$sigma2),
  nLvls = redVC_wtt.mod$s.nlevels,
  fac = redVC_wtt.mod$s.names
) |>  
  mutate(
    fac = c(
      'CSS grp.',
      'CSS grp./obs.',
      'mig. yr.'
    )
  ) |> 
  flextable() |>  
  align(
    i = 1,
    j = 1,
    align = 'center',
    part =  'header'
  ) |> 
  align(
    align = 'center',
    part = 'all'
  ) |> 
  set_header_labels(
    sigNm = 'Parameter',
    sigEst = 'Estimate',
    sQRtsigEst = 'Sqr. Rt. Estimate',
    nLvls = 'No. Levels',
    fac = 'Factor'
  ) |> 
  set_formatter(
    sigEst = function(x) ifelse(is.na(x),'', formatC(x,digits = 4, format = 'f')),
    sQRtsigEst = function(x) ifelse(is.na(x),'', formatC(x,digits = 4, format = 'f')),
    nLvls = function(x) ifelse(is.na(x),'', formatC(x,digits = 0, format = 'f'))
  ) |>  
  fontsize(
    size = 12,
    part = 'all'
  ) |> 
  font(
    fontname = 'Times New Roman',
    part = 'all'
  ) |> 
  border_remove() |> 
  hline(
    i=1,
    part='header',
    border = fp_border(
      color='black',
      width = 1
    )
  ) |> 
  hline_top(
    part='header',
    border = fp_border(
      color='black',
      width = 2
    )
  ) |> 
  hline_bottom(
    part='body',
    border = fp_border(
      color='black',
      width = 2
    )
  ) |>  
  mk_par(
    j = 'sigNm',
    value = as_paragraph(as_equation(sigNm))
  ) |> 
  autofit() |> 
  set_caption(
    caption = as_paragraph(
      as_chunk(
        "Reduced model (mods. = spp., WTT)", 
        props = fp_text_default(
          bold = TRUE, 
          font.family = 'Times New Roman', 
          font.size = 14
        )
      )
    ),
    align_with_table = FALSE,
    word_stylename = "Table Caption",
    fp_p = fp_par(text.align = "left", padding = 3)
  ) |>
  print()

#d--- generate and summarize predictions
#i--- estimate random effects
randEffEsts <- ranef(
  bestMod
)

#ii--- generate predictions
sarMeta.pred <- predict(
  bestMod,
  addx = TRUE
) |>  
  as.data.frame() |>  
  mutate(
    zone = sar.meta.dat$zone,
    css.grp = sar.meta.dat$css.grp,
    mig.yr = sar.meta.dat$mig.yr,
    obs.id = sar.meta.dat$obs.id,
    nest = paste0(
      sar.meta.dat$css.grp,
      '/',
      sar.meta.dat$obs.id)
  ) |> 
  left_join(
    tibble::rownames_to_column(randEffEsts$mig.yr[1], 'mig.yr') |>  
      as.data.frame() |>  
      mutate(
        mig.yr = as.numeric(mig.yr)
      ),
    by = 'mig.yr'
  ) |> 
  rename(
    fmig.yr = intrcpt
  ) |> 
  left_join(
    tibble::rownames_to_column(randEffEsts$css.grp[1], 'css.grp') |>  
      as.data.frame(),
    by = 'css.grp'
  ) |> 
  rename(
    fcss.grp = intrcpt
  ) |> 
  
  left_join(
    tibble::rownames_to_column(randEffEsts$`css.grp/obs.id`[1], 'nest') |>  
      as.data.frame(),
    by = 'nest'
  ) |>  
  rename(
    fnest = intrcpt
  ) |> 
  rowwise() |> 
  mutate(
    flogitPred = rowSums(
      pick(
        pred, 
        fmig.yr, 
        fcss.grp, 
        fnest), 
      na.rm = FALSE
    )
  ) |> 
  #iii--- back-transform predictions
  mutate(
    arithPred = inv.logit(pred),
    farithPred = inv.logit(flogitPred)
  ) |> 
  mutate(
    zone=replace(zone, zone=='SNAK', 'Snake R.')
  ) |> 
  mutate(
    zone=replace(zone, zone=='MCOL', 'Middle Columbia R.')
  ) |>
  mutate(
    zone=replace(zone, zone=='UCOL', 'Upper Columbia R.')
  ) |>
  mutate(
    across(
      zone,
      factor
    )
  ) |> 
  as.data.frame()

#4--- model verification
#a--- manipulate data
modVerif <- sarMeta.pred |>  
  mutate(
    arithObs = sar.meta.es$sar.est/100,
    spp = sar.meta.dat$spp.code,
    logitObs = logit(sar.meta.es$sar.est/100)
  ) |> 
  mutate(
    spp=replace(spp, spp=='CH', 'Chinook')
  ) |> 
  mutate(
    spp=replace(spp, spp=='ST', 'steelhead')
  )

#b--- model performance
#i--- estimate index of agreement (arithmatic scale; fixed params)
sarMeta.IOAarithFix <- modStats(
  modVerif,
  mod = 'arithPred',
  obs = 'arithObs',
  statistic = c('IOA')
) |> 
  print()

#ii--- estimate index of agreement (logit scale; fixed params)
sarMeta.IOAlogitFix <- modStats(
  modVerif,
  mod = 'pred',
  obs = 'logitObs',
  statistic = c('IOA')
) |> 
  print()

#iii--- estimate index of agreement (arithmetic; random)
sarMeta.IOAarithRand <- modStats(
  modVerif,
  mod = 'farithPred',
  obs = 'arithObs',
  statistic = c('IOA')
) |> 
  print()

#iii--- estimate index of agreement (logit scale; random)
sarMeta.IOAlogitRand <- modStats(
  modVerif,
  mod = 'flogitPred',
  obs = 'logitObs',
  statistic = c('IOA')
) |> 
  print()

#iV--- generate plot (arithmatic scale; fixed params)
sarMetaVerif.arithFixPlot<- ggplotly(
  ggplot(data = modVerif, aes(x = arithPred, y = arithObs, fill = as.factor(zone), color = as.factor(spp))) +
    theme_bw()+
    theme(panel.border = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.line = element_line(color = 'black'),
          axis.title.y = element_text(face = 'bold', size = 26,vjust = 1,margin = margin(t = 0, r = 10, b = 0, l = 0),family = 'Calibri'),
          axis.title.x = element_text(face = 'bold', size = 26,vjust = -1,margin = margin(t = 10, r = 0, b = 0, l = 0),family = 'Calibri'),
          axis.text.x = element_text(face = 'bold',size = 26,color='black', vjust=0.5,family = 'Calibri'),
          axis.text.y = element_text(face = 'bold',size = 26,color='black',family = 'Calibri'),
          legend.title = element_blank(),
          plot.margin = margin(0.5, 1, 0.5, 0.5, 'cm'),
          legend.text=element_blank(),
          # legend.position = "none",
          axis.ticks.length = unit(0.15, 'cm'))+
    labs(title ='', y = 'Estimated', x = 'Predicted') +
    theme(plot.title = element_text(hjust = 0.5,size = 16,face = 'bold',family = 'Calibri')) +
    geom_abline(intercept = 0, slope = 1)+
    geom_point(shape = 21,size = 4.5,stroke=0.5) +
    scale_color_manual(name = '',values = c(NA, 'black')) +
    scale_fill_manual(name = '', values = c('#999999', '#E69F00', '#56B4E9')) +
    scale_y_continuous(limits=c(0,0.11),breaks = seq(0,0.11,0.02),labels = scales::percent, expand = c(0,0))+
    scale_x_continuous(limits=c(0,0.11),breaks = seq(0,0.11,0.02),labels = scales::percent, expand = c(0,0))
) |>  
  add_annotations(x = 0.009, y = 0.090, paste('dr =',round(sarMeta.IOAarithFix[,2],2)), showarrow = F, font = list(size = 35, family = 'Calibri')) |> 
  layout(legend = list(x = 0.50, 
                       y = 0.09,
                       bgcolor = 'rgba(0,0,0,0)',
                       font = list(size = 25)))

#v--- generate plot (logit scale; fixed params)
sarMetaVerif.logitFixPlot<- ggplotly(
  ggplot(data = modVerif, aes(x = pred, y = logitObs, fill = as.factor(zone), color = as.factor(spp))) +
    theme_bw()+
    theme(panel.border = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.line = element_line(color = 'black'),
          axis.title.y = element_text(face = 'bold', size = 26,vjust = 1,margin = margin(t = 0, r = 10, b = 0, l = 0),family = 'Calibri'),
          axis.title.x = element_text(face = 'bold', size = 26,vjust = -1,margin = margin(t = 10, r = 0, b = 0, l = 0),family = 'Calibri'),
          axis.text.x = element_text(face = 'bold',size = 26,color='black', vjust=0.5,family = 'Calibri'),
          axis.text.y = element_text(face = 'bold',size = 26,color='black',family = 'Calibri'),
          legend.title = element_blank(),
          plot.margin = margin(0.5, 1, 0.5, 0.5, 'cm'),
          legend.text=element_text(face = 'bold',size = 18,color='black',family = 'Calibri'),
          axis.ticks.length = unit(0.15, 'cm'))+
    labs(title ='', y = 'logit(Estimated)', x = 'logit(Predicted)') +
    theme(plot.title = element_text(hjust = 0.5,size = 16,face = 'bold',family = 'Calibri')) +
    geom_abline(intercept = 0, slope = 1)+
    geom_point(shape = 21,size = 4.5,stroke=0.5) +
    scale_color_manual(name = '',values = c(NA, 'black')) +
    scale_fill_manual(name = '', values = c('#999999', '#E69F00', '#56B4E9'))+
    scale_y_continuous(limits=c(-6,-2),breaks = seq(-6,-2,1.0),expand = c(0,0))+
    scale_x_continuous(limits=c(-6,-2),breaks = seq(-6,-2,1.0),expand = c(0,0))
) |> 
  add_annotations(x = -5.7, y = -3.0, paste('dr =',round(sarMeta.IOAlogitFix[,2],2)), showarrow = F, font = list(size = 35, family = 'Calibri')) |> 
  layout(legend = list(x = 0.55, 
                       y = 0.09,
                       bgcolor = 'rgba(0,0,0,0)',
                       font = list(size = 22))) |> 
  hide_legend()

#vi--- generate plot (arithmatic scale; random params)
sarMetaVerif.arithRandPlot<- ggplotly(
  ggplot(data = modVerif, aes(x = farithPred, y = arithObs, fill = as.factor(zone), color = as.factor(spp))) +
    theme_bw()+
    theme(panel.border = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.line = element_line(color = 'black'),
          axis.title.y = element_text(face = 'bold', size = 26,vjust = 1,margin = margin(t = 0, r = 10, b = 0, l = 0),family = 'Calibri'),
          axis.title.x = element_text(face = 'bold', size = 26,vjust = -1,margin = margin(t = 10, r = 0, b = 0, l = 0),family = 'Calibri'),
          axis.text.x = element_text(face = 'bold',size = 26,color='black', vjust=0.5,family = 'Calibri'),
          axis.text.y = element_text(face = 'bold',size = 26,color='black',family = 'Calibri'),
          legend.title = element_blank(),
          plot.margin = margin(0.5, 1, 0.5, 0.5, 'cm'),
          legend.text=element_text(face = 'bold',size = 18,color='black',family = 'Calibri'),
          axis.ticks.length = unit(0.15, 'cm'))+
    labs(title ='', y = '', x = 'Predicted') +
    theme(plot.title = element_text(hjust = 0.5,size = 16,face = 'bold',family = 'Calibri')) +
    geom_abline(intercept = 0, slope = 1)+
    geom_point(shape = 21,size = 4.5,stroke=0.5) +
    scale_color_manual(name = '',values = c(NA, 'black')) +
    scale_fill_manual(name = '', values = c('#999999', '#E69F00', '#56B4E9')) +
    scale_y_continuous(limits=c(0,0.11),breaks = seq(0,0.11,0.02),labels = scales::percent, expand = c(0,0))+
    scale_x_continuous(limits=c(0,0.11),breaks = seq(0,0.11,0.02),labels = scales::percent, expand = c(0,0))
) |>  
  add_annotations(x = 0.009, y = 0.090, paste('dr =',round(sarMeta.IOAarithRand[,2],2)), showarrow = F, font = list(size = 35, family = 'Calibri')) |> 
  layout(legend = list(x = 0.55, 
                       y = 0.09,
                       bgcolor = 'rgba(0,0,0,0)',
                       font = list(size = 22))) |> 
  hide_legend()

#vii--- generate plot (logit scale; random params)
sarMetaVerif.logitRandPlot<- ggplotly(
  ggplot(data = modVerif, aes(x = flogitPred, y = logitObs, fill = as.factor(zone), color = as.factor(spp))) +
    theme_bw()+
    theme(panel.border = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.line = element_line(color = 'black'),
          axis.title.y = element_text(face = 'bold', size = 26,vjust = 1,margin = margin(t = 0, r = 10, b = 0, l = 0),family = 'Calibri'),
          axis.title.x = element_text(face = 'bold', size = 26,vjust = -1,margin = margin(t = 10, r = 0, b = 0, l = 0),family = 'Calibri'),
          axis.text.x = element_text(face = 'bold',size = 26,color='black', vjust=0.5,family = 'Calibri'),
          axis.text.y = element_text(face = 'bold',size = 26,color='black',family = 'Calibri'),
          legend.title = element_blank(),
          plot.margin = margin(0.5, 1, 0.5, 0.5, 'cm'),
          legend.text=element_text(face = 'bold',size = 18,color='black',family = 'Calibri'),
          axis.ticks.length = unit(0.15, 'cm'))+
    labs(title ='', y = '', x = 'logit(Predicted)') +
    theme(plot.title = element_text(hjust = 0.5,size = 16,face = 'bold',family = 'Calibri')) +
    geom_abline(intercept = 0, slope = 1)+
    geom_point(shape = 21,size = 4.5,stroke=0.5) +
    scale_color_manual(name = '',values = c(NA, 'black')) +
    scale_fill_manual(name = '', values = c('#999999', '#E69F00', '#56B4E9'))+
    scale_y_continuous(limits=c(-6,-2),breaks = seq(-6,-2,1.0),expand = c(0,0))+
    scale_x_continuous(limits=c(-6,-2),breaks = seq(-6,-2,1.0),expand = c(0,0))
) |> 
  add_annotations(x = -5.7, y = -3.0, paste('dr =',round(sarMeta.IOAlogitRand[,2],2)), showarrow = F, font = list(size = 35, family = 'Calibri')) |> 
  layout(legend = list(x = 0.55, 
                       y = 0.09,
                       bgcolor = 'rgba(0,0,0,0)',
                       font = list(size = 22))) |> 
  hide_legend()


