### Run models functions

run_my_cbs_models <- function(data, name, part, seed) {
  require(tidyverse)
  require(brms)

  if (is.null(seed))
    seed <- NA
  
  
  data <- data%>%
    dplyr::select(sex, Year, part)%>%
    na.omit(.)
  
  f1 <- brm(
    formula = bf(paste(part, "~ 1")),
    data = data,
    family = student(link="identity"),
    cores = 8,
    chains = 4,
    thin = 10,
    #chop out transitions?
    warmup = 5000,
    #half of iterations
    iter = 10000,
    # prior = c(prior(student_t(3, 2.8, 2.5), "Intercept"),
    #           prior(student_t(3, 0, 2.5), "sigma")),
    #prior = priors,
    save_pars = save_pars(all = TRUE),
    #need this for loo comparison
    control = list(adapt_delta = 0.97, max_treedepth = 17),
    #adapt_delta the target average proposal acceptance probability during Stan's adaptation period
    seed = seed
  )
  f2 <- brm(
    formula = bf(paste0(part, " ~ 1 + Year")),
    #autocor = cor_ar(~1, p = 1),
    data = data,
    family = student(link="identity"),
    cores = 8,
    chains = 4,
    thin = 10,
    #chop out transitions?
    warmup = 5000,
    #half of iterations
    iter = 10000,
    #prior = c(
    #     prior(normal(0, 10), "Intercept"),
    #     prior(cauchy(0, 2.5), "sigma")),
    # prior = c(
    #   prior(student_t(3, 2.8, 2.5), "Intercept"),
    #   prior(student_t(3, 0, 2.5), "sigma"),
    #   prior(normal(0, 5), class = "b")
    # ),
    save_pars = save_pars(all = TRUE),
    #need this for loo comparison
    control = list(adapt_delta = 0.97, max_treedepth = 17),
    #adapt_delta the target average proposal acceptance probability during Stan's adaptation period
    seed = seed
  )
  
  f3 <- brm(
    formula = bf(paste0(part, "~ 1 + sex")),
    data = data,
    family = student(link="identity"),
    cores = 8,
    chains = 4,
    thin = 10,
    #chop out transitions?
    warmup = 5000,
    #half of iterations
    iter = 10000,
    #prior = c(
    #     prior(normal(0, 10), "Intercept"),
    #     prior(cauchy(0, 2.5), "sigma")),
    # prior = c(
    #   prior(student_t(3, 2.8, 2.5), "Intercept"),
    #   prior(normal(0, 5), class = "b")
    # ),
    save_pars = save_pars(all = TRUE),
    #need this for loo comparison
    control = list(adapt_delta = 0.97, max_treedepth = 17),
    #adapt_delta the target average proposal acceptance probability during Stan's adaptation period
    seed = seed
  )
  f4 <- brm(
    formula = bf(paste0(part, "~ 1 + Year + sex")),
    #autocor = cor_ar(~1, p = 1),
    data = data,
    family = student(link="identity"),
    cores = 8,
    chains = 4,
    thin = 10,
    #chop out transitions?
    warmup = 5000,
    #half of iterations
    iter = 10000,
    # prior = c(
    #   prior(student_t(3, 2.8, 2.5), "Intercept"),
    #   prior(normal(0, 5), class = "b")
    # ),
    #prior = priors,
    save_pars = save_pars(all = TRUE),
    #need this for loo comparison
    control = list(adapt_delta = 0.97, max_treedepth = 17),
    #adapt_delta the target average proposal acceptance probability during Stan's adaptation period
    seed = seed
  )
  
  f5 <- brm(
    formula = bf(paste0(part, "~ 1 + Year + sex + (1|County)")),
    #autocor = cor_ar(~1, p = 1),
    data = data,
    family = student(link="identity"),
    cores = 8,
    chains = 4,
    thin = 10,
    #chop out transitions?
    warmup = 5000,
    #half of iterations
    iter = 10000,
    # prior = c(
    #   prior(student_t(3, 2.8, 2.5), "Intercept"),
    #   prior(normal(0, 5), class = "b")
    # ),
    #prior = priors,
    save_pars = save_pars(all = TRUE),
    #need this for loo comparison
    control = list(adapt_delta = 0.97, max_treedepth = 17),
    #adapt_delta the target average proposal acceptance probability during Stan's adaptation period
    seed = seed
  )
  
  f6 <- brm(
    formula = bf(paste0(part, "~ 1 + Year + sex + Year * sex")),
    data = data,
    #autocor = cor_ar(~1, p = 1),
    family = student(link="identity"),
    cores = 8,
    chains = 4,
    thin = 10,
    #chop out transitions?
    warmup = 5000,
    #half of iterations
    iter = 10000,
    #prior = c(
    #     prior(normal(0, 10), "Intercept"),
    #     prior(cauchy(0, 2.5), "sigma")),
    # prior = c(
    #   prior(student_t(3, 2.8, 2.5), "Intercept"),
    #   prior(normal(0, 5), class = "b")
    # ),
    save_pars = save_pars(all = TRUE),
    #need this for loo comparison
    control = list(adapt_delta = 0.97, max_treedepth = 17),
    #adapt_delta the target average proposal acceptance probability during Stan's adaptation period
    seed = seed
  )
  
  f7 <- brm(
    formula = bf(paste0(part, "~ 1 + Year + sex + Year * sex + Age + sex.ratio + Year * sex.ratio")),
    data = data,
    #autocor = cor_ar(~1, p = 1),
    family = student(link="identity"),
    cores = 8,
    chains = 4,
    thin = 10,
    #chop out transitions?
    warmup = 5000,
    #half of iterations
    iter = 10000,
    #prior = c(
    #     prior(normal(0, 10), "Intercept"),
    #     prior(cauchy(0, 2.5), "sigma")),
    # prior = c(
    #   prior(student_t(3, 2.8, 2.5), "Intercept"),
    #   prior(normal(0, 5), class = "b")
    # ),
    save_pars = save_pars(all = TRUE),
    #need this for loo comparison
    control = list(adapt_delta = 0.98, max_treedepth = 17),
    #adapt_delta the target average proposal acceptance probability during Stan's adaptation period
    seed = seed
  )
  
  f8 <- brm(
    formula = bf(paste0(part, "~ 1 + Year + sex + Age + sex.ratio")),
    data = data,
    #autocor = cor_ar(~1, p = 1),
    family = student(link="identity"),
    cores = 8,
    chains = 4,
    thin = 10,
    #chop out transitions?
    warmup = 5000,
    #half of iterations
    iter = 10000,
    #prior = c(
    #     prior(normal(0, 10), "Intercept"),
    #     prior(cauchy(0, 2.5), "sigma")),
    # prior = c(
    #   prior(student_t(3, 2.8, 2.5), "Intercept"),
    #   prior(normal(0, 5), class = "b")
    # ),
    save_pars = save_pars(all = TRUE),
    #need this for loo comparison
    control = list(adapt_delta = 0.98, max_treedepth = 17),
    #adapt_delta the target average proposal acceptance probability during Stan's adaptation period
    seed = seed
  )
  
  f9 <- brm(
    formula = bf(paste0(part, "~ 1 + Year + sex + Year * sex + Age + sex.ratio")),
    data = data,
    #autocor = cor_ar(~1, p = 1),
    family = student(link="identity"),
    cores = 8,
    chains = 4,
    thin = 10,
    #chop out transitions?
    warmup = 5000,
    #half of iterations
    iter = 10000,
    #prior = c(
    #     prior(normal(0, 10), "Intercept"),
    #     prior(cauchy(0, 2.5), "sigma")),
    # prior = c(
    #   prior(student_t(3, 2.8, 2.5), "Intercept"),
    #   prior(normal(0, 5), class = "b")
    # ),
    save_pars = save_pars(all = TRUE),
    #need this for loo comparison
    control = list(adapt_delta = 0.98, max_treedepth = 17),
    #adapt_delta the target average proposal acceptance probability during Stan's adaptation period
    seed = seed
  )
  
  f10 <- brm(
    formula = bf(paste0(part, "~ 1 + Year + sex + Year * sex + Age + sex.ratio + Year * sex.ratio + (1|County)")),
    data = data,
    #autocor = cor_ar(~1, p = 1),
    family = student(link="identity"),
    cores = 8,
    chains = 4,
    thin = 10,
    #chop out transitions?
    warmup = 5000,
    #half of iterations
    iter = 10000,
    #prior = c(
    #     prior(normal(0, 10), "Intercept"),
    #     prior(cauchy(0, 2.5), "sigma")),
    # prior = c(
    #   prior(student_t(3, 2.8, 2.5), "Intercept"),
    #   prior(normal(0, 5), class = "b")
    # ),
    save_pars = save_pars(all = TRUE),
    #need this for loo comparison
    control = list(adapt_delta = 0.98, max_treedepth = 17),
    #adapt_delta the target average proposal acceptance probability during Stan's adaptation period
    seed = seed
  )
  
  #compare model with LOOIC
  m.comp<-LOO(f1,f2,f3,f4, f5, f6, f7, f8, f9, f10, moment_match=T)
  
  #Get Bayes R^2
  br1<-bayes_R2(f1)
  br2<-bayes_R2(f2)
  br3<-bayes_R2(f3)
  br4<-bayes_R2(f4)
  br5<-bayes_R2(f5)
  br6<-bayes_R2(f6)
  br7<-bayes_R2(f7)
  br8<-bayes_R2(f8)
  br9<-bayes_R2(f9)
  br10<-bayes_R2(f10)
  
  br2L <- list(br1, br2, br3, br4, br5, br6, br7, br8, br9, br10)
  
  t <- tibble(dataset = c(rep(name,times=10),"LOO_comp", "BayesR2"),
                  model_set = c(1:10, "model_compare", "BayesR2"),
                  m = list(f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, m.comp, br2L))
  
  assign(name, t, envir=globalenv())
} 

