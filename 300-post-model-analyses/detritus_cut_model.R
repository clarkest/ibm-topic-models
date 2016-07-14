
cuts.20.3.exc60 <- cutModels(0.2, controls=c("is.manager", "is.exec", "gender", 
                                             "u.s.time.", "continent2", "last.period",
                                             "log.length", "focus", #"solo.topic",
                                             "is.first.comment","missing.parent", "excitement.60",#"log.sec.since.parent", 
                                             "forum"),
                             interaction.terms = c("log.length : focus"))
cuts.20.3.exc30 <- cutModels(0.2, controls=c("is.manager", "is.exec", "gender", 
                                             "u.s.time.", "continent2", "last.period",
                                             "log.length", "focus", #"solo.topic",
                                             "is.first.comment","missing.parent", "excitement.30",#"log.sec.since.parent", 
                                             "forum"),
                             interaction.terms = c("log.length : focus"))
cuts.20.3.exc15 <- cutModels(0.2, controls=c("is.manager", "is.exec", "gender", 
                                             "u.s.time.", "continent2", "last.period",
                                             "log.length", "focus", #"solo.topic",
                                             "is.first.comment","missing.parent", "excitement.15",#"log.sec.since.parent", 
                                             "forum"),
                             interaction.terms = c("log.length : focus"))
cuts.20.3.exc120 <- cutModels(0.2, controls=c("is.manager", "is.exec", "gender", 
                                              "u.s.time.", "continent2", "last.period",
                                              "log.length", "focus", #"solo.topic",
                                              "is.first.comment","missing.parent", "excitement.120",#"log.sec.since.parent", 
                                              "forum"),
                              interaction.terms = c("log.length : focus"))
cuts.20.3.exc180 <- cutModels(0.2, controls=c("is.manager", "is.exec", "gender", 
                                              "u.s.time.", "continent2", "last.period",
                                              "log.length", "focus", #"solo.topic",
                                              "is.first.comment","missing.parent", "excitement.180",#"log.sec.since.parent", 
                                              "forum"),
                              interaction.terms = c("log.length : focus"))
cuts.20.3.exc240 <- cutModels(0.2, controls=c("is.manager", "is.exec", "gender", 
                                              "u.s.time.", "continent2", "last.period",
                                              "log.length", "focus", #"solo.topic",
                                              "is.first.comment","missing.parent", "excitement.240",#"log.sec.since.parent", 
                                              "forum"),
                              interaction.terms = c("log.length : focus"))
cuts.20.3.exc <- cutModels(0.2, controls=c("is.manager", "is.exec", "gender", 
                                           "u.s.time.", "continent2", "last.period",
                                           "log.length", "focus", #"solo.topic",
                                           "is.first.comment","missing.parent",#"log.sec.since.parent", 
                                           "excitement.240", "excitement.120", "excitement.60", "excitement.15",
                                           "forum"),
                           interaction.terms = c("log.length : focus"))


cuts.20.3.exp5 <- cutModels(0.2, controls=c("is.manager", "is.exec", "gender", 
                                            "u.s.time.", "continent2", "last.period",
                                            "log.length", "focus", #"solo.topic",
                                            "is.first.comment","missing.parent", "exp.excite.5","log.sec.since.parent", 
                                            "forum"),
                            interaction.terms = c("log.length : focus"))
cuts.20.3.exp20 <- cutModels(0.2, controls=c("is.manager", "is.exec", "gender", 
                                             "u.s.time.", "continent2", "last.period",
                                             "log.length", "focus", #"solo.topic",
                                             "is.first.comment","missing.parent", "exp.excite.20","log.sec.since.parent", 
                                             "forum"),
                             interaction.terms = c("log.length : focus"))
cuts.20.3.exp30 <- cutModels(0.2, controls=c("is.manager", "is.exec", "gender", 
                                             "u.s.time.", "continent2", "last.period",
                                             "log.length", "focus", #"solo.topic",
                                             "is.first.comment","missing.parent", "exp.excite.30","log.sec.since.parent", 
                                             "forum"),
                             interaction.terms = c("log.length : focus"))
cuts.20.3.exp60 <- cutModels(0.2, controls=c("is.manager", "is.exec", "gender", 
                                             "u.s.time.", "continent2", "last.period",
                                             "log.length", "focus", #"solo.topic",
                                             "is.first.comment","missing.parent", "exp.excite.60","log.sec.since.parent", 
                                             "forum"),
                             interaction.terms = c("log.length : focus"))



# Excitation Models
stargazer(cuts.20.3.int$world, cuts.20.3.exc15$world, 
          cuts.20.3.exc30$world, cuts.20.3.exc60$world,
          cuts.20.3.exc120$world, cuts.20.3.exc180$world,
          cuts.20.3.exc240$world, cuts.20.3.exc$world,
          type='text',
          report = "vct*", t.auto=F, p.auto=F, apply.coef=exp,
          out="outputs/excite_linear_world.txt")
stargazer(cuts.20.3.int$values, cuts.20.3.exc15$values, 
          cuts.20.3.exc30$values, cuts.20.3.exc60$values,
          cuts.20.3.exc120$values, cuts.20.3.exc180$values,
          cuts.20.3.exc240$values, cuts.20.3.exc$values,
          type='text',
          report = "vct*", t.auto=F, p.auto=F, apply.coef=exp,
          out="outputs/excite_linear_values.txt")

stargazer(cuts.20.3.int$world, cuts.20.3.exp5$world, 
          cuts.20.3.exp10$world, cuts.20.3.exp20$world,
          cuts.20.3.exp30$world, cuts.20.3.exp60$world,
          type='text',
          report = "vct*", t.auto=F, p.auto=F, apply.coef=exp,
          out="outputs/excite_exp_world.txt")
stargazer(cuts.20.3.int$values, cuts.20.3.exp5$values, 
          cuts.20.3.exp10$values, cuts.20.3.exp20$values,
          cuts.20.3.exp30$values, cuts.20.3.exp60$values,
          type='text',
          report = "vct*", t.auto=F, p.auto=F, apply.coef=exp,
          out="outputs/excite_exp_values.txt")