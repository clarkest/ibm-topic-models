
# base models for exploring

cuts.15 <- cutModels(0.15, controls=c("is.manager", "is.exec", "gender"))
cuts.20 <- cutModels(0.20, controls=c("is.manager", "is.exec", "gender"))
cuts.25 <- cutModels(0.25, controls=c("is.manager", "is.exec", "gender"))
cuts.20.forum <- 
  cutModels(0.20, controls=c("is.manager", "is.exec", "gender", "forum"))
stargazer(cuts.15, cuts.15.a, type="text")



# with focus
#cuts.15.c <- cutModels(0.15, controls=c("is.manager", "is.exec", "gender", "focus"))
cuts.15.noac <- cutModels(0.15, controls=c("is.manager", "is.exec", "gender"))
cuts.15.ac <- cutModels(0.15, controls=c("is.manager", "is.exec", "gender", 
                                         "adj.focus", "solo.topic"))
cuts.15.ac.2 <- cutModels(0.15, controls=c("is.manager", "is.exec", "gender", 
                                           "adj.focus", "adj.conc.sq", "solo.topic"))

stargazer(cuts.15.noac, cuts.15.ac, cuts.15.ac.2, type="text")

# both focus and interacting focus with topic
# ----  though this isn't taking into account how concentrated the focal topic is 
cuts.20.ac.time <- cutModels(0.2, controls=c("is.manager", "is.exec", "gender", "adj.focus", "solo.topic",
                                             "is.first.comment","missing.parent","log.sec.since.parent", 
                                             "log.sec.since.parent.sq","u.s.time.", "continent2","last.period"))
cuts.20.ac <- cutModels(0.2, controls=c("is.manager", "is.exec", "gender", "adj.focus", 
                                        "solo.topic", "u.s.time.", "continent2","last.period"))
cuts.20.ac.int <- cutModels(0.2, 
                            controls=c("is.manager", "is.exec", "gender", "adj.focus", "solo.topic"), 
                            topic.interaction="adj.focus")
# stargazer(cuts.20.ac, cuts.20.ac.int, type="text")
stargazer(cuts.20.ac, cuts.20.ac.time, type="text")


# 1	occupation, gender, time and region
cuts.20.1 <- cutModels(NULL, controls=c("is.manager", "is.exec", "gender", 
                                        "u.s.time.", "continent2", "last.period"))
# 2	above + post length, solo topic, concentration, first comment, missing parent, log seconds, and forum
cuts.20.2 <- cutModels(NULL, controls=c("is.manager", "is.exec", "gender", 
                                        "u.s.time.", "continent2", "last.period",
                                        "log.length", "focus",  #"solo.topic",
                                        "is.first.comment","missing.parent","log.sec.since.parent", 
                                        "forum"))
cuts.20.2.int <- cutModels(NULL, controls=c("is.manager", "is.exec", "gender", 
                                            "u.s.time.", "continent2", "last.period",
                                            "log.length", "focus",  #"solo.topic",
                                            "is.first.comment","missing.parent","log.sec.since.parent", 
                                            "forum"),
                           interaction.terms = c("log.length : focus"))

# 3	above + topics > .20
cuts.20.3 <- cutModels(0.2, controls=c("is.manager", "is.exec", "gender", 
                                       "u.s.time.", "continent2", "last.period",
                                       "log.length", "focus", #"solo.topic",
                                       "is.first.comment","missing.parent","log.sec.since.parent", 
                                       "forum"))
cuts.20.3.int <- cutModels(0.2, controls=c("is.manager", "is.exec", "gender", 
                                           "u.s.time.", "continent2", "last.period",
                                           "log.length", "focus", #"solo.topic",
                                           "is.first.comment","missing.parent","log.sec.since.parent", 
                                           "forum"),
                           interaction.terms = c("log.length : focus"))

cuts.20.3.exp10 <- cutModels(0.2, controls=c("is.manager", "is.exec", "gender", 
                                             "u.s.time.", "continent2", "last.period",
                                             "log.length", "focus", #"solo.topic",
                                             "is.first.comment","missing.parent","exp.excite.10","log.sec.since.parent", 
                                             "forum"),
                             interaction.terms = c("log.length : focus"))


cuts.20.3.we <- cutModels(0.2, controls=c("is.manager", "is.exec", "gender", 
                                          "u.s.time.", "continent2", "last.period",
                                          "log.length", "focus", #"solo.topic",
                                          "is.first.comment","missing.parent", "log.sec.since.parent", 
                                          "forum", "exp.excite.10",
                                          "identity_"))
# 4 maybe do a model with topic dummies only (to give us baseline descriptive measure)
cuts.20.4 <- cutModels(0.2, controls=c("last.period", "log.length"))





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