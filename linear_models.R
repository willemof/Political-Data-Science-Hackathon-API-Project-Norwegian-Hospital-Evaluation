#AIC 

# On polikliniske_konsultasjoner  -----------------------------------------

# Test how each variable perfoms seperately 
value_arsverk.mod <- lm(polikliniske_konsultasjoner ~ value_arsverk, 
                        data = super_merge)

value_arsverk.mod


value_driftskostnader.mod <- lm(polikliniske_konsultasjoner ~ value_driftskostnader, 
                                data = super_merge)

value_driftskostnader.mod


value_erfaringer.mod <- lm(polikliniske_konsultasjoner ~ value_erfaringer, 
                                data = super_merge)

value_erfaringer.mod

# Looking at the fist two variables together: 

value_arsverk.value_driftskostnader.mod <- lm(polikliniske_konsultasjoner ~ value_arsverk + value_driftskostnader, 
                           data = super_merge)

value_arsverk.value_driftskostnader.mod

# Look at the all three variables together: 

combination.mod <- lm(polikliniske_konsultasjoner ~ value_arsverk +
                                              value_driftskostnader +
                                              value_erfaringer, 
                                              data = super_merge)

combination.mod

# Look at the interaction of all three variables, if they can explain changes in consultations better
# than previous models: 


interaction.mod <- lm(polikliniske_konsultasjoner ~ value_arsverk*
                                                value_driftskostnader*
                                                value_erfaringer, 
                                              data = super_merge)
interaction.mod

summary(interaction.mod)

interaction1.mod <- lm(polikliniske_konsultasjoner ~ value_arsverk*
                         value_driftskostnader, 
                       data = super_merge)
interaction1.mod

# Compare the model 

install.packages("AICcmodavg")
library(AICcmodavg)

# Put the models into a list and name them so the AIC table is easier to read 

models <- list(value_arsverk.mod, value_driftskostnader.mod, value_erfaringer.mod, 
               value_arsverk.value_driftskostnader.mod, 
               combination.mod, interaction.mod, interaction1.mod)

model.names <- c('value_arsverk.mod', 'value_driftskostnader.mod', 'value_erfaringer.mod', 
                 'value_arsverk.value_driftskostnader.mod', 
                 'combination.mod', 'interaction.mod', 'interaction1.mod')

# Run aictab() to do the comparison 
aictab(cand.set = models, modnames = model.names)


# Interpreting results: 
# The best-fit model is always listed first 
# The smaller AICc value - the better the model fit 
# How to make a graph of interaction mode? 

# On dagbehandlinger  -----------------------------------------------------
value_arsverk.mod <- lm(dagbehandlinger_oppholdsdager ~ value_arsverk, 
                        data = super_merge)

value_arsverk.mod


value_driftskostnader.mod <- lm(dagbehandlinger_oppholdsdager ~ value_driftskostnader, 
                                data = super_merge)

value_driftskostnader.mod


value_erfaringer.mod <- lm(dagbehandlinger_oppholdsdager ~ value_erfaringer, 
                           data = super_merge)

value_erfaringer.mod

# Looking at the fist two variables together: 

value_arsverk.value_driftskostnader.mod <- lm(dagbehandlinger_oppholdsdager ~ value_arsverk +
                                                value_driftskostnader, 
                                              data = super_merge)

value_arsverk.value_driftskostnader.mod

# Look at the all three variables together: 

combination.mod <- lm(dagbehandlinger_oppholdsdager ~ value_arsverk +
                        value_driftskostnader +
                        value_erfaringer, 
                      data = super_merge)

combination.mod

# Look at the interaction of all three variables, if they can explain changes in consultations better
# than previous models: 


interaction.mod <- lm(dagbehandlinger_oppholdsdager ~ value_arsverk*
                        value_driftskostnader*
                        value_erfaringer, 
                      data = super_merge)
interaction.mod

summary(interaction.mod)

interaction1.mod <- lm(dagbehandlinger_oppholdsdager ~ value_arsverk*
                         value_driftskostnader, 
                       data = super_merge)
interaction1.mod

# Compare the model 

# Put the models into a list and name them so the AIC table is easier to read 

models <- list(value_arsverk.mod, value_driftskostnader.mod, value_erfaringer.mod, 
               value_arsverk.value_driftskostnader.mod, 
               combination.mod, interaction.mod, interaction1.mod)

model.names <- c('value_arsverk.mod', 'value_driftskostnader.mod', 'value_erfaringer.mod', 
                 'value_arsverk.value_driftskostnader.mod', 
                 'combination.mod', 'interaction.mod', 'interaction1.mod')

# Run aictab() to do the comparison 
aictab(cand.set = models, modnames = model.names)


# On liggedager  ----------------------------------------------------------

value_arsverk.mod <- lm(liggedager_oppholdsdogn ~ value_arsverk, 
                        data = super_merge)

value_arsverk.mod


value_driftskostnader.mod <- lm(liggedager_oppholdsdogn ~ value_driftskostnader, 
                                data = super_merge)

value_driftskostnader.mod


value_erfaringer.mod <- lm(liggedager_oppholdsdogn ~ value_erfaringer, 
                           data = super_merge)

value_erfaringer.mod

# Looking at the fist two variables together: 

value_arsverk.value_driftskostnader.mod <- lm(liggedager_oppholdsdogn ~ value_arsverk +
                                                value_driftskostnader, 
                                              data = super_merge)

value_arsverk.value_driftskostnader.mod

# Look at the all three variables together: 

combination.mod <- lm(liggedager_oppholdsdogn ~ value_arsverk +
                        value_driftskostnader +
                        value_erfaringer, 
                      data = super_merge)

combination.mod

# Look at the interaction of all three variables, if they can explain changes in consultations better
# than previous models: 


interaction.mod <- lm(liggedager_oppholdsdogn ~ value_arsverk*
                        value_driftskostnader*
                        value_erfaringer, 
                      data = super_merge)
interaction.mod

summary(interaction.mod)

interaction1.mod <- lm(liggedager_oppholdsdogn ~ value_arsverk*
                         value_driftskostnader, 
                       data = super_merge)
interaction1.mod

# Compare the model 

# Put the models into a list and name them so the AIC table is easier to read 

models <- list(value_arsverk.mod, value_driftskostnader.mod, value_erfaringer.mod, 
               value_arsverk.value_driftskostnader.mod, 
               combination.mod, interaction.mod, interaction1.mod)

model.names <- c('value_arsverk.mod', 'value_driftskostnader.mod', 'value_erfaringer.mod', 
                 'value_arsverk.value_driftskostnader.mod', 
                 'combination.mod', 'interaction.mod', 'interaction1.mod')

# Run aictab() to do the comparison 
aictab(cand.set = models, modnames = model.names)


# Run the three linear models ---------------------------------------------
# Consultations
interaction.polikliniske_konsultasjoner.mod <- lm(polikliniske_konsultasjoner ~ value_arsverk*
                        value_driftskostnader*
                        value_erfaringer, 
                      data = super_merge)
interaction.polikliniske_konsultasjoner.mod

summary(interaction.polikliniske_konsultasjoner.mod)


interaction.dagbehandlinger.mod <- lm(dagbehandlinger_oppholdsdager ~ value_arsverk*
                        value_driftskostnader*
                        value_erfaringer, 
                      data = super_merge)
interaction.dagbehandlinger.mod

summary(interaction.dagbehandlinger.mod)


interaction.liggedager.mod <- lm(liggedager_oppholdsdogn ~ value_arsverk*
                        value_driftskostnader*
                        value_erfaringer, 
                      data = super_merge)
interaction.liggedager.mod

summary(interaction.liggedager.mod)

































