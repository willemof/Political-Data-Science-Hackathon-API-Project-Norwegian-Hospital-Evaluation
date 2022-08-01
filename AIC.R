#AIC 
#Test how each variable performs separately 
value_arsverk.mod <- lm(polikliniske_konsultasjoner ~ value_arsverk, 
                    data = super_merge)

value_driftskostnader.mod <- lm(polikliniske_konsultasjoner ~ value_driftskostnader, 
                                data = super_merge)

