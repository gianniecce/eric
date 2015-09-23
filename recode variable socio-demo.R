
giac$eduH = ifelse(giac$w3h1Q35 == 'universite, ecole polytechnique', 1, 0)
giac$eduF = ifelse(giac$w3f1Q35 == 'universite, ecole polytechnique', 1, 0)

giac$emptauxH = cut( as.numeric(as.character(giac$w3h1Q36_1)), breaks = c(-Inf, 0, 99, Inf), labels = c('Not Working', 'Part Time', 'Full Time')) 
giac$emptauxF = cut( as.numeric(as.character(giac$w3f1Q36_1)), breaks = c(-Inf, 0, 99, Inf), labels = c('Not Working', 'Part Time', 'Full Time')) 

as.numeric(dta3$AgeLastChild) 
