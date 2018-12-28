# Compression
# Compression of Mortality: the evolution of the variability in age at death in Latin America

This repository contains code and data from our paper looking at evolution of life expectancy and variability of age at death in Latin America. Please, note that there are codes in Stata and R. Relational models for mortality were developed in Stata, later analysis (IQR, C-50, and figures) were done in R. We tried to describe in detail what we did in each file. Paper is accepted for publication - Revista Latinoamericana de Población. The paper is co-authored by: Marcos Gonzaga, Bernardo L. Queiroz and Everton Lima

    1) Stata code to estimate latin america mortality pattern (standardmodel.do)
    2) Stata code to estimate mortality age profile for all countries in the study (standardmodel.do)
    3) R codes to produce all graphs presented in the paper. (figure.r and iqr_c50.r)
    4) Original database to performe all calculations (dados_mortalidade_to_stata.csv)
    5) Under-registration of death counts - please look at https://cran.r-project.org/web/packages/DDM/index.html
    6) Estimates of completeness of death counts coverage (death_counts_under-registration.csv)
    
    
# Information about paper

Marcos Roberto Gonzaga,
Universidade Federal do Rio Grande do Norte (UFRN)
marcosrg@ccet.ufrn.br
https://orcid.org/0000-0002-6088-3453

Bernardo L. Queiroz,
Universidade Federal de Minas Gerais (UFMG)
lanza@cedeplar.ufmg.br
https://orcid.org/0000-0002-2890-1025

Everton E. Campos De Lima,
Universidade Estadual de Campinas (UNICAMP)
everton@nepo.unicamp.br
https://orcid.org/0000-0001-6275-9854

Latin American countries are undergoing major changes in their mortality profiles due to unique epidemiological and health transitions in the region. The main goal of this paper is to study the evolution of the mortality age profiles and the distribution of age at death for a series of Latin America countries in order to identify the effects of mortality changes on the variability of age at death. We use data from different and alternative sources (WHO, LAHMD, and LAMBdA) to study this issue in the region. We first evaluate the quality of national-level mortality data overtime in Latin American countries. Using a relational model we estimate the mortality patterns by single year age-groups, for each country in Latin America. Lastly, we use traditional metrics of age at death variability to perform the analysis. Our results indicate that the quality of mortality data is improving over time for all countries we include. We also find a decrease in variability of age at death, and that the decrease has happened faster for females than for males. In recent years, increasing mortality due to external causes of deaths related to violence, have reduced the rise in life expectancy and birth and increased the variability in the age at death for several countries in the region. These results contribute to the study of mortality changes in Latin America looking at mortality compression and the variability of age at death. Over the last half-century there has been a reduction in the variability of age at death, but more recently, increases in external causes of death have been associated with a stagnation in the compression process. The analysis also provides some insight and questions about morbidity trends in the region. 
