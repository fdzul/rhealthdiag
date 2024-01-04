
<!-- README.md is generated from README.Rmd. Please edit that file -->

# rhealthdiag

<!-- badges: start -->
<!-- badges: end -->

*The goal of rhealthdiag is a package developed in the department of
prevention and control of diseases transmitted by vector of the
[Secretary of Health of Veracruz](https://www.ssaver.gob.mx/) and with
colaboration of the federal level.*

### **overview**

**rhealthdiag** is a package designed to generate a health diagnosis of
the mortality of a state, municipality or locality using data from the
[INEGI](https://www.inegi.org.mx/programas/mortalidad/#Datos_abiertos).
The package contain the mortality of Puebla state (with the updates we
add all states) and the functions for generate the tables, static and
interactive maps.

**Datasets**

-   **`mortalidad_general_21`** Mortality of Puebla State (2012\_2019).
-   **`mortalidad_general_12`** Mortality of Guerrero State
    (2012\_2019).

**Functions**

-   **`mort_year_edo_dt`** Generates tables of causes of death from 2012
    to 2019 at the state level.

-   **`mort_year_mpo_dt`** generates tables of causes of death from 2012
    to 2019 at the municipality level.

-   **`mort_year_loc_dt`** Generates tables of causes of death from 2012
    to 2019 at the locality level.

## Installation

You can install the released version of healthdiagr from
[CRAN](https://CRAN.R-project.org) with:

``` r
#install.packages("rhealthdiag")
```

## Development version

To get a bug fix, or use a feature from the development version, you can
install boldenr from GitHub.

``` r
# install.packages("remotes")
remotes::install_github("fdzul/rhealthdiag")
```

or

``` r
# install.packages("devtools")
devtools::install_github("fdzul/rhealthdiag")
```

## Example

This is a basic example which shows you how load the mortality dataset
in R:

``` r
library(rhealthdiag)
head(rhealthdiag::mortalidad_general_21)
#>   ent_ocurr mun_ocurr loc_ocurr causa_def_cve lista_mex_cve   sexo edad_cve
#> 1        21       011      0001          I219           28A Hombre     4053
#> 2        21       085      0001          I619           30B  Mujer     4062
#> 3        21       005      9999          X700            54  Mujer     4014
#> 4        21       051      0012          V892           49B Hombre     4010
#> 5        21       114      0001          V892           49B Hombre     4019
#> 6        21       082      0001          G932           23Z Hombre     4082
#>   anio_ocur anio_nacim ocupacion_cve escolaridad_cve
#> 1      2012       1959            41               4
#> 2      2012       1950             2               1
#> 3      2012       1998             2               5
#> 4      2012       2002             2               3
#> 5      2012       1993            72               6
#> 6      2012       1930            41               4
#>                                                                                         causa_def_des
#> 1                                               Infarto agudo del miocardio, sin otra especificaciÃ³n
#> 2                                                        Hemorragia intraencefÃ¡lica, no especificada
#> 3 LesiÃ³n autoinfligida intencionalmente por ahorcamiento, estrangulamiento o sofocaciÃ³n en vivienda
#> 4                  Persona lesionada en accidente de trÃ¡nsito, de vehÃ­culo de motor no especificado
#> 5                  Persona lesionada en accidente de trÃ¡nsito, de vehÃ­culo de motor no especificado
#> 6                                                                  HipertensiÃ³n intracraneal benigna
#>         escolaridad_des              ocupacion_des              edad_des
#> 1     Primaria completa                       <NA> Cincuenta y tres años
#> 2       Sin escolaridad Profesionistas y tÃ©cnicos    Sesenta y dos años
#> 3 Secundaria incompleta Profesionistas y tÃ©cnicos          Catorce años
#> 4   Primaria incompleta Profesionistas y tÃ©cnicos             Diez años
#> 5   Secundaria completa                       <NA>       Diecinueve años
#> 6     Primaria completa                       <NA>    Ochenta y dos años
#>                                                                  lista_mex_des
#> 1                                                  Infarto agudo del miocardio
#> 2 Hemorragia intraencefálica y otras hemorragias intracraneales no traumáticas
#> 3                                     Lesiones autoinfligidas intencionalmente
#> 4                                  Accidentes de tráfico de vehículos de motor
#> 5                                  Accidentes de tráfico de vehículos de motor
#> 6                                  Las demás enfermedades del sistema nervioso
```

## Authors

-   **Felipe Antonio Dzul Manzanilla** -**<https://github.com/fdzul>** -
    Packages developed in github:

    1.  [denhotspots](https://github.com/fdzul/denhotspots).
    2.  [boldenr](https://github.com/fdzul/boldenr).
    3.  [dendata](https://github.com/fdzul/dendata).
    4.  [deneggs](https://github.com/fdzul/deneggs).
    5.  [rgeomex](https://github.com/fdzul/rgeomex).
    6.  [covid19mx](https://github.com/fdzul/covid19mx).

See also the list of
[contributors](https://github.com/fdzul/deneggs/contributors) who
participated in this project.

## License

This project is licensed under the MIT License - see the
[LICENSE.md](LICENSE.md) file for details

## Inspiration

The package was inspired by the need to contribute to generate health
diagnoses at the state, municipality and local level.

## Getting help

If you encounter a clear bug, please file a minimal reproducible example
on [github](https://github.com/fdzul/healthdiagr/issues). For questions
and other discussion, please feel free to contact me
(<felipe.dzul.m@gmail.com>)

------------------------------------------------------------------------

Please note that this project is released with a [Contributor Code of
Conduct](https://dplyr.tidyverse.org/CODE_OF_CONDUCT). By participating
in this project you agree to abide by its terms.
