

require(data.table)
require(here)
require(ggplot2)
require(magrittr)


wip <- fread(here("data", "WB-WiP.csv"),
             skip=4, header = TRUE,
             check.names = TRUE)


# Verificar que todos os valores sao NA:
wip[, .N, by=.(V65)]

# Remover colunas:
wip[,c("Indicator.Name", "Indicator.Code", "V65") := NULL]

# Alterar os nomes de colunas:
setnames(wip, c("Country.Name", "Country.Code"),
         c("Country", "Code"))

head(names(wip))
tail(names(wip))


# Reshape data: para cada pais o ano torna-se uma linha:
WP <- melt(wip,
           id.vars = c("Country", "Code"),
           measure = patterns ("^X"),
           variable.name = "YearC",
           value.name = c("pctWiP"),
           na.rm = TRUE)

# Atencao: as variaveis não são todas do mesmo tipo:
WP

# Definir Ano como variavel numerica, e criar a variavel para o ratio de homens no parlamento:
# (por os acentos ao contrario para funcionar)
WP[, `:=` (Year=as.numeric(gsub("[^[:digit:].]", "", YearC)),
         Ratio = (100-pctWiP)/pctWiP)][
           , YearC := NULL]
setcolorder(WP, c("Country", "Code", "Year","pctWiP","Ratio"))

WP

WP[Country %in% "Portugal"]

WP[Country %in% "Portugal"] %>%
ggplot(aes(Year, pctWiP)) +
  geom_line() + geom_point() +
  scale_y_continuous(limits=c(0,50)) +
  ylab("% Women in Parliament")

