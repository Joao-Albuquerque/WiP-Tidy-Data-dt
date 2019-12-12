

#### WOMEN IN PARLIAMENT - data.table ####


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



# Portugal versus European Union countries:
WP[Country %in% c("Portugal", "Sweden", "Spain", "Hungary", "Romania",
                  "Finland", "Germany", "European Union")] %>%
  ggplot(aes(Year, pctWiP, colour=Country)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = seq(1990, 2020, 5)) +
  scale_y_continuous(limits = c(0,50),
                     breaks = seq(0,50,10)) +
  ggtitle("Women in Parliament: EU Countries") +
  ylab("% Women in Parliament")


# Countries with the highest percentage of women in parliament:
WP[order(-pctWiP), head(.SD,10)]

# Highest percentage by year:
WP[order(Year, -pctWiP), head(.SD, 1), by=Year]


# Merging continent: (acrescenta duas novas variaveis: o continente e o codigo do pais)
# install.packages("countrycode")
require(countrycode)

c1 <- as.data.table(codelist)[, .(continent, wb)]
setnames(c1, c("continent", "wb"),
         c("Continent", "Code"))
cWP <- c1[WP, on="Code"]
cWP


# Highest percentage by year and continent:
cWP[Year %in% c(1990,2018) & !is.na(Continent)][
  order(Year, -pctWiP), head(.SD, 1),
  by = .(Year,Continent)][
    order(Continent, Year),
    .(Continent, Year, Country, pctWiP)
  ]


# Decline in percentage:
dWP <- cWP[
  order(Country, Year), .SD[c(1,.N)],
  by=Country][,
    pctDiff := pctWiP - shift(pctWiP), by=Country][
    pctDiff<0][
    order(pctDiff)]

dWP[!is.na(Continent),
    .(Country, pctWiP, pctDiff)]


# Plot the trend lines for countries with at least a 5% decline:

# Select the countries to plot:
dclpct <- unique(dWP[!is.na(Continent) &
                       pctDiff <= -5]$Country)

WP[Country %in% dclpct] %>%
  ggplot(aes(Year, pctWiP, colour=Country)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks=seq(1990, 2020, 5)) +
  scale_y_continuous(limits=c(0,40),
                     breaks=seq(0, 40, 10)) +
  ggtitle("Women in Parliament: Decline >= 5%") +
  ylab("% Women in Parliament")



cWP[!is.na(Continent),
    `:=`(RankG = rank(-pctWiP), TotalG = .N),
    by= .(Year)]

cWP[Country=="Portugal",
    .(Country, Year, pctWiP, Ratio, RankG, TotalG)][
      order(Year)
      ]
# Continent ranks by year:
cWP[!is.na(Continent),
    `:=`(RankC = rank(-pctWiP), TotalC = .N),
    by = .(Continent, Year)
    ]
cWP[Country=="Portugal",
    .(Country, Year, pctWiP, Ratio, RankC, TotalC)][
      order(Year)
    ]

# Plot of Portugal's ranking in Europe:
cWP[Country %in% c("Portugal", "Sweden", "Spain", "Hungary", "Romania", "Finland", "Germany")] %>%
  ggplot(aes(Year, RankC, colour=Country)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = seq(1990, 2020, 5)) +
  scale_y_continuous(limits=c(0,45),
                     breaks = seq(0, 45, 10)) +
  ggtitle("Women in Parliament: Ranked") +
  ylab("Rank in Europe")




