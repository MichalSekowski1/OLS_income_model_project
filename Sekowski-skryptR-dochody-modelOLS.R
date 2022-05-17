library(pander)
library(ggplot2)
library(knitr) 
library(dplyr)
library(readxl)
library(car)
library(sandwich)
library(normtest)
dane <- read_excel("Baza-danych-ekonometria.xlsx", sheet = "gotowe")
View(dane)
#zbior danych zostal opracowany pod wzgledem brakow danych jeszcze w trakcie pracy w arkuszu programu Excel
#zmienna 'miejsce'mowiaca o wielkosci miejscowosci zamieszkania respondenta zostala juz rozkodowana na zmienne 0-1
#nalezy rozkodowac jeszcze zmienna dotyczaca wojewodztwa i wyksztalcenia
#wojewodztwa
dane$dolnoslaskie = 0
dane$kujawskopomorskie = 0
dane$lubelskie = 0
dane$lubuskie = 0
dane$lodzkie = 0
dane$malopolskie = 0
dane$mazowieckie = 0
dane$opolskie = 0
dane$podkarpackie = 0
dane$podlaskie = 0
dane$pomorskie = 0
dane$slaskie = 0
dane$swietokrzyskie = 0
dane$warminskomazurskie = 0
dane$wielkopolskie = 0
dane$zachodniopomorskie = 0
##

dane$dolnoslaskie[dane$woj==2] = 1
dane$kujawskopomorskie[dane$woj==4] = 1
dane$lubelskie[dane$woj==6] = 1
dane$lubuskie[dane$woj==8] = 1
dane$lodzkie[dane$woj==10] = 1
dane$malopolskie[dane$woj==12] = 1
dane$mazowieckie[dane$woj==14] = 1
dane$opolskie[dane$woj==16] = 1
dane$podkarpackie[dane$woj==18] = 1
dane$podlaskie[dane$woj==20] = 1
dane$pomorskie[dane$woj==22] = 1
dane$slaskie[dane$woj==24] = 1
dane$swietokrzyskie[dane$woj==26] = 1
dane$warminskomazurskie[dane$woj==28] = 1
dane$wielkopolskie[dane$woj==30] = 1
dane$zachodniopomorskie[dane$woj==32] = 1

# i wyksztalcenie
dane$gimnazjalne =0
dane$zasadniczezawodowe =0
dane$srednieogolnoksztalcace =0
dane$sredniezawodowe =0
dane$sredniepolicealne =0
dane$wyzszelic =0
dane$wyzszemgr =0

##

dane$gimnazjalne[dane$wykszt_7k==1] = 1
dane$zasadniczezawodowe[dane$wykszt_7k==2] = 1
dane$srednieogolnoksztalcace[dane$wykszt_7k==3] = 1
dane$sredniezawodowe[dane$wykszt_7k==4] = 1
dane$sredniepolicealne[dane$wykszt_7k==5] = 1
dane$wyzszelic[dane$wykszt_7k==6] = 1
dane$wyzszemgr[dane$wykszt_7k==7] = 1

#przeglad danych
##############################################dochod
ggplot(data=dane, aes(dane$dochod)) + 
  geom_histogram( colour="red", fill="cyan")
#histogram sugeruje uzycie zmiennej w formie zlogarytmowanej
dane$logdochod = log(dane$dochod)
ggplot(data=dane, aes(dane$logdochod)) + 
  geom_histogram( colour="red", fill="cyan")
#nadal nie jest to rozklad normalny, ale ksztalt histogramu jest znacznie bardziej zblizony do krzywej gaussa
#boxplot
ggplot(data=dane, aes( y=dane$logdochod)) +
  geom_boxplot() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  xlab("")
#############################################wiek
ggplot(data=dane, aes(dane$wiek)) + 
  geom_histogram( colour="red", fill="cyan")
#rozklad nie jest normalny, chociaz symetryczny
ggplot(data=dane, aes( y=dane$wiek)) +
  geom_boxplot() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  xlab("")
#############################################staz
ggplot(data=dane, aes(dane$staz)) + 
  geom_histogram( colour="red", fill="cyan")
ggplot(data=dane, aes( y=dane$staz)) +
  geom_boxplot() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  xlab("")

#pozostale zmienne sa binarne
#wstepny przeglad korelacji zmiennych
korploty <- read_excel("Baza-danych-ekonometria.xlsx", sheet = "korelacje")
View(korploty)


library(corrplot)
kor<-cor(korploty)
corrplot(kor, method="circle")
#wykres korelacji wybranych zmiennych wyglada w porzadku, z wyjatkiem relacji staz-wiek, tu występuje bardzo silna korelacja 
#(zgodnie z intuicja) i ktoras z tych zmiennych bedzie nalezalo usunac


korploty$logdochod <- dane$logdochod
pairs(dane[,c(4,6,9,5,12)])
#poza stazem i wiekiem nie widac szczegolnie liniowych zaleznosci
cor.test(dane$wiek, dane$staz, method=c("pearson"))
cor.test(dane$logdochod, dane$staz, method=c("pearson"))
cor.test(dane$logdochod, dane$wiek, method=c("pearson"))
##budowa modelu
#przy zmiennych 0-1 przyjmuje za poziom bazowy mieszkanca wsi z wojewodztwa mazowieckiego, z wyksztaleceniem gimnazjalnym lub nizszym
podstawa<-lm(logdochod~staz+I(staz^2)+plec+kierownik+zagranica+jobcy+obcy2+obcy3+w_matka1+w_ojciec1+malzenstwo+miasto20k+miasto100k+duzemiasto+warszawa+dolnoslaskie+lubelskie+lubuskie+lodzkie+malopolskie+opolskie+podkarpackie+podlaskie+pomorskie+slaskie+swietokrzyskie+wielkopolskie+zachodniopomorskie+kujawskopomorskie+warminskomazurskie+zasadniczezawodowe+srednieogolnoksztalcace+sredniezawodowe+sredniepolicealne+wyzszelic+wyzszemgr+I(plec*w_matka1)+I(plec*w_ojciec1)+I(plec*malzenstwo)+I(plec*staz), data=dane)
summary(podstawa)
##sprawdzenie czy model podstawowy spelnia zalozenia

#1.Zalozenie o prawidlowosci formy funkcyjnej

library(lmtest)
resettest(podstawa, power=2:3, type="fitted")
resettest(podstawa, power=2:3, type="regressor")

#p-value jest w obu przypadkach wieksze od 0,05 -> brak podstaw do odrzucenia hipotezy o liniowosci


#2. Zalozenie o homoskedastycznosci reszt
#analiza graficzna
reszty <- qplot(podstawa$residuals,
            geom = "histogram",
            bins = 100) +
  labs(title = "Histogram reszt",
       x = "reszty")
reszty
#histogram wskazuje brak normalnosci rozkladu
jb.norm.test(residuals(podstawa))
#test Breuscha_Pagana

bptest(podstawa)
#p-value jest znacznie mniejsze od 0,05; w modelu występuje heteroskedastycznosc reszt

plot(podstawa)
vif(podstawa)
##ogony reszt nie sa dopasowane do rozkladu normalnego, jednak nie ma obserwacji ktore w istotny sposob szkodziliby modelowi
##jest jednak kilka obserwacji ktorym warto sie przyjrzec
##macierz odporna white
coeftest(podstawa, vcov. = vcovHC(podstawa, type="HC0"))
odporny = coeftest(podstawa, vcov. = vcovHC(podstawa, type="HC0"))
show(odporny)
##########################################################################
#procedura od ogolu do szczegolu dla modeli oszacowanych przy uzyciu macierzy odpornej white
library(stargazer)
macierz_odporna = vcovHC(podstawa, type="HC0")
podstawa_odporna = coeftest(podstawa, vcov.=macierz_odporna)
#usuwam zmienna zasadnicze zawodowe
iteracja1<-lm(logdochod~staz+I(staz^2)+plec+kierownik+zagranica+jobcy+obcy2+obcy3+w_matka1+w_ojciec1+malzenstwo+miasto20k+miasto100k+duzemiasto+warszawa+dolnoslaskie+lubelskie+lubuskie+lodzkie+malopolskie+opolskie+podkarpackie+podlaskie+pomorskie+slaskie+swietokrzyskie+wielkopolskie+zachodniopomorskie+kujawskopomorskie+warminskomazurskie+srednieogolnoksztalcace+sredniezawodowe+sredniepolicealne+wyzszelic+wyzszemgr+I(plec*w_matka1)+I(plec*w_ojciec1)+I(plec*malzenstwo)+I(plec*staz), data=dane)
macierz_odporna1 = vcovHC(iteracja1, type="HC0")
iteracja1odporna = coeftest(iteracja1, vcov.=macierz_odporna1)
show(iteracja1odporna)
#
linearHypothesis(model = podstawa, c("w_ojciec1=0",
                                     "zasadniczezawodowe=0"),
                 vcov.=macierz_odporna)
#
iteracja2<-lm(logdochod~staz+I(staz^2)+plec+kierownik+zagranica+jobcy+obcy2+obcy3+w_matka1+malzenstwo+miasto20k+miasto100k+duzemiasto+warszawa+dolnoslaskie+lubelskie+lubuskie+lodzkie+malopolskie+opolskie+podkarpackie+podlaskie+pomorskie+slaskie+swietokrzyskie+wielkopolskie+zachodniopomorskie+kujawskopomorskie+warminskomazurskie+srednieogolnoksztalcace+sredniezawodowe+sredniepolicealne+wyzszelic+wyzszemgr+I(plec*w_matka1)+I(plec*w_ojciec1)+I(plec*malzenstwo)+I(plec*staz), data=dane)
macierz_odporna2 = vcovHC(iteracja2, type="HC0")
iteracja2odporna = coeftest(iteracja2, vcov.=macierz_odporna2)
show(iteracja2odporna)
#
linearHypothesis(model = podstawa, c("w_ojciec1=0",
                                     "zasadniczezawodowe=0",
                                     "obcy2=0"),
                 vcov.=macierz_odporna)
#
iteracja3<-lm(logdochod~staz+I(staz^2)+plec+kierownik+zagranica+jobcy+obcy3+w_matka1+malzenstwo+miasto20k+miasto100k+duzemiasto+warszawa+dolnoslaskie+lubelskie+lubuskie+lodzkie+malopolskie+opolskie+podkarpackie+podlaskie+pomorskie+slaskie+swietokrzyskie+wielkopolskie+zachodniopomorskie+kujawskopomorskie+warminskomazurskie+srednieogolnoksztalcace+sredniezawodowe+sredniepolicealne+wyzszelic+wyzszemgr+I(plec*w_matka1)+I(plec*w_ojciec1)+I(plec*malzenstwo)+I(plec*staz), data=dane)
macierz_odporna3 = vcovHC(iteracja3, type="HC0")
iteracja3odporna = coeftest(iteracja3, vcov.=macierz_odporna3)
show(iteracja3odporna)
#
linearHypothesis(model = podstawa, c("w_ojciec1=0",
                                     "zasadniczezawodowe=0",
                                     "obcy2=0",
                                     "miasto20k=0",
                                     "miasto100k=0",
                                     "duzemiasto=0"),
                 vcov.=macierz_odporna)
#
iteracja4<-lm(logdochod~staz+I(staz^2)+plec+kierownik+zagranica+jobcy+obcy3+w_matka1+malzenstwo+warszawa+dolnoslaskie+lubelskie+lubuskie+lodzkie+malopolskie+opolskie+podkarpackie+podlaskie+pomorskie+slaskie+swietokrzyskie+wielkopolskie+zachodniopomorskie+kujawskopomorskie+warminskomazurskie+srednieogolnoksztalcace+sredniezawodowe+sredniepolicealne+wyzszelic+wyzszemgr+I(plec*w_matka1)+I(plec*w_ojciec1)+I(plec*malzenstwo)+I(plec*staz), data=dane)
macierz_odporna4 = vcovHC(iteracja4, type="HC0")
iteracja4odporna = coeftest(iteracja4, vcov.=macierz_odporna4)
show(iteracja4odporna)
#
linearHypothesis(model = podstawa, c("w_ojciec1=0",
                                     "zasadniczezawodowe=0",
                                     "obcy2=0",
                                     "miasto20k=0",
                                     "miasto100k=0",
                                     "duzemiasto=0",
                                     "zachodniopomorskie=0",
                                     "slaskie=0",
                                     "dolnoslaskie=0"),
                 vcov.=macierz_odporna)
#
iteracja5<-lm(logdochod~staz+I(staz^2)+plec+kierownik+zagranica+jobcy+obcy3+w_matka1+malzenstwo+warszawa+lubelskie+lubuskie+lodzkie+malopolskie+opolskie+podkarpackie+podlaskie+pomorskie+swietokrzyskie+wielkopolskie+kujawskopomorskie+warminskomazurskie+srednieogolnoksztalcace+sredniezawodowe+sredniepolicealne+wyzszelic+wyzszemgr+I(plec*w_matka1)+I(plec*w_ojciec1)+I(plec*malzenstwo)+I(plec*staz), data=dane)
macierz_odporna5 = vcovHC(iteracja5, type="HC0")
iteracja5odporna = coeftest(iteracja5, vcov.=macierz_odporna5)
show(iteracja5odporna)
#
linearHypothesis(model = podstawa, c("w_ojciec1=0",
                                     "zasadniczezawodowe=0",
                                     "obcy2=0",
                                     "miasto20k=0",
                                     "miasto100k=0",
                                     "duzemiasto=0",
                                     "zachodniopomorskie=0",
                                     "slaskie=0",
                                     "dolnoslaskie=0",
                                     "pomorskie=0",
                                     "podlaskie=0",
                                     "lubuskie=0"),
                 vcov.=macierz_odporna)
#
iteracja6<-lm(logdochod~staz+I(staz^2)+plec+kierownik+zagranica+jobcy+obcy3+w_matka1+malzenstwo+warszawa+lubelskie+lodzkie+malopolskie+opolskie+podkarpackie+swietokrzyskie+wielkopolskie+kujawskopomorskie+warminskomazurskie+srednieogolnoksztalcace+sredniezawodowe+sredniepolicealne+wyzszelic+wyzszemgr+I(plec*w_matka1)+I(plec*w_ojciec1)+I(plec*malzenstwo)+I(plec*staz), data=dane)
macierz_odporna6 = vcovHC(iteracja6, type="HC0")
iteracja6odporna = coeftest(iteracja6, vcov.=macierz_odporna6)
show(iteracja6odporna)
#
linearHypothesis(model = podstawa, c("w_ojciec1=0",
                                     "zasadniczezawodowe=0",
                                     "obcy2=0",
                                     "miasto20k=0",
                                     "miasto100k=0",
                                     "duzemiasto=0",
                                     "zachodniopomorskie=0",
                                     "slaskie=0",
                                     "dolnoslaskie=0",
                                     "pomorskie=0",
                                     "podlaskie=0",
                                     "lubuskie=0"),
                 vcov.=macierz_odporna)
#
iteracja7<-lm(logdochod~staz+I(staz^2)+plec+kierownik+zagranica+jobcy+obcy3+w_matka1+malzenstwo+warszawa+lubelskie+lodzkie+malopolskie+opolskie+podkarpackie+swietokrzyskie+wielkopolskie+kujawskopomorskie+warminskomazurskie+srednieogolnoksztalcace+sredniezawodowe+sredniepolicealne+wyzszelic+wyzszemgr+I(plec*w_matka1)+I(plec*w_ojciec1)+I(plec*malzenstwo)+I(plec*staz), data=dane)
macierz_odporna7 = vcovHC(iteracja7, type="HC0")
iteracja7odporna = coeftest(iteracja7, vcov.=macierz_odporna7)
show(iteracja7odporna)
#
linearHypothesis(model = podstawa, c("w_ojciec1=0",
                                     "zasadniczezawodowe=0",
                                     "obcy2=0",
                                     "miasto20k=0",
                                     "miasto100k=0",
                                     "duzemiasto=0",
                                     "zachodniopomorskie=0",
                                     "slaskie=0",
                                     "dolnoslaskie=0",
                                     "pomorskie=0",
                                     "podlaskie=0",
                                     "lubuskie=0",
                                     
                                     "malopolskie=0",
                                     "kujawskopomorskie=0"),
                 vcov.=macierz_odporna)
#
iteracja8<-lm(logdochod~staz+I(staz^2)+plec+kierownik+zagranica+jobcy+obcy3+w_matka1+malzenstwo+warszawa+lubelskie+lodzkie+opolskie+podkarpackie+swietokrzyskie+wielkopolskie+warminskomazurskie+srednieogolnoksztalcace+sredniezawodowe+sredniepolicealne+wyzszelic+wyzszemgr+I(plec*w_matka1)+I(plec*w_ojciec1)+I(plec*malzenstwo)+I(plec*staz), data=dane)
macierz_odporna8 = vcovHC(iteracja8, type="HC0")
iteracja8odporna = coeftest(iteracja8, vcov.=macierz_odporna8)
show(iteracja8odporna)
#
linearHypothesis(model = podstawa, c("w_ojciec1=0",
                                     "zasadniczezawodowe=0",
                                     "obcy2=0",
                                     "miasto20k=0",
                                     "miasto100k=0",
                                     "duzemiasto=0",
                                     "zachodniopomorskie=0",
                                     "slaskie=0",
                                     "dolnoslaskie=0",
                                     "pomorskie=0",
                                     "podlaskie=0",
                                     "lubuskie=0",
                                    
                                     "malopolskie=0",
                                     "kujawskopomorskie=0",
                                     "I(plec * w_matka1)=0"),
                 vcov.=macierz_odporna)
#
iteracja9<-lm(logdochod~staz+I(staz^2)+plec+kierownik+zagranica+jobcy+obcy3+w_matka1+malzenstwo+warszawa+lubelskie+lodzkie+opolskie+podkarpackie+swietokrzyskie+wielkopolskie+warminskomazurskie+srednieogolnoksztalcace+sredniezawodowe+sredniepolicealne+wyzszelic+wyzszemgr+I(plec*w_ojciec1)+I(plec*malzenstwo)+I(plec*staz), data=dane)
macierz_odporna9 = vcovHC(iteracja9, type="HC0")
iteracja9odporna = coeftest(iteracja9, vcov.=macierz_odporna9)
show(iteracja9odporna)
#
linearHypothesis(model = podstawa, c("w_ojciec1=0",
                                     "zasadniczezawodowe=0",
                                     "obcy2=0",
                                     "miasto20k=0",
                                     "miasto100k=0",
                                     "duzemiasto=0",
                                     "zachodniopomorskie=0",
                                     "slaskie=0",
                                     "dolnoslaskie=0",
                                     "pomorskie=0",
                                     "podlaskie=0",
                                     "lubuskie=0",
                                     
                                     "malopolskie=0",
                                     "kujawskopomorskie=0",
                                     "I(plec * w_matka1)=0",
                                     "obcy3=0",
                                     "swietokrzyskie=0"),
                 vcov.=macierz_odporna)
#
iteracja10<-lm(logdochod~staz+I(staz^2)+plec+kierownik+zagranica+jobcy+w_matka1+malzenstwo+warszawa+lubelskie+lodzkie+opolskie+podkarpackie+wielkopolskie+warminskomazurskie+srednieogolnoksztalcace+sredniezawodowe+sredniepolicealne+wyzszelic+wyzszemgr+I(plec*w_ojciec1)+I(plec*malzenstwo)+I(plec*staz), data=dane)
macierz_odporna10 = vcovHC(iteracja10, type="HC0")
iteracja10odporna = coeftest(iteracja10, vcov.=macierz_odporna10)
show(iteracja10odporna)
#
linearHypothesis(model = podstawa, c("w_ojciec1=0",
                                     "zasadniczezawodowe=0",
                                     "obcy2=0",
                                     "miasto20k=0",
                                     "miasto100k=0",
                                     "duzemiasto=0",
                                     "zachodniopomorskie=0",
                                     "slaskie=0",
                                     "dolnoslaskie=0",
                                     "pomorskie=0",
                                     "podlaskie=0",
                                     "lubuskie=0",
                                     
                                     "malopolskie=0",
                                     "kujawskopomorskie=0",
                                     "I(plec * w_matka1)=0",
                                     "obcy3=0",
                                     "swietokrzyskie=0",
                                     "w_matka1=0",
                                     "I(plec * w_ojciec1)=0"),
                 vcov.=macierz_odporna)
#
iteracja11<-lm(logdochod~staz+I(staz^2)+plec+kierownik+zagranica+jobcy+malzenstwo+warszawa+lubelskie+lodzkie+opolskie+podkarpackie+wielkopolskie+warminskomazurskie+srednieogolnoksztalcace+sredniezawodowe+sredniepolicealne+wyzszelic+wyzszemgr+I(plec*malzenstwo)+I(plec*staz), data=dane)
macierz_odporna11 = vcovHC(iteracja11, type="HC0")
iteracja11odporna = coeftest(iteracja11, vcov.=macierz_odporna11)
show(iteracja11odporna)
#
linearHypothesis(model = podstawa, c("w_ojciec1=0",
                                     "zasadniczezawodowe=0",
                                     "obcy2=0",
                                     "miasto20k=0",
                                     "miasto100k=0",
                                     "duzemiasto=0",
                                     "zachodniopomorskie=0",
                                     "slaskie=0",
                                     "dolnoslaskie=0",
                                     "pomorskie=0",
                                     "podlaskie=0",
                                     "lubuskie=0",
                                     
                                     "malopolskie=0",
                                     "kujawskopomorskie=0",
                                     "I(plec * w_matka1)=0",
                                     "obcy3=0",
                                     "swietokrzyskie=0",
                                     "w_matka1=0",
                                     "I(plec * w_ojciec1)=0",
                                     "wielkopolskie=0",
                                     "opolskie=0"),
                 vcov.=macierz_odporna)
#
iteracja12<-lm(logdochod~staz+I(staz^2)+plec+kierownik+zagranica+jobcy+malzenstwo+warszawa+lubelskie+lodzkie+podkarpackie+warminskomazurskie+srednieogolnoksztalcace+sredniezawodowe+sredniepolicealne+wyzszelic+wyzszemgr+I(plec*malzenstwo)+I(plec*staz), data=dane)
macierz_odporna12 = vcovHC(iteracja12, type="HC0")
iteracja12odporna = coeftest(iteracja12, vcov.=macierz_odporna12)
show(iteracja12odporna)
#
linearHypothesis(model = podstawa, c("w_ojciec1=0",
                                     "zasadniczezawodowe=0",
                                     "obcy2=0",
                                     "miasto20k=0",
                                     "miasto100k=0",
                                     "duzemiasto=0",
                                     "zachodniopomorskie=0",
                                     "slaskie=0",
                                     "dolnoslaskie=0",
                                     "pomorskie=0",
                                     "podlaskie=0",
                                     "lubuskie=0",
                                     
                                     "malopolskie=0",
                                     "kujawskopomorskie=0",
                                     "I(plec * w_matka1)=0",
                                     "obcy3=0",
                                     "swietokrzyskie=0",
                                     "w_matka1=0",
                                     "I(plec * w_ojciec1)=0",
                                     "wielkopolskie=0",
                                     "opolskie=0",
                                     "warminskomazurskie=0",
                                     "lodzkie=0"),
                 vcov.=macierz_odporna)
#
iteracja13<-lm(logdochod~staz+I(staz^2)+plec+kierownik+zagranica+jobcy+malzenstwo+warszawa+lubelskie+podkarpackie+srednieogolnoksztalcace+sredniezawodowe+sredniepolicealne+wyzszelic+wyzszemgr+I(plec*malzenstwo)+I(plec*staz), data=dane)
macierz_odporna13 = vcovHC(iteracja13, type="HC0")
iteracja13odporna = coeftest(iteracja13, vcov.=macierz_odporna13)
show(iteracja13odporna)
############################################################
#osiagnieto statystyczna istotnosc wszystkich parametrow
summary(iteracja13)
stargazer(podstawa)
resettest(iteracja13, power=2:3, type="fitted")
resettest(iteracja13, power=2:3, type="regressor")
vif(iteracja13)
stargazer(podstawa, podstawa_odporna, iteracja13, iteracja13odporna, type=text(), title="Results", align=TRUE)
#########








