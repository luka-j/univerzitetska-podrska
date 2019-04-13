setwd("~/Documents/up")
library(stringr)
library(readr)
library(dplyr)
library(magrittr)
library(ggplot2)
source("zvanja.R")

ffbg.txt <- read_file("raw/ff-bg.txt")
ffbg <- str_split_fixed(ffbg.txt, fixed("\n"), n = str_count(ffbg.txt, "\n")) %>% str_trim
ffbg.data <- data.frame(ime = str_split_fixed(ffbg, fixed(","), n=2)[,1])
ffbg.data$zvanje <- dict[[str_trim(str_split_fixed(ffbg, fixed(","), 2)[,2])]]
ffbg.data$drzava <- "Srbija"
ffbg.data$univerzitet <- "Univerzitet u Beogradu"
ffbg.data$fakultet <- "Filozofski fakultet"

ffns.txt <- read_file("raw/ff-ns.txt")
ffns <- str_split_fixed(str_split_fixed(ffns.txt, fixed("\n"), str_count(ffns.txt, "\n")), fixed("."), 2)[,2] %>% str_trim
ffns.data <- data.frame(ime = str_split_fixed(ffns, fixed(","), n=2)[,1])
ffns.data$zvanje <- dict[[str_trim(str_split_fixed(ffns, fixed(","), 2)[,2])]]
ffns.data$drzava <- "Srbija"
ffns.data$univerzitet <- "Univerzitet u Novom Sadu"
ffns.data$fakultet <- "Filozofski fakultet"

fpn.txt <- read_file("raw/fpn.txt")
fpn <- str_split_fixed(fpn.txt, fixed("\n"), n = str_count(fpn.txt, "\n")) %>% str_trim
fpn.data <- data.frame(ime = str_split_fixed(fpn, fixed(","), n=2)[,1])
fpn.data$zvanje <- dict[[str_trim(str_split_fixed(fpn, fixed(","), 2)[,2])]]
fpn.data$drzava <- "Srbija"
fpn.data$univerzitet <- "Univerzitet u Beogradu"
fpn.data$fakultet <- "Fakultet političkih nauka"

ifdt.txt <- read_file("raw/ifdt.txt")
ifdt <- str_split_fixed(ifdt.txt, fixed("\n"), n = str_count(ifdt.txt, "\n")) %>% str_trim
ifdt.data <- data.frame(ime = str_split_fixed(ifdt, fixed(","), n=2)[,1])
ifdt.data$zvanje <- dict[[str_trim(str_split_fixed(ifdt, fixed(","), 2)[,2])]]
ifdt.data$drzava <- "Srbija"
ifdt.data$univerzitet <- "Univerzitet u Beogradu"
ifdt.data$fakultet <- "Institut za filozofiju i društvenu teoriju"

pravni.txt <- read_file("raw/pravni.txt")
pravni <- str_split_fixed(pravni.txt, fixed("\n"), n = str_count(pravni.txt, "\n")) %>% str_trim
pravni <- ifelse(str_detect(pravni, fixed(" dr ")), 
                 str_split_fixed(pravni, fixed(" dr "), 2)[,2], 
                 str_split_fixed(pravni, fixed(". "), 2)[,2])
pravni.data <- data.frame(ime = str_split_fixed(pravni, fixed(","), n=2)[,1])
pravni.data$zvanje <- dict[[str_trim(str_split_fixed(pravni, fixed(","), 2)[,2])]]
pravni.data$drzava <- "Srbija"
pravni.data$univerzitet <- "Univerzitet u Beogradu"
pravni.data$fakultet <- "Institut za filozofiju i društvenu teoriju"

socffbg.txt <- read_file("raw/soc-ff-bg.txt")
socffbg <- str_split_fixed(socffbg.txt, fixed("\n"), n = str_count(socffbg.txt, "\n")) %>% str_trim
socffbg.data <- data.frame(ime = str_split_fixed(socffbg, fixed(","), n=2)[,1])
socffbg.data$zvanje <- dict[[str_trim(str_split_fixed(socffbg, fixed(","), 2)[,2])]]
socffbg.data$drzava <- "Srbija"
socffbg.data$univerzitet <- "Univerzitet u Beogradu"
socffbg.data$fakultet <- "Filozofski fakultet"

tmf.txt <- read_file("raw/tmf.txt")
tmf <- str_split_fixed(str_split_fixed(tmf.txt, fixed("\n"), str_count(tmf.txt, "\n")), fixed("."), 2)[,2] %>% str_trim
tmf.data <- data.frame(ime = str_split_fixed(tmf, fixed(","), n=2)[,1])
tmf.data$zvanje <- dict[[str_trim(str_split_fixed(tmf, fixed(","), 2)[,2])]]
tmf.data$drzava <- "Srbija"
tmf.data$univerzitet <- "Univerzitet u Beogradu"
tmf.data$fakultet <- "Tehnološko-metalurški fakultet"

dijaspora.txt <- read_file("raw/dijaspora.txt")
dijaspora <- str_split_fixed(dijaspora.txt, fixed("\n"), n = str_count(dijaspora.txt, "\n")) %>% str_trim
dijaspora.data <- data.frame(ime = str_split_fixed(dijaspora, fixed(","), 4)[,1])
dijaspora.data$mesto <- str_trim(str_split_fixed(dijaspora, fixed(","), 4)[,2])
dij.zvanje <- str_trim(str_split_fixed(dijaspora, fixed(","), 4)[,3])
dijaspora.data$zvanje <- ifelse(!is.na(dict[[dij.zvanje]]), dict[[dij.zvanje]], dij.zvanje)
dijaspora.data$univerzitet <- str_trim(str_split_fixed(dijaspora, fixed(","), 4)[,4])
dijaspora.data$drzava <- "dijaspora"

up.txt <- read_file("raw/up-main.txt")
up.txt %<>% str_replace_all(fixed(";"), fixed(","))
up.txt %<>% str_replace_all(fixed(", Univerziteta"), fixed(", Univerzitet"))
up.txt %<>% str_replace_all(fixed("Univerziteta"), fixed(", Univerzitet"))
up.txt %<>% str_replace_all(fixed(", Fakulteta"), fixed(", Fakultet"))
up.txt %<>% str_replace_all(fixed("Fakulteta"), fixed(", Fakultet"))
up.txt %<>% str_replace_all(fixed(", Instituta"), fixed(", Institut"))
up.txt %<>% str_replace_all(fixed(", u penziji"), fixed("u penziji"))
up.txt %<>% str_replace_all(fixed("Instituta"), fixed(", Institut"))
up.txt %<>% str_replace_all(fixed("dj"), fixed("đ"))
up.txt %<>% str_replace_all(fixed("Dj"), fixed("Đ"))
up.txt %<>% str_replace_all(fixed("naucni"), fixed("naučni"))
up.txt %<>% str_replace_all(fixed("Akademik"), fixed(""))
up.txt %<>% str_replace_all(fixed(", Jagodina"), fixed(""))
up.txt %<>% str_replace_all(fixed(", Beograd"), fixed(""))
up.txt %<>% str_replace_all(fixed(", Novi Sad"), fixed(""))
up.txt %<>% str_replace_all(fixed(", Čačak"), fixed(""))
up.txt %<>% str_replace_all(fixed(", Niš"), fixed(""))
up.txt %<>% str_replace_all(fixed(", Kragujevac"), fixed(""))
up.txt %<>% str_replace_all(fixed(", Kruševac"), fixed(""))
up.txt %<>% str_replace_all(fixed("Tehnolosko-metalurski fakultet"), 
                            fixed("Tehnološko-metalurški fakultet"))
up.txt %<>% str_replace_all(fixed("Institut za hemiju, tehnologiju i metalurgiju"), 
                            fixed("Institut za hemiju tehnologiju i metalurgiju"))
up.txt %<>% str_replace_all(fixed("Matematički institut SANU"), fixed("Matematički institut SANU, SANU"))
up.txt %<>% str_replace_all(fixed("Institut za onkologiju i radiologiju Srbije"), 
                            fixed("Institut za onkologiju i radiologiju Srbije, Institut za onkologiju i radiologiju Srbije"))
up.txt %<>% str_replace_all(fixed("Institut za onkologiju i radiologiju Beograd"), 
                            fixed("Institut za onkologiju i radiologiju Srbije, Institut za onkologiju i radiologiju Srbije"))
up.txt %<>% str_replace_all(fixed("Visoka tehnološka škola strukovnih studija u Aranđelovcu"), 
                            fixed("Visoka tehnološka škola strukovnih studija u Aranđelovcu, Visoka tehnološka škola strukovnih studija u Aranđelovcu"))
up.txt %<>% str_replace_all(fixed("Nova akademija umetnosti"), 
                            fixed("Nova akademija umetnosti, Nova akademija umetnosti"))
up.txt %<>% str_replace_all(fixed("redovni član SANU"), 
                            fixed("redovni član SANU, SANU, SANU"))
up.txt %<>% str_replace_all(fixed("dopisni član SANU"), 
                            fixed("dopisni član SANU, SANU, SANU"))
up.txt %<>% str_replace_all(fixed("Balkanološki institut SANU"), 
                            fixed("Balkanološki institut SANU, SANU"))
up.txt %<>% str_replace_all(fixed("Institut tehničkih nauka SANU"), 
                            fixed("Institut tehničkih nauka SANU, SANU"))
up.txt %<>% str_replace_all(fixed("Muzikološki institut SANU"), 
                            fixed("Muzikološki institut SANU, SANU"))
up.txt %<>% str_replace_all(fixed("Etnografski institut SANU"), 
                            fixed("Etnografski institut SANU, SANU"))
up.txt %<>% str_replace_all(fixed("Institut za uporedno pravo"), 
                            fixed("Institut za uporedno pravo, Institut za uporedno pravo"))
up.txt %<>% str_replace_all(fixed("Visoka zdravstveno-sanitarna škola strukovnih studija"), 
                            fixed("Visoka zdravstveno-sanitarna škola strukovnih studija, Visoka zdravstveno-sanitarna škola strukovnih studija"))
up.txt %<>% str_replace_all(fixed("Visoka tekstilna strukovna škola za dizajn, tehnologiju i menadžment"), 
                            fixed("Visoka tekstilna strukovna škola za dizajn tehnologiju i menadžment, Visoka tekstilna strukovna škola za dizajn tehnologiju i menadžment"))
up.txt %<>% str_replace_all(fixed("Fakultet zdravstvenih, pravnih i poslovnih studija"), 
                            fixed("Fakultet zdravstvenih pravnih i poslovnih studija"))
up.txt %<>% str_replace_all(fixed("Institut za proučavanje lekovitog bilja “Dr Josif Pančić”"), 
                            fixed("Institut za proučavanje lekovitog bilja “Dr Josif Pančić”, Institut za proučavanje lekovitog bilja “Dr Josif Pančić”"))
up.txt %<>% str_replace_all(fixed("Institut za krmno bilje"), 
                            fixed("Institut za krmno bilje, Institut za krmno bilje"))
up.txt %<>% str_replace_all(fixed("Visoka građevinsko-tehnička škola"), 
                            fixed("Visoka građevinsko-tehnička škola, Visoka građevinsko-tehnička škola"))
up.txt %<>% str_replace_all(fixed("Institut društvenih nauka"), 
                            fixed("Institut društvenih nauka, Institut društvenih nauka"))
up.txt %<>% str_replace_all(fixed("Institut za kriminološka i sociološka istraživanja"), 
                            fixed("Institut za kriminološka i sociološka istraživanja, Institut za kriminološka i sociološka istraživanja"))
up.txt %<>% str_replace_all(fixed("Visoka škola strukovnih studija za informacione i komunikacione tehnologije"), 
                            fixed("Visoka škola strukovnih studija za informacione i komunikacione tehnologije, Visoka škola strukovnih studija za informacione i komunikacione tehnologije"))
up.txt %<>% str_replace_all(fixed("Republički zavod za zaštitu spomenika kulture Beograd"), 
                            fixed("Republički zavod za zaštitu spomenika kulture, Republički zavod za zaštitu spomenika kulture"))
up.txt %<>% str_replace_all(fixed("Naučni institut za veterinarstvo “Novi Sad”"), 
                            fixed("Naučni institut za veterinarstvo “Novi Sad”, Naučni institut za veterinarstvo “Novi Sad”"))
up.txt %<>% str_replace_all(fixed("Kriminalističko-policijski univerzitet"), 
                            fixed("Kriminalističko-policijski univerzitet, Kriminalističko-policijski univerzitet"))
up.txt %<>% str_replace_all(fixed("Institut za virusologiju, vakcine i serume “Torlak”"), 
                            fixed("Institut za virusologiju vakcine i serume “Torlak”, Institut za virusologiju vakcine i serume “Torlak”"))
up.txt %<>% str_replace_all(fixed("Institut za književnost i umetnost"), 
                            fixed("Institut za književnost i umetnost, Institut za književnost i umetnost"))
up.txt %<>% str_replace_all(fixed("Astronomska opservatorija"), 
                            fixed("Astronomska opservatorija, Astronomska opservatorija"))
up.txt %<>% str_replace_all(fixed("Arheološki institut"), 
                            fixed("Arheološki institut, Arheološki institut"))
up.txt %<>% str_replace_all(fixed("Naučno društvo Srbije"), 
                            fixed("Naučno društvo Srbije, Naučno društvo Srbije"))
up.txt %<>% str_replace_all(fixed("Institut za opštu i fizičku hemiju"), 
                            fixed("Institut za opštu i fizičku hemiju, Institut za opštu i fizičku hemiju"))
up.txt %<>% str_replace_all(fixed("Lola institut"), 
                            fixed("Lola institut, Lola institut"))
up.txt %<>% str_replace_all(fixed("Institut za ekonomiku poljoprivrede"), 
                            fixed("Institut za ekonomiku poljoprivrede, Institut za ekonomiku poljoprivrede"))
up.txt %<>% str_replace_all(fixed("Visoka poslovna škola strukovnih studija"), 
                            fixed("Visoka poslovna škola strukovnih studija, Visoka poslovna škola strukovnih studija"))
up.txt %<>% str_replace_all(fixed("Institut za evropske studije"), 
                            fixed("Institut za evropske studije, Institut za evropske studije"))
up.txt %<>% str_replace_all(fixed("Institut za zaštitu bilja i životnu sredinu"), 
                            fixed("Institut za zaštitu bilja i životnu sredinu, Institut za zaštitu bilja i životnu sredinu"))
up.txt %<>% str_replace_all(fixed("Institut za stočarstvo"), 
                            fixed("Institut za stočarstvo, Institut za stočarstvo"))
up.txt %<>% str_replace_all(fixed("Institut za srpski jezik SANU"), 
                            fixed("Institut za srpski jezik SANU, SANU"))
up.txt %<>% str_replace_all(fixed("Institut za arhitekturu i urbanizam Srbije"), 
                            fixed("Institut za arhitekturu i urbanizam Srbije, Institut za arhitekturu i urbanizam Srbije"))
up.txt %<>% str_replace_all(fixed("Institut za ispitivanje materijala"), 
                            fixed("Institut za ispitivanje materijala, Institut za ispitivanje materijala"))
up.txt %<>% str_replace_all(fixed("Institut za tehnologiju nuklearnih i drugih mineralnih sirovina"), 
                            fixed("Institut za tehnologiju nuklearnih i drugih mineralnih sirovina, Institut za tehnologiju nuklearnih i drugih mineralnih sirovina"))
up.txt %<>% str_replace_all(fixed("Institut za voćarstvo"), 
                            fixed("Institut za voćarstvo, Institut za voćarstvo"))
up.txt %<>% str_replace_all(fixed("Istorijski institut"), 
                            fixed("Istorijski institut, Istorijski institut"))
up.txt %<>% str_replace_all(fixed("Visoka tehnička škola strukovnih studija Požarevac"), 
                            fixed("Visoka tehnička škola strukovnih studija Požarevac, Visoka tehnička škola strukovnih studija Požarevac"))
up.txt %<>% str_replace_all(fixed("Institut za zemljište"), 
                            fixed("Institut za zemljište, Institut za zemljište"))
up.txt %<>% str_replace_all(fixed("Institut za ratarstvo i povrtarstvo"), 
                            fixed("Institut za ratarstvo i povrtarstvo, Institut za ratarstvo i povrtarstvo"))
up.txt %<>% str_replace_all(fixed("Naučni institut za veterinarstvo Srbije"), 
                            fixed("Naučni institut za veterinarstvo Srbije, Naučni institut za veterinarstvo Srbije"))
up.txt %<>% str_replace_all(fixed("Institut za savremenu istoriju"), 
                            fixed("Institut za savremenu istoriju, Institut za savremenu istoriju"))
up.txt %<>% str_replace_all(fixed("Institut za strategijska istraživanja"), 
                            fixed("Institut za strategijska istraživanja, Institut za strategijska istraživanja"))
up.txt %<>% str_replace_all(fixed("Institut za rudarstvo i metalurgiju"), 
                            fixed("Institut za rudarstvo i metalurgiju, Institut za rudarstvo i metalurgiju"))
up.txt %<>% str_replace_all(fixed("Institut za noviju istoriju Srbije"), 
                            fixed("Institut za noviju istoriju Srbije, Institut za noviju istoriju Srbije"))
up.txt %<>% str_replace_all(fixed("Kriminalističko–policijski univerzitet"), 
                            fixed("Kriminalističko–policijski univerzitet, Kriminalističko–policijski univerzitet"))
up.txt %<>% str_replace_all(fixed("Vojnotehnički institut"), 
                            fixed("Vojnotehnički institut, Vojnotehnički institut"))
up.txt %<>% str_replace_all(fixed("fakultet FEFA"), fixed("Fakultet FEFA"))
up.txt %<>% str_replace_all(fixed("Visoka škola strukovnih studija za obrazovanje vaspitača"), 
                            fixed("Visoka škola strukovnih studija za obrazovanje vaspitača, Visoka škola strukovnih studija za obrazovanje vaspitača"))
up.txt %<>% str_replace_all(fixed("Akademija poslovnih strukovnih studija"),
                            fixed("Akademija poslovnih strukovnih studija, Akademija poslovnih strukovnih studija"))
up.txt %<>% str_replace_all(fixed("Visoka škola strukovnih studija za vaspitače"),
                            fixed("Visoka škola strukovnih studija za vaspitače, Visoka škola strukovnih studija za vaspitače"))
up.txt %<>% str_replace_all(fixed("Institut za naučna istraživanja PKB Agroekonomik"),
                            fixed("Institut za naučna istraživanja PKB Agroekonomik, Institut za naučna istraživanja PKB Agroekonomik"))
brpotpisnika <- str_count(up.txt, regex("^[0-9]+.*$", multiline=T))+1 #fun fact: fali rbr 727
up <- str_split_fixed(up.txt, "[0-9]+\\.\\s*", brpotpisnika)[-1] %>% str_replace_all(fixed("\n"), fixed(" ")) %>% str_trim
up.split <- str_split_fixed(up, fixed(","), 4)
up.data <- data.frame(ime=up.split[,1] %>% str_trim, zvanje=dict[[up.split[,2] %>% str_trim]], fakultet=up.split[,3] %>% str_trim, univerzitet=up.split[,4] %>% str_trim)
up.data$drzava <- "Srbija"

data <- bind_rows(up.data, dijaspora.data, ffbg.data, ffns.data, fpn.data, ifdt.data, pravni.data, socffbg.data, tmf.data)
data <- data[!duplicated(data), ]
write.csv(data, "dataset.csv")
#ggplot(up.data %>% filter(fakultet %in% (up.data %>% group_by(fakultet) %>% summarise(no_rows=length(fakultet)) %>% filter(no_rows > 29))$fakultet), aes(x=zvanje,fill=zvanje)) + geom_bar() + facet_grid(cols = vars(fakultet))
#data %>% group_by(fakultet, univerzitet) %>% summarise(no_rows=length(fakultet)) %>% filter(no_rows > 29) %>% arrange(-no_rows)