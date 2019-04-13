# Univerzitetska podrška protestima 2018/19
### Clean & tidy dataset

Potpisi podrške protestima od strane univerzitetske zajednice na jednom mestu. Prikupljeno sa raznih izvora, kao što su [zvanični sajt](https://univerzitetskapodrska.home.blog/), ali i saopštenja raznih fakulteta. Fajlovi se nalaze u `raw/` folderu — to nisu nužno izvorni fajlovi (posebno u slučaju najobimnijeg spiska), već su oni prilagođeni tako da se otklone razne tipografske greške, nekonzistentan format potpisa i naziva institucija i sl. Oni koji su potpisani sa dva zvanja uglavnom su razdvojeni na dva unosa (to su retki slučajevi). Sva zvanja su u muškom rodu, radi lakše obrade.

Sređen dataset je dostupan u fajlu `dataset.csv`. `clean.R` učitava podatke iz fajlova, parsira ih i vrši neke korekcije. `zvanja.R` sadrži mapu zvanja. Greške su moguće — štaviše, očekivane. Osećajte se slobodno da uputite pull request.

## Licenca
MPL (kod) & ODbL (podaci); videti istoimene fajlove za tekst licenci.
