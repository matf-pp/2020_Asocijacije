# asocijacije

Program `asocijacije` je implementacija istoimene igre iz kviza *Slagalica* u Haskell programskom jeziku. Ovaj program je napisan za potrebe kursa [Programski jezici](http://www.programskijezici.matf.bg.ac.rs/) na [Matematičkom fakultetu](http://www.matf.bg.ac.rs/) u Beogradu.

## Instalacija

Za kompajlicaju programa je potrebno standardno razvojno okrženje za Haskell (`ghc` + `cabal`). Program se kompajlira tako što se unutar glavnog foldera pokrene komanda

```
cabal sandbox init
```

a zatim i 

```
cabal install -j
```

### Biblioteke

Programa koristi sledeće biblioteke:

+ base     
+ filepath
+ gi-cairo
+ gi-gdk
+ gi-gdkpixbuf
+ gi-gio
+ gi-gtk
+ haskell-gi-base
+ text 

Ako program kompajlirate na način koji je gore opisan, tada će se `cabal` sam pobrinuti za preuzimanje i instalaciju svih potrebnih biblioteka. Imajte na umu da taj proces može potrajati preko sat vremena. 

## Upotreba

Pogledati RTS1 radnim danom u 19h.

## Autori

Autori programa su Filip Filpović i Nikola Ubavić, studenti Matematičkog fakulteta u Beogradu.

## Licenca

Program je licenciran MIT licencom. Za više informacija pogledajte fajl [LICENSE](LICENSE).
