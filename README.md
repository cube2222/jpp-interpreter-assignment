Język realizuje zestaw featurów na 25 punktów:
  01 (dwa typy)
  02 (arytmetyka, porównania)
  03 (if)
  04 (funkcje wieloargumentowe, rekurencja)
  05 (funkcje anonimowe i wyższego rzędu, częściowa aplikacja)
  06 (obsługa błędów wykonania)
  07 (z pattern matchingiem)
  09 (lukier)
  10 (listy dowolnego typu, zagnieżdżone i listy funkcji)
  12 (statyczne wiązanie identyfikatorów)
  13 (statyczne typowanie)

Testy są automatyczne i mozna je uruchomić poprzez make test.
Testy mozna wyświetlić w pliku test/Spec.hs.

komenda test też jest nastawiona na maszynę students, na innym urządzeniu można użyć:
```
cabal test -j1
```
i spojrzeć na plik z logiem w celu zobaczenia efektów.

make budujący binarke wyciąga ją z katalogu dist. Nazwy katalogów są dostosowane do wersji cabala i systemu na students. Na innym urządzeniu może być potrzeba ręcznie wyciągnąć tę binarkę z katalogu podobnego do dist-newstyle/build/x86_64-${system}/ghc-${version}/Interpreter-0.1.0.0/x/Interpreter-exe/build/Interpreter-exe
