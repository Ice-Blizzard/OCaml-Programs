(* Rafał Szulc Uniwersytet Warszawski *)

(* 
Punkt na płaszczyźnie euklidesowej 
reprezentowany przed dwie liczby rzeczywiste (a,b) 
*)
type point = float * float

(* 
Poskładana kartka: 
ile razy kartkę przebije szpilka wbita w danym punkcie
*)
type kartka = point -> int

(* 
w geometrii obliczeniowej na floatach są problemy z dokladnoscia liczb
bo np w ocaml 
# -3. *. (5. -. 3.8);;
- : float = -3.60000000000000053
stąd deklaruje epsilon by uwzględniać te końcówki by nie psuły wyników
*)
let epsilon = 1e-15

(* wyjątek na wypadek gdy ktoś wprowadzi prostokąt który nie spełnia założeń *)
exception TO_NIE_PROSTOKAT

(*
val prostokat : point -> point -> kartka
[prostokat p1 p2] zwraca kartkę, reprezentującą domknięty prostokąt
o bokach równoległych do osi układu współrzędnych i lewym dolnym rogu [p1]
a prawym górnym [p2]. Punkt [p1] musi więc być nieostro na lewo i w dół
od punktu [p2]. Gdy w kartkę tę wbije się szpilkę wewnątrz
(lub na krawędziach) prostokąta, kartka zostanie przebita 1 raz,
w pozostałych przypadkach 0 razy
*)
let prostokat p1 p2 x =
  if fst p1 > fst p2 || snd p1 > snd p2
  then raise (TO_NIE_PROSTOKAT)
  else
    if fst x < fst p1 || fst x > fst p2 || snd x < snd p1 || snd x > snd p2
    then 0
    else 1

(* wyjątek na wypadek gdy ktoś wprowadzi kółko o ujemnym promieniu *)
exception TO_NIE_KOLKO

(*
val kolko : point -> float -> kartka
[kolko p r] zwraca kartkę, reprezentującą kółko domknięte o środku
w punkcie [p] i promieniu [r]
*)
let kolko p r x =
  if r < 0.
  then raise (TO_NIE_KOLKO) 
  else
    let x1 = fst x -. fst p in
    let x2 = snd x -. snd p in
    if sqrt(x1 *. x1 +. x2 *. x2) > r
    then 0
    else 1

(* wyjątek gdy punkty przy zloz są sobie równe *)
exception ZLE_ZLOZENIE

(*
val zloz : point -> point -> kartka -> kartka
[zloz p1 p2 k] składa kartkę [k] wzdłuż prostej przechodzącej przez
punkty [p1] i [p2] (muszą to być różne punkty). Papier jest składany
w ten sposób, że z prawej strony prostej (patrząc w kierunku od [p1] do [p2])
jest przekładany na lewą. Wynikiem funkcji jest złożona kartka. Jej
przebicie po prawej stronie prostej powinno więc zwrócić 0.
Przebicie dokładnie na prostej powinno zwrócić tyle samo,
co przebicie kartki przed złożeniem. Po stronie lewej -
tyle co przed złożeniem plus przebicie rozłożonej kartki w punkcie,
który nałożył się na punkt przebicia.
Korzystam z iloczynu wektorowego 
i wyznacznika tak jak na pierwszych ćwiczeniach
z wpf z Doktorem Jackiem Chrząszczem w podobnym problemie.
A także wzoru na równanie prostej i odbicie punktu względem prostej.
ay+bx+c - prosta przechodząca przez p1 i p2
(e,f) - wzpółrzędne odbicia p wzgl prostej ay+bx+c
*)
let zloz p1 p2 funkcja p =
  if fst p1 = fst p2 && snd p1 = snd p2
  then raise (ZLE_ZLOZENIE)
  else
    let wyznacznik1 = (fst p1 -. fst p) *. (snd p2 -. snd p) in
    let wyznacznik2 = (snd p1 -. snd p) *. (fst p2 -. fst p) in
    let wyznacznik = wyznacznik1 -. wyznacznik2 in
    if wyznacznik < epsilon && wyznacznik > (-1. *. epsilon) 
    then funkcja p
    else
      if wyznacznik <= (-1. *. epsilon)
      then 0
      else 
        let a = fst p2 -. fst p1 in
        let b = snd p1 -. snd p2 in
        let c0 = snd p1 *. fst p1 -. snd p1 *. fst p2 +. snd p2 *. fst p1 in
        let c = c0 -. snd p1 *. fst p1 in
        let e0 = fst p *. (a *. a -. b *. b) -. 2. *. b *. (a *. snd p +. c) in
        let e = e0 /. (a *. a +. b *. b) in
        let f0 = snd p *. (b *. b -. a *. a) -. 2. *. a *. (b *. fst p +. c) in
        let f = f0 /. (a *. a +. b *. b) in
        funkcja p + funkcja (e,f)

(* bede korzystac z modułu List, a konkretnie z fold_left *)
open List

(*
val skladaj : (point * point) list -> kartka -> kartka
[skladaj [(p1_1,p2_1);...;(p1_n,p2_n)] k = 
zloz p1_n p2_n (zloz ... (zloz p1_1 p2_1 k)...)]
czyli wynikiem jest złożenie kartki [k] kolejno wzdłuż wszystkich prostych
z listy
*)
let skladaj lista k = 
  fold_left (fun akumulator (p1, p2) -> zloz p1 p2 akumulator) k lista;;


