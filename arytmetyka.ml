(* autor kodu - Rafał Szulc gr nr 3 *)

(* 
   wartosc zawsze bedzie:
   1. przypadek: pusta
   2. przypadek: ciaglym przedzialem 
   3. przypadek: suma 2 rozlacznych ciaglych przedzialow
   dlatego interpretuje ja jako 4 liczby takie ze: 
   l1 <= l2 < p1 <= p2 
   gdzie li pi to liczby rzeczywiste, +inf lub -inf 
   jesli przedzial jest ciaglym pojedynczy to p1 = p2 = nan
   jesli przedzialu nie ma to l1 = l2 = p1 = p2 = nan
   (l1,l2) to pierwszy przedzial
   (p1,p2) to drugi przedzial
   widac ze jesli wartosc ma dwa przedzialy to jej min to -inf a max to inf 
*)

type wartosc = 
{
l1 : float ; 
l2 : float ; 
p1 : float ; 
p2 : float
}

(* nazywam nieskonczonosc i -nieskoczonosc w krotszy sposob *)
let inf = infinity
let ninf = neg_infinity

(* konstruktory *)

(* 
   x z dokladnoscia do p% to przedzial z definicji 
   dla ujemnych bedzie on przeciwny co widac matematycznie
*)
let wartosc_dokladnosc x p = 
{
l1 = min (x -. x *. (p /. 100.)) (x +. x *. (p /. 100.)) ; 
l2 = max (x +. x *. (p /. 100.)) (x -. x *. (p /. 100.)) ; 
p1 = nan ; 
p2 = nan
}

(* widac ze (x+y)/2 +- (y-x)/2 to przedzial od x do y *)
let wartosc_od_do x y = 
{
l1 = x ; 
l2 = y ; 
p1 = nan ; 
p2 = nan
}

(* przedzial zawierajacy tylko x *)
let wartosc_dokladna x = 
{
l1 = x ; 
l2 = x ; 
p1 = nan ; 
p2 = nan
}

(* koniec konstruktorów *)

(* selektory *)

(* 
   funkcja in_wartosc x y sprawdza 
   czy y (liczba rzeczywista lub +inf lub -inf) nalezy do wartosci x 
*)
let in_wartosc x y =
  (* jesli wartosc x to zbior pusty to y do niego nie nalezy *)
  if ((classify_float x.l1) = FP_nan)  then false else
  (* jesli y < x.l1 skad wynika y < x.l1 <= x.l2 to y nie nalezy do x *)   
  if (y < x.l1) then false else
  (* jesli x.l1 <= y <= x.l2 to oczywiscie y nalexy do x *)
  if y <= x.l2 then true else
  (* jesli y nie nalezy do pierwszego przedzialu a nie ma drugiego to false *) 
  if ((classify_float x.p1) = FP_nan)  then false else
  (* jesli drugi przedzial jest a y jest miedzy 1. i 2. przedzialem to false *)
  if y < x.p1 then false else
  (* jesli y jest w 2 przedziale to true *)
  if y <= x.p2 then true else 
  (* jesli y jest wiekjsze niz najwieksza liczba z x to oczywiscie false *)
  false

(* minimalna liczba w x *)
let min_wartosc x = 
  (* jesli x jest pusty to nie ma minimum *)
  if ((classify_float x.l1) = FP_nan)  then nan else 
  (* jesli nie jest pusty to jego minimum to jego minimum po prostu *)
  x.l1

(* maksymalna liczba w x *)
let max_wartosc x = 
  (* w pustym przedziale nie ma maksimum *)
  if ((classify_float x.l1) = FP_nan)  then nan else
  (* jesli p1 to nan to wezmie x.l2 inaczej x.p2 *)
  if ((classify_float x.p1) = FP_nan) then x.l2 else x.p2

(* 
   tworze funkckje wyliczajaca srednia z minimum i maksimum w x
   lub nan jesli ta srednia jest symbolem nieoznaczonym
*)
let sr_wartosc x =
  (* srednia przedzialu -inf,inf to nan *)
  if ((classify_float x.p1) = FP_nan && 
  x.l1 = ninf && 
  x.l2 = inf) then nan else
  (* srednia przedzialu (-inf,liczba1) + (liczba2,inf) to nan *)
  if ((classify_float x.p1) <> FP_nan && 
  x.l1 = ninf && 
  x.p2 = inf) then nan else
  (* srednia przedzialu (liczba1,liczba2) + (liczba3,inf) to inf *)
  if ((classify_float x.p1) <> FP_nan && 
  x.l1 <> ninf && 
  x.p2 = inf) then inf else
  (* srednia przedzialu (liczba1,inf) to inf *)
  if ((classify_float x.p1) = FP_nan && 
  x.l1 <> ninf && 
  x.l2 = inf) then inf else
  (* 
     maks w x nie moze byc inf bo to juz rozwazylismy wiec jesli min w x jest
     rowne -inf to srednia z definicji jest -inf
  *)
  if x.l1 = ninf then ninf else
  (*
     skoro przedzial istnieje i sklada sie z liczb rzeczywistych to liczymy
     srednia normalnie wersja dla 2 przedzialow
  *)
  if ((classify_float x.p1) <> FP_nan) then ((x.l1 +. x.p2) /. 2.) else
  (* wersja dla 1 przedzialu *)
  ((x.l1 +. x.l2) /. 2.)

(* koniec selektorow *)

(* kilka przydatnych funkcji *)

(* funkcja wyliczajaca max ale taka ze max (2,nan) = 2 *)
let max2 a b =
  if ((classify_float a) = FP_nan) then b else
  if ((classify_float b) = FP_nan) then a else
  max a b

(* funkcja wyliczajaca min ale taka ze max (2,nan) = 2 *)
let min2 a b =
  if ((classify_float a) = FP_nan) then b else
  if ((classify_float b) = FP_nan) then a else
  min a b

(* tworze funkcje wyliczajaca maksimum z 4 liczb *)
let max4 i j k l = max2 (max2 i j) (max2 k l)

(* tworze funckje wyliczajaca minimum z 4 liczb *)
let min4 i j k l = min2 (min2 i j ) (min2 k l)

(* tworze funckje wyliczaja minimalna liczbe rzeczywista jesli istnieje *)
let minr2 a b =
  if a = ninf then b else
  if b = ninf then a else
  min2 a b

(* tworze funckje wyliczaja maksymalna liczbe rzeczywista jesli istnieje *)
let maxr2 a b =
  if a = inf then b else
  if b = inf then a else
  max2 a b

(* tworze funckje wyliczaja minimalna liczbe rzeczywista jesli istnieje z 4 *)
let minr4 a b c d = minr2 (minr2 a b) (minr2 c d)

(* tworze funckje wyliczaja minimalna liczbe rzeczywista jesli istnieje z 4 *)
let maxr4 a b c d = maxr2 (maxr2 a b) (maxr2 c d)

(* funckja ktora przypisuje liczbie jej liczbe przeciwna *)
let p x =
  (-1.) *. x

(* 
   tworze funkcje ktora z wartosci x da mi wartosc y zawierajaca
   liczby przeciwne do tych z wartosci x
   widac ze jesli x : (liczba1,liczba2) to y : (-liczba2,-liczba1)
   a jesli x: (liczba1,liczba2) + (liczba3,liczba4) to
           y: (-liczba4,-liczba3) + (-liczba2,-liczba1)
*)
let przeciw x =
  (* jesli x sklada sie z dwoch przedzialow *)
  if ((classify_float x.p1) <> FP_nan) 
  then {l1 = (p x.p2) ; l2 = (p x.p1) ; p1 = (p x.l2) ; p2 = (p x.l1)} 
  else
    (* jesli x sklada sie z jednego przedzialu *)
    {l1 = (p x.l2) ; l2 = (p x.l1) ; p1 = nan ; p2 = nan}

(* funckja ktora przypisuje liczbie jej liczbe odwrotna *)
let o x =
  1. /. x

(* 
  funckja ktora z wartosci x da mi wartosc y zawierajaca 
  liczby przeciwne do tych z wartosci x
*)
let odwro x =
  (* przedzial pusty nie ma przedzialu odwrotnego *)
  if ((classify_float x.l1) = FP_nan) 
  then {l1 = nan ; l2 = nan ; p1 = nan ; p2 = nan} 
  else
    (* przedzial z samym zerem nie ma przedzialu odwrotego *)
    if (((classify_float x.p1) = FP_nan) && 
    x.l1 = 0. && x.l2 = 0.)
    then {l1 = nan ; l2 = nan ; p1 = nan ; p2 = nan} 
    else
      (* jesli przedzial/y nie zawiera zera *)
      if (in_wartosc x 0. = false)
      then
        (* wersja z jednym przedzialem *)
        if ((classify_float x.p1) = FP_nan)
        then {l1 = (o x.l2) ; l2 = (o x.l1) ; p1 = nan ; p2 = nan}
        else
          (* wersja z dwoma przedzialami *)
          {l1 = (o x.l2) ; l2 = (o x.l1) ; p1 = (o x.p2) ; p2 = (o x.p1)}        
      else
        (* wersja ze przedzialy sa dwa i zawieraja 0. *)
        if ((classify_float x.p1) <> FP_nan)
        then
          (* przedzial (-inf,0.) + (dodatnia,inf) *)
          if x.l2 = 0.
          then {l1 = ninf ; l2 = (o x.l1) ; p1 = nan ; p2 = nan}
          else
            (* przedzial (-inf,ujemna) + (0.,inf) *)
            if x.p1 = 0.
            then {l1 = (o x.p1) ; l2 = inf ; p1 = nan ; p2 = nan}
            else
              (* przedzial (-inf,dodatnia) + (dodatnia,inf) *)
              if x.l2 > 0.
              then {l1 = ninf ; l2 = (o x.p1) ; p1 = (o x.l2) ; p2 = inf}
              else
                (* przedzial (-inf,ujemna) + (ujemna,inf) *)
                {l1 = ninf ; l2 = (o x.p1) ; p1 = (o x.l2) ; p2 = inf}
        else
          (* wersja jest jeden przedzial i zawiera 0. *)
          (* przedzial zawiera -inf *) 
          if (x.l1 = ninf)
          then
            (* przedzial (-inf,inf) *)
            if (x.l2 = inf)
            then {l1 = ninf ; l2 = inf ; p1 = nan ; p2 = nan}
            else
              (* przedzial (-inf,0.) *)
              if (x.l2 = 0.)
              then {l1 = ninf ; l2 = 0. ; p1 = nan ; p2 = nan}
              else
                (* przedzial (-inf,dodatnia) *)
                {l1 = ninf ; l2 = 0. ; p1 = (o x.l2) ; p2 = inf}
          else
            (* przedzial nie zawiera -inf *)
            (* przedzial zawiera inf *)
            if (x.l2 = inf)
            then
              (* przedzial (0.,inf) *)
              if (x.l1 = 0.)
              then {l1 = 0. ; l2 = inf ; p1 = nan ; p2 = nan}
              else
                (* przedzial (ujemna,inf) *)
                {l1 = ninf ; l2 = (o x.l1) ; p1 = 0. ; p2 = inf}
            else  
              (* przedzial nie zawiera nieskonczonosci *)
              (* przedzial (ujemna,0.) *)
              if x.l2 = 0.
              then {l1 = ninf ; l2 = (o x.l1) ; p1 = nan ; p2 = nan}
              else
                (* przedzial (0.,dodatnia) *)
                if x.l1 = 0.
                then {l1 = (o x.l2) ; l2 = inf ; p1 = nan ; p2 = nan}
                else
                  (* przedzial (ujemna,dodatnia *)
                  {l1 = ninf ; l2 = (o x.l1) ; p1 = (o x.l2) ; p2 = inf}

(* 
   funkcja ktora oblicza sume zbioru 4 wartosci
   ktore maja taka wlasnosc ze ich suma jest wartoscia
   jak pozniej widac bedziemy ja wywolywac tylko wtedy gdy wartosci a b c d
   beda wartosciami z tylko jednym przedzialem 
   bedzie to wartosc z 1 przedzialem lub z dwoma - jesli ma dwa to oczywiscie
   jesli w postaci (-inf,liczba1) + (liczba2,inf)
*)
let suma a b c d =
  (*jesli wynik to przedzial ciagly -inf,inf *)
  if (a.l1 = ninf &&
      a.l2 = inf)
  then {l1 = ninf ; l2 = inf ; p1 = nan ; p2 = nan}
  else
    if (b.l1 = ninf &&
        b.l2 = inf)
    then {l1 = ninf ; l2 = inf ; p1 = nan ; p2 = nan}
    else
      if (c.l1 = ninf &&
          c.l2 = inf)
      then {l1 = ninf ; l2 = inf ; p1 = nan ; p2 = nan}
      else
        if (d.l1 = ninf &&
            d.l2 = inf)
        then {l1 = ninf ; l2 = inf ; p1 = nan ; p2 = nan}
        else
          if ( (min4 a.l1 b.l1 c.l1 d.l1 = ninf) &&
              (max4 a.l2 b.l2 c.l2 d.l2 = inf) &&
              ((minr4 a.l1 b.l1 c.l1 d.l1) <= (maxr4 a.l2 b.l2 c.l2 d.l2)))
          then {l1 = ninf ; l2 = inf ; p1 = nan ; p2 = nan}
          else
            (* jesli to (-inf,liczba1) + (liczba2, inf) *)
            if ( (min4 a.l1 b.l1 c.l1 d.l1 = ninf) &&
              (max4 a.l2 b.l2 c.l2 d.l2 = inf) &&
              ((minr4 a.l1 b.l1 c.l1 d.l1) > (maxr4 a.l2 b.l2 c.l2 d.l2)))
            then {
                  l1 = ninf ; 
                  l2 = (maxr4 a.l2 b.l2 c.l2 d.l2) ; 
                  p1 = (minr4 a.l1 b.l1 c.l1 d.l1) ; 
                  p2 = inf
                 }
            else
              (* przedzial ciagly *)
              {
               l1 = (min4 a.l1 b.l1 c.l1 d.l1) ; 
               l2 = (max4 a.l2 b.l2 c.l2 d.l2) ; 
               p1 = nan ; 
               p2 = nan
              }
      
(* funckja plus ale rekurencyjna *)
(* 
  zauwazam ze plus (a,b) + (c,d) ; (e,f) + (g,h) to suma zbiorow plusowania
  kazdego przedzialu z kazdym wiec wystarczy wiedziec jak plusowac wartosci
  z jednym przedzialem
*)
let rec plusr a b =
  (* jak a lub b jest nan to plus tez jest nan *)
  if (((classify_float a.l1) = FP_nan) || ((classify_float b.l1) = FP_nan))
  then {l1 = nan ; l2 = nan ; p1 = nan ; p2 = nan} 
  else
    (* rozbijanie przedzialow *)
    if (((classify_float a.p1) <> FP_nan) || ((classify_float b.p1) <> FP_nan))
    then suma 
      (plusr ({l1 = a.l1 ; l2 = a.l2 ; p1 = nan ; p2 = nan}) 
             ({l1 = b.l1 ; l2 = b.l2 ; p1 = nan ; p2 = nan})) 
      (plusr ({l1 = a.l1 ; l2 = a.l2 ; p1 = nan ; p2 = nan}) 
             ({l1 = b.p1 ; l2 = b.p2 ; p1 = nan ; p2 = nan})) 
      (plusr ({l1 = a.p1 ; l2 = a.p2 ; p1 = nan ; p2 = nan}) 
             ({l1 = b.p1 ; l2 = b.p2 ; p1 = nan ; p2 = nan})) 
      (plusr ({l1 = a.p1 ; l2 = a.p2 ; p1 = nan ; p2 = nan}) 
             ({l1 = b.l1 ; l2 = b.l2 ; p1 = nan ; p2 = nan})) 
    else
      (* dodawanie dwoch pojedynczych przedzialow *)
      {l1 = a.l1 +. b.l1 ; l2 = a.l2 +. b.l2 ; p1 = nan ; p2 = nan}
  
(* funckja razy ale rekurencyjna *)
(* 
  zauwazam ze razy (a,b) + (c,d) ; (e,f) + (g,h) to suma zbiorow mnozenia
  kazdego przedzialu z kazdym wiec wystarczy wiedziec jak mnozyc wartosci
  z jednym przedzialem
*)
let rec razyr a b =
  (* jak a lub b jest nan to razy tez jest nan *)
  if (((classify_float a.l1) = FP_nan) || 
      ((classify_float b.l1) = FP_nan))
  then {l1 = nan ; l2 = nan ; p1 = nan ; p2 = nan} 
  else
    (* mnozenie przez (0.,.0) *)
    if (a.l1 = 0. && a.l2 = 0. && ((classify_float a.p1) = FP_nan))
    then {l1 = 0. ; l2 = 0. ; p1 = nan ; p2 = nan}
    else
      if (b.l1 = 0. && b.l2 = 0. && ((classify_float b.p1) = FP_nan))
      then {l1 = 0. ; l2 = 0. ; p1 = nan ; p2 = nan}
      else
        (* rozbijanie przedzialow *)
        if (((classify_float a.p1) <> FP_nan) ||
            ((classify_float b.p1) <> FP_nan))
        then suma 
          (razyr ({l1 = a.l1 ; l2 = a.l2 ; p1 = nan ; p2 = nan}) 
                 ({l1 = b.l1 ; l2 = b.l2 ; p1 = nan ; p2 = nan})) 
          (razyr ({l1 = a.l1 ; l2 = a.l2 ; p1 = nan ; p2 = nan}) 
                 ({l1 = b.p1 ; l2 = b.p2 ; p1 = nan ; p2 = nan})) 
          (razyr ({l1 = a.p1 ; l2 = a.p2 ; p1 = nan ; p2 = nan}) 
                 ({l1 = b.p1 ; l2 = b.p2 ; p1 = nan ; p2 = nan})) 
          (razyr ({l1 = a.p1 ; l2 = a.p2 ; p1 = nan ; p2 = nan}) 
                 ({l1 = b.l1 ; l2 = b.l2 ; p1 = nan ; p2 = nan})) 
    else
      (* mnozenie dwoch pojedynczych przedzialow *)
      {
       l1 = min4 (a.l1 *. b.l1) (a.l1 *. b.l2) (a.l2 *. b.l1) (a.l2 *. b.l2) ;
       l2 = max4 (a.l1 *. b.l1) (a.l1 *. b.l2) (a.l2 *. b.l1) (a.l2 *. b.l2) ;
       p1 = nan ; 
       p2 = nan
      }

(* koniec kilku przydatnych funkcji *)

(* modyfikatory *)

(* wywolanie plusr jako dodawania *)
let plus a b = plusr a b

(* odejmowanie a i b to dodawanie a i przeciw b co mozna latwo udowodnic *)
let minus a b = plus a (przeciw b)

(* wywolanie razyr jako mnozenia *)
let razy a b = razyr a b

(*dzielenie a i b to mnozenie a i odw b co mozna latwo udowodnic *)
let podzielic a b = razy a (odwro b);;

(* koniec modyfikatorow *)
