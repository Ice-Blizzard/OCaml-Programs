(* autor kodu Rafał Szulc *)

(* szukana minimalna ilość przelań lub -1 gdy to niemożliwe *)
(* val przelewanka : (int * int) array -> int *)
let przelewanka rozwazane_przelanie =
  (* n - ilość szklanek jak zmienna z zadania *)
  let n =
    Array.length rozwazane_przelanie
  in
  let wynik =
    ref (-1)
  in
  (* nwd dwóch nieujemnych liczb a,b algorytm euklidesa, nwd (0,0) = 0 *)
  let rec nwd a b =
    if a < b
    then nwd b a
    else
      if b = 0
      then a
      else nwd b (a mod b)
  in
  (* sprawdzenie czy można otrzymać ostateczny wynik szklanki *)
  let czy_szklanka =
    let nwd =
      Array.fold_left
        (fun acc (x, _) -> nwd x acc)
        0
        rozwazane_przelanie
    in
    match nwd with
    | 0 -> true
    | _ -> Array.fold_left 
             (fun acc (_, y) -> (acc && (y mod nwd = 0)))
             true
             rozwazane_przelanie
  in
  (* czy można osiągnąć szukany stan końcowy *)
  let czy_koniec =
    Array.fold_left
      (fun acc (x, y) -> (y = 0 || x = y || acc))
      (n = 0)
      rozwazane_przelanie
  in
  (* jeśli w 0 ruchów można mieć szukany stan *)
  let czy_wynik_to_0 =
    Array.fold_left
      (fun acc (_, y) -> (if (y = 0 && acc = true) then true else false)) 
      true
      rozwazane_przelanie
  in
  if czy_wynik_to_0 = true
  then 0
  else
    ( 
    (* jeśli nie da się osiągnąć stanu końcowego wypisujemy wynik *)
    if (czy_szklanka && czy_koniec)
    then
      (
      (* funkcja która z danego stanu robi
      stan z wylaną wodą z k-tej szklanki *)
      let wylej stan k =
        let kopia =
          Array.copy stan
        in
        (kopia.(k) <- 0; kopia;)
      in
      (* funkcja która z danego stanu robi
      stan z wlaną wodą do k-tej szklanki *)
      let wlej stan k =
        let kopia =
          Array.copy stan
        in
        (kopia.(k) <- fst rozwazane_przelanie.(k); kopia;)
      in
      (* funkcja która z danego stanu robi listę stanów z przelaną wodą *)
      (* z k-tej szklanki do innych szklanek (czyli lista ma n-1 elementów) *)
      let przelej stan k =
        let lista =
          ref []
        in
        let kopia =
          Array.copy stan
        in
        for i = 0
        to (n - 1)
        do
          if (k <> i)
          then
            let kopia2 =
              Array.copy kopia
            in
            if (fst rozwazane_przelanie.(i) - kopia.(i) < kopia.(k))
            then
              (  
              kopia2.(k) <- kopia.(k) - fst rozwazane_przelanie.(i) + kopia.(i);
              kopia2.(i) <- fst rozwazane_przelanie.(i);
              lista := kopia2 :: (!lista)
              )
            else
              (
              kopia2.(i) <- kopia.(i) + kopia.(k);
              kopia2.(k) <- 0;
              lista := kopia2 :: (!lista)
              )
          else ()
        done;
        !lista;
      in
      (* sprawdza czy stan jest tym szukanym *)
      let czy_ostateczny_stan stan =
        fst
          (
          Array.fold_left
            (
            fun (a, b) x ->
              if snd rozwazane_przelanie.(b) = x
              then (a, b + 1)
              else (false, b + 1)
            )
            (true, 0)
            stan
          )
      in
      (* dodaje stan do tablicy hashy wtedy i 
      tylko wtedy gdy go nie ma w niej *)
      let czy_dodac_do_hashy hash_tablica stan d =
        if not (Hashtbl.mem hash_tablica stan)
        then (Hashtbl.add hash_tablica stan d; true)
        else false
      in
      (* BFS po wszystkich możliwych stanach -
      dzięki stworzonej tablicy hashy
      każdy stan będzie odwiedzony dokładnie jeden raz *)
      let stan =
        Array.make n 0
      in
      let hash_tablica =
        Hashtbl.create 2137
      in
      let kolejka_stanow =
        Queue.create ()
      in
      (
      Hashtbl.add hash_tablica stan 0;
      Queue.push stan kolejka_stanow;
      while (not (Queue.is_empty kolejka_stanow) && !wynik = -1)
      do
        let chwilowy_stan =
          Queue.pop kolejka_stanow
        in
        let d =
          Hashtbl.find hash_tablica chwilowy_stan + 1
        in
        for i = 0
        to (n - 1)
        do
          let lista =
            (wlej chwilowy_stan i) ::
            (wylej chwilowy_stan i) ::
            (przelej chwilowy_stan i)
          in    
          (
          List.iter
          (
          fun x ->
            if (czy_dodac_do_hashy hash_tablica x d)
            then 
              begin
                if (czy_ostateczny_stan x)
                then wynik := d;
                Queue.push x kolejka_stanow
              end
          )
          lista;
          )
        done;
      done;
      !wynik;
      );  
    )
    else !wynik
    );;


