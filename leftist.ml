(* Rafał Szulc - autor kodu *)

(* 
  typ kolejki jako wierzcholek lub lisc 
  - dla wierzcholka ustalamy wartosc jego priorytetu, 
  jego lewego syna,prawego syna 
  i wysokosc prawego poddrzewa 
*)
type 'a queue = 
  Node of {wartosc: 'a; lewy: 'a queue; prawy: 'a queue; prawa_wysokosc: int} | 
  Leaf

(* pusta kolejka priorytetowa *)
let empty = Leaf

(* zlaczenie kolejek q1 i q2 *)
let rec join q1 q2 =
  match (q1,q2) with
  | (Leaf,x) -> x
  | (y,Leaf) -> y
  | (Node(qq1),Node(qq2)) ->
      if qq1.wartosc > qq2.wartosc
      then join (Node(qq2)) (Node(qq1))
      else
        let q3 = join qq1.prawy (Node(qq2)) in
        match (qq1.lewy,q3) with
        | (Leaf,Node(qq3)) -> 
            Node({wartosc = qq1.wartosc; 
                  lewy = (Node(qq3));
                  prawy = Leaf; 
                  prawa_wysokosc = 0})
        | (Node(qq4),Node(qq3)) ->
            if qq4.prawa_wysokosc < qq3.prawa_wysokosc 
            then Node({wartosc = qq1.wartosc; 
                       lewy = (Node(qq3)); 
                       prawy = (Node(qq4)); 
                       prawa_wysokosc = (qq4.prawa_wysokosc + 1)})
            else
              Node({wartosc = qq1.wartosc; 
                    lewy = (Node(qq4)); 
                    prawy = (Node(qq3)); 
                    prawa_wysokosc = (qq3.prawa_wysokosc + 1)}) 

(* dodawanie nowego elementu (e) do kolejki q *)
let add e q =
  join q (Node({wartosc = e; lewy = Leaf; prawy = Leaf; prawa_wysokosc = 0}))

(* wyjatek pusta - kolejka *)
exception Empty

(* 
  z niepustej kolejki q usuwamy jej korzen 
  i go zwracamy oraz pozostalosc tej kolejki 
*)
let delete_min q =
  match q with
  | Leaf -> raise Empty
  | Node(qa) -> (qa.wartosc,join qa.lewy qa.prawy)

(* czy kolejka q jest pusta? *)
let is_empty q =
  match q with
  | Leaf -> true
  | Node(qq) -> false;;
