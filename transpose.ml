let transpose lista =
  let wynik = List.fold_left (fun acc _ -> ([] :: acc)) [] (List.hd lista) in
  let wynik2 = List.fold_left (fun acc element -> List.rev(List.fold_left2 (fun ak x y -> ((x :: y) :: ak)) [] element acc)) wynik lista in
  let wynik3 = List.fold_left (fun acc element -> ((List.rev element) :: acc)) [] wynik2 in
  List.rev wynik3;;

(* transpose [[1;2;3;4];[5;6;7;8];[9;10;11;12];[13;14;15;16]];; *)
