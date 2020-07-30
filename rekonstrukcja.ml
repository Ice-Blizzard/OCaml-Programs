type 'a heap =
  Node of 'a heap * 'a * 'a heap |
  Null
let rec rekonstrukcja tab =
  match tab with
  | [||] -> Null
  | [|x|] -> Node(Null, x, Null)
  | tab ->
    let poz = ref 0 in
    let maxi = ref tab.(0) in
    begin
      for i = 1 to (Array.length tab - 1) do
        if (tab.(i) > !maxi) then 
        (maxi := tab.(i); poz := i)
      done
    end;
    let tab1 = Array.make !poz 0 in
    let tab2 = Array.make (Array.length tab - !poz - 1) 0 in
    begin
      for i = 0 to (!poz - 1) do
        tab1.(i) <- tab.(i)
      done  
    end;
    begin
      for i = (!poz + 1) to (Array.length tab - 1) do
        tab2.(i - 1 - !poz) <- tab.(i)
      done  
    end;
    Node((rekonstrukcja tab1), !maxi, (rekonstrukcja tab2))
;;
