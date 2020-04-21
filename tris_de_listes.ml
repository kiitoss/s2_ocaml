(* L’utilisation de List.hd et List.tl est globalement interdite. *)







(* --------------------------- *)
(* TRI PAR CREATION DU MAXIMUM *)
(* --------------------------- *)

(*
liste aleatoire nb_entiers borne_max renvoie une liste de nombres entiers tirés aléatoirement.
  -> Chaque nombre a une valeur comprise entre 0 inclus et borne max excluse.
  -> La liste renvoyée peut bien sûr contenir des doublons.
*)
let rec liste_aleatoire nb_entiers borne_max =
  if nb_entiers = 0 then
    []
  else
    Random.int borne_max :: liste_aleatoire (nb_entiers-1) borne_max;;



(*
selectionne_max l calcule le maximum de la liste l.
*)
let rec selectionne_max l =
  match l with
  |[] -> failwith "La liste est vide, aucun maximum"
  |h::[] -> h
  |h::g::t ->
      if h>g
      then selectionne_max (h :: t)
      else selectionne_max (g :: t);;



(*
supprime e l retourne la liste l privée de la 1ère occurence de e quand elle existe.
*)
let rec supprime e l =
  match l with
  |[] -> failwith "La liste est vide, aucun élément à supprimer" 
  |h::t ->
      if h = e
      then t
      else
        match t with
        | [] -> h :: t
        | t -> h :: supprime e t;;
                                      


(*
ajoute_fin e l retourne la liste l dans laquelle l’élément e a été ajouté en dernière position.
*)
let rec ajoute_fin e l =
  match l with
  |[] -> [e]
  |h::t -> h :: ajoute_fin e t;; 




(*
tri_creation_max l trie une liste.
Pour cela, cette fonction sélectionne récursivement le maximum de l, et l’ajoute à la fin du reste de la liste déjà triée,
(c’est-à-dire à la fin du rappel de la fonction sur la liste privée de son maximum).
*)
let rec tri_creation_max l =
  match l with
  |[] -> failwith "La liste est vide, aucun élément à trier"
  |h::[] -> ajoute_fin (selectionne_max l) (supprime (selectionne_max l) l)
  |h::t -> ajoute_fin (selectionne_max l) (tri_creation_max (supprime (selectionne_max l) l));;












(* --------- *)
(* TRI PIVOT *)
(* --------- *)

let rec partitionne_pivot_bis l pivot l1 l2 l3 =
  match l with
  |[] -> (l1, l2, l3)
  |h::t ->
      if h < pivot
      then partitionne_pivot_bis t pivot (h :: l1) l2 l3
      else if h = pivot
      then partitionne_pivot_bis t pivot l1 (h :: l2) l3
      else partitionne_pivot_bis t pivot l1 l2 (h :: l3);;

let partitionne_pivot l pivot =
  match l with
  |[] -> failwith "Votre liste est vide"
  |t -> partitionne_pivot_bis t pivot [] [] [];;

let tri_pivot l =
  match l with
  |[] -> failwith "Votre liste est vide"
  |h::t -> partitionne_pivot l h;;