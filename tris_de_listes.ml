(* L’utilisation de List.hd et List.tl est globalement interdite. *)

(*
A TESTER:
  -> Nombres négatifs,
  -> Doublons.

A VERIFIER:
  -> Pas de rec inutile,
  -> Pas de match dans match avant tri perles 2.
*)





(* --------------------------- *)
(* TRI PAR CREATION DU MAXIMUM *)
(* --------------------------- *)

(*
liste aleatoire nb_entiers borne_max renvoie une liste de nombres entiers tirés aléatoirement.
  -> Chaque nombre a une valeur comprise entre 0 inclus et borne max excluse.
  -> La liste renvoyée peut bien sûr contenir des doublons.
*)
let rec liste_aleatoire nb_entiers borne_max =
  if nb_entiers = 0
  then []
  else Random.int borne_max :: liste_aleatoire (nb_entiers-1) borne_max;;



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

(*
ici, le cas d'une liste vide en entrée ne renvoie pas d'erreur "liste vide" mais simplement une liste vide.
*)

(*
partitionne_pivot l pivot partage la liste l en trois listes et les renvoie sous forme de triplet.
La première liste contenant tous les éléments de l plus petits que le pivot,
la seconde contenant tous les éléments qui lui sont égaux,
la troisième contenant tous les éléments qui lui sont supérieurs.
*)
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
  |t -> partitionne_pivot_bis t pivot [] [] [];;




(*
tri_pivot l sélectionne la tête x de l (qu’on appelle le pivot) et calcule le triplet de listes (l1,l2,l3).
Le résultat du rappel récursif de la fonction sur l1 et l3 est ensuite concaténé à l2,
dans le bon ordre, afin de renvoyer la liste finale triée.
*)
let rec tri_pivot l =
  match l with
  |[] -> []
  |h::t ->
      match partitionne_pivot l h with
      |(l1, l2, l3) -> tri_pivot l1 @ l2 @ tri_pivot l3;;


























(* ------------ *)
(* TRI COMPTAGE *)
(* ------------ *)

(*
compter_tous v l compte le nombre de fois où la valeur v apparaı̂t dans l.
*)
let rec compter_tous v l =
  match l with
  |[] -> 0
  |h::t ->
      if h = v
      then 1+compter_tous v t
      else compter_tous v t;;



(*
enlever_tous v l supprime toutes les occurences de v dans l.
*)
let rec enlever_tous v l =
  match l with
  |[] -> failwith "La liste est vide, aucun élément à supprimer."
  |h::[] ->
      if h = v
      then []
      else [h]
  |h::t ->
      if h = v
      then enlever_tous v t
      else h :: enlever_tous v t;;


(*
liste_associative l borne fin renvoie une liste associative, c’est-à-dire une liste de paires i,nb i,
pour toutes les valeurs possibles de i dans l (i allant de 0 inclus à borne fin excluse).
Chaque nb i correspond au nombre d’occurences de i dans l.
*)
let rec liste_associative l borne_fin = 
  if borne_fin = 0
  then failwith "Vous devez rentrer un nombre supérieur à 0"
  else
  if borne_fin = 1
  then [(borne_fin-1, compter_tous (borne_fin-1) l)]
  else
    liste_associative (enlever_tous (borne_fin-1) l) (borne_fin-1) @ [(borne_fin-1, compter_tous (borne_fin-1) l)];;


(*
liste_entiers x nb x renvoie une liste de nb x fois l’entier x.
*)
let rec liste_entiers x nb_x = 
  if nb_x = 0
  then []
  else [x] @ liste_entiers x (nb_x-1);;





(*
reconstituer_liste la reconstitue une liste classique,
à partir d’une liste associative i,nb i (crée une liste avec nb i fois chaque i).
*)
let rec reconstituer_liste la = 
  match la with 
  |[] -> []
  |(nb, i)::t -> liste_entiers nb i @ reconstituer_liste t;;



(*
tri_comptage l borne_max crée la liste associative correspondant à l et sa borne max,
puis reconstitue la liste triée correspondante.
*)
let tri_comptage l borne_max =
  match l with
  |[] -> failwith "La liste est vide, aucun élément à trier"
  |_ -> reconstituer_liste (liste_associative l borne_max);;


























(* ---------------- *)
(* TRI DE PERLES V1 *)
(* ---------------- *)

(*
Récupération des fonctions selectionne_max et liste_entiers
*)

(*
ajoute1 nb_premiers_elements l ajoute 1 aux nb_premiers_elements de l.
*)
let rec ajout1 nb_premiers_elements l = 
  match l with
  |[] -> []
  |h::t ->
      if nb_premiers_elements > 0
      then (h+1) :: ajout1 (nb_premiers_elements-1) t
      else h :: t;;


(*
Si l1 = [x1;...;xn], alors pour tout xi de l1, incrementation l1 l2 ajoute 1 à tous les xi premiers elements de l2.
*)
let rec incrementation l1 l2 =
  match l1 with
  |[] -> l2
  |h::t ->
      if h=0
      then incrementation t l2
      else incrementation t (ajout1 h l2);;



(*
nombre_positifs l renvoie le nombre d’éléments de l strictement positifs.
*)
let rec nombre_positifs l =
  match l with
  |[] -> 0
  |h::t ->
      if h > 0
      then 1 + nombre_positifs t
      else nombre_positifs t;;



(*
enleve1 l décrémente de 1 tous les éléments de l qui sont strictement positifs.
*)
let rec enleve1 l =
  match l with
  |[] -> []
  |h::t ->
      if h > 0
      then (h-1) :: enleve1 t
      else h :: enleve1 t;;


(*
decompte_elements l n créé la liste des nombres représentés dans l.
Plus clairement, decompte_elements l n récupère le nombre d’éléments positifs dans l,
et le concatène au rappel de la fonction sur l,
dans laquelle tous les éléments positifs ont été décrémentés de 1.
Cette récursion a lieu n fois.
*)
let rec decompte_elements l n =
  match l with
  |[] -> []
  |h::t ->
      if n = 0
      then [nombre_positifs l]
      else [nombre_positifs l] @ (decompte_elements (enleve1 l) (n-1));;

(*
inverser_l inverse les éléments de l (contrainte: interdit d’utiliser List.rev).
*)
let rec inverser l =
  match l with
  |[] -> []
  |h::[] -> [h]
  |h::t -> (inverser t) @ [h];;





(*
tri_perles1 récupère le maximum max de l et crée une liste de max 0.
Cette liste est alors incrémentée selon l, puis le résultat est décompté List.length l - 1 fois.
Enfin, le décompte est inversé, ce qui renvoie la liste l triée.
*)
let tri_perles1 l =
  match l with
  |[] -> failwith "La liste est vide, aucun élément à trier"
  |_ -> inverser (decompte_elements (incrementation l (liste_entiers 0 (selectionne_max l))) ((List.length l) - 1));; 


























(* ---------------- *)
(* TRI DE PERLES V2 *)
(* ---------------- *)

(*
Récupération de la fonction liste_entiers.
*)


(*
repliquer1_liste l réplique la liste l sous forme de listes de 1.
*)
let rec repliquer1_liste l =
  match l with
  |[] -> []
  |h::t -> (liste_entiers 1 h) :: repliquer1_liste t;;




(*
tetes_liste l prend en paramètre une liste de listes, et renvoie la liste composée de chacun de leurs premiers éléments.
Si une liste de l est vide, alors elle est ignorée.
*)
let rec tetes_liste l =
  match l with
  |[] -> []
  |h::t ->
      match h with
      |[] -> tetes_liste t
      |h2::t2 -> h2 :: tetes_liste t;;




(*
supprimer_tetes_liste l prend en paramètre une liste de listes, et la renvoie en ayant supprimé le premier élément de chacune.
Si une liste de l est vide, alors elle est ignorée.
*)
let rec supprimer_tetes_liste l =
  match l with
  |[] -> []
  |h::t ->
      match h with
      |[] -> [] :: supprimer_tetes_liste t
      |h2::t2 ->  t2 :: supprimer_tetes_liste t;;



(*
nb_par_colonne l n calcule récursivement (n fois) la liste des têtes de liste de l,
puis de l dont on a supprimé les têtes de listes, etc.
*)
let rec nb_par_colonne l n = 
  if n=0
  then []
  else tetes_liste l :: (nb_par_colonne (supprimer_tetes_liste l) (n-1));;


(* Erreur dans l'énoncé *)
(*
nb_par_liste l1 l2 calcule dans l2 la longueur de chaque liste de l1, de droite à gauche.
*)
let rec nb_par_liste l1 l2 =
  match l1 with
  |[] -> l2
  |h::t -> nb_par_liste t (List.length h :: l2);;


(*
tri_perles2 l récupère le maximum max de l et le nombre de fois nb 0 où le chiffre 0 apparaı̂t dans l.
Puis l est répliquée sous forme de listes de 1.
Ensuite, le nombre d’éléments par colonne de cette liste de 1 est calculé, max + nb 0 fois.
Après, c’est au tour du nombre d’éléments par colonne de cette nouvelle liste, List.length l fois.
Enfin, le nombre d’éléments par liste de cette dernière liste est renvoyé, ce qui donne le résultat final.
*)
let tri_perles2 l = 
  match l with
  |[] -> failwith "La liste est vide, aucun élément à trier"
  |_ -> nb_par_liste (nb_par_colonne (nb_par_colonne (repliquer1_liste l) ((selectionne_max l) + (compter_tous 0 l))) (List.length l)) []
  
  