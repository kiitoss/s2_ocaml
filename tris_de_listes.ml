(* L’utilisation de List.hd et List.tl est globalement interdite. *)

(*
A TESTER:
  -> Nombres négatifs,
  -> Doublons,
  -> Float.
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
  |[] -> []
  |h::t ->
      if h = e
      then t
      else h :: (supprime e t);;
                                      


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
  |h::[] -> [h]
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
  partitionne_pivot_bis l pivot [] [] [];;




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
  |[] -> []
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
  |_ -> nb_par_liste (nb_par_colonne (nb_par_colonne (repliquer1_liste l) ((selectionne_max l) + (compter_tous 0 l))) (List.length l)) [];;




(*
Calcule du temps d'execution des fonctions:

let temps_debut = Sys.time () in 
let _ = telle_fonction_de_tri in 
let temps_fin = Sys.time () in 
(temps_fin -. temps_debut) ;;
*)

(*
tri_creation_max
TOPLEVEL 1 - 10:
  10
    - : float = 0.000999927520751953125
    - : float = 0.000999927520751953125
    - : float = 0.
  100
    - : float = 0.00599980354309082
    - : float = 0.00400018692016601563
    - : float = 0.00500011444091796875
  1000
    - : float = 0.226999998092651367
    - : float = 0.229000091552734375
    - : float = 0.216000080108642578
  2000
    - : float = 1.05100011825561523
    - : float = 1.04799985885620117
    - : float = 1.05200004577636719
  3000
    - : float = 2.35899996757507324
    - : float = 2.27999997138977051
    - : float = 2.16700005531311035
  4000
    - : float = 3.84800004959106445
    - : float = 3.76099991798400879
    - : float = 3.75399994850158691
  4500
    - : float = 5.15400004386901855
    - : float = 4.71200013160705566
    - : float = 4.84899997711181641

TOPLEVEL 2 - 100:
  10
    - : float = 0.00199985504150390625
    - : float = 0.000999927520751953125
    - : float = 0.
  100
    - : float = 0.00599980354309082
    - : float = 0.00599980354309082
    - : float = 0.00399994850158691406
  1000
    - : float = 0.248999834060668945
    - : float = 0.223999977111816406
    - : float = 0.228000164031982422
  2000
    - : float = 0.889000177383422852
    - : float = 0.922999858856201172
    - : float = 0.927000045776367188
  3000
    - : float = 2.50600004196167
    - : float = 2.35199999809265137
    - : float = 2.50699996948242188
  4000
    - : float = 3.87400007247924805
    - : float = 4.01100015640258789
    - : float = 3.95099997520446777
  4500
    - : float = 4.94499993324279785
    - : float = 5.0279998779296875
    - : float = 4.86299991607666

TOPLEVEL 3 - 1000:
  10
    - : float = 0.
    - : float = 0.
    - : float = 0.
  100
    - : float = 0.00599980354309082
    - : float = 0.00499987602233886719
    - : float = 0.00500011444091796875
  1000
    - : float = 0.226000070571899414
    - : float = 0.221999883651733398
    - : float = 0.240000009536743164
  2000
    - : float = 0.957000017166137695
    - : float = 0.878999948501586914
    - : float = 0.951999902725219727
  3000
    - : float = 2.57800006866455078
    - : float = 2.58999991416931152
    - : float = 2.40499997138977051
  4000
    - : float = 3.9440000057220459
    - : float = 3.97600007057189941
    - : float = 4.20300006866455078
  4500
    - : float = 5.22399997711181641
    - : float = 5.29500007629394531
    - : float = 5.33899998664856
*)

(*
tri_pivot
TOPLEVEL 1 - 10:
  10
    - : float = 0.
    - : float = 0.000999927520751953125
    - : float = 0.
  100
    - : float = 0.000999927520751953125
    - : float = 0.000999927520751953125
    - : float = 0.000999927520751953125
  1000
    - : float = 0.00499987602233886719
    - : float = 0.00399994850158691406
    - : float = 0.00199985504150390625
  2000
    - : float = 0.006999969482421875
    - : float = 0.00500011444091796875
    - : float = 0.00399994850158691406
  3000
    - : float = 0.00999999046325683594
    - : float = 0.006999969482421875
    - : float = 0.00899982452392578125
  4000
    - : float = 0.0110001564025878906
    - : float = 0.0120000839233398438
    - : float = 0.0109999179840087891
  4500
    - : float = 0.00900006294250488281
    - : float = 0.0120000839233398438
    - : float = 0.0160000324249267578

TOPLEVEL 2 - 100:
  10
    - : float = 0.
    - : float = 0.000999927520751953125
    - : float = 0.000999927520751953125
  100
    - : float = 0.00100016593933105469
    - : float = 0.00100016593933105469
    - : float = 0.000999927520751953125
  1000
    - : float = 0.00500011444091796875
    - : float = 0.00600004196166992188
    - : float = 0.00600004196166992188
  2000
    - : float = 0.00999999046325683594
    - : float = 0.006999969482421875
    - : float = 0.00599980354309082
  3000
    - : float = 0.0149998664855957031
    - : float = 0.0130000114440917969
    - : float = 0.00999999046325683594
  4000
    - : float = 0.0130000114440917969
    - : float = 0.0189998149871826172
    - : float = 0.0130000114440917969
  4500
    - : float = 0.0150001049041748047
    - : float = 0.0199999809265136719
    - : float = 0.0130000114440917969

TOPLEVEL 3 - 1000:
  10
    - : float = 0.000999927520751953125
    - : float = 0.00100016593933105469
    - : float = 0.00100016593933105469
  100
    - : float = 0.000999927520751953125
    - : float = 0.00100016593933105469
    - : float = 0.000999927520751953125
  1000
    - : float = 0.00800013542175293
    - : float = 0.00799989700317382813
    - : float = 0.00999999046325683594
  2000
    - : float = 0.00999999046325683594
    - : float = 0.0119998455047607422
    - : float = 0.00999999046325683594
  3000
    - : float = 0.0280001163482666
    - : float = 0.0170001983642578125
    - : float = 0.0190000534057617188
  4000
    - : float = 0.020999908447265625
    - : float = 0.0210001468658447266
    - : float = 0.0190000534057617188
  4500
    - : float = 0.0210001468658447266
    - : float = 0.020999908447265625
    - : float = 0.0250000953674316406
*)

(*
tri_comptage
TOPLEVEL 1 - 10:
  10
    - : float = 0.00100016593933105469
    - : float = 0.000999927520751953125
    - : float = 0.00200009346008300781
  100
    - : float = 0.00200009346008300781
    - : float = 0.00200009346008300781
    - : float = 0.000999927520751953125
  1000
    - : float = 0.00800013542175293
    - : float = 0.00499987602233886719
    - : float = 0.00499987602233886719
  2000
    - : float = 0.00799989700317382813
    - : float = 0.00600004196166992188
    - : float = 0.00899982452392578125
  3000
    - : float = 0.0329999923706054688
    - : float = 0.0570001602172851563
    - : float = 0.00900006294250488281
  4000
    - : float = 0.0910000801086425781
    - : float = 0.0329999923706054688
    - : float = 0.107000112533569336
  4500
    - : float = 0.0329999923706054688
    - : float = 0.0390000343322753906
    - : float = 0.105999946594238281

TOPLEVEL 2 - 100:
  10
    - : float = 0.00200009346008300781
    - : float = 0.00300002098083496094
    - : float = 0.00199985504150390625
  100
    - : float = 0.006999969482421875
    - : float = 0.00399994850158691406
    - : float = 0.00399994850158691406
  1000
    - : float = 0.0230000019073486328
    - : float = 0.0230000019073486328
    - : float = 0.0199999809265136719
  2000
    - : float = 0.0479998588562011719
    - : float = 0.0440001487731933594
    - : float = 0.0420000553131103516
  3000
    - : float = 0.122999906539916992
    - : float = 0.128999948501586914
    - : float = 0.128999948501586914
  4000
    - : float = 0.137999773025512695
    - : float = 0.187999963760375977
    - : float = 0.112999916076660156
  4500
    - : float = 0.187000036239624023
    - : float = 0.171000003814697266
    - : float = 0.180999994277954102

TOPLEVEL 3 - 1000:
  10
    - : float = 0.0350000858306884766
    - : float = 0.0350000858306884766
    - : float = 0.0339999198913574219
  100
    - : float = 0.0610001087188720703
    - : float = 0.0580000877380371094
    - : float = 0.0510001182556152344
  1000
    - : float = 0.25
    - : float = 0.243999958038330078
    - : float = 0.265000104904174805
  2000
    - : float = 0.766000032424926758
    - : float = 0.74199986457824707
    - : float = 0.778000116348266602
  3000
    - : float = 0.781000137329101563
    - : float = 0.828000068664550781
    - : float = 0.710000038146972656
  4000
    - : float = 1.01200008392333984
    - : float = 1.25300002098083496
    - : float = 0.884000062942504883
  4500
    - : float = 1.
    - : float = 1.13700008392333984
    - : float = 1.02699995040893555
*)

(*
tri_perles1
TOPLEVEL 1 - 10:
  10
    - : float = 0.00100016593933105469
    - : float = 0.000999927520751953125
    - : float = 0.
  100
    - : float = 0.00399994850158691406
    - : float = 0.00200009346008300781
    - : float = 0.00200009346008300781
  1000
    - : float = 0.0210001468658447266
    - : float = 0.0169999599456787109
    - : float = 0.0190000534057617188
  2000
    - : float = 0.0580000877380371094
    - : float = 0.0559999942779541
    - : float = 0.0550000667572021484
  3000
    - : float = 0.177000045776367188
    - : float = 0.154000043869018555
    - : float = 0.176999807357788086
  4000
    - : float = 1.57600021362304688
    - : float = 1.4570000171661377
    - : float = 1.42700004577636719
  4500
    - : float = 1.43600010871887207
    - : float = 2.10100007057189941
    - : float = 1.59899997711181641

TOPLEVEL 2 - 100:
  10
    - : float = 0.000999927520751953125
    - : float = 0.
    - : float = 0.
  100
    - : float = 0.00799989700317382813
    - : float = 0.00499987602233886719
    - : float = 0.00499987602233886719
  1000
    - : float = 0.0389997959136962891
    - : float = 0.0360000133514404297
    - : float = 0.0360000133514404297
  2000
    - : float = 0.333999872207641602
    - : float = 0.337999820709228516
    - : float = 0.340000152587890625
  3000
    - : float = 0.749000072479248
    - : float = 0.684000015258789063
    - : float = 0.717000007629394531
  4000
    - : float = 1.64800000190734863
    - : float = 1.34200000762939453
    - : float = 1.31599998474121094
  4500
    - : float = 1.87199997901916504
    - : float = 1.8469998836517334
    - : float = 1.92400002479553223

TOPLEVEL 3 - 1000:
  10
    - : float = 0.00799989700317382813
    - : float = 0.00500011444091796875
    - : float = 0.00799989700317382813
  100
    - : float = 0.0360000133514404297
    - : float = 0.0160000324249267578
    - : float = 0.0169999599456787109
  1000
    - : float = 0.414999961853027344
    - : float = 0.427999973297119141
    - : float = 0.430999994277954102
  2000
    - : float = 0.848000049591064453
    - : float = 0.654000043869018555
    - : float = 0.60299992561340332
  3000
    - : float = 1.2220001220703125
    - : float = 1.20199990272521973
    - : float = 1.24799990653991699
  4000
    - : float = 1.9309999942779541
    - : float = 0.796000003814697266
    - : float = 1.81100010871887207
  4500
    - : float = 2.33299994468688965
    - : float = 2.32200002670288086
    - : float = 2.28099989891052246
*)

(*
tri_perles2
TOPLEVEL 1 - 10:
  10
    - : float = 0.
    - : float = 0.
    - : float = 0.
  100
    - : float = 0.00300002098083496094
    - : float = 0.00300002098083496094
    - : float = 0.00300002098083496094
  1000
    - : float = 0.0290000438690185547
    - : float = 0.0329999923706054688
    - : float = 0.159000158309936523
  2000
    - : float = 0.123999834060668945
    - : float = 0.14700007438659668
    - : float = 0.128999948501586914
  3000
    - : float = 0.715999841690063477
    - : float = 0.594000101089477539
    - : float = 0.592999935150146484
  4000
    - : float = 0.869999885559082
    - : float = 0.560999870300293
    - : float = 0.626000165939331
  4500
    - : float = 0.648000001907348633
    - : float = 0.694999933242797852
    - : float = 0.746999979019165

TOPLEVEL 2 - 100:
  10
    - : float = 0.00200009346008300781
    - : float = 0.00199985504150390625
    - : float = 0.000999927520751953125
  100
    - : float = 0.00899982452392578125
    - : float = 0.006999969482421875
    - : float = 0.00700020790100097656
  1000
    - : float = 0.0639998912811279297
    - : float = 0.0590000152587890625
    - : float = 0.0700001716613769531
  2000
    - : float = 0.346000194549560547
    - : float = 0.340000152587890625
    - : float = 0.319999933242797852
  3000
    - : float = 0.383999824523925781
    - : float = 0.41100001335144043
    - : float = 0.315999984741210938
  4000
    - : float = 0.411999940872192383
    - : float = 0.5970001220703125
    - : float = 0.51399993896484375
  4500
    - : float = 0.684000015258789063
    - : float = 0.719000101089477539
    - : float = 0.674000024795532227

TOPLEVEL 3 - 1000:
  10
    - : float = 0.006999969482421875
    - : float = 0.00599980354309082
    - : float = 0.00900006294250488281
  100
    - : float = 0.0640001296997070313
    - : float = 0.0350000858306884766
    - : float = 0.0490000247955322266
  1000
    - : float = 0.659000158309936523
    - : float = 0.663999795913696289
    - : float = 0.657999992370605469
  2000
    - : float = 0.983999967575073242
    - : float = 0.976999998092651367
    - : float = 1.00599980354309082
  3000
    - : float = 1.74000000953674316
    - : float =  1.75999999046325684
    - : float = 1.67499995231628418
  4000
    - : float = 2.22599983215332031
    - : float = 2.39299988746643066
    - : float = 2.22100019454956055
  4500
    - : float = 5.85000014305114746
    - : float = 2.23000001907348633
    - : float = 2.21900010108947754
*)