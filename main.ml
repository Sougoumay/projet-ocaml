
Random.self_init()

(* la definition des types *)

(*Personnage*)
type classe= Archer
          | Guerrier
          | Magicien
                              
type  objet =
            | Piece
            | Cuisse
            | Eponge

type sac = (objet*int) list

type personnage = { nom: string;
                    classe: classe;
                    genre : string;
                    nb_vie : int;
                    nb_exp : int;
                    niveau : int;
                    sac_personnage : sac ; 
                  } 

(*Monstre*)
type golem
type sanglier
type nuee_de_moustique
 
type genre_monstre = | Golem
                    | Sanglier
                    | Nue_de_Moustique

type monstre = { genre_monstre : genre_monstre;
                objet_du_monstre: objet;
                pt_de_vie: int
              }
(** affichage du contenu du sac du personnage *)
let rec aux_contenu : (objet*int) list -> string = fun elt ->
  match elt with
  | [(Cuisse,n)] -> string_of_int n ^ " Cuisse,"  
  | [(Eponge,n)] -> string_of_int n ^ " Eponge," 
  | [(Piece,n)] -> string_of_int n ^  " Piece\n"
  | hd::tl -> 
      match hd with 
      | (Cuisse,n) -> string_of_int n ^ " Cuisse,"  ^ aux_contenu tl
      | (Eponge,n) ->string_of_int n ^ " Eponge," ^ aux_contenu tl
      | (Piece,n) ->   string_of_int n ^  " Piece\n" ^ aux_contenu tl  


let rec contenu = fun personnage -> 
  match personnage.sac_personnage with
  | [] -> "Votre sac est vide" 
  | [h] -> aux_contenu [h]
  | h::t -> aux_contenu (h::t)

(** L'affichage de l'etat du personnage *)
let etat_personnage = fun etat_personnage -> 
  " Nom: " ^ etat_personnage.nom ^ " \n" ^
  " Genre: "^etat_personnage.genre ^ "\n" ^
  " Niveau: "^(string_of_int etat_personnage.niveau)^"\n"^
  " Point de vie: " ^" " ^(string_of_int etat_personnage.nb_vie)^"\n"^
  " Expérience: "  ^ " " ^(string_of_int etat_personnage.nb_exp) ^"\n"^
  " Sac: " ^  aux_contenu etat_personnage.sac_personnage 

(** Elle genere aleatoirement un objet lors de la creation du monstre *)
let generer_objet : unit -> objet = fun () ->
  let n = (Random.int 3) in
  match n with
  | 0 -> Cuisse
  | 1 -> Piece
  | 2 -> Eponge

(** La creation d'un monstre en fonction des caracteristiques specifiques de celui-ci *)
let genrer_monstre : unit -> monstre = fun () ->
  let n = (Random.int 3) in
  match n with
  | 0 -> let point_de_vie = 25 + (Random.int 6) in
      {genre_monstre = Golem; objet_du_monstre = generer_objet (); pt_de_vie = point_de_vie}
  | 1 -> let nb_moustique = Random.int (Random.int 30) in
      {genre_monstre = Nue_de_Moustique; objet_du_monstre = generer_objet(); pt_de_vie = 2 + nb_moustique}
  | 2 -> let point_de_vie = 10 + (Random.int 4) in
      {genre_monstre = Sanglier; objet_du_monstre = generer_objet (); pt_de_vie = point_de_vie}

 (** Le personnage frappe un monstre, il prend un personnage et un monstre en parametre et retourne le nouveau etat du monstre en ojet monstre *)       
let frapper : personnage -> monstre -> monstre*unit = fun p m ->
  match p.classe with
  | Guerrier -> 
      let chance = Random.int 100 in
      if chance >= (30+5*p.niveau) then 
        let pt_de_vie = m.pt_de_vie - 10 in
        let genre = m.genre_monstre in
        let objet = m.objet_du_monstre in
        { genre_monstre = genre; objet_du_monstre = objet; pt_de_vie = pt_de_vie},
        print_string("Vous frappez et infligez 10 points de degat\n"^"Le monstre a "^string_of_int(pt_de_vie)^" point de vie \n")
      else m,print_string("Vous attaquez, mais vous manquez la cible\n"^"Le monstre a "^string_of_int(m.pt_de_vie)^" point de vie \n")
  | Archer -> 
      let chance = Random.int 100 in
      if chance >= (70+5*p.niveau) then
        let pt_de_vie = m.pt_de_vie - 4 in
        let genre = m.genre_monstre in
        let objet = m.objet_du_monstre in
        { genre_monstre = genre; objet_du_monstre = objet; pt_de_vie = pt_de_vie},
        print_string("Vous frappez et infligez 4 points de degat\n"^"Le monstre a "^string_of_int(pt_de_vie)^" point de vie \n") 
      else m,print_string("Vous attaquez, mais vous manquez la cible\n"^"Le monstre a "^string_of_int(m.pt_de_vie)^" point de vie \n")
        
  | Magicien -> 
      let chance = Random.int 100 in
      if chance >= (50+5*p.niveau) then 
        let pt_de_vie = m.pt_de_vie - 5 in
        let genre = m.genre_monstre in
        let objet = m.objet_du_monstre in
        { genre_monstre = genre; objet_du_monstre = objet; pt_de_vie = pt_de_vie},
        print_string("Vous frappez et infligez 5 points de degat\n"^"Le monstre a "^string_of_int(pt_de_vie)^" point de vie \n") 
      else m,print_string("Vous attaquez, mais vous manquez la cible\n"^"Le monstre a "^string_of_int(m.pt_de_vie)^" point de vie \n") 

(** Cette fonction prend en parametre un personnage et retourne un quintuplet des attributs du personnage sans son point de vie *)
let personnage_5 : personnage -> (classe*string*string*int*int*sac) = fun pers ->
  let classe = pers.classe in 
  let genre = pers.genre in
  let exp = pers.nb_exp in
  let nom = pers.nom in
  let niveau = pers.niveau in
  let sac = pers.sac_personnage in
  (classe,nom,genre,exp,niveau,sac) 

(** Cette fonction ajoute le point de vie du personnage s;il arrive a manger et renvoie un tuple d'un int et tuple qui represente les cuisses qu'il a mange, s'il a reussi a mange 
et l'etat de celui-ci *)
let nouveau_etat_manger : personnage -> int*(bool*personnage) = fun pers ->
  if pers.nb_vie < 20 then 
    if pers.nb_vie <= 18 then
      let vie = pers.nb_vie + 2 in 
      let classe, nom, genre, exp, niveau, sac = personnage_5 pers in
      (2,(true,{
          nom = nom; classe = classe; genre = genre; nb_exp = exp;
          nb_vie = vie; niveau = niveau; sac_personnage = sac
        }))
    else
      let vie = pers.nb_vie + 1 in 
      let classe, nom, genre, exp, niveau, sac = personnage_5 pers in
      (1,(true,{
          nom = nom; classe = classe; genre = genre; nb_exp = exp;
          nb_vie = vie; niveau = niveau; sac_personnage = sac
        }))
  else (0,(false,pers))
 
(** Le monstre frappe le personnage qu'on les passes tous les deux en parametres et retourne le nouveau etat du personnage qui a subit des degats *)
let monstre_frapper : monstre -> personnage -> personnage =
  fun monstre pers ->
    match monstre.genre_monstre with
    | Golem ->
        let vie = pers.nb_vie - 4 in
        let classe, nom, genre, exp, niveau, sac = personnage_5 pers in
        {
            nom = nom; classe = classe; genre = genre; nb_exp = exp;
            nb_vie = vie; niveau = niveau; sac_personnage = sac
          }
    | Sanglier ->
        let vie = pers.nb_vie - 2 in
        let classe, nom, genre, exp, niveau, sac = personnage_5 pers in
        {
            nom = nom; classe = classe; genre = genre; nb_exp = exp;
            nb_vie = vie; niveau = niveau; sac_personnage = sac
          }
    | Nue_de_Moustique ->
        let armee_moustique = Random.int (Random.int 30) in
        let vie = pers.nb_vie - armee_moustique/2 in
        let classe, nom, genre, exp, niveau, sac = personnage_5 pers in
        {
            nom = nom; classe = classe; genre = genre; nb_exp = exp;
            nb_vie = vie; niveau = niveau; sac_personnage = sac
          }

(** Cette fonction modifie le sac du personnage lorsqu'il mange pour le mettre a jour *)
let rec modif_sac_manger : sac -> int -> sac = fun bag n ->
  match bag with
  | [] -> []
  | [(Cuisse,x)] -> [(Cuisse,x-n)]
  | hd::tl ->
      if fst hd = Cuisse then (fst hd, (snd hd - n))::tl
      else hd::(modif_sac_manger tl n)

(** Elle prend en parametre un personnage et un entier qui reprenste le nombre de cuisse passee en parametre et qui retourne un quituplet des attributs du personnage 
sans le point de vie tout en modifiant le sac de ce dernier *)
let rec personnage_4 : personnage -> int -> (classe*string*string*int*int*sac) =
  fun pers n -> 
    let classe = pers.classe in 
    let genre = pers.genre in
    let exp = pers.nb_exp in
    let nom = pers.nom in
    let niveau = pers.niveau in
    let sac = modif_sac_manger pers.sac_personnage n in
    (classe,nom,genre,exp,niveau,sac)

(** Elle prend en parametre un personnage et retourne l'etat de celui-ci s'il a reussi a manger ou pas *)
let manger : personnage -> int*(bool*personnage) = fun pers ->
  let sac = pers.sac_personnage in
  let rec aux_contenu = fun sacs ->
    match sacs with 
    | hd::tl ->
        if fst hd = Cuisse then (fst hd, snd hd)
        else aux_contenu tl
  in let obj = aux_contenu sac in 
  if snd obj >= 1 then
    nouveau_etat_manger pers 
  else (0,(false, pers))

(** Elle prend en parametre le nouveau etat du personnage dans la fonction choix_aventure et affiche un etat sur l'ecran qu'il a reussi a manger ou pas *)
let string_manger:int*(bool*personnage) -> unit=fun x->
  match fst (snd x) with
  |true -> print_string("Vous avez réussi a manger 1 Poulet et gagnez "^string_of_int(fst x)^" point(s) de vie\n")
  |false ->print_string("Vous n'avez pas réussi a manger\n")

(** Elle ajoute la vie de la personne quand elle arrive a dormir sans depasser 20 points de vie *)
let nouveau_etat_dormir =fun pers ->
  if pers.nb_vie < 20 then 
    if pers.nb_vie <= 16 then
    
      {
          nom = pers.nom; classe = pers.classe; genre = pers.genre; nb_exp = pers.nb_exp;
          nb_vie = pers.nb_vie+4; niveau = pers.niveau; sac_personnage = pers.sac_personnage
        }
    else if pers.nb_vie = 17 then
      {
          nom = pers.nom; classe = pers.classe; genre = pers.genre; nb_exp = pers.nb_exp;
          nb_vie = pers.nb_vie+3; niveau = pers.niveau; sac_personnage = pers.sac_personnage
        }
    else if pers.nb_vie = 18 then
      {
          nom = pers.nom; classe = pers.classe; genre = pers.genre; nb_exp = pers.nb_exp;
          nb_vie = pers.nb_vie+2; niveau = pers.niveau; sac_personnage = pers.sac_personnage
        }
    else
        {
            nom = pers.nom; classe = pers.classe; genre = pers.genre; nb_exp = pers.nb_exp;
            nb_vie = pers.nb_vie+1; niveau = pers.niveau; sac_personnage = pers.sac_personnage
          }
  else pers        

(** On signale la fin d'une partie en passant en parametre la raison de l'arret du programme *)
exception Fin_partie of string

(** Elle fait dormir le personnage mais avec un risque que celui-ci soit tuer *)
let dormir : personnage -> personnage*unit = fun personnage -> 
  let mons=genrer_monstre () and
   m="Vous vous endormiez \n" and chance = Random.int 100
   in
  if chance > 5
  then nouveau_etat_dormir personnage,print_string(m^etat_personnage ( nouveau_etat_dormir personnage ))
  else 
    match mons.genre_monstre with
    |Golem->print_string(m^"Un golem vous à tuer pendant la nuit \n");raise (Fin_partie "Vous etes mort")
    |Sanglier ->print_string(m^"Un sanglier vous à tuer pendant la nuit \n");raise (Fin_partie "Vous etes mort")
    |Nue_de_Moustique ->print_string(m^"Une nuee de moustiques vous à tuer pendant la nuit \n");raise (Fin_partie "Vous etes mort")

(** Elle ajoute au sac du personnage les objets du monstre qui a été dans le combat, elle prend en paramètre le sac du personnage,
l'objet recupéré au monstre et la quantité qu'il l'a *)
let rec ajout_sac: sac -> objet->int-> sac = fun sac obj qtt ->
  match sac with
  |[]->[(obj,qtt)]
  |(i,j)::t -> if i=obj then (obj,j+qtt)::t else (i,j)::(ajout_sac t obj qtt)

(** Elle prend en paramètre un personnage et un monstre et vide retourne le personnage lui ajoutant au sac de celui-ci l'objet recuperé au mosntre *)
let modif_sac_mons : monstre->personnage->personnage = fun m p->
  match m.objet_du_monstre with
  | Piece -> {
      nom = p.nom; classe = p.classe; genre = p.genre; nb_exp = p.nb_exp;
      nb_vie = p.nb_vie; niveau = p.niveau; sac_personnage = ajout_sac p.sac_personnage Piece 1
    }
  | Cuisse -> {
      nom = p.nom; classe = p.classe; genre = p.genre; nb_exp = p.nb_exp;
      nb_vie = p.nb_vie; niveau = p.niveau; sac_personnage = ajout_sac p.sac_personnage Cuisse 1
    }
  | Eponge -> {
      nom = p.nom; classe = p.classe; genre = p.genre; nb_exp = p.nb_exp;
      nb_vie = p.nb_vie; niveau = p.niveau; sac_personnage = ajout_sac p.sac_personnage Eponge 1
    }

(** Elle calcule 2 puissance n pour ajouter l'experience du personnage en fonction du niveau n qu'il a atteint *)
let rec pow : int -> int -> int = fun x y ->
  if y<=0 then 1
  else x*(pow x (y-1))

(** Elle affiche un message pour dire au personnage l'experience qu'il a gagné *)
let afficher_exp=fun x -> 
  "Vous gagnez "^string_of_int(x)^" points d'experience\n"

(** Elle modifie l'experience du personnage a chauqe niveau que celui atteint *)
let modif_exp_level:int->personnage->personnage*unit = fun x p ->
   print_string(afficher_exp(x)) ;
   let i= p.nb_exp+x in 
   if i=(pow 2 p.niveau)+10 then let j =p.niveau+1  in 
    if j>10 then
      raise (Fin_partie "Vous atteint le niveau 10\n")
    else {
        nom = p.nom; classe = p.classe; genre = p.genre; nb_exp = i;
        nb_vie = p.nb_vie; niveau = j; sac_personnage = p.sac_personnage},
        print_string("Vous progressez au niveau "^string_of_int(j)^"\n")
   else {
    nom = p.nom; classe = p.classe; genre = p.genre; nb_exp = i;
    nb_vie = p.nb_vie; niveau = p.niveau; sac_personnage = p.sac_personnage
  },()

(** Elle lance un combat entre un personnage et un monstre, d'abord le personnage frappe le monstre et ensuite le monstre riposte, il retourne l'etat du personnage *)
let rec combattre : personnage-> monstre -> (unit*personnage)*monstre= fun p m ->
  let mons = fst(frapper p m) in 
  if mons.pt_de_vie <= 0 then let p1 = modif_sac_mons m p and m=print_string("Vous avez terrasser le monstre \n")in
    match mons.genre_monstre with
    | Golem -> (m,fst(modif_exp_level 8 p1)),mons
    | Nue_de_Moustique -> (m,fst(modif_exp_level 2 p1)),mons
    | Sanglier -> (m,fst(modif_exp_level 4 p1)),mons
      
  else 
    let p2=(monstre_frapper m p) in print_string("Le monstre vous a attaquer\n");
    if p2.nb_vie <= 0 then raise (Fin_partie "Vous etes mort")
    else ((),p2),mons

(** On tranforme le monstre en chaine de caractere pour pouvoir l'utiliser dans les affichages *)
let print_monstre = fun m->
  match m.genre_monstre with
  | Golem ->"un golem\n"
  | Nue_de_Moustique ->"une nuee de moustiques\n"
  | Sanglier ->"un sanglier\n"

(*Get_from_user*)

let rec generic = fun read message ->
print_string message;
try
  read()
with
  Failure _ -> generic read message

let int : string -> int =
generic read_int

let float : string -> float =
generic read_float

let string: string-> string =
generic read_line 

let message_nom = "Veuillez entrer un nom valide\n"

let message_genre="Veuillez saisir M pour masculin ou F pour feminin\n"

let message_classe="Veuillez saisir une classe : Magicien | Archer | Guerrier\n"

let message_combat= 
"Que faites-vous ? \n"^
"(A) Attaquer \n"^
"(F) Fuir \n"^
"(V) Visualiser l'etat de votre personnage\n"

let message_aventure=
"Que faites-vous ? \n"^
"(C) Continuer votre chemin \n"^
"(D) Dormir \n"^
"(M) Manger \n"^
"(V) Visualiser l'etat de votre personnage \n"^
"(Q) Quitter l'aventure\n"

let rec genre = fun ()->
  let g=string message_genre in
  match g with
  |"M"->"Masculin"
  |"F"->"Feminin"
  |_->genre()

let rec classe=fun ()->
  let c=string message_classe in
  match c with
  |"Magicien"->Magicien
  |"Guerrier"->Guerrier
  |"Archer"->Archer
  |_->classe()    


(** On cree un personnage par le menu pour le demerrage du cours *)
let cree_personnage: unit -> personnage = fun ()->
  let nomP=string message_nom and
  genreP=genre() and
  classeP=classe() in
  {nom=nomP;classe=classeP;genre=genreP;nb_vie=20;nb_exp=0;niveau=1;sac_personnage=[(Cuisse,0);(Eponge,0);(Piece,0)]}

(** Elle modifie le sac du personnage qu'il fuit l'adversaire *)
let rec aux_modif_sac_fuir : sac -> objet -> int -> sac=fun s o x ->
  match s with
  | [] -> []
  | hd::tl -> if fst hd =o then
    (fst hd, (snd hd)-x)::tl
  else 
    hd::aux_modif_sac_fuir tl o x
  ;;
let modif_sac_fuir:personnage->objet->int->personnage=fun p o x->
  {
    nom = p.nom; classe = p.classe; genre = p.genre; nb_exp = p.nb_exp;
    nb_vie = p.nb_vie; niveau = p.niveau; sac_personnage = aux_modif_sac_fuir p.sac_personnage o x
    }

(** Elle tranforme les objets en chaine de caractere pour les concatener facilement dans les affichages des messages *)
let string_of_objet=fun o ->
  match o with
  |Eponge->"eponge"
  |Piece->"piece"
  |Cuisse->"poulet"


(** Le personnage fui l'adversiare, il prend en parametre un personnage et renvoie celui-ci avec un message indiquand l'objet qu'il aurait perdu en cours de route *)
let fuir:personnage->personnage*string=fun p->
  let n = Random.int 3 in
  let o = List.nth p.sac_personnage n in
    let n=snd(o) in
    if n>0 then
      let x=Random.int n in
      modif_sac_fuir p (fst o) n,"Dans votre précipitation à fuir vous perdez  "^string_of_int(x)^" "^string_of_objet (fst o)^"\n"
    else
      p,"Votre sac est vide, vous ne perdez rien\n" 

(** Elle appelle le personnage a faire un choix de combat en lui proposant un menu de comabt *)
let choixCombat:unit->string=fun ()->
  string message_combat
      
(*let malheureuse_rencontre:personnage->(unit*personnage)*unit = fun personnage ->
  let monstre=genrer_monstre() in
  print_string("Soudain, vous tombez nez à nez avec "^print_monstre(monstre)^"Ils vous attaquent !\n");
  choix_aventure p m
*)  

(** Elle appelle le personnage a faire un choix d'aventure quand il lance le jeu en lui proposant un menu d'aventure *)
let choixAventurePersonnage :unit->string=fun ()->
  string message_aventure

(** Elle propose au personnage a base d'un menu de faire un choix d'aventure et applique ce choix en appelant la focntion appropriee, les deux fonctions choix_aventure
et combat s'appelle mutuellement, par consequent leur definition lié par and *)
let rec choix_aventure:personnage->monstre->(unit*personnage)*unit=fun p m->
  let choixA=choixAventurePersonnage() in
    match choixA with
    |"C"->let malheureuse_rencontre:personnage->(unit*personnage)*unit = fun personnage ->
      let monstre=genrer_monstre() in
      print_string("Soudain, vous tombez nez à nez avec "^print_monstre(monstre)^"Ils vous attaquent !\n");
      choix_combat p monstre;in malheureuse_rencontre p
    |"D"->let p1=fst(dormir p) in ((),p),dormir p ;choix_aventure p1 m
    |"M"->let mange=manger p and p2=snd(snd(manger p)) in ((),p),string_manger(mange);choix_aventure p2 m
    |"V"->((),p),print_string(etat_personnage p);choix_aventure p m
    |"Q"->raise (Fin_partie "Vous avez quitté l'aventure")
    |_-> choix_aventure p m    

and choix_combat:personnage->monstre->(unit*personnage)*unit= fun p m->
  let choixC=choixCombat() in
    match choixC with
    |"A"->let combat=combattre p m in let p1=snd(fst(combat)) and m1=snd(combat) in 
    if m1.pt_de_vie>0 then
       choix_combat p1 m1
    else choix_aventure p1 m1
    |"V"->((),p),print_string(etat_personnage p);choix_combat p m
    |"F"->let p1=fst(fuir p) in ((),fst(fuir p)),print_string(snd(fuir p));choix_aventure p1 m
    |_->choix_combat p m;
;;

(** Elle retourne le monstre en chaine de caractere pour pouvoir le concatener facilement dans les affichages *)
let string_of_monstre:monstre->string=fun m->
  match m.genre_monstre with
  |Golem -> "un golem"
  |Sanglier ->"un sanglier"
  |Nue_de_Moustique -> "une nuée de moustiques"

(** Elle annonce le debut de l'appelle de fonction combattre dans lequel elle annonce l'arrive d'un monstre *)
let rencontre_monstre:monstre->string =fun m ->
  "Vous entendez du bruit à l’orée de la forêt et vous apercevez "^string_of_monstre(m)^"\n"

(** La fonction principale qui lance le jeu  *)
let main : unit-> unit = fun ()->
  let p = cree_personnage() and
  m =genrer_monstre() 
in
  print_string(rencontre_monstre m) ;
  snd(choix_combat p m);
  
;;
main()
