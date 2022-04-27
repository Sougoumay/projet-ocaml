# Devoir de programmation Fonctionnelle

## Première Partie ( Répartition du travail)

Tous les membre du groupe ont travailler sur tous les exercices du devoir. Ce choix à été adopter pour permettre à tous les membres de pouvoir touché à tous,   de mieux comprendre ce langage mais aussi de réviser par la meme occasion pour l'examens de fin de semestre. On se donner régulièrement rdv à la bibliothèque pour travailler ensemble.

## Deuxième Partie( Mode d'emploi du Jeu )
On lance avec la commande ocamlrun main.exe si le fichier n'existe pas on fait ocamlc main.ml -o main.exe  
Le jeux commence par la création du personnage:nom(string),genre(M ou F),classe(Archer ou Guerrier ou Magicien).  
Puis on fait malheureuse_rencontre qui génère aléatoirement un monstre et 

Le Personnage doit faire un choix(choix_combat):
                                                (A) Attaquer
                                                (F) Fuir
                                                (V) Visualiser l’´etat de votre personnage
    Si A, on fait combatre le personnage et le monstre jusqu'a que l'un des deux meurt,
    apres que le personnage attaque  le monstre ce dernier riposte, la fonction choix_combat est appelée,
    si le personnage meurt le jeu s'arrete en levant un exception et si c'est le monstre qui meurt on appel la fonction choix_aventure.
    Si F, le personnage perd un objet et on appelle directement choix_aventure.

Ensuite le personnage doit faire un choix(choix_aventure):
                                                        (C) Continuer votre chemin
                                                        (D) Dormir
                                                        (M) Manger
                                                        (V) Visualiser l’´etat de votre personnage
                                                        (Q) Quitter l’aventure
    Si C, on refait malheureuse_rencontre et on appelle choix_combat
	Si D, le personnage dors et gagne des points de vie.
	Si M, le personnage mange et gagne des points de vie.
	Si V, on visualise les attributs du personnage.
	

Le jeu se termine si on tape sur la touche Q, si le personnage atteint le niveau 10 ou si il meurt.
         