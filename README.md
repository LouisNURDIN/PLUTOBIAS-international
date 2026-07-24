Readme projet PLUTOBIAS International

00 - Préparation des bases
=> ATTENTION : Ce fichier est très lourd et long à exécuter dans son entièreté. Il faut attendre longtemps avant d'arriver au bout. 

=> Il s'agit du fichier de début dans lequel on traite les données de nos bases de données avec les enquêtes d'opinion;
=> La base "all_elections" répertorie toutes les élections législatives et/ou présidentielles des pays que l'on couvre. 
    => Pour les régimes présidentiels, la colonne "élection_présidentielle" sert à répertorier si il s'agit d'une élection présidentielle ou non. Pour ces pays-là, seules les élections présidentielles doivent être prises en compte pour le moment. 

=> On commence par identifier et renommer  les variables qui nous intéressent : 

        => isoname = nom du pays ; 
        => year = année de l'enquête, si possible utiliser l'information sur la date de l'interview; 
        => election_date = date de l'élection associée à l'enquête (pas toujours disponible)
          => si non disponible on la calcule ultérieurement avec une fonction qui rattache chaque répondant à l'élection correspondante, en fonction de la date de l'enquête, du type de sondage et de la date des                       élections du pays du répondant
        => interview_date = date de réalisation de l'enquête (pas toujours disponible) 
        => dataset_party_id = parti voté à l'élection, avec l'id enregistré dans la base; 
        => survey = type de sondage (pré ou post-électoral)
        => source = si il s'agit d'une base avec une enquête pour chaque pays/année, alors on renomme source avec le nom de la base (exemple : source = CSES)
          => si il existe différentes enquêtes pour un même pays/année, on garde bien le nom de l'enquête à chaque fois. Cela arrive par exemple dans WPID ou ESS, où l'on peut avoir différentes enquêtes ou vagues qui                 couvrent un même pays/année
        => source_recode = variable que l'on crée et où l'on inscrit le nom de la source des données : WPID, CSES, ESS... Cela nous permet de savoir de quel fichier viennent exactement les données 
        => type = type d'élection; on garde les élections présidentielles pour les régimes présidentiels (de préférence le 1er tour de l'élection)
        => turnout = si la personne a voté ou non lors de l'élection
        => inc, educ, gender, age,
        => weight

=> Traitement de la variable turnout : on fait attention à enlever les personnes qui ont NA ou toutes réponses invalides dans turnout, car on ne peut pas savoir si ils ont voté ou non 
    => Bien lire les codebook de chaque source pour savoir à quoi correspondent les modalités de réponse dans Turnout
    => Les personnes qui n'ont pas voté, ont voté blanc ou nul sont associés à "Abstention" dans dataset_party_id et partyfacts_id
    => Ces informations sont contenues soient dans "turnout", soit dans "dataset_party_id" en fonction des bases
    
=> Pour chaque base dans laquelle on dispose d'une date d'interview valide, une fonction existe pour raccorder chaque répondant à la bonne élection. Cette fonction prend en compte le type de sondage dont il s'agit (si pré-électoral on rattache à la prochaine élection, sinon on rattache à a dernière élection). La liste de toutes les élections par pays est contenue dans le df "all_elections". 

=> Pour chaque base, on rattache dataset_party_id au Partyfacts_id correspondant. Dans Partyfacts, il faut bien sélectionner la bonne base de données pour avoir les ids correspondants à la base traitée. 

=> Lorsque chaque base est traitée, on doit obtenir une base avec 16 variables, organisée comme ceux-ci :
=> isoname, interview_date, source, source_recode, survey, type, inc, gender, educ, age, turnout, dataset_party_id, weight, election_date, election_year.  

=> A la fin du fichier, on empile toutes nos bases avec "bind_rows" pour avoir une unique base qui les agrège toutes ensembles
    => Cela nous donne "Base_all_sources"
=> Enfin, un filtre avec case_when existe à la fin du fichier afin de corriger tous les id partyfacts qui se joinent mal entre nos différentes bases
      => Lorsque l'on veut changer un id partyfacts erroné, il faut le reporter à cet endroit-là du code. 


01 - Calcul fonction déciles
=> Il s'agit du fichier dans lequel on calcule le vote aux élections au sein de chacune de nos catégories (inc, homme/femme top/bot educ, top/bot age)
=> Pour les déciles de revenu, notre fonction répartit les répondants en dix catégories de taille égale; pour les autres catégories, les répondants sont répartis en deux groupes : top 50% et bot 50%
exemple pour l'âge : 50% plus âgés VS 50%plus jeunes
=> Nos fonctions prennent en compte à la fois le "weight" présent dans les bases, mais également le nouveau weight crée avec la fonction, qui recalcule le poids de chaque répondant au sein des nouvelles catégories créés, 
nous reprenons ici la méthode utilisée par Gethin. 
=> Nous avons à chaque fois un df spécifique pour chaque catégorie au sein de chaque base : par exemple, ESS revenus, ESS âge, ESS educ...
=> Une fois la fonction exécutée, on crée dans chaque df une variable "bias", qui indique de quel clivage il s'agit : revenu = "plutocracy" ; genre = "androcracy"; educ = "epistocracy"; âge = "gerontocracy"

=> Une fois tous nos calculs du vote effectués, nous empilons toutes nos df ensemble avec la commande "bind_rows". Les variables "source", "source_recode", "bias" et "category" nous permettent de situer dans quelle base 
nous sommes et de quel clivage il s'agit; la base finale obtenue s'apelle "Base_all_clivages"

02 - Base Parlement
=> Il s'agit du fichier dans lequel on va fusionner en une base les données sur les compositions des parlements après les élections avec les données sur le vote
=> Notre principal base de données est la base "Elections Globals", qui contient des données sur les Parlements pour toutes les élections jusqu'à 2015; pour les élections post-2015, nous rajoutons les données de Parlgov
pour les pays couverts par Parlgov. 
=> On commence par traiter et filtrer la base Elections Globals (renommer les variables, modifier les données erronnées, rajouter à chaque fois une ligne pour l'abstention...)
=> Le join entre les deux bases se fait avec les variables "election_date" et "election_year" 
=> La base obtenue s'appelle "base_vote_parlement_global"

=> Le df "manquants", crée vers la fin du fichier, permet de lister pour chaque pays/élection, les partis qui ont eu au moins 10% des sièges au parlement, référencés dans Elections Globals mais pas dans notre base sur le*
vote. Cela nous permet d'identifier les partis qui joinent mal entre les bases et de pouvoir les corriger, lorsque c'est possible. 
=> On rajoute à la fin les données de la base Parline. Nous utilisons Parline uniquement pour avoir les informations sur le taux de femmes députées par parlement

03 - Base Gouvernement 
=> Il s'agit du fichier dans lequel on fusionne les données sur les gouvernements avec notre base sur le vote et la composition des parlements
=> Notre source de données pour les gouvernements est la base whogov. 
=> On commence par créer une base "whogov_parties", dans laquelle on compile pour chaque pays/années le nombre de ministres par partis politiques. 
=> On obtient une base avec 1 ligne = 1 pays, 1 année, 1 parti politique, le nombre de ministres du parti, le taux de ministres du parti au sein du gouvernement... 
=> Avant de fusionner Whogov avec "base_vote_parlement_global", on rajoute les années manquantes dans notre base, c'est-à-dire les années où il n'y a pas eu d'élection. Pour cela, on duplique les données sur le vote
et sur le parlement de la dernière élection, et on les répète pour les années suivantes où il n'y a pas eu d'élection 
  => exemple : pour la France, il y a eu des élections en 2002 et 2007. Avec cette commande, on va rajouter les années 2003, 2004, 2005 et 2006 dans notre base, auquel on va répliquer les données du vote et du parlement 
               de l'élection 2002. Ainsi, nous pourrons fusionner les données whogov pour ces années là dans notre base complète. 
=> Pour le join de whogov avec la base vote-parlement, on crée une variable "join_year_whogov", qui permet de dire pour les années où il y a eu une élection, à quelle année les données whogov doivent être rattachées : 
comme whogov indique la composition du gouvernement au mois de Juillet de chaque année, si l'élection a eu lieu après cette date, alors on rattache les données whogov de l'année correspondante à celles de l'élection correspondante.
En revanche, si l'élection a eu lieu avant Juillet, on peut rattacher les données du gouvernement de cette année à cette élection-là. 

=> Nous obtenons à la fin notre df "Base_complète", qui contient les données sur le vote + parlement + gouvernement, pour chaque pays/année. 
=> On garde bien à chaque fois la distinction entre chaque "source", "source_recode", "bias" et "category", pour ne pas les mélanger. 

04 - Plutocracy index (peut-être changer le nom du fichier)
=> Il s'agit du fichier dans lequel nous allons calculer tous nos indices (représentation de chaque groupe au gouvernement, différence de participation, votes => sièges, sièges => ministres, votes => ministres dans le cas
des régimes présidentiels). 
=> On commence par recréer deux variables : category_recode1 et category_recode2
  => Pour les données sur le genre, l'éducation et l'âge, ces données sont identiques. Il s'agit simplement de renommer les "catégories" commençant par "top" en "top", et celles avec "bot" en "bot". 
  => La seule différence se fait pour la ploutocratie. Dans catégory_recode_1, on renomme le premier en décile en "bot", et le dernier décile en "top". Les autres déciles renvoient NA. Dans category_recode2, on renomme les
  cinq déciles les plus pauvres en "bot" et les cinq déciles les plus riches en "top". 
  




