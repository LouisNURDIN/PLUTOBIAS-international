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

=> Il s'agit du fichier dans lequel on calcule le vote aux élections au sein de chacune de nos catégories en fonction de nos variables socio-démo : income, gender, educ, age. 

=> Pour chacune de nos sources et de nos clivages, à l'exception du genre on a deux fonctions différentes : l'une pour calculer le vote top 10 / bottom 10 et l'autre pour calculer top 50 / bottom 50
        => Pour le genre, on dispose d'une fonction par source. 
        => Cela nous donne 7 fonctions pour chaque source de données (wpid, cses, ess...)
        => par exemple : income 50, income 10, gender, educ 50, educ 10, age 50; age 10

=> Avant chaque fonction, on indique la source à partir de laquelle on va travailler : 
    => Par exemple, si il s'agit de wpid, on va filtrer en demandant de ne garder que les lignes qui ont source_recode == WPID, dans "Base_all_sources". 
    => On filtre ensuite notre df pour ne garder que les lignes valides en fonction de la variable socio-étudiée
        => exemple : filtrer les données de la variable "inc" lorsqu'il s'agit des fonctions income; même chose pour les fonctions gender, educ et age. 

=> Nos fonctions reprennent la méthode utilisée par Gethin. Cela consiste à répartir nos répondants dans de nouvelles catégories que l'on crée artificiellement (2 pour les fonctions 50/50 ; 10 pour les fonctions 10/10)
=> Les répondants sont répartis dans ces catégories en fonction de leur position dans la distribution initiale dans l'enquête. 
    => Par exemple, si un répondant fait partie des 10% les plus âgés de l'enquête, il sera assigné au groupe "top 50%" et "top 10%"
    => Une personne appartenant aux 20% les moins diplômés de l'enquête sera assignée au groupe "bot 50% income". 

=> Les fonctions 10/10 ne gardent à la fin que les répondants appartenant au top 10% et au bottom 10%. 

=> Après le calcul de chaque fonction, on crée les variables "bias" et "category"
    => "category" où l'on indique à quel groupe la ligne fait référence : bot-50, top50, bot10, top10
    => la variable "bias" pour indiquer de quel clivage il s'agit : plutocracy, androcracy, epistocratie, gerontocratie. 

=> Ainsi, les variables "bias" et "category" indiquent à quel clivage et à quel groupe socio-démographique la ligne fait référence
    => Exemple : Bias = "plutocracy" et category = "top-income-10" => la ligne fait référence aux 10% les plus riches de l'enquête

=> Pour une même source/clivage, on empile les bases 50/50 et 10/10 pour les avoir dans une même base 
    => Exemple : Base_wpid_income = bind_rows(wpid_inc_50, wpid_inc_10)

=> A la fin du fichier, on agrège finalement toutes les bases crées avec nos fonctions en une seule base : "Base_all_clivages"
    => Son format est le suivant : une ligne = un pays, une élection (avec l'année et la date si possible de l'élection), un clivage (bias), un groupe (category), le parti dont il est question (partyfacts_id) et les informations sur le vote du groupe pour ce parti
    => Pour les informations sur le vote d'un groupe pour un parti, la variable qui compte est "pct_votes". Il s'agit du pourcentage que l'on a calculé avec nos fonctions et c'est celui qu'on utilisera pour calculer nos indices plus tard. 
    => Normalement, au sein de chaque isoname/source/source_recode/election_year/election_date/bias/category,  pct_votes doit être égale à 100. Une commande existe pour vérifier cela. 

=> On exporte "Base_all_clivages". 

    

02 - Base Parlement

=> Il s'agit du fichier dans lequel on va fusionner en une base les données sur les compositions des parlements après les élections avec les données sur le vote

=> Notre principal base de données est la base "Elections Globals", qui contient des données sur les Parlements pour toutes les élections jusqu'à 2015;
    => pour les élections post-2015, nous rajoutons les données de Parlgov
    => Cependant Parlgov ne couvre pas autant de pays que Elections_Globals, il y a donc des pays pour lesquels nous ne disposons pas de données sur les élections après 2015.  
    
=> On commence par traiter et filtrer la base Elections Globals (renommer les variables, modifier les données erronnées, rajouter à chaque fois une ligne pour l'abstention...)
=> Le join entre les deux bases se fait avec les variables "election_date" et "election_year" 
=> La base obtenue s'appelle "base_vote_parlement_global"

=> Le df "manquants", crée vers la fin du fichier, permet de lister pour chaque pays/élection, les partis qui ont eu au moins 10% des sièges au parlement, référencés dans Elections Globals mais pas dans notre base sur le vote. Cela nous permet d'identifier les partis qui joinent mal entre les bases et de pouvoir les corriger, lorsque c'est possible. 
    => Les modifications se font à la fin du tout premier fichier (00 - Préparation des bases)

=> On crée la variable "election_couverture_seats" qui indique pour chaque pays/élection le nombre de députés couverts par les partis dans notre base. 
    => Concrètement, cela permet de connaître les pays/élections où notre base n'arrive pas à bien couvrir les députés.
    => Cela peut-être liée à un problème de join des ids partyfacts entre les bases ou bien d'une absence de certains partis dans l'une de nos bases. 
    => La variable "Other_seats" sert à lister le taux de députés appartenant aux partis référencés comme "Other" dans Elections Globals; Grâce à cette commande on peut savoir si le manque de députés couverts est lié aux données des bases (les partis référencés comme "Other" ne peuvent pas être rattachés à un autre parti donc on doit les laisser de côté), ou si le problème se situe ailleurs. 
    
=> On rajoute à la fin les données de la base Parline, pour les données sur le taux de femmes députées dans chaque parlement (cette partie est facultative). 

=> On exporte "Base_vote_parlement_global". 



03 - Base Gouvernement 

=> Il s'agit du fichier dans lequel on fusionne les données sur les gouvernements avec notre base sur le vote et la composition des parlements. 

=> Notre source de données pour les gouvernements est la base whogov. Les données de whogov commencent à partir de 1966, c'est pourquoi nous filtrons la base pour enlever les années avant 1966. 

=> On commence par créer une base "whogov_parties", dans laquelle on compile pour chaque pays/années le nombre de ministres par partis politiques. 
    => On obtient une base avec 1 ligne = 1 pays, 1 année, 1 parti politique, le nombre de ministres du parti, le taux de ministres du parti au sein du gouvernement... 
    
=> Avant de fusionner Whogov dans notre base, on rajoute les années manquantes dans notre base, c'est-à-dire les années où il n'y a pas eu d'élection. Pour cela, on duplique les données sur le vote
   et sur le parlement de la dernière élection à l'aide d'une fonction, et on les répète pour les années suivantes où il n'y a pas eu d'élection.
  => exemple : pour la France, il y a eu des élections en 2002 et 2007. Avec cette commande, on va rajouter les années 2003, 2004, 2005 et 2006 dans notre base, auquel on va répliquer les données du vote et du parlement 
               de l'élection 2002. Ainsi, nous pourrons fusionner les données whogov pour ces années là dans notre base complète. 
               
=> Pour le join de whogov avec la base vote-parlement, on crée une variable "join_year_whogov", qui permet de dire pour les années où il y a eu une élection, à quelle année les données whogov doivent être rattachées : 
comme whogov indique la composition du gouvernement au mois de Juillet de chaque année, si l'élection a eu lieu après cette date, alors on rattache les données de l'élection à celles de l'année whogov suivante. 
    => En revanche, si l'élection a eu lieu avant Juillet, on peut rattacher les données du gouvernement de cette année à cette élection-là. 

 => On crée la variable "election_couverture_ministers" qui indique pour chaque pays/élection le nombre de ministres couverts par les partis dans notre base. 
    => Concrètement, cela permet de connaître les pays/élections où notre base n'arrive pas à bien couvrir les députés.
    => Cela peut-être liée à un problème de join des ids partyfacts entre les bases ou bien d'une absence de certains partis dans l'une de nos bases. 
    => La variable "Other_seats" sert à lister le taux de députés appartenant aux partis référencés comme "Other" dans Elections Globals; Grâce à cette commande on peut savoir si le manque de députés couverts est lié aux données des bases (les partis référencés comme "Other" ne peuvent pas être rattachés à un autre parti donc on doit les laisser de côté), ou si le problème se situe ailleurs. 

=> Les variables "election_couverture_seats" et "election_couverture_ministers" nous serviront plus tard à connaître les pays/années où l'on couvre suffisament de députés/ministres pour pouvoir les inclure dans nos analyses. Le seuil fixé est actuellement de 75% de députés et 75% de ministres couverts. En-dessous de ce seuil, les pays/années concernées ne seront pas inclus dans nos analyses. 
    => Ce seuil est modifiable. 

=> Nous obtenons à la fin notre df "Base_complète", qui contient les données sur le vote + parlement + gouvernement, pour chaque pays/année. 
    => Une ligne = source, source_recode,isoname,year,bias,category,un parti, votes du groupe pour le parti, députées du parti au parlement, ministres du parti au gouvernement
    => toutes les autres variables (type, election_date,election_year, survey, election_couverture_seats, election_couverture_ministers) sont aussi conservées. 


    

04 - Plutocracy index (peut-être changer le nom du fichier)

=> Il s'agit du fichier dans lequel nous allons calculer tous nos indices (représentation de chaque groupe au gouvernement, différence de participation, votes => sièges, sièges => ministres, votes => ministres dans le cas
des régimes présidentiels). 
=> On commence par recréer deux variables : category_recode1 et category_recode2
  => category_recode1 s'applique uniquements aux groupes "top-10" et "bot-10", ainsi que "men" et "women"
  => category_recode2 s'applique uniquements aux groupes "top-50" et "bot-50", ainsi que "men" et "women"

=> Pour nos catégories 10/10 on calcule des indices qui finissent par "top_bot", à partir de category_recode1
=> Pour nos catégories 50/50 on calcule des indices qui finissent par "top_bot2", à partir de category_recode2

=> Détail des ratios/indices pour les régimes parlementaires
    => ratio_participation = différence de participation entre le groupe top et bot
    => ratio_votes_valides_en_sieges = indice intermédiaire de la conversion des votes en sièges
    => ratio_sieges_en_ministres = indice intermédiaire de la conversion des sièges en ministres ; n'existe pas pour les régimes présidentiels
    => ratio_gouvernement = indice final de représentation au gouvernement
        => ce ratio peut se calculer de deux manières :
        => 1) ratio entre le nombre total de ministres obtenus par le groupe top et le groupe bot
        => 2) en faisant : ratio_participation * ratio_votes_valides_en_sieges * ratio_votes_valides_en_ministres
            => la variable "verif_ratio" effectue cette multiplication et sert à vérifier que le calcul de notre indice final est juste. La corrélation entre vérif_ratio et ratio_gouvernement doit être égal à 1
    => ratio_sieges = indice de représentation au parlement; cette indice est facultatif; il correspond à : ratio_participation * ratio_votes_valides_en_sieges

=> Détail des ratios/indices pour les régimes présidentiels
    => ratio_participation = différence de participation entre le groupe top et bot
    => ratio_votes_valides_en_ministres = indice intermédiaire de la conversion des votes en ministres; n'existe que pour les régimes présidentiels
    => ratio_gouvernement = indice final de représentation au gouvernement
        => ratio_gouvernement =  ratio_participation * ratio_votes_valides_en_ministres
            => la variable "verif_ratio_presidentiel" effectue cette multiplication et sert à vérifier que le calcul de notre indice final est juste. La corrélation entre vérif_ratio et ratio_gouvernement doit être égal à 1

=> Chacun de ces indices existe sous la forme "top_bot" (10/10) et "top_bot2 (50/50)

=> On obtient la base "Base_complete_clean" avec nos ratios calculés pour chaque source/source_recode/isoname/year/bias/category/partyfacts
=> Comme nous sommes encore au format : une ligne = une élection, un groupe, un parti, nos ratios sont dupliqués pour toutes les liges appartenant à la même combinaison de source/source_recode/isoname/year/bias
=> Afin d'arriver à une base au format : une ligne = une source, un pays, une année, un biais + valeur des ratios, on crée les bases "Base_complete_legislative_index" et "Base_regimes_presidentiels_index", qui ramènent tout sur une ligne. 

=> On exporte "Base_complete_legislative_index" et "Base_regimes_presidentiels_index"


05 - Analyses 

=> Il s'agit du fichier dans lequel on va tracer les graphiques et réaliser tous les tests/analyses que l'on souhaite

=> On commence par travailler sur "Base_complete_legislative_index"

=> On établit une hiérarchie des sources pour les pays/années où différentes sources existent 
=> Pour faire cela, on attribue un score à chaque source comme suit, à partir du type de survey(type) et du type d'élection (type) : 
    => Survey = post-électoral et type = parlementaire/présidentiel =  1  (CSES, WPID)
    => Survey = post-électorl et type = general election = 2 (ESS)
    => Survey = pré-électoral et type = parlementaire/présidentiel = 3 (WPID)
    => Survey = pré-électoral et type = general election = 4 (WVS)

A partir de cela, on commence par créer "Base_complete_legislative_grosses_sources"
    => cette base calcule pour chaque pays/année la meilleure source au sein de chaque "source_recode"
    => Si il reste plusieurs sources au sein d'un source_recode, alors on calcule une moyenne géométrique de nos ratios 
    => Exemple : si pour un pays/année, on a deux sources de WPID, une source de CSES et une source de WVS, on garde la source de CSES et de WVS, et on calcule une moyenne géométrique des ratios de nos deux sources de WPID, pour ne garder à la fin qu'une seule ligne 
    => Cette base va nous servir pour tracer des heatmap de corrélation de nos indices entre chaque source_recode, qui ont des données pour des pays/années en commun. 

=> On crée ensuite "Base_complete_legislative_best_sources"
    => L'objectif est de garder pour chaque pays/année une seule source et une seule valeur pour chaque ratio
    => Grâce au score attribué à chaque source, on filtre la base pour ne garder pour chaque pays/année la source avec le score le plus bas (= la meilleure source)
    => Si pour un pays/année plusieurs sources ont le même score et sont les meilleurs sources, alors on calcule une moyenne géométrique de nos ratios sur ces sources-là pour ce pays/année
    => Pour un pays/année, j'ai une source de  ESS, et une de WVS ; la meilleure source est EES = on garde uniquement la ligne avec la source ESS
    => Si j'ai plusieurs sources dans ESS pour ce même pays/année (car plusieurs vagues pour le même pays/année), alors je calcule une moyenne géométrique de mes ratios à partir de mes sources qui existent dans ESS pour ce pays/année là. 


=> Box-plot

=> Graphique évolution des indices par pays depuis 1980

=> Graphique en barres moyenne géométrique des indices depuis 2000
