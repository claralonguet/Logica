% Prédicat pour évaluer les expressions arithmétiques
eval(Expr, _, Expr) :-
    number(Expr). % Si l'expression est un nombre, retourne directement sa valeur

eval(Var, Etat, Valeur) :-
    atom(Var), % Si l'expression est une variable
    member((Var, Valeur), Etat). % Recherche sa valeur dans l'état

eval(Expr, Etat, Valeur) :-
    Expr =.. [Op, Gauche, Droite], % Si l'expression est une expression composée
    member(Op, [+, -, *, /, **, mod]), % Vérifie si Op est un opérateur arithmétique
    eval(Gauche, Etat, ValeurGauche), % Évalue le côté gauche
    eval(Droite, Etat, ValeurDroite), % Évalue le côté droit
    (   Op = mod ->
        Valeur is ValeurGauche mod ValeurDroite % Évaluation de l'opérateur modulo
    ;   ExprEval =.. [Op, ValeurGauche, ValeurDroite], 
        Valeur is ExprEval
    ).

eval(Expr, Etat, true) :-
    Expr =.. [>=, Gauche, Droite], % Si l'expression est une expression de comparaison >=
    eval(Gauche, Etat, ValeurGauche), % Évalue le côté gauche
    eval(Droite, Etat, ValeurDroite), % Évalue le côté droit
    ValeurGauche >= ValeurDroite.

eval(Expr, Etat, true) :-
    Expr =.. [=<, Gauche, Droite], % Si l'expression est une expression de comparaison =<
    eval(Gauche, Etat, ValeurGauche), % Évalue le côté gauche
    eval(Droite, Etat, ValeurDroite), % Évalue le côté droit
    ValeurGauche =< ValeurDroite.

eval(Expr, Etat, true) :-
    Expr =.. [<, Gauche, Droite], % Si l'expression est une expression de comparaison <
    eval(Gauche, Etat, ValeurGauche), % Évalue le côté gauche
    eval(Droite, Etat, ValeurDroite), % Évalue le côté droit
    ValeurGauche < ValeurDroite.

eval(Expr, Etat, true) :-
    Expr =.. [>, Gauche, Droite], % Si l'expression est une expression de comparaison >
    eval(Gauche, Etat, ValeurGauche), % Évalue le côté gauche
    eval(Droite, Etat, ValeurDroite), % Évalue le côté droit
    ValeurGauche > ValeurDroite.

eval(Expr, Etat, true) :-
    Expr =.. [=, Gauche, Droite], % Si l'expression est une expression de comparaison =
    eval(Gauche, Etat, ValeurGauche), % Évalue le côté gauche
    eval(Droite, Etat, ValeurDroite), % Évalue le côté droit
    ValeurGauche =:= ValeurDroite.

eval(Expr, Etat, true) :-
    Expr =.. [=\=, Gauche, Droite], % Si l'expression est une expression de comparaison =\=
    eval(Gauche, Etat, ValeurGauche), % Évalue le côté gauche
    eval(Droite, Etat, ValeurDroite), % Évalue le côté droit
    ValeurGauche =\= ValeurDroite.

% Prédicat pour mettre à jour l'état du programme après une assignation de variable
update_state(Var, Val, [], [(Var, Val)]).
update_state(Var, Val, [(Var, _)|T], [(Var, Val)|T]).
update_state(Var, Val, [(X, V)|T], [(X, V)|NouvelleT]) :-
    Var \= X,
    update_state(Var, Val, T, NouvelleT).

% Prédicat pour exécuter les instructions MiniLang et mettre à jour l'état du programme
run(Etat, [], Etat). % Cas de base : si le programme est vide, l'état final est l'état actuel

run(EtatPrecedent, Var = Expr, EtatFinal) :-
    eval(Expr, EtatPrecedent, Valeur),
    update_state(Var, Valeur, EtatPrecedent, NouvelEtat),
    EtatFinal = NouvelEtat.

run(EtatPrecedent, print(StringOrKey), EtatPrecedent) :-
    (   atomic(StringOrKey) ->
        (   member((StringOrKey, Value), EtatPrecedent) ->
            write(Value)
        ;   write(StringOrKey)
        )
    ;   eval(StringOrKey, EtatPrecedent, Value),
        write(Value)
    ),
    nl.

run(EtatPrecedent,(Instruction; Suite), EtatFinal) :-
    run(EtatPrecedent, Instruction, NouvelEtat),
    run(NouvelEtat, Suite, EtatFinal).

run(EtatPrecedent,if (Cond then(Then)), EtatFinal) :-
    (   eval(Cond, EtatPrecedent, true)
    ->  run(EtatPrecedent, Then, EtatFinal)
    ;   EtatFinal = EtatPrecedent
    ).

run(EtatPrecedent, if Cond Then() Else(), EtatFinal) :-
    (   eval(Cond, EtatPrecedent, true)
    ->  run(EtatPrecedent, Then, EtatFinal)
    ;   run(EtatPrecedent, Else, EtatFinal)
    ).

run(EtatPrecedent, while(Cond, do(Do)), EtatFinal) :-
    (   eval(Cond, EtatPrecedent, true)
    ->  run(EtatPrecedent, Do, NouvelEtat), % Exécute le bloc d'instructions
        run(NouvelEtat, while(Cond, do(Do)), EtatFinal) % Réexécute la boucle avec le nouvel état
    ;   EtatFinal = EtatPrecedent % Si la condition n'est plus vraie, retourne l'état précédent
    ).

% Prédicat pour exécuter les programmes à partir de fichiers
run_from_file(InitialState, FileName, FinalState) :-
    open(FileName, read, Stream),
    read_term(Stream, Prog, []),
    close(Stream),
    run(InitialState, Prog, FinalState).
