% Prédicat pour évaluer les expressions arithmétiques
% eval(Expression, Etat, Valeur)
eval(Expr, _, Expr) :-
    number(Expr). % Si l'expression est un nombre, retourne directement sa valeur

eval(Var, Etat, Valeur) :-
    atom(Var), % Si l'expression est une variable
    member((Var, Valeur), Etat). % Recherche sa valeur dans l'état
eval(Expr, Etat, Valeur) :-
    Expr =.. [Op, Gauche, Droite], % Si l'expression est une expression composée
    eval(Gauche, Etat, ValeurGauche), % Évalue le côté gauche
    eval(Droite, Etat, ValeurDroite), % Évalue le côté droit
    ExprEval =.. [Op, ValeurGauche, ValeurDroite], % Construit une nouvelle expression
    Valeur is ExprEval. % Évalue la nouvelle expression

% Prédicat pour mettre à jour l'état du programme après une assignation de variable
% update_state(Variable, Valeur, EtatPrecedent, NouvelEtat)
update_state(Var, Val, [], [(Var, Val)]).
update_state(Var, Val, [(Var, _)|T], [(Var, Val)|T]).
update_state(Var, Val, [(X, V)|T], [(X, V)|NouvelleT]) :-
    Var \= X,
    update_state(Var, Val, T, NouvelleT).

% Prédicat pour exécuter les instructions MiniLang et mettre à jour l'état du programme
% run(EtatPrecedent, Programme, EtatFinal)
run(Etat, [], Etat). % Cas de base : si le programme est vide, l'état final est l'état actuel

run(EtatPrecedent, Var = Expr, EtatFinal) :-
    eval(Expr, EtatPrecedent, Valeur),
    update_state(Var, Valeur, EtatPrecedent, NouvelEtat),
    EtatFinal = NouvelEtat.

run(EtatPrecedent, (Instruction; Suite), EtatFinal) :-
    run(EtatPrecedent, Instruction, NouvelEtat),
    run(NouvelEtat, Suite, EtatFinal).

run(EtatPrecedent, if(Cond, Alors)|T, EtatFinal) :-
    (   eval(Cond, EtatPrecedent, true)
    ->  run(EtatPrecedent, Alors, NouvelEtat)
    ;   NouvelEtat = EtatPrecedent
    ),
    run(NouvelEtat, T, EtatFinal).

run(EtatPrecedent, if(Cond, Alors, Sinon)|T, EtatFinal) :-
    (   eval(Cond, EtatPrecedent, true)
    ->  run(EtatPrecedent, Alors, NouvelEtat)
    ;   run(EtatPrecedent, Sinon, NouvelEtat)
    ),
    run(NouvelEtat, T, EtatFinal).

run(EtatPrecedent, while(Cond, Faire)|T, EtatFinal) :-
    (   eval(Cond, EtatPrecedent, true)
    ->  run(EtatPrecedent, Faire, NouvelEtat1), % Exécute le bloc
        run(NouvelEtat1, [while(Cond, Faire)], NouvelEtat) % Exécute la boucle while récursivement
    ;   NouvelEtat = EtatPrecedent
    ),
    run(NouvelEtat, T, EtatFinal).


    run_from_file( EtatPrecedent , FileName , EtatFinal ) :-
        open( FileName , read , Stream ),
        read_term(Stream , Prog , []) ,
        close( Stream ),
        run( EtatPrecedent , Prog , EtatFinal ).

run(InitialState, print(Expr), FinalState) :-
    (
        eval(Expr,InitialState,Valeur)
    ->  print(Valeur)
    ),
    run(InitialState, [], FinalState).
