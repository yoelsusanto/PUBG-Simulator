/* fakta */
dynamic(inventory/1).
dynamic(health/1).
dynamic(armor/1).
dynamic(currweapon/1).
dynamic(position/2).    % position predicate
dynamic(player/1).      % player predicate
dynamic(play/1).        % is playing predicate

inventory(none).        % set inventory to no item

% default health is 100
health(100).

% starting armor is 20
armor(20).

% current weapon is none
currweapon(none).

% weapon variations
weapon(ak47).
weapon(pistol).

% set locations area
location(1, 1, 6, 6, 'pochinki').
location(7, 1, 9, 6, 'the forest').
location(10, 1, 15, 6, 'the desert').
location(1, 7, 5, 15, 'labtek V').
location(6, 7, 12, 15, 'labtek VI').
location(13, 7, 15, 15, 'CC barat').

play(false).

mapM(
    [
    ['X', 'X', 'X', 'X', 'X', 'X', 'X', 'X', 'X', 'X', 'X', 'X', 'X', 'X', 'X', 'X', 'X'],
    ['X', '-', '-', '-', '-', '-', '-', '-', '-', '-', '-', '-', '-', '-', '-', '-', 'X'],
    ['X', '-', '-', '-', '-', '-', '-', '-', '-', '-', '-', '-', '-', '-', '-', '-', 'X'],
    ['X', '-', '-', '-', '-', '-', '-', '-', '-', '-', '-', '-', '-', '-', '-', '-', 'X'],
    ['X', '-', '-', '-', '-', '-', '-', '-', '-', '-', '-', '-', '-', '-', '-', '-', 'X'],
    ['X', '-', '-', '-', '-', '-', '-', '-', '-', '-', '-', '-', '-', '-', '-', '-', 'X'],
    ['X', '-', '-', '-', '-', '-', '-', '-', '-', '-', '-', '-', '-', '-', '-', '-', 'X'],
    ['X', '-', '-', '-', '-', '-', '-', '-', '-', '-', '-', '-', '-', '-', '-', '-', 'X'],
    ['X', '-', '-', '-', '-', '-', '-', '-', '-', '-', '-', '-', '-', '-', '-', '-', 'X'],
    ['X', '-', '-', '-', '-', '-', '-', '-', '-', '-', '-', '-', '-', '-', '-', '-', 'X'],
    ['X', '-', '-', '-', '-', '-', '-', '-', '-', '-', '-', '-', '-', '-', '-', '-', 'X'],
    ['X', '-', '-', '-', '-', '-', '-', '-', '-', '-', '-', '-', '-', '-', '-', '-', 'X'],
    ['X', '-', '-', '-', '-', '-', '-', '-', '-', '-', '-', '-', '-', '-', '-', '-', 'X'],
    ['X', '-', '-', '-', '-', '-', '-', '-', '-', '-', '-', '-', '-', '-', '-', '-', 'X'],
    ['X', '-', '-', '-', '-', '-', '-', '-', '-', '-', '-', '-', '-', '-', '-', '-', 'X'],
    ['X', '-', '-', '-', '-', '-', '-', '-', '-', '-', '-', '-', '-', '-', '-', '-', 'X'],
    ['X', 'X', 'X', 'X', 'X', 'X', 'X', 'X', 'X', 'X', 'X', 'X', 'X', 'X', 'X', 'X', 'X']
    ]).

/* rule */
start :-
    % randomly place player
    random(1, 16, X), random(1, 16, Y), assert(player([X, Y])),
    % randomly place weapon
    forall(
        (random(2, 5, N), between(1, N, _)), forall(weapon(Z), (random(1, 16, A), random(1, 16, B), assert(position(Z, [A, B]))))
        ),
    % erase play from false to true
    retract(play(false)),
    assert(play(true)),
    writeln(',d88~~\\                         d8                 d8              e                 d8           ,88~-_      88~\\    88~\\   888   ,e,                        '),
    writeln('8888       /~~~8e   888-~88e  _d88__   e88~~8e   _d88__           d8b      888-~\\  _d88__        d888   \\   _888__  _888__   888    "    888-~88e    e88~~8e  '),
    writeln('`Y88b          88b  888  888   888    d888  88b   888            /Y88b     888      888         88888    |   888     888     888   888   888  888   d888  88b '),
    writeln(' `Y88b,   e88~-888  888  888   888    8888__888   888           /  Y88b    888      888         88888    |   888     888     888   888   888  888   8888__888 '),
    writeln('   8888  C888  888  888  888   888    Y888    ,   888          /____Y88b   888      888          Y888   /    888     888     888   888   888  888   Y888    , '),
    writeln('\\__88P''   "88_-888  888  888   "88_/   "88___/    "88_/       /      Y88b  888      "88_/         `88_-~     888     888     888   888   888  888    "88___/  '), nl, nl,
    writeln('Welcome to the battlefield!'),
    writeln('You have been chosen as one of the lucky contestants. Be the last man standing and you will be remembered as one of the victors.'), nl,
    writeln('Available commands:'),
    writeln('start. -- start the game!'),
    writeln('help. -- show available commands'),
    writeln('quit. -- quit the game'),
    writeln('look. -- look around you'),
    writeln('n. s. e. w. -- move'),
    writeln('map. -- look at the map and detect enemies'),
    writeln('take(Object). -- pick up an object'),
    writeln('drop(Object). -- drop an object'),
    writeln('use(Object). -- use an object'),
    writeln('attack. -- attack enemy that crosses your path'),
    writeln('status. -- show your status'),
    writeln('save(Filename). -- save your game'),
    writeln('load(Filename). -- load previously saved game'),
    writeln('Legends:'),
    writeln('W = weapon'),
    writeln('A = armor'),
    writeln('M = medicine'),
    writeln('O = ammo'),
    writeln('P = player'),
    writeln('E = enemy'),
    writeln('- = accessible'),
    writeln('X = inaccessible').

% if quit, make play to false and retract player position
quit :- retract(play(true)), assert(play(false)), retractall(player(_)).

map :- play(X), X == false, !, write('You must start the game using "start." first.'), fail.
map :- mapM(M), printMatrix(M).

% move player position
n :- play(X), X == false, !, write('You must start the game using "start." first.'), fail.
n :- player([X|Y]), Xn is X + 1, retractall(player(_)), assert(player([Xn|Y])).
s :- play(X), X == false, !, write('You must start the game using "start." first.'), fail.
s :- player([X|Y]), Xn is X - 1, retractall(player(_)), assert(player([Xn|Y])).
e :- play(X), X == false, !, write('You must start the game using "start." first.'), fail.
e :- player([X, Y]), Yn is Y + 1, retractall(player(_)), assert(player([X, Yn])).
w :- play(X), X == false, !, write('You must start the game using "start." first.'), fail.
w :- player([X, Y]), Yn is Y - 1, retractall(player(_)), assert(player([X, Yn])).

% Dipanggil pada saat ada perintah move
areaAround :-
    player([X, Y]), N is X + 1, S is X - 1, E is Y + 1, W is Y - 1,
    (location(Xmin, Ymin, Xmax, Ymax, LocationName),
    (Xmin =< X, X =< Xmax, Ymin =< Y, Y =< Ymax, !, write('You are in '), write(LocationName), write('. '))),
    (location(XminN, YminN, XmaxN, YmaxN, LocationNName),
    (XminN =< N, N =< XmaxN, YminN =< Y, Y =< YmaxN, !, write('To the north is '), write(LocationNName), write('. '))),
    (location(XminE, YminE, XmaxE, YmaxE, LocationEName),
    (XminE =< X, X =< XmaxE, YminE =< E, E =< YmaxE, !, write('To the east is '), write(LocationEName), write('. '))),
    (location(XminS, YminS, XmaxS, YmaxS, LocationSName),
    (XminS =< S, S =< XmaxS, YminS =< Y, Y =< YmaxS, !, write('To the south is '), write(LocationSName), write('. '))),
    (location(XminW, YminW, XmaxW, YmaxW, LocationWName),
    (XminW =< X, X =< XmaxW, YminW =< W, W =< YmaxW, !, write('To the west is '), write(LocationWName), write('.'))), nl.

printMatrix(M) :- printRows(M, 0).
printRows([], _).
printRows([H|T], R) :- !,
    printRow(H, R, 0),
    Rpp is R + 1,
    printRows(T, Rpp).
printRow([], _, _) :- nl.
printRow([H|T], R, C) :-
    ((player([X, Y|_]), R == X, C == Y), write('P'); write(H)), !,
    write(' '),
    Cpp is C + 1,
    printRow(T, R, Cpp).

take(_) :- play(X), X == false, !, write('You must start the game using "start." first.'), fail.
take(X) :- \+weapon(X), !, write('Weapon doesnt exist.'), fail.
take(X) :- \+nearby(X), !, write('There is no '), write(X), write(' around here.'), fail.
take(X) :- asserta(inventory(X)).

nearby(X) :- position(X, [A,B|_]), player([P1,P2|_]), A == P1, B == P2.

use(_) :- play(X), X == false, !, write('You must start the game using "start." first.'), fail.
use(X) :- \+inventory(X), !, write('Item doesnt exist in inventory.'), fail.
use(X) :- retract(inventory(X)), asserta(currweapon(X)).

% status command
status :- play(X), X == false, !, write('You must start the game using "start." first.'), fail.
status :-
    health(X), !, write('Health : '), write(X),
    armor(Y), !, write('Armor : '),  write(Y),
    currweapon(Z), !, write('Weapon : '), write(Z),
    inventory(A), !, write('Inventory : '), write(A), write(' '),
    forall((inventory(B), B \== A, B \== none), (write(A), write(' '))), nl.