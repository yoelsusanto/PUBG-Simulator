/* fakta */
:- dynamic(inventory/1).
:- dynamic(health/1).
:- dynamic(armor/1).
:- dynamic(currweapon/1).
:- dynamic(position/2).    % position predicate
:- dynamic(player/1).      % player predicate
:- dynamic(play/1).        % is playing predicate
:- dynamic(time/1).

inventory(none).            % set inventory to no item

time(0).

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

% set play to default false
play(false).

/* rule */
start :-
    % randomly place player
    random(1, 16, X), random(1, 16, Y), asserta(player([X, Y])),
    % randomly place weapon
    forall(
        (random(2, 5, N), between(1, N, _)), forall(weapon(Z), (random(1, 16, A), random(1, 16, B), asserta(position(Z, [A, B]))))
        ),
    % erase play from false to true
    retract(play(false)),
    asserta(play(true)),
    write(',d88~~\\                         d8                 d8              e                 d8           ,88~-_      88~\\    88~\\   888   ,e,                        '), nl,
    write('8888       /~~~8e   888-~88e  _d88__   e88~~8e   _d88__           d8b      888-~\\  _d88__        d888   \\   _888__  _888__   888    "    888-~88e    e88~~8e  '), nl,
    write('`Y88b          88b  888  888   888    d888  88b   888            /Y88b     888      888         88888    |   888     888     888   888   888  888   d888  88b '), nl,
    write(' `Y88b,   e88~-888  888  888   888    8888__888   888           /  Y88b    888      888         88888    |   888     888     888   888   888  888   8888__888 '), nl,
    write('   8888  C888  888  888  888   888    Y888    ,   888          /____Y88b   888      888          Y888   /    888     888     888   888   888  888   Y888    , '), nl,
    write('\\__88P''   "88_-888  888  888   "88_/   "88___/    "88_/       /      Y88b  888      "88_/         `88_-~     888     888     888   888   888  888    "88___/  '), nl, nl,
    write('Welcome to the battlefield!'), nl,
    write('You have been chosen as one of the lucky contestants. Be the last man standing and you will be remembered as one of the victors.'), nl, nl,
    write('Available commands:'), nl,
    write('   start. -- start the game!'), nl,
    write('   help. -- show available commands'), nl,
    write('   quit. -- quit the game'), nl,
    write('   look. -- look around you'), nl,
    write('   n. s. e. w. -- move'), nl,
    write('   map. -- look at the map and your position'), nl,
    write('   take(Object). -- pick up an object'), nl,
    write('   drop(Object). -- drop an object'), nl,
    write('   use(Object). -- use an object'), nl,
    write('   attack. -- attack enemy that crosses your path'), nl,
    write('   status. -- show your status'), nl,
    write('   save(Filename). -- save your game'), nl,
    write('   load(Filename). -- load previously saved game'), nl, nl,
    write('Legends:'), nl,
    write('   W = weapon'), nl,
    write('   A = armor'), nl,
    write('   M = medicine'), nl,
    write('   O = ammo'), nl,
    write('   P = player'), nl,
    write('   E = enemy'), nl,
    write('   - = accessible'), nl,
    write('   X = inaccessible'), nl,
    asserta(play(true)).

% if quit, make play to false and retract player position
quit :- retract(play(true)), asserta(play(false)), retractall(player(_)).

% map :- play(X), X == false, !, write('You must start the game using "start." first.'), fail.
map :- time(X), Block is X//3, printMap(0,0,Block+1).


printMap(X,Y,DeadZone) :- player([X,Y]) , write('P'), write(' '), Xa is X+1, printMap(Xa,Y,DeadZone),!.
printMap(X,Y,_) :- X>=17, Y>=16.
printMap(X,Y,DeadZone) :- X==17, nl, Ya is Y + 1, printMap(0,Ya,DeadZone), !.
printMap(X,Y,DeadZone) :-
    ((X < DeadZone; X >= (17-DeadZone); Y < DeadZone; Y >= (17-DeadZone)), write('X'); write('-')), !,
    write(' '),
    Xa is X+1, printMap(Xa,Y,DeadZone).

% move player position
n :- play(X), X == false, !, write('You must start the game using "start." first.'), fail.
n :- player([X|Y]), Xn is X + 1, retractall(player(_)), asserta(player([Xn|Y])), areaAround.
s :- play(X), X == false, !, write('You must start the game using "start." first.'), fail.
s :- player([X|Y]), Xn is X - 1, retractall(player(_)), asserta(player([Xn|Y])), areaAround.
e :- play(X), X == false, !, write('You must start the game using "start." first.'), fail.
e :- player([X, Y]), Yn is Y + 1, retractall(player(_)), asserta(player([X, Yn])), areaAround.
w :- play(X), X == false, !, write('You must start the game using "start." first.'), fail.
w :- player([X, Y]), Yn is Y - 1, retractall(player(_)), asserta(player([X, Yn])), areaAround.

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

% help
help :- play(X), X == false, !, write('You must start the game using "start." first.'), fail.
help :-
    write('Available commands:'), nl,
    write('   start. -- start the game!'), nl,
    write('   help. -- show available commands'), nl,
    write('   quit. -- quit the game'), nl,
    write('   look. -- look around you'), nl,
    write('   n. s. e. w. -- move'), nl,
    write('   map. -- look at the map and your position'), nl,
    write('   take(Object). -- pick up an object'), nl,
    write('   drop(Object). -- drop an object'), nl,
    write('   use(Object). -- use an object'), nl,
    write('   attack. -- attack enemy that crosses your path'), nl,
    write('   status. -- show your status'), nl,
    write('   save(Filename). -- save your game'), nl,
    write('   load(Filename). -- load previously saved game'), nl, nl,
    write('Legends:'), nl,
    write('   W = weapon'), nl,
    write('   A = armor'), nl,
    write('   M = medicine'), nl,
    write('   O = ammo'), nl,
    write('   P = player'), nl,
    write('   E = enemy'), nl,
    write('   - = accessible'), nl,
    write('   X = inaccessible'), nl.
    
% look
% look :-

% Drop Item
drop(_) :- play(X), X == false, !, write('You must start the game using "start." first.'), fail.
drop(X) :- \+inventory(X), !, write('Item doesnt exist in inventory.'), fail.
drop(X) :- \+inventory(X), currweapon(X), !, write('You have to put your weapon in your inventory to drop it.'), fail.
drop(X) :- retract(inventory(X)), player(L), asserta(position(X,L)).