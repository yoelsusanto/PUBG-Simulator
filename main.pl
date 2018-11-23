/* fakta */
:- dynamic(inventory/1).
:- dynamic(health/1).
:- dynamic(armor/1).
:- dynamic(currweapon/1).
:- dynamic(position/2).    % position predicate
:- dynamic(player/1).      % player predicate
:- dynamic(play/1).        % is playing predicate
:- dynamic(waktu/1).

% set inventory to no item
inventory(none).

% set waktu to zero
waktu(0).

% default health is 100
health(100).

% starting armor is 20
armor(20).

% current weapon is none
currweapon(none).

% ------------- Item Variations -------------------
% weapon variations
weapon(sumpitan).
weapon('voodoo_equipment').
weapon('cursing_equipment').

% enemy variations
enemy(hantu).
enemy(tubes).

% medicine
medicine(batu1).
medicine(batu2).

% armor
variasiArmor(aluminium).
variasiArmor(cangkang).

% ammo
ammo(peluru).

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
    forall((random(2, 5, N), between(1, N, _)), forall(weapon(Z), (random(1, 16, A), random(1, 16, B), asserta(position(Z, [A, B]) ) ) ) ),
    
    % randomly place enemy
    forall((random(2, 5, N), between(1, N, _)), forall(enemy(Z), (random(1, 16, A), random(1, 16, B), asserta(position(Z, [A, B]) ) ) ) ),

    % randomly place medicine
    forall((random(2, 5, N), between(1, N, _)), forall(medicine(Z), (random(1, 16, A), random(1, 16, B), asserta(position(Z, [A, B]) ) ) ) ),
    
    % randomly place armor
    forall((random(2, 5, N), between(1, N, _)), forall(variasiArmor(Z), (random(1, 16, A), random(1, 16, B), asserta(position(Z, [A, B]) ) ) ) ),
    
    % randomly place ammo
    forall((random(2, 5, N), between(1, N, _)), forall(ammo(Z), (random(1, 16, A), random(1, 16, B), asserta(position(Z, [A, B]) ) ) ) ),
    
    % erase play from false to true
    retract(play(false)),
    asserta(play(true)),

    % print required texts
    printHeader,printHelp.

save(_) :- play(X), X == false, !, write('You must start the game using "start." first.'), fail.
save(S) :- inventory(I), health(H), armor(A), currweapon(Cw), position(X, Y), player(P), play(Pl), waktu(W),
        open(S, write, Str),
        write(Str, H), write(Str,'.'), nl(Str),
        write(Str, A), write(Str,'.'), nl(Str),
        write(Str, Cw), write(Str,'.'), nl(Str),
        write(Str, P), write(Str,'.'), nl(Str),
        write(Str, Pl), write(Str,'.'), nl(Str),
        write(Str, W), write(Str,'.'), nl(Str),
        write(Str, X), write(Str,'.'), nl(Str),
        write(Str, Y), write(Str,'.'), nl(Str),
        forall((position(Xl,Yl), Yl \== Y, Yl \== none) , (write(Str, Xl), write(Str, '.'), nl(Str), write(Str, Yl), write(Str, '.'), nl(Str))),
        write(Str, 'inventory.'), nl(Str),
        write(Str, I), write(Str,'.'), nl(Str),
        forall((inventory(Ilagi), Ilagi \== I, Ilagi \== none), (write(Str, Ilagi), write(Str, '.'), nl(Str))),
        close(Str), !.

loads(_) :- play(X), X == false, !, write('You must start the game using "start." first.'), fail.
loads(L) :- retractall(inventory(_)), retract(health(_)), retract(armor(_)), retract(currweapon(_)), retractall(position(_, _)), retract(player(_)), retract(play(_)), retract(waktu(_)), 
        open(L, read, Str),
        read(Str, H), read(Str, A), read(Str, Cw),  read(Str, P), read(Str, Pl), read(Str, W),read(Str, X), read_position(Str, X), read_inventory(Str, _),
        close(Str),
        asserta(health(H)), asserta(armor(A)), asserta(currweapon(Cw)),  asserta(player(P)),  asserta(waktu(W)), asserta(play(Pl)), !.        

end_of_inventory('inventory').

read_position(_, X) :- end_of_inventory(X), !.

read_position(Stream, X):- 
    \+end_of_inventory(X),  
    read(Stream, Y),
    asserta(position(X, Y)),
    read(Stream, Z),
    read_position(Stream, Z).
    

read_inventory(Stream, _) :- at_end_of_stream(Stream), !. 
    
read_inventory(Stream,[X|L]):- 
    \+ at_end_of_stream(Stream), 
    read(Stream,X),
    asserta(inventory(X)), 
    read_inventory(Stream,L).

% if quit, make play to false and retract player position
quit :- retract(play(true)), asserta(play(false)), retractall(player(_)).

% Map (Final)
map :- play(X), X == false, !, write('You must start the game using "start." first.'), fail.
map :- waktu(X), Block is X//3, printMap(0,0,Block+1).

printMap(X,Y,DeadZone) :- player([X,Y]) , write('P'), write(' '), Xa is X+1, printMap(Xa,Y,DeadZone),!.
printMap(X,Y,_) :- X>=17, Y>=16.
printMap(X,Y,DeadZone) :- X==17, nl, Ya is Y + 1, printMap(0,Ya,DeadZone), !.
printMap(X,Y,DeadZone) :-
    ((X < DeadZone; X >= (17-DeadZone); Y < DeadZone; Y >= (17-DeadZone)), write('X'); write('-')), !,
    write(' '),
    Xa is X+1, printMap(Xa,Y,DeadZone).

% move player position (Final)
n :- play(X), X == false, !, write('You must start the game using "start." first.'), fail.
n :- player([X, Y]), Yn is Y - 1, isDeadZone(X,Yn), write('You cant move into deadzone!'),!. % apa lagi nih
n :- player([X, Y]), Yn is Y - 1, retractall(player(_)), asserta(player([X, Yn])), areaAround, updateGame, !.

s :- play(X), X == false, !, write('You must start the game using "start." first.'), fail.
s :- player([X, Y]), Yn is Y + 1, isDeadZone(X,Yn), write('You cant move into deadzone!'),!.
s :- player([X, Y]), Yn is Y + 1, retractall(player(_)), asserta(player([X, Yn])), areaAround, updateGame, !.

e :- play(X), X == false, !, write('You must start the game using "start." first.'), fail.
e :- player([X, Y]), Xn is X + 1, isDeadZone(Xn,Y), write('You cant move into deadzone!'),!.
e :- player([X, Y]), Xn is X + 1, retractall(player(_)), asserta(player([Xn, Y])), areaAround, updateGame, !.

w :- play(X), X == false, !, write('You must start the game using "start." first.'), fail.
w :- player([X, Y]), Xn is X - 1, isDeadZone(Xn,Y), write('You cant move into deadzone!'),!.
w :- player([X, Y]), Xn is X - 1, retractall(player(_)), asserta(player([Xn, Y])), areaAround, updateGame, !.

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
take(X) :- asserta(inventory(X)), player(L), retract(position(X, L)), updateGame.

nearby(X) :- position(X, [A,B|_]), player([P1,P2|_]), A == P1, B == P2.

use(_) :- play(X), X == false, !, write('You must start the game using "start." first.'), fail.
use(X) :- \+inventory(X), !, write('Item doesnt exist in inventory.'), fail.
use(X) :- retract(inventory(X)), asserta(currweapon(X)), updateGame.

% status command
status :- play(X), X == false, !, write('You must start the game using "start." first.'), fail.
status :-
    health(X), !, write('Health : '), write(X), nl,
    armor(Y), !, write('Armor : '),  write(Y), nl,
    currweapon(Z), !, write('Weapon : '), write(Z), nl,
    inventory(A), !, write('Inventory : '), write(A), write(' '),
    forall((inventory(B), B \== A, B \== none), (write(B), write(' '))), nl.

% help (Final)
help :- play(X), X == false, !, write('You must start the game using "start." first.'), fail.
help :- printHelp.
    
% look
look :-
    player([X, Y]),
    (location(Xmin, Ymin, Xmax, Ymax, LocationName),
    (Xmin =< X, X =< Xmax, Ymin =< Y, Y =< Ymax, !, write('You are in '), write(LocationName), write('. '))),
    forall(position(A, [X, Y]), (((weapon(A)), write('You see an empty '); write('You see a ')), write(A), write(' lying on the grass. '))), nl,
    A is X-1, B is X+1, C is Y-1, D is Y+1,
    printPrio(A, C), write(' '), printPrio(X, C), write(' '), printPrio(B, C), nl,
    printPrio(A, Y), write(' '), printPrio(X, Y), write(' '), printPrio(B, Y), nl,
    printPrio(A, D), write(' '), printPrio(X, D), write(' '), printPrio(B, D), nl.

printPrio(X,Y) :- isDeadZone(X,Y), !, write('X').
printPrio(X,Y) :- position(Z, [X,Y]), enemy(Z), !, write('E').
printPrio(X,Y) :- position(Z, [X,Y]), medicine(Z), !, write('M').
printPrio(X,Y) :- position(Z, [X,Y]), weapon(Z), !, write('W').
printPrio(X,Y) :- position(Z, [X,Y]), variasiArmor(Z), !, write('A').
printPrio(X,Y) :- position(Z, [X,Y]), ammo(Z), !, write('O').
printPrio(X,Y) :- player([X,Y]), !, write('P').
printPrio(_,_) :- write('-').


% Drop Item
drop(_) :- play(X), X == false, !, write('You must start the game using "start." first.'), fail.
drop(X) :- \+inventory(X), !, write('Item doesnt exist in inventory.'), fail.
drop(X) :- \+inventory(X), currweapon(X), !, write('You have to put your weapon in your inventory to drop it.'), fail.
drop(X) :- retract(inventory(X)), player(L), asserta(position(X,L)), updateGame.

% Check deadzone
isDeadZone(X,Y) :- waktu(Waktu), Block is (Waktu//3)+1, ((X < Block; X >= (17-Block); Y < Block; Y >= (17-Block))),!.

% Add time
addTime :- waktu(X), Y is X, NewX is Y + 1, retract(waktu(X)), asserta(waktu(NewX)).

% Update Game (including add time, )
updateGame :- addTime, moveEnemy.

% periodicDrop :- 
% move enemy toward player
moveEnemy :- forall((position(Z,[X,Y]), enemy(Z)), (retract(position(Z,[X,Y])), random(1,4,N), movePosition(Z,X,Y,N)) ).

movePosition(Z,X,Y,N) :-
    ((N==1),(asserta(position(Z,[X+1,Y]))));
    ((N==2),(asserta(position(Z,[X-1,Y]))));
    ((N==3),(asserta(position(Z,[X,Y+1]))));
    ((N==4),(asserta(position(Z,[X,Y-1])))).

printHeader :-
    write(',d88~~\\                         d8                 d8              e                 d8           ,88~-_      88~\\    88~\\   888   ,e,                        '), nl,
    write('8888       /~~~8e   888-~88e  _d88__   e88~~8e   _d88__           d8b      888-~\\  _d88__        d888   \\   _888__  _888__   888    "    888-~88e    e88~~8e  '), nl,
    write('`Y88b          88b  888  888   888    d888  88b   888            /Y88b     888      888         88888    |   888     888     888   888   888  888   d888  88b '), nl,
    write(' `Y88b,   e88~-888  888  888   888    8888__888   888           /  Y88b    888      888         88888    |   888     888     888   888   888  888   8888__888 '), nl,
    write('   8888  C888  888  888  888   888    Y888    ,   888          /____Y88b   888      888          Y888   /    888     888     888   888   888  888   Y888    , '), nl,
    write('\\__88P''   "88_-888  888  888   "88_/   "88___/    "88_/       /      Y88b  888      "88_/         `88_-~     888     888     888   888   888  888    "88___/  '), nl, nl,
    write('Welcome to the battlefield!'), nl,
    write('You have been chosen as one of the lucky contestants. Be the last man standing and you will be remembered as one of the victors.'), nl, nl.

printHelp :-
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
    write('   loads(Filename). -- load previously saved game'), nl, nl,
    write('Legends:'), nl,
    write('   W = weapon'), nl,
    write('   A = armor'), nl,
    write('   M = medicine'), nl,
    write('   O = ammo'), nl,
    write('   P = player'), nl,
    write('   E = enemy'), nl,
    write('   - = accessible'), nl,
    write('   X = inaccessible'), nl.