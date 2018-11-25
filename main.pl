/* fakta */
:- dynamic(maxInventory/1). %to define the maximum type of item to be stored in inventory
:- dynamic(currweapon/2).
:- dynamic(inventory/2).
:- dynamic(position/2).    % position predicate
:- dynamic(player/1).      % player predicate
:- dynamic(health/1).
:- dynamic(armor/1).
:- dynamic(waktu/1).
:- dynamic(play/1).        % is playing predicate
:- dynamic(lose/1).
:- dynamic(win/1).
% ---------------- Item Variations ---------------- %
% weapon variations
weapon('sumpitan').
weapon('Mini 14').
weapon('SCAR-L').
weapon('AKM').

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
ammo('sumpitan', 'anak sumpit').
ammo('Mini 14', '5.56 mm').
ammo('SCAR-L', '5.56 mm').
ammo('AKM', '7.62 mm').

% set locations area
location(10, 1, 15, 6, 'the desert').
location(6, 7, 12, 15, 'labtek VI').
location(13, 7, 15, 15, 'CC barat').
location(7, 1, 9, 6, 'the forest').
location(1, 7, 5, 15, 'labtek V').
location(1, 1, 6, 6, 'pochinki').

% set play to false
play(false).

/* rule */
start :-
    % randomly place player
    random(1, 16, X), random(1, 16, Y), asserta(player([X, Y])),
    
    % randomly place weapon
    forall((random(2, 5, N), between(1, N, _)), forall(weapon(Z), (random(1, 16, A), random(1, 16, B), asserta(position(Z, [A, B]) ) ) ) ),
    
    % randomly place enemy
    forall((random(24, 25, N), between(1, N, _)), forall(enemy(Z), (random(1, 16, A), random(1, 16, B), asserta(position(Z, [A, B]) ) ) ) ),

    % randomly place medicine
    forall((random(2, 5, N), between(1, N, _)), forall(medicine(Z), (random(1, 16, A), random(1, 16, B), asserta(position(Z, [A, B]) ) ) ) ),
    
    % randomly place armor
    forall((random(2, 5, N), between(1, N, _)), forall(variasiArmor(Z), (random(1, 16, A), random(1, 16, B), asserta(position(Z, [A, B]) ) ) ) ),
    
    % randomly place ammo
    forall((random(2, 5, N), between(1, N, _)), forall(ammo(_, Z), (random(1, 16, A), random(1, 16, B), asserta(position(Z, [A, B]) ) ) ) ),
    
    % set max inventory
    asserta(maxInventory(10)),

    % erase play from false to true
    retract(play(false)),
    asserta(play(true)),

    % testing
    asserta(currweapon('Mini 14',0)),
    asserta(inventory('5.56 mm',5)),
    asserta(inventory('7.62 mm',5)),

    % set inventory to no item (no inventory() facts)
    % asserta(inventory(none)),
    % asserta(inventory(neone)),
    % asserta(inventory(neeone)),

    % set waktu to zero
    asserta(waktu(0)),

    % default health is 100
    asserta(health(100)),

    % starting armor is 20
    asserta(armor(20)),

    % current weapon is none
    % currweapon(nama_weapon, jml peluru untuk weapon itu)
    % asserta(currweapon(none, 0)),
    
    % set play to default false
    asserta(win(false)),
    asserta(lose(false)),

    % print required texts
    printHeader,printHelp.

% save(_) :- play(X), X == false, !, write('You must start the game using "start." first.'), fail.
% save(S) :- inventory(I), health(H), armor(A), currweapon(Cw, Ca), position(X, Y), player(P), play(Pl), waktu(W),
%         open(S, write, Str),
%         write(Str, H), write(Str,'.'), nl(Str),
%         write(Str, A), write(Str,'.'), nl(Str),
%         write(Str, Cw), write(Str,'.'), nl(Str),
%         write(Str, Ca), write(Str,'.'), nl(Str),
%         write(Str, P), write(Str,'.'), nl(Str),
%         write(Str, Pl), write(Str,'.'), nl(Str),
%         write(Str, W), write(Str,'.'), nl(Str),
%         write(Str, X), write(Str,'.'), nl(Str),
%         write(Str, Y), write(Str,'.'), nl(Str),
%         forall((position(Xl,Yl), Yl \== Y, Yl \== none) , (write(Str, Xl), write(Str, '.'), nl(Str), write(Str, Yl), write(Str, '.'), nl(Str))),
%         write(Str, 'inventory.'), nl(Str),
%         write(Str, I), write(Str,'.'), nl(Str),Fc
%         forall((inventory(Ilagi), Ilagi \== I, Ilagi \== none), (write(Str, Ilagi), write(Str, '.'), nl(Str))),
%         close(Str), !.

% loads(_) :- play(X), X == false, !, write('You must start the game using "start." first.'), fail.
% loads(L) :- retractall(inventory(_)), retract(health(_)), retract(armor(_)), retract(currweapon(_, _)), retractall(position(_, _)), retract(player(_)), retract(play(_)), retract(waktu(_)), 
%         open(L, read, Str),
%         read(Str, H), read(Str, A), read(Str, Cw), read(Str, Ca), read(Str, P), read(Str, Pl), read(Str, W),read(Str, X), read_position(Str, X), read_inventory(Str, _),
%         close(Str),
%         asserta(health(H)), asserta(armor(A)), asserta(currweapon(Cw, Ca)),  asserta(player(P)),  asserta(waktu(W)), asserta(play(Pl)), !.        

% end_of_inventory('inventory').

% read_position(_, X) :- end_of_inventory(X), !.

% read_position(Stream, X):- 
%     \+end_of_inventory(X),  
%     read(Stream, Y),
%     asserta(position(X, Y)),
%     read(Stream, Z),
%     read_position(Stream, Z).

% read_inventory(Stream, _) :- at_end_of_stream(Stream), !. 
    
% read_inventory(Stream,[X|L]):- 
%     \+ at_end_of_stream(Stream), 
%     read(Stream,X),
%     asserta(inventory(X)), 
%     read_inventory(Stream,L).

% if quit, make play to false and retract player position
quit :-
    retract(play(true)),
    retractall(currweapon(_, _)),
    retractall(inventory(_)),
    retractall(position(_, _)),
    retractall(player(_)),
    retractall(health(_)),
    retractall(armor(_)),
    retractall(waktu(_)),
    retractall(play(_)),
    retractall(lose(_)),
    retractall(win(_)),
    asserta(play(false)).

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
n :- player([X, Y]), Yn is Y - 1, isDeadZone(X,Yn), write('You cant move into deadzone!'),!.
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

item(X) :- weapon(X), !.
item(X) :- variasiArmor(X), !.
item(X) :- medicine(X), !.
item(X) :- ammo(_, X), !.

take(_) :- play(X), X == false, !, write('You must start the game using "start." first.'), fail.
take(X) :- \+item(X), !, write('Item does not exist.'), fail.
take(X) :- \+nearby(X), !, write('There is no '), write(X), write(' around here.'), fail.
take(_) :- countInven(Quantity), maxInventory(Max), Quantity == Max, !, write('Inventory is full! You can not sstore anything else!'), nl, fail.
take(NamaItem) :- 
    (ammo(_,NamaItem) -> Qty is 5 ; Qty is 1) , addItem(NamaItem,Qty),
    write('You took the '), write(NamaItem), write('.'), nl, player(L), retract(position(NamaItem, L)), updateGame.

haventhave(NamaItem) :-
    \+ inventory(NamaItem,_).

addItem(NamaItem,Qty) :-
    haventhave(NamaItem) -> (asserta(inventory(NamaItem,Qty))) ;
    (inventory(NamaItem,JmlAwal), retract(inventory(NamaItem,JmlAwal)), JmlAkhir is JmlAwal+Qty, asserta(inventory(NamaItem,JmlAkhir))).

nearby(X) :- position(X, Lt), player(Lt).

countInven(Count) :-
    findall(X, (inventory(X,_), X \== none), L),
    length(L,Count).

use(_) :- play(X), X == false, !, write('You must start the game using "start." first.'), fail.
use(X) :- \+inventory(X,_), !, write('Item doesnt exist in inventory.'), fail.
% untuk ammo
use(X) :-
    ammo(JenisWeapon, X),
    (( currweapon(JenisWeapon,JlhAmmo)) -> (inventory(X,Jml), JlhAmmoNew is JlhAmmo + Jml, retract(currweapon(JenisWeapon,JlhAmmo)), asserta(currweapon(JenisWeapon,JlhAmmoNew)), retract(inventory(X,Jml)), !, write('Ammo has been added.') ) ; 
    (!, write('You are not equipping weapon or the ammo is not suitable for your weapon type.')) ), nl, updateGame.

% untuk armor
use(X) :- variasiArmor(X), !, reduceItem(X), retract(armor(Y)), Yn is Y + 20, asserta(armor(Yn)), write('Your armor has been fortified.'), nl.
% untuk weapon
use(X) :- weapon(X), !, reduceItem(X), asserta(currweapon(X, 0)), write(X), write(' is equipped!'), nl, updateGame.
% untuk medicine
use(X) :- medicine(X), !, reduceItem(X), addHealth(X).

reduceItem(X) :-
    inventory(X,Jml), retract(inventory(X,Jml)),
    ((Jml > 1) -> JmlBaru is Jml-1, asserta(inventory(X,JmlBaru)) ; true).

addHealth(X) :-
    X=='batu1', health(CurHealth), retract(health(CurHealth)), UpHealth is CurHealth+10,
    ((UpHealth > 100) -> asserta(health(100));asserta(health(UpHealth))).

addHealth(X) :-
    X=='batu2', health(CurHealth), retract(health(CurHealth)), UpHealth is CurHealth+20,
    ((UpHealth > 100) -> asserta(health(100));asserta(health(UpHealth))).

% status command
status :- play(X), X == false, !, write('You must start the game using "start." first.'), fail.
status :-
    health(X), !, write('Health : '), write(X), nl,
    armor(Y), !, write('Armor : '),  write(Y), nl,
    (currweapon(NamaWeapon,JmlAmmo) -> (format('Weapon: ~w; Ammo: ~w',[NamaWeapon, JmlAmmo]), nl) ; (write('No weapon equipped!'), nl)),
    ((\+ inventory(_,_), !, write('Inventory kosong!')) ;
    (findall([Something,Jml],inventory(Something,Jml),L), write('Inventory: '), write(L))), nl.

% help (Final)
help :- play(X), X == false, !, write('You must start the game using "start." first.'), fail.
help :- printHelp.
    
% look
look :- play(X), X == false, !, write('You must start the game using "start." first.'), fail.
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
printPrio(X,Y) :- position(Z, [X,Y]), ammo(_, Z), !, write('O').
printPrio(X,Y) :- player([X,Y]), !, write('P').
printPrio(_,_) :- write('-').

% Drop Item
drop(_) :- play(X), X == false, !, write('You must start the game using "start." first.'), fail.
drop(X) :- \+inventory(X,_), !, write('Item does not exist in inventory.'), fail.
drop(X) :- weapon(X), currweapon(X, _), !, write('You have to unequip your weapon in your inventory to drop it.'), fail.
drop(X) :- reduceItem(X), player(L), asserta(position(X,L)), write('You have dropped '), write(X), updateGame.

unequip :-
    currweapon(NamaWeapon,Pelor) -> (retract(currweapon(NamaWeapon,Pelor)), ammo(NamaWeapon,NamaPelor), addItem(NamaPelor,Pelor), addItem(NamaWeapon,1), format('~w is now in your inventory and its corresponding ammo is back on your inventory.',[NamaWeapon]));
    format('Are you sure you are equiping any weapon?~N',[]).

% Check deadzone
isDeadZone(X,Y) :- waktu(Waktu), Block is (Waktu//3)+1, ((X < Block; X >= (17-Block); Y < Block; Y >= (17-Block))),!.

% Add time
addTime :- retract(waktu(X)), Y is X + 1, asserta(waktu(Y)).

% Update Game (including add time)
updateGame :- addTime, cleanObjects, moveEnemy, attacked, winLose, win(W), lose(L), (W \== true, L \== true) -> upGame.
upGame :-periodicDrop.

% clean objects untuk benda-benda yang sudah berada di dead zone.
cleanObjects :- forall((position(Z,[X,Y]), isDeadZone(X,Y)), (retract(position(Z,[X,Y])))).

% getattacked
attacked :-
    player(Lplayer) , forall((position(X,Lplayer), enemy(X)), (reduceHealth(-20), write('You got attacked! HP -20!'))), nl.
attacked.

reduceHealth(Qty) :-
    health(CurHealth), retract(health(CurHealth)), NewHealth is CurHealth+Qty, asserta(health(NewHealth)).

% check if the player has won or lost
winLose :-
    \+ (enemy(X), position(X,[_,_])), retract(win(false)), asserta(win(true)), printWinFalse, quit.
winLose :-
    ((player([X,Y]), isDeadZone(X,Y)) ; (health(Hp), (Hp < 1))), retract(lose(false)), asserta(lose(true)), printWinFalse, quit.
winLose.

printWinFalse :-
    (win(true), write('You have won! You are the last man standing!'));
    (win(false), write('You died! You lose!')).

% realisasi fungsi attack
attack :-
    player(LPosition), \+ position(Z,LPosition), enemy(Z), !, write('Tidak ada enemy!'), nl.
attack :-
    \+ currweapon(_,_), !, write('You have no weapon!').
attack :-
    currweapon(_, X), X == 0, !, write('You have no ammo!').
attack :- 
    player(LPosition), position(Z,LPosition), enemy(Z), retract(position(Z,LPosition)), !, write('Enemy killed!'), reduceAmmo, nl.

reduceAmmo :-
    currweapon(NamaWeapon, X), Y is X-1, retract(currweapon(NamaWeapon, X)), asserta(currweapon(NamaWeapon, Y)).

% periodicDrop :- 
periodicDrop :-
    waktu(Waktu), Y is mod(Waktu,3),Y == 0, Block is (Waktu//3)+1,

    % randomly place weapon
    forall((random(1, 1, N), between(1, N, _)), forall(weapon(Z), (X1 is 0 + Block, Y1 is 17 - Block, random(X1, Y1, A), random(X1, Y1, B), asserta(position(Z, [A, B]) ) ) ) ),

    % randomly place medicine
    forall((random(1, 1, N), between(1, N, _)), forall(medicine(Z), (X1 is 0 + Block, Y1 is 17 - Block, random(X1, Y1, A), random(X1, Y1, B), asserta(position(Z, [A, B]) ) ) ) ),

    write('Supply drop has arrived, go hunting!'),nl.
periodicDrop.

% move enemy toward player
moveEnemy :- forall((position(Z,[X,Y]), enemy(Z)), (retract(position(Z,[X,Y])), random(1,4,N), movePosition(Z,X,Y,N)) ).

movePosition(Z,X,Y,N) :-
    ((N==1), K is X+1, (asserta(position(Z,[K,Y]))));
    ((N==2), K is X-1, (asserta(position(Z,[K,Y]))));
    ((N==3), K is Y+1, (asserta(position(Z,[X,K]))));
    ((N==4), K is Y-1, (asserta(position(Z,[X,K])))).

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