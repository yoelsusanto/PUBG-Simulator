/* fakta */
:- dynamic(maxInventory/1). %to define the maximum type of item to be stored in inventory
:- dynamic(currweapon/2).
:- dynamic(inventory/2).
:- dynamic(position/2).    % position predicate
:- dynamic(player/1).      % player predicate
:- dynamic(health/1).
:- dynamic(curBag/1).
:- dynamic(armor/1).
:- dynamic(waktu/1).
:- dynamic(play/1).        % is playing predicate
:- dynamic(lose/1).
:- dynamic(win/1).

% ---------------- Item Variations ---------------- %
% weapon variations
weapon('alat_guna_guna').
weapon('sharingan').
weapon('sumpitan').
weapon('voodoo').

% enemy variations
enemy(deadline).
enemy(hantu).
enemy(tubes).

% medicine
medicine(kitab_suci).
medicine(bola_naga).

% armor
variasiArmor(alat_ibadah).
variasiArmor(perkamen).

% ammo
ammo('alat_guna_guna', 'sesajen').
ammo('sharingan', 'tenaga_dalam').
ammo('sumpitan', 'anak_sumpit').
ammo('voodoo', 'boneka_voodoo').

% bag
bag(selempang_suci).
bag(koper_suci).

% set locations area
location(10, 1, 15, 6, 'padang pasir').
location(6, 7, 12, 15, 'labtek VI santuy').
location(13, 7, 15, 15, 'CC barat').
location(7, 1, 9, 6, 'hutan gelap').
location(1, 7, 5, 15, 'labtek V terkutuk').
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
    
    % randomly place bag
    forall((random(24, 25, N), between(1, N, _)), forall(bag(Z), (random(1, 16, A), random(1, 16, B), asserta(position(Z, [A, B]) ) ) ) ),
    
    % set max inventory
    asserta(maxInventory(10)),
    asserta(curBag(smallBag)),

    % erase play from false to true
    retract(play(false)),
    asserta(play(true)),

    % set waktu to zero
    asserta(waktu(0)),

    % default health is 100
    asserta(health(100)),

    % starting armor is 20
    asserta(armor(20)),

    % current weapon is none
    % currweapon(nama_weapon, jml peluru untuk weapon itu)
    asserta(currweapon(none, 0)),
    
    % set play to default false
    asserta(win(false)),
    asserta(lose(false)),

    % print required texts
    printHeader,printHelp.

save(_) :- play(X), X == false, !, write('Mulai main game dulu pake "start." ya.'), fail.

save(S) :- inventory(Ia, Ib), health(H), armor(A), currweapon(Cw, Ca), position(X, Y), player(P), play(Pl), waktu(W), curBag(Cb),
        tell(S),
        write(H), write('.'), nl,
        write(A), write('.'), nl,
        write(Cw), write('.'), nl,
        write(Ca), write('.'), nl,
        write(P), write('.'), nl,
        write(Pl), write('.'), nl,
        write(W), write('.'), nl,
        write(Cb), write('.'), nl,
        write(X), write('.'), nl,
        write(Y), write('.'), nl,
        forall((position(Xl,Yl), Yl \== Y, Yl \== none) , (write(Xl), write('.'), nl, write(Yl), write('.'), nl)),
        write('done.'), nl,
        write(Ia), write('.'), nl,
        write(Ib), write('.'), nl,
        forall((inventory(Ialagi, Iblagi), Ialagi \== Ia, Ialagi \== none), (write(Ialagi), write('.'), nl, write(Iblagi), write('.'), nl)),
        write('done.'),
        told, !.

loads(_) :- play(X), X == false, !, write('Mulai main game dulu pake "start." ya.'), fail.

loads(L) :- retractall(inventory(_,_)), retract(health(_)), retract(armor(_)), retract(currweapon(_, _)), retractall(position(_, _)), retract(player(_)), retract(play(_)), retract(waktu(_)), 
        see(L),
        read(H), read(A), read(Cw), read(Ca), read(P), read(Pl), read(W), read(Cb), read(X), read_position(X), read(I), read_inventory(I),
        seen,
        asserta(health(H)), asserta(armor(A)), asserta(currweapon(Cw, Ca)), asserta(player(P)), asserta(waktu(W)), asserta(play(Pl)), asserta(curBag(Cb)), !. 
end_of_everything('done').


read_position(X) :- end_of_everything(X), !.
read_position(X):- 
    \+end_of_everything(X), 
    read(Y),
    asserta(position(X, Y)),
    read(Z),
    read_position(Z).

read_inventory(I) :- end_of_everything(I), !. 
read_inventory(X):- 
    \+ end_of_everything(X), 
    read(Y),
    asserta(inventory(X, Y)),
    read(I), 
    read_inventory(I).

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
map :- play(X), X == false, !, write('Mulai main game dulu pake "start." ya.'), fail.
map :- waktu(X), Block is X//3, printMap(0,0,Block+1).

printMap(X,Y,DeadZone) :- player([X,Y]) , write('P'), write(' '), Xa is X+1, printMap(Xa,Y,DeadZone),!.
printMap(X,Y,_) :- X>=17, Y>=16.
printMap(X,Y,DeadZone) :- X==17, nl, Ya is Y + 1, printMap(0,Ya,DeadZone), !.
printMap(X,Y,DeadZone) :-
    ((X < DeadZone; X >= (17-DeadZone); Y < DeadZone; Y >= (17-DeadZone)), write('X'); write('-')), !,
    write(' '),
    Xa is X+1, printMap(Xa,Y,DeadZone).

% move player position (Final)
n :- play(X), X == false, !, write('Mulai main game dulu pake "start." ya.'), fail.
n :- player([X, Y]), Yn is Y - 1, isDeadZone(X,Yn), write('Hei, kau gila ya ingin ke tempat yang sudah dipenuhi kegelapan!'),!.
n :- player([X, Y]), Yn is Y - 1, retractall(player(_)), asserta(player([X, Yn])), write('oke'), areaAround, write('oke1'), updateGame, write('oke2'), !.

s :- play(X), X == false, !, write('Mulai main game dulu pake "start." ya.'), fail.
s :- player([X, Y]), Yn is Y + 1, isDeadZone(X,Yn), write('Hei, kau gila ya ingin ke tempat yang sudah dipenuhi kegelapan!'),!.
s :- player([X, Y]), Yn is Y + 1, retractall(player(_)), asserta(player([X, Yn])), write('oke'), areaAround, write('oke1'), updateGame, write('oke2'), !.

e :- play(X), X == false, !, write('Mulai main game dulu pake "start." ya.'), fail.
e :- player([X, Y]), Xn is X + 1, isDeadZone(Xn,Y), write('Hei, kau gila ya ingin ke tempat yang sudah dipenuhi kegelapan!'),!.
e :- player([X, Y]), Xn is X + 1, retractall(player(_)), asserta(player([Xn, Y])), write('oke'), areaAround, write('oke1'), updateGame, write('oke2'), !.

w :- play(X), X == false, !, write('Mulai main game dulu pake "start." ya.'), fail.
w :- player([X, Y]), Xn is X - 1, isDeadZone(Xn,Y), write('Hei, kau gila ya ingin ke tempat yang sudah dipenuhi kegelapan!'),!.
w :- player([X, Y]), Xn is X - 1, retractall(player(_)), asserta(player([Xn, Y])), write('oke'), areaAround, write('oke1'), updateGame, write('oke2'), !.

% Dipanggil pada saat ada perintah move
areaAround :-
    player([X, Y]), N is Y - 1, S is Y + 1, E is X + 1, W is X - 1,
    (location(Xmin, Ymin, Xmax, Ymax, LocationName),
    (Xmin =< X, X =< Xmax, Ymin =< Y, Y =< Ymax, !, write('Kamu sedang berada di '), write(LocationName), write('. '))),
    ((isDeadZone(X, N)) -> (write('Disebelah utara adalah zona kebinasaan! ')); (location(XminN, YminN, XmaxN, YmaxN, LocationNName),
    (XminN =< X, X =< XmaxN, YminN =< N, N =< YmaxN, !, write('Disebelah utara adalah '), write(LocationNName), write('. ')))),
    ((isDeadZone(E, Y)) -> (write('Disebelah timur adalah zona kebinasaan! ')); (location(XminE, YminE, XmaxE, YmaxE, LocationEName),
    (XminE =< E, E =< XmaxE, YminE =< Y, Y =< YmaxE, !, write('Disebelah timur adalah '), write(LocationEName), write('. ')))),
    ((isDeadZone(X, S)) -> (write('Disebelah selatan adalah zona kebinasaan! ')); (location(XminS, YminS, XmaxS, YmaxS, LocationSName),
    (XminS =< X, X =< XmaxS, YminS =< S, S =< YmaxS, !, write('Disebelah selatan adalah '), write(LocationSName), write('. ')))),
    ((isDeadZone(W, Y)) -> (write('Disebelah barat adalah zona kebinasaan! ')); (location(XminW, YminW, XmaxW, YmaxW, LocationWName),
    (XminW =< W, W =< XmaxW, YminW =< Y, Y =< YmaxW, !, write('Disebelah barat adalah '), write(LocationWName), write('.')))), nl.

item(X) :- weapon(X), !.
item(X) :- variasiArmor(X), !.
item(X) :- medicine(X), !.
item(X) :- ammo(_, X), !.
item(X) :- bag(X), !.

take(_) :- play(X), X == false, !, write('Mulai main game dulu pake "start." ya.'), fail.
take(X) :- \+item(X), !, write('Fokus mas, barang itu tidak ada!'), fail.
take(X) :- \+nearby(X), !, write('Coba liat lagi sekeliling, '), write(X), write(' tidak ada disekitarmu.'), fail.
take(_) :- countInven(Quantity), maxInventory(Max), Quantity == Max, !, write('Tas sucimu sudah penuh, ayo cari tas yang lebih besar atau buang barang tidak berguna!'), nl, fail.
take(NamaItem) :- 
    addItem(NamaItem,1),
    write('Kamu mengambil '), write(NamaItem), write('.'), nl, player(L), retract(position(NamaItem, L)), !, updateGame.

haventhave(NamaItem) :-
    \+ inventory(NamaItem,_).

addItem(NamaItem,Qty) :-
    haventhave(NamaItem) -> (asserta(inventory(NamaItem,Qty))) ;
    (inventory(NamaItem,JmlAwal), retract(inventory(NamaItem,JmlAwal)), JmlAkhir is JmlAwal+Qty, asserta(inventory(NamaItem,JmlAkhir))).

nearby(X) :- position(X, Lt), player(Lt).

countInven(Count) :-
    findall(X, (inventory(X,_), X \== none), L),
    length(L,Count).

use(_) :- play(X), X == false, !, write('Mulai main game dulu pake "start." ya.'), fail.
use(X) :- \+inventory(X,_), !, write('Tidak ada benda seperti ini di tas sucimu! Apakah kau sudah kehilangan akal sehatmu?'), fail.
% untuk ammo
use(X) :-
    ammo(JenisWeapon, X),
    (( currweapon(JenisWeapon,JlhAmmo), !) -> (inventory(X,Jml), JlhAmmoNew is JlhAmmo + Jml, retract(currweapon(JenisWeapon,JlhAmmo)), asserta(currweapon(JenisWeapon,JlhAmmoNew)), retract(inventory(X,Jml)), !, write('Senjata anda sudah berhasil diisi! Pergunakanlah dengan bijak!') ) ; 
    (!, write('Kamu sedang tidak menggunakan senjata atau bahan yang kamu punya tidak cocok dengan senjatamu! Hati-hati dalam mencampurkan bahan-bahan!')) ), !, nl, updateGame.

% untuk armor
use(X) :- variasiArmor(X), reduceItem(X), retract(armor(Y)), Yn is Y + 20, asserta(armor(Yn)), !, write('Iman anda sudah bertambah! Kendati demikian tetap berhati-hati dan jangan menganggap mudah!'), nl, updateGame.
% untuk weapon
use(X) :- weapon(X), !, reduceItem(X), retract(currweapon(W, Qty)), ammo(W, A), addItem(W, 1), addItem(A, Qty), asserta(currweapon(X, 0)), write(X), write(' berhasil digunakan! Berhati-hatilah, efeknya sangat mematikan!'), nl, updateGame.
% untuk medicine
use(X) :- medicine(X), reduceItem(X), addHealth(X), !, write('Anda baru saja memakan obat! Kewarasan anda sudah dipulihkan sedikit!'), nl, updateGame.
% untuk bag
use(X) :- 
    bag(X), 
    (((curBag(smallBag), X=='selempang_suci')) -> (format('Atas usaha kerasmu, tas sucimu sekarang sudah bisa menampung 15 benda! Gunakan dengan sebaik-baiknya!',[]), asserta(curBag(selempang_suci)), retractall(maxInventory(_)), asserta(maxInventory(15)), retract(curBag(smallBag))) ;
    (((curBag(smallBag) ; curBag(selempang_suci)), X=='koper_suci') -> (format('Atas usaha kerasmu, tas sucimu sekarang sudah bisa menampung 20 benda! Gunakan dengan sebaik-baiknya!',[]), asserta(curBag(koper_suci)), retractall(maxInventory(_)), asserta(maxInventory(20)), (retract(curBag(smallBag));retract(curBag(selempang_suci)))) ;
    (write('Waduh! Kamu gagal berganti tas suci!')))),
    nl, !, updateGame.

addHealth(X) :-
    X=='batu1', health(CurHealth), retract(health(CurHealth)), UpHealth is CurHealth+10,
    ((UpHealth > 100) -> asserta(health(100));asserta(health(UpHealth))).

addHealth(X) :-
    X=='batu2', health(CurHealth), retract(health(CurHealth)), UpHealth is CurHealth+20,
    ((UpHealth > 100) -> asserta(health(100));asserta(health(UpHealth))).

% status command
status :- play(X), X == false, !, write('Mulai main game dulu pake "start." ya.'), fail.
status :-
    health(X), !, write('Kewarasan : '), write(X), nl,
    armor(Y), !, write('Iman : '),  write(Y), nl,
    (currweapon(NamaWeapon,JmlAmmo) -> (format('Alat ilmu hitam : ~w; Ammo : ~w',[NamaWeapon, JmlAmmo]), nl) ; (write('Tidak ada alat ilmu hitam yang sedang kamu pakai! Cepat, carilah alatmu, kalau tidak kamu tidak akan bertahan!'), nl)),
    ((\+ inventory(_,_), !, write('Tas sucimu masih kosong!')) ;
    (findall([Something,Jml],inventory(Something,Jml),L), write('Isi tas suci : '), write(L))), nl.

% help (Final)
help :- play(X), X == false, !, write('Mulai main game dulu pake "start." ya.'), fail.
help :- printHelp.
    
% look
look :- play(X), X == false, !, write('Mulai main game dulu pake "start." ya.'), fail.
look :-
    player([X, Y]),
    (location(Xmin, Ymin, Xmax, Ymax, LocationName),
    (Xmin =< X, X =< Xmax, Ymin =< Y, Y =< Ymax, !, write('Kamu berada di '), write(LocationName), write('! '))),
    forall(position(A, [X, Y]), (((weapon(A)), format('Kamu melihat sebuah ~w yang kosong ',[A]); format('Kamu melihat ada ~w', [A])), write(' didekatmu. '))), nl,
    A is X-1, B is X+1, C is Y-1, D is Y+1,
    printPrio(A, C), write(' '), printPrio(X, C), write(' '), printPrio(B, C), nl,
    printPrio(A, Y), write(' '), printPrio(X, Y), write(' '), printPrio(B, Y), nl,
    printPrio(A, D), write(' '), printPrio(X, D), write(' '), printPrio(B, D), nl.

printPrio(X,Y) :- isDeadZone(X,Y), !, write('X').
printPrio(X,Y) :- position(Z, [X,Y]), enemy(Z), !, write('E').
printPrio(X,Y) :- position(Z, [X,Y]), bag(Z), !, write('B').
printPrio(X,Y) :- position(Z, [X,Y]), medicine(Z), !, write('M').
printPrio(X,Y) :- position(Z, [X,Y]), weapon(Z), !, write('W').
printPrio(X,Y) :- position(Z, [X,Y]), variasiArmor(Z), !, write('A').
printPrio(X,Y) :- position(Z, [X,Y]), ammo(_, Z), !, write('O').
printPrio(X,Y) :- player([X,Y]), !, write('P').
printPrio(_,_) :- write('-').

% Drop Item
drop(_) :- play(X), X == false, !, write('Mulai main game dulu pake "start." ya.'), fail.
drop(X) :- \+inventory(X,_), !, write('Tidak ada benda seperti ini di tas sucimu! Apakah kau sudah kehilangan akal sehatmu?.'), fail.
drop(X) :- weapon(X), currweapon(X, _), !, write('Hei! Alat ilmu hitam yang mau kamu jatuhkan masih kamu pegang erat-erat.'), fail.
drop(X) :- reduceItem(X), player(L), asserta(position(X,L)), write('Kamu telah menjatuhkan '), write(X), updateGame.

reduceItem(X) :-
    inventory(X,Jml), retract(inventory(X,Jml)),
    ((Jml > 1) -> JmlBaru is Jml-1, asserta(inventory(X,JmlBaru)) ; true).
    
unequip :-
    (currweapon(NamaWeapon,Pelor),
    ( (maxInventory(Max), countInven(Qty), Slsh is Max-Qty, Slsh >=2) ;
    ( maxInventory(Max), countInven(Qty), Slsh is Max-Qty, Slsh >=1, (inventory(NamaWeapon,_); inventory(Pelor,_) ) ) ;
    ( maxInventory(Max), countInven(Qty), Slsh is Max-Qty, Slsh == 0, inventory(NamaWeapon,_), inventory(Pelor,_) )  )           )->
    (retract(currweapon(NamaWeapon,Pelor)), ammo(NamaWeapon,NamaPelor), addItem(NamaPelor,Pelor), addItem(NamaWeapon,1), format('~w sudah disimpan di tas sucimu, begitu juga dengan isinya!',[NamaWeapon]));
    format('Sepertinya anda masih cupu! Anda gagal menyimpan alat ilmu hitam anda!~N',[]).

% Check Deadzone
isDeadZone(X,Y) :- waktu(Waktu), Block is (Waktu//3)+1, ((X < Block; X >= (17-Block); Y < Block; Y >= (17-Block))),!.

% Add time
addTime :- retract(waktu(X)), Y is X + 1, asserta(waktu(Y)).

% Update Game (including add time)
updateGame :- addTime, cleanObjects, moveEnemy, attacked, winLose, win(W), lose(L), ((W \== true, L \== true) -> upGame;true), !.
upGame :-periodicDrop.

% clean objects untuk benda-benda yang sudah berada di dead zone.
cleanObjects :- forall((position(Z,[X,Y]), isDeadZone(X,Y)), (retract(position(Z,[X,Y])))).

% get attacked
attacked :-
    player(Lplayer) , forall((position(X,Lplayer), enemy(X)), (reduceHealth(20), write('Kamu sudah terkena ilmu hitam! Kesehatan jiwa anda terancam! Cepatlah berlindung!'))), nl.
attacked.

reduceHealth(Qty) :-
    armor(QArmor),
    (QArmor >= Qty) -> (write('suk1'),(Sisa is QArmor - Qty), retract(armor(QArmor)), asserta(armor(Sisa)));
    (write('suk2'),Sisa is Qty - QArmor, retract(armor(QArmor)), asserta(armor(0)),
    health(CurHealth), retract(health(CurHealth)), NewHealth is CurHealth - Sisa, asserta(health(NewHealth))).

% check if the player has won or lost
winLose :-
    \+ (enemy(X), position(X,[_,_])), retract(win(false)), asserta(win(true)), printWinFalse, quit.
winLose :-
    ((player([X,Y]), isDeadZone(X,Y)) ; (health(Hp), (Hp < 1))), retract(lose(false)), asserta(lose(true)), printWinFalse, quit.
winLose.

printWinFalse :-
    (win(true), write('Harus kuakui! Kamu adalah orang tersakti dinegeri ini! Buatlah negeri ini menjadi negeri yang hebat!'));
    (win(false), write('Sangat mengecewakan! Kamu mati! Roh mu akan menjadi tumbal untuk musuhmu!')).

% realisasi fungsi attack
attack :-
    player(LPosition), \+ position(Z,LPosition), enemy(Z), !, write('Tidak ada enemy!'), nl.
attack :-
    \+ currweapon(_,_), !, write('Mana alat ilmu hitam mu???? Tidak semudah itu ferguso!'), nl.
attack :-
    currweapon(_, X), X == 0, !, write('Hei! Tayo! Maaf maksud saya Hei kamu! Alat ilmu hitam mu tidak ada isinya! Kamu bercanda mas?').
attack :- 
    player(LPosition), position(Z,LPosition), enemy(Z), retract(position(Z,LPosition)), !, write('Kamu berhasil membunuh musuh! Tapi jangan sombong, kamu hanya hoki, dewa tubes sedang memihakmu!'), nl, reduceAmmo, random(1,3,Pil), enemyDieDrop(Pil), nl.

reduceAmmo :-
    currweapon(NamaWeapon, X), Y is X-1, retract(currweapon(NamaWeapon, X)), asserta(currweapon(NamaWeapon, Y)).

% enemyDieDrop
enemyDieDrop(Pil) :-
    (Pil==1) ->

    % randomly place weapon
    (findall(Wep, weapon(Wep), ListWeapon), random_member(Each,ListWeapon), player(LPos), asserta(position(Each, LPos)), format('Musuh malang itu telah meninggalkan ~w, ambil atau tidak, pilihlah dengan bijak!',[Each]));

    % randomly place medicine
    (findall(Med, medicine(Med), ListMed), random_member(Each,ListMed), player(LPos), asserta(position(Each, LPos)), format('Musuh malang itu telah meninggalkan ~w, ambil atau tidak, pilihlah dengan bijak!',[Each])).

% random member
random_member(X, List) :-
    length(List, Len),
    Len > 0,
    random(0, Len, N),
    nth0(N, List, X).

% periodicDrop :- 
periodicDrop :-
    waktu(Waktu), Y is mod(Waktu,3),Y == 0, Block is (Waktu//3)+1,

    % randomly place weapon
    forall((random(1, 2, N), between(1, N, _)), forall(weapon(Z), (X1 is 0 + Block, Y1 is 17 - Block, random(X1, Y1, A), random(X1, Y1, B), asserta(position(Z, [A, B]) ) ) ) ),

    % randomly place medicine
    forall((random(1, 2, N), between(1, N, _)), forall(medicine(Z), (X1 is 0 + Block, Y1 is 17 - Block, random(X1, Y1, A), random(X1, Y1, B), asserta(position(Z, [A, B]) ) ) ) ),

    write('Dewa tubes sedang berbaik hati, dia baru saja menurunkan alat-alat ilmu hitam untuk membantumu! Tapi tidak semudah itu ferguso, anda harus mencarinya! Hohoho!'),nl.
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
    write('Selamat datang di negeri ilmu hitam!'), nl,
    write('Bertahanlah! Jadilah orang terakhir yang masih hidup, maka anda akan dinobatkan sebagai orang sakti di negeri ini!'), nl, nl.

printHelp :-
    write('Available commands:'), nl,
    write('   start.            -- memulai permainan'), nl,
    write('   help.             -- waduh, kamu butuh bantuan?'), nl,
    write('   quit.             -- meng=inggalkan permainan'), nl,
    write('   look.             -- lihat barang disekitarmu'), nl,
    write('   n. s. e. w.       -- bergerak'), nl,
    write('   map.              -- lihat peta seluruh negeri dan posisi kamu sekarang'), nl,
    write('   take(Object).     -- mengambil barang yang ada di dekatmu'), nl,
    write('   drop(Object).     -- mejatuhkan barang yang kamu punya di tas suci'), nl,
    write('   use(Object).      -- menggunakan barang yang ada di tas sucimu'), nl,
    write('   attack.           -- menyerang musuh didekatmu dengan ilmu hitam'), nl,
    write('   status.           -- lihat keadaan mental kamu'), nl,
    write('   save(Filename).   -- sudah lelah bermain? simpan keadaan permainan dengan ini'), nl,
    write('   loads(Filename).  -- sudah siap melanjutkan permainan? lanjutkan dengan ini'), nl, nl,
    write('Legends:'), nl,
    write('   W = alat ilmu hitam'), nl,
    write('   A = menambah keimanan'), nl,
    write('   M = menambah kewarasan'), nl,
    write('   O = bahan untuk alat ilmu hitam'), nl,
    write('   P = posisi kamu sekarang'), nl,
    write('   E = musuh'), nl,
    write('   - = anda bisa kesini'), nl,
    write('   X = daerah kegelapan, tidak ada yang berani ke daerah ini'), nl.