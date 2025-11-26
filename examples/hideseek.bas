10 REM ===============================================
20 REM     HIDE AND SEEK ADVENTURE GAME
30 REM     A text-based hide and seek game
40 REM     Written in BASIC
50 REM ===============================================
60 REM
70 REM Variable Declarations and Initialization
80 DIM ROOM$(20), HIDINGSPOT$(50), FOUND(50)
90 DIM ROOMCONNECT(20, 4), SPOTINROOM(50)
100 DIM PLAYERNAME$, DIFFICULTY$
110 DIM SCORE, TOTALHIDDEN, TIMEREMAINING, TURNS
120 DIM CURRENTROOM, GAMESEED, HINTSUSED
130 DIM BESTSCORE, GAMESPLAYED
140 REM
150 REM Initialize random number generator
160 RANDOMIZE TIMER
170 REM
180 REM Set initial values
190 SCORE = 0
200 TOTALHIDDEN = 0
210 TIMEREMAINING = 300
220 TURNS = 0
230 CURRENTROOM = 1
240 HINTSUSED = 0
250 BESTSCORE = 0
260 GAMESPLAYED = 0
270 REM
280 REM ===============================================
290 REM     MAIN MENU
300 REM ===============================================
310 CLS
320 GOSUB 9000: REM Display Title Screen
330 PRINT
340 PRINT "     MAIN MENU"
350 PRINT "     ========="
360 PRINT
370 PRINT "     1. New Game"
380 PRINT "     2. Instructions"
390 PRINT "     3. High Scores"
400 PRINT "     4. Credits"
410 PRINT "     5. Exit"
420 PRINT
430 INPUT "     Select an option (1-5): ", CHOICE$
440 IF CHOICE$ = "1" THEN GOTO 500
450 IF CHOICE$ = "2" THEN GOSUB 8000: GOTO 310
460 IF CHOICE$ = "3" THEN GOSUB 8500: GOTO 310
470 IF CHOICE$ = "4" THEN GOSUB 8700: GOTO 310
480 IF CHOICE$ = "5" THEN GOTO 9900
490 GOTO 310
500 REM
510 REM ===============================================
520 REM     GAME SETUP
530 REM ===============================================
540 CLS
550 PRINT "==============================================="
560 PRINT "          GAME SETUP"
570 PRINT "==============================================="
580 PRINT
590 INPUT "Enter your name: ", PLAYERNAME$
600 IF PLAYERNAME$ = "" THEN PLAYERNAME$ = "Player"
610 PRINT
620 PRINT "Select Difficulty:"
630 PRINT "1. Easy   (15 hiding spots, 50 turns)"
640 PRINT "2. Medium (25 hiding spots, 40 turns)"
650 PRINT "3. Hard   (35 hiding spots, 30 turns)"
660 PRINT
670 INPUT "Choose difficulty (1-3): ", DIFF$
680 IF DIFF$ = "1" THEN DIFFICULTY$ = "EASY": TOTALHIDDEN = 15: TURNS = 50
690 IF DIFF$ = "2" THEN DIFFICULTY$ = "MEDIUM": TOTALHIDDEN = 25: TURNS = 40
700 IF DIFF$ = "3" THEN DIFFICULTY$ = "HARD": TOTALHIDDEN = 35: TURNS = 30
710 IF DIFF$ < "1" OR DIFF$ > "3" THEN GOTO 670
720 REM
730 REM Initialize game data
740 GOSUB 1000: REM Setup rooms
750 GOSUB 2000: REM Setup hiding spots
760 GOSUB 3000: REM Place hidden items/people
770 REM
780 CLS
790 PRINT "Game initialized!"
800 PRINT "You are seeking ", TOTALHIDDEN, " hidden friends."
810 PRINT "You have ", TURNS, " turns to find them all."
820 PRINT
830 PRINT "Press any key to start..."
840 A$ = INPUT$(1)
850 GAMESPLAYED = GAMESPLAYED + 1
860 GOTO 4000: REM Start main game loop
870 REM
880 REM ===============================================
890 REM     SUBROUTINE: SETUP ROOMS
900 REM ===============================================
1000 ROOM$(1) = "Living Room"
1010 ROOM$(2) = "Kitchen"
1020 ROOM$(3) = "Bedroom"
1030 ROOM$(4) = "Bathroom"
1040 ROOM$(5) = "Basement"
1050 ROOM$(6) = "Attic"
1060 ROOM$(7) = "Garage"
1070 ROOM$(8) = "Garden"
1080 ROOM$(9) = "Study"
1090 ROOM$(10) = "Hallway"
1100 ROOM$(11) = "Dining Room"
1110 ROOM$(12) = "Closet"
1120 REM
1130 REM Setup room connections (N, S, E, W)
1140 REM Living Room connections
1150 ROOMCONNECT(1, 1) = 10: REM North to Hallway
1160 ROOMCONNECT(1, 2) = 0: REM South
1170 ROOMCONNECT(1, 3) = 2: REM East to Kitchen
1180 ROOMCONNECT(1, 4) = 0: REM West
1190 REM
1200 REM Kitchen connections
1210 ROOMCONNECT(2, 1) = 0
1220 ROOMCONNECT(2, 2) = 8: REM South to Garden
1230 ROOMCONNECT(2, 3) = 11: REM East to Dining Room
1240 ROOMCONNECT(2, 4) = 1: REM West to Living Room
1250 REM
1260 REM Bedroom connections
1270 ROOMCONNECT(3, 1) = 0
1280 ROOMCONNECT(3, 2) = 10: REM South to Hallway
1290 ROOMCONNECT(3, 3) = 4: REM East to Bathroom
1300 ROOMCONNECT(3, 4) = 12: REM West to Closet
1310 REM
1320 REM Bathroom connections
1330 ROOMCONNECT(4, 1) = 0
1340 ROOMCONNECT(4, 2) = 0
1350 ROOMCONNECT(4, 3) = 0
1360 ROOMCONNECT(4, 4) = 3: REM West to Bedroom
1370 REM
1380 REM Basement connections
1390 ROOMCONNECT(5, 1) = 10: REM North to Hallway
1400 ROOMCONNECT(5, 2) = 0
1410 ROOMCONNECT(5, 3) = 0
1420 ROOMCONNECT(5, 4) = 0
1430 REM
1440 REM Attic connections
1450 ROOMCONNECT(6, 1) = 0
1460 ROOMCONNECT(6, 2) = 10: REM South to Hallway
1470 ROOMCONNECT(6, 3) = 0
1480 ROOMCONNECT(6, 4) = 0
1490 REM
1500 REM Garage connections
1510 ROOMCONNECT(7, 1) = 0
1520 ROOMCONNECT(7, 2) = 0
1530 ROOMCONNECT(7, 3) = 10: REM East to Hallway
1540 ROOMCONNECT(7, 4) = 0
1550 REM
1560 REM Garden connections
1570 ROOMCONNECT(8, 1) = 2: REM North to Kitchen
1580 ROOMCONNECT(8, 2) = 0
1590 ROOMCONNECT(8, 3) = 0
1600 ROOMCONNECT(8, 4) = 0
1610 REM
1620 REM Study connections
1630 ROOMCONNECT(9, 1) = 0
1640 ROOMCONNECT(9, 2) = 0
1650 ROOMCONNECT(9, 3) = 0
1660 ROOMCONNECT(9, 4) = 10: REM West to Hallway
1670 REM
1680 REM Hallway connections (central hub)
1690 ROOMCONNECT(10, 1) = 3: REM North to Bedroom
1700 ROOMCONNECT(10, 2) = 1: REM South to Living Room
1710 ROOMCONNECT(10, 3) = 9: REM East to Study
1720 ROOMCONNECT(10, 4) = 7: REM West to Garage
1730 REM Additional connections from Hallway
1740 REM (Attic up, Basement down handled separately)
1750 REM
1760 REM Dining Room connections
1770 ROOMCONNECT(11, 1) = 0
1780 ROOMCONNECT(11, 2) = 0
1790 ROOMCONNECT(11, 3) = 0
1800 ROOMCONNECT(11, 4) = 2: REM West to Kitchen
1810 REM
1820 REM Closet connections
1830 ROOMCONNECT(12, 1) = 0
1840 ROOMCONNECT(12, 2) = 0
1850 ROOMCONNECT(12, 3) = 3: REM East to Bedroom
1860 ROOMCONNECT(12, 4) = 0
1870 REM
1880 RETURN
1890 REM
1900 REM ===============================================
1910 REM     SUBROUTINE: SETUP HIDING SPOTS
1920 REM ===============================================
2000 REM Living Room hiding spots
2010 HIDINGSPOT$(1) = "Behind the couch"
2020 SPOTINROOM(1) = 1
2030 HIDINGSPOT$(2) = "Under the coffee table"
2040 SPOTINROOM(2) = 1
2050 HIDINGSPOT$(3) = "Behind the curtains"
2060 SPOTINROOM(3) = 1
2070 HIDINGSPOT$(4) = "In the TV cabinet"
2080 SPOTINROOM(4) = 1
2090 REM
2100 REM Kitchen hiding spots
2110 HIDINGSPOT$(5) = "In the pantry"
2120 SPOTINROOM(5) = 2
2130 HIDINGSPOT$(6) = "Under the sink"
2140 SPOTINROOM(6) = 2
2150 HIDINGSPOT$(7) = "In the refrigerator"
2160 SPOTINROOM(7) = 2
2170 HIDINGSPOT$(8) = "Behind the door"
2180 SPOTINROOM(8) = 2
2190 REM
2200 REM Bedroom hiding spots
2210 HIDINGSPOT$(9) = "Under the bed"
2220 SPOTINROOM(9) = 3
2230 HIDINGSPOT$(10) = "In the wardrobe"
2240 SPOTINROOM(10) = 3
2250 HIDINGSPOT$(11) = "Behind the dresser"
2260 SPOTINROOM(11) = 3
2270 HIDINGSPOT$(12) = "Under the blankets"
2280 SPOTINROOM(12) = 3
2290 REM
2300 REM Bathroom hiding spots
2310 HIDINGSPOT$(13) = "In the shower"
2320 SPOTINROOM(13) = 4
2330 HIDINGSPOT$(14) = "Behind the door"
2340 SPOTINROOM(14) = 4
2350 HIDINGSPOT$(15) = "In the linen closet"
2360 SPOTINROOM(15) = 4
2370 REM
2380 REM Basement hiding spots
2390 HIDINGSPOT$(16) = "Behind the furnace"
2400 SPOTINROOM(16) = 5
2410 HIDINGSPOT$(17) = "In the storage boxes"
2420 SPOTINROOM(17) = 5
2430 HIDINGSPOT$(18) = "Under the stairs"
2440 SPOTINROOM(18) = 5
2450 HIDINGSPOT$(19) = "Behind the water heater"
2460 SPOTINROOM(19) = 5
2470 REM
2480 REM Attic hiding spots
2490 HIDINGSPOT$(20) = "In the old trunk"
2500 SPOTINROOM(20) = 6
2510 HIDINGSPOT$(21) = "Behind the boxes"
2520 SPOTINROOM(21) = 6
2530 HIDINGSPOT$(22) = "Under the insulation"
2540 SPOTINROOM(22) = 6
2550 HIDINGSPOT$(23) = "In the corner"
2560 SPOTINROOM(23) = 6
2570 REM
2580 REM Garage hiding spots
2590 HIDINGSPOT$(24) = "Behind the car"
2600 SPOTINROOM(24) = 7
2610 HIDINGSPOT$(25) = "In the toolbox"
2620 SPOTINROOM(25) = 7
2630 HIDINGSPOT$(26) = "Under the workbench"
2640 SPOTINROOM(26) = 7
2650 HIDINGSPOT$(27) = "In the garbage can"
2660 SPOTINROOM(27) = 7
2670 REM
2680 REM Garden hiding spots
2690 HIDINGSPOT$(28) = "Behind the bushes"
2700 SPOTINROOM(28) = 8
2710 HIDINGSPOT$(29) = "In the shed"
2720 SPOTINROOM(29) = 8
2730 HIDINGSPOT$(30) = "Under the deck"
2740 SPOTINROOM(30) = 8
2750 HIDINGSPOT$(31) = "Behind the tree"
2760 SPOTINROOM(31) = 8
2770 REM
2780 REM Study hiding spots
2790 HIDINGSPOT$(32) = "Behind the bookshelf"
2800 SPOTINROOM(32) = 9
2810 HIDINGSPOT$(33) = "Under the desk"
2820 SPOTINROOM(33) = 9
2830 HIDINGSPOT$(34) = "In the filing cabinet"
2840 SPOTINROOM(34) = 9
2850 REM
2860 REM Hallway hiding spots
2870 HIDINGSPOT$(35) = "In the coat closet"
2880 SPOTINROOM(35) = 10
2890 HIDINGSPOT$(36) = "Behind the plant"
2900 SPOTINROOM(36) = 10
2910 REM
2920 REM Dining Room hiding spots
2930 HIDINGSPOT$(37) = "Under the table"
2940 SPOTINROOM(37) = 11
2950 HIDINGSPOT$(38) = "In the china cabinet"
2960 SPOTINROOM(38) = 11
2970 HIDINGSPOT$(39) = "Behind the curtains"
2980 SPOTINROOM(39) = 11
2990 REM
3000 REM Closet hiding spots
3010 HIDINGSPOT$(40) = "On the top shelf"
3020 SPOTINROOM(40) = 12
3030 HIDINGSPOT$(41) = "Behind the clothes"
3040 SPOTINROOM(41) = 12
3050 HIDINGSPOT$(42) = "In the shoe box"
3060 SPOTINROOM(42) = 12
3070 REM
3080 REM Initialize all spots as not found
3090 FOR I = 1 TO 42
3100     FOUND(I) = 0
3110 NEXT I
3120 REM
3130 REM ===============================================
3140 REM     SUBROUTINE: PLACE HIDDEN ITEMS/PEOPLE
3150 REM ===============================================
3160 REM Randomly select hiding spots to have people
3170 HIDDEN = 0
3180 FOR I = 1 TO TOTALHIDDEN
3190     REM Pick a random spot that hasn't been used
3200     SPOTNUM = INT(RND * 42) + 1
3210     IF FOUND(SPOTNUM) = -1 THEN GOTO 3200
3220     FOUND(SPOTNUM) = -1: REM Mark as occupied (not yet found)
3230     HIDDEN = HIDDEN + 1
3240 NEXT I
3250 REM
3260 RETURN
3270 REM
3280 REM ===============================================
3290 REM     MAIN GAME LOOP
3300 REM ===============================================
4000 CLS
4010 GOSUB 5000: REM Display current room
4020 GOSUB 6000: REM Display status
4030 PRINT
4040 PRINT "What do you want to do?"
4050 PRINT "  (N)orth  (S)outh  (E)ast  (W)est"
4060 PRINT "  (U)p  (D)own  (L)ook  (S)earch"
4070 PRINT "  (H)int  (M)ap  (Q)uit"
4080 PRINT
4090 INPUT "Enter command: ", CMD$
4100 CMD$ = LEFT$(UCASE$(CMD$), 1)
4110 REM
4120 REM Process movement commands
4130 IF CMD$ = "N" THEN GOSUB 7000: GOTO 4000
4140 IF CMD$ = "S" THEN GOSUB 7100: GOTO 4000
4150 IF CMD$ = "E" THEN GOSUB 7200: GOTO 4000
4160 IF CMD$ = "W" THEN GOSUB 7300: GOTO 4000
4170 IF CMD$ = "U" THEN GOSUB 7400: GOTO 4000
4180 IF CMD$ = "D" THEN GOSUB 7500: GOTO 4000
4190 REM
4200 REM Process action commands
4210 IF CMD$ = "L" THEN GOSUB 5500: GOTO 4000
4220 IF CMD$ = "S" THEN GOSUB 6500: GOTO 4000
4230 IF CMD$ = "H" THEN GOSUB 6700: GOTO 4000
4240 IF CMD$ = "M" THEN GOSUB 6900: GOTO 4000
4250 IF CMD$ = "Q" THEN GOSUB 9500: GOTO 310
4260 REM
4270 PRINT "Invalid command!"
4280 A$ = INPUT$(1)
4290 GOTO 4000
4300 REM
4310 REM ===============================================
4320 REM     SUBROUTINE: DISPLAY CURRENT ROOM
4330 REM ===============================================
5000 PRINT "==============================================="
5010 PRINT "  CURRENT LOCATION: "; ROOM$(CURRENTROOM)
5020 PRINT "==============================================="
5030 PRINT
5040 REM Display room description
5050 IF CURRENTROOM = 1 THEN PRINT "A cozy living room with a fireplace and comfortable furniture."
5060 IF CURRENTROOM = 2 THEN PRINT "A modern kitchen with stainless steel appliances."
5070 IF CURRENTROOM = 3 THEN PRINT "A bedroom with a large bed and wooden furniture."
5080 IF CURRENTROOM = 4 THEN PRINT "A clean bathroom with white tiles."
5090 IF CURRENTROOM = 5 THEN PRINT "A dark basement filled with old boxes and equipment."
5100 IF CURRENTROOM = 6 THEN PRINT "A dusty attic with old furniture and memorabilia."
5110 IF CURRENTROOM = 7 THEN PRINT "A garage with tools and a car."
5120 IF CURRENTROOM = 8 THEN PRINT "A beautiful garden with flowers and trees."
5130 IF CURRENTROOM = 9 THEN PRINT "A quiet study lined with bookshelves."
5140 IF CURRENTROOM = 10 THEN PRINT "A long hallway connecting various rooms."
5150 IF CURRENTROOM = 11 THEN PRINT "An elegant dining room with a large table."
5160 IF CURRENTROOM = 12 THEN PRINT "A small closet filled with clothes and storage."
5170 PRINT
5180 REM Display available exits
5190 PRINT "Exits: ";
5200 IF ROOMCONNECT(CURRENTROOM, 1) > 0 THEN PRINT "North ";
5210 IF ROOMCONNECT(CURRENTROOM, 2) > 0 THEN PRINT "South ";
5220 IF ROOMCONNECT(CURRENTROOM, 3) > 0 THEN PRINT "East ";
5230 IF ROOMCONNECT(CURRENTROOM, 4) > 0 THEN PRINT "West ";
5240 IF CURRENTROOM = 10 THEN PRINT "Up Down ";
5250 PRINT
5260 RETURN
5270 REM
5280 REM ===============================================
5290 REM     SUBROUTINE: LOOK AROUND
5300 REM ===============================================
5500 CLS
5510 PRINT "You look around carefully..."
5520 PRINT
5530 REM List potential hiding spots in this room
5540 SPOTS = 0
5550 FOR I = 1 TO 42
5560     IF SPOTINROOM(I) = CURRENTROOM THEN
5570         PRINT "  - "; HIDINGSPOT$(I)
5580         SPOTS = SPOTS + 1
5590     END IF
5600 NEXT I
5610 IF SPOTS = 0 THEN PRINT "There are no obvious hiding spots here."
5620 PRINT
5630 PRINT "Press any key to continue..."
5640 A$ = INPUT$(1)
5650 RETURN
5660 REM
5670 REM ===============================================
5680 REM     SUBROUTINE: DISPLAY STATUS
5690 REM ===============================================
6000 PRINT "-----------------------------------------------"
6010 FOUNDCOUNT = 0
6020 FOR I = 1 TO 42
6030     IF FOUND(I) = 1 THEN FOUNDCOUNT = FOUNDCOUNT + 1
6040 NEXT I
6050 PRINT "Found: "; FOUNDCOUNT; "/"; TOTALHIDDEN
6060 PRINT "Turns Left: "; TURNS
6070 PRINT "Score: "; SCORE
6080 PRINT "-----------------------------------------------"
6090 REM
6100 REM Check win condition
6110 IF FOUNDCOUNT = TOTALHIDDEN THEN GOSUB 9200: GOTO 310
6120 REM Check lose condition
6130 IF TURNS <= 0 THEN GOSUB 9400: GOTO 310
6140 RETURN
6150 REM
6160 REM ===============================================
6170 REM     SUBROUTINE: SEARCH FOR HIDDEN PEOPLE
6180 REM ===============================================
6500 PRINT
6510 PRINT "Where do you want to search?"
6520 PRINT
6530 REM List spots in current room
6540 SPOTCOUNT = 0
6550 FOR I = 1 TO 42
6560     IF SPOTINROOM(I) = CURRENTROOM THEN
6570         SPOTCOUNT = SPOTCOUNT + 1
6580         PRINT SPOTCOUNT; ". "; HIDINGSPOT$(I)
6590         SPOTLIST(SPOTCOUNT) = I
6600     END IF
6610 NEXT I
6620 IF SPOTCOUNT = 0 THEN
6630     PRINT "There are no hiding spots here!"
6640     PRINT "Press any key..."
6650     A$ = INPUT$(1)
6660     RETURN
6670 END IF
6680 PRINT
6690 INPUT "Select spot (number): ", SPOTSEL
6700 IF SPOTSEL < 1 OR SPOTSEL > SPOTCOUNT THEN
6710     PRINT "Invalid selection!"
6720     A$ = INPUT$(1)
6730     RETURN
6740 END IF
6750 REM
6760 ACTUALSPOT = SPOTLIST(SPOTSEL)
6770 TURNS = TURNS - 1
6780 PRINT
6790 IF FOUND(ACTUALSPOT) = -1 THEN
6800     PRINT "*** YOU FOUND SOMEONE! ***"
6810     PRINT "They were hiding "; HIDINGSPOT$(ACTUALSPOT)
6820     FOUND(ACTUALSPOT) = 1
6830     SCORE = SCORE + 100
6840     IF TURNS > 10 THEN SCORE = SCORE + (TURNS * 2)
6850     PRINT "You earned "; SCORE; " points!"
6860     FOR I = 1 TO 5
6870         BEEP
6880         FOR J = 1 TO 100: NEXT J
6890     NEXT I
6900 ELSEIF FOUND(ACTUALSPOT) = 1 THEN
6910     PRINT "You already found someone here!"
6920 ELSE
6930     PRINT "Nobody is hiding there."
6940     SCORE = SCORE - 10
6950 END IF
6960 PRINT
6970 PRINT "Press any key..."
6980 A$ = INPUT$(1)
6990 RETURN
7000 REM
7010 REM ===============================================
7020 REM     SUBROUTINE: GET HINT
7030 REM ===============================================
7040 REM (Jump target from line 4230)
6700 PRINT
6710 IF HINTSUSED >= 3 THEN
6720     PRINT "You've used all your hints!"
6730     A$ = INPUT$(1)
6740     RETURN
6750 END IF
6760 PRINT "Getting a hint will cost 50 points."
6770 INPUT "Are you sure? (Y/N): ", YN$
6780 IF LEFT$(UCASE$(YN$), 1) <> "Y" THEN RETURN
6790 SCORE = SCORE - 50
6800 HINTSUSED = HINTSUSED + 1
6810 REM Find a hiding spot with someone in it
6820 FOR I = 1 TO 42
6830     IF FOUND(I) = -1 THEN
6840         PRINT "Hint: Someone is hiding in the "; ROOM$(SPOTINROOM(I))
6850         GOTO 6880
6860     END IF
6870 NEXT I
6880 PRINT "Press any key..."
6890 A$ = INPUT$(1)
6900 RETURN
6910 REM
6920 REM ===============================================
6930 REM     SUBROUTINE: SHOW MAP
6940 REM ===============================================
6950 CLS
6960 PRINT "==============================================="
6970 PRINT "                    MAP"
6980 PRINT "==============================================="
6990 PRINT
7000 PRINT "           [Attic]"
7010 PRINT "              |"
7020 PRINT "    [Bedroom]-[Hallway]-[Study]"
7030 PRINT "        |         |         "
7040 PRINT "    [Closet] [Living Rm] [Garage]"
7050 PRINT "                  |"
7060 PRINT "     [Bath]   [Kitchen]"
7070 PRINT "                  |"
7080 PRINT "             [Garden]"
7090 PRINT
7100 PRINT "    [Dining Rm]"
7110 PRINT
7120 PRINT "    From Hallway: [Basement] (Down)"
7130 PRINT
7140 PRINT "    * = Current Location"
7150 PRINT
7160 PRINT "Press any key..."
7170 A$ = INPUT$(1)
7180 RETURN
7190 REM
7200 REM ===============================================
7210 REM     MOVEMENT SUBROUTINES
7220 REM ===============================================
7230 REM Move North
7240 IF ROOMCONNECT(CURRENTROOM, 1) > 0 THEN
7250     CURRENTROOM = ROOMCONNECT(CURRENTROOM, 1)
7260     TURNS = TURNS - 1
7270 ELSE
7280     PRINT "You can't go that way!"
7290     A$ = INPUT$(1)
7300 END IF
7310 RETURN
7320 REM
7330 REM Move South
7340 IF ROOMCONNECT(CURRENTROOM, 2) > 0 THEN
7350     CURRENTROOM = ROOMCONNECT(CURRENTROOM, 2)
7360     TURNS = TURNS - 1
7370 ELSE
7380     PRINT "You can't go that way!"
7390     A$ = INPUT$(1)
7400 END IF
7410 RETURN
7420 REM
7430 REM Move East
7440 IF ROOMCONNECT(CURRENTROOM, 3) > 0 THEN
7450     CURRENTROOM = ROOMCONNECT(CURRENTROOM, 3)
7460     TURNS = TURNS - 1
7470 ELSE
7480     PRINT "You can't go that way!"
7490     A$ = INPUT$(1)
7500 END IF
7510 RETURN
7520 REM
7530 REM Move West
7540 IF ROOMCONNECT(CURRENTROOM, 4) > 0 THEN
7550     CURRENTROOM = ROOMCONNECT(CURRENTROOM, 4)
7560     TURNS = TURNS - 1
7570 ELSE
7580     PRINT "You can't go that way!"
7590     A$ = INPUT$(1)
7600 END IF
7610 RETURN
7620 REM
7630 REM Move Up (Attic from Hallway)
7640 IF CURRENTROOM = 10 THEN
7650     CURRENTROOM = 6
7660     TURNS = TURNS - 1
7670 ELSE
7680     PRINT "You can't go that way!"
7690     A$ = INPUT$(1)
7700 END IF
7710 RETURN
7720 REM
7730 REM Move Down (Basement from Hallway)
7740 IF CURRENTROOM = 10 THEN
7750     CURRENTROOM = 5
7760     TURNS = TURNS - 1
7770 ELSE
7780     PRINT "You can't go that way!"
7790     A$ = INPUT$(1)
7800 END IF
7810 RETURN
7820 REM
7830 REM ===============================================
7840 REM     SUBROUTINE: INSTRUCTIONS
7850 REM ===============================================
8000 CLS
8010 PRINT "==============================================="
8020 PRINT "            HOW TO PLAY"
8030 PRINT "==============================================="
8040 PRINT
8050 PRINT "OBJECTIVE:"
8060 PRINT "  Find all the hidden friends before you"
8070 PRINT "  run out of turns!"
8080 PRINT
8090 PRINT "COMMANDS:"
8100 PRINT "  N, S, E, W - Move North, South, East, West"
8110 PRINT "  U, D       - Move Up or Down (from Hallway)"
8120 PRINT "  L          - Look around current room"
8130 PRINT "  S          - Search a hiding spot"
8140 PRINT "  H          - Get a hint (costs 50 points)"
8150 PRINT "  M          - View map"
8160 PRINT "  Q          - Quit game"
8170 PRINT
8180 PRINT "SCORING:"
8190 PRINT "  - Finding someone: 100 points"
8200 PRINT "  - Time bonus: 2 points per turn remaining"
8210 PRINT "  - Wrong search: -10 points"
8220 PRINT "  - Using a hint: -50 points"
8230 PRINT
8240 PRINT "TIPS:"
8250 PRINT "  - Use the Look command to see hiding spots"
8260 PRINT "  - The map shows room connections"
8270 PRINT "  - Hints reveal which room has someone hiding"
8280 PRINT "  - Higher difficulty = more to find!"
8290 PRINT
8300 PRINT "Press any key to return..."
8310 A$ = INPUT$(1)
8320 RETURN
8330 REM
8340 REM ===============================================
8350 REM     SUBROUTINE: HIGH SCORES
8360 REM ===============================================
8500 CLS
8510 PRINT "==============================================="
8520 PRINT "            HIGH SCORES"
8530 PRINT "==============================================="
8540 PRINT
8550 PRINT "  Games Played: "; GAMESPLAYED
8560 PRINT "  Best Score:   "; BESTSCORE
8570 PRINT
8580 PRINT "  (Scores are not saved between sessions)"
8590 PRINT
8600 PRINT "Press any key to return..."
8610 A$ = INPUT$(1)
8620 RETURN
8630 REM
8640 REM ===============================================
8650 REM     SUBROUTINE: CREDITS
8660 REM ===============================================
8700 CLS
8710 PRINT "==============================================="
8720 PRINT "              CREDITS"
8730 PRINT "==============================================="
8740 PRINT
8750 PRINT "       HIDE AND SEEK ADVENTURE"
8760 PRINT
8770 PRINT "  A Classic BASIC Game"
8780 PRINT
8790 PRINT "  Programming:  BASIC Language"
8800 PRINT "  Design:       Text Adventure Style"
8810 PRINT "  Genre:        Puzzle/Adventure"
8820 PRINT
8830 PRINT "  Special thanks to all the classic"
8840 PRINT "  text adventure games that inspired"
8850 PRINT "  this project!"
8860 PRINT
8870 PRINT "  Version 1.0"
8880 PRINT
8890 PRINT "Press any key to return..."
8900 A$ = INPUT$(1)
8910 RETURN
8920 REM
8930 REM ===============================================
8940 REM     SUBROUTINE: TITLE SCREEN
8950 REM ===============================================
9000 PRINT "==============================================="
9010 PRINT "                                              "
9020 PRINT "     H  H  I  DDD   EEEEE        &           "
9030 PRINT "     H  H  I  D  D  E            &           "
9040 PRINT "     HHHH  I  D  D  EEE          &           "
9050 PRINT "     H  H  I  D  D  E            &           "
9060 PRINT "     H  H  I  DDD   EEEEE        &           "
9070 PRINT "                                              "
9080 PRINT "    SSS   EEEEE  EEEEE  K  K                "
9090 PRINT "   S      E      E      K K                 "
9100 PRINT "    SSS   EEE    EEE    KK                  "
9110 PRINT "       S  E      E      K K                 "
9120 PRINT "    SSS   EEEEE  EEEEE  K  K                "
9130 PRINT "                                              "
9140 PRINT "==============================================="
9150 RETURN
9160 REM
9170 REM ===============================================
9180 REM     SUBROUTINE: WIN SCREEN
9190 REM ===============================================
9200 CLS
9210 PRINT "==============================================="
9220 PRINT "          CONGRATULATIONS!"
9230 PRINT "==============================================="
9240 PRINT
9250 PRINT "  You found all "; TOTALHIDDEN; " friends!"
9260 PRINT
9270 PRINT "  Final Score: "; SCORE
9280 PRINT "  Turns Used:  "; (50 - TURNS)
9290 PRINT "  Hints Used:  "; HINTSUSED
9300 PRINT
9310 IF SCORE > BESTSCORE THEN BESTSCORE = SCORE: PRINT "  NEW HIGH SCORE!"
9320 PRINT
9330 PRINT "  Well done, "; PLAYERNAME$; "!"
9340 PRINT
9350 PRINT "Press any key to return to menu..."
9360 A$ = INPUT$(1)
9370 RETURN
9380 REM
9390 REM ===============================================
9400 REM     SUBROUTINE: GAME OVER SCREEN
9410 REM ===============================================
9420 CLS
9430 PRINT "==============================================="
9440 PRINT "            GAME OVER"
9450 PRINT "==============================================="
9460 PRINT
9470 PRINT "  You ran out of turns!"
9480 PRINT
9490 FOUNDCOUNT = 0
9500 FOR I = 1 TO 42
9510     IF FOUND(I) = 1 THEN FOUNDCOUNT = FOUNDCOUNT + 1
9520 NEXT I
9530 PRINT "  You found: "; FOUNDCOUNT; " out of "; TOTALHIDDEN
9540 PRINT "  Final Score: "; SCORE
9550 PRINT
9560 IF SCORE > BESTSCORE THEN BESTSCORE = SCORE
9570 PRINT "  Better luck next time, "; PLAYERNAME$; "!"
9580 PRINT
9590 PRINT "Press any key to return to menu..."
9600 A$ = INPUT$(1)
9610 RETURN
9620 REM
9630 REM ===============================================
9640 REM     SUBROUTINE: QUIT CONFIRMATION
9650 REM ===============================================
9500 PRINT
9510 INPUT "Are you sure you want to quit? (Y/N): ", QUIT$
9520 IF LEFT$(UCASE$(QUIT$), 1) = "Y" THEN
9530     PRINT "Thanks for playing!"
9540     RETURN
9550 END IF
9560 RETURN
9570 REM
9580 REM ===============================================
9590 REM     EXIT PROGRAM
9600 REM ===============================================
9900 CLS
9910 PRINT "==============================================="
9920 PRINT "     Thanks for playing HIDE AND SEEK!"
9930 PRINT "==============================================="
9940 PRINT
9950 PRINT "  Total Games Played: "; GAMESPLAYED
9960 PRINT "  Best Score:         "; BESTSCORE
9970 PRINT
9980 PRINT "  See you next time!"
9990 PRINT
10000 END
