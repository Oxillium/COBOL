      ******************************************************************
      * AUTHOR:
      * DATE:
      * PURPOSE:
      * TECTONICS: COBC
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROJ3.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT MVTASS ASSIGN TO DDMVTASS
           ORGANIZATION IS SEQUENTIAL
           ACCESS IS SEQUENTIAL
           FILE STATUS IS MVTASS-FS.
       DATA DIVISION.
       FILE SECTION.
       FD  MVTASS.
       01  MVTASS-RECORD PIC X(80).


       WORKING-STORAGE SECTION.

       01  MVTASS-FS   PIC 99 VALUE ZEROES.

      *    READ INTO
       01  ASSURES.
           05 MATRICULE PIC 9(6).
           05 NOM-PRENOM PIC X(20).
           05 RUE-ADRESSE PIC X(18).
           05 CODE-POSTAL PIC 9(5).
           05 VILLE PIC X(12).
           05 TYPE-VEHICULE PIC X.
           05 PRIME-BASE PIC 9(4)V99.
           05 BONUS-MALUS PIC X.
           05 TAUX PIC 99.
           05 PIC X(9).

      *    ZONE ACCESSEUR
      *     VALEURS POSSIBLES :
      *                 'L' (LECTURE),
      *                 'C' (CRÉATION),
      *                 'M' (MODIFICATION),
      *                 'S' (SUPPRESSION),
      *                 'O' (OUVERTURE DE LA LISTE),
      *                 'Q' (LECTURE SÉQUENTIELLE),
      *                 'F' (FERMETURE DU KSDS)

       01 ZASSURES.
           05 ZCODE-FONC PIC X.
           05 ZASSURESRED.
              10 ZMATRICULE PIC 9(6).
              10 ZNOM-PRENOM PIC X(20).
              10 ZRUE-ADRESSE PIC X(18).
              10 ZCODE-POSTAL PIC 9(5).
              10 ZVILLE PIC X(12).
              10 ZTYPE-VEHICULE PIC X.
              10 ZPRIME PIC 9(4)V99.
              10 ZBONUS-MALUS PIC X.
              10 ZTAUX PIC 99.
              10 ZCODE-RET PIC 99.
              10 ZLIBERR PIC X(50).

      *    ZCODE-RET CORRESPOND AU NUMÉRO D'ERREUR
      *    CODE 1 POUR MATRICULE INVALIDE
      *    CODE 2 POUR NOM PRENOM NO RENSEIGNE
      *    CODE 3 POUR FIN DE LISTE
      *    CODE 99 POURP ROBLEME SUR FICHIER KSDS

      *    INDICATEUR FIN DE FICHIER MVT-ASS
       01                          PIC X VALUE SPACES.
           88 FIN-MVTASS                VALUE '5'.

       01  COMPTEURS.
           05 CPT-MVT-LUS      PIC 99 VALUE ZEROES.
           05 CPT-MVT-C        PIC 99 VALUE ZEROES.
           05 CPT-MVT-M        PIC 99 VALUE ZEROES.
           05 CPT-MVT-S        PIC 99 VALUE ZEROES.

      * INDICATEUR D'ERREUR
       01                      PIC XX    VALUE SPACES.
           88 OK                         VALUE 'OK'.
           88 ERREUR                     VALUE 'KO'.

       PROCEDURE DIVISION.

           PERFORM DEBUT

      *    1ERE LECTURE MVT, TEST FICHIER VIDE
           PERFORM READ-MVTASS
           IF FIN-MVTASS
           DISPLAY 'ERREUR FICHIER MVT VIDE'
           PERFORM FIN-ERREUR-GRAVE
           END-IF.

      *    APPEL DE LA BOUCLE DE LECTURE DU FICHIER
           PERFORM UNTIL FIN-MVTASS
           PERFORM READ-MVTASS
           PERFORM CONTROLES
           PERFORM TRAITEMENT
           END-PERFORM
           .

      *    FERMETURE DU FICHIER
           PERFORM FIN.

       DEBUT.
           OPEN INPUT MVTASS
           IF MVTASS-FS NOT = ZEROES
              DISPLAY 'ERREUR OPEN MVTASS, FS : ' MVTASS-FS
              PERFORM FIN-ERREUR-GRAVE
           END-IF
           .

      *    CONTROLES DES MOUVEMENT DU FICHIER
       CONTROLES.
           IF ZMATRICULE NOT = SPACE AND ZMATRICULE IS NUMERIC
              IF ZNOM-PRENOM NOT = SPACE
                 MOVE 'L' TO ZCODE-FONC
                 CALL 'ACCESS3' USING ZASSURES
                 ELSE
                     MOVE 'S' TO ZCODE-FONC
                     CALL 'ACCESS3' USING ZASSURES
               END-IF
              ELSE MOVE '1' TO ZLIBERR
           END-IF.

      *    LECTURE MVTASS
       READ-MVTASS.
           READ MVTASS INTO ZASSURESRED
                AT END SET FIN-MVTASS TO TRUE
                NOT AT END ADD 1 TO CPT-MVT-LUS
           END-READ
           IF MVTASS-FS NOT = ZEROES AND 10
              DISPLAY 'ERREUR READ MVT, FS : ' MVTASS-FS
              PERFORM FIN-ERREUR-GRAVE
              END-IF
           .

       TRAITEMENT.
           IF ZCODE-FONC NOT = 'S'
              CALL 'ACCESS3' USING ZASSURES
           END-IF
           .

       FIN-ERREUR-GRAVE.
           MOVE 15 TO RETURN-CODE
           PERFORM FIN
           .
       FIN.
           CLOSE MVTASS
            STOP RUN
           .
