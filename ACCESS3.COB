      ******************************************************************
      * AUTHOR:
      * DATE:
      * PURPOSE:
      * TECTONICS: COBC
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. ACCESS3.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT KSDASS ASSIGN TO DDKSDASS
           ORGANIZATION     IS INDEXED
           ACCESS           IS DYNAMIC
           RECORD KEY       IS KSDASS-KEY
           FILE STATUS      IS KSDASS-FS.
       DATA DIVISION.
       FILE SECTION.
       FD  KSDASS.
       01  KSDASS-REC.
           05  KSDASS-KEY         PIC 9(6).
           05  KSDASS-RECORD      PIC X(74).

       WORKING-STORAGE SECTION.

       01  KSDASS-FS              PIC 99 VALUE ZEROES.

       01  ASSURES.
           05 MATRICULE           PIC 9(6).
           05 NOM-PRENOM          PIC X(20).
           05 RUE-ADRESSE         PIC X(18).
           05 CODE-POSTAL         PIC 9(5).
           05 VILLE               PIC X(12).
           05 TYPE-VEHICULE       PIC X.
           05 PRIME-BASE          PIC 9(4)V99.
           05 BONUS-MALUS         PIC X.
           05 TAUX                PIC 99.
           05                     PIC X(9).

       LINKAGE SECTION.

       01  ZASSURES.
           05 ZCODE-FONC          PIC X.
           05 ZASSURESRED.
              10 ZMATRICULE       PIC 9(6).
              10 ZNOM-PRENOM      PIC X(20).
              10 ZRUE-ADRESSE     PIC X(18).
              10 ZCODE-POSTAL     PIC 9(5).
              10 ZVILLE           PIC X(12).
              10 ZTYPE-VEHICULE   PIC X.
              10 ZPRIME           PIC 9(4)V99.
              10 ZBONUS-MALUS     PIC X.
              10 ZTAUX            PIC 99.
              10                  PIC x(9).
           05 ZCODE-RET           PIC 99.
           05 ZLIBERR             PIC X(50).

       PROCEDURE DIVISION USING ZASSURES.

           PERFORM DEBUT

            EVALUATE ZCODE-FONC
      * LECTURE D'UN EMPLOYE, RECUPERATION DES INFOS DE L'EMPLOYE
              WHEN 'L'
                PERFORM LECTURE-EMPLOYE
      * CREATION D'UN EMPLOYE
              WHEN 'C'
                PERFORM CREATION-EMPLOYE
      * MODIFICATION D'UN EMPLOYE
              WHEN 'M'
                PERFORM MODIFICATION-EMPLOYE
      * SUPPRESSION D'UN EMPLOYE
              WHEN 'S'
                PERFORM SUPPRESSION-EMPLOYE
      * LECTURE SEQUENTIELLE
              WHEN 'Q'
                PERFORM LECTURE-SEQ
      *> * FERMETURE DU CURSEUR
      *>         WHEN 'F'
      *>           PERFORM FERMETURE-LISTE
              WHEN OTHER
                MOVE 1 TO ZCODE-RET
                DISPLAY 'CODE FONCTION ERRONE'
            END-EVALUATE
            PERFORM FIN
           .


      *    OUVERTURE DU FICHIER + TEST FS
       DEBUT.
           OPEN I-O KSDASS
           IF KSDASS-FS NOT = ZEROES
              DISPLAY 'ERREUR OPEN KSDASS, FS : ' KSDASS-FS
              MOVE '99' TO ZLIBERR
              PERFORM FIN
           END-IF
           .


      *    LECTURE EMPLOYE KSDASS
       LECTURE-EMPLOYE.
           MOVE ZMATRICULE TO KSDASS-KEY
           READ KSDASS INTO ASSURES
            IF KSDASS-FS = '00'
               MOVE 'M' TO ZCODE-FONC
            ELSE
               IF KSDASS-FS = '23'
                  DISPLAY 'ATTENTION CODE ' KSDASS-KEY ' INEXISTANT ! '
                  MOVE 'C' TO ZCODE-FONC
               END-IF
            END-IF
           IF KSDASS-FS NOT = ZEROES AND 10
              DISPLAY 'ERREUR READ KSD, FS : ' KSDASS-FS
              MOVE '99' TO ZLIBERR
           PERFORM FIN
              END-IF
           .

      *    CREATION EMPLOYE DANS LE FICHIER KSDS
       CREATION-EMPLOYE.
           MOVE ZASSURESRED TO KSDASS-REC
           WRITE KSDASS-REC
           .

      *    MODIFICATION EMPLOYE DANS LE FICHIER KSDS
       MODIFICATION-EMPLOYE.
           MOVE ZASSURESRED TO KSDASS-REC
           READ KSDASS INTO ASSURES
           IF ZASSURESRED = ASSURES
              DISPLAY "AUNCUNE MODIFICATION, PAS DE CHANGEMENT"
           ELSE
              MOVE ZASSURESRED TO KSDASS-REC
              REWRITE KSDASS-REC
           .

      *    SUPRESSION EMPLOYE DANS LE FICHIER KSDS
       SUPPRESSION-EMPLOYE.
           MOVE ZASSURESRED TO KSDASS-REC
           READ KSDASS INTO ASSURES
           DELETE KSDASS
           .

      *    LECTURE SEQUENTIELLE DANS LE FICHIER KSDS
       LECTURE-SEQ.
           MOVE ZEROES TO KSDASS-KEY
           .

       FIN.
           CLOSE KSDASS
            GOBACK
           .
