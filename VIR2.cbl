      ******************************************************************
      * Author:RAFAEL
      * Date:01/06/2023
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. VIR2.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

      *    DECLARATION DU FICHIER VIRMVT
           SELECT VIRMVT ASSIGN TO DDVIRMVT
           ORGANIZATION IS SEQUENTIAL
           ACCESS IS SEQUENTIAL
           FILE STATUS IS VIRMVT-FS.

       DATA DIVISION.
       FILE SECTION.

      *    DECLARATION DU BUFFER DU FICHIER VIRMVTMVT
       FD  VIRMVT.
       01  VIRMVT-RECORD                   PIC X(80).

       WORKING-STORAGE SECTION.

      *    DECLARATION DE LA SQLCA / ZONES UTILIES A DB2
      *     EXEC SQL INCLUDE SQLCA END-EXEC.

      * DECLARATION DE LA TABLE VIR_DEVISE
      *     EXEC SQL INCLUDE VIR_DEVISE END-EXEC.

      * DECLARATION DE LA TABLE VIR_CHANGE
      *     EXEC SQL INCLUDE VIR_CHANGE END-EXEC.

      * DECLARATION DE LA TABLE VIR_BIC
      *     EXEC SQL INCLUDE VIR_BIC END-EXEC.


      *    FILE STATUS
       01  VIRMVT-FS   PIC 99 VALUE ZEROES.

      *    DESCRIPTION DE L'ENREGISTREMENT
       01  VIRMVT-ENR.
           05 NUMCLID                        PIC 9(2).
           05 NUMCPTD                        PIC 9(2).
           05 DATECH                         PIC 9(8).
           05 NUMDDE                         PIC 9(2).
           05 DATVAL                         PIC 9(8).
           05 CODDEV                         PIC X(3).
           05 MTTRAN                         PIC S9(9)V9(9).
           05 NOMCLIC                        PIC X(20).
           05 NUMCPTC                        PIC 9(2).
           05 BICCRD                         PIC X(11).
           05 FILLER                         PIC X(12).

      *    ZONE DE COMMUNICATION AVEC ACCESSEUR
       01  Z-VIRMVT-ENR.
           05 Z-CODE-FUNC                    PIC X.
           05 Z-NUMCLID                      PIC 9(2).
           05 Z-NUMCPTD                      PIC 9(2).
      *>      05 Z-DATECH                       PIC 9(8).
      *>      05 Z-NUMDDE                       PIC 9(2).
      *>      05 Z-DATVAL                       PIC 9(8).
      *>      05 Z-CODDEV                       PIC X(3).
           05 Z-MTTRAN                       PIC S9(9)V9(9).
           05 Z-NOMCLIC                      PIC X(20).
      *>      05 Z-NUMCPTC                      PIC 9(2).
      *>      05 Z-BICCRD                       PIC X(11).
           05 Z-CODE-ERR                     PIC 9(2).


      *    ZONE DE COMMUNICATION AVEC SPDAT
       01  ZCOM.
           05 ZDATE.
              10 ZANNEE                      PIC 9(4).
              10 ZMOIS                       PIC 9(2).
              10 ZJOUR                       PIC 9(2).
           05 ZCDRET                         PIC 9.
              88 ZCDRET-OK                   VALUE 0.
              88 ZCDRET-KO                   VALUE 1.
           05 ZDATE-EDIT                     PIC X(17).

      *    ZONE DE COMMUNICATION AVEC VIR_CHANGE POUR RECUPERER LA DEVISE
       01  Z-VIR-CHANGE.
           05 Z-CODDEV1                      PIC 9(2).
           05 Z-CODDEV2                      PIC 9(2).
           05 Z-CHANGE                       PIC S9(9)v9(9).

      *    ZONE DE COMMUNICATION AVEC VIRS3
       01  ZVIRS3.
           05 Z3-DATECH           PIC 9(8).
           05 Z3-NUMDDE           PIC 99.
           05 Z3-CODRET           PIC X.
              88 Z3CODRET-OK                 VALUE '0'.
              88 Z3CODRET-NON-OK             VALUE '1'.

      *    ZONE DE COMMUNICATION
       01  Z1_103.
           05 Z1_TYPMSG  PIC 9(3).
           05 Z1_DATVAL  PIC 9(8).
           05 Z1_CODDEV  PIC X(3).
           05 Z1_MTTRAN  PIC S9(9)V9(9).
           05 Z1_NUMCLIC PIC 9(2).
           05 Z1_NOMCLIC PIC X(15).
           05 Z1_NUMCPTC PIC 9(2).
           05 Z1_BICCRD  PIC X(11).
           05 Z1_CODERET PIC 9(2).
           05 Z1_LIBERR  PIC X(30).

      *    TABLE DES ERREURS
       01  TAB-ERR.
           05                  PIC X(50) VALUE
           '1  - Numéro du compte à débiter non renseigné '.
           05                  PIC X(50) VALUE
           '2  - Numéro du compte à débiter non numérique '.
           05                  PIC X(50) VALUE
           '3  - Numéro du compte à débiter inexistant '.
           05                  PIC X(50) VALUE
           '4  - Date de valeur non renseignée '.
           05                  PIC X(50) VALUE
           '5  - Date de valeur non numérique ou invalide '.
           05                  PIC X(50) VALUE
           '6  - Code devise non renseigné '.
           05                  PIC X(50) VALUE
           '7  - Code devise inexistant '.
           05                  PIC X(50) VALUE
           '8  - Montant à transférer non renseigné '.
           05                  PIC X(50) VALUE
           '9  - Montant à transférer non numérique '.
           05                  PIC X(70) VALUE
           '10 - Montant à transférer sup au solde du cpte à débiter '.
           05                  PIC X(70) VALUE
           '11 - Num du cpte à crder ou nm du clt à crder nn renseigné'.
           05                  PIC X(50) VALUE
           '12 - Banque à créditer non renseignée '.
           05                  PIC X(50) VALUE
           '13 - Le cpte à débiter n appartient pas au client '.
           05                  PIC X(50) VALUE
           '14 - Banque à créditer inexistante '.
           05                  PIC X(50) VALUE
           '80 - Erreur lecture table ST_BIC '.
           05                  PIC X(50) VALUE
           '81 - Erreur lecture table ST_CHANGE '.
           05                  PIC X(50) VALUE
           '82 - Erreur lecture table ST_DEVISE '.
           05                  PIC X(50) VALUE
           '83 - Erreur lecture table ST_MSWIFT '.
           05                  PIC X(50) VALUE
           '84 - Erreur écriture table ST_MSWIFT '.
           05                  PIC X(70) VALUE
           '85 - Erreur appel accesseur (puis le nom de l accesseur) '.
           05                  PIC X(50) VALUE
           '86 - Erreur appel module de date '.

       01  REDEFINES TAB-ERR.
           05 POSTE-ERR             PIC X(50) OCCURS 21.

      * COMPTEURS
       01  CPT-MVT-LUS                  PIC 99 VALUE ZEROES.


      *    INDICATEUR DE FIN DE FICHIER MVT
       01                           PIC X  VALUE SPACES.
           88 FIN-MVT               VALUE 'Y'.

      *    INDICATEUR D'ERREUR
       01                           PIC X  VALUE SPACES.
           88 OK                    VALUE '0'.
           88 ERREUR                VALUE 'Y'.


       PROCEDURE DIVISION.



      *> *    LECTURE VIRMVT
      *>  READ-VIRMVT.
      *>      READ VIRMVT INTO VIRMVT-ENR
      *>           AT END SET FIN-MVT TO TRUE
      *>           NOT AT END ADD 1 TO CPT-MVT-LUS
      *>      END-READ
      *>      IF VIRMVT-FS NOT = ZEROES AND 10
      *>         DISPLAY 'ERREUR READ MVT, FS : ' VIRMVT-FS
      *>         PERFORM FIN-ERREUR-GRAVE
      *>         END-IF
      *>      .



       DEBUT.
           OPEN INPUT VIRMVT
           IF VIRMVT-FS NOT = ZEROES
              DISPLAY 'ERREUR OPEN VIRMVT, FS : ' VIRMVT-FS
              PERFORM FIN-ERREUR-GRAVE
           END-IF
           .

      *    LECTURE DU FICHIER VIRMVT
       LECTURE-EMPLOYE.
           READ VIRMVT INTO VIRMVT-ENR
                AT END SET FIN-MVT TO TRUE
                NOT AT END
                ADD 1 TO CPT-MVT-LUS
           END-READ
           IF VIRMVT-FS NOT = ZEROES AND 10
              DISPLAY 'ERREUR READ MVT, FS : ' VIRMVT-FS
              PERFORM FIN-ERREUR-GRAVE
              END-IF
           .


      *>  CONTROLES.
      *>      PERFORM CTRLNUMCLID
      *>      PERFORM CTRLNUMCPTD
      *>      PERFORM CTRLDATECH
      *>      PERFORM CTRLNUMDDE
      *>      PERFORM CTRLDATVAL
      *>      PERFORM CTRLCODDEV
      *>      PERFORM CTRLMTTRAN
      *>      PERFORM CTRLNOMCLIC
      *>      PERFORM CTRLNUMCLIC
      *>      PERFORM CTRLBICCRD
      *>      .

      *    CONTROLE DU NUMERO DE CLIENT DOIT EXISTER ET ETRE NUMERIC
       CTRLNUMCLID.
           IF NUMCLID = ZEROES OR NUMCLID = SPACES
      *       DISPLAY POSTE-ERR(0)
              SET ERREUR TO TRUE
              ELSE
                 IF NUMCLID NOT NUMERIC
      *             DISPLAY POSTE-ERR(0)
                    SET ERREUR TO TRUE
                    ELSE
                       MOVE 'E' TO Z-CODE-FUNC
                       CALL 'ACCESSEUR-BAC'
                       IF Z-CODE-ERR = 1
      *                   DISPLAY POSTE-ERR(0)
                          SET ERREUR TO TRUE
                          ELSE
                             PERFORM CTRLNUMCPTD
                       END-IF
                 END-IF
           END-IF
           .

      *    CONTROLE DU NUMERO DE COMPTE DOIT EXISTER ET ETRE NUMERIC
       CTRLNUMCPTD.
           IF NUMCPTD = ZEROES OR NUMCPTD = SPACES
              DISPLAY POSTE-ERR(1)
              SET ERREUR TO TRUE
              ELSE
                 IF NUMCPTD NOT NUMERIC
                    DISPLAY POSTE-ERR(2)
                    SET ERREUR TO TRUE
                    ELSE
                       MOVE 'O' TO Z-CODE-FUNC
                       CALL 'ACCESSEUR-BAC'
                       IF Z-CODE-ERR = 1
                          DISPLAY POSTE-ERR(3)
                          SET ERREUR TO TRUE
                          ELSE
                             PERFORM CTRLDATVAL
                       END-IF
                 END-IF
           END-IF
           .

      *    CONTROLE DE LA DATE DOIT EXISTER, NUMERIC ET VALIDE
           CTRLDATVAL.
           IF DATVAL = ZEROES OR DATVAL = SPACES
              DISPLAY POSTE-ERR(4)
              SET ERREUR TO TRUE
              ELSE
                   IF DATVAL NOT NUMERIC
                      DISPLAY POSTE-ERR(5)
                      SET ERREUR TO TRUE
                   ELSE
      *    APPELLE DU SOUS PROGRAMME SPDAT POUR VERIFIER LA VALIDITE
                      MOVE DATECH TO ZDATE
                      CALL 'SPDAT' USING ZCOM
                      IF ZCDRET-KO
      * ---> DATE ENTREE ERRONEE
                         DISPLAY POSTE-ERR(5)
                         SET ERREUR TO TRUE
                         ELSE
                            PERFORM CTRLCODDEV
                      END-IF
                   END-IF
           END-IF
           .

      *--------------------------
       CTRLCODDEV.
           IF CODDEV = SPACES
              DISPLAY POSTE-ERR(6)
              SET ERREUR TO TRUE
              ELSE
                 DISPLAY 'SQL'
      *>            EXEC SQL
      *>             SELECT CODDEV
      *>             INTO :CODDEV
      *>             FROM VIR_DEVISE
      *>             WHERE CODDEV = :CODDEV
      *>            END-EXEC
                  IF SQLCODE = +100
                     DISPLAY POSTE-ERR(7)
                     SET ERREUR TO TRUE
                  END-IF
           END-IF
           .
      *--------------------------

       CTRLMTTRAN.

           IF MTTRAN = ZEROES OR MTTRAN = SPACES
              DISPLAY POSTE-ERR(8)
              SET ERREUR TO TRUE
              ELSE
                 IF MTTRAN NOT NUMERIC
                    DISPLAY POSTE-ERR(9)
                    SET ERREUR TO TRUE
                    ELSE
                       IF Z-CODDEV1 = 'EUR'
                          DISPLAY 'devise déjà en euros, rien a faire'
                          ELSE
                             DISPLAY 'call sql'
                             DISPLAY 'PIERRE'
      *>                     EXEC SQL
      *>                      SELECT CHANGE
      *>                      INTO :Z-CHANGE
      *>                      FROM VIR_DEVISE
      *>                      WHERE CODDEV1 = :Z-CODDEV1
      *>                      AND Z-CODDEV2 = :Z-CODDEV2
      *>                     END-EXEC
                             IF SQLCODE = +100
                                DISPLAY 'ECHEC CALCUL DEVISE'
                                SET ERREUR TO TRUE
                                ELSE
                                   COMPUTE MTTRAN = MTTRAN * Z-CHANGE
                             END-IF
                       END-IF
      *    CODE MOUVEMENT A DEFINIR AVEC BAC POUR VERIFIER LE MONTANT
      *    NE DOIT PAS ETRE SUPERIEUR AU SOLDE DU COMPTE A DEBITER
      *    MONTANT DU COMPTE CLIENT DOIT ETRE SUPERIEUR A MTTRAN
                       CALL 'ACCESSEUR-BAC'
                 END-IF
           END-IF
           .

       CTRLNOMCLIC.
           IF NOMCLIC = ZEROES OR NOMCLIC = SPACES
              IF NUMCPTC = ZEROES OR NUMCPTC = SPACES
                 DISPLAY POSTE-ERR(11)
                 SET ERREUR TO TRUE
              END-IF
           END-IF
           .

       CTRLBICCRD.
           IF BICCRD = ZEROES or BICCRD = SPACES
              DISPLAY POSTE-ERR(12)
              SET ERREUR TO TRUE
              ELSE
                 DISPLAY 'PIERRE'
      *>         EXEC SQL
      *>          SELECT BICCRD
      *>          INTO :BICCRD
      *>          FROM VIR_BIC
      *>          WHERE BICCRD = :BICCRD
      *>         END-EXEC
                 IF SQLCODE = +100
                    DISPLAY POSTE-ERR(14)
                    SET ERREUR TO TRUE
      *    GÉRER L'ERREUR 13
      *    LE COMPTE A DEBITER N'APPARTIENS PAS AU CLIENT
                    ELSE
                       CALL 'ACCESSEUR-BAC'
                       IF Z-CODE-ERR = 1
                          DISPLAY POSTE-ERR(13)
                       END-IF
                 END-IF
           END-IF
           .

       FIN-ERREUR-GRAVE.
           MOVE 15 TO RETURN-CODE
           PERFORM FIN
           .


       FIN.
           CLOSE VIRMVT
            STOP RUN
           .
