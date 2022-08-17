      ******************************************************************
      * Author:    Camille Richard
      * Date:      09/08/2022
      * Purpose:   COBOL EXERCICE 1
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. COBOL_EX_1.


       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT FILE-FICENT ASSIGN TO "FICENT.txt"
           ORGANISATION IS LINE SEQUENTIAL.


       DATA DIVISION.
       FILE SECTION.
       FD FILE-FICENT.
           01  E-FICENT.
               05 E-FICENT-Chaine      PIC 9.
               05 E-FICENT-Jour        PIC 9.
               05 E-FICENT-Tranche     PIC 9.
               05 E-FICENT-NbrSpe      PIC 9(4).

       WORKING-STORAGE SECTION.
       01  WS-FICENT.
           05 WS-FICENT-CHAINE         PIC 9.
           05 WS-FICENT-JOUR           PIC 9.
           05 WS-FICENT-TRANCHE        PIC 9.
           05 WS-FICENT-NBRSPE         PIC 9(4).
       01  END-OF-FILE PIC A.

       01  LINE-COUNT PIC 9(4) VALUE 0.

       01  TABLE-TELE.
           05 CHAINE                           OCCURS 6.
               10 JOUR                         OCCURS 7.
                   15 TRANCHE                  OCCURS 7.
                       20 NB-SPECTATEUR        PIC 9(4).

      *         VARIABLE TO STORE RECORD DATAS
       01  WS-CHAINE PIC 9(1).
       01  WS-JOUR PIC 9(1).
       01  WS-TRANCHE PIC 9(1).
       01  WS-NBRSPE PIC 9(4).
       01  WS-TOTSPE PIC 9(5).

      *        VARIABLE FOR QUESTION 3 + 4 + 5
       01  WS-Q3RESULT PIC 9999.99.
       01  WS-Q4RESULT PIC 9999.99.
       01  WS-Q4BASE PIC 9(5).
       01  WS-Q4JOUR PIC 9 VALUE 1.
       01  WS-Q4TRANCHE PIC 9 VALUE 1.
       01  WS-Q5COMPARE PIC 9(4) VALUE 0.
       01  WS-Q5.
           05 WS-Q5CHAINE PIC 9.
           05 WS-Q5JOUR PIC 9.
           05 WS-Q5TRANCHE PIC 9.
           05 WS-Q5NBRSPEC PIC 9(4).

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
      *        FILLING TABLE-TELE DATAS
            OPEN INPUT FILE-FICENT.
                PERFORM UNTIL END-OF-FILE = "Y"
                   READ FILE-FICENT INTO WS-FICENT
                       AT END
                           MOVE "Y" TO END-OF-FILE
                       NOT AT END
                               ADD WS-FICENT-NBRSPE TO NB-SPECTATEUR
                                  (WS-FICENT-CHAINE
                                   WS-FICENT-JOUR
                                   WS-FICENT-TRANCHE)
                               ADD 1 TO LINE-COUNT
                  END-PERFORM
            CLOSE FILE-FICENT.

      *        DISPLAYING AUDIENCE FOR CHAINE 6 ON PLAGE 6 FOR EACH DAY
            DISPLAY '-------------------'
            DISPLAY 'RESULT FOR QUESTION 2:'
            DISPLAY NB-SPECTATEUR(6 1 6)
            DISPLAY NB-SPECTATEUR(6 2 6)
            DISPLAY NB-SPECTATEUR(6 3 6)
            DISPLAY NB-SPECTATEUR(6 4 6)
            DISPLAY NB-SPECTATEUR(6 5 6)
            DISPLAY NB-SPECTATEUR(6 6 6)
            DISPLAY NB-SPECTATEUR(6 7 6)
            DISPLAY '-------------------'
            DISPLAY 'LINE COUNT: ' LINE-COUNT
            DISPLAY '-------------------'

      *        QUESTION 3
      *        DISPLAYING AUDIENCE PART FOR CHAINE 2 ON TUESDAY FROM 20H TO 21H
            DISPLAY 'RESULT FOR QUESTION 3:'
            COMPUTE WS-TOTSPE = 0
            PERFORM VARYING WS-CHAINE FROM 1 BY 1 UNTIL WS-CHAINE > 6
               ADD NB-SPECTATEUR(WS-CHAINE 2 3) TO WS-TOTSPE
            END-PERFORM
            DISPLAY 'NB SPEC: ' NB-SPECTATEUR(2 2 3)
            DISPLAY 'NB TOT SPEC: ' WS-TOTSPE
            COMPUTE WS-Q3RESULT = NB-SPECTATEUR(2 2 3) / WS-TOTSPE * 100
            DISPLAY 'PART D AUDIENCE DE LA CHAINE 2 LE MARDI '
                      'ENTRE 20H ET 21H: ' WS-Q3RESULT '%'
            DISPLAY '-------------------'.

      *        QUESTION 4
            DISPLAY 'QUESTION 4 - PART DAUDIENCE DE LA CHAINE 4 '
               'TOUS LES JOURS SUR TOUTES LES TRANCHES: '
      *>       Calcul du nbre tot de spectateurs tous les jours et toutes tranches confondues
            COMPUTE WS-TOTSPE = 0
            COMPUTE WS-Q4BASE = 0
            PERFORM VARYING WS-CHAINE FROM 1 BY 1 UNTIL WS-CHAINE > 6
               PERFORM VARYING WS-JOUR FROM 1 BY 1 UNTIL WS-JOUR > 7
                   PERFORM VARYING WS-TRANCHE FROM 1 BY 1
                   UNTIL WS-TRANCHE > 7
                       ADD NB-SPECTATEUR(WS-CHAINE WS-JOUR WS-TRANCHE)
                       TO WS-TOTSPE
                   END-PERFORM
               END-PERFORM
            END-PERFORM
            DISPLAY 'NB TOT SPEC = ' WS-TOTSPE
      *>       calcul de la part daudience de la chaine 4
            PERFORM VARYING WS-JOUR FROM 1 BY 1 UNTIL WS-JOUR > 7
               PERFORM VARYING WS-TRANCHE FROM 1 BY 1
               UNTIL WS-TRANCHE > 7
                   ADD NB-SPECTATEUR(4 WS-JOUR WS-TRANCHE)
                   TO WS-Q4BASE
               END-PERFORM
            END-PERFORM
            COMPUTE WS-Q4RESULT = WS-Q4BASE / WS-TOTSPE * 100
            DISPLAY 'PART DAUDIENCE DE LA CHAINE 4 TOUS LES JOURS '
            'ET TOUTES TRANCHES CONFONDUES: ' WS-Q4RESULT '%.'
      *        QUESTION 5

            DISPLAY '-------------------'.
            DISPLAY 'QUESTION 5 - NBRE MAX SPEC'
            PERFORM VARYING WS-CHAINE FROM 1 BY 1 UNTIL WS-CHAINE > 6
            AFTER WS-JOUR FROM 1 BY 1 UNTIL WS-JOUR > 7
            AFTER WS-TRANCHE FROM 1 BY 1 UNTIL WS-TRANCHE > 7
               IF NB-SPECTATEUR(WS-CHAINE WS-JOUR WS-TRANCHE)
               > WS-Q5COMPARE
                   COMPUTE WS-Q5COMPARE =
                   NB-SPECTATEUR(WS-CHAINE WS-JOUR WS-TRANCHE)
                   COMPUTE WS-Q5CHAINE = WS-CHAINE
                   COMPUTE WS-Q5JOUR = WS-JOUR
                   COMPUTE WS-Q5TRANCHE = WS-TRANCHE
                   COMPUTE WS-Q5NBRSPEC =
                       NB-SPECTATEUR(WS-CHAINE WS-JOUR WS-TRANCHE)

            END-PERFORM.
            DISPLAY 'Le nombre maximum de spectateur était de '
               WS-Q5NBRSPEC ', sur la chaine ' WS-Q5CHAINE
               ' et a été atteint jour ' WS-Q5JOUR ' sur la tranche '
               WS-Q5TRANCHE
            STOP RUN.
       END PROGRAM COBOL_EX_1.
