       IDENTIFICATION DIVISION.
       PROGRAM-ID. COBOL_EX2.


       ENVIRONMENT DIVISION.

       CONFIGURATION SECTION.

       SOURCE-COMPUTER. JVM WITH DEBUGGING MODE.
       OBJECT-COMPUTER. JVM.
       SPECIAL-NAMES. DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       SELECT COMPTES  ASSIGN  TO 'COMPTES'
                      ORGANIZATION IS LINE SEQUENTIAL
                      FILE STATUS IS L-Fst.

       DATA DIVISION.

       FILE SECTION.

       FD  COMPTES.
       01  E-COMPTES.
           05 E-NUMBER PIC 9 OCCURS 10.



       WORKING-STORAGE SECTION.
       LOCAL-STORAGE SECTION.

       01 L-Pgm                               PIC X(9)
                                              VALUE 'COBOL_EX2'.

      *-- file status fichier
       01 L-Fst                                   PIC 99.

       01 L-FinFic                                PIC X.
           88 L-FinFic-OK                         VALUE 'O'.

       01 L-Nbr.
           05 L-NbrEnrLus                         PIC 9(5).

      *-- Variables pour le stockage des enregistrements
       01 L-Comptes.
           05 L-Number PIC 9 OCCURS 10.

       01 L-RESULTS.
           05 L-RESULT PIC 9(10) OCCURS 5.

      *--  Variables utilitaires
       01 L-IndI PIC 9(10) VALUE 1.
       01 L-IndJ PIC 9(10) VALUE 1.

       01 L-MinTmp PIC 9.
       01 L-Res5Tmp PIC 9(2).
       01 L-ModTmp PIC 9.

       01 L-KEY PIC 9.

       PROCEDURE DIVISION.

       SQUELETTE.

           PERFORM INIT
       *> Lecture initiale
           PERFORM LECTURE-FICHIER

           PERFORM UNTIL L-FinFic-OK
                PERFORM TRAITEMENT
                PERFORM LECTURE-FICHIER
           END-PERFORM

           PERFORM FIN-TRT
           .


      *----------------------------------------------------------------------------
       INIT.
      *-----
           DISPLAY '*************************************************'
           DISPLAY '      DEBUT PROGRAMME ' L-Pgm
           DISPLAY '*************************************************'

           MOVE LOW-VALUE                              TO L-FinFic
           INITIALIZE L-Nbr
                      L-MinTmp
                      L-Comptes
                      L-RESULTS
                      L-Res5Tmp
                      L-ModTmp
                      L-KEY

           OPEN INPUT COMPTES
           *> controle que l'ouverture du fichier c'est bien faite
           IF L-Fst NOT = ZERO
              DISPLAY 'Erreur ouverture fichier FS =' L-Fst '>'
              PERFORM ERREUR
           END-IF
           .
      *----------------------------------------------------------------------------
       LECTURE-FICHIER.
      *-----------
           READ COMPTES
           AT END
             SET L-FinFic-OK                  TO TRUE

           NOT AT END
              IF L-Fst NOT = ZERO
                 DISPLAY 'Erreur lecture fichier FS =' L-Fst '>'
                 PERFORM ERREUR
              END-IF
      *--      Les données lues sont-elles numériques?
              IF E-COMPTES IS NOT NUMERIC
                  DISPLAY 'Erreur: données en lecture invalides.'
                  PERFORM ERREUR
              END-IF

      D       DISPLAY 'E-COMPTES =' E-COMPTES '>'
              ADD 1                            TO L-NbrEnrLus
           END-READ


           .
      *----------------------------------------------------------------------------
       TRAITEMENT.
      *-----------
      *-- Calcul de RESULT1

           DISPLAY 'RECORD ' L-NbrEnrLus ' :'

           COMPUTE L-RESULT(1) = 0
           PERFORM VARYING L-IndI FROM 1 BY 1 UNTIL L-IndI > E-NUMBER(1)
               ADD L-IndI TO L-RESULT(1)
           END-PERFORM

           DISPLAY '------ RESULT1: ' L-RESULT(1)

      *-- Calcul de RESULT2
           COMPUTE L-RESULT(2) = 0
           PERFORM VARYING L-IndI FROM 1 BY 1 UNTIL L-IndI > 5
               ADD E-NUMBER(L-IndI) TO L-RESULT(2)
           END-PERFORM
           DISPLAY '------ RESULT2: ' L-RESULT(2)

      *-- Calcul de RESULT3
           COMPUTE L-RESULT(3) = E-NUMBER(1)
           PERFORM VARYING L-IndI FROM 2 BY 1 UNTIL L-IndI > 4
               IF E-NUMBER(L-IndI) < L-RESULT(3)
                   MOVE E-NUMBER(L-IndI) TO L-RESULT(3)
           END-PERFORM
           DISPLAY '------ RESULT3: ' L-RESULT(3)

      *-- Calcul de RESULT4
           IF E-NUMBER(7) = 0
               COMPUTE L-RESULT(4) = 0
           END-IF
           IF E-NUMBER(7) NOT EQUAL 0 AND E-NUMBER(9) = 0
               COMPUTE L-RESULT(4) = 1
           END-IF
           IF E-NUMBER(7) NOT EQUAL 0 AND E-NUMBER(9) NOT EQUAL 0
               COMPUTE L-RESULT(4) = E-NUMBER(7)**E-NUMBER(9)
           END-IF

           DISPLAY '------ RESULT4: ' L-RESULT(4)

      *-- Calcul de RESULT5
           COMPUTE L-Res5Tmp =
           FUNCTION MOD((E-NUMBER(7) + E-NUMBER(8)) 2)
           IF L-Res5Tmp = 0
               COMPUTE L-RESULT(5) = E-NUMBER(8) + E-NUMBER(10) - 2
           END-IF
           IF L-Res5Tmp NOT EQUAL 0
               COMPUTE L-RESULT(5) = E-NUMBER(8) + E-NUMBER(10) + 1
           END-IF
           DISPLAY '------ RESULT5: ' L-RESULT(5)


      *-- Calcul de la clé
           COMPUTE L-KEY = 0
           PERFORM VARYING L-IndI FROM 1 BY 1 UNTIL L-IndI > 5
               ADD L-RESULT(L-IndI) TO L-KEY
           END-PERFORM
           COMPUTE L-KEY = FUNCTION MOD(L-KEY 10)
           DISPLAY '----------- KEY IS: ' L-KEY

           DISPLAY '_______________________________'
           CONTINUE
           .
      *----------------------------------------------------------------------------
       FIN-TRT.
      *----
           CLOSE COMPTES
           Display 'Nbre enregs lus =' L-NbrEnrLus '>'
           DISPLAY '*************************************************'
           DISPLAY '      FIN   PROGRAMME ' L-Pgm
           DISPLAY '*************************************************'
           GOBACK.


      *----------------------------------------------------------------------------
       ERREUR.
      *----
           DISPLAY 'Fin anormale'
           PERFORM FIN-TRT.
