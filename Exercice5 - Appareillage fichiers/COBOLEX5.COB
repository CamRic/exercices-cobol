       IDENTIFICATION DIVISION.
       PROGRAM-ID. LECTFIC.

      * Ce template de programme lit un fichier en entrÃ©e
      * A adapter en fonction des besoins
        *> penser Ã  renommer le mot COBOLEX5

       ENVIRONMENT DIVISION.

       CONFIGURATION SECTION.

       SOURCE-COMPUTER. JVM WITH DEBUGGING MODE.
       OBJECT-COMPUTER. JVM.
       SPECIAL-NAMES. DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       SELECT FICENT1  ASSIGN  TO 'FIC1.txt'
                      ORGANIZATION IS LINE SEQUENTIAL
                      FILE STATUS IS L-Fst.
       SELECT FICENT2  ASSIGN  TO 'FIC2.txt'
                      ORGANIZATION IS LINE SEQUENTIAL
                      FILE STATUS IS L-Fst2.


       SELECT FICSOR  ASSIGN  TO 'FICSOR'
                      ORGANIZATION IS LINE SEQUENTIAL
                      FILE STATUS IS L-Fst3.

       SELECT SORT-DATA ASSIGN TO 'SRT-DT'.

       DATA DIVISION.

       FILE SECTION.

       FD  FICENT1.
       01  E-FICENT1.
           05 E-FICENT1-REF                        PIC X(9).
           05 FILLER                               PIC X.
           05 E-FICENT1-CODSIT                     PIC X.
               88 E-FICENT1-CODSIT-CRS             VALUE 'C'.
               88 E-FICENT1-CODSIT-ITC             VALUE 'I'.

       FD FICENT2.
       01 E-FICENT2.
           05 E-FICENT2-REF                        PIC X(9).
           05 FILLER                               PIC X.
           05 E-FICENT2-CODINT                     PIC 9(7).

       FD FICSOR.
       01 S-FICSOR.
           05 S-FICSOR-REF                         PIC X(9).
           05 FILLER                               PIC X.
           05 S-FICSOR-CODSIT                      PIC X.
           05 FILLER                               PIC X.
           05 S-FICSOR-CODINT                      PIC 9(7).


       SD SORT-DATA.
       01 SD-SORT-DATA.
           05 SD-SORT-DATA-REF                     PIC X(9).
           05 SD-SORT-DATA-CODSIT                  PIC X.
           05 SD-SORT-DATA-CODINT                  PIC 9(7).




       WORKING-STORAGE SECTION.

       01 TABLE-DONNEES.
           05 CONTRAT                              OCCURS 99 TIMES
                                                   INDEXED BY IND-CTR.
               10 REFCTR                               PIC X(9).
               10 FILLER                               PIC X.
               10 CODSIT                               PIC X.
               10 FILLER                               PIC X.
               10 CODINT                               PIC X(7).

       01 TempCtr                                  PIC X(19).


       LOCAL-STORAGE SECTION.

       01 L-Pgm                               PIC X(8) VALUE 'COBOLEX5'.

      *-- file status fichier
       01 L-Fst                                   PIC 99.
       01 L-Fst2                                  PIC 99.
       01 L-Fst3                                  PIC 99.

       01 L-FinFic.
           05 L-FinFic1                           PIC X.
               88 L-FinFic1-OK                    VALUE 'O'.
           05 L-FinFic2                           PIC X.
               88 L-FinFic2-OK                    VALUE 'O'.

       01 L-Nbr.
           05 L-NbrEnrLus                         PIC 9(5).
           05 L-NbrEnrEcr                         PIC 9(5).


       01 L-IndI                                  PIC 99.
       01 L-IndJ                                  PIC 99.
       01 L-IndSort                               PIC 99.

       01 L-NewCtr                                PIC 99.


       PROCEDURE DIVISION.

       SQUELETTE.

           PERFORM INIT

           PERFORM UNTIL L-FinFic1-OK
               PERFORM LECTURE-FICHIER1
               PERFORM ENR-DONNEES-FICENT1
           END-PERFORM

           PERFORM UNTIL L-FinFic2-OK
               PERFORM LECTURE-FICHIER2
               PERFORM ENR-DONNEES-FICENT2
           END-PERFORM

           PERFORM SORT-CTR

           PERFORM ECRITURE

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

           SET IND-CTR TO 0
           SET L-IndI TO 1
           SET L-IndJ TO 1
           SET L-NewCtr TO 0

           OPEN INPUT FICENT1
           IF L-Fst NOT = ZERO
              DISPLAY 'Erreur ouverture fichier FS =' L-Fst '>'
              PERFORM ERREUR
           END-IF

           OPEN INPUT FICENT2
           IF L-Fst2 NOT = ZERO
              DISPLAY 'Erreur ouverture fichier FS =' L-Fst2 '>'
              PERFORM ERREUR
           END-IF

           OPEN OUTPUT FICSOR
           IF L-Fst3 NOT = ZERO
              DISPLAY 'Erreur ouverture fichier FS =' L-Fst3 '>'
              PERFORM ERREUR
           END-IF
           .
      *----------------------------------------------------------------------------
       LECTURE-FICHIER1.
      *-----------
           READ FICENT1
           AT END
             SET L-FinFic1-OK                  TO TRUE

           NOT AT END
              IF L-Fst NOT = ZERO
                 DISPLAY 'Erreur lecture fichier FS =' L-Fst '>'
                 PERFORM ERREUR
              END-IF

      *> D       DISPLAY 'E-FICENT1 =' E-FICENT1 '>'
              ADD 1                            TO L-NbrEnrLus
           END-READ


           .


      *----------------------------------------------------------------------------
       LECTURE-FICHIER2.
      *-----------

           READ FICENT2
           AT END
             SET L-FinFic2-OK                  TO TRUE

           NOT AT END
              IF L-Fst2 NOT = ZERO
                 DISPLAY 'Erreur lecture fichier FS =' L-Fst2 '>'
                 PERFORM ERREUR
              END-IF

      *> D       DISPLAY 'E-FICENT2 =' E-FICENT2 '>'
              ADD 1                            TO L-NbrEnrLus
           END-READ


           .



      *----------------------------------------------------------------------------
       ENR-DONNEES-FICENT1.
      *--------------------

           IF NOT L-FinFic1-OK
               ADD 1 TO IND-CTR
               MOVE E-FICENT1-REF TO REFCTR(IND-CTR)
               MOVE E-FICENT1-CODSIT TO CODSIT(IND-CTR)
           END-IF

           CONTINUE
           .


      *----------------------------------------------------------------------------
       ENR-DONNEES-FICENT2.
      *--------------------

           IF NOT L-FinFic2-OK
               SET L-NewCtr TO 0
               PERFORM VARYING L-IndI FROM 1 BY 1 UNTIL L-IndI > IND-CTR
                   IF E-FICENT2-REF = REFCTR(L-IndI)
                       MOVE E-FICENT2-CODINT   TO  CODINT(L-IndI)
                   ELSE
                       ADD 1 TO L-NewCtr
                   END-IF
               END-PERFORM

               IF L-NewCtr = IND-CTR
                   ADD 1 TO IND-CTR
                   MOVE E-FICENT2-REF TO REFCTR(IND-CTR)
                   MOVE E-FICENT2-CODINT   TO  CODINT(IND-CTR)
               END-IF

           END-IF

           CONTINUE
           .


      *----------------------------------------------------------------------------
       SORT-CTR.
      *---------



      *>              TRIER LES CONTRATS PAR REFERENCE
           PERFORM VARYING L-IndI FROM 1 BY 1 UNTIL L-IndI > IND-CTR - 1
               ADD 1 TO L-IndI GIVING L-IndJ
               PERFORM UNTIL L-IndJ > IND-CTR
                   IF REFCTR(L-IndI) > REFCTR(L-IndJ)
                       MOVE CONTRAT(L-IndI) TO TempCtr
                       MOVE CONTRAT(L-IndJ) TO CONTRAT(L-IndI)
                       MOVE TempCtr TO CONTRAT(L-IndJ)
                   END-IF
                   ADD 1 TO L-IndJ
               END-PERFORM
           END-PERFORM


           CONTINUE
           .


      *----------------------------------------------------------------------------
       ECRITURE.
      *-----------

           PERFORM VARYING L-IndI FROM 1 BY 1 UNTIL L-IndI > IND-CTR
               MOVE CONTRAT(L-IndI) TO S-FICSOR
               WRITE S-FICSOR END-WRITE
               ADD 1 TO L-NbrEnrEcr
           END-PERFORM


           CONTINUE
           .
      *----------------------------------------------------------------------------
       FIN-TRT.
      *----
           CLOSE FICENT1 FICENT2 FICSOR
           Display 'Nbre enregs lus =' L-NbrEnrLus '>'
           Display 'Nbre enregs ecris =' L-NbrEnrEcr '>'
           DISPLAY '*************************************************'
           DISPLAY '      FIN   PROGRAMME ' L-Pgm
           DISPLAY '*************************************************'
           GOBACK.


      *----------------------------------------------------------------------------
       ERREUR.
      *----
           DISPLAY 'Fin anormale'
           PERFORM FIN-TRT.
