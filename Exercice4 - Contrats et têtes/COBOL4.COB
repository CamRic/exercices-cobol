       IDENTIFICATION DIVISION.
       PROGRAM-ID. COBOLEX4.


       ENVIRONMENT DIVISION.

       CONFIGURATION SECTION.

       SOURCE-COMPUTER. JVM WITH DEBUGGING MODE.
       OBJECT-COMPUTER. JVM.
       SPECIAL-NAMES. DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       SELECT FICENT  ASSIGN  TO 'FICENT'
                      ORGANIZATION IS LINE SEQUENTIAL
                      ACCESS IS SEQUENTIAL
                      FILE STATUS IS L-Fst.

       SELECT FICSOR   ASSIGN TO 'FICSOR'
                       ORGANIZATION IS LINE SEQUENTIAL
                       FILE STATUS IS L-Fst2.


       DATA DIVISION.

       FILE SECTION.

       FD  FICENT.
       01  E-FICENT.
           05 E-FICENT-TypEnr                      PIC X.
           05 E-FICENT-Donnees                     PIC X(46).


       FD FICSOR.
       01 S-FICSOR                                 PIC X(46).



       WORKING-STORAGE SECTION.

       01 WS-DATA.
           05 RECORD-TYPE                          PIC X.
               88 RECORD-TYPE-C                    VALUE 'C'.
               88 RECORD-TYPE-T                    VALUE 'T'.
           05 RECORD-SIT                           PIC X.
               88 RECORD-SIT-CRS                   VALUE 'C'.
               88 RECORD-SIT-ITC                   VALUE 'I'.

       LOCAL-STORAGE SECTION.

       01 L-Pgm                               PIC X(8) VALUE 'COBOLEX4'.

      *-- file status fichier
       01 L-Fst                                   PIC 99.
       01 L-Fst2                                  PIC 99.

       01 L-FinFic                                PIC X.
           88 L-FinFic-OK                         VALUE 'O'.

       01 L-Nbr.
           05 L-NbrEnrLus                         PIC 9(5).
           05 L-NbrEnrTrt                         PIC 9(5).
           05 L-NbrEnrEcr                         PIC 9(5).

      *-- DONNEES



       01 L-TAB-DATA.
           05 L-CTR.
               10 L-CTR-HDR                        PIC X.
               10 L-CTR-CODPRD                     PIC XX.
               10 L-CTR-NUMPOL                     PIC 9(7).
               10 L-CTR-SIT                        PIC X.

               10 L-CTR-TET                        OCCURS 99 TIMES
                                                   INDEXED BY IND-TET.
                   15 L-CTR-TET-HDR                PIC X.
                   15 L-CTR-TET-TIT                PIC X(3).
                   15 L-CTR-TET-NOM                PIC X(20).
                   15 L-CTR-TET-PRN                PIC X(20).
                   15 L-CTR-TET-SIT                PIC X.


      *-- UTILS
       01 L-NEWCONTRACT                            PIC 9 VALUE 0.
       01 L-NBR-TET                                PIC 99.

       PROCEDURE DIVISION.

       SQUELETTE.

           PERFORM INIT
       *> Lecture initiale
           PERFORM LECTURE-FICHIER

      *>              VERIFIE SI PREMIERE ENTREE EST UNE LIGNE CONTRAT
               IF E-FICENT-TypEnr <> 'C'
                  DISPLAY 'ERROR FILE ENTRY INVALID'
                  PERFORM ERREUR
               END-IF

           PERFORM UNTIL L-FinFic-OK
                PERFORM LOAD-CTR-TET
                PERFORM UPDATE-CONTRAT
                PERFORM WRITE-CONTRAT
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
                      L-TAB-DATA

           OPEN INPUT FICENT
           *> controle que l'ouverture du fichier c'est bien faite
           IF L-Fst NOT = ZERO
              DISPLAY 'Erreur ouverture fichier FS =' L-Fst '>'
              PERFORM ERREUR
           END-IF

           OPEN OUTPUT FICSOR
           IF L-Fst2 NOT = 0
              DISPLAY 'Erreur ouverture fichier FS =' L-Fst2 '>'
              PERFORM ERREUR
           END-IF
           .
      *----------------------------------------------------------------------------
       LECTURE-FICHIER.
      *-----------
           READ FICENT
           AT END
             SET L-FinFic-OK                  TO TRUE

           NOT AT END
              IF L-Fst NOT = ZERO
                 DISPLAY 'Erreur lecture fichier FS =' L-Fst '>'
                 PERFORM ERREUR
              END-IF


              ADD 1                            TO L-NbrEnrLus
           END-READ


           .
      *----------------------------------------------------------------------------
       LOAD-CTR-TET.
      *-----------


           SET L-NEWCONTRACT TO 0

           MOVE 'C' TO L-CTR-HDR

           MOVE E-FICENT-Donnees(1:2) TO L-CTR-CODPRD
           MOVE E-FICENT-Donnees(3:7) TO L-CTR-NUMPOL


           SET IND-TET TO 1


           PERFORM UNTIL L-NEWCONTRACT = 1

               PERFORM LECTURE-FICHIER

               IF E-FICENT-TypEnr = 'T' AND NOT L-FinFic-OK
                   MOVE E-FICENT TO L-CTR-TET(IND-TET)
                   ADD 1 TO IND-TET

               ELSE
                   MOVE 1 TO L-NEWCONTRACT
               END-IF

           END-PERFORM

           COMPUTE L-NBR-TET = IND-TET

           CONTINUE
           .

       UPDATE-CONTRAT.

           MOVE 'C' TO L-CTR-SIT

           PERFORM VARYING IND-TET FROM 1 BY 1 UNTIL IND-TET = L-NBR-TET
               IF L-CTR-TET-SIT(IND-TET) = 'I'
                   MOVE 'I' TO L-CTR-SIT
               END-IF
           END-PERFORM

           CONTINUE
           .


       WRITE-CONTRAT.


           MOVE L-CTR(1:11) TO S-FICSOR
           PERFORM WRITE-S-FICSOR

           PERFORM VARYING IND-TET FROM 1 BY 1 UNTIL IND-TET = L-NBR-TET

               MOVE L-CTR-TET(IND-TET) TO S-FICSOR
               PERFORM WRITE-S-FICSOR

           END-PERFORM


           CONTINUE
           .

       WRITE-S-FICSOR.

           DISPLAY 'WRITING RECORD'
           WRITE S-FICSOR END-WRITE
           ADD 1 TO L-NbrEnrEcr
           CONTINUE
           .

      *----------------------------------------------------------------------------
       FIN-TRT.
      *----
           CLOSE FICENT
           CLOSE FICSOR
           Display 'Nbre enregs lus =' L-NbrEnrLus '>'
           DISPLAY 'Nbre enregs ecris =' L-NbrEnrEcr '>'
           DISPLAY '*************************************************'
           DISPLAY '      FIN   PROGRAMME ' L-Pgm
           DISPLAY '*************************************************'
           GOBACK.


      *----------------------------------------------------------------------------
       ERREUR.
      *----
           DISPLAY 'Fin anormale'
           PERFORM FIN-TRT.
