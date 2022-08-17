       IDENTIFICATION DIVISION.
       PROGRAM-ID. COBOL3-2.
       ENVIRONMENT DIVISION.

       CONFIGURATION SECTION.

       SOURCE-COMPUTER. JVM WITH DEBUGGING MODE.
       OBJECT-COMPUTER. JVM.
       SPECIAL-NAMES. DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       SELECT DECOMPTE  ASSIGN  TO 'DECOMPTE.txt'
                      ORGANIZATION IS LINE SEQUENTIAL
                      FILE STATUS IS L-Fst.

       SELECT OUTPUT-FILE ASSIGN TO 'RESULT2.txt'
                       ORGANIZATION IS LINE SEQUENTIAL
                       FILE STATUS IS L-Fst2.

       DATA DIVISION.

       FILE SECTION.

       FD  DECOMPTE.
       01  E-DECOMPTE.
           05 E-CODEPDT    PIC A(2).
           05 FILLER       PIC A(3).
           05 E-POLICENUM  PIC 9(12).
           05 FILLER       PIC A(1).
           05 E-DATESOIN   PIC 9(7).
           05 FILLER       PIC A(1).
           05 E-MONTANT-U.
               10 E-MONTANT PIC 9(9)V99.

       FD  OUTPUT-FILE.
       01  ENRECR                                   PIC X(70).



       WORKING-STORAGE SECTION.
       LOCAL-STORAGE SECTION.

       01 L-Pgm                               PIC X(8) VALUE 'COBOL3-2'.

      *-- file status fichier
       01 L-Fst                                   PIC 99.
       01 L-Fst2                                  PIC 99.

       01 L-FinFic                                PIC X.
           88 L-FinFic-OK                         VALUE 'O'.

       01 L-Nbr.
           05 L-NbrEnrLus                         PIC 9(5).
           05 L-NbrEnrEcr                         PIC 9(5).
           05 L-NbrEnrTrt                         PIC 9(5).


      *-- SUB-PGROM VARIABLES
       01 L-DATE-YYYYDDD                          PIC 9(7).
       01 L-DATE-YYYYMMDD                         PIC 9(8).

      *-- DONNEES

       01 L-ANNEE-1                               PIC 9(4).
       01 L-INDANNEE-MAX                          PIC 9(2).
       01 L-TMPANNEE                              PIC 9(4).
       01 L-TMPMONT                               PIC 9(9)V99.
       01 L-INDANNEE                              PIC 9(2).
       01 L-INDMOIS                               PIC 9(2).
       01 L-INDI                                  PIC 99.

       01 TABLEAU-ENR.
           05  TABLEAU-ANNEES              OCCURS 99 TIMES.
               10  ENR-MOIS                OCCURS 12 TIMES.
                   15  ENR-MONTMENS        PIC 9(9)V99.

       01 L-OUTFILE.
           05 ECR-MOIS     PIC 99.
           05 FILLER       PIC X(7).
           05 ECR-ANNEE    PIC 9(4).
           05 FILLER       PIC X(7).
           05 ECR-MONTANT  PIC Z.ZZZ.ZZ9,99.

       01 L-OUTFILE-HEADER.
           05 HDR-MOIS     PIC X(4) VALUE 'MOIS'.
           05 FILLER       PIC X(5).
           05 HDR-ANNEE    PIC X(5) VALUE 'ANNEE'.
           05 FILLER       PIC X(6).
           05 HDR-MONTANT  PIC A(19) VALUE 'MONTANT PRESTATIONS'.


       PROCEDURE DIVISION.

       SQUELETTE.

           PERFORM INIT
       *> Lecture initiale
           PERFORM LECTURE-FICHIER

           PERFORM UNTIL L-FinFic-OK
               PERFORM TRAITEMENT
               PERFORM LECTURE-FICHIER
           END-PERFORM

           PERFORM ECRITURE-FICHIER

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

           OPEN INPUT DECOMPTE
           *> controle que l'ouverture du fichier c'est bien faite
           IF L-Fst NOT = ZERO
              DISPLAY 'Erreur ouverture fichier FS =' L-Fst '>'
              PERFORM ERREUR
           END-IF

           OPEN OUTPUT OUTPUT-FILE
           IF L-Fst2 NOT = ZERO
              DISPLAY 'Erreur ouverture fichier FS =' L-Fst2 '>'
              PERFORM ERREUR
           END-IF

           .
      *----------------------------------------------------------------------------
       LECTURE-FICHIER.
      *-----------
           READ DECOMPTE
           AT END
             SET L-FinFic-OK                  TO TRUE

           NOT AT END
              IF L-Fst NOT = ZERO
                 DISPLAY 'Erreur lecture fichier FS =' L-Fst '>'
                 PERFORM ERREUR
              END-IF

              IF L-NbrEnrLus = 0
                  MOVE E-DATESOIN(1:4) TO L-ANNEE-1
                  END-IF

      *> D       DISPLAY 'E-DECOMPTE =' E-DECOMPTE '>'
              ADD 1                            TO L-NbrEnrLus
           END-READ


           .
      *----------------------------------------------------------------------------
       TRAITEMENT.
      *-----------

           MOVE E-DATESOIN TO L-DATE-YYYYDDD

           CALL 'CONVDATE' USING L-DATE-YYYYDDD L-DATE-YYYYMMDD

           MOVE L-DATE-YYYYMMDD(1:4) TO L-TMPANNEE
           MOVE L-DATE-YYYYMMDD(5:2) TO L-INDMOIS
           MOVE E-MONTANT(1:9) TO L-TMPMONT

           COMPUTE L-TMPMONT = L-TMPMONT / 100
           COMPUTE L-INDANNEE = L-TMPANNEE - L-ANNEE-1 + 1

           ADD L-TMPMONT TO ENR-MONTMENS(
               L-INDANNEE L-INDMOIS)

           ADD 1 TO L-NbrEnrTrt


           CONTINUE
           .


      *----------------------------------------------------------------------------
       ECRITURE-FICHIER.
      *----


           PERFORM VARYING L-INDI FROM 1 BY 1 UNTIL
                           L-INDI > L-INDANNEE

               MOVE L-OUTFILE-HEADER TO ENRECR
               WRITE ENRECR END-WRITE

               PERFORM VARYING L-INDMOIS FROM 1 BY 1 UNTIL
                               L-INDMOIS > 12

                           MOVE L-INDMOIS TO ECR-MOIS
                           COMPUTE L-TMPANNEE =
                                   L-ANNEE-1 + L-INDI - 1
                           MOVE L-TMPANNEE TO ECR-ANNEE
                           MOVE ENR-MONTMENS(L-INDI L-INDMOIS)
                                   TO ECR-MONTANT
                           MOVE L-OUTFILE TO ENRECR
                           WRITE ENRECR END-WRITE
                           ADD 1 TO L-NbrEnrEcr

               END-PERFORM
           END-PERFORM



           CONTINUE.


      *----------------------------------------------------------------------------
       FIN-TRT.
      *----
           CLOSE DECOMPTE
           CLOSE OUTPUT-FILE
           Display 'Nbre enregs lus =' L-NbrEnrLus '>'
           Display 'Nbre enregs traites =' L-NbrEnrTrt '>'
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
