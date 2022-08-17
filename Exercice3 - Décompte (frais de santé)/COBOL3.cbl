       IDENTIFICATION DIVISION.
       PROGRAM-ID. COBOLEXERCICE3.
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

       SELECT OUTPUT-FILE ASSIGN TO 'RESULT.txt'
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

       01 L-Pgm                               PIC X(15)
                                              VALUE 'COBOLEXERCICE3'.

      *-- file status fichier
       01 L-Fst                                   PIC 99.
       01 L-Fst2                                  PIC 99.

       01 L-FinFic                                PIC X.
           88 L-FinFic-OK                         VALUE 'O'.

       01 L-Nbr.
           05 L-NbrEnrLus                         PIC 9(5).
           05 L-NbrEnrEcr                         PIC 9(5).
           05 L-NbrEnrTrt                         PIC 9(5).

       01 L-ANNEE_1                               PIC 9(4).
       01 L-INDANNEE-MAX                          PIC 9(2).
       01 L-BISSEXTILE                            PIC 9 VALUE 0.
       01 L-INDANNEE                              PIC 9(2) VALUE 1.
       01 L-INDMOIS                               PIC 9(2).

       01 L-TMPMONT                               PIC 9(9)V99.
       01 L-TMPANNEE                              PIC 9(4).

      *-- DONNEES

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

      *--  CONDITIONS BISSEXTILE
       01 BISSEXTILE                 PIC 9(3).
           88 B-JANVIER              VALUE 1 THRU 31.
           88 B-FEVRIER              VALUE 32 THRU 60.
           88 B-MARS                 VALUE 61 THRU 91.
           88 B-AVRIL                VALUE 92 THRU 121.
           88 B-MAI                  VALUE 122 THRU 152.
           88 B-JUIN                 VALUE 153 THRU 182.
           88 B-JUILLET              VALUE 183 THRU 213.
           88 B-AOUT                 VALUE 214 THRU 244.
           88 B-SEPTEMBRE            VALUE 245 THRU 274.
           88 B-OCTOBRE              VALUE 275 THRU 305.
           88 B-NOVEMBRE             VALUE 306 THRU 335.
           88 B-DECEMBRE             VALUE 336 THRU 366.

       01 NON-BISSEXTILE           PIC 9(3).
           88 JANVIER              VALUE 1 THRU 31.
           88 FEVRIER              VALUE 32 THRU 59.
           88 MARS                 VALUE 60 THRU 90.
           88 AVRIL                VALUE 91 THRU 120.
           88 MAI                  VALUE 121 THRU 151.
           88 JUIN                 VALUE 152 THRU 181.
           88 JUILLET              VALUE 182 THRU 212.
           88 AOUT                 VALUE 213 THRU 243.
           88 SEPTEMBRE            VALUE 244 THRU 273.
           88 OCTOBRE              VALUE 274 THRU 304.
           88 NOVEMBRE             VALUE 305 THRU 334.
           88 DECEMBRE             VALUE 335 THRU 365.

       PROCEDURE DIVISION.

       SQUELETTE.

           PERFORM INIT

           PERFORM LECTURE-FICHIER

           PERFORM UNTIL L-FinFic-OK
                PERFORM TRAITEMENT
                PERFORM LECTURE-FICHIER
           END-PERFORM

           MOVE L-INDANNEE TO L-INDANNEE-MAX

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
               MOVE E-DATESOIN(1:4) TO L-ANNEE_1
               PERFORM CALCUL-BISSEXTILE
           END-IF


              ADD 1                            TO L-NbrEnrLus
           END-READ


           .
      *----------------------------------------------------------------------------
       TRAITEMENT.
      *-----------

      *>              CHANGEMENT D ANNEE?
           IF E-DATESOIN(1:4) <> L-ANNEE_1 + L-INDANNEE - 1
               ADD 1 TO L-INDANNEE
               PERFORM CALCUL-BISSEXTILE
               END-IF

      *>              CALCUL DU MOIS
           PERFORM CALCUL-MOIS
           MOVE E-MONTANT(1:9) TO L-TMPMONT
           COMPUTE L-TMPMONT = L-TMPMONT / 100
           ADD L-TMPMONT TO ENR-MONTMENS(L-INDANNEE L-INDMOIS)

           ADD 1 TO L-NbrEnrTrt

           CONTINUE
           .


      *----------------------------------------------------------------------------
       ECRITURE-FICHIER.
      *-----------



           PERFORM VARYING L-INDANNEE FROM 1 BY 1 UNTIL
                           L-INDANNEE > L-INDANNEE-MAX
      *>          ECRITURE DU HEADER APRES CHAQUE NOUVELLE ANNEE
               MOVE L-OUTFILE-HEADER TO ENRECR
               WRITE ENRECR END-WRITE
      *>          ECRITURE DUNE LIGNE PAR MOIS DE LANNEE
               PERFORM VARYING L-INDMOIS FROM 1 BY 1 UNTIL
                               L-INDMOIS > 12

                           MOVE L-INDMOIS TO ECR-MOIS
                           COMPUTE L-TMPANNEE =
                                   L-ANNEE_1 + L-INDANNEE - 1
                           MOVE L-TMPANNEE TO ECR-ANNEE
                           MOVE ENR-MONTMENS(L-INDANNEE L-INDMOIS)
                                   TO ECR-MONTANT
                           MOVE L-OUTFILE TO ENRECR
                           WRITE ENRECR END-WRITE
                           ADD 1 TO L-NbrEnrEcr

               END-PERFORM
           END-PERFORM


           CONTINUE.
      *----------------------------------------------------------------------------
       DISPLAY-DATA.
      *-----------

           PERFORM VARYING L-INDANNEE FROM 1 BY 1
                           UNTIL L-INDANNEE > L-INDANNEE-MAX
               PERFORM VARYING L-INDMOIS FROM 1 BY 1
                               UNTIL L-INDMOIS > 12

                   DISPLAY 'ANNEE: ' L-INDANNEE ', MOIS: ' L-INDMOIS
                   ', TOTAL: ' ENR-MONTMENS(L-INDANNEE L-INDMOIS)


               END-PERFORM

           END-PERFORM

           CONTINUE.

      *----------------------------------------------------------------------------
       CALCUL-BISSEXTILE.
      *-----------
      *>              ANNEE NON BISSEXTILE PAR DEFAULT
           MOVE 0 TO L-BISSEXTILE
      *>              SI DIVISIBLE PAR 4 ANNEE BISSEXTILE
           IF FUNCTION MOD(E-DATESOIN(1:4) 4) = 0
               COMPUTE L-BISSEXTILE = 1
               END-IF
      *>              SI DIVISIBLE PAR 100 NON BISSEXTILE
           IF FUNCTION MOD(E-DATESOIN(1:4) 100) = 0
               COMPUTE L-BISSEXTILE = 0
               END-IF
      *>              SI DIVISIBLE PAR 400 BISSEXTILE
           IF FUNCTION MOD(E-DATESOIN(1:4) 400) = 0
               COMPUTE L-BISSEXTILE = 1
               END-IF



           CONTINUE.


      *----------------------------------------------------------------------------
       CALCUL-MOIS.
      *-----------

      *>              CONDITION NON BISSEXTILE
           IF L-BISSEXTILE = 0
               MOVE E-DATESOIN(5:3) TO NON-BISSEXTILE
               IF JANVIER
                   MOVE 1 TO L-INDMOIS
               END-IF
               IF FEVRIER
                   MOVE 2 TO L-INDMOIS
               END-IF
               IF MARS
                   MOVE 3 TO L-INDMOIS
               END-IF
               IF AVRIL
                   MOVE 4 TO L-INDMOIS
               END-IF
               IF MAI
                   MOVE 5 TO L-INDMOIS
               END-IF
               IF JUIN
                   MOVE 6 TO L-INDMOIS
               END-IF
               IF JUILLET
                   MOVE 7 TO L-INDMOIS
               END-IF
               IF AOUT
                   MOVE 8 TO L-INDMOIS
               END-IF
               IF SEPTEMBRE
                   MOVE 9 TO L-INDMOIS
               END-IF
               IF OCTOBRE
                   MOVE 10 TO L-INDMOIS
               END-IF
               IF NOVEMBRE
                   MOVE 11 TO L-INDMOIS
               END-IF
               IF DECEMBRE
                   MOVE 12 TO L-INDMOIS
               END-IF
           END-IF


      *>              CONDITIONS BISSEXTILE
           IF L-BISSEXTILE = 1
               MOVE E-DATESOIN(5:3) TO BISSEXTILE
               IF B-JANVIER
                   MOVE 1 TO L-INDMOIS
               END-IF
               IF B-FEVRIER
                   MOVE 2 TO L-INDMOIS
               END-IF
               IF B-MARS
                   MOVE 3 TO L-INDMOIS
               END-IF
               IF B-AVRIL
                   MOVE 4 TO L-INDMOIS
               END-IF
               IF B-MAI
                   MOVE 5 TO L-INDMOIS
               END-IF
               IF B-JUIN
                   MOVE 6 TO L-INDMOIS
               END-IF
               IF B-JUILLET
                   MOVE 7 TO L-INDMOIS
               END-IF
               IF B-AOUT
                   MOVE 8 TO L-INDMOIS
               END-IF
               IF B-SEPTEMBRE
                   MOVE 9 TO L-INDMOIS
               END-IF
               IF B-OCTOBRE
                   MOVE 10 TO L-INDMOIS
               END-IF
               IF B-NOVEMBRE
                   MOVE 11 TO L-INDMOIS
               END-IF
               IF B-DECEMBRE
                   MOVE 12 TO L-INDMOIS
               END-IF
           END-IF

           CONTINUE.

      *----------------------------------------------------------------------------
       FIN-TRT.
      *----
           CLOSE DECOMPTE
           CLOSE OUTPUT-FILE
           Display 'Nbre enregs lus =' L-NbrEnrLus '>'
           Display 'Nbre enregs ECRIS =' L-NbrEnrEcr'>'
           Display 'Nbre enregs TRAITE =' L-NbrEnrTrt'>'
           DISPLAY '*************************************************'
           DISPLAY '      FIN   PROGRAMME ' L-Pgm
           DISPLAY '*************************************************'
           GOBACK.


      *----------------------------------------------------------------------------
       ERREUR.
      *----
           DISPLAY 'Fin anormale'
           PERFORM FIN-TRT.
