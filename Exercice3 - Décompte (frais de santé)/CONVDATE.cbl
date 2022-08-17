      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CONVDATE.

       ENVIRONMENT DIVISION.
           CONFIGURATION SECTION.

               SOURCE-COMPUTER. JVM WITH DEBUGGING MODE.
               OBJECT-COMPUTER. JVM.
               SPECIAL-NAMES. DECIMAL-POINT IS COMMA.

           INPUT-OUTPUT SECTION.
           FILE-CONTROL.

       DATA DIVISION.
           FILE SECTION.

           WORKING-STORAGE SECTION.
           01 WS-BISSEXTILE        PIC 9 VALUE 0.


           LOCAL-STORAGE SECTION.
           01 LS-DATE-YYYYDDD      PIC 9(7).
           01 LS-DATE-YYYYMMDD     PIC 9(8).



           LINKAGE SECTION.
           01 LK-DATE-YYYYDDD.
               05 L-QANNEE          PIC 9(4).
               05 L-QJOUR           PIC 9(3).

           01 LK-DATE-YYYYMMDD.
               05 L-ANNEE          PIC 9(4).
               05 L-MOIS           PIC 9(2).
               05 L-JOUR           PIC 9(2).



       PROCEDURE DIVISION USING LK-DATE-YYYYDDD LK-DATE-YYYYMMDD.


       MAIN-PROCEDURE.

            MOVE LK-DATE-YYYYDDD TO LS-DATE-YYYYDDD

            COMPUTE LS-DATE-YYYYMMDD =
                   FUNCTION DATE-OF-INTEGER (
                       FUNCTION INTEGER-OF-DAY(LS-DATE-YYYYDDD))

            MOVE LS-DATE-YYYYMMDD TO LK-DATE-YYYYMMDD


            GOBACK.



      *>  CALCUL-BISSEXTILE.

      *>      DISPLAY 'CALCUL BISSTEXTILE'

      *>      MOVE 0 TO WS-BISSEXTILE

      *>      IF FUNCTION MOD(L-ANNEE 4) = 0
      *>          MOVE 1 TO WS-BISSEXTILE
      *>          END-IF

      *>      IF FUNCTION MOD(L-ANNEE 100) = 0
      *>          MOVE 0 TO WS-BISSEXTILE
      *>          END-IF

      *>      IF FUNCTION MOD(L-ANNEE 400) = 0
      *>          MOVE 1 TO WS-BISSEXTILE
      *>          END-IF
      *>      .


       END PROGRAM CONVDATE.
