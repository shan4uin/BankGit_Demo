      *SET CICSECM(LINKAGE=YES)
       IDENTIFICATION DIVISION.
       PROGRAM-ID. HELOWRLD.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01  WS-WORKAREA                 PIC X(50) VALUE 'HELLO'.

       LINKAGE SECTION.
       
       01  DFHCOMMAREA                 PIC X(50).

       PROCEDURE DIVISION.

           MOVE FUNCTION REVERSE(DFHCOMMAREA) TO WS-WORKAREA.
           MOVE WS-WORKAREA                   TO DFHCOMMAREA.
           DISPLAY WS-WORKAREA.
           DISPLAY DFHCOMMAREA.

           GOBACK.
