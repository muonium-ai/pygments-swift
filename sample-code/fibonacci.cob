      * COBOL sample
       IDENTIFICATION DIVISION.
       PROGRAM-ID. FIB.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 N PIC 9(2) VALUE 10.
       01 I PIC 9(2).
       01 A PIC 9(9) VALUE 0.
       01 B PIC 9(9) VALUE 1.
       01 T PIC 9(9).

       PROCEDURE DIVISION.
           DISPLAY "fib(" N ")".
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > N
               COMPUTE T = A + B
               MOVE B TO A
               MOVE T TO B
           END-PERFORM
           STOP RUN.
