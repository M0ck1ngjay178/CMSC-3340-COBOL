      *>--------------GROUP HEADER-------------------------
       IDENTIFICATION DIVISION.
       PROGRAM-ID. BonalGeregaRuffing.
       AUTHOR. BONAL, GEREGA, RUFFING.
       *> CMSC-3340-001 GROUP ASSIGNMENT - PROGRAM 3
       *> DUE: NOV. 14, 2024
       *> NOTE: THIS IS A OPEN-COBOL-IDE APPLICATION, PLEASE USE FOR COMPILATION
       *>---------------------------------------------------


       environment division.
       INPUT-OUTPUT SECTION.
       *> File Declarations
       FILE-CONTROL.
           SELECT EMPLOYEE-FILE    ASSIGN TO 'input.txt'
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT PRINT-FILE       ASSIGN TO 'output.txt'
               ORGANIZATION IS LINE SEQUENTIAL.


       DATA DIVISION.
       FILE SECTION.

       *> File descriptions
       FD EMPLOYEE-FILE
           RECORD CONTAINS 33 CHARACTERS
           DATA RECORD IS EMPLOYEE-IN.
       01 EMPLOYEE-IN.
               08 EMP-FNAME  PIC X(10).
               08 EMP-LNAME  PIC X(15).
               08 EMP-HOURS  PIC 99V99 VALUE ZEROS.
               08 EMP-WAGE   PIC 99V99 VALUE ZEROS.


       FD PRINT-FILE
           RECORD CONTAINS 35 CHARACTERS
           DATA RECORD IS PRINT-WAGE.
       01 PRINT-WAGE PIC X(40).


       WORKING-STORAGE SECTION.
       *>Declaration of subscripts used in Procedure division
       01  SUBSCRIPTS.
           05 MAIN-SUB     PIC 99 VALUE ZEROS.
           05 BONUS-SUB    PIC 99 VALUE ZEROS.
           05 PRINT-SUB    PIC 99 VALUE ZEROS.

       *> End of file indicatior
       01  END-OF-FILE PIC X(3) VALUE 'NO '.

       *>Variable to check for the bonus for over 35 hours worked
       01  EMP-BONUS-CHECKER PIC 99 VALUE ZEROS.

       *>Number of employees variable
       01  NUM-OF-EMP      PIC   99 VALUE ZEROS.

       *>Employee information record
       01 EMP-INFORMATION.
         05 LNAME          PIC X(15).
         05 FILLER         PIC X(2) VALUE SPACES.
         05 FNAME          PIC X(10).
         05 FILLER         PIC X(1) VALUE SPACES.
         05 GROSS-PAY      PIC $Z,999.99.

       *>Output header record
       01  OUTPUT-HEADING.
           05  FILLER      PIC X(17)   VALUE 'LAST NAME        '.
           05  FILLER      PIC X(11)   VALUE 'FIRST NAME '.
           05  FILLER      PIC X(8)    VALUE 'GROSSPAY'.

       *>Employee table declaration
       01  EMP-TABLE.
           05   EMP-TABLE-INFO OCCURS 25 TIMES.
               10  ET-FNAME        PIC X(15).
               10  ET-LNAME        PIC X(10).
               10  ET-GROSSPAY     PIC 9999V99.


       PROCEDURE DIVISION.
       PROCESS-EMPLOYEES.
           *>Open both files for reading, and then write the heading
           OPEN INPUT EMPLOYEE-FILE
               OUTPUT PRINT-FILE.
           PERFORM WRITE-HEADING.
           PERFORM UNTIL END-OF-FILE = 'YES'
               *>Read through the file and begin building output
               READ EMPLOYEE-FILE
                   AT END
                       MOVE 'YES' TO END-OF-FILE
                   NOT AT END
                       ADD 1 TO MAIN-SUB NUM-OF-EMP
                       PERFORM 100-BUILD-OUTPUT
               END-READ
           END-PERFORM


           PERFORM 130-EMP-BONUS.
           PERFORM 150-PRINT
               VARYING PRINT-SUB FROM 1 BY 1
                   UNTIL PRINT-SUB > NUM-OF-EMP.
           CLOSE EMPLOYEE-FILE
               PRINT-FILE.
           STOP RUN.

       WRITE-HEADING.

           WRITE PRINT-WAGE FROM OUTPUT-HEADING.

       100-BUILD-OUTPUT.

           *>First call comes to this paragraph, which calls another
           PERFORM 110-PROCESS-GROSSPAY-TO-TABLE.


       110-PROCESS-GROSSPAY-TO-TABLE.

           *> Check if the current employee worked over 40 hours and pay
           *>them accordingly
           IF EMP-HOURS > 40.0
               COMPUTE  ET-GROSSPAY (MAIN-SUB) ROUNDED =
               (40 * EMP-WAGE) +
                 ((EMP-HOURS - 40)
                 * EMP-WAGE * 1.5)
           ELSE
               COMPUTE ET-GROSSPAY (MAIN-SUB) ROUNDED =
               EMP-HOURS * EMP-WAGE
           END-IF.

           *>Check to see if the employee worked at least 35 hours
           IF EMP-HOURS >= 35.0
               ADD 1 TO EMP-BONUS-CHECKER
           END-IF.

           PERFORM 120-INPUT-TO-TABLE.



       120-INPUT-TO-TABLE.
           *>Move the first and last name of the employee to our current index position
           MOVE EMP-FNAME TO ET-FNAME (MAIN-SUB).
           MOVE EMP-LNAME TO ET-LNAME (MAIN-SUB).



       130-EMP-BONUS.
           *>Check if every employee worked at least 35 hours, process a bonus if they did
           IF EMP-BONUS-CHECKER IS EQUAL TO NUM-OF-EMP
               PERFORM 140-BONUS-ADDER
                   VARYING BONUS-SUB FROM 1 BY 1
                       UNTIL BONUS-SUB >= NUM-OF-EMP.


       140-BONUS-ADDER.
           *>add 50 to the paycheck of each employee for 35 hours or more
           COMPUTE
            ET-GROSSPAY (BONUS-SUB) = ET-GROSSPAY (BONUS-SUB) + 50.00

           *>update the gross pay
           MOVE ET-GROSSPAY (BONUS-SUB) TO GROSS-PAY.


       150-PRINT.
           *> move our information to the output and write it.
           MOVE ET-FNAME (PRINT-SUB) TO FNAME.
           MOVE ET-LNAME (PRINT-SUB) TO LNAME.
           MOVE ET-GROSSPAY (PRINT-SUB) TO GROSS-PAY.
           MOVE EMP-INFORMATION TO PRINT-WAGE.
           WRITE PRINT-WAGE.
