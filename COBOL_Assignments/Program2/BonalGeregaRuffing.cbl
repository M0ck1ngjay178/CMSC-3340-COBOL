       *>--------------GROUP HEADER-------------------------
       IDENTIFICATION DIVISION.
       PROGRAM-ID. BonalGeregaRuffing.
       AUTHOR. BONAL, GEREGA, RUFFING.
       *> CMSC-3340-001 GROUP ASSIGNMENT - PROGRAM 2
       *> DUE: SEPT. 24, 2024
       *> NOTE: THIS IS A OPEN-COBOL-IDE APPLICATION, PLEASE USE FOR COMPILATION
       *>---------------------------------------------------

      *>====================ENVIRONMENT DIVISION==============================================
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

       *> File association is set here, CAR-SALES-FILE is our
       *> input with PRINT-FILE and PRINT-FILEBAD our output

       SELECT CAR-SALES-FILE ASSIGN TO 'input.txt'
                         ORGANIZATION IS LINE SEQUENTIAL.
       SELECT PRINT-FILE   ASSIGN TO 'good.txt'
                         ORGANIZATION IS LINE SEQUENTIAL.
        SELECT PRINT-FILEBAD ASSIGN TO 'bad.txt'
                        ORGANIZATION IS LINE SEQUENTIAL.

      *>====================END ENVIRONMENT DIVISION============================================

      *>====================DATA DIVISION==============================================
       DATA DIVISION.
       FILE SECTION.
       *>FILE DESCRIPTION FOR INPUT FILE
       FD CAR-SALES-FILE
           RECORD CONTAINS 67 CHARACTERS
           DATA RECORD IS CAR-SALES-FILE.
       01 CAR-IN.
         05 LOCATION           PIC X(11).
         05 BRANCH             PIC X(4).
         05 SALESPERSON        PIC X(10).
         05 CUSTOMER-NAME      PIC X(10).
         05 SALE-DATE          PIC X(6).
           *>08 DAY-NUM          PIC X(2).
           *>08 MONTH            PIC X(2).
           *>08 YR-NUM           PIC X(2).
         05 SALE-AMOUNT        PIC X(6).
         05 COMMISION-RATE     PIC XXX.
           88 REASONABLE-RATE  VALUE 0 THRU 100.
         05 CAR-MODEL          PIC X(13).
         05 CAR-YR             PIC X(4).
           88 REASONABLE-YEAR  VALUE 1930 THRU 1995.


       *>FILE DESCRIPTION FOR GOOD OUTPUT FILE
       FD  PRINT-FILE
                 RECORD CONTAINS 75 CHARACTERS
                  DATA RECORD IS PRINT-LINE-GOOD.
       01 PRINT-LINE-GOOD PIC X(75).

       *>FILE DESCRIPTION FOR BAD OUTPUT FILE
       FD  PRINT-FILEBAD
                 RECORD CONTAINS 89 CHARACTERS
                  DATA RECORD IS PRINT-LINE-BAD.
       01 PRINT-LINE-BAD PIC X(89).

       WORKING-STORAGE SECTION.
       *>DATA REMAINS VARIABLE
       01 DATA-REMAINS-SWITCH PIC X(2) VALUE SPACES.

       *>WORKING VARIABLES FOR MONTH AND DAY
       01  WORK-DAY      PIC X(2).
       01  WORK-MONTH    PIC X(2).

       *>GOOD-CAR RECORD
       01 GOOD-CAR.
         05 GOOD-LOC PIC X(11).
         05 FILLER PIC X(1).
         05 GOOD-BRANCH PIC 9(4).
         05 FILLER PIC X(1).
         05 GOOD-SP PIC X(10).
         05 FILLER PIC X(1).
         05 GOOD-CNAME PIC X(10).
         05 FILLER PIC X(1).
         05 GOOD-SD PIC 9(6).
         05 FILLER PIC X(1).
         05 GOOD-SAMT PIC 9(6).
         05 FILLER PIC X(1).
         05 GOOD-CRATE PIC 999.
         05 FILLER PIC X(1).
         05 GOOD-MODEL PIC X(13).
         05 FILLER PIC X(1).
         05 GOOD-YEAR PIC 9(4).

       *> BAD CAR RECORD
       01 BAD-CAR.
         05 BAD-LOC PIC X(11).
         05 FILLER PIC X(1).
         05 BAD-BRANCH PIC x(4).
         05 FILLER PIC X(1).
         05 BAD-SP PIC X(10).
         05 FILLER PIC X(1).
         05 BAD-CNAME PIC X(10).
         05 FILLER PIC X(1).
         05 BAD-SD PIC X(6).
         05 FILLER PIC X(1).
         05 BAD-SAMT PIC X(6).
         05 FILLER PIC X(1).
         05 BAD-CRATE PIC XXX.
         05 FILLER PIC X(1).
         05 BAD-MODEL PIC X(13).
         05 FILLER PIC X(1).
         05 BAD-YEAR PIC X(4).

       *>ERROR MESSAGE RECORD
       01 ERROR-MSG PIC X(14) VALUE 'ERROR IN DATA:'.

       01  MISSING  PIC X(29) VALUE 'INCOMING RECORD MISSING DATA'.
       01  INVALID-MONTH PIC X(14) VALUE ' INVALID MONTH'.
       01  INVALID-DAY   PIC X(12) VALUE ' INVALID DAY'.

       01 UNREASONABLE-RATE PIC X(22) VALUE 'INVALID COMMISION-RATE'.
       01 UNREASONABLE-YEAR PIC X(13) VALUE ' INVALID YEAR'.
       01 NUM-MSG PIC X(21) VALUE 'INVALID NUMERIC FIELD'.


      *>====================END DATA DIVISION=================================================

      *>====================PROCEDURE PARAGRAPHS==============================================
       PROCEDURE DIVISION.
      *>-------------MAIN OPERATIONS PROCEDURE------------------------------
       *>OPEN ALL FILES, BEGIN READING THE INPUT FILE
       PREPARE-CAR-STATEMENT.
           OPEN INPUT CAR-SALES-FILE OUTPUT PRINT-FILE OUTPUT
             PRINT-FILEBAD.
           READ CAR-SALES-FILE
               AT END
                   MOVE 'NO' TO DATA-REMAINS-SWITCH
           END-READ.

           PERFORM PROCESS-CAR UNTIL DATA-REMAINS-SWITCH = 'NO'.

           CLOSE CAR-SALES-FILE
             PRINT-FILE
             PRINT-FILEBAD.
           STOP RUN.
      *>-------------END MAIN OPERATIONS PROCEDURE------------------------------


       PROCESS-CAR.
           *>First, check to see if any data field is empty
            IF LOCATION = SPACES
           OR BRANCH = SPACES
           OR SALESPERSON = SPACES
           OR CUSTOMER-NAME = SPACES
           OR SALE-AMOUNT = SPACES
           OR COMMISION-RATE = SPACES
           OR CAR-MODEL = SPACES
           OR CAR-YR = SPACES
               PERFORM INVALID-CAR
           END-IF.
           *> CHECK IF ALL FIELDS ARE NOT NUMERIC OR NOT IN THE APPROPRIATE RANGE
           IF BRANCH IS NOT NUMERIC
           OR SALE-DATE IS NOT NUMERIC
           OR SALE-AMOUNT IS NOT NUMERIC
           OR COMMISION-RATE IS NOT NUMERIC
           OR NOT REASONABLE-RATE
           OR NOT REASONABLE-YEAR
               *>IF ALL CONDITIONS ARE TRUE, DATA IS INVALID
               PERFORM INVALID-CAR
           END-IF.


           *>CHECK TO ENSURE DATA IS NUMERIC AND WITHIN SPECIFIED RANGES
           IF BRANCH IS NUMERIC
           AND SALE-DATE IS NUMERIC
           AND SALE-AMOUNT IS NUMERIC
           AND COMMISION-RATE IS NUMERIC
           AND REASONABLE-RATE
           AND REASONABLE-YEAR
               PERFORM VALID-CAR
           END-IF.




       *>VALID CAR PROCEDURE
       VALID-CAR.
           *>MOVE DATA INTO PROPER FIELDS NOW THAT WE KNOW IT IS VALID DATA
           MOVE LOCATION TO GOOD-LOC.
           MOVE BRANCH TO GOOD-BRANCH.
           MOVE SALESPERSON TO GOOD-SP.
           MOVE CUSTOMER-NAME TO GOOD-CNAME.
           MOVE SALE-DATE TO GOOD-SD.
           MOVE SALE-AMOUNT TO GOOD-SAMT.
           MOVE COMMISION-RATE TO GOOD-CRATE.
           MOVE CAR-MODEL TO GOOD-MODEL.
           MOVE CAR-YR TO GOOD-YEAR.

           *>MOVE THE DATA TO OUR GOOD OUTPUT, AND THEN PRINT THE LINE
           MOVE GOOD-CAR TO PRINT-LINE-GOOD.
           WRITE PRINT-LINE-GOOD.
           READ CAR-SALES-FILE
               AT END
                   MOVE 'NO' TO DATA-REMAINS-SWITCH
           END-READ.

       *>INVALID CAR PROCEDURE
       INVALID-CAR.
           *>OUR DATA IS NOT VALID, SO WE MOVE TO OUR BAD RECORD
           MOVE LOCATION TO BAD-LOC.
           MOVE BRANCH TO BAD-BRANCH.
           MOVE SALESPERSON TO BAD-SP.
           MOVE CUSTOMER-NAME TO BAD-CNAME.
           MOVE SALE-DATE TO BAD-SD.
           MOVE SALE-AMOUNT TO BAD-SAMT.
           MOVE COMMISION-RATE TO BAD-CRATE.
           MOVE CAR-MODEL TO BAD-MODEL.
           MOVE CAR-YR TO BAD-YEAR.

           *>PUT OUR ERROR MESSAGE INTO THE BAD OUTPUT AND PRINT IT
           *>Check if the error is a empty field error
           IF LOCATION = SPACES
           OR BRANCH = SPACES
           OR SALESPERSON = SPACES
           OR CUSTOMER-NAME = SPACES
           OR SALE-AMOUNT = SPACES
           OR COMMISION-RATE = SPACES
           OR CAR-MODEL = SPACES
           OR CAR-YR = SPACES
               MOVE MISSING TO PRINT-LINE-BAD
               WRITE PRINT-LINE-BAD
           *>Check if the error is an unreasonable rate
           ELSE IF NOT REASONABLE-RATE
               MOVE UNREASONABLE-RATE TO PRINT-LINE-BAD
               WRITE PRINT-LINE-BAD
           *>check if the error is an unreasonable year
           ELSE IF NOT REASONABLE-YEAR
               MOVE UNREASONABLE-YEAR TO PRINT-LINE-BAD
               WRITE PRINT-LINE-BAD
           *>check if the numeric values are not numeric
           ELSE IF BRANCH IS NOT NUMERIC
             OR SALE-DATE IS NOT NUMERIC
             OR SALE-AMOUNT IS NOT NUMERIC
             OR COMMISION-RATE IS NOT NUMERIC
             OR NOT REASONABLE-RATE
             OR NOT REASONABLE-YEAR
               MOVE NUM-MSG TO PRINT-LINE-BAD
               WRITE PRINT-LINE-BAD
           *>otherwise just a general error message
           ELSE
               MOVE ERROR-MSG TO PRINT-LINE-BAD
               WRITE PRINT-LINE-BAD
           END-IF.

           PERFORM VALIDATE-DAY-N-MONTH


           *>PUT OUR BAD DATA INTO THE BAD OUTPUT AND PRINT IT
           MOVE BAD-CAR TO PRINT-LINE-BAD.
           WRITE PRINT-LINE-BAD.

           *>PUT SPACES BETWEEN DATA
           MOVE SPACES TO PRINT-LINE-BAD.
           WRITE PRINT-LINE-BAD.

           *>MOVE ERROR-MSG TO PRINT-LINE-BAD.
           *>WRITE PRINT-LINE-BAD.
           READ CAR-SALES-FILE
               AT END
                   MOVE 'NO' TO DATA-REMAINS-SWITCH
           END-READ.


       *> Procedure for validating the day and month
       VALIDATE-DAY-N-MONTH.
           *> EXTRACT YR, DAY(MMDDYY)
           MOVE SALE-DATE (3:2) TO WORK-DAY.
           MOVE SALE-DATE (1:2) TO WORK-MONTH.

           *>Check if the month is outside the valid range
           IF WORK-MONTH < 1 OR WORK-MONTH > 12
               *>print an error message if the month is outside the range
               MOVE INVALID-MONTH TO PRINT-LINE-BAD
               WRITE PRINT-LINE-BAD
           END-IF.

           *> Check the if the day is in Feb. then check the valid days
           IF WORK-DAY IS EQUAL TO 2 THEN
               IF WORK-DAY < 1 OR WORK-DAY > 28
                   MOVE INVALID-DAY TO PRINT-LINE-BAD
                   WRITE PRINT-LINE-BAD
               END-IF
           *>Check if the value is outside the range of months with 30 days
           ELSE IF WORK-DAY = 4 OR
               WORK-DAY = 6 OR
               WORK-DAY = 9 OR
               WORK-DAY = 11 THEN

               IF WORK-DAY < 1 OR WORK-DAY > 30
                   MOVE INVALID-DAY TO PRINT-LINE-BAD
                   WRITE PRINT-LINE-BAD
               END-IF
           ELSE
               *>Check the value for the range of months with 31 days
               IF WORK-DAY < 1 OR WORK-DAY > 31
                   MOVE INVALID-DAY TO PRINT-LINE-BAD
                   WRITE PRINT-LINE-BAD
               END-IF
           END-IF.
