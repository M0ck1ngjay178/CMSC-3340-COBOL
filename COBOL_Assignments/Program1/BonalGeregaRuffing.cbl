       *>--------------GROUP HEADER-------------------------
       IDENTIFICATION DIVISION.
       PROGRAM-ID. BonalGeregaRuffing.
       AUTHOR. BONAL, GEREGA, RUFFING.
       *> CMSC-3340-001 GROUP ASSIGNMENT - PROGRAM 1
       *> DUE: SEPT. 19, 2024
       *> NOTE: THIS IS A ROCKET COBOL-VISUAL STUDIO CODE APPLICATION, PLEASE USE FOR COMPILATION
       *>---------------------------------------------------

      *>====================ENVIRONMENT DIVISION==============================================
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

       *> File association is set here, ELECTRIC-STATEMENT is our
       *> input with PRINT-FILE and FRINT-FILEBAD our output

       SELECT ELECTRIC-STATEMENT ASSIGN TO 'input.txt'
                         ORGANIZATION IS LINE SEQUENTIAL.
       SELECT PRINT-FILE   ASSIGN TO 'good.txt'
                         ORGANIZATION IS LINE SEQUENTIAL.
        SELECT PRINT-FILEBAD ASSIGN TO 'bad.txt'
                        ORGANIZATION IS LINE SEQUENTIAL.

      *>====================END ENVIRONMENT DIVISION============================================

      *>====================DATA DIVISION==============================================
       DATA DIVISION.
       FILE SECTION.
       FD ELECTRIC-STATEMENT                   *>FILE Description for electric statement, contains 70 characters and the record is electric-in
           RECORD CONTAINS 70 CHARACTERS
           DATA RECORD IS ELECTRIC-IN.
       01 ELECTRIC-IN.                          *>Electric in record
         05 FIRST-NAME PIC X(12).               *>First name designated 12 alphanumeric characters
         05 LAST-NAME PIC X(12).                *>Last name designated 12 alphanumeric characters
         05 ST-ADDRESS PIC X(15).               *>Street address designated 15 alphanumeric characters
         05 CITY PIC X(13).                     *>City designated 13 alphanumeric characters
         05 STATE PIC X(2).                     *>State designated 2 alphanumeric characters
         05 ACCT-NUM PIC 9(6).                  *>Account number designated 6 numeric characters
         05 PREV-METER PIC 9(5).                *>Previous meter designated 5 numeric characters
         05 CURR-METER PIC 9(5).                *>Current meter designated 5 numeric characters

       FD  PRINT-FILE                             *>FILE description for Print file, whihc has 70 characters, and two records of PRINT-LINE-GOOD and PRINT-GOOD-HEADING
                 RECORD CONTAINS 70 CHARACTERS
                  DATA RECORD IS PRINT-LINE-GOOD
                  DATA RECORD IS PRINT-GOOD-HEADING.
       01 PRINT-LINE-GOOD PIC X(70).              *>PRINT-LINE-GOOD designated 70 alphanumeric characters
       01 PRINT-GOOD-HEADING PIC X(61).           *>Print good heading designated 61 alphanumeric charactes

       FD  PRINT-FILEBAD                           *>FILE description for PRINT-FILEBAD, which contains 35 characters and a data record of PRINT-LINE-BAD AND PRINT-BAD-HEADING
                 RECORD CONTAINS 35 CHARACTERS
                  DATA RECORD IS PRINT-LINE-BAD
                  DATA RECORD IS PRINT-BAD-HEADING.
       01 PRINT-LINE-BAD PIC X(35).                         *>PRINT-LINE-BAD designated 35 alphanumeric characters
       01 PRINT-BAD-HEADING PIC X(35).

       WORKING-STORAGE SECTION.                             *>DATA SECTION FOR WORKING STORAGE
       01 DATA-REMAINS-SWITCH PIC X(2) VALUE SPACES.        *>RECORD FOR OUR FUNCTIONS IN PROCEDURE DIVISION TO KNOW WHEN THE END OF FILE IS REACHED

       01 CUSTOMER-INFO.                                    *>CUSTOMER-INFO SPECIFIES EACH FIELD OF PERSONAL DATA READ FROM INPUT ELECTRIC STATEMENT
         05 L-NAME PIC X(12).                               *>LAST NAME REQUIRES 12 ALPHNUMERIC CHARACTERS
         05 FILLER PIC X(2).                                *>FILLER ALLOCATED FOR OUTPUT FILE READABILITY
         05 F-NAME PIC X(12).                               *>FIRST NAME REQUIRES 12 ALPHNUMERIC CHARACTERS
         05 FILLER PIC X(1).                                *>FILLER ALLOCATED FOR OUTPUT FILE READABILITY
         05 ACC-NUM PIC 9(6).                               *>ACC-NUM REQUIRES 9 NUMERIC CHARACTERS
         05 FILLER PIC X(4).                                *>FILLER ALLOCATED FOR OUTPUT FILE READABILITY
         05 STR-ADDR PIC X(15).                             *>STR-ADDR REQUIRES 15 ALPHNUMERIC CHARACTERS
         05 FILLER PIC X(2).                                *>FILLER ALLOCATED FOR OUTPUT FILE READABILITY
         05 UNITS PIC 9(5).                                 *>NUMERIC VALUE OF FIVE CHARACTERS REQUIRED FOR UNITS

       01 BAD-CUSTOMER.                                     *>BAD-CUSTOMER SPECIFIES EACH FIELD OF INVALID CUSTOMER DATA FOR BAD FILE OUTPUT
         05 BAD-ACC PIC 9(6).                               *>BAD-ACC REQUIRES 6 NUMERIC CHARACTERS
         05 FILLER PIC X(3).                                *>FILLER ALLOCATED FOR OUTPUT FILE READABILITY
         05 BAD-PREV PIC 9(5).                              *>BAD-PREV REQUIRES 5 NUMERIC CHARACTERS
         05 FILLER PIC X(11).                               *>FILLER ALLOCATED FOR OUTPUT FILE READABILITY
         05 BAD-CUR PIC 9(5).                               *>BAD-CURR REQUIRES 5 NUMERIC CHARACTERS

       01 HEADING-LINE-GOOD.                                *>GOOD.TXT OUTPUT FILE HEADER TO SPECIFY COLUMN LAYOUT
         05 FILLER PIC X(12) VALUE 'LAST NAME   '.          *>LAST NAME COLUMN LABEL - 12 ALPHNUMERIC CHARACTERS
         05 FILLER PIC X(2) VALUE SPACES.                   *>FILLER SPACE ALLOCATED FOR HEADER READABILITY
         05 FILLER PIC X(12) VALUE 'FIRST NAME  '.          *>FIRST NAME COLUMN LABEL - 12 ALPHNUMERIC CHARACTERS
         05 FILLER PIC X(2) VALUE SPACES.                   *>FILLER SPACE ALLOCATED FOR HEADER READABILITY
         05 FILLER PIC X(9) VALUE 'ACCOUNT  '.              *>ACCOUNT COLUMN LABEL - 9 ALPHNUMERIC CHARACTERS
         05 FILLER PIC X(2) VALUE SPACES.                   *>FILLER SPACE ALLOCATED FOR HEADER READABILITY
         05 FILLER PIC X(15) VALUE 'ADDRESS        '.       *>ADDRESS COLUMN LABEL - 15 ALPHNUMERIC CHARACTERS
         05 FILLER PIC X(1) VALUE SPACES.                   *>FILLER SPACE ALLOCATED FOR HEADER READABILITY
         05 FILLER PIC X(5) VALUE 'UNITS'.                  *>UNITS COLUMN LABEL - 5 ALPHNUMERIC CHARACTERS

       01 HEADING-LINE-BAD.                                 *>BAD.TXT OUTPUT FILE HEADER TO SPECIFY COLUMN LAYOUT
         05 FILLER PIC X(7) VALUE 'ACCOUNT'.                *>ACCOUNT COLUMN LABEL - 7 ALPHNUMERIC CHARACTERS
         05 FILLER PIC X(2) VALUE SPACES.                   *>FILLER SPACE ALLOCATED FOR HEADER READABILITY
         05 FILLER PIC X(14) VALUE 'CURRENT METER'.         *>CURRENT METER COLUMN LABEL - 14 ALPHNUMERIC CHARACTERS
         05 FILLER PIC X(2) VALUE SPACES.                   *>FILLER SPACE ALLOCATED FOR HEADER READABILITY
         05 FILLER PIC X(10) VALUE 'PREV METER'.            *>PREV METER COLUMN LABEL - 10 ALPHNUMERIC CHARACTERS

       01 DOTTED-LINE.                                      *>DOTTED LINE TO BOUND GOOD.TXT FILE HEADER FOR READABILITY
         05 FILLER PIC X(60) VALUE                          *>FILLER REQUIRES 60 ALPHNUMERIC CHARACTERS

        '------------------------------------------------------------'.


      *>====================END DATA DIVISION=================================================

      *>====================PROCEDURE PARAGRAPHS==============================================
       PROCEDURE DIVISION.
      *>-------------MAIN OPERATIONS PROCEDURE------------------------------
       PREPARE-ELECTRIC-STATEMENT.                        *>PROCEDURE TO PROCESS OPENING OF INPUT/OUTPIT FILES, AND THE WRITING OF COORESPONDING DATA
           OPEN INPUT ELECTRIC-STATEMENT OUTPUT PRINT-FILE OUTPUT *>OPEN INPUT FILE
             PRINT-FILEBAD.
           READ ELECTRIC-STATEMENT                        *>READ INPUT FILE
               AT END                                     *>CONDITION TO FIND EOF
                   MOVE 'NO' TO DATA-REMAINS-SWITCH       *>IF NO DATA REMAINS, MOVE NO FLAG INTO DATA-REMAINS-SWITCH
           END-READ.                                      *>END READ PROCESS
           PERFORM WRITE-HEADING-LINE.                    *>CALL TO HEADING DISPLAY PARAGRAPH
           PERFORM PROCESS-ELECTRIC UNTIL DATA-REMAINS-SWITCH = 'NO'.     *>UPDATE DATA-REMAINS CONDITION
                                                                          *>CALL TO PROCESS-ELECTRIC PARAGRAPH
           CLOSE ELECTRIC-STATEMENT                                       *>CLOSE INPUT FILE
             PRINT-FILE                                                   *>CLOSE OUTPUT FILE
             PRINT-FILEBAD.                                               *>CLOSE OUTPUT FILE
           STOP RUN.                                                      *>FINISH MAIN EXECUTION
      *>-------------END MAIN OPERATIONS PROCEDURE------------------------------

           *>-------------WRITE-HEADING-LINE PARAGRAPH------------------------------
       WRITE-HEADING-LINE.                                                *>PROCEDURE PARAGRAPH TO WRITE OUTPUT BANNERS AT TOP OF FILE
           MOVE DOTTED-LINE TO PRINT-GOOD-HEADING.                        *>PREPARE DOTTED-LINE BY MOVING INTO THE PRINT-GOOD-HEADING BUFFER
           WRITE PRINT-GOOD-HEADING.                                      *>WRITE DOTTED LINE IN BUFFER TO GOOD.TXT
           MOVE HEADING-LINE-GOOD TO PRINT-GOOD-HEADING.                  *>PREPARE FILE COLUMNS LABELS BY LOADING INTO BUFFER
           WRITE PRINT-GOOD-HEADING.                                      *>WRITE COLUMN NAMES TO GOOD.TXT
           MOVE DOTTED-LINE TO PRINT-GOOD-HEADING.                        *>PREPARE DOTTED-LINE BY MOVING INTO THE PRINT-GOOD-HEADING BUFFER
           WRITE PRINT-GOOD-HEADING.                                      *>FINISH HEADER BY CLOSING OFF WITH SECOND DOTTED LINE FOR READABILITY

           MOVE DOTTED-LINE TO PRINT-BAD-HEADING.                         *>PREPARE DOTTED LINE FOR PRINT-BAD-HEADING
           WRITE PRINT-BAD-HEADING.                                       *>WRITE THE DOTTED LINE
           MOVE HEADING-LINE-BAD TO PRINT-BAD-HEADING. *>PREPARE HEADER   *>MOVE THE HEADING-LINE-BAD INTO PRINT-BAD-HEADING
           WRITE PRINT-BAD-HEADING.                                       *>WRITE BAD.TXT HEADER TO OUTPUT
           MOVE DOTTED-LINE TO PRINT-BAD-HEADING.                         *>PREPARE SECOND DOTTED LINE
           WRITE PRINT-BAD-HEADING.                                       *>WRITE SECOND DOTTED LINE
           *>-------------END WRITE-HEADING-LINE PARAGRAPH----------------------------
       PROCESS-ELECTRIC.                                                  *>PROCEDURE PARAGRAPH TO SEPRATE GOOD AND BAD CUSTOMER DATA
           IF ACCT-NUM IS NOT NUMERIC                                     *>TESTS IF ACCT-NUM HAS NON-NUMERIC DATA
           OR PREV-METER IS NOT NUMERIC                                   *>OR IF PREV-METER HAS NON-NUMERIC DATA
           OR CURR-METER IS NOT NUMERIC                                   *>OR IF CURR-METER HAS NON-NUMERIC DATA
               PERFORM BAD-ELECTRIC                                       *>IF ANY PREVIOUS TESTS ARE SUCCESSFUL WILL PERFORM BAD-ELECTRIC PARAGRAPH
           END-IF.                                                        *>ENDS THE IF STATEMENT

           IF ACCT-NUM IS NUMERIC                                          *>TESTS ACCT-NUM FOR NUMERIC DATA
           AND PREV-METER IS NUMERIC                                       *>AND TESTS PREV-METER FOR NUMERIC DATA
           AND CURR-METER IS NUMERIC                                       *>AND TESTS CURR-METER FOR NUMERIC DATA
               PERFORM GOOD-ELECTRIC                                       *>IF ALL PREVIOUS TESTS ARE SUCCESSFUL WILL PERFORM GOOD-ELECTRIC PARAGRAPH
           END-IF.                                                         *>ENDS THE IF STATEMENT

       GOOD-ELECTRIC.                                                       *>PROCEDURE PARAGRAPH TO PROCESS GOOD CUSTOMER DATA

           MOVE FIRST-NAME TO F-NAME.                                      *>MOVES DATA IN FIRST-NAME TO DATA OUT F-NAME
           MOVE LAST-NAME TO L-NAME.                                       *>MOVES DATA IN LAST-NAME TO DATA OUT L-NAME
           MOVE ACCT-NUM TO ACC-NUM.                                       *>MOVES DATA IN ACCT-NUM TO DATA OUT ACC-NUM
           MOVE ST-ADDRESS TO STR-ADDR.                                    *>MOVES DATA IN ST-ADDRESS TO DATA OUT STR-ADDR
           IF PREV-METER IS GREATER THAN CURR-METER                        *>TESTS IF PREV-METER IS GREATER THAN CURR-METER
               SUBTRACT PREV-METER FROM 100000 GIVING PREV-METER           *>IF SO IT SUBTRACTS PREV-METER FROM 1000000 TO ACCOUNT FOR OVERLAP
               ADD CURR-METER TO PREV-METER GIVING UNITS                    *>AND ADDS CURR-METER TO PREV-METER TO GET UNITS USED
           ELSE                                                            *>IF PREV-METER IS NOT GREATER THAN CURR-METER
               SUBTRACT PREV-METER FROM CURR-METER GIVING UNITS            *>IF SO SUBTRACTS PREV-METER FROM CURR-METER TO GET UNITS USED
           END-IF                                                          *>ENDS THE IF STATEMENT

           MOVE CUSTOMER-INFO TO PRINT-LINE-GOOD.                          *>PREPARE GOOD CUSTOMER DATA TO BE PRINTED INTO GOOD.TXR
           WRITE PRINT-LINE-GOOD.                                          *>WRITE GOOD CUSTOMER DATA TO GOOD.TXT
           READ ELECTRIC-STATEMENT                                         *>READ INPUT FILE
               AT END                                                      *>CONDITION TO FIND EOF
                   MOVE 'NO' TO DATA-REMAINS-SWITCH                        *>IF NO DATA REMAINS, MOVE NO FLAG INTO DATA-REMAINS-SWITCH
           END-READ.                                                       *>END READ PROCESS

       BAD-ELECTRIC.                                                       *>PROCEDURE PARAGRAPH TO PROCESS BAD CUSTOMER DATA
           MOVE ACCT-NUM TO BAD-ACC.                                       *>MOVES DATA IN ACCT-NUM TO DATA OUT BADD-ACC
           MOVE PREV-METER TO BAD-PREV.                                    *>MOVES DATA IN PREV-METER TO DATA OUT BAD-PREV
           MOVE CURR-METER TO BAD-CUR.                                     *>MOVES DATA IN CURR-METER TO DATA OUT BAD-CUR

           MOVE BAD-CUSTOMER TO PRINT-LINE-BAD.                            *>PREPARE BAD CUSTOMER DATA TO BE PRINTED INTO BAD.TXT
           WRITE PRINT-LINE-BAD.                                           *>WRITES BAD CUSTOMER DATA TO BAD.TXT
           READ ELECTRIC-STATEMENT                                         *>READ INPUT FILE
               AT END                                                      *>CONDITION TO FIND EOF
                   MOVE 'NO' TO DATA-REMAINS-SWITCH                        *>IF NO DATA REMAINS, MOVE NO FLAG INTO DATA-REMAINS-SWITCH
           END-READ.                                                       *>END READ PROCESS
