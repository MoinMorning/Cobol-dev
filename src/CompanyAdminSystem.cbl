       IDENTIFICATION DIVISION.
       PROGRAM-ID. CompanyAdminSystem.
       AUTHOR. lek.

       ENVIRONMENT DIVISION. 
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT MachineFile
           ASSIGN TO "machines.txt"
           ORGANIZATION IS LINE SEQUENTIAL
           ACCESS MODE IS SEQUENTIAL
           FILE STATUS IS ws-MachineFile-Status.

           SELECT TempMachineFile
           ASSIGN TO 
           "temp_machines.txt"
           ORGANIZATION IS LINE SEQUENTIAL
           ACCESS MODE IS SEQUENTIAL.

           SELECT StoreFile
           ASSIGN TO "store.txt"
           ORGANIZATION IS LINE SEQUENTIAL
           ACCESS MODE IS SEQUENTIAL
           FILE STATUS IS ws-StoreFile-Status.

       DATA DIVISION.
       FILE SECTION.
       FD MachineFile.
       01 MachineRecord.
           05 MachineNum PIC X9(10).
           05 MachineTyp PIC X(10).
           05 MachineManu PIC X(10).
           05 MachineUsername PIC X(20).
           05 MachineSpec PIC X(50).
       
       FD StoreFile.
       01 StoreRecord.
           05 StoreMachineNum PIC X9(10).
           05 StoreMachineTyp PIC X(10).
           05 StoreMachineManu PIC X(10).
           05 StoreMachineUsername PIC X(20).
           05 StoreMachineSpec PIC X(50).

       FD TempMachineFile.
       01 TempMachineRecord.
           05 TempMachineNum PIC X9(10).
           05 TempMachineTyp PIC X(10).
           05 TempMachineManu PIC X(10).
           05 TempMachineUsername PIC X(20).
           05 TempMachineSpec PIC X(50).
       
       WORKING-STORAGE SECTION.
       01 ws-StoreFile-Status PIC 99.
       01 ws-MachineFile-Status PIC 99. 
       01 ws-MenuOption PIC 9.
       01 ws-MachineNumber PIC 9(10) VALUE 0.
       01 ws-MachineIndex PIC 9(10) VALUE 1.
       01 ws-MachineList.
           05 ws-MachineNum PIC 9(10) OCCURS 100 TIMES.
       01 Username PIC X(20).
       01 ws-MachineSpec PIC X(50).
       01 ws-MachineTyp PIC X(10).
       01 ws-MachineManu PIC X(10).

       PROCEDURE DIVISION.
           PERFORM InitializeFile
           OPEN INPUT MachineFile
             CLOSE MachineFile.
       
       InitializeFile.
           OPEN OUTPUT MachineFile
           MOVE "Num" TO MachineNum
           MOVE "Typ" TO MachineTyp
           MOVE "Manufacture" TO MachineManu
           MOVE "Username" TO MachineUsername
           MOVE "Spec" TO MachineSpec
           WRITE MachineRecord
           CLOSE MachineFile
           OPEN EXTEND MachineFile.


       Begin.
           PERFORM DisplayMenu UNTIL ws-MenuOption = 5
           STOP RUN.

       DisplayMenu.
           DISPLAY " AdminSystem"
           DISPLAY "1. Add Machine Information"
           DISPLAY "2. List Machine Numbers"
           DISPLAY "3. Delete Machine Number"
           DISPLAY "4. Search Machine Number"
           DISPLAY "5. Exit"
           ACCEPT ws-MenuOption
           PERFORM MenuAction.

       MenuAction.
           EVALUATE ws-MenuOption
           WHEN 1
            PERFORM AddMachine
           WHEN 2
            PERFORM CheckMachineNumbers
           WHEN 3
            PERFORM DeleteMachine
           WHEN 4
            PERFORM SearchMachine  
           WHEN 5
            EXIT PROGRAM
       END-EVALUATE.
       

       AddMachine.
       
           DISPLAY "Enter Machine Number to add: "
           ACCEPT ws-MachineNumber
           IF ws-MachineNumber LESS THAN 1 OR ws-MachineNumber GREATER 
           THAN 999999999
           DISPLAY "Invalid Machine Number. Please enter a number betwe"
           "en 1 and 999999999."
           GO TO DisplayMenu 
           END-IF.

           DISPLAY "Enter Username: "
           ACCEPT Username
       
           IF Username EQUAL SPACES
           DISPLAY "Username cannot be empty."
           GO TO DisplayMenu
           END-IF. 

           DISPLAY "Enter Machine Type: "
           ACCEPT ws-MachineTyp
           IF ws-MachineTyp EQUAL SPACES
           DISPLAY "Machine Type cannot be empty."
           GO TO DisplayMenu
           END-IF.

           DISPLAY "Enter Machine Manufacturer: "
           ACCEPT ws-MachineManu
           IF ws-MachineManu EQUAL SPACES
           DISPLAY "Machine Manufacturer cannot be empty."
           GO TO DisplayMenu
           END-IF.

           DISPLAY "Enter Machine Specifications: "
           ACCEPT ws-MachineSpec 

           IF ws-MachineSpec EQUAL SPACES
           DISPLAY "Machine Specifications cannot be empty."
            GO TO DisplayMenu
           END-IF.

           PERFORM AppendMachineToFile
           CLOSE MachineFile
           ADD 1 TO ws-MachineIndex
           PERFORM CopyToStoreFile 
           PERFORM DisplayMenu.

       CheckMachineNumbers.
           OPEN INPUT StoreFile 
           READ StoreFile  INTO StoreRecord 
           AT END
              DISPLAY "No machine numbers stored yet."
           NOT AT END
           PERFORM UNTIL ws-StoreFile-Status = 10
             DISPLAY "Machine Number: " StoreMachineNum 
             READ StoreFile  INTO StoreRecord 
             AT END
           MOVE 10 TO ws-StoreFile-Status
             END-PERFORM
           CLOSE StoreFile 
           PERFORM DisplayMenu.

       DeleteMachine.
           DISPLAY "Enter username to delete: "
           ACCEPT Username  
           OPEN INPUT StoreFile
           OPEN OUTPUT TempMachineFile
           READ StoreFile INTO StoreRecord
           AT END
              DISPLAY "No machine numbers stored yet."
           NOT AT END
               PERFORM UNTIL ws-MachineIndex > 100
                   IF StoreMachineUsername NOT EQUAL TO Username  
                         MOVE StoreMachineNum TO TempMachineNum
                         MOVE StoreMachineTyp TO TempMachineTyp
                         MOVE StoreMachineManu TO TempMachineManu
                         MOVE StoreMachineUsername TO
                          TempMachineUsername
                         OF TempMachineRecord
                         MOVE StoreMachineSpec TO TempMachineSpec 
                         OF TempMachineRecord
                         WRITE TempMachineRecord
                   END-IF 
                   READ StoreFile INTO StoreRecord
                      AT END
                          EXIT PERFORM 
               END-PERFORM
           CLOSE StoreFile 
           CLOSE TempMachineFile
           PERFORM DisplayMenu.
        
        SearchMachine.
           DISPLAY "Enter Machine Number to search: "
           ACCEPT ws-MachineNumber 
           OPEN INPUT StoreFile 
           READ StoreFile  INTO StoreRecord 
               AT END
            DISPLAY "No machine numbers stored yet."
               NOT AT END
            PERFORM UNTIL ws-MachineIndex > 100
                IF StoreMachineNum  EQUAL TO ws-MachineNumber 
                    DISPLAY "Machine Number found: " StoreMachineNum 
                    DISPLAY "Machine Type: " StoreMachineTyp
                    DISPLAY "Machine Manufacturer: " StoreMachineManu
                    DISPLAY "Machine Username: " StoreMachineUsername
                    DISPLAY "Machine Specifications: " StoreMachineSpec
                    EXIT PERFORM
                END-IF 
                READ StoreFile INTO StoreRecord 
                    AT END
                        DISPLAY "Machine Number not found."
                        EXIT PERFORM 
              END-PERFORM
            CLOSE StoreFile 
            PERFORM DisplayMenu.
                       
       
       AppendMachineToFile.
           IF ws-MachineIndex EQUAL 1
              OPEN OUTPUT MachineFile
           ELSE
              OPEN EXTEND MachineFile
           END-IF

           MOVE ws-MachineNumber TO MachineNum
           MOVE Username TO MachineUsername  OF MachineRecord
           MOVE ws-MachineSpec  TO MachineSpec OF MachineRecord
           MOVE ws-MachineTyp TO MachineTyp OF MachineRecord
           MOVE ws-MachineManu TO MachineManu OF MachineRecord
           WRITE MachineRecord
           CLOSE MachineFile.
       
       CopyToStoreFile.
           OPEN INPUT MachineFile
           OPEN EXTEND  StoreFile
           READ MachineFile INTO MachineRecord
           AT END
              DISPLAY "No machine numbers stored yet."
           NOT AT END
              PERFORM UNTIL ws-MachineIndex > 100
                IF MachineNum EQUAL TO ws-MachineNumber
                MOVE MachineNum TO StoreMachineNum
                MOVE MachineTyp TO StoreMachineTyp
                MOVE MachineManu TO StoreMachineManu
                MOVE MachineUsername TO StoreMachineUsername
                MOVE MachineSpec TO StoreMachineSpec
                WRITE StoreRecord FROM MachineRecord
             EXIT PERFORM
             END-IF 
             READ MachineFile INTO MachineRecord
                AT END
                    EXIT PERFORM
            END-PERFORM
           CLOSE MachineFile 
           CLOSE StoreFile.

  
           COMPUTE ws-MachineIndex = ws-MachineIndex + 1 ON SIZE ERROR
           DISPLAY "Error: Overflow occurred when incrementing"
           "ws-MachineIndex."
           STOP RUN
           END-COMPUTE.
