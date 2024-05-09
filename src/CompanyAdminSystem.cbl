       IDENTIFICATION DIVISION.
       PROGRAM-ID. CompanyAdminSystem.
       AUTHOR. lek.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 ws-MenuOption PIC 9.
       01 ws-MachineNumber PIC 9(5) VALUE 0.
       01 ws-MachineIndex PIC 9(3) VALUE 1.
       01 ws-MachineList.
       05 ws-MachineNum PIC 9(5) OCCURS 100 TIMES.

       PROCEDURE DIVISION.
       Begin.
       PERFORM DisplayMenu UNTIL ws-MenuOption = 3
       STOP RUN.

       DisplayMenu.
       DISPLAY "Company Admin System"
       DISPLAY "1. Add Machine Number"
       DISPLAY "2. Check Machine Numbers"
       DISPLAY "3. Exit"
       ACCEPT ws-MenuOption
       PERFORM MenuAction.

       MenuAction.
       EVALUATE ws-MenuOption
        WHEN 1
            PERFORM AddMachine
        WHEN 2
            PERFORM CheckMachineNumbers
        WHEN 3
            EXIT PROGRAM
        WHEN OTHER
            DISPLAY "Invalid option. Please try again."
       END-EVALUATE.

       AddMachine.
       DISPLAY "Enter Machine Number to add: "
       ACCEPT ws-MachineNumber
       MOVE ws-MachineNumber TO ws-MachineNum(ws-MachineIndex)
       ADD 1 TO ws-MachineIndex
       PERFORM DisplayMenu.

       CheckMachineNumbers.
       DISPLAY "Machine Numbers in the company:"
           PERFORM VARYING ws-MachineIndex FROM 1 BY 1 UNTIL 
           ws-MachineIndex > 100
           IF ws-MachineNum(ws-MachineIndex) NOT = 0
               DISPLAY ws-MachineNum(ws-MachineIndex)
           END-PERFORM
       PERFORM DisplayMenu.
