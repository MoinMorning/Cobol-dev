IDENTIFICATION DIVISION.
PROGRAM-ID. CompanyAdminSystem.

DATA DIVISION.
WORKING-STORAGE SECTION.
01 MenuOption PIC 9.
01 MachineNumber PIC 9(5) VALUE 0.
01 MachineList.
   05 MachineNum PIC 9(5) OCCURS 100 TIMES.

PROCEDURE DIVISION.
Begin.
    PERFORM UNTIL MenuOption = 3
        PERFORM MenuOption
    END-PERFORM.
    STOP RUN.

MenuOption.
    DISPLAY "Company Admin System"
    DISPLAY "1. Add Machine Number"
    DISPLAY "2. Check Machine Numbers"
    DISPLAY "3. Exit"
    ACCEPT MenuOption
    PERFORM MenuAction.

MenuAction.
    EVALUATE MenuOption
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
    ACCEPT MachineNumber
    ADD 1 TO MachineList(MachineNumber)
    PERFORM MenuOption.

CheckMachineNumbers.
    DISPLAY "Machine Numbers in the company:"
    PERFORM VARYING MachineIndex FROM 1 BY 1 UNTIL MachineIndex > 100
        IF MachineList(MachineIndex) NOT = 0
            DISPLAY MachineList(MachineIndex)
    END-PERFORM.
    PERFORM MenuOption.

BEGIN.
    PERFORM Begin.
