PROGRAM MAIN
		
		USE NODE
		USE CLASS_FACULTY
		USE PRINT_HELPER
		USE UPDATE_HELPER
		USE INSERT_AND_DELETE

		INTERFACE
			SUBROUTINE PRINT_ALL(HEAD)
				USE NODE
				TYPE(NODE), POINTER		:: HEAD
			END SUBROUTINE

			FUNCTION PRINT_INDIVIDUAL(ELEM)
				USE NODE
				TYPE(NODE), POINTER		:: ELEM
			END FUNCTION

			FUNCTION INSERTNODE(HEAD, ELEM)
				USE NODE
				TYPE(NODE), POINTER		:: HEAD, ELEM, INSERTLIST
			END FUNCTION

			SUBROUTINE DELETENODE(PREVIOUS, CURRENT)
				USE NODE
				TYPE(NODE), POINTER		:: PREVIOUS, CURRENT
			END SUBROUTINE
		END INTERFACE

		CHARACTER(LEN=66) 				:: INPUT_STRING
		CHARACTER*2						:: COMMAND
		TYPE(NODE), POINTER				:: HEAD

		NULLIFY(HEAD)

		OPEN(5, STATUS="OLD", FILE="lab1data.txt")
		OPEN(6, FILE="lab1ANS.txt")

		WRITE(6, *) "*** PROGRAM STARTED ***"
! start reading loops
		! read a line at a time
	50	READ(5, 100, END=999) INPUT_STRING
	100		FORMAT(A66)

			! read out first 2 letter to determine which command to do
			READ(INPUT_STRING, 200) COMMAND
	200		FORMAT(A2)

			WRITE(*, *) "COMMAND = ", COMMAND

			! start if 
			IF (COMMAND == "IN") THEN
				CALL INSERT_OP(INPUT_STRING, HEAD)
			ELSE IF (COMMAND == "DE") THEN
				CALL DELETE_OP(INPUT_STRING, HEAD)


			ELSE IF (COMMAND == "UD") THEN
				CALL UPDATE_DEPT_OP(INPUT_STRING, HEAD)
			ELSE IF (COMMAND == "UT") THEN
				CALL UPDATE_TITLE_OP(INPUT_STRING, HEAD)
			ELSE IF (COMMAND == "UP") THEN
				CALL UPDATE_PAY_OP(INPUT_STRING, HEAD)



			ELSE IF (COMMAND == "PA") THEN
				CALL PRINT_ALL_OP(INPUT_STRING, HEAD)
			ELSE IF (COMMAND == "PI") THEN
				CALL PRINT_ID_OP(INPUT_STRING, HEAD)
			ELSE IF (COMMAND == "PT") THEN
				CALL PRINT_TITLE_OP(INPUT_STRING, HEAD)
			ELSE IF (COMMAND == "PD") THEN
				CALL PRINT_DEPT_OP(INPUT_STRING, HEAD)
			END IF

		GO TO 50


	999	WRITE (6, *) "END OF DATA REACHED"
END






