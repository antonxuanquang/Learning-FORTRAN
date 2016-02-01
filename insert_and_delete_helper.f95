MODULE INSERT_AND_DELETE

	USE CLASS_FACULTY
	USE FINDER


	IMPLICIT NONE

    CONTAINS

	INTEGER FUNCTION INSERT_OP(INPUT_STRING, FACULTY_LIST, HEAD_INT)
			CHARACTER*66 			:: INPUT_STRING
			INTEGER					:: IDNUM, EMPTY_INDEX, HEAD_INT
			CHARACTER*13			:: NAME
			CHARACTER*4				:: DEPT
			CHARACTER*9				:: RANK
			REAL					:: PAY
			TYPE(FACULTY)			:: FACULTY_MEMBER
			TYPE(FACULTY), DIMENSION(100)      :: FACULTY_LIST

			READ(INPUT_STRING, 300) IDNUM, NAME, DEPT, RANK, PAY
		300	FORMAT(3X, I8, 1X, A13, 1X, A4, 11X, A9, 8X, F9.2)
			WRITE(6, *) "** Inserting faculty w/ ID = ", IDNUM, " NAME = ", NAME, " **"

			!create new faculty member
			FACULTY_MEMBER%IDNUM = IDNUM
			FACULTY_MEMBER%NAME = NAME
			FACULTY_MEMBER%DEPT = DEPT
			FACULTY_MEMBER%RANK = RANK
			FACULTY_MEMBER%PAY = PAY

			!point to next old first position
            FACULTY_MEMBER%NEXT_INDEX = HEAD_INT

            !find the empty index to put in
            EMPTY_INDEX = FIND_EMPTY_INDEX(FACULTY_LIST)

            !update array
            FACULTY_LIST(EMPTY_INDEX) = FACULTY_MEMBER

            !return head
            INSERT_OP = EMPTY_INDEX
	END FUNCTION INSERT_OP


	SUBROUTINE DELETE_OP(INPUT_STRING, FACULTY_LIST, HEAD_INT)
            INTEGER                             :: IDNUM, PREVIOUS_INDEX, INDEX , HEAD_INT
            CHARACTER*66                        :: INPUT_STRING
            TYPE(FACULTY)                       :: FACULTY_MEMBER
            TYPE(FACULTY), DIMENSION(100)       :: FACULTY_LIST

            READ(INPUT_STRING, 200) IDNUM
        200 FORMAT(3X,I8)

            PREVIOUS_INDEX = FIND_PREVIOUS_FACULTY(IDNUM, FACULTY_LIST, HEAD_INT)
            IF (PREVIOUS_INDEX > 0) THEN
                INDEX = NEXT(FACULTY_LIST(PREVIOUS_INDEX))
                WRITE(6, *) "** Deleting faculty w/ ID = ", FACULTY_LIST(INDEX)%IDNUM, " NAME = ", FACULTY_LIST(INDEX)%NAME, " **"
                !change pointer address
                FACULTY_LIST(PREVIOUS_INDEX)%NEXT_INDEX = FACULTY_LIST(INDEX)%NEXT_INDEX

                !delete faculty (e.i: change ID number to -999999)
                FACULTY_LIST(INDEX)%IDNUM = -99999999
                FACULTY_LIST(INDEX)%NEXT_INDEX = -99
            ELSE
                WRITE (*,*) "CAN'T FIND ID: ", IDNUM
            END IF

	END SUBROUTINE DELETE_OP

END MODULE INSERT_AND_DELETE
