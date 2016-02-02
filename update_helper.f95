!**************************************************************************!
!*  This module helps the main program updates information of a faculty   *!
!*  member.                                                               *!
!**************************************************************************!
MODULE UPDATE_HELPER

    USE FINDER
    USE CLASS_FACULTY

	IMPLICIT NONE

	CONTAINS

    ! update department information of a faculty given id and name
	SUBROUTINE UPDATE_DEPT_OP(INPUT_STRING,FACULTY_LIST,HEAD_INDEX)
        CHARACTER*66 			        :: INPUT_STRING
        INTEGER				    	    :: IDNUMBER_ELEM, HEAD_INDEX, INDEX
        CHARACTER*13	    		    :: NAME_ELEM
        CHARACTER*4	    			    :: DEPT_ELEM
        TYPE(FACULTY), DIMENSION(100)   :: FACULTY_LIST

        READ(INPUT_STRING, 300) IDNUMBER_ELEM, NAME_ELEM, DEPT_ELEM
    300	FORMAT(3X, I8, 1X, A13, 1X, A4)

        INDEX = FIND_FACULTY_BY_NAME_AND_ID(IDNUMBER_ELEM, NAME_ELEM, FACULTY_LIST,HEAD_INDEX)
        IF(INDEX>0) THEN
            IF(FACULTY_LIST(INDEX)%IDNUM==IDNUMBER_ELEM) THEN
                FACULTY_LIST(INDEX)%DEPT = DEPT_ELEM
                WRITE(*,*) "** Updating faculty member w/ ID = ",FACULTY_LIST(INDEX)%IDNUM," NAME = ",FACULTY_LIST(INDEX)%NAME," **"
            END IF
        ELSE
            WRITE(*,*) "CAN'T UPDATE FACULTY MEMBER"
        END IF

	END SUBROUTINE UPDATE_DEPT_OP


    ! update title information of a faculty given id and name
	SUBROUTINE UPDATE_TITLE_OP(INPUT_STRING,FACULTY_LIST,HEAD_INDEX)
        CHARACTER*66 			        :: INPUT_STRING
        INTEGER				    	    :: IDNUMBER_ELEM, HEAD_INDEX, INDEX
        CHARACTER*13	    		    :: NAME_ELEM
        CHARACTER*9 	    			:: RANK_ELEM
        TYPE(FACULTY), DIMENSION(100)   :: FACULTY_LIST

        READ(INPUT_STRING, 300) IDNUMBER_ELEM, NAME_ELEM, RANK_ELEM
    300	FORMAT(3X, I8, 1X, A13, 1X, A9)

        INDEX = FIND_FACULTY_BY_NAME_AND_ID(IDNUMBER_ELEM, NAME_ELEM, FACULTY_LIST,HEAD_INDEX)
        IF(INDEX>0) THEN
            IF(FACULTY_LIST(INDEX)%IDNUM==IDNUMBER_ELEM) THEN
                FACULTY_LIST(INDEX)%RANK = RANK_ELEM
                WRITE(*,*) "** Updating faculty member w/ ID = ",FACULTY_LIST(INDEX)%IDNUM," NAME = ",FACULTY_LIST(INDEX)%NAME," **"
            END IF
        ELSE
            WRITE(*,*) "CAN'T UPDATE FACULTY MEMBER"
        END IF
	END SUBROUTINE UPDATE_TITLE_OP


    ! update pay information of a faculty given id and name
	SUBROUTINE UPDATE_PAY_OP(INPUT_STRING,FACULTY_LIST,HEAD_INDEX)
        CHARACTER*66 			        :: INPUT_STRING
        INTEGER				    	    :: IDNUMBER_ELEM, HEAD_INDEX, INDEX
        CHARACTER*13	    		    :: NAME_ELEM
        REAL                            :: PAY_ELEM
        TYPE(FACULTY), DIMENSION(100)   :: FACULTY_LIST

        READ(INPUT_STRING, 300) IDNUMBER_ELEM, NAME_ELEM, PAY_ELEM
    300	FORMAT(3X, I8, 1X, A13, 1X, F8.2)

        INDEX = FIND_FACULTY_BY_NAME_AND_ID(IDNUMBER_ELEM, NAME_ELEM, FACULTY_LIST,HEAD_INDEX)
        IF(INDEX>0) THEN
            IF(FACULTY_LIST(INDEX)%IDNUM==IDNUMBER_ELEM) THEN
                FACULTY_LIST(INDEX)%PAY=PAY_ELEM
                WRITE(*,*) "** Updating faculty member w/ ID = ",FACULTY_LIST(INDEX)%IDNUM," NAME = ",FACULTY_LIST(INDEX)%NAME," **"
            END IF
        ELSE
            WRITE(*,*) "CAN'T UPDATE FACULTY MEMBER"
        END IF
	END SUBROUTINE UPDATE_PAY_OP

END MODULE UPDATE_HELPER
