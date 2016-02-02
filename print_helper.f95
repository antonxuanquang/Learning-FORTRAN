!**************************************************************************!
!*  This module helps the main program print information of all (some)    *!
!*  faculty members.                                                      *!
!**************************************************************************!

MODULE PRINT_HELPER

    USE CLASS_FACULTY
    USE FINDER
	IMPLICIT NONE

	CONTAINS

    ! prints all faculty information in the linked list
	SUBROUTINE PRINT_ALL_OP(FACULTY_LIST,HEAD_INDEX)
        INTEGER                         :: HEAD_INDEX, INDEX
        TYPE(FACULTY), DIMENSION (100)  :: FACULTY_LIST

        INDEX = HEAD_INDEX
        DO WHILE (INDEX >= 0)
            WRITE (*,*) PRINT_FACULTY(FACULTY_LIST(INDEX))
            INDEX = NEXT(FACULTY_LIST(INDEX));
        END DO

	END SUBROUTINE PRINT_ALL_OP

    ! prints all faculty members that have certain title
	SUBROUTINE PRINT_TITLE_OP(INPUT_STRING,FACULTY_LIST,HEAD_INDEX)
        CHARACTER*66                    :: INPUT_STRING
        CHARACTER*9                     :: TITLE
        TYPE(FACULTY), DIMENSION(100)   :: FACULTY_LIST
        INTEGER                         :: HEAD_INDEX, CURRENT

            READ(INPUT_STRING, 200) TITLE
        200 FORMAT(3X, A9)

            CURRENT = HEAD_INDEX
            DO WHILE (CURRENT /= -99)
                IF (FACULTY_LIST(CURRENT)%RANK == TITLE) THEN
                    IF(CURRENT>0) THEN
                        WRITE(*,*) PRINT_FACULTY(FACULTY_LIST(CURRENT))
                    ELSE
                        WRITE (*,*) "FACULTY MEMBER DOES NOT EXIST"
                    END IF
                END IF
                CURRENT = NEXT(FACULTY_LIST(CURRENT));
            END DO

	END SUBROUTINE PRINT_TITLE_OP

    ! prints all faculty members that in certain department
	SUBROUTINE PRINT_DEPT_OP(INPUT_STRING,FACULTY_LIST,HEAD_INDEX)
        CHARACTER*66                    :: INPUT_STRING
        CHARACTER*4                     :: DEPT
        TYPE(FACULTY), DIMENSION(100)   :: FACULTY_LIST
        INTEGER                         :: HEAD_INDEX, CURRENT

            READ(INPUT_STRING, 200) DEPT
        200 FORMAT(3X, A4)

            CURRENT = HEAD_INDEX
            DO WHILE (CURRENT /= -99)
                IF (FACULTY_LIST(CURRENT)%DEPT == DEPT) THEN
                    IF(CURRENT>0) THEN
                        WRITE(*,*) PRINT_FACULTY(FACULTY_LIST(CURRENT))
                    ELSE
                        WRITE (*,*) "FACULTY MEMBER DOES NOT EXIST"
                    END IF
                END IF
                CURRENT = NEXT(FACULTY_LIST(CURRENT));
            END DO
	END SUBROUTINE PRINT_DEPT_OP

    ! prints all faculty members that have certain id number
	SUBROUTINE PRINT_ID_OP(INPUT_STRING,FACULTY_LIST,HEAD_INDEX)
        CHARACTER*66                    :: INPUT_STRING
        INTEGER                         :: IDNUMBER, HEAD_INDEX, CURRENT
        TYPE(FACULTY), DIMENSION(100)   :: FACULTY_LIST

            READ(INPUT_STRING, 200) IDNUMBER
        200 FORMAT(3X, I8)

            CURRENT = FIND_FACULTY_MEMBER(IDNUMBER, FACULTY_LIST, HEAD_INDEX)
            IF(CURRENT>0) THEN
                WRITE (*,*) PRINT_FACULTY(FACULTY_LIST(CURRENT));
            ELSE
                WRITE (*,*) "FACULTY MEMBER DOES NOT EXIST"
            END IF


	END SUBROUTINE PRINT_ID_OP

END MODULE PRINT_HELPER
