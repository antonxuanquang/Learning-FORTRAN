MODULE NODEMODULE
!---------------------------------------------------------------------
!
!  Module containing definitions needed to define node for the linked list
!  Must be compiled before any program units that use the module. When
!  compiled, a .o file is created AND also a .mod file that must be present
!  in order to compile a unit that uses the module
!
!---------------------------------------------------------------------
 	IMPLICIT NONE
  	TYPE NODE
		CHARACTER(15)  :: NAME
		CHARACTER(4)  :: DEPARTMENT
    	INTEGER AGE
    	TYPE (NODE), POINTER :: LINK
    END TYPE NODE
    
    CONTAINS
!---------------------------------------------------------------------
!
!  Subroutines and functions that work on nodes in the linked list
!
!---------------------------------------------------------------------
    SUBROUTINE TRAVERSE(PTR1)  
	    IMPLICIT NONE 
		TYPE (NODE), POINTER :: PTR1 
		WRITE(*,*) 'LOOP OVER ALL ELEMENTS'

		DO
			if(.NOT.ASSOCIATED(PTR1)) EXIT
			WRITE(*,*)  PTR1%NAME, '  ', PTR1%DEPARTMENT, '  ', PTR1%AGE
			PTR1 => PTR1%LINK
		END DO
	END SUBROUTINE TRAVERSE 
END MODULE NODEMODULE



PROGRAM MAIN
	USE NODEMODULE
	
	IMPLICIT NONE
	
    TYPE (NODE), POINTER :: HEAD, TEMP1, TEMP2
    
    WRITE(*,*) "START OF LIST"
    
    ALLOCATE(HEAD)
    	HEAD%NAME = "BILL GATES"
    	HEAD%DEPARTMENT = "CHEM"
    	HEAD%AGE = 25
    	
    ALLOCATE(TEMP1)
    	TEMP1%NAME = "JOE BLOW"
    	TEMP1%DEPARTMENT = "BIOL"
    	TEMP1%AGE = 18
    	
    ALLOCATE(TEMP2)
    	TEMP2%NAME = "STEVE JOBS"
    	TEMP2%DEPARTMENT = "COSC"
    	TEMP2%AGE = 28

    HEAD%LINK => TEMP1
    TEMP1%LINK => TEMP2
    NULLIFY(TEMP2%LINK)  
       
    CALL TRAVERSE(HEAD)
    
    WRITE (*,*) "END OF LIST REACHED"
END PROGRAM
