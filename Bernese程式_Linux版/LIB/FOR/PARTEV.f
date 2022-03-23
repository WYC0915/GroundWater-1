      MODULE s_PARTEV
      CONTAINS

C*
      SUBROUTINE PARTEV(nvar,iarc,OBSTIM,TA,TB,QVAR,HVAR,T0VAR,
     1                  coeVAR,NSHAD,TSHAD,locq,DELEDP)
CC
CC NAME       : PARTEV
CC
CC PURPOSE    : EVALUATE PARTIAL DERIVATES W.R.T. DYNAMICAL ORBIT
CC              PARAMETERS UNDER CONSIDERATION OF THE LIGHT-SHADOW
CC              BOUNDARIES
CC
CC PARAMETERS :
CC        IN  : nvar    : # DYNAMICAL PARAMETERS                    I*4
CC              iarc    : CURRENT ARC NUMBER                        I*4
CC              OBSTIM  : OBSERVATION TIME (MJD)                    R*8
CC              TA, TB  : INTERVAL BOUNDARIES                       R*8
CC              QVAR    : ORDER OF apPROXIMATION                    I*4
CC              HVAR    : NORMALIZATION CONSTANT                    R*8
CC              T0VAR   : ORIGIN OF DEVELOPMENT                     R*8
CC              coeVAR  : coeFFICIENTS OF DEVELOPMENT               R*8
CC              NSHAD   : TOTAL NUMBER OF SHADOW PASSES             R*8
CC              THAD    : SHADOW BOUNDARIES (MJD)                   R*8
CC              locq    : PARAMETER CHARACTERIZATION                R*8
CC        OUT : DELEDP  : RESULTING PARTIALS                        R*8
CC
CC REMARKS    :
CC
CC AUTHOR     :  G.BEUTLER
CC
CC VERSION    :  4.1
CC
CC CREATED    :  98/03/08
CC
CC CHANGES    :  18-SEP-12 : RD: CHECK ARRAY ALLOCATION
CC               18-SEP-12 : RD: USE M_BERN WITH ONLY
CC               18-SEP-12 : RD: REMOVE UNUSED VARIABLES/MODULES
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1998     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      USE m_bern,  ONLY: i4b, r8b

      USE s_polevn
      USE s_alcerr

      IMPLICIT NONE
      INTEGER(i4b) :: maxshd
      INTEGER(i4b) :: iarc
      REAL(r8b) :: ta
      REAL(r8b) :: taold
      INTEGER(i4b) :: nshadl
      INTEGER(i4b) :: ishad
      INTEGER(i4b) :: nshad
      REAL(r8b) :: tb
      REAL(r8b) :: dt
      REAL(r8b) :: t0var
      INTEGER(i4b) :: nvar
      REAL(r8b) :: obstim
      INTEGER(i4b) :: ivar
      INTEGER(i4b) :: ind0
      INTEGER(i4b) :: k
      REAL(r8b) :: hvar
      INTEGER(i4b) :: iac

      PARAMETER (MAXSHD=50)
C
      INTEGER*4   QVAR,locq(:,:,:)
      REAL*8      coeVAR(:),TSHAD(:,:),DELEDP(*)
      REAL*8      TSHADL(2,MAXSHD)

      REAL(r8b),DIMENSION(:,:,:),ALLOCATABLE,SAVE  :: PART
      INTEGER(i4b),SAVE :: iarcOLD=0
C
      DATA TAOLD/0.D0/

! *********************
! ALLOCATE ARRAY
! --------------
      IF (iarc/=iarcOLD) THEN
        IF (iarcOLD==0) THEN
          ALLOCATE(PART(6*locq(6,1,iarc),2,MAXSHD))
          CALL ALCERR(IAC,'PART',(/6*locq(6,1,iarc),2,MAXSHD/),'PARTEV')
        ELSE IF (locq(6,7,iarc)/=locq(6,7,iarcOLD)) THEN
          IF (ALLOCATED(PART)) DEALLOCATE(PART)
          ALLOCATE(PART(6*locq(6,1,iarc),2,MAXSHD))
          CALL ALCERR(IAC,'PART',(/6*locq(6,1,iarc),2,MAXSHD/),'PARTEV')
        END IF
        iarcOLD=iarc
      END IF
! *********************
C
C SHADOW PASSES WITHIN CURRENT SUBINTERVAL
C ----------------------------------------
      IF(TA.NE.TAOLD)THEN
        TAOLD=TA
        NSHADL=0
        DO ishad=1,NSHAD
C
C SHADOW EXIT IN CURRENT INTERVAL?
          IF(TSHAD(2,ishad).GT.TA.AND.TSHAD(1,ishad).LE.TB)THEN
            NSHADL=NSHADL+1
            IF(TSHAD(2,ishad).LT.TB)THEN
              TSHADL(2,NSHADL)=TSHAD(2,ishad)
            ELSE
              TSHADL(2,NSHADL)=TB
            ENDIF
C
            IF(TSHAD(1,ishad).GE.TA)THEN
              TSHADL(1,NSHADL)=TSHAD(1,ishad)
            ELSE
              TSHADL(1,NSHADL)=TA
            ENDIF
          ENDIF
        ENDDO
C
C EVALUATE PARTIALS AT INTERVAL BOUNDARIES
        DO ishad=1,NSHADL
          DT=TSHADL(1,ishad)-T0VAR
          CALL POLEVN(1,QVAR,6*nvar,DT,HVAR,coeVAR,
     &                PART(1,1,ishad))
          DT=TSHADL(2,ishad)-T0VAR
          CALL POLEVN(1,QVAR,6*nvar,DT,HVAR,coeVAR,
     &                PART(1,2,ishad))
        ENDDO
      END IF
cc        if(nshadl.ne.0)then
cc        write(*,*)'nshadl=',nshadl
cc        do i=1,nshadl
cc          write(*,*)'i,ta,tb=',i,ta,tb
cc          write(*,*)'i,tshad=',i,(tshadl(k,i),k=1,2)
cc        enddo
cc        PAUSE
cc        endif
C
C NORMAL EVALUATION OF PARTIALS
C -----------------------------
      DT=OBSTIM-T0VAR
      CALL POLEVN(1,QVAR,6*nvar,DT,HVAR,coeVAR,DELEDP)
C
C CORRECT FOR SHADOW PASSES
C -------------------------
      DO ishad=1,NSHADL
C
C SHADOW PASS # ishad OVER?
        IF(OBSTIM.GE.TSHADL(2,ishad))THEN
          DO IVAR=1,nvar
            IF((locq(4,6+IVAR,iarc).lt.16.and.
     1          locq(7,6+IVAR,iarc).EQ.2).OR.
     1          locq(4,6+IVAR,iarc).EQ.19)THEN
              IND0=6*(IVAR-1)
              DO K=6,1,-1
                DELEDP(IND0+K) = DELEDP(IND0+K) -
     1                (PART(IND0+K,2,ishad)-PART(IND0+K,1,ishad))
              ENDDO
            ENDIF
          ENDDO
        ENDIF
C
C OBSTIM IN SHADOW PASS # ishad?
        IF(OBSTIM.GT.TSHADL(1,ishad).AND.
     &     OBSTIM.LT.TSHADL(2,ishad))THEN
          DO IVAR=1,nvar
            IF((locq(4,6+IVAR,iarc).lt.16.and.
     1          locq(7,6+IVAR,iarc).EQ.2).OR.
     1          locq(4,6+IVAR,iarc).EQ.19)THEN
              IND0=6*(IVAR-1)
              DO K=6,1,-1
                DELEDP(IND0+K)=PART(IND0+K,1,ishad)
              ENDDO
            ENDIF
          ENDDO
        ENDIF
      ENDDO

999   CONTINUE
      RETURN
      END SUBROUTINE

      END MODULE
