MODULE s_WRITELE
CONTAINS


! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE WRITELE(title,elefil,numsat,stvtim,eleold,elenew,satnum,orbdsc, &
                   orbSys)

! -------------------------------------------------------------------------
! Purpose:    Writing ELE file according to given parameters
!
! Author:     K. Sosnica
!
! Created:    10-Jan-2011
!
! Changes:    21-Apr-2011 DT: Some modifications; add orbSys to SR call;
!                             satcom->satnum (directly PRN);
!                             remove unused variables
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

  USE m_bern,   ONLY: i4b, r8b, lfnloc, lfnerr, fileNameLength
  USE m_maxdim, ONLY: maxsat
  USE p_orbgen, ONLY: t_orbmodel

  USE s_opnerr
  USE s_opnfil

  IMPLICIT NONE

! List of Parameters
! ------------------
  CHARACTER(LEN=80)                                :: title
  CHARACTER(LEN=fileNameLength)                    :: elefil
  TYPE(t_orbmodel)                                 :: orbdsc

  INTEGER(i4b)                                     :: numsat
  INTEGER(i4b)                                     :: orbSys
  INTEGER(i4b), DIMENSION(numsat)                  :: satnum

  REAL(r8b)                                        :: stvtim(*)
  REAL(r8b)                                        :: eleold(numsat,6)
  REAL(r8b)                                        :: elenew(numsat,6)

! Local Parameters
! ----------------

! Local Variables
! ---------------
  CHARACTER(LEN=23), DIMENSION(16)  :: txtele

  INTEGER(i4b)                      :: ioStat
  INTEGER(i4b)                      :: iele
  INTEGER(i4b)                      :: iii
  INTEGER(i4b)                      :: ifmtel

  REAL(r8b)                         :: rmsi


! Some initializations
! --------------------
  IFMTEL = 1
  RMSI = 0d0


! Get filename
! ------------
  CALL opnfil(lfnloc,elefil,'UNKNOWN',' ', ' ',' ',ioStat)
  CALL opnerr(lfnerr,lfnloc,ioStat,elefil,'sr writele')

! TEXT TO BE WRITTEN INTO THE OUTPUT FILE:
! ---------------------------------------
  TXTELE(1) ='ARC-NUMBER          ='
  TXTELE(2) ='A                   ='
  TXTELE(3) ='E                   ='
  TXTELE(4) ='I                   ='
  TXTELE(5) ='NODE                ='
  TXTELE(6) ='PERIGEE             ='
  TXTELE(7) ='ARG. OF LAT (START) ='
  TXTELE(8) ='D0                  ='
  TXTELE(9) ='S0                  ='
  TXTELE(10)='W0                  ='
  TXTELE(11)='RC                  ='
  TXTELE(12)='SC                  ='
  TXTELE(13)='WC                  ='
  TXTELE(14)='RS                  ='
  TXTELE(15)='SS                  ='
  TXTELE(16)='WS                  ='


! PRINT TITLE
! -----------
  WRITE(LFNLOC,"(1X,A80)") TITLE

  WRITE(LFNLOC,"(80('-'))")
  WRITE(LFNLOC,"('FORMAT: ',2I6)") IFMTEL,ORBDSC%NLIN

  DO IELE=1,ORBDSC%NLIN
    WRITE(LFNLOC,"(A80)") orbdsc%orbmod(IELE)
  ENDDO

  WRITE(LFNLOC,"(80('-'))")

  DO III=1,numsat

    ! WRITE TITLE LINE FOR SATELLITE
    ! ------------------------------
    WRITE(LFNLOC, "(A21,I3,' SATELLITE           =',I3,' TOSC=',F20.12, /,75('-'))") &
          TXTELE(1), 1, satnum(iii), stvtim(1)

    ! Write osculating elements
    ! -------------------------
    DO IELE=1,6
      IF(IELE.EQ.1)THEN
        WRITE(LFNLOC,"(A21,2F16.5,' +-',F12.3,4X,'ORBSYS',I2)")  &
              TXTELE(1+IELE),ELEOLD(iii,IELE),ELENEW(iii,IELE),RMSI, &
              orbSys

      ELSE IF(IELE.EQ.2)THEN
        WRITE(LFNLOC,"(A21,2F16.10,' +-',F12.9,4X,'ORBSYS',I2)") &
              TXTELE(1+IELE),ELEOLD(iii,IELE),ELENEW(iii,IELE),RMSI, &
              orbSys

      ELSE IF(IELE.GE.3.AND.IELE.LE.6)THEN
        WRITE(LFNLOC,"(A21,2F16.9,' +-',F12.9,4X,'ORBSYS',I2)" ) &
              TXTELE(1+IELE),ELEOLD(iii,IELE),ELENEW(iii,IELE),RMSI, &
              orbSys
      END IF

    ENDDO

    ! WRITE RADIATION PRESSURE PARAMETERS
    ! -----------------------------------
    DO IELE=7,15
      WRITE(LFNLOC,"(A21, 2D16.9,' +-',D12.5,2X,A1,1X,A8)") &
                TXTELE(1+IELE),0.0,0.0,RMSI,"*","C061001F"
    ENDDO

  ENDDO

! Close file
! ----------
  CLOSE (lfnloc)

  RETURN


END SUBROUTINE WRITELE

END MODULE
