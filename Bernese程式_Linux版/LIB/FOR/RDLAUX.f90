MODULE s_RDLAUX
CONTAINS

! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE rdlaux(isel,head_out,cdata,irc)

!-------------------------------------------------------------------------
! Purpose:    Reads a LEO-Auxiliary file (accelerations,attitude and
!             thruster firings)
!
! Author:     H.Bock
!
! Created:    24-Jul-2000
!
! Changes:    25-Sep-2002 HU: Remove i_astlib
!             24-Jun-2003 HB: Read revision and version of auxiliary file
!             13-Jan-2009 RD: Use '' as EOF-identifier in NEXTLINE
!             27-Mar-2012 RD: Use LISTC1 as module now
!             20-Aug-2012 SL: Shape of listc1 parameter changed
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

! Modules
! -------
  USE m_bern,    ONLY: i4b, lfnerr, lfn002, fileNameLength
  USE p_leoaux,  ONLY: t_chphead, t_chpdata, maxTyp, initia, head
  USE s_opnfil
  USE f_nextline
  USE s_inquire
  USE s_opnerr
  USE f_listc1
  USE s_exitrc
  USE f_lengt1
  USE s_gtflna
  IMPLICIT NONE

! List of Parameters
! ------------------
! IN:
  INTEGER(i4b)      :: isel     !  =0: open file and read header
                                !  >0: read the data
! OUT:
  type(t_chphead)   :: head_out ! structure with header information
  type(t_chpdata)   :: cdata    ! structure with data information
  INTEGER(i4b)      :: irc      ! return code

! Local Variables
! ---------------
  CHARACTER(LEN=fileNameLength)           :: filchp
  CHARACTER(LEN=80),SAVE                  :: line
  CHARACTER(LEN=3),DIMENSION(maxTyp),SAVE :: list
  CHARACTER(LEN=3)                        :: typ
  CHARACTER(LEN=6)                        :: satNum
  CHARACTER(LEN=5)                        :: satNam

  INTEGER(i4b),DIMENSION(maxTyp),SAVE :: nList
  INTEGER(i4b),SAVE                   :: nDat
  INTEGER(i4b) :: iDat
  INTEGER(i4b) :: iTyp
  INTEGER(i4b) :: irchp
  INTEGER(i4b) :: iostat
  INTEGER(i4b) :: jj

  LOGICAL,SAVE :: first=.TRUE.
  LOGICAL      :: open

! Initialize some values
! ----------------------
  irc = 0

! Read the header
! ---------------
  IF (isel==1) THEN
    CALL gtflna(1,'LEOAUX',filchp,irchp)
    CALL inquire(UNIT=lfn002,OPENED=open)
    IF (open) close(lfn002)

! Open file
! ---------
    CALL opnfil(lfn002,filchp,'OLD',' ', 'READONLY',' ',iostat)
    CALL opnerr(lfnerr,lfn002,iostat,filchp,'RDLAUX')

! Read header at the first call of this SR
! ----------------------------------------
    IF (first) THEN
      first=.FALSE.
! Read title lines
      list(:)=''
      nList=0
      HEADER_LOOP: DO
        line = nextLine(lfn002,0)
        SELECT CASE (line(1:1))
        CASE ('%')
! Wrong datatype
          IF (line(2:6)/='chacc') THEN
            WRITE(lfnerr,'(A)')&
                 '### SR rdlaux: Wrong Datatype in LEO-Auxiliary File'
            CALL exitrc(2)
          ELSE
            READ(line(16:19),'(A)')head%versio
            READ(line(30:32),'(I4)')head%revis
            head_out%versio = head%versio
            head_out%revis = head%revis

! Initialization of attitude data
! -------------------------------
            CALL initia(head)
            CALL initia(head_out)
            CYCLE HEADER_LOOP
          ENDIF
        CASE ('+')
          SELECT CASE (line(2:10))
          CASE ('satellite')
            READ(line(12:80),'(A7,1X,A)')satNum,satNam
            IF (satNum/=head%satNum.and.satNam/=head%satNam) THEN
              WRITE(lfnerr,'(A)')&
                   '### SR rdlaux: Wrong satellite in file'
              CALL exitrc(2)
            ENDIF
          CASE ('data_____')
            ndat=(lengt1(line(11:80))+2)/4              ! number of data types
            LOOP_TYP: DO iDat=1,ndat
              DO iTyp=1,maxTyp
                IF(line((iDat-1)*4+12:(iDat-1)*4+12+2)& ! check of data types
                     &==head%datTyp(iTyp)%typ)THEN      !  in internal list
                  head%flgTyp(iTyp)=1
                  nList(iDat)=iTyp
                  list(iDat)=head%datTyp(iTyp)%typ
                ENDIF
              ENDDO
            ENDDO LOOP_TYP
            CYCLE HEADER_LOOP
          CASE ('reference')
            DO iDat=1,ndat
              DO iTyp=1,maxTyp
                IF(nList(iDat)==iTyp)THEN
                  head%datTyp(iTyp)%ref=line((iDat-1)*4+12:(iDat-1)*4+12+2)
                ENDIF
              ENDDO
            ENDDO
          CASE ('first____')
            READ(line(12:38),'(A)')head%timfst
          CASE ('last_____')
            READ(line(12:38),'(A)')head%timlst
          CASE ('software_')
            CYCLE HEADER_LOOP
          CASE ('acl_k0___')
            READ(line(11:80),'(3F16.10,1X,I1)')head%aclk(1,:),&
                 head%faclk(1)
          CASE ('acl_k1___')
            READ(line(11:80),'(3F16.10,1X,I1)')head%aclk(2,:),&
                 head%faclk(2)
          CASE ('acl_k2___')
          READ(line(11:80),'(3F16.10,1X,I1)')head%aclk(3,:),&
                 head%faclk(3)
          CASE ('aca_k0___')
            READ(line(11:80),'(3F16.10,1X,I1)')head%acak(1,:),&
                 head%facak(1)
          CASE ('aca_k1___')
            READ(line(11:80),'(3F16.10,1X,I1)')head%acak(2,:),&
                 head%facak(2)
          CASE ('aca_k2___')
            READ(line(11:80),'(3F16.10,1X,I1)')head%acak(3,:),&
                 head%facak(3)
          CASE ('scmass___')
            READ(line(12:80),'(F7.3)')head%mass
          CASE ('asc2sc___')
            READ(line(12:80),'(4(1X,F13.10))')head%asc2sc(:)
          CASE ('filt_____')
            READ(line(12:14),'(A3)')head%filt
          CASE ('format___')
            WRITE(lfnerr,'(A)')&
                 '*** SR rdlaux: New format description for data types'
            DO iTyp=1,maxTyp
              IF (line(12:14)==head%datTyp(iTyp)%typ) THEN
                head%datTyp(iTyp)%form=line(16:33)
              ENDIF
              IF (line(35:37)==head%datTyp(iTyp)%typ) THEN
                head%datTyp(iTyp)%form=line(39:68)
              ENDIF
            ENDDO
          CASE DEFAULT
            WRITE(lfnerr,'(A,A10)')&
                 '*** SR rdlaux: Unknown header line specification: ',&
                 line(1:10)
            CYCLE HEADER_LOOP
          END SELECT
        CASE DEFAULT
          EXIT HEADER_LOOP
        END SELECT
      ENDDO HEADER_LOOP
    ENDIF
    head_out = head

! END of HEADER
  ELSEIF (isel>1) THEN

! Data
! ----
    jj=0
    cdata%flgdat(:) = cdata%flgdat(:) - 1
    epoch_loop: DO
! Search for index in list of data types
      iTyp=listc1(0,3,maxTyp,list(1:1),line(1:3),nDat)
      SELECT CASE (nList(iTyp))
      CASE(1) ! TYP tim
        READ(line,head%datTyp(nList(iTyp))%form)typ,cdata%timstr
        cdata%flgdat(nList(iTyp))=cdata%flgdat(nList(iTyp))+1
      CASE(2) ! TYP acl
        READ(line,head%datTyp(nList(iTyp))%form)typ,cdata%acl(:),cdata%sacl
        cdata%flgdat(nList(iTyp))=cdata%flgdat(nList(iTyp))+1
      CASE(3) ! TYP aca
        READ(line,head%datTyp(nList(iTyp))%form)typ,cdata%aca(:),cdata%saca
        cdata%flgdat(nList(iTyp))=cdata%flgdat(nList(iTyp))+1
      CASE(4) ! TYP acc
        jj=jj+1
        READ(line,head%datTyp(nList(iTyp))%form)typ,cdata%facc(jj,:),&
             cdata%acc(jj,:)
        cdata%flgdat(nList(iTyp))=cdata%flgdat(nList(iTyp))+1
      CASE(5) ! TYP att
        READ(line,head%datTyp(nList(iTyp))%form)typ,cdata%starfl(:),&
             cdata%att(:),cdata%quatt
        cdata%flgdat(nList(iTyp))=cdata%flgdat(nList(iTyp))+1
      CASE(6) ! TYP thr
        READ(line,head%datTyp(nList(iTyp))%form)typ,cdata%thrflg(:),&
             cdata%thrpul
        cdata%flgdat(nList(iTyp))=cdata%flgdat(nList(iTyp))+1
      CASE(7) ! TYP hk1
        READ(line,head%datTyp(nList(iTyp))%form)typ,cdata%hk1(:)
        cdata%flgdat(nList(iTyp))=cdata%flgdat(nList(iTyp))+1
      CASE(8) ! TYP hk2
        READ(line,head%datTyp(nList(iTyp))%form)typ,cdata%hk2(:)
        cdata%flgdat(nList(iTyp))=cdata%flgdat(nList(iTyp))+1
      CASE(9) ! TYP hk3
        READ(line,head%datTyp(nList(iTyp))%form)typ,cdata%flhk3(:),cdata%nbc,&
             cdata%hk3(:)
        cdata%flgdat(nList(iTyp))=cdata%flgdat(nList(iTyp))+1
      CASE(10) ! TYP hk4
        READ(line,head%datTyp(nList(iTyp))%form)typ,cdata%hk4(:)
        cdata%flgdat(nList(iTyp))=cdata%flgdat(nList(iTyp))+1
      CASE(11) ! TYP hka
        READ(line,head%datTyp(nList(iTyp))%form)typ,cdata%tmpacc
        cdata%flgdat(nList(iTyp))=cdata%flgdat(nList(iTyp))+1
      CASE(12) ! TYP fgm
        READ(line,head%datTyp(nList(iTyp))%form)typ,cdata%fgm(:),cdata%fgmnr
        cdata%flgdat(nList(iTyp))=cdata%flgdat(nList(iTyp))+1
      CASE DEFAULT
!        write(lfnerr,*)
!!        write(*,*)'kein solcher Typ in Liste vorhanden!!!!',line(1:3)
      END SELECT
      line = nextLine(lfn002,0)
      IF (line(1:4)=='%eof'.OR.line=='') THEN
        irc = 2
        EXIT epoch_loop
      ELSEIF (line(1:3)=='tim') THEN ! Next sequence starts
        EXIT epoch_loop
      ENDIF
    ENDDO epoch_loop
  ENDIF

  RETURN
  END SUBROUTINE rdlaux

END MODULE
