       program grdintrp
c      program to interpolate 3-D deformation gridded results to a particular latitude and longitude
c      the program uses a bicubic spline subroutine from numerical recipes
c      and longitude
c
c      displacement grid is 1. deg x 1. deg
c      361 longitudes
c      181 latitudes
c
c      dr=radial displacements
c      vt=horizontal (north) displacement
c      vl=horizontal (east) displacement
c
c      the program takes as input the sine and cosine of the displacements (mm):
c      ss1,cs1,ss2,cs2
c
c      the user needs to specify iref(reference frame) and iout(output format)
c      within the program
c      and the program prompts the user for the station latitude and longitude
c
c      rlat and rlon are the latitudes and longitudes of displacement grid
c
c
c      program written: september, 2005 T. vanDam
c
c      please note: i already know that i am not the most efficient or clear
c      programmer in the world. so you might have to work some things out on your own
C
C Created:    16-Sep-2010
C
C Changes:    16-Sep-2010 RD: Adapted to BSW environment
C             06-Oct-2010 RD: EXITRC added at the end of the pgm
C             15-Nov-2010 RD: CMC corrections added to the ATL-file
C             26-Jan-2011 RD: Use GETCO3 instead of GETCOO
C             07-Jul-2011 RD: Accept also the middle of a cell
C             14-Nov-2011 SL: use m_bern with ONLY, PRITIT call added
C             08-Feb-2012 RD: Add a blank line after the last record
C             27-Apr-2012 RD: Nullify pointers
C
C
C Modules need for the Bernese part:
       USE m_bern,   ONLY: i4b, r8b,
     1                     shortLineLength, lineLength, keyValueLength,
     1                     fileNameLength, staNameLength, lfnErr
       USE m_cpu,    ONLY: cpu_start
       USE d_inpkey, ONLY: inpKey, init_inpkey
       USE d_const,  ONLY: rho
C
       USE s_readinpf
       USE s_opnsys
       USE s_defcon
       USE s_pritit
       USE s_gtflna
       USE s_opnfil
       USE s_opnerr
       USE s_getco3
       USE s_exitrc
       USE s_readKeys
       USE s_ckoptr
       USE s_ckoptc
       USE s_getdat
       USE s_xyzell
       implicit none
C
C Variables need for the Bernese part:
       CHARACTER(LEN=7), PARAMETER            :: pgName = 'GRDS1S2'
       CHARACTER(LEN=8), PARAMETER,
     1                   DIMENSION(2,2,3)     :: S1S2keys =
     2 reshape ( source =
     3 (/ 'CMC_S1CX','CMC_S2CX','CMC_S1SX','CMC_S2SX',
     4    'CMC_S1CY','CMC_S2CY','CMC_S1SY','CMC_S2SY',
     5    'CMC_S1CZ','CMC_S2CZ','CMC_S1SZ','CMC_S2SZ' /),
     6    shape = (/ 2,2,3 /) )
       CHARACTER(LEN=3), PARAMETER,
     1                   DIMENSION(2)         :: cmcTxt=(/' NO','YES'/)

       INTEGER(i4b)                                      :: iCmc
       REAL(r8b),        DIMENSION(2,2,3)                :: S1S2cmc
       CHARACTER(LEN=shortLineLength)                    :: CMCtext
       CHARACTER(LEN=lineLength)                         :: line
       CHARACTER(LEN=keyValueLength),POINTER,
     1                               DIMENSION(:)        :: keyValue

       CHARACTER(LEN=3)                                  :: cmcYN
       CHARACTER(LEN=fileNameLength)                     :: fileout
       CHARACTER(LEN=fileNameLength)                     :: coofil
       INTEGER(i4b)                                      :: nStat
       CHARACTER(LEN=staNameLength), DIMENSION(:), POINTER   :: stName
       INTEGER(i4b),                 DIMENSION(:), POINTER   :: stanum
       CHARACTER(LEN=1),             DIMENSION(:), POINTER   :: staFlg
       REAL(r8b),                    DIMENSION(:,:), POINTER :: xStat
       REAL(r8b),                    DIMENSION(3)        :: xStell
       CHARACTER(LEN=staNameLength)                      :: datum
       REAL(r8b)                                         :: aell,bell
       REAL(r8b),                    DIMENSION(3)        :: dxell,drell
       REAL(r8b)                                         :: scell
       INTEGER(i4b)                                      :: irc, ios
       INTEGER(i4b)                                      :: irCode
       INTEGER(i4b)                                      :: ii,jj,kk
       REAL                                              :: hlp
       LOGICAL                                           :: sorted
C
       integer mxlon,mxlat,nlon,nlat,i,j,iref,iout,ista
       parameter(mxlon=361,mxlat=182)
       real*4 ss1dr(mxlon,mxlat),cs1dr(mxlon,mxlat),ss2dr(mxlon,mxlat),
     +  cs2dr(mxlon,mxlat),
     +  ss1vt(mxlon,mxlat),cs1vt(mxlon,mxlat),ss2vt(mxlon,mxlat),
     +  cs2vt(mxlon,mxlat),
     +  ss1vl(mxlon,mxlat),cs1vl(mxlon,mxlat),ss2vl(mxlon,mxlat),
     +  cs2vl(mxlon,mxlat)
       real*4 ss1dri(mxlon,mxlat),cs1dri(mxlon,mxlat),
     +  ss2dri(mxlon,mxlat),cs2dri(mxlon,mxlat),
     +  ss1vti(mxlon,mxlat),cs1vti(mxlon,mxlat),ss2vti(mxlon,mxlat),
     +  cs2vti(mxlon,mxlat),
     +  ss1vli(mxlon,mxlat),cs1vli(mxlon,mxlat),ss2vli(mxlon,mxlat),
     +  cs2vli(mxlon,mxlat)
       real*4 utot(4),vttot(4),vltot(4)
       real*4 uamp1,uamp2,upha1,upha2,vtamp1,vtamp2,vtpha1,vtpha2,
     +  vlamp1,vlamp2,vlpha1,vlpha2
       real*4 rlat(mxlat),rlon(mxlon),rlt,rln,skp1,skp2,orlt,erln,dtr,
     + corlt
       parameter(dtr=6.2831853071795865/360.)
       character sta*4,filein*48
C
C START CPU COUNTER
C -----------------
      CALL cpu_start(.TRUE.)
C
C NULLIFY POINTERS
C ----------------
      NULLIFY(staNum)
      NULLIFY(stName)
      NULLIFY(staFlg)
      NULLIFY(xStat)
      NULLIFY(keyValue)
C
CC
      CALL init_inpkey(inpKey)
      CALL readinpf(' ',inpKey)
C
      CALL opnsys
      CALL defcon(0)
      CALL pritit(pgName,
     1            'Extract atmospheric tidal loading coefficients')
C
C Read CMC corrections
C --------------------
      irCode = 0
      CALL readKeys('CMCYN',keyValue,irc)
C
      CALL ckoptc(1,'CMCYN',keyValue,
     1            (/'CoM','CoF'/),pgName,
     2            'CMC applied',irc,irCode,
     3            valList=(/0,1/),maxVal=1,result1=iCmc)
C
      cmcYN = cmcTxt(iCmc+1)
C
C CMC in case of the CoF-frame
      IF ( iCmc==1 ) THEN
        CALL readKeys('CMCMODEL',keyValue,irc)
C
        CALL ckoptc(1,'CMCMODEL',keyValue,
     1              (/'NONE     ','RAY_PONTE','MANUAL   '/),pgName,
     2              'Model for CMC corrections',irc,irCode,
     3              valList=(/0,1,-1/),maxVal=1,result1=iCmc)
      ENDIF
C
      S1S2cmc = 0d0
C
C Coefficients from RAY_PONTE
      IF (iCmc == 1) THEN
C          A1 (cos)    B1 (sin)    A2 (cos)    B2 (sin)
C     dX  2.1188E-04 -7.6861E-04  1.4472E-04 -1.7844E-04
C     dY -7.2766E-04 -2.3582E-04 -3.2691E-04 -1.5878E-04
C     dZ -1.2176E-05  3.2243E-05 -9.6271E-05  1.6976E-05
       S1S2cmc = reshape ( source =
     1 (/  2.1188D-04, 1.4472D-04,-7.6861D-04,-1.7844D-04,
     2    -7.2766D-04,-3.2691D-04,-2.3582D-04,-1.5878D-04,
     3    -1.2176D-05,-9.6271D-05, 3.2243D-05, 1.6976D-05 /),
     4    shape = (/ 2,2,3 /) )
        CMCtext = 'Ray_Ponte,2003'
C
C User defined set of coefficients
      ELSE IF (iCmc == -1) THEN
        DO ii = 1,2
          DO jj = 1,2
            DO kk = 1,3
              CALL readkeys(S1S2keys(ii,jj,kk), keyValue, irc)

              CALL ckoptr(1,S1S2keys(ii,jj,kk), keyValue, pgName,
     1                    'CMC corrections, manual',irc,irCode,
     2                    result1=S1S2cmc(ii,jj,kk))
            ENDDO
          ENDDO
        ENDDO
C
        CALL readKeys('CMCNAME',keyValue,irc)
        CMCtext = keyValue(1)
        IF (LEN_TRIM(CMCtext) == 0) CMCtext = 'USER values'
      ENDIF
C
      DEALLOCATE(keyValue,stat=irc)
      IF (irCode /= 0) CALL exitrc(2)
C
      CALL gtflna(1,'COORD',coofil,irc)
      CALL getco3(coofil,1,(/'@'/), NSTAT ,STNAME,stanum=STANUM,
     1    staflg=STAFLG,xstat=XSTAT ,datum=DATUM)

      CALL getdat(DATUM ,AELL  ,BELL  ,DXELL ,DRELL ,SCELL)
C
C Start the original source code here
C -----------------------------------
c bsw     specify reference frame:
c bsw     if reference frame=ce then iref=1
c bsw     if reference frame=cm then iref=2
c bsw     if reference frame=cf then iref=3
c bsw      iref=1
c bsw      if(iref .eq. 1) filein='s1_s2_def_ce.dat'
c bsw      if(iref .eq. 2) filein='s1_s2_def_cm.dat'
c bsw       if(iref .eq. 3) filein='s1_s2_def_cf.dat'
c bsw       open(unit=11,status='old',file=filein)
c bsw       open(unit=12,status='old',file='in.grdintrp')
c bsw       open(unit=21,status='unknown',file='grdintrp.dat')
C
C Open files in a Bernese style manner
C ------------------------------------
       CALL gtflna(1,'S1_S2_DATA',filein,irc)
       CALL opnfil(11,filein,'OLD','FORMATTED','READONLY',' ',irc)
       CALL opnerr(lfnerr,11,irc,filein,'GRDS1S2')
C
       CALL gtflna(1,'S1_S2_OUT',fileout,irc)
       CALL opnfil(21,fileout,'UNKNOWN','FORMATTED',' ',' ',irc)
       CALL opnerr(lfnerr,21,irc,fileout,'GRDS1S2')
C
C Now the original source continues
C ---------------------------------
c      check the next line: iout=1 => output sines and cosines; iout=2 output amplitudes and phases
       iout=1
       write(21,'(3(A,/),A)')
     1       '$$ Atmospheric Tidal loading displacememt',
     2       '$$ ',
     3       '$$ calculated using grdinterp.f (t. van Dam)',
     4       '$$ '
       do
         read(11,'(A)') line
         if ( line(1:2) == '$$' ) then
           write(21,'(A)') TRIM(line)
         else
           exit
         endif
       enddo
       write(21,'(3(A,/),A)')
     7       '$$ ',
     8       '$$ Displacement is defined positive up, north',
     9       '$$ and east directions',
     1       '$$ '
       if(iout .eq. 1) then
         write(21,'(8(A,/),A)')
     1       '$$ Column order coss1 sins1 coss2 sins2',
     2       '$$ ',
     3       '$$ Row order:',
     4       '$$ RADIAL',
     5       '$$ TANGENTIAL NS',
     6       '$$ TANGENTIAL EW',
     7       '$$ ',
     8       '$$ ',
     9       '$$ '
       else
         write(21,'(10(A,/),A)')
     1       '$$ Phase lag relative to Greenwich and the lags',
     2       '$$ are positive',
     3       '$$ Column order: radial s1, radial s2, north s1,',
     4       '$$ north s2, east s1, east s2',
     5       '$$ ',
     6       '$$ Row order:',
     7       '$$ Amplitude (mm)',
     8       '$$ Phase (deg)',
     9       '$$ ',
     1       '$$ ',
     2       '$$ '
       end if
C
C BSW-specific: add section with CMC corrections
C ----------------------------------------------
       IF (iCmc /= 0) THEN
         write(21,'(A)') '$$ CMC:  '//cmcYN//
     1            '  (corr.tide centre of mass)'
         write(21,'(A)') '$$ '
         write(21,'(A)') '$$ Model name:       '//TRIM(CMCtext)
         write(21,'(A)') '$$ '
         write(21,'(A)') '$$ CMC start : ' //
     1            'center of mass coefficient file content (X,Y,Z)'
         write(21,'(A)') '$$ CMC format: (a,1p,t42,3(2x,2e12.4))'
         write(21,'(a,1p,a36,3(2x,2e12.4))') '$$ CMC frequ : S1   ',
     1            CMCtext,(S1S2cmc(1,1:2,ii),ii=1,3)
         write(21,'(a,1p,a36,3(2x,2e12.4))') '$$ CMC frequ : S2   ',
     1            CMCtext,(S1S2cmc(2,1:2,ii),ii=1,3)
         write(21,'(A)') '$$ CMC end   :'
         write(21,'(A)') '$$ '
       ENDIF
       write(21,'(A)') '$$ '
       write(21,'(A)') '$$ END HEADER'
       write(21,'(A)') '$$ '
C
C Start the original source code here
C -----------------------------------
c bsw       do i=1,nlat
c bsw         rlat(i)=(i-1)*1.
c bsw       end do
c bsw       rlat(nlat)=179.99
c bsw       do i=1,nlon
c bsw         rlon(i)=(i-1)*1.
c bsw       end do
c bsw       do i=1,nlon
c bsw         do j=1,nlat
c bsw           read(11,*) skp1,skp2,
c bsw     +                cs1dr(i,j),ss1dr(i,j),cs2dr(i,j),ss2dr(i,j),
c bsw     +                cs1vt(i,j),ss1vt(i,j),cs2vt(i,j),ss2vt(i,j),
c bsw     +                cs1vl(i,j),ss1vl(i,j),cs2vl(i,j),ss2vl(i,j)
c bsw         end do
c bsw       end do
       nlon = 0
       nlat = 0
C
       rlon=1.e30
       rlat=1.e30
       ios = 0
       DO WHILE (ios == 0)
         read(line,*) skp1,skp2
         do i=1,mxlon
           if (rlon(i) == skp1 .OR. rlon(i) == 1.e30) then
             if (rlon(i) == 1.e30) nlon=nlon+1
             do j=1,mxlat
               if (rlat(j) == skp2 .OR. rlat(j) == 1.e30) then
                 if (rlat(j) == 1.e30) nlat=nlat+1
                 read(line,*,iostat=ios) rlon(i),rlat(j),
     +                cs1dr(i,j),ss1dr(i,j),cs2dr(i,j),ss2dr(i,j),
     +                cs1vt(i,j),ss1vt(i,j),cs2vt(i,j),ss2vt(i,j),
     +                cs1vl(i,j),ss1vl(i,j),cs2vl(i,j),ss2vl(i,j)
                 line = ''
                 exit
               endif
             enddo
             if ( line == '' ) EXIT
          endif
         enddo
         read(11,'(A)',iostat=ios) line
       enddo
C Order the records
       sorted = .false.
       do while (.not. sorted)
         sorted=.true.
C
         do i=1,nlat-1
           if (rlat(i) < rlat(i+1)) then
             hlp=rlat(i);rlat(i)=rlat(i+1);rlat(i+1)=hlp
             do j=1,nlon
               hlp=cs1dr(j,i);cs1dr(j,i)=cs1dr(j,i+1);cs1dr(j,i+1)=hlp
               hlp=ss1dr(j,i);ss1dr(j,i)=ss1dr(j,i+1);ss1dr(j,i+1)=hlp
               hlp=cs2dr(j,i);cs2dr(j,i)=cs2dr(j,i+1);cs2dr(j,i+1)=hlp
               hlp=ss2dr(j,i);ss2dr(j,i)=ss2dr(j,i+1);ss2dr(j,i+1)=hlp
               hlp=cs1vt(j,i);cs1vt(j,i)=cs1vt(j,i+1);cs1vt(j,i+1)=hlp
               hlp=ss1vt(j,i);ss1vt(j,i)=ss1vt(j,i+1);ss1vt(j,i+1)=hlp
               hlp=cs2vt(j,i);cs2vt(j,i)=cs2vt(j,i+1);cs2vt(j,i+1)=hlp
               hlp=ss2vt(j,i);ss2vt(j,i)=ss2vt(j,i+1);ss2vt(j,i+1)=hlp
               hlp=cs1vl(j,i);cs1vl(j,i)=cs1vl(j,i+1);cs1vl(j,i+1)=hlp
               hlp=ss1vl(j,i);ss1vl(j,i)=ss1vl(j,i+1);ss1vl(j,i+1)=hlp
               hlp=cs2vl(j,i);cs2vl(j,i)=cs2vl(j,i+1);cs2vl(j,i+1)=hlp
               hlp=ss2vl(j,i);ss2vl(j,i)=ss2vl(j,i+1);ss2vl(j,i+1)=hlp
             enddo
             sorted = .false.
           endif
         enddo
C
         do j=1,nlon-1
           if (rlon(j+1) < rlon(j)) then
             hlp=rlon(j);rlon(j)=rlon(j+1);rlon(j+1)=hlp
             do i=1,nlat
               hlp=cs1dr(j,i);cs1dr(j,i)=cs1dr(j+1,i);cs1dr(j+1,i)=hlp
               hlp=ss1dr(j,i);ss1dr(j,i)=ss1dr(j+1,i);ss1dr(j+1,i)=hlp
               hlp=cs2dr(j,i);cs2dr(j,i)=cs2dr(j+1,i);cs2dr(j+1,i)=hlp
               hlp=ss2dr(j,i);ss2dr(j,i)=ss2dr(j+1,i);ss2dr(j+1,i)=hlp
               hlp=cs1vt(j,i);cs1vt(j,i)=cs1vt(j+1,i);cs1vt(j+1,i)=hlp
               hlp=ss1vt(j,i);ss1vt(j,i)=ss1vt(j+1,i);ss1vt(j+1,i)=hlp
               hlp=cs2vt(j,i);cs2vt(j,i)=cs2vt(j+1,i);cs2vt(j+1,i)=hlp
               hlp=ss2vt(j,i);ss2vt(j,i)=ss2vt(j+1,i);ss2vt(j+1,i)=hlp
               hlp=cs1vl(j,i);cs1vl(j,i)=cs1vl(j+1,i);cs1vl(j+1,i)=hlp
               hlp=ss1vl(j,i);ss1vl(j,i)=ss1vl(j+1,i);ss1vl(j+1,i)=hlp
               hlp=cs2vl(j,i);cs2vl(j,i)=cs2vl(j+1,i);cs2vl(j+1,i)=hlp
               hlp=ss2vl(j,i);ss2vl(j,i)=ss2vl(j+1,i);ss2vl(j+1,i)=hlp
             enddo
             sorted = .false.
           endif
         enddo
       enddo
C Conver to colatitude and copy the 0-meridian to 360
       do i=1,nlat
         if (rlat(nlat) < 0.) rlat(i)=90.0-rlat(i)
C
         do j=2,mxlon
           if (rlon(1)+360. == rlon(j) .OR. rlon(j)==1.e30) THEN
             if (rlon(j)==1.e30) then
               rlon(j)=rlon(1)+360.
               nlon=nlon+1
             endif
             cs1dr(j,i)=cs1dr(1,i)
             ss1dr(j,i)=ss1dr(1,i)
             cs2dr(j,i)=cs2dr(1,i)
             ss2dr(j,i)=ss2dr(1,i)
             cs1vt(j,i)=cs1vt(1,i)
             ss1vt(j,i)=ss1vt(1,i)
             cs2vt(j,i)=cs2vt(1,i)
             ss2vt(j,i)=ss2vt(1,i)
             cs1vl(j,i)=cs1vl(1,i)
             ss1vl(j,i)=ss1vl(1,i)
             cs2vl(j,i)=cs2vl(1,i)
             ss2vl(j,i)=ss2vl(1,i)
           endif
         enddo
       enddo
C
C Start the original source code here
C -----------------------------------
       call splie2(rlon,rlat,ss1dr,nlon,nlat,ss1dri)
       call splie2(rlon,rlat,cs1dr,nlon,nlat,cs1dri)
       call splie2(rlon,rlat,ss2dr,nlon,nlat,ss2dri)
       call splie2(rlon,rlat,cs2dr,nlon,nlat,cs2dri)
       call splie2(rlon,rlat,ss1vt,nlon,nlat,ss1vti)
       call splie2(rlon,rlat,cs1vt,nlon,nlat,cs1vti)
       call splie2(rlon,rlat,ss2vt,nlon,nlat,ss2vti)
       call splie2(rlon,rlat,cs2vt,nlon,nlat,cs2vti)
       call splie2(rlon,rlat,ss1vl,nlon,nlat,ss1vli)
       call splie2(rlon,rlat,cs1vl,nlon,nlat,cs1vli)
       call splie2(rlon,rlat,ss2vl,nlon,nlat,ss2vli)
       call splie2(rlon,rlat,cs2vl,nlon,nlat,cs2vli)
C
C From Bernese we have the list of stations already in the memory
C bsw       do ista=1,50000
C bsw         read(12,*,end=100) sta,rln,rlt
       do ista=1,nStat
         sta = stName(ista)(1:4)
         CALL xyzell(AELL,BELL,DXELL,DRELL,SCELL,XSTAT(1:3,ista),XSTELL)
         rln = REAL(xstell(2)*rho)
         rlt = REAL(xstell(1)*rho)
c        convert latitude to co-latitude
         corlt=90.-rlt
c        tf necessary, convert longitude to east longitude
         erln=rln
         if(rln .lt. rlon(1)) erln=rln+360.
         call splin2(rlon,rlat,cs1dr,cs1dri,nlon,nlat,erln,corlt
     +      ,utot(1))
         call splin2(rlon,rlat,ss1dr,ss1dri,nlon,nlat,erln,corlt
     +      ,utot(2))
         call splin2(rlon,rlat,cs2dr,cs2dri,nlon,nlat,erln,corlt
     +      ,utot(3))
         call splin2(rlon,rlat,ss2dr,ss2dri,nlon,nlat,erln,corlt
     +      ,utot(4))
         call splin2(rlon,rlat,cs1vt,cs1vti,nlon,nlat,erln,corlt
     +      ,vttot(1))
         call splin2(rlon,rlat,ss1vt,ss1vti,nlon,nlat,erln,corlt
     +      ,vttot(2))
         call splin2(rlon,rlat,cs2vt,cs2vti,nlon,nlat,erln,corlt
     +      ,vttot(3))
         call splin2(rlon,rlat,ss2vt,ss2vti,nlon,nlat,erln,corlt
     +      ,vttot(4))
         call splin2(rlon,rlat,cs1vl,cs1vli,nlon,nlat,erln,corlt
     +      ,vltot(1))
         call splin2(rlon,rlat,ss1vl,ss1vli,nlon,nlat,erln,corlt
     +      ,vltot(2))
         call splin2(rlon,rlat,cs2vl,cs2vli,nlon,nlat,erln,corlt
     +      ,vltot(3))
         call splin2(rlon,rlat,ss2vl,ss2vli,nlon,nlat,erln,corlt
     +      ,vltot(4))
         if(iout .eq. 1) then
           write(21,'(A)') '$$ '
           write(21,'(3A,2F12.6)')
     1          '$$ station ',sta,'; coord.(long,lat)',rln,rlt
           write(21,'(A)') '$$ '
C BSW
C    add the full station name
C BSW
           write (21,'(A)') TRIM(stName(ista))
C BSW
           write (21,1000) (utot(i),i=1,4)
           write (21,1000) (vttot(i),i=1,4)
           write (21,1000) (vltot(i),i=1,4)
          else
           uamp1=sqrt(utot(1)**2+utot(2)**2)
           uamp2=sqrt(utot(3)**2+utot(4)**2)
           vtamp1=sqrt(vttot(1)**2+vttot(2)**2)
           vtamp2=sqrt(vttot(3)**2+vttot(4)**2)
           vlamp1=sqrt(vltot(1)**2+vltot(2)**2)
           vlamp2=sqrt(vltot(3)**2+vltot(4)**2)
           upha1=atan2(utot(2),utot(1))/dtr
           upha2=atan2(utot(4),utot(3))/dtr
           if(upha1 .lt. 0) upha1=upha1+360.
           if(upha2 .lt. 0) upha2=upha2+360.
           vtpha1=atan2(vttot(2),vttot(1))/dtr
           vtpha2=atan2(vttot(4),vttot(3))/dtr
           if(vtpha1 .lt. 0) vtpha1=vtpha1+360.
           if(vtpha2 .lt. 0) vtpha2=vtpha2+360.
           vlpha1=atan2(vltot(2),vltot(1))/dtr
           vlpha2=atan2(vltot(4),vltot(3))/dtr
           if(vlpha1 .lt. 0) vlpha1=vlpha1+360.
           if(vlpha2 .lt. 0) vlpha2=vlpha2+360.
           write(21,'(A)') '$$ '
           write(21,'(3A,2F12.6)')
     1           '$$ station ',sta,'; coord.(long,lat)',rln,rlt
           write(21,'(A)') '$$ '
           write(21,2000) uamp1,uamp2,vtamp1,vtamp2,vlamp1,vlamp2
           write(21,2000) upha1,upha2,vtpha1,vtpha2,vlpha1,vlpha2
          end if
1000      format(12f12.4)
2000      format(6f12.4)
        end do
100     continue
        write(21,*)
        close(unit=21)
        call exitrc(0)
        stop
        end
      SUBROUTINE splin2(x1a,x2a,ya,y2a,m,n,x1,x2,y)
      INTEGER m,n,NN
      REAL x1,x2,y,x1a(m),x2a(n),y2a(m,n),ya(m,n)
      PARAMETER (NN=500)
CU    USES spline,splint
      INTEGER j,k
      REAL y2tmp(NN),ytmp(NN),yytmp(NN)
      do 12 j=1,m
        do 11 k=1,n
          ytmp(k)=ya(j,k)
          y2tmp(k)=y2a(j,k)
11      continue
        call splint(x2a,ytmp,y2tmp,n,x2,yytmp(j))
12    continue
      call spline(x1a,yytmp,m,1.e30,1.e30,y2tmp)
      call splint(x1a,yytmp,y2tmp,m,x1,y)
      return
      END
      SUBROUTINE splie2(x1a,x2a,ya,m,n,y2a)
      INTEGER m,n,NN
      REAL x1a(m),x2a(n),y2a(m,n),ya(m,n)
      PARAMETER (NN=500)
CU    USES spline
      INTEGER j,k
      REAL y2tmp(NN),ytmp(NN)
      do 13 j=1,m
        do 11 k=1,n
          ytmp(k)=ya(j,k)
11      continue
        call spline(x2a,ytmp,n,1.e30,1.e30,y2tmp)
        do 12 k=1,n
          y2a(j,k)=y2tmp(k)
12      continue
13    continue
      return
      END
      SUBROUTINE splint(xa,ya,y2a,n,x,y)
      INTEGER n
      REAL x,y,xa(n),y2a(n),ya(n)
      INTEGER k,khi,klo
      REAL a,b,h
      klo=1
      khi=n
1     if (khi-klo.gt.1) then
        k=(khi+klo)/2
        if(xa(k).gt.x)then
          khi=k
        else
          klo=k
        endif
      goto 1
      endif
      h=xa(khi)-xa(klo)
      if (h.eq.0.) pause 'bad xa input in splint'
      a=(xa(khi)-x)/h
      b=(x-xa(klo))/h
      y=a*ya(klo)+b*ya(khi)+((a**3-a)*y2a(klo)+(b**3-b)*y2a(khi))*(h**
     *2)/6.
      return
      END
      SUBROUTINE spline(x,y,n,yp1,ypn,y2)
      INTEGER n,NMAX
      REAL yp1,ypn,x(n),y(n),y2(n)
      PARAMETER (NMAX=500)
      INTEGER i,k
      REAL p,qn,sig,un,u(NMAX)
      if (yp1.gt..99e30) then
        y2(1)=0.
        u(1)=0.
      else
        y2(1)=-0.5
        u(1)=(3./(x(2)-x(1)))*((y(2)-y(1))/(x(2)-x(1))-yp1)
      endif
      do 11 i=2,n-1
        sig=(x(i)-x(i-1))/(x(i+1)-x(i-1))
        p=sig*y2(i-1)+2.
        y2(i)=(sig-1.)/p
        u(i)=(6.*((y(i+1)-y(i))/(x(i+
     *1)-x(i))-(y(i)-y(i-1))/(x(i)-x(i-1)))/(x(i+1)-x(i-1))-sig*
     *u(i-1))/p
11    continue
      if (ypn.gt..99e30) then
        qn=0.
        un=0.
      else
        qn=0.5
        un=(3./(x(n)-x(n-1)))*(ypn-(y(n)-y(n-1))/(x(n)-x(n-1)))
      endif
      y2(n)=(un-qn*u(n-1))/(qn*y2(n-1)+1.)
      do 12 k=n-1,1,-1
        y2(k)=y2(k)*y2(k+1)+u(k)
12    continue
      return
      END
