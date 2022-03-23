      MODULE s_harload

      PRIVATE
      PUBLIC :: harload

      CONTAINS
C*
      SUBROUTINE harload(xmjd,ocnamp,ocnphs,doload)
CC
CC PURPOSE    :  COMPUTES EFFECT OF OCEAN LOADING ON STATION COORDINATES
CC               ACCORDING TO IERS CONVENTIONS 1996
CC               THE FOLLOWING TERMS ARE USED:
CC                  M2, S2, N2, K2, K1, O1, P1, Q1, MF, MM, SSA
CC                  (values from SCHERNECK)
CC
CC PARAMETERS :
CC         IN :  TOBS: OBSERVATION EPOCH (MJD)                R*8
CC               OCNAMP: OCEAN LOADING AMPLITUDES             R*8(3,11)
CC               OCNPHS: OCEAN LOADING AMPLITUDES             R*8(3,11)
CC         OUT:  DOLOAD: OCEAN LAODING CORRECTIONS (-N,-E,U)  R*8
CC
CC REMARK     : Derived from hardisp.f from Duncan Agnew, August 2005,
CC              based on the program hartid distributed with the SPOTL
CC              loading package. Distributed by IERS.
CC
CC              Computes tidal displacments, using an expanded set of tidal
CC              constituents, whose amplitudes and phases are found by spline
CC              interpolation of the tidal admittance. A total of 141
CC              constituent tides are included, which gives a precision of
CC              about 1%
CC
CC  ADAPTED: 01-MAR-06 HU
CC
CC  CHANGES: 25-MAR-09 : HB: ADAPTED FROM NEW VERSION OF HARDISP.F (JUN-2008)
CC                           - MORE TIDAL CONSTITUENTS
CC                           - PHASE CORRECTED
CC                           A TOTAL OF 342 CONSTITUENT TIDES ARE INCLUDED,
CC                           WHICH GIVES A PRECISION OF ABOUT 0.1%
CC           19-JUL-10 : SL: TAB CHARACTERS REMOVED
CC
C*
      USE m_bern
      USE m_maxdim, only: maxocn
      IMPLICIT NONE

      INTEGER(i4b),PARAMETER        :: ntin = 11
      INTEGER(i4b),PARAMETER        :: ntout=342
      REAL(r8b)                     :: xmjd
      REAL(r8b),DIMENSION(3,maxocn) :: ocnamp,ocnphs
      REAL(r8b),DIMENSION(3)        :: doload

      INTEGER(i4b)                  :: ii,nout
      INTEGER(i4b),DIMENSION(6,ntin):: idt
      REAL(r8b),DIMENSION(ntout)    :: pz,pw,ps
      REAL(r8b),DIMENSION(ntout)    :: az,aw,as
      REAL(r8b),DIMENSION(ntout)    :: freq
      REAL(r8b)                     :: rho

      DATA rho/.01745329252d0/

c  Cartwright-Tayler numbers of tides used in Scherneck lists:
c      M2, S2, N2, K2, K1, O1, P1, Q1, Mf, Mm, Ssa
      data idt/
     1  2, 0, 0, 0, 0, 0,   2, 2,-2, 0, 0, 0,   2,-1, 0, 1, 0, 0,
     2  2, 2, 0, 0, 0, 0,   1, 1, 0, 0, 0, 0,   1,-1, 0, 0, 0, 0,
     3  1, 1,-2, 0, 0, 0,   1,-2, 0, 1, 0, 0,   0, 2, 0, 0, 0, 0,
     4  0, 1, 0,-1, 0, 0,   0, 0, 2, 0, 0, 0/
c*******************************************************************
c
c  find amplitudes and phases for all constituents, for each of the
c    three displacements. Note that the same frequencies are returned each
c    time
c  BLQ format order is vertical, horizontal EW, horizontal NS
c
c*******************************************************************
      call admint(xmjd,ocnamp(1,:),idt,-ocnphs(1,:),as,freq,ps,ntin,
     1                                                         nout)
      call admint(xmjd,ocnamp(2,:),idt,-ocnphs(2,:),aw,freq,pw,ntin,
     1                                                         nout)
      call admint(xmjd,ocnamp(3,:),idt,-ocnphs(3,:),az,freq,pz,ntin,
     1                                                         nout)
c
      do ii=1,nout
        ps(ii) = rho*ps(ii)
        pw(ii) = rho*pw(ii)
        pz(ii) = rho*pz(ii)
      enddo
c
      doload=0d0
      do ii=1,nout
        doload(1)=doload(1)+as(ii)*dcos(ps(ii))
        doload(2)=doload(2)+aw(ii)*dcos(pw(ii))
        doload(3)=doload(3)+az(ii)*dcos(pz(ii))
      enddo
      end subroutine
c ---------------------------------------------------------------------
      SUBROUTINE admint(xmjd,ampin,idtin,phin,amp,f,p,nin,nout)
c  Returns the amplitude, frequency, and phase of a set of tidal
c  constituents. n is input as the number wanted, and returned as the number
c  provided.  The constituents used are stored in the arrays idd (doodson
c  number) and tamp (Cartwright-Edden amplitude).  The actual amp and
c  phase of each of these are determined by spline interpolation of the
c  real and imaginary part of the admittance, as specified at a subset
c  of the constituents.
c  The phase is determined for a time set in common block /date/ (see
c  subroutine tdfrph), outside of this subroutine.
c   Note that the arrays f and p must be specified as double precision.
c
      USE m_bern
      USE m_maxdim, ONLY: maxocn
      IMPLICIT NONE
      INTEGER(i4b),PARAMETER         :: ntin = 11
      INTEGER(i4b),PARAMETER         :: ntout=342
      REAL(r8b)                      :: xmjd
      REAL(r8b),DIMENSION(maxocn)    :: ampin,phin
      REAL(r8b),DIMENSION(ntout)     :: amp,p,f
!!      REAL(r8b)                      :: eval
      INTEGER(i4b),DIMENSION(6,ntin) :: idtin

      REAL(r8b)                      :: fr,pr,dtr,sf,re,am
      INTEGER(i4b)                   :: nt,ncon,nlp,ndi,nsd,ntd,ll
      INTEGER(i4b)                   :: nin,kk,ii,k,j,i,nout
      INTEGER(i4b),DIMENSION(6,ntout):: idd
c  arrays containing information about all stored constituents
      REAL(r8b),DIMENSION(ntout)     :: tamp
c  arrays containing information about the subset whose amp and phase may
c  be specified, and scratch arrays for the spline routines
c   at most 20 constituents may be specified.
      REAL(r8b),DIMENSION(20)        :: rl,aim,rf,scr
      REAL(r8b),DIMENSION(20)        :: zdi,zdr,di,dr
      REAL(r8b),DIMENSION(20)        :: sdi,sdr,tdi,tdr
      INTEGER(i4b),DIMENSION(20)     :: key
      data dtr/.017453293D0/
      data nt/342/,ncon/20/
      data rl/20*0D0/,aim/20*0D0/,rf/20*0D0/
      data zdi/20*0D0/,zdr/20*0D0/,di/20*0D0/,dr/20*0D0/
      data sdi/20*0D0/,sdr/20*0D0/,tdi/20*0D0/,tdr/20*0D0/
      data tamp/
     1 .632208D0, .294107D0, .121046D0, .079915D0, .023818D0,-.023589D0,
     1 .022994D0, .019333D0,-.017871D0, .017192D0, .016018D0, .004671D0,
     1-.004662D0,-.004519D0, .004470D0, .004467D0, .002589D0,-.002455D0,
     1-.002172D0, .001972D0, .001947D0, .001914D0,-.001898D0, .001802D0,
     1 .001304D0, .001170D0, .001130D0, .001061D0,-.001022D0,-.001017D0,
     1 .001014D0, .000901D0,-.000857D0, .000855D0, .000855D0, .000772D0,
     1 .000741D0, .000741D0,-.000721D0, .000698D0, .000658D0, .000654D0,
     1-.000653D0, .000633D0, .000626D0,-.000598D0, .000590D0, .000544D0,
     1 .000479D0,-.000464D0, .000413D0,-.000390D0, .000373D0, .000366D0,
     1 .000366D0,-.000360D0,-.000355D0, .000354D0, .000329D0, .000328D0,
     1 .000319D0, .000302D0, .000279D0,-.000274D0,-.000272D0, .000248D0,
     1-.000225D0, .000224D0,-.000223D0,-.000216D0, .000211D0, .000209D0,
     1 .000194D0, .000185D0,-.000174D0,-.000171D0, .000159D0, .000131D0,
     1 .000127D0, .000120D0, .000118D0, .000117D0, .000108D0, .000107D0,
     1 .000105D0,-.000102D0, .000102D0, .000099D0,-.000096D0, .000095D0,
     1-.000089D0,-.000085D0,-.000084D0,-.000081D0,-.000077D0,-.000072D0,
     1-.000067D0, .000066D0, .000064D0, .000063D0, .000063D0, .000063D0,
     1 .000062D0, .000062D0,-.000060D0, .000056D0, .000053D0, .000051D0,
     1 .000050D0, .368645D0,-.262232D0,-.121995D0,-.050208D0, .050031D0,
     1-.049470D0, .020620D0, .020613D0, .011279D0,-.009530D0,-.009469D0,
     1-.008012D0, .007414D0,-.007300D0, .007227D0,-.007131D0,-.006644D0,
     1 .005249D0, .004137D0, .004087D0, .003944D0, .003943D0, .003420D0,
     1 .003418D0, .002885D0, .002884D0, .002160D0,-.001936D0, .001934D0,
     1-.001798D0, .001690D0, .001689D0, .001516D0, .001514D0,-.001511D0,
     1 .001383D0, .001372D0, .001371D0,-.001253D0,-.001075D0, .001020D0,
     1 .000901D0, .000865D0,-.000794D0, .000788D0, .000782D0,-.000747D0,
     1-.000745D0, .000670D0,-.000603D0,-.000597D0, .000542D0, .000542D0,
     1-.000541D0,-.000469D0,-.000440D0, .000438D0, .000422D0, .000410D0,
     1-.000374D0,-.000365D0, .000345D0, .000335D0,-.000321D0,-.000319D0,
     1 .000307D0, .000291D0, .000290D0,-.000289D0, .000286D0, .000275D0,
     1 .000271D0, .000263D0,-.000245D0, .000225D0, .000225D0, .000221D0,
     1-.000202D0,-.000200D0,-.000199D0, .000192D0, .000183D0, .000183D0,
     1 .000183D0,-.000170D0, .000169D0, .000168D0, .000162D0, .000149D0,
     1-.000147D0,-.000141D0, .000138D0, .000136D0, .000136D0, .000127D0,
     1 .000127D0,-.000126D0,-.000121D0,-.000121D0, .000117D0,-.000116D0,
     1-.000114D0,-.000114D0,-.000114D0, .000114D0, .000113D0, .000109D0,
     1 .000108D0, .000106D0,-.000106D0,-.000106D0, .000105D0, .000104D0,
     1-.000103D0,-.000100D0,-.000100D0,-.000100D0, .000099D0,-.000098D0,
     1 .000093D0, .000093D0, .000090D0,-.000088D0, .000083D0,-.000083D0,
     1-.000082D0,-.000081D0,-.000079D0,-.000077D0,-.000075D0,-.000075D0,
     1-.000075D0, .000071D0, .000071D0,-.000071D0, .000068D0, .000068D0,
     1 .000065D0, .000065D0, .000064D0, .000064D0, .000064D0,-.000064D0,
     1-.000060D0, .000056D0, .000056D0, .000053D0, .000053D0, .000053D0,
     1-.000053D0, .000053D0, .000053D0, .000052D0, .000050D0,-.066607D0,
     1-.035184D0,-.030988D0, .027929D0,-.027616D0,-.012753D0,-.006728D0,
     1-.005837D0,-.005286D0,-.004921D0,-.002884D0,-.002583D0,-.002422D0,
     1 .002310D0, .002283D0,-.002037D0, .001883D0,-.001811D0,-.001687D0,
     1-.001004D0,-.000925D0,-.000844D0, .000766D0, .000766D0,-.000700D0,
     1-.000495D0,-.000492D0, .000491D0, .000483D0, .000437D0,-.000416D0,
     1-.000384D0, .000374D0,-.000312D0,-.000288D0,-.000273D0, .000259D0,
     1 .000245D0,-.000232D0, .000229D0,-.000216D0, .000206D0,-.000204D0,
     1-.000202D0, .000200D0, .000195D0,-.000190D0, .000187D0, .000180D0,
     1-.000179D0, .000170D0, .000153D0,-.000137D0,-.000119D0,-.000119D0,
     1-.000112D0,-.000110D0,-.000110D0, .000107D0,-.000095D0,-.000095D0,
     1-.000091D0,-.000090D0,-.000081D0,-.000079D0,-.000079D0, .000077D0,
     1-.000073D0, .000069D0,-.000067D0,-.000066D0, .000065D0, .000064D0,
     1-.000062D0, .000060D0, .000059D0,-.000056D0, .000055D0,-.000051D0/
      data idd/
     1  2, 0, 0, 0, 0, 0,   2, 2,-2, 0, 0, 0,   2,-1, 0, 1, 0, 0,
     2  2, 2, 0, 0, 0, 0,   2, 2, 0, 0, 1, 0,   2, 0, 0, 0,-1, 0,
     3  2,-1, 2,-1, 0, 0,   2,-2, 2, 0, 0, 0,   2, 1, 0,-1, 0, 0,
     4  2, 2,-3, 0, 0, 1,   2,-2, 0, 2, 0, 0,   2,-3, 2, 1, 0, 0,
     5  2, 1,-2, 1, 0, 0,   2,-1, 0, 1,-1, 0,   2, 3, 0,-1, 0, 0,
     6  2, 1, 0, 1, 0, 0,   2, 2, 0, 0, 2, 0,   2, 2,-1, 0, 0,-1,
     7  2, 0,-1, 0, 0, 1,   2, 1, 0, 1, 1, 0,   2, 3, 0,-1, 1, 0,
     8  2, 0, 1, 0, 0,-1,   2, 0,-2, 2, 0, 0,   2,-3, 0, 3, 0, 0,
     9  2,-2, 3, 0, 0,-1,   2, 4, 0, 0, 0, 0,   2,-1, 1, 1, 0,-1,
     1  2,-1, 3,-1, 0,-1,   2, 2, 0, 0,-1, 0,   2,-1,-1, 1, 0, 1,
     2  2, 4, 0, 0, 1, 0,   2,-3, 4,-1, 0, 0,   2,-1, 2,-1,-1, 0,
     3  2, 3,-2, 1, 0, 0,   2, 1, 2,-1, 0, 0,   2,-4, 2, 2, 0, 0,
     4  2, 4,-2, 0, 0, 0,   2, 0, 2, 0, 0, 0,   2,-2, 2, 0,-1, 0,
     5  2, 2,-4, 0, 0, 2,   2, 2,-2, 0,-1, 0,   2, 1, 0,-1,-1, 0,
     6  2,-1, 1, 0, 0, 0,   2, 2,-1, 0, 0, 1,   2, 2, 1, 0, 0,-1,
     7  2,-2, 0, 2,-1, 0,   2,-2, 4,-2, 0, 0,   2, 2, 2, 0, 0, 0,
     8  2,-4, 4, 0, 0, 0,   2,-1, 0,-1,-2, 0,   2, 1, 2,-1, 1, 0,
     9  2,-1,-2, 3, 0, 0,   2, 3,-2, 1, 1, 0,   2, 4, 0,-2, 0, 0,
     1  2, 0, 0, 2, 0, 0,   2, 0, 2,-2, 0, 0,   2, 0, 2, 0, 1, 0,
     2  2,-3, 3, 1, 0,-1,   2, 0, 0, 0,-2, 0,   2, 4, 0, 0, 2, 0,
     3  2, 4,-2, 0, 1, 0,   2, 0, 0, 0, 0, 2,   2, 1, 0, 1, 2, 0,
     4  2, 0,-2, 0,-2, 0,   2,-2, 1, 0, 0, 1,   2,-2, 1, 2, 0,-1,
     5  2,-1, 1,-1, 0, 1,   2, 5, 0,-1, 0, 0,   2, 1,-3, 1, 0, 1,
     6  2,-2,-1, 2, 0, 1,   2, 3, 0,-1, 2, 0,   2, 1,-2, 1,-1, 0,
     7  2, 5, 0,-1, 1, 0,   2,-4, 0, 4, 0, 0,   2,-3, 2, 1,-1, 0,
     8  2,-2, 1, 1, 0, 0,   2, 4, 0,-2, 1, 0,   2, 0, 0, 2, 1, 0,
     9  2,-5, 4, 1, 0, 0,   2, 0, 2, 0, 2, 0,   2,-1, 2, 1, 0, 0,
     1  2, 5,-2,-1, 0, 0,   2, 1,-1, 0, 0, 0,   2, 2,-2, 0, 0, 2,
     2  2,-5, 2, 3, 0, 0,   2,-1,-2, 1,-2, 0,   2,-3, 5,-1, 0,-1,
     3  2,-1, 0, 0, 0, 1,   2,-2, 0, 0,-2, 0,   2, 0,-1, 1, 0, 0,
     4  2,-3, 1, 1, 0, 1,   2, 3, 0,-1,-1, 0,   2, 1, 0, 1,-1, 0,
     5  2,-1, 2, 1, 1, 0,   2, 0,-3, 2, 0, 1,   2, 1,-1,-1, 0, 1,
     6  2,-3, 0, 3,-1, 0,   2, 0,-2, 2,-1, 0,   2,-4, 3, 2, 0,-1,
     7  2,-1, 0, 1,-2, 0,   2, 5, 0,-1, 2, 0,   2,-4, 5, 0, 0,-1,
     8  2,-2, 4, 0, 0,-2,   2,-1, 0, 1, 0, 2,   2,-2,-2, 4, 0, 0,
     9  2, 3,-2,-1,-1, 0,   2,-2, 5,-2, 0,-1,   2, 0,-1, 0,-1, 1,
     1  2, 5,-2,-1, 1, 0,   1, 1, 0, 0, 0, 0,   1,-1, 0, 0, 0, 0,
     2  1, 1,-2, 0, 0, 0,   1,-2, 0, 1, 0, 0,   1, 1, 0, 0, 1, 0,
     3  1,-1, 0, 0,-1, 0,   1, 2, 0,-1, 0, 0,   1, 0, 0, 1, 0, 0,
     4  1, 3, 0, 0, 0, 0,   1,-2, 2,-1, 0, 0,   1,-2, 0, 1,-1, 0,
     5  1,-3, 2, 0, 0, 0,   1, 0, 0,-1, 0, 0,   1, 1, 0, 0,-1, 0,
     6  1, 3, 0, 0, 1, 0,   1, 1,-3, 0, 0, 1,   1,-3, 0, 2, 0, 0,
     7  1, 1, 2, 0, 0, 0,   1, 0, 0, 1, 1, 0,   1, 2, 0,-1, 1, 0,
     8  1, 0, 2,-1, 0, 0,   1, 2,-2, 1, 0, 0,   1, 3,-2, 0, 0, 0,
     9  1,-1, 2, 0, 0, 0,   1, 1, 1, 0, 0,-1,   1, 1,-1, 0, 0, 1,
     1  1, 4, 0,-1, 0, 0,   1,-4, 2, 1, 0, 0,   1, 0,-2, 1, 0, 0,
     2  1,-2, 2,-1,-1, 0,   1, 3, 0,-2, 0, 0,   1,-1, 0, 2, 0, 0,
     3  1,-1, 0, 0,-2, 0,   1, 3, 0, 0, 2, 0,   1,-3, 2, 0,-1, 0,
     4  1, 4, 0,-1, 1, 0,   1, 0, 0,-1,-1, 0,   1, 1,-2, 0,-1, 0,
     5  1,-3, 0, 2,-1, 0,   1, 1, 0, 0, 2, 0,   1, 1,-1, 0, 0,-1,
     6  1,-1,-1, 0, 0, 1,   1, 0, 2,-1, 1, 0,   1,-1, 1, 0, 0,-1,
     7  1,-1,-2, 2, 0, 0,   1, 2,-2, 1, 1, 0,   1,-4, 0, 3, 0, 0,
     8  1,-1, 2, 0, 1, 0,   1, 3,-2, 0, 1, 0,   1, 2, 0,-1,-1, 0,
     9  1, 0, 0, 1,-1, 0,   1,-2, 2, 1, 0, 0,   1, 4,-2,-1, 0, 0,
     1  1,-3, 3, 0, 0,-1,   1,-2, 1, 1, 0,-1,   1,-2, 3,-1, 0,-1,
     2  1, 0,-2, 1,-1, 0,   1,-2,-1, 1, 0, 1,   1, 4,-2, 1, 0, 0,
     3  1,-4, 4,-1, 0, 0,   1,-4, 2, 1,-1, 0,   1, 5,-2, 0, 0, 0,
     4  1, 3, 0,-2, 1, 0,   1,-5, 2, 2, 0, 0,   1, 2, 0, 1, 0, 0,
     5  1, 1, 3, 0, 0,-1,   1,-2, 0, 1,-2, 0,   1, 4, 0,-1, 2, 0,
     6  1, 1,-4, 0, 0, 2,   1, 5, 0,-2, 0, 0,   1,-1, 0, 2, 1, 0,
     7  1,-2, 1, 0, 0, 0,   1, 4,-2, 1, 1, 0,   1,-3, 4,-2, 0, 0,
     8  1,-1, 3, 0, 0,-1,   1, 3,-3, 0, 0, 1,   1, 5,-2, 0, 1, 0,
     9  1, 1, 2, 0, 1, 0,   1, 2, 0, 1, 1, 0,   1,-5, 4, 0, 0, 0,
     1  1,-2, 0,-1,-2, 0,   1, 5, 0,-2, 1, 0,   1, 1, 2,-2, 0, 0,
     2  1, 1,-2, 2, 0, 0,   1,-2, 2, 1, 1, 0,   1, 0, 3,-1, 0,-1,
     3  1, 2,-3, 1, 0, 1,   1,-2,-2, 3, 0, 0,   1,-1, 2,-2, 0, 0,
     4  1,-4, 3, 1, 0,-1,   1,-4, 0, 3,-1, 0,   1,-1,-2, 2,-1, 0,
     5  1,-2, 0, 3, 0, 0,   1, 4, 0,-3, 0, 0,   1, 0, 1, 1, 0,-1,
     6  1, 2,-1,-1, 0, 1,   1, 2,-2, 1,-1, 0,   1, 0, 0,-1,-2, 0,
     7  1, 2, 0, 1, 2, 0,   1, 2,-2,-1,-1, 0,   1, 0, 0, 1, 2, 0,
     8  1, 0, 1, 0, 0, 0,   1, 2,-1, 0, 0, 0,   1, 0, 2,-1,-1, 0,
     9  1,-1,-2, 0,-2, 0,   1,-3, 1, 0, 0, 1,   1, 3,-2, 0,-1, 0,
     1  1,-1,-1, 0,-1, 1,   1, 4,-2,-1, 1, 0,   1, 2, 1,-1, 0,-1,
     2  1, 0,-1, 1, 0, 1,   1,-2, 4,-1, 0, 0,   1, 4,-4, 1, 0, 0,
     3  1,-3, 1, 2, 0,-1,   1,-3, 3, 0,-1,-1,   1, 1, 2, 0, 2, 0,
     4  1, 1,-2, 0,-2, 0,   1, 3, 0, 0, 3, 0,   1,-1, 2, 0,-1, 0,
     5  1,-2, 1,-1, 0, 1,   1, 0,-3, 1, 0, 1,   1,-3,-1, 2, 0, 1,
     6  1, 2, 0,-1, 2, 0,   1, 6,-2,-1, 0, 0,   1, 2, 2,-1, 0, 0,
     7  1,-1, 1, 0,-1,-1,   1,-2, 3,-1,-1,-1,   1,-1, 0, 0, 0, 2,
     8  1,-5, 0, 4, 0, 0,   1, 1, 0, 0, 0,-2,   1,-2, 1, 1,-1,-1,
     9  1, 1,-1, 0, 1, 1,   1, 1, 2, 0, 0,-2,   1,-3, 1, 1, 0, 0,
     1  1,-4, 4,-1,-1, 0,   1, 1, 0,-2,-1, 0,   1,-2,-1, 1,-1, 1,
     2  1,-3, 2, 2, 0, 0,   1, 5,-2,-2, 0, 0,   1, 3,-4, 2, 0, 0,
     3  1, 1,-2, 0, 0, 2,   1,-1, 4,-2, 0, 0,   1, 2, 2,-1, 1, 0,
     4  1,-5, 2, 2,-1, 0,   1, 1,-3, 0,-1, 1,   1, 1, 1, 0, 1,-1,
     5  1, 6,-2,-1, 1, 0,   1,-2, 2,-1,-2, 0,   1, 4,-2, 1, 2, 0,
     6  1,-6, 4, 1, 0, 0,   1, 5,-4, 0, 0, 0,   1,-3, 4, 0, 0, 0,
     7  1, 1, 2,-2, 1, 0,   1,-2, 1, 0,-1, 0,   0, 2, 0, 0, 0, 0,
     8  0, 1, 0,-1, 0, 0,   0, 0, 2, 0, 0, 0,   0, 0, 0, 0, 1, 0,
     9  0, 2, 0, 0, 1, 0,   0, 3, 0,-1, 0, 0,   0, 1,-2, 1, 0, 0,
     1  0, 2,-2, 0, 0, 0,   0, 3, 0,-1, 1, 0,   0, 0, 1, 0, 0,-1,
     2  0, 2, 0,-2, 0, 0,   0, 2, 0, 0, 2, 0,   0, 3,-2, 1, 0, 0,
     3  0, 1, 0,-1,-1, 0,   0, 1, 0,-1, 1, 0,   0, 4,-2, 0, 0, 0,
     4  0, 1, 0, 1, 0, 0,   0, 0, 3, 0, 0,-1,   0, 4, 0,-2, 0, 0,
     5  0, 3,-2, 1, 1, 0,   0, 3,-2,-1, 0, 0,   0, 4,-2, 0, 1, 0,
     6  0, 0, 2, 0, 1, 0,   0, 1, 0, 1, 1, 0,   0, 4, 0,-2, 1, 0,
     7  0, 3, 0,-1, 2, 0,   0, 5,-2,-1, 0, 0,   0, 1, 2,-1, 0, 0,
     8  0, 1,-2, 1,-1, 0,   0, 1,-2, 1, 1, 0,   0, 2,-2, 0,-1, 0,
     9  0, 2,-3, 0, 0, 1,   0, 2,-2, 0, 1, 0,   0, 0, 2,-2, 0, 0,
     1  0, 1,-3, 1, 0, 1,   0, 0, 0, 0, 2, 0,   0, 0, 1, 0, 0, 1,
     2  0, 1, 2,-1, 1, 0,   0, 3, 0,-3, 0, 0,   0, 2, 1, 0, 0,-1,
     3  0, 1,-1,-1, 0, 1,   0, 1, 0, 1, 2, 0,   0, 5,-2,-1, 1, 0,
     4  0, 2,-1, 0, 0, 1,   0, 2, 2,-2, 0, 0,   0, 1,-1, 0, 0, 0,
     5  0, 5, 0,-3, 0, 0,   0, 2, 0,-2, 1, 0,   0, 1, 1,-1, 0,-1,
     6  0, 3,-4, 1, 0, 0,   0, 0, 2, 0, 2, 0,   0, 2, 0,-2,-1, 0,
     7  0, 4,-3, 0, 0, 1,   0, 3,-1,-1, 0, 1,   0, 0, 2, 0, 0,-2,
     8  0, 3,-3, 1, 0, 1,   0, 2,-4, 2, 0, 0,   0, 4,-2,-2, 0, 0,
     9  0, 3, 1,-1, 0,-1,   0, 5,-4, 1, 0, 0,   0, 3,-2,-1,-1, 0,
     1  0, 3,-2, 1, 2, 0,   0, 4,-4, 0, 0, 0,   0, 6,-2,-2, 0, 0,
     2  0, 5, 0,-3, 1, 0,   0, 4,-2, 0, 2, 0,   0, 2, 2,-2, 1, 0,
     3  0, 0, 4, 0, 0,-2,   0, 3,-1, 0, 0, 0,   0, 3,-3,-1, 0, 1,
     4  0, 4, 0,-2, 2, 0,   0, 1,-2,-1,-1, 0,   0, 2,-1, 0, 0,-1,
     5  0, 4,-4, 2, 0, 0,   0, 2, 1, 0, 1,-1,   0, 3,-2,-1, 1, 0,
     6  0, 4,-3, 0, 1, 1,   0, 2, 0, 0, 3, 0,   0, 6,-4, 0, 0, 0/
      k = 0
      nlp = 0
      ndi = 0
      nsd = 0
      ntd = 0
      do 16 ll=1,nin
c  see if Doodson numbers match
      do 14 kk=1,nt
      ii = 0
      do 13 i=1,6
 13   ii = ii + iabs(idd(i,kk)-idtin(i,ll))
      if(ii.eq.0) go to 15
 14   continue
c  have a match - put line into array
 15   if(ii.eq.0.and.k.lt.ncon) then
         k = k + 1
         rl(k) = ampin(ll)*cos(phin(ll))/abs(tamp(kk))
         aim(k)= ampin(ll)*sin(phin(ll))/abs(tamp(kk))
c  Now have real and imaginary parts of admittance, scaled by C-E
c  amplitude. Admittance phase is whatever was used in the original
c  expression. (Usually phase is given relative to some reference
c  but amplitude is in absolute units). Next get frequency.
         call tdfrph(xmjd,idd(1,kk),fr,pr)
         rf(k) = fr
      endif
 16   continue
c  done going through constituents--there are k of them
c  have specified admittance at a number of points. sort these by frequency
c  and separate diurnal and semidiurnal, recopying admittances to get them in
c  order
      call shells(rf,key,k)
      do 21 i=1,k
      if(rf(i).lt.0.5) nlp = nlp + 1
      if(rf(i).lt.1.5.and.rf(i).gt.0.5) ndi = ndi + 1
      if(rf(i).lt.2.5.and.rf(i).gt.1.5) nsd = nsd + 1
      if(rf(i).lt.3.5.and.rf(i).gt.2.5) ntd = ntd + 1
 21   scr(i) = rl(key(i))
      do 23 i=1,k
      rl(i) = scr(i)
 23   scr(i) = aim(key(i))
      do 25 i=1,k
 25   aim(i) = scr(i)
c  now set up splines (8 cases - four species, each real and imag)
c  we have to allow for the case when there are no constituent amplitudes
c  for the long-period or terdiurnal.
      if(nlp.ne.0) call spline3(nlp,rf,rl,zdr,scr)
      if(nlp.ne.0) call spline3(nlp,rf,aim,zdi,scr)
      call spline3(ndi,rf(nlp+1),rl(nlp+1),dr,scr)
      call spline3(ndi,rf(nlp+1),aim(nlp+1),di,scr)
      call spline3(nsd,rf(nlp+ndi+1),rl(nlp+ndi+1),sdr,scr)
      call spline3(nsd,rf(nlp+ndi+1),aim(nlp+ndi+1),sdi,scr)
c  evaluate all harmonics using the interpolated admittance
      j = 1
      do 31 i=1,nt
      if(idd(1,i).eq.0.and.nlp.eq.0)
     1  go to 31
      call tdfrph(xmjd,idd(1,i),f(j),p(j))
c  equilibrium phase corrections
      if(idd(1,i).eq.0) p(j) = p(j) + 180.D0
      if(idd(1,i).eq.1) p(j) = p(j) + 90.D0
      sf = f(j)

      if(idd(1,i).eq.0) re = eval(sf,nlp,rf,rl,zdr)
      if(idd(1,i).eq.0) am = eval(sf,nlp,rf,aim,zdi)
      if(idd(1,i).eq.1) re = eval(sf,ndi,rf(nlp+1),rl(nlp+1),dr)
      if(idd(1,i).eq.1) am = eval(sf,ndi,rf(nlp+1),aim(nlp+1),di)
      if(idd(1,i).eq.2) re =
     1   eval(sf,nsd,rf(nlp+ndi+1),rl(nlp+ndi+1),sdr)
      if(idd(1,i).eq.2) am =
     1   eval(sf,nsd,rf(nlp+ndi+1),aim(nlp+ndi+1),sdi)

      amp(j) = tamp(i)*sqrt(re**2+am**2)
      IF (am.ne.0d0.or.re.ne.0d0) p(j) = p(j) + atan2(am,re)/dtr
      if(p(j).gt.180) p(j)=p(j)-360.D0
       j = j + 1
 31   continue
      nout = j - 1
      return
      end subroutine
c ---------------------------------------------------------------------
      subroutine tdfrph(xmjd,idood,freq,phase)
c
c   Given the Doodson number of a tidal constituent (in idood), returns
c  the frequency and phase.  Phase is returned in degrees and frequency
c  in cycles/day.
c
c   Note that phases must be decreased by 90 degrees if the sum of the order
c  and the species number is odd (as for the 2nd degree diurnals, and 3rd
c  degree low-frequency and semidiurnals).
c   These phases may need further adjustment to allow for the spherical
c  harmonic normalization used; e.g. for that used for the potential by
c  Cartwright and Tayler, 180 degrees must be added for (species,order)
c  = (1,2), (1,3), or (3,3).
c
c  Common block date stores time information, in UT
c
c    calls toymd, leap, juldat, etutc (all for timekeeping)
c
      USE m_bern
      IMPLICIT NONE
      REAL(r8b)                   :: xmjd
      INTEGER(i4b),DIMENSION(6)   :: idood
      REAL(r8b)                   :: freq
      REAL(r8b)                   :: phase

      REAL(r8b),SAVE              :: xmjdsav=0D0
      REAL(r8b),DIMENSION(6),SAVE :: d,dd
      REAL(r8b)                   :: f1,f2,f3,f4,f5
      REAL(r8b)                   :: fd1,fd2,fd3,fd4,fd5
      REAL(r8b)                   :: dayfr,t
      INTEGER(i4b)                :: i
c
c  test to see if time has changed; if so, set the phases and frequencies
c   for each of the Doodson arguments
c
      if (xmjd.ne.xmjdsav) then
        xmjdsav=xmjd
c
c convert times to Julian days (UT) then to Julian centuries from J2000.00
c   (ET)
c
        t = (xmjd - 51544.5D0 + (19.D0+32.184D0)/86400.d0)/36525.d0
        dayfr=xmjd-INT(xmjd)
c
c IERS expressions for the Delauney arguments, in degrees
c
        f1 =     134.9634025100d0 +
     1    t*( 477198.8675605000d0 +
     1    t*(      0.0088553333d0 +
     1    t*(      0.0000143431d0 +
     1    t*(     -0.0000000680d0 ))))
        f2 =     357.5291091806d0 +
     1    t*(  35999.0502911389d0 +
     1    t*(     -0.0001536667d0 +
     1    t*(      0.0000000378d0 +
     1    t*(     -0.0000000032d0 ))))
        f3 =      93.2720906200d0 +
     1    t*( 483202.0174577222d0 +
     1    t*(     -0.0035420000d0 +
     1    t*(     -0.0000002881d0 +
     1    t*(      0.0000000012d0 ))))
        f4 =     297.8501954694d0 +
     1    t*( 445267.1114469445d0 +
     1    t*(     -0.0017696111d0 +
     1    t*(      0.0000018314d0 +
     1    t*(     -0.0000000088d0 ))))
        f5 =     125.0445550100d0 +
     1    t*(  -1934.1362619722d0 +
     1    t*(      0.0020756111d0 +
     1    t*(      0.0000021394d0 +
     1    t*(     -0.0000000165d0 ))))
c
c  convert to Doodson (Darwin) variables
c
        d(1) = 360.d0*dayfr - f4
        d(2) = f3 + f5
        d(3) = d(2) - f4
        d(4) = d(2) - f1
        d(5) = -f5
        d(6) = d(3) - f2
c
c   find frequencies of Delauney variables (in cycles/day), and from these
c    the same for the Doodson arguments
c
        fd1 =  0.0362916471 + 0.0000000013*t
        fd2 =  0.0027377786
        fd3 =  0.0367481951 - 0.0000000005*t
        fd4 =  0.0338631920 - 0.0000000003*t
        fd5 = -0.0001470938 + 0.0000000003*t
        dd(1) = 1.d0 - fd4
        dd(2) = fd3 + fd5
        dd(3) = dd(2) - fd4
        dd(4) = dd(2) - fd1
        dd(5) = -fd5
        dd(6) = dd(3) - fd2
      endif
c
c   end of intialization (likely to be called only once)
c
c  compute phase and frequency of the given tidal constituent
c
      freq=0.d0
      phase=0.d0
      do i = 1,6
         freq =   freq + idood(i)*dd(i)
         phase = phase + idood(i)*d(i)
      enddo
c adjust phases to fall in the positive range 0 to 360
      phase = dmod(phase,360.d0)
      if(phase.lt.0.d0) phase = phase + 360.d0
      return
      end subroutine
c ---------------------------------------------------------------------
      SUBROUTINE shells(x,k,n)
      USE m_bern
      IMPLICIT NONE
c  sorts an array x, of length n, sorting upward, and returns
c  an array k which may be used to key another array to the
c  sorted pattern (i.e., if we had an array f to which x
c  corresponded before sorting, then after calling shells,
c  f(k(1)) will be the element of f corresponding to the
c  smallest x, f(k(2)) the next smallest, and so on.
c   revised 29-dec-82 so k is sorted in turn, the values of
c  k that point to identical values of x being put in increasing
c  order
c
c    calls no other routines
c
      INTEGER(i4b)              :: n
      REAL(r8b),DIMENSION(n)    :: x
      INTEGER(i4b),DIMENSION(n) :: k

      INTEGER(i4b)              :: igap,imax,i,iex,ipl,ik,j,l
      REAL(r8b)                 :: sv
      igap = n
      do 1 i = 1,n
 1    k(i) = i
 5    if(igap.le.1) go to 25
      igap = igap/2
      imax = n - igap
 10   iex = 0
      do 20 i = 1,imax
      ipl = i + igap
      if(x(i).le.x(ipl)) go to 20
      sv = x(i)
      ik = k(i)
      x(i) = x(ipl)
      k(i) = k(ipl)
      x(ipl) = sv
      k(ipl) = ik
      iex = iex + 1
 20   continue
      if(iex.gt.0) go to 10
      go to 5
c
c  now sort k's (for identical values of x, if any)
c
 25   j = 1
 30   if(j.ge.n) return
      if(x(j).eq.x(j+1)) go to 33
      j = j + 1
      go to 30
c  have at least two x's with the same value - see how long this is true
 33   l = j
 35   if(x(l).ne.x(l+1)) go to 38
      l = l + 1
      if(l.lt.n) go to 35
c  j and l are the indices within which x(i) does not change - sort k
 38   igap = l - j + 1
 40   if(igap.le.1) j = l + 1
      if(igap.le.1) go to 30
      igap = igap/2
      imax = l-j+1 - igap
 45   iex = 0
      do 50 i=1,imax
      ipl = i + igap + j - 1
      if(k(i+j-1).le.k(ipl)) go to 50
      ik = k(i+j-1)
      k(i+j-1) = k(ipl)
      k(ipl) = ik
      iex = iex + 1
 50   continue
      if(iex.gt.0) go to 45
      go to 40
      end subroutine
c ---------------------------------------------------------------------
      subroutine spline3(nn,x,u,s,a)
c  finds array  s  for spline interpolator  eval.
c  nn  number of data points supplied (may be negative, see below)
c  x  array containing x-coordinates where function is sampled.  xx(1),xx(2),...
c     must be a strictly increasing sequence.
c  u  array containing sample values that are to be interpolated.
c  s  output array of 2nd derivative at sample points.
c  a  working space array of dimension at least  nn.
c  if the user wishes to force the derivatives at the ends of the series to
c  assume specified values, he should put du(1)/dx and du(n)/dx in s1,s2
c  and call the routine with nn=-number of terms in series.  normally a parabola
c  is fitted through the 1st and last 3 points to find the slopes.
c  if less than 4 points are supplied, straight lines are fitted.
      USE m_bern
      IMPLICIT NONE
      INTEGER(i4b)             :: nn
      REAL(r8b),DIMENSION(*)   :: x
      REAL(r8b),DIMENSION(*)   :: u
      REAL(r8b),DIMENSION(*)   :: s
      REAL(r8b),DIMENSION(*)   :: a

      REAL(r8b)                :: u1,x1,u2,x2,q1,qn,c,q
      INTEGER(i4b)             :: n,n1,i,j
c
      q(u1,x1,u2,x2)=(u1/x1**2-u2/x2**2)/(1.0/x1-1.0/x2)
c
      n=iabs(nn)
      if (n.le.3) go to 5000
      q1=q(u(2)-u(1),x(2)-x(1),u(3)-u(1),x(3)-x(1))
      qn=q(u(n-1)-u(n),x(n-1)-x(n),u(n-2)-u(n),x(n-2)-x(n))
      if (nn.gt.0) go to 1000
      q1=s(1)
      qn=s(2)
 1000 s(1)=6.0*((u(2)-u(1))/(x(2)-x(1)) - q1)
      n1= n - 1
      do 2000 i=2,n1
      s(i)= (u(i-1)/(x(i)-x(i-1)) - u(i)*(1.0/(x(i)-x(i-1))+
     + 1.0/(x(i+1)-x(i))) + u(i+1)/(x(i+1)-x(i)))*6.0
 2000 continue
      s(n)=6.0*(qn + (u(n1)-u(n))/(x(n)-x(n1)))
      a(1)=2.0*(x(2)-x(1))
      a(2)=1.5*(x(2)-x(1)) + 2.0*(x(3)-x(2))
      s(2)=s(2) - 0.5*s(1)
      do 3000 i=3,n1
      c=(x(i)-x(i-1))/a(i-1)
      a(i)=2.0*(x(i+1)-x(i-1)) - c*(x(i)-x(i-1))
      s(i)=s(i) - c*s(i-1)
 3000 continue
      c=(x(n)-x(n1))/a(n1)
      a(n)=(2.0-c)*(x(n)-x(n1))
      s(n)=s(n) - c*s(n1)
c  back substitiute
      s(n)= s(n)/a(n)
      do 4000 j=1,n1
      i=n-j
      s(i) =(s(i) - (x(i+1)-x(i))*s(i+1))/a(i)
 4000 continue
      return
c  series too short for cubic spline - use straight lines.
 5000 do 5500 i=1,n
 5500 s(i)=0.0
      return
      end subroutine
c ---------------------------------------------------------------------
      function eval(y,nn,x,u,s)
c  performs cubic spline interpolation of a function sampled at unequally
c  spaced intervals.  the routine spline  should be called to set up the array s
c  y  the coordinate at which function value is desired.
c  nn  number of samples of original function.
c  x  array containing sample coordinates. the sequence x(1),x(2).....x(nn)
c     must be strictly increasing.
c  u  array containing samples of function at the coordinates x(1),x(2)...
c  s  array containing the 2nd derivatives at the sample points.  found by the
c     routine  spline, which must be called once before beginning interpolation.
c  if  y  falls outside range(x(1),x(nn))  the value at the nearest endpoint
c  of the series is used.
      USE m_bern
      IMPLICIT NONE
      REAL(r8b)                 :: y
      INTEGER(i4b)              :: nn
      REAL(r8b),DIMENSION(*)    :: x
      REAL(r8b),DIMENSION(*)    :: u
      REAL(r8b),DIMENSION(*)    :: s
      REAL(r8b)                 :: eval

      INTEGER(i4b)              :: istart,k,k1
      REAL(r8b)                 :: dy,dy1,dk,deli,ff1,ff2,f1,f2,f3
      data istart/1/
c
      nn=iabs(nn)
      if (y.lt.x(1))  go to 3000
      if (y.gt.x(nn)) go to 3100
c  locate interval (x(k1),x(k))  containing y
      if (y-x(istart)) 1200,1000,1000
c  scan up the x array
 1000 do 1100 k=istart,nn
      if (x(k).gt.y) go to 1150
 1100 continue
 1150 k1=k-1
      go to 1500
c  scan downwards in x array
 1200 do 1300 k=1,istart
      k1=istart-k
      if (x(k1).le.y) go to 1350
 1300 continue
 1350 k=k1+1
 1500 istart=k1
c  evaluate interpolate
      dy=x(k)-y
      dy1=y-x(k1)
      dk=x(k)-x(k1)
      deli=1./(6.0*dk)
      ff1=s(k1)*dy*dy*dy
      ff2=s(k)*dy1*dy1*dy1
      f1=(ff1+ff2)*deli
      f2=dy1*((u(k)/dk)-(s(k)*dk)/6.)
      f3=dy*((u(k1)/dk)-(s(k1)*dk)/6.0)
      eval=f1+f2+f3
      return
c  out of range.  substitute end values.
 3000 eval=u(1)
      return
 3100 eval=u(nn)
      return
      end function eval
      end module s_harload
