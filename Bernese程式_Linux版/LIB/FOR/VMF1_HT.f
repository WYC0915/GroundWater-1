      MODULE s_vmf1_ht
      CONTAINS

      subroutine vmf1_ht (ah,aw,dmjd,dlat,ht,zd,vmf1h,vmf1w)

C     !!! This is the version with height correction !!!
C     !!! It has to be used with the grid !!!
C
C     This subroutine determines the VMF1 (Vienna Mapping Functions 1)
C     Reference: Boehm, J., B. Werl, H. Schuh (2006),
C     Troposphere mapping functions for GPS and very long baseline interferometry
C     from European Centre for Medium-Range Weather Forecasts operational analysis data,
C     J. Geoph. Res., Vol. 111, B02406, doi:10.1029/2005JB003629.
C
C     input data
C     ----------
C     ah:   hydrostatic coefficient a (www.hg.tuwien.ac.at/~ecmwf1)
C     aw:   wet coefficient a         (www.hg.tuwien.ac.at/~ecmwf1)
C     dmjd: modified julian date
C     dlat: latitude in radians
C     ht:   ellipsoidal height in meter
C     zd:   zenith distance in radians
C
C     output data
C     -----------
C     vmf1h: hydrostatic mapping function
C     vmf1w: wet mapping function
C
C     Johannes Boehm, 2005 October 2
C
C     Adaptation to Bernese GPS Software:  R. Dach        (30-06-2008)
C

      USE m_bern
      USE d_const, ONLY: pi
      IMPLICIT NONE
C
C PARAMETERS (description see above)
      REAL(r8b) :: ah, aw
      REAL(r8b) :: dmjd
      REAL(r8b) :: dlat, ht
      REAL(r8b) :: zd

      REAL(r8b),DIMENSION(2) :: vmf1h,vmf1w
C
C LOCAL VARIABLES
      REAL(r8b) :: doy
      REAL(r8b) :: beta,gamma
      REAL(r8b) :: a_ht, b_ht
      REAL(r8b) :: bw
      REAL(r8b) :: c11h,c10h,c0h
      REAL(r8b) :: c_ht
      REAL(r8b) :: cw,ch,bh
      REAL(r8b) :: ht_corr,ht_corr_coef
      REAL(r8b) :: hs_km,phh
      REAL(r8b) :: topcon,sine,cose

C     Pi is defined by defcon
C      pi = 3.14159265359d0

C     reference day is 28 January
C     this is taken from Niell (1996) to be consistent
      doy = dmjd  - 44239.d0 + 1 - 28

      bh = 0.0029d0
      c0h = 0.062d0
      if (dlat.lt.0.d0) then   ! southern hemisphere
          phh  = pi
          c11h = 0.007d0
          c10h = 0.002d0
      else                     ! northern hemisphere
          phh  = 0.d0
          c11h = 0.005d0
          c10h = 0.001d0
      end if
      ch = c0h + ((dcos(doy/365.25d0*2.d0*pi + phh)+1.d0)*c11h/2.d0
     .     + c10h)*(1.d0-dcos(dlat))


      sine   = dsin(pi/2.d0 - zd)
      cose   = dcos(pi/2.d0 - zd)
      beta   = bh/( sine + ch  )
      gamma  = ah/( sine + beta)
      topcon = (1.d0 + ah/(1.d0 + bh/(1.d0 + ch)))
      vmf1h(1)   = topcon/(sine+gamma)

c derivative
      vmf1h(2)= vmf1h(1)**2/topcon*cose*(1d0-gamma**2/ah*(1-beta**2/bh))

C  height correction [Niell, 1996]
      a_ht = 2.53d-5
      b_ht = 5.49d-3
      c_ht = 1.14d-3
      hs_km  = ht/1000.d0
      beta         = b_ht/( sine + c_ht)
      gamma        = a_ht/( sine + beta)
      topcon       = (1.d0 + a_ht/(1.d0 + b_ht/(1.d0 + c_ht)))
      ht_corr_coef = 1.d0/sine - topcon/(sine + gamma)
      ht_corr      = ht_corr_coef * hs_km
      vmf1h(1)     = vmf1h(1) + ht_corr

c derivative
      vmf1h(2)= vmf1h(2) + (cose/sine**2 -
     1                     topcon*cose/(sine+gamma)**2*
     2                     (1d0-gamma**2/a_ht*(1-beta**2/b_ht))) * hs_km

      bw = 0.00146d0
      cw = 0.04391d0
      beta   = bw/( sine + cw )
      gamma  = aw/( sine + beta)
      topcon = (1.d0 + aw/(1.d0 + bw/(1.d0 + cw)))
      vmf1w(1)   = topcon/(sine+gamma)

c derivative
      vmf1w(2)= vmf1w(1)**2/topcon*cose*
     1         (1d0-gamma**2/aw*(1d0-beta**2/bw))

      end subroutine

      END MODULE

