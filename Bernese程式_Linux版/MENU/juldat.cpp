
/* -------------------------------------------------------------------------
 * Bernese GPS Software Version 5.0
 * -------------------------------------------------------------------------
 *
 * Class:      t_juldat
 *
 * Purpose:    This class performes the computations of different date
 *             formats (modified julian date, GPS week, day of year etc.)
 *
 * Author:     L. Mervart
 *
 * Created:    18-APR-2000
 *
 * Changes:
 *
 * Copyright:  Astronomical Institute
 *              University of Berne
 *                  Switzerland
 * -----------------------------------------------------------------------*/

#include <math.h>
#include "juldat.h"

// Constructor
////////////////////////////////////////////////////////////////////////////
t_juldat::t_juldat()
{
}

// Destructor
////////////////////////////////////////////////////////////////////////////
t_juldat::~t_juldat()
{
}

// Set Modified Julian Date
////////////////////////////////////////////////////////////////////////////
void t_juldat::setMJD(double MJD)
{
  mjd = MJD;
}

// Set year, month and day
////////////////////////////////////////////////////////////////////////////
void t_juldat::setYMD(int YY, int MM, double DD)
{
  int  ii, kk;

  if( MM <= 2 ) {
    YY = YY - 1;
    MM = MM + 12;
  }

  ii = YY/100;
  kk = 2 - ii + ii/4;
  mjd = (365.25*YY - fmod( 365.25*YY, 1.0 )) - 679006.0;
  mjd = mjd + floor( 30.6001*(MM + 1) ) + DD + kk;
}

// Set GPS week and day of week
////////////////////////////////////////////////////////////////////////////
void t_juldat::setGPS(int GPSWeek, double WeekDay)
{
  mjd = 44244.0 + GPSWeek*7.0 + WeekDay ;
}

// Set year and day of year
////////////////////////////////////////////////////////////////////////////
void t_juldat::setDoY(int YY, double DoY)
{
  t_juldat hlp;
  hlp.setYMD(YY, 1, 0.0);
  mjd = hlp.getMJD() + DoY;
}

// Get Modified Julian Date
////////////////////////////////////////////////////////////////////////////
double t_juldat::getMJD()
{
  return mjd;
}

// Get year, month and day
////////////////////////////////////////////////////////////////////////////
void t_juldat::getYMD(int& YY, int& MM, double& DD)
{
  long   ih, ih1, ih2    ;
  double t1, t2 , t3 , t4;

  t1  = 1.0 + mjd - fmod( mjd, 1.0 ) + 2400000.0;
  t4  = fmod( mjd, 1.0 );
  ih  =  long( (t1 - 1867216.25)/36524.25 );
  t2  = t1 + 1 + ih - ih/4;
  t3  = t2 - 1720995.0;
  ih1 =  long( (t3 - 122.1)/365.25 );
  t1  = 365.25*ih1 - fmod( 365.25*ih1, 1.0 );
  ih2 = long( (t3 - t1)/30.6001 );

  DD  = t3 - t1 - (int)( 30.6001*ih2 ) + t4;

  MM  = ih2 - 1;
  if ( ih2 > 13 ) MM = ih2 - 13;

  YY  = ih1;
  if ( MM <= 2 ) YY = YY + 1;
}

// Get GPS week and day of week
////////////////////////////////////////////////////////////////////////////
void t_juldat::getGPS(int& GPSWeek, double& WeekDay)
{
  double  deltat = mjd - 44244.0 ;

  GPSWeek = (int) floor(deltat/7.0);
  WeekDay = deltat - GPSWeek * 7.0;
}

// Get year and day of year
////////////////////////////////////////////////////////////////////////////
void t_juldat::getDoY(int& YY, double& DoY)
{
  int      MM;
  double   DD;

  getYMD(YY, MM, DD);

  t_juldat hlp;
  hlp.setYMD(YY, 1, 0.0);

  DoY = getMJD() - hlp.getMJD();
}
