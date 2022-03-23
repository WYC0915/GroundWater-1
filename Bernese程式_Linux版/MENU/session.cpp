
/* -------------------------------------------------------------------------
 * Bernese GPS Software Version 5.0
 * -------------------------------------------------------------------------
 *
 * Class:      t_session
 *
 * Purpose:    This class stores session processed by BPE
 *
 * Author:     L. Mervart
 *
 * Created:    23-APR-2001
 *
 * Changes:
 *
 * Copyright:  Astronomical Institute
 *              University of Berne
 *                  Switzerland
 * -----------------------------------------------------------------------*/

#include <stdlib.h>
#include <math.h>
#include <qregexp.h>
#include "session.h"
#include "initmenu.h"
#include "errormsg.h"
#include "menutils.h"
#include "script.h"

// Constructor
////////////////////////////////////////////////////////////////////////////
t_session::t_session(const QString& yearStr, const QString& sessStr,
                     int offset) {

  _foundInListOfSessions = false;

  // Set the waiting status (used in BPE)
  // ------------------------------------
  _status = waiting;

  // Wild Cards ?
  // ------------
  _wildCards = false;
  for (int jj = 0; jj < initmenu.list_of_sessions().count(); jj++) {
    if ( initmenu.list_of_sessions()[jj].left(3) == "???" ) {
      _wildCards = true;
      break;
    }
  }

  // Index of this session
  // ---------------------
  _index = -1;
  for (int ii = 0; ii < initmenu.list_of_sessions().count(); ii++) {
    if ( initmenu.list_of_sessions()[ii] == sessStr ||
         ( initmenu.list_of_sessions()[ii].left(3) == "???" &&
           initmenu.list_of_sessions()[ii].mid(3,1) == sessStr.mid(3,1) ) ) {
      _index = ii;
      break;
    }
  }
  if (_index == -1) {
    errormsg("Session " + sessStr + " not found in session table");
    _index = 0;
    offset = 0;
    initmenu.setSessChar( sessChar(offset) );
  }
  else {
    _foundInListOfSessions = true;
  }

  // Compute the MJD
  // ---------------
  if (_wildCards) {
    t_juldat hlp; hlp.setDoY(yearStr.toInt(), sessStr.left(3).toDouble());
    _MJD = hlp.getMJD() + dayOffset(offset);
  }
  else {
    _MJD  = 0;
  }

  // Apply the offset
  // ----------------
  _index = (offset + _index);
  if (_wildCards) {
    while (_index < 0) {
      _index += initmenu.list_of_sessions().count();
    }
    _index = _index % initmenu.list_of_sessions().count();
  }
  if (_index < 0 || _index >= (int) initmenu.list_of_sessions().count()) {
    errormsg("Session offset out of session table range");
    _index = -1;
  }
}

// Destructor
////////////////////////////////////////////////////////////////////////////
t_session::~t_session() {
}

// Recompute the offset into days
////////////////////////////////////////////////////////////////////////////
int t_session::dayOffset(int offset) const {
  if (_wildCards) {
    return int( floor( double(offset + _index) /
                       double(initmenu.list_of_sessions().count()) ) );
  }
  else {
    return 0;
  }
}

// Get the correct MJD
////////////////////////////////////////////////////////////////////////////
int t_session::getJuldat(int offset, t_juldat& hlp) const {

  if (_index == -1) return 1;

  if (_wildCards) {
    hlp.setMJD(_MJD + dayOffset(offset));
    return 0;
  }
  else {
    int ii = _index + offset;
    if (ii >= 0 && ii < (int) initmenu.list_of_sessions().count()) {
      QStringList ymd = QStringList::split(QRegExp("\\s+"),
                                           initmenu.list_of_sessions(1)[ii]);
      if (ymd.size() < 3) {
        return 1;
      }
      hlp.setYMD(ymd[0].toInt(), ymd[1].toInt(), ymd[2].toDouble());
      return 0;
    }
  }
  return 1;
}

// Get current session + an optional offset in various formats
////////////////////////////////////////////////////////////////////////////
QString t_session::currMJD(int offset) const {
  t_juldat hlp;
  if (getJuldat(offset,hlp) == 0) {
    return( QString().sprintf("%5.5d", (int) hlp.getMJD()) );
  }
  else {
    return "     ";
  }
}

//
////////////////////////////////////////////////////////////////////////////
QString t_session::currYear(int offset) const {
  t_juldat hlp;
  if (getJuldat(offset,hlp) == 0) {
    int    YY ;
    int    MM ;
    double DD ;
    hlp.getYMD(YY, MM, DD);
    return( QString().sprintf("%2.2d", YY % 100) );
  }
  else {
    return "  ";
  }
}

//
////////////////////////////////////////////////////////////////////////////
QString t_session::currYr_4(int offset) const {
  t_juldat hlp;
  if (getJuldat(offset,hlp) == 0) {
    int    YY ;
    int    MM ;
    double DD ;
    hlp.getYMD(YY, MM, DD);
    return( QString().sprintf("%4.4d", YY) );
  }
  else {
    return "    ";
  }
}

//
////////////////////////////////////////////////////////////////////////////
QString t_session::currMonth(int offset) const {
  t_juldat hlp;
  if (getJuldat(offset,hlp) == 0) {
    int    YY ;
    int    MM ;
    double DD ;
    hlp.getYMD(YY, MM, DD);
    return(QString().sprintf("%2.2d", MM));
  }
  else {
    return "  ";
  }
}

//
////////////////////////////////////////////////////////////////////////////
QString t_session::currDay(int offset) const {
  t_juldat hlp;
  if (getJuldat(offset,hlp) == 0) {
    int    YY ;
    int    MM ;
    double DD ;
    hlp.getYMD(YY, MM, DD);
    return(QString().sprintf("%2.2d", (int) DD));
  }
  else {
    return "  ";
  }
}

//
////////////////////////////////////////////////////////////////////////////
QString t_session::currSession(int offset) const {
  t_juldat hlp;
  if (getJuldat(offset,hlp) == 0) {
    int    YY ;
    double DoY ;
    hlp.getDoY(YY, DoY);
    return(QString().sprintf("%3.3d", (int) DoY) + sessChar(offset));
  }
  else {
    return "   ";
  }
}

//
////////////////////////////////////////////////////////////////////////////
QString t_session::currDayyear(int offset) const {
  t_juldat hlp;
  if (getJuldat(offset,hlp) == 0) {
    int    YY ;
    double DoY ;
    hlp.getDoY(YY, DoY);
    return(QString().sprintf("%3.3d", (int) DoY));
  }
  else {
    return "   ";
  }
}

//
////////////////////////////////////////////////////////////////////////////
QString t_session::currGPSweek(int offset) const {
  t_juldat hlp;
  if (getJuldat(offset,hlp) == 0) {
    int    GPSWeek ;
    double WeekDay;
    hlp.getGPS(GPSWeek, WeekDay);
    return(QString().sprintf("%4.4d", GPSWeek));
  }
  else {
    return "    ";
  }
}

//
////////////////////////////////////////////////////////////////////////////
QString t_session::currDayweek(int offset) const {
  t_juldat hlp;
  if (getJuldat(offset,hlp) == 0) {
    int    GPSWeek ;
    double WeekDay;
    hlp.getGPS(GPSWeek, WeekDay);
    return(QString().sprintf("%d", (int) WeekDay));
  }
  else {
    return " ";
  }
}

//
////////////////////////////////////////////////////////////////////////////
QString t_session::sessChar(int offset) const {
  if (_index != -1 && initmenu.list_of_sessions().count() > 0) {
    int ii = (offset + _index);
    if (_wildCards) {
      while (ii < 0) {
        ii += initmenu.list_of_sessions().count();
      }
      ii = ii % initmenu.list_of_sessions().count();
    }
    if (ii >= 0 && ii < (int) initmenu.list_of_sessions().count()) {
      return initmenu.list_of_sessions()[ii].mid(3,1);
    }
  }
  return " ";
}

//
////////////////////////////////////////////////////////////////////////////
void t_session::updateTimeStatistics(const t_script* script, bool error,
                                     QString& taskid) {

  if (_ts.taskid.isEmpty()) {
    _ts.taskid = script->getKey("TASKID") + currYear()
               + currDayyear() + sessChar();
  }

  taskid = _ts.taskid;

  QString pidSubPid = script->pid() + "_" + script->subPid();

  double cpu   = script->timeCpu();
  double pgm   = script->timePgm();
  double aux   = script->timeAux();
  double delay = script->timeDelay();
  double queue = script->timeQueue();

  _ts.cpu   += cpu;
  _ts.pgm   += pgm;
  _ts.aux   += aux;
  _ts.delay += delay;
  _ts.queue += queue;

  if (_ts.pidMaxCpu.isEmpty() || _ts.maxCpu < cpu) {
    _ts.pidMaxCpu = pidSubPid;
    _ts.maxCpu = cpu;
  }
  if (_ts.pidMaxPgm.isEmpty() || _ts.maxPgm < pgm) {
    _ts.pidMaxPgm = pidSubPid;
    _ts.maxPgm = pgm;
  }
  if (_ts.pidMaxAux.isEmpty() || _ts.maxAux < aux) {
    _ts.pidMaxAux = pidSubPid;
    _ts.maxAux = aux;
  }
  if (_ts.pidMaxDelay.isEmpty() || _ts.maxDelay < delay) {
    _ts.pidMaxDelay = pidSubPid;
    _ts.maxDelay = delay;
  }
  if (_ts.pidMaxQueue.isEmpty() || _ts.maxQueue < queue) {
    _ts.pidMaxQueue = pidSubPid;
    _ts.maxQueue = queue;
  }

  if (script->subPid() == "000") {
    _ts.numPid += 1;
  }
  else {
    _ts.numSubPid += 1;
  }

  if (script->rerunCount() > 1) {
    _ts.numRerun += 1;
  }

  if (error) {
    _ts.numError += 1;
  }
  else {
    _ts.numOk += 1;
  }
}

//
////////////////////////////////////////////////////////////////////////////
QString t_session::printTimeStatistics() {
  int timeBpe = statBpe();
  QString msg;
  QTime duration = QTime(0,0,0).addSecs(timeBpe);
  msg += QString().sprintf("%s  %6d  %6d  %6d  %6d  %6d  %6d   "
                           "%6d  %6d  %6d  %6d  %6d",
                           _ts.taskid.toAscii().data(),
                           timeBpe, int(_ts.cpu), int(_ts.pgm),
                           int(_ts.aux), int(_ts.delay), int(_ts.queue),
                           _ts.numPid, _ts.numSubPid, _ts.numOk,
                           _ts.numError, _ts.numRerun);
  msg += _ts.start.toString("  dd-MMM-yyyy hh:mm:ss")
       + _ts.end.toString("  dd-MMM-yyyy hh:mm:ss")
       + duration.toString("  HH:mm:ss\n");
  msg += QString().sprintf("Max               %6d  %6d  %6d  %6d  %6d\n",
                           int(_ts.maxCpu), int(_ts.maxPgm), int(_ts.maxAux),
                           int(_ts.maxDelay), int(_ts.maxQueue));
  msg += "PID_SUB          " + _ts.pidMaxCpu + " " + _ts.pidMaxPgm + " "
       + _ts.pidMaxAux + " " + _ts.pidMaxDelay + " " + _ts.pidMaxQueue + "\n";
  msg += "-------------------------------------------------------------------------------------------------------------------------------------------------------\n";
  return msg;
}
