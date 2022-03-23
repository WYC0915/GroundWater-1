
/* -------------------------------------------------------------------------
 * Bernese GPS Software Version 5.0
 * -------------------------------------------------------------------------
 *
 * Class:      t_bpelog
 *
 * Purpose:    This class displays/writes BPE messages
 *
 * Author:     L. Mervart
 *
 * Created:    26-DEC-2000
 *
 * Changes:
 *
 * Copyright:  Astronomical Institute
 *              University of Berne
 *                  Switzerland
 * -----------------------------------------------------------------------*/

#include <qmessagebox.h>
#include <qdatetime.h>

#include "bpelog.h"
#include "initmenu.h"
#include "errormsg.h"
#include "menutils.h"
#include "server.h"
#include "script.h"
#include "r_file.h"

// Constructor
////////////////////////////////////////////////////////////////////////////
t_bpelog::t_bpelog(QWidget* parent, const QString& caption,
                   const QString& logFileName, const QString& errFileName,
                   t_bpe* bpe) : QObject() {

  if (initmenu.getIntModus() == t_initmenu::INT) {
    _bpedial = new t_bpedial(parent, caption, bpe);
  }
  else {
    _bpedial = 0;
  }

  // Log File (SYSOUT)
  // -----------------
  if (!logFileName.isEmpty()) {
    _logFile = new r_file( expandEnvVar(logFileName) );
    if ( !_logFile->open(QIODevice::WriteOnly | QIODevice::Text) ) {
      errormsg("Cannot write into file " + logFileName);
      return ;
    }
  }
  else {
    _logFile = 0;
  }

  // Error File (SYSERR)
  // -------------------
  if (!errFileName.isEmpty()) {
    _errFile = new r_file( expandEnvVar(errFileName) );
    if ( !_errFile->open(QIODevice::WriteOnly | QIODevice::Text) ) {
      errormsg("Cannot write into file " + errFileName);
      return ;
    }
  }
  else {
    _errFile = _logFile;
  }

}

// Destructor
////////////////////////////////////////////////////////////////////////////
t_bpelog::~t_bpelog() {
  if (_errFile && _errFile != _logFile) {
    _errFile->close();
    delete _errFile; _errFile = 0;
  }
  if (_logFile) {
    _logFile->close(); _logFile = 0;
    delete _logFile;
  }
  if (_bpedial) {
    _bpedial->enableClose();
  }
}

// Format the Message stemming from script
////////////////////////////////////////////////////////////////////////////
void t_bpelog::message(t_script* script,
                       const QString& msg, t_msgtype msgtype) {

  if (msgtype == t_bpelog::msg) {
    QDateTime dateTime = QDateTime::currentDateTime();
    QDate date = dateTime.date();
    QTime time = dateTime.time();
    QString dtStr;
    dtStr.sprintf("%2.2d-%3s-%4.4d %2.2d:%2.2d:%2.2d", date.day(),
                  date.monthName( date.month() ).ascii(), date.year(),
                  time.hour(), time.minute(), time.second());

    message( QString().sprintf(" %s  %s %s_%s  %-8s %-8s : %s\n",
                               dtStr.ascii(), script->session().ascii(),
                               script->pid().ascii(),
                               script->subPid().ascii(),
                               script->getKey("SCRIPT").ascii(),
                               script->getKey("OPT_DIR").ascii(),
                               msg.ascii()), msgtype );
  }
  else if (msgtype == t_bpelog::error) {
    message( QString().sprintf("\n *** BPE ERROR: %s\n"
                                 "                PID:     %s_%s\n"
                                 "                Script:  %s\n\n",
                               msg.ascii(),
                               script->pid().ascii(),
                               script->subPid().ascii(),
                               script->getKey("SCRIPT").ascii()), msgtype );
  }
  else if (msgtype == t_bpelog::debug && script->debug()) {
    message(msg + "\n", msgtype);
  }
}

// Format the Message stemming from server
////////////////////////////////////////////////////////////////////////////
void t_bpelog::message(t_server* server,
                       const QString& msg, t_msgtype msgtype) {

  if (msgtype == t_bpelog::msg) {
    QDateTime dateTime = QDateTime::currentDateTime();
    QDate date = dateTime.date();
    QTime time = dateTime.time();
    QString dtStr;
    dtStr.sprintf("%2.2d-%3s-%4.4d %2.2d:%2.2d:%2.2d", date.day(),
                  date.monthName( date.month() ).ascii(), date.year(),
                  time.hour(), time.minute(), time.second());

    message( QString().sprintf(" %s  %s YR:%s  %-17s : %s\n",
                               dtStr.ascii(),
                               server->session()->currSession().ascii(),
                               server->session()->currYr_4().ascii(),
                               server->pcfName(true).ascii(),
                               msg.ascii()), msgtype );
  }
  else if (msgtype == t_bpelog::error) {
    message( QString().sprintf("\n *** BPE ERROR: %s\n"
                                 "                Session: %s/%s"
                                 "                PCFile:  %s\n\n",
                               msg.ascii(),
                               server->session()->currYear().ascii(),
                               server->session()->currSession().ascii(),
                               server->pcfName(true).ascii()), msgtype );
  }
  else if (msgtype == t_bpelog::debug && server->debug()) {
    message(msg + "\n", msgtype);
  }
}

// Display and Write the Message (private function)
////////////////////////////////////////////////////////////////////////////
void t_bpelog::message(const QString& msg, t_msgtype msgtype, int maxLines) {

  if      (msgtype == t_bpelog::summary) {
    if (_bpedial) {
      _bpedial->setText(msg,maxLines);
    }
  }
  else if (msgtype == t_bpelog::error) {
    if (_errFile) {
      _errFile->writeBlock(msg, msg.length());
      _errFile->flush();
    }
    if (_bpedial) {
      _bpedial->addText(msg,0);
    }
  }
  else {
    if (_logFile) {
      _logFile->writeBlock(msg, msg.length());
      _logFile->flush();
    }
  }
}
