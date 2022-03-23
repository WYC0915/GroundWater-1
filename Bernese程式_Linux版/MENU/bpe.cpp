
/* -------------------------------------------------------------------------
 * Bernese GPS Software Version 5.0
 * -------------------------------------------------------------------------
 *
 * Class:      t_bpe
 *
 * Purpose:    This class implements the Bernese Processing Engine
 *
 * Author:     L. Mervart
 *
 * Created:    24-APR-2002
 *
 * Changes:
 *
 * Copyright:  Astronomical Institute
 *              University of Berne
 *                  Switzerland
 * -----------------------------------------------------------------------*/

#include <stdlib.h>
#include <qdatetime.h>
#include <math.h>

#include "bpe.h"
#include "bpeinp.h"
#include "server.h"
#include "session.h"
#include "menutils.h"
#include "initmenu.h"
#include "errormsg.h"
#include "r_file.h"

// Constructor
////////////////////////////////////////////////////////////////////////////
t_bpe::t_bpe(QWidget* parent, const QString& inpFileName) :
                                                   QObject(parent, 0) {
  _bpelog      = 0;
  _cpufile     = 0;
  _statusTimer = 0;
  _statusFile  = 0;
  _reprMode    = false;
  _timeExpired = false;
  _bpeTotalCPUtime = 0.0;
  _startTime.start();

  initmenu.setExcept(true);
  try {

    // Read the Input Options
    // ----------------------
    _inpFileName = initmenu.getPath("PTH_SCR")
                 + stripPath(inpFileName) +  "_" + initmenu.uniqueString() ;
    fileCopy(inpFileName, _inpFileName);
    t_bpeinp inpfile(_inpFileName, false); inpfile.expandSelList();

    // Name of the PCF file
    // --------------------
    _pcfNamePath = inpfile.getKeySel0("PCF_FILE");
    _pcfName     = stripPath(_pcfNamePath);

    // Copy NEWCAMP_INP File
    // ---------------------
    QString inpFileNameNewCamp = initmenu.getKeySel0("NEWCAMP_INP");
    _inpFileNameNewCamp = initmenu.getPath("PTH_SCR")
                        + stripPath(inpFileNameNewCamp)
                        + "_" + initmenu.uniqueString() ;
    fileCopy(inpFileNameNewCamp, _inpFileNameNewCamp);

    // Check for Suspended PCF
    // -----------------------
    t_cpufile cpuInpFile( inpfile.getKeySel0("CPU_FILE") );
    if (!cpuInpFile.ok()) {
      qApp->exit(1);
    }
    if (cpuInpFile.suspended(_pcfName)) {
      errormsg("PCF " + _pcfName + " suspended");
      ///      delete this;
      ///      return;
    }

    _bpeMaxtime = inpfile.getKeySel0("BPE_MAXTIME").toInt();

    // Way of processing the sessions (serial, parallel, crazy superBPE)
    // -----------------------------------------------------------------
    int numSess = inpfile.getKeySel0("NUM_SESS").toInt();
    if (numSess == 0) {
      numSess = 1;
    }
    if ( abs(numSess) <= 1 || inpfile.getKeySel0("SUPERBPE") != "1" ) {
      _superBPE = serial;
    }
    else if ( inpfile.getKeySel0("RADIO_P") == "1") {
      _superBPE    = parallel;
      _maxParallel = inpfile.getKeySel0("MAXSESS").toInt();
      if (inpfile.getKeySel0("REPR_MODE") == "1") {
        _reprMode = true;
      }
    }
    else {
      _superBPE = crazy;
      numSess   = 1;
    }

    // CPU File
    // --------
    if (_superBPE != crazy) {
      _cpufile = new t_cpufile( inpfile.getKeySel0("CPU_FILE") );
      if (!_cpufile->ok()) {
        qApp->exit(1);
      }
    }

    // Continue with next session after error or not
    // ---------------------------------------------
    _maxNumErrors = inpfile.getKeySel0("NEXTSESS").toInt();

    // List of processed sessions
    // --------------------------
    int moduloSess = inpfile.getKeySel0("MODULO_SESS").toInt();
    if (moduloSess == 0) moduloSess = 1;
    for (int ii = 0; ii < abs(numSess); ii += moduloSess) {
      int offset = ii;
      if (numSess < 0) {
        offset = -ii;
      }
      _sessionList.append( new t_session( inpfile.getKeySel0("YEAR"),
                                       inpfile.getKeySel0("SESSION"), offset) );
    }

    if (_superBPE == crazy) {
      inpfile.setSuperBPE(true);
    }

    // Initialize the BPE Log
    // ----------------------
    QString sysoutName = inpfile.getKeySel0("SYSOUT");

    QString syserrName;
    if (inpfile.getKeySel0("ERRMRG") != "1") {
      syserrName = inpfile.getKeySel0("SYSERR");
    }

    _bpelog = new t_bpelog(parent, "BPE Server: " +
                           stripPath(inpfile.getKeySel0("PCF_FILE")),
                           sysoutName, syserrName, this);

    // Status (summary) File and Timer
    // -------------------------------
    _statusTimer = new QTimer(this);
    connect(_statusTimer, SIGNAL(timeout()), this, SLOT(slotWriteStatus()));
    QString statusFileName;
    if (_superBPE == crazy) {
      statusFileName = inpfile.getKeySel0("S_STATUS") ;
    }
    else {
      statusFileName = inpfile.getKeySel0("STATUS") ;
    }
    if (!statusFileName.isEmpty()) {
      _statusFile = new r_file(expandEnvVar(statusFileName));
    }

    // Name of the PCF file
    // Start the processing
    // --------------------
    startMessage();
    startSessions();
    slotWriteStatus();

  }
  catch (const QString& str) {
    errormsg(str);
    qApp->quit();
  }
}

// Destructor
////////////////////////////////////////////////////////////////////////////
t_bpe::~t_bpe() {
}

void t_bpe::cleanAll() {

  slotWriteStatus();
  delete _statusTimer; _statusTimer = 0;
  delete _statusFile;  _statusFile = 0;
  for (unsigned ii = 0; ii < _serverList.count(); ii++) {
    delete _serverList.at(ii);
  }
  for (unsigned jj = 0; jj < _sessionList.count(); jj++) {
    delete _sessionList.at(jj);
  }
  delete _bpelog;
  delete _cpufile;
  if (initmenu.getIntModus() == t_initmenu::NOINT) {
    qApp->quit();
  }
  r_file inpFile(expandEnvVar(_inpFileName));
  inpFile.remove();
  r_file inpFileNewCamp(expandEnvVar(_inpFileNameNewCamp));
  inpFileNewCamp.remove();
}

// One Server is Finished
////////////////////////////////////////////////////////////////////////////
void t_bpe::slotServerFinished(const t_session* session) {

  // Find the session and server index
  // ---------------------------------
  int sessIndex = _sessionList.find(session);
  if (sessIndex == -1) {
    errormsg("bpe.cpp: session not found");
    cleanAll();
    return;
  }

  // Find the server index
  // ---------------------
  int servIndex = findServerIndex(session);
  if (servIndex == -1) {
    errormsg("bpe.cpp: server not found");
    cleanAll();
    return;
  }

  // Set the session status
  // ----------------------
  if ( _serverList.at(servIndex)->status() == t_server::error) {
    _sessionList.at(sessIndex)->setStatus(t_session::error);
  }
  else {
    _sessionList.at(sessIndex)->setStatus(t_session::finished);
  }

  // Close the server
  // ----------------
  delete _serverList.at(servIndex);
  _serverList.remove(servIndex);

  // Start next session(s)
  // ---------------------
  startSessions();
}

// Start as many sessions as possible
////////////////////////////////////////////////////////////////////////////
void t_bpe::startSessions() {

  // Count number of sessions waiting, running, and error reporting
  // --------------------------------------------------------------
  unsigned numWaiting  = 0;
  unsigned numRunning  = 0;
  unsigned numErrors   = 0;
  unsigned numFinished = 0;
  for (unsigned ii = 0; ii < _sessionList.count(); ii++) {
    if      (_sessionList.at(ii)->status() == t_session::waiting)  {
      ++numWaiting;
    }
    else if (_sessionList.at(ii)->status() == t_session::running)  {
      ++numRunning;
    }
    else if (_sessionList.at(ii)->status() == t_session::error)    {
      ++numErrors;
    }
    else if (_sessionList.at(ii)->status() == t_session::finished) {
      ++numFinished;
    }
  }

  // Finish the BPE if no sessions waiting or running
  // ------------------------------------------------
  if (numWaiting + numRunning == 0) {
    endMessage(numFinished, numErrors);
    cleanAll();
    return;
  }

  // Optionally finish the BPE after an error in session
  // ---------------------------------------------------
  if (int(numErrors) > _maxNumErrors) {
    if (numRunning == 0) {
      endMessage(numFinished, numErrors);
      cleanAll();
      return;
    }
    return;
  }

  // Start the session(s)
  // --------------------
  unsigned maxStarted = 1;
  if (_superBPE == parallel) {
    if (_maxParallel < 1) {
      maxStarted = numWaiting;
    }
    else {
      maxStarted = _maxParallel - numRunning;
    }
  }
  unsigned numStarted = 0;
  for (unsigned jj = 0; jj < _sessionList.count(); jj++) {
    if (_sessionList.at(jj)->status() == t_session::waiting &&
        numStarted < maxStarted) {
      ++numStarted;
      startServer(jj);
    }
  }
}

// Start the BPE server
////////////////////////////////////////////////////////////////////////////
void t_bpe::startServer(int sessIndex) {

  bool superFlag = false;
  if (_superBPE == crazy) {
    superFlag = true;
  }

  bool firstSession  = (sessIndex == 0) ? true : false;

  bool waitForHeader = (_superBPE == parallel &&
                        sessIndex < (int)_maxParallel) ? true : false;

  _serverList.append( new t_server(this, _inpFileName, _cpufile,
                                   _sessionList.at(sessIndex), _bpelog,
                                   superFlag, firstSession,
                                   _sessionList.count(), waitForHeader,
                                   _reprMode ) );

  if (_serverList.count() > 1) {
    connect(_serverList.first(), SIGNAL(singletonFinished(const QString&)),
            _serverList.last(), SLOT(slotScriptFinished(const QString&)));

    connect(_serverList.last(), SIGNAL(serverFinished(const t_session*)),
            _serverList.first(), SLOT(slotStartTail()));
  }

  if (_serverList.last()->status() == t_server::waiting) {
    _sessionList.at(sessIndex)->setStatus(t_session::running);
    connect( _serverList.last(), SIGNAL(serverFinished(const t_session*)),
             this, SLOT(slotServerFinished(const t_session*)) );
    _serverList.last()->start();
  }
  else {
    _sessionList.at(sessIndex)->setStatus(t_session::error);
    slotServerFinished(_serverList.last()->session());
  }
}

// Print the first message
////////////////////////////////////////////////////////////////////////////
void t_bpe::startMessage() {
  if (_bpelog) {
    QString msg = " Time                  Sess PID      Script   "
                  "Option     Status\n";
    _bpelog->message(msg, t_bpelog::msg);
    QString sep; sep.fill('-',80);
    _bpelog->message(sep+"\n", t_bpelog::msg);
  }
}

// Print the last message
////////////////////////////////////////////////////////////////////////////
void t_bpe::endMessage(int numFinished, int numErrors) {
  if (_bpelog) {
    QString sep; sep.fill('-',80);
    _bpelog->message(sep+"\n", t_bpelog::msg);
    QString msg;
    int sec  = _startTime.elapsed()/1000;
    QTime duration = QTime(0,0,0).addSecs(sec);

    msg.sprintf(" Sessions finished: OK: %d    Error: %d              "
                "Total Time:   ", numFinished, numErrors);

    msg += duration.toString("HH:mm:ss\n");

    // CPU and Real-Time Statistics
    // ----------------------------
    msg += "\n\n-------------------------------------------------------------------------------------------------------------------------------------------------------\n"
               "                 Statistics concerning time in sec                Statistics concerning # of jobs                                                      \n"
               "Session      BPE     CPU  =  PGM  +  Aux + Delay Queuing      PID  +  SUB   =  OK + Error   Rerun  Start                 End                   Duration\n"
               "-------------------------------------------------------------------------------------------------------------------------------------------------------\n";
    for (unsigned jj = 0; jj < _sessionList.count(); jj++) {
      t_session* session = _sessionList.at(jj);
      msg += session->printTimeStatistics();
    }
    msg += this->printTimeStatisticsSessions();

    msg += this->printTimeStatistics();

    if (_cpufile) {
      msg += _cpufile->printTimeStatistics();
    }

    _bpelog->message(msg, t_bpelog::msg);
  }
}

// Find the server index
////////////////////////////////////////////////////////////////////////////
int t_bpe::findServerIndex(const t_session* session) {
  for (int ii = 0; ii < (int) _serverList.count(); ii++) {
    if ( _serverList.at(ii)->session() == session ) {
      return  ii;
    }
  }
  return -1;
}

// Re-Write the Status File
////////////////////////////////////////////////////////////////////////////
void t_bpe::slotWriteStatus() {

  if (_statusTimer && _statusTimer->isActive()) {
    _statusTimer->stop();
  }

  QString msg = "\nStatus of " + _pcfName + " at " +
                 QDateTime::currentDateTime().toString() + "\n\n";

  for (unsigned jj = 0; jj < _sessionList.count(); jj++) {
    const t_session* sess = _sessionList.at(jj);
    if      (sess->status() == t_session::waiting) {
      msg += QString().sprintf("Session %s: waiting\n",
                               sess->currSession().ascii());
    }
    else if (sess->status() == t_session::running) {
      msg += QString().sprintf("Session %s: running\n",
                               sess->currSession().ascii());
      int servIndex = findServerIndex(sess);
      if (servIndex != -1) {
        const t_server* serv = _serverList.at(servIndex);
        QStringList pids;
        for (unsigned ii = 0; ii < serv->scriptList()->count(); ii++) {
          const t_script* script = serv->scriptAt(ii);

          // Already printed ?
          // -----------------
          if (pids.find(script->pid()) == pids.end()) {
            pids.append(script->pid());

            // Count the number of running (possible parallel) scripts
            // -------------------------------------------------------
            unsigned numRemaining = 0;
            for (unsigned kk=0; kk < serv->scriptList()->count();kk++) {
              const t_script* hlpScr = serv->scriptAt(kk);
              if (script->pid() == hlpScr->pid() && hlpScr->subPid() != "000"){
                ++numRemaining;
              }
            }
            QString statusStr = script->status();
            if (numRemaining > 0) {
              statusStr = "running";
            }

            msg += QString().sprintf("  %s %-8s %-8s    %s",
                                      script->pid().ascii(),
                                      script->getKey("SCRIPT").ascii(),
                                      script->getKey("OPT_DIR").ascii(),
                                      statusStr.ascii());

            if (statusStr == "running") {
              msg += "   <";
            }
            if (numRemaining > 0) {
              msg += QString().sprintf("      (%d remaining)", numRemaining);
            }
            msg += "\n";
          }
        }
      }
    }
    else if (sess->status() == t_session::finished) {
      msg += QString().sprintf("Session %s: finished\n",
                               sess->currSession().ascii());
    }
    else if (sess->status() == t_session::error) {
      msg += QString().sprintf("Session %s: error\n",
                               sess->currSession().ascii());
    }
  }

  if (_bpelog) {
    _bpelog->message(msg, t_bpelog::summary);
  }

  if (_statusFile) {
    if ( !_statusFile->open(QIODevice::WriteOnly | QIODevice::Text) ) {
      errormsg("Cannot write into file " + _statusFile->name());
      return;
    }
    _statusFile->writeBlock(msg, msg.length());
    _statusFile->flush(false);
  }

  const int statusUpdRate = 5000;  // 5 sec
  if (_statusTimer) _statusTimer->start(statusUpdRate, true);
}

//
////////////////////////////////////////////////////////////////////////////
void t_bpe::updateTimeStatistics(const t_script* script, bool error,
                                 const QString& taskid) {

  QString pid = script->pid();

  t_timeStatistics& ts = _ts[pid];

  ts.scriptName = script->getKey("SCRIPT");
  ts.optDir     = script->getKey("OPT_DIR");

  ts.numRuns += 1;
  ts.cpu     += script->timeCpu();
  ts.pgm     += script->timePgm();
  ts.aux     += script->timeAux();

  if (_bpeMaxtime > 0) {
    _bpeTotalCPUtime += ts.cpu;
    if (_bpeTotalCPUtime > _bpeMaxtime) {
      _timeExpired = true;
    }
  }

  if (ts.cpuMaxSess.isEmpty() || ts.cpuMax < script->timeCpu()) {
    ts.cpuMax     = script->timeCpu();
    ts.cpuMaxSess = (taskid.length() > 6) ? taskid.mid(2) : taskid;
    ts.cpuMaxPid  = script->pid() + "_" + script->subPid();
  }

  if (script->rerunCount() > 1) {
    ts.numRerun += 1;
  }
  if (error) {
    ts.numError += 1;
  }
}

//
////////////////////////////////////////////////////////////////////////////
QString t_bpe::printTimeStatistics() {

  QString msg;

  msg += "\n\nPCF-related BPE statistics:\n"
             "===========================\n";

  msg += _pcfNamePath + "\n";

  msg += "-----------------------------------------------------------------------------------\n"
         "                           Mean time in sec                                        \n"
         "PID  Script    OPT dir     CPU = PGM + Aux  Max CPU  Session  PID_SUB  Rerun  Error\n"
         "-----------------------------------------------------------------------------------\n";

  QMapIterator<QString, t_timeStatistics> it(_ts);
  while (it.hasNext()) {
    const t_timeStatistics& ts = it.next().value();
    QString pid = it.key();
    QString scriptName = ts.scriptName.leftJustified(8,' ');
    QString optDir     = ts.optDir.leftJustified(8,' ');

    msg += QString().sprintf("%s  %s  %s  %5d %5d %5d  %6d   %s   %s  %5d  %5d\n",
                             pid.toAscii().data(), scriptName.toAscii().data(),
                             optDir.toAscii().data(),
                             int(ts.cpu/ts.numRuns), int(ts.pgm/ts.numRuns),
                             int(ts.aux/ts.numRuns), int(ts.cpuMax),
                             ts.cpuMaxSess.toAscii().data(),
                             ts.cpuMaxPid.toAscii().data(), ts.numRerun,
                             ts.numError);
  }

  msg += "-----------------------------------------------------------------------------------\n";
  return msg;
}

//
////////////////////////////////////////////////////////////////////////////
QString t_bpe::printTimeStatisticsSessions() {

  unsigned numSess = _sessionList.count();
  if (numSess < 2) {
    return "";
  }

  QString msg;

  double totalBpe       = 0.0;
  double totalCpu       = 0.0;
  double totalPgm       = 0.0;
  double totalAux       = 0.0;
  double totalDelay     = 0.0;
  double totalQueue     = 0.0;
  int    totalNumPid    = 0;
  int    totalNumSubPid = 0;
  int    totalNumOk     = 0;
  int    totalNumError  = 0;
  int    totalNumRerun  = 0;

  double maxBpe       = 0.0; QString maxBpeSess;
  double maxCpu       = 0.0; QString maxCpuSess;
  double maxPgm       = 0.0; QString maxPgmSess;
  double maxAux       = 0.0; QString maxAuxSess;
  double maxDelay     = 0.0; QString maxDelaySess;
  double maxQueue     = 0.0; QString maxQueueSess;
  int    maxNumPid    = 0;   QString maxNumPidSess;
  int    maxNumSubPid = 0;   QString maxNumSubPidSess;
  int    maxNumOk     = 0;   QString maxNumOkSess;
  int    maxNumError  = 0;   QString maxNumErrorSess;
  int    maxNumRerun  = 0;   QString maxNumRerunSess;

  double minBpe       = 0.0; QString minBpeSess;
  double minCpu       = 0.0; QString minCpuSess;
  double minPgm       = 0.0; QString minPgmSess;
  double minAux       = 0.0; QString minAuxSess;
  double minDelay     = 0.0; QString minDelaySess;
  double minQueue     = 0.0; QString minQueueSess;
  int    minNumPid    = 0;   QString minNumPidSess;
  int    minNumSubPid = 0;   QString minNumSubPidSess;
  int    minNumOk     = 0;   QString minNumOkSess;
  int    minNumError  = 0;   QString minNumErrorSess;
  int    minNumRerun  = 0;   QString minNumRerunSess;

  QDateTime startTime;
  QDateTime endTime;

  for (unsigned jj = 0; jj < numSess; jj++) {
    t_session* session = _sessionList.at(jj);
    QString sessStr = session->currYear()
                    + session->currDayyear() + session->sessChar();

    if (jj == 0 || session->statStartTime() < startTime) {
      startTime = session->statStartTime();
    }
    if (jj == 0 || session->statEndTime() > endTime) {
      endTime = session->statEndTime();
    }

    totalBpe       += session->statBpe();
    totalCpu       += session->statCpu();
    totalPgm       += session->statPgm();
    totalAux       += session->statAux();
    totalDelay     += session->statDelay();
    totalQueue     += session->statQueue();
    totalNumPid    += session->statNumPid();
    totalNumSubPid += session->statNumSubPid();
    totalNumOk     += session->statNumOk();
    totalNumError  += session->statNumError();
    totalNumRerun  += session->statNumRerun();

    if (jj == 0 || maxBpe < session->statBpe()) {
      maxBpe     = session->statBpe();
      maxBpeSess = sessStr;
    }
    if (jj == 0 || maxCpu < session->statCpu()) {
      maxCpu     = session->statCpu();
      maxCpuSess = sessStr;
    }
    if (jj == 0 || maxPgm < session->statPgm()) {
      maxPgm     = session->statPgm();
      maxPgmSess = sessStr;
    }
    if (jj == 0 || maxAux < session->statAux()) {
      maxAux     = session->statAux();
      maxAuxSess = sessStr;
    }
    if (jj == 0 || maxDelay < session->statDelay()) {
      maxDelay     = session->statDelay();
      maxDelaySess = sessStr;
    }
    if (jj == 0 || maxQueue < session->statQueue()) {
      maxQueue     = session->statQueue();
      maxQueueSess = sessStr;
    }
    if (jj == 0 || maxNumPid < session->statNumPid()) {
      maxNumPid     = session->statNumPid();
      maxNumPidSess = sessStr;
    }
    if (jj == 0 || maxNumSubPid < session->statNumSubPid()) {
      maxNumSubPid     = session->statNumSubPid();
      maxNumSubPidSess = sessStr;
    }
    if (jj == 0 || maxNumOk < session->statNumOk()) {
      maxNumOk     = session->statNumOk();
      maxNumOkSess = sessStr;
    }
    if (jj == 0 || maxNumError < session->statNumError()) {
      maxNumError     = session->statNumError();
      maxNumErrorSess = sessStr;
    }
    if (jj == 0 || maxNumRerun < session->statNumRerun()) {
      maxNumRerun     = session->statNumRerun();
      maxNumRerunSess = sessStr;
    }

    if (jj == 0 || minBpe > session->statBpe()) {
      minBpe     = session->statBpe();
      minBpeSess = sessStr;
    }
    if (jj == 0 || minCpu > session->statCpu()) {
      minCpu     = session->statCpu();
      minCpuSess = sessStr;
    }
    if (jj == 0 || minPgm > session->statPgm()) {
      minPgm     = session->statPgm();
      minPgmSess = sessStr;
    }
    if (jj == 0 || minAux > session->statAux()) {
      minAux     = session->statAux();
      minAuxSess = sessStr;
    }
    if (jj == 0 || minDelay > session->statDelay()) {
      minDelay     = session->statDelay();
      minDelaySess = sessStr;
    }
    if (jj == 0 || minQueue > session->statQueue()) {
      minQueue     = session->statQueue();
      minQueueSess = sessStr;
    }
    if (jj == 0 || minNumPid > session->statNumPid()) {
      minNumPid     = session->statNumPid();
      minNumPidSess = sessStr;
    }
    if (jj == 0 || minNumSubPid > session->statNumSubPid()) {
      minNumSubPid     = session->statNumSubPid();
      minNumSubPidSess = sessStr;
    }
    if (jj == 0 || minNumOk > session->statNumOk()) {
      minNumOk     = session->statNumOk();
      minNumOkSess = sessStr;
    }
    if (jj == 0 || minNumError > session->statNumError()) {
      minNumError     = session->statNumError();
      minNumErrorSess = sessStr;
    }
    if (jj == 0 || minNumRerun > session->statNumRerun()) {
      minNumRerun     = session->statNumRerun();
      minNumRerunSess = sessStr;
    }
  }

  msg += QString().sprintf("Total    %7d %7d %7d %7d %7d  %6d   "
                           "%6d  %6d  %6d  %6d  %6d",
                           int(totalBpe), int(totalCpu), int(totalPgm),
                           int(totalAux), int(totalDelay), int(totalQueue),
                           totalNumPid, totalNumSubPid, totalNumOk,
                           totalNumError, totalNumRerun);

  int totalTime = startTime.secsTo(endTime);
  QTime duration = QTime(0,0,0).addSecs(totalTime);

  msg += startTime.toString("  dd-MMM-yyyy hh:mm:ss")
       + endTime.toString("  dd-MMM-yyyy hh:mm:ss")
       + duration.toString("  HH:mm:ss\n");

  msg += "-------------------------------------------------------------------------------------------------------------------------------------------------------\n";

  double meanBpe       = totalBpe       / numSess;
  double meanCpu       = totalCpu       / numSess;
  double meanPgm       = totalPgm       / numSess;
  double meanAux       = totalAux       / numSess;
  double meanDelay     = totalDelay     / numSess;
  double meanQueue     = totalQueue     / numSess;
  int    meanNumPid    = totalNumPid    / numSess;
  int    meanNumSubPid = totalNumSubPid / numSess;
  int    meanNumOk     = totalNumOk     / numSess;
  int    meanNumError  = totalNumError  / numSess;
  int    meanNumRerun  = totalNumRerun  / numSess;

  double stdBpe   = 0.0;
  double stdCpu   = 0.0;
  double stdPgm   = 0.0;
  double stdAux   = 0.0;
  double stdDelay = 0.0;
  double stdQueue = 0.0;

  for (unsigned jj = 0; jj < numSess; jj++) {
    t_session* session = _sessionList.at(jj);
    stdBpe   += (session->statBpe()   - meanBpe  ) * (session->statBpe()   - meanBpe  );
    stdCpu   += (session->statCpu()   - meanCpu  ) * (session->statCpu()   - meanCpu  );
    stdPgm   += (session->statPgm()   - meanPgm  ) * (session->statPgm()   - meanPgm  );
    stdAux   += (session->statAux()   - meanAux  ) * (session->statAux()   - meanAux  );
    stdDelay += (session->statDelay() - meanDelay) * (session->statDelay() - meanDelay);
    stdQueue += (session->statQueue() - meanQueue) * (session->statQueue() - meanQueue);
  }
  stdBpe   = sqrt(stdBpe/numSess);
  stdCpu   = sqrt(stdCpu/numSess);
  stdPgm   = sqrt(stdPgm/numSess);
  stdAux   = sqrt(stdAux/numSess);
  stdDelay = sqrt(stdDelay/numSess);
  stdQueue = sqrt(stdQueue/numSess);

  msg += QString().sprintf("Mean     %7d %7d %7d %7d %7d  %6d   "
                           "%6d  %6d  %6d  %6d  %6d\n",
                           int(meanBpe), int(meanCpu), int(meanPgm),
                           int(meanAux), int(meanDelay), int(meanQueue),
                           meanNumPid, meanNumSubPid, meanNumOk,
                           meanNumError, meanNumRerun);

  msg += QString().sprintf("Std dev  %7d %7d %7d %7d %7d  %6d\n",
                           int(stdBpe), int(stdCpu), int(stdPgm),
                           int(stdAux), int(stdDelay), int(stdQueue));

  msg += "-------------------------------------------------------------------------------------------------------------------------------------------------------\n";

  msg += QString().sprintf("Max      %7d %7d %7d %7d %7d  %6d   "
                           "%6d  %6d  %6d  %6d  %6d\n",
                           int(maxBpe), int(maxCpu), int(maxPgm),
                           int(maxAux), int(maxDelay), int(maxQueue),
                           maxNumPid, maxNumSubPid, maxNumOk,
                           maxNumError, maxNumRerun);
  msg += "Session   "
       + maxBpeSess      + "  " + maxCpuSess       + "  " + maxPgmSess   + "  "
       + maxAuxSess      + "  " + maxDelaySess     + "  " + maxQueueSess + "   "
       + maxNumPidSess   + "  " + maxNumSubPidSess + "  " + maxNumOkSess + "  "
       + maxNumErrorSess + "  " + maxNumOkSess     + "\n";

  msg += "-------------------------------------------------------------------------------------------------------------------------------------------------------\n";

  msg += QString().sprintf("Min      %7d %7d %7d %7d %7d  %6d   "
                           "%6d  %6d  %6d  %6d  %6d\n",
                           int(minBpe), int(minCpu), int(minPgm),
                           int(minAux), int(minDelay), int(minQueue),
                           minNumPid, minNumSubPid, minNumOk,
                           minNumError, minNumRerun);

  msg += "Session   "
       + minBpeSess      + "  " + minCpuSess       + "  " + minPgmSess   + "  "
       + minAuxSess      + "  " + minDelaySess     + "  " + minQueueSess + "   "
       + minNumPidSess   + "  " + minNumSubPidSess + "  " + minNumOkSess + "  "
       + minNumErrorSess + "  " + minNumOkSess     + "\n";

  msg += "-------------------------------------------------------------------------------------------------------------------------------------------------------\n";

  return msg;
}
