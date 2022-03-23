
/* -------------------------------------------------------------------------
 * Bernese GPS Software Version 5.0
 * -------------------------------------------------------------------------
 *
 * Class:      t_server
 *
 * Purpose:    This class implements a BPE server
 *
 * Author:     L. Mervart
 *
 * Created:    28-OCT-2000
 *
 * Changes:
 *
 * Copyright:  Astronomical Institute
 *              University of Berne
 *                  Switzerland
 * -----------------------------------------------------------------------*/

#include <stdlib.h>
#include <qstring.h>
#include <qlabel.h>
#include <qregexp.h>

#include "server.h"
#include "initmenu.h"
#include "errormsg.h"
#include "juldat.h"
#include "menutils.h"
#include "mainwin.h"
#include "session.h"
#include "r_file.h"
#include "r_dir.h"
#include "bpe.h"

// Constructor
////////////////////////////////////////////////////////////////////////////
t_server::t_server(t_bpe* bpe, const QString& inpFileName,
                   t_cpufile* cpufile, t_session* session,
                   t_bpelog* bpelog, bool superBPE, bool firstSession,
                   int numSessions, bool waitForHeader, bool reprMode) :
Q3ServerSocket( (Q_UINT16) 0, (int) 30, (QObject*) 0, (const char*) 0) {

  _bpe              = bpe;
  _superBPE         = superBPE;
  _firstSession     = firstSession;
  _startTailCounter = 0;
  _numSessions      = numSessions;
  _waitForHeader    = waitForHeader;
  _inpfile          = new t_bpeinp(inpFileName, _superBPE);
  _bpelog           = bpelog;
  _pcffile          = 0;
  _cpufile          = cpufile;
  _scriptList       = 0;
  _session          = session;
  _status           = waiting;
  _reprMode         = reprMode;
  _timer            = new QTimer(this);
  connect(_timer, SIGNAL(timeout()), this, SLOT(slotStartScripts()));

  if (readOptions() == 0) {
    _bpelog->message(this, QString().sprintf("Server started at %d", port()),
                     t_bpelog::msg);

    // CPU update rate (milliseconds)
    // ------------------------------
    _cpuUpdRate = abs( _inpfile->getKeySel0("CPUUPDRATE").toInt() * 1000 );
    if (_cpuUpdRate == 0) {
      _cpuUpdRate = 300000;  // 5 minutes
    }

    // Set _debug variable
    // -------------------
    if (_inpfile->getKeySel0("DEBUG") == "1") {
      _debug = true;
    }
    else {
      _debug = false;
    }

  }
}

// Destructor
////////////////////////////////////////////////////////////////////////////
t_server::~t_server() {
  delete _inpfile;
  if (_pcffile) {
    r_file hlpFile(_pcffile->getName());
    hlpFile.remove();
    delete _pcffile;
  }
  if (_superBPE) {
    delete _cpufile;
  }
  delete _scriptList;
  delete _timer;
}

// Read all Options
////////////////////////////////////////////////////////////////////////////
int t_server::readOptions() {

  _inpfile->expandSelList();

  // PCF File
  // --------
  QString PCFname     = _inpfile->getKeySel0("PCF_FILE");
  QString PCFskeleton = t_mainwin::convPCF(&PCFname, true);

  if ( PCFskeleton.isEmpty() ) {
    close(error, false);
    return 1;
  }

  _pcffile = new t_pcffile(PCFskeleton);

  // CPU File
  // --------
  if (_superBPE) {
    _cpufile = new t_cpufile( _inpfile->getKeySel0("CPU_FILE") );
    if (!_cpufile->ok()) {
      close(error);
      return 1;
    }
  }

  // Read and store all server options
  // ---------------------------------
  getServerOptions();

  return 0;
}

// Start the Session
////////////////////////////////////////////////////////////////////////////
void t_server::start() {

  _status = running;

  _session->rememberStartTime();

  QString startScriptOpt = _inpfile->getKeySel0("SCRIPT_START");
  int index = startScriptOpt.find(' ');
  if (index !=  -1) {
    startScriptOpt = startScriptOpt.left(index);
  }
  QString startScript;

  // Fill the List of Scripts
  // ------------------------
  delete _scriptList;
  _scriptList = new Q3PtrList<t_script>();
  _scriptList->setAutoDelete(true);

  for (int ii = 0; ii < _pcffile->getNumberOfScripts(); ii++) {
    QMap<QString, QString> sOptions = _sOptions;
    bool specialCamp;
    if (_pcffile->addToOptions(sOptions, ii, specialCamp)) {
      close(error);
      return;
    }

    // Create a new script
    // -------------------
    _scriptList->append( new t_script(this, sOptions, port(), _bpelog, _reprMode) );
    _scriptList->last()->setSpecialCamp(specialCamp);
    connect( _scriptList->last(), SIGNAL(scriptFinished(const QString&)),
             this, SLOT(slotScriptFinished(const QString&)) );

    if (_scriptList->last()->pid() == startScriptOpt) {
      startScript = _scriptList->last()->pid();
    }
  }

  // Start with given Script
  // -----------------------
  if (!startScript.isEmpty()) {
    for (unsigned jj = 0; jj < _scriptList->count(); jj++) {
      if (_scriptList->at(jj)->pid() == startScript) {
        break;
      }
      _scriptList->at(jj)->setSkipped();
    }
  }

  // Skip given Scripts
  // ------------------
  t_keyword* skipKey = _inpfile->getKey("SCRIPT_SKIP");
  if (skipKey != 0) {
    QStringList skipScripts = skipKey->getSelList();
    for (int ii = 0; ii < skipScripts.count(); ii++) {
      int index = skipScripts[ii].find(' ');
      if (index !=  -1) {
        skipScripts[ii] = skipScripts[ii].left(index);
      }
      for (unsigned jj = 0; jj < _scriptList->count(); jj++) {
        if (_scriptList->at(jj)->pid() == skipScripts[ii]) {
          _scriptList->at(jj)->setSkipped();
          _scriptList->at(jj)->setKey("SKIP", "TRUE");
        }
      }
    }
  }

  // Check the options
  // -----------------
  int irc = 0;
  for (unsigned jj = 0; jj < _scriptList->count(); jj++) {
    irc += _scriptList->at(jj)->checkOptions(_scriptList);

    /////    _scriptList->at(jj)->removePrtFile();
    if (jj == 0) {
      QString taskid     = _scriptList->at(jj)->getKey("TASKID");
      QString year       = _scriptList->at(jj)->getKey("YEAR");
      QString session    = _scriptList->at(jj)->getKey("SESSION");
      QString pth_bpeprt = _scriptList->at(jj)->getKey("PTH_BPEPRT");
      QString pth_bpelog = _scriptList->at(jj)->getKey("PTH_BPELOG");
      QString ext_bpeprt = _scriptList->at(jj)->getKey("EXT_BPEPRT");
      QString ext_bpelog = _scriptList->at(jj)->getKey("EXT_BPELOG");
      removeAllPrtFiles(taskid, year, session, pth_bpeprt, pth_bpelog,
                        ext_bpeprt, ext_bpelog);
    }
  }

  // Start the BPE
  // -------------
  if (irc == 0) {
    slotStartScripts();
  }
  else {
    close(error);
  }
}

// Try to start as many Scripts as possible
////////////////////////////////////////////////////////////////////////////
void t_server::slotStartScripts() {

  if (_timer->isActive()) {
    _timer->stop();
  }

  // If error, do not start any more scripts, try to close the session
  // -----------------------------------------------------------------
  if (_status == error) {
    close(error);
    return;
  }

  _cpufile->readFile();

  // Check for Suspended PCF
  // -----------------------
  if ( _cpufile->suspended(pcfName(false)) ) {
    _bpelog->message(this, "Suspended", t_bpelog::msg);
    _timer->start(_cpuUpdRate);
    return;
  }

  unsigned noCPU = 0;
  QString  lastNotFree;
  QString  lastPidAllowedStarted;

  for (int iScript = 0; iScript < (int) _scriptList->count(); iScript++) {

    t_script* script = _scriptList->at(iScript);

    QString reqCpuName = script->getKey("REQ_CPU");

    bool turboStart = false;
    QString currentPid = script->pid();
    if (currentPid == lastPidAllowedStarted &&
        script->subPid().toInt() > 1) {
      turboStart = true;

      if (reqCpuName == lastNotFree) {
        continue;
      }
    }

    if ( turboStart || canBeStarted(iScript) ) {

      if (!turboStart) {
        lastPidAllowedStarted = currentPid;
      }

      if (script->isSkipped()) {
        _bpelog->message(script, "Script skipped",
                         t_bpelog::msg);
        script->setFinished();

        // It can happen that all scripts are already finished
        // ---------------------------------------------------
        bool allFinished = true;
        for (int ii = 0; ii < (int) _scriptList->count(); ii++) {
          if ( !_scriptList->at(ii)->isFinished() ) {
            allFinished = false;
          }
        }
        if (allFinished) {
          close(finished);
          return;
        }

        continue;
      }

      bool start = true;
      if ( script->isSingleton() ){
        if (!_firstSession) {
          if (iScript != 0) {
            close(finished);
            return;
          }
          else {
            if (_waitForHeader) {
              start = false;
            }
            else {
              script->setFinished();
              slotStartScripts();
              return;
            }
          }
        }
        else {
          if (iScript != 0 && _waitForHeader &&
              _startTailCounter < _numSessions-1) {
            start = false;
          }
        }
      }
      if (start) {
        QString cpuNickname;
        QString cpuCommand;
        int     maxWait;
        if (reqCpuName != lastNotFree && _cpufile->findFreeCPU(script->idStr(),
                              reqCpuName, cpuNickname, cpuCommand, maxWait) ) {
          if (script->startClient(cpuNickname, cpuCommand, maxWait)) {
            _cpufile->freeCPU(script->idStr(), script->getCPUnickname() );
          }
        }
        else {
          lastNotFree = reqCpuName;
          ++noCPU;
          _bpelog->message(script, "Waiting for CPU: " +
                           reqCpuName, t_bpelog::msg );
        }
      }
    }
  }

  ///  if (noCPU > 0) {
  _timer->start(_cpuUpdRate); // because of check for killed scripts (.JOB file)
  ///  }
}

// Decide if the script can be started or if it has to wait
////////////////////////////////////////////////////////////////////////////
bool t_server::canBeStarted(int iScript) {

  t_script* script = _scriptList->at(iScript);

  if (! script->canBeStarted()) {
    return false;
  }
  QStringList waitingList = QStringList::split( QRegExp("\\s"),
                               script->getKey("WAIT_FOR") );

  // Additional condition if jump backwards
  // --------------------------------------
  QStringList jumpList = script->jumps();
  for (int jj = 0; jj < jumpList.count(); jj++) {
    int ind = getScriptIndex(jumpList[jj], "");
    if (ind != -1 && ind < iScript) {
      for (int kk = ind; kk < iScript; kk++) {
        waitingList.append( _scriptList->at(kk)->pid() );
      }
    }
  }

  // Decide if all required scripts are finished
  // -------------------------------------------
  for (int ii = 0; ii < (int) _scriptList->count(); ii++) {
    if ( waitingList.findIndex(_scriptList->at(ii)->getKey("PID")) != -1 ) {
      if ( !_scriptList->at(ii)->isFinished() ) {
        return false;
      }
      if (_scriptList->at(ii)->isSkipped() && !canBeStarted(ii)) {
        return false;
      }
    }
  }
  return true;
}

// Handling new connection
////////////////////////////////////////////////////////////////////////////
void t_server::newConnection(int socketNumber) {

  static const int MAX_SOCKET_FD = 64;

  if (socketNumber >= MAX_SOCKET_FD - 1) {
    QString msg;
    msg.sprintf("Warning: socket fd reaches %d. "
                "This may cause problems on some platforms "
                "Please, check the local socket descriptor limit "
                "and set the CPU-file accordingly", socketNumber);
    _bpelog->message(this, msg, t_bpelog::msg);
  }

  Q3Socket* newSocket = new Q3Socket(0);

  newSocket->setSocket(socketNumber);

  // Wait for handshaking
  // --------------------
  const int maxDelay = 30000;
  const int waitTime = 100;
  int       delay    = 0;
  while (!newSocket->canReadLine()) {
    newSocket->waitForMore(waitTime);
    delay += waitTime;
    if (delay > maxDelay) {
      errormsg("Too long delay");
      return;
    }
  }
  QString inMsg( newSocket->readLine() );

  // Connect the new socket to the appropriate script
  // ------------------------------------------------
  int iScript = getScriptIndex( inMsg );
  if (iScript != -1) {
    _scriptList->at(iScript)->setSocket(newSocket);
  }
}

// Finishing one Script
////////////////////////////////////////////////////////////////////////////
void t_server::slotScriptFinished(const QString& inMsg) {

  int iScript      = getScriptIndex( inMsg );
  t_script* script = _scriptList->at(iScript);

  script->setFinished();

  _cpufile->freeCPU(script->idStr(), script->getCPUnickname() );

  // Check the status of the script
  // ------------------------------
  bool errFlg = getKeyValFromMsg(inMsg, "STATUS").toInt() == 0 ? false : true;

  bool cont_err = false;
  if (errFlg && script->cont_err()) {
    errFlg = false;
    cont_err = true;
  }

  // Handle the Singleton Finish
  // ---------------------------
  if (script->isSingleton()) {
    if (errFlg) {
      for (_scriptList->first(); _scriptList->current(); _scriptList->next()) {
        _scriptList->current()->setFinished();
        _cpufile->freeCPU(_scriptList->current()->idStr(),
                          _scriptList->current()->getCPUnickname());
      }
    }
    emit singletonFinished(inMsg);
  }

  // Remember CPU Usage of the script
  // --------------------------------
  QString taskid;
  _session->updateTimeStatistics(script, errFlg, taskid);
  _bpe->updateTimeStatistics(script, errFlg, taskid);
  _cpufile->updateTimeStatistics(script, errFlg, taskid);

  // Check the status of the script
  // ------------------------------
  if (errFlg) {
    _bpelog->message(script, "Script finished  ERROR",
                     t_bpelog::msg);
    close(error);
    return;
  }
  else {

    // Check Maximum BPE Time
    // ----------------------
    if (_bpe->timeExpired()) {
      _bpelog->message(script, "Script finished - Time Expired",
                       t_bpelog::msg);
      close(error);
      return;
    }

    // Continue with the next script
    // -----------------------------
    if (cont_err) {
      _bpelog->message(script, "Script finished not OK but continue",
                       t_bpelog::msg);
    }
    else {
      _bpelog->message(script, "Script finished  OK",
                       t_bpelog::msg);
    }
  }

  // If error, do not start any more scripts, try to close the session
  // -----------------------------------------------------------------
  if (_status == error) {
    close(error);
    return;
  }

  // Jump ?
  // ------
  QStringList msgList = QStringList::split(QRegExp(" & ?"),
                             getKeyValFromMsg(inMsg,"PRT_FILE_CONTENT"));
  QString jumpPid = script->jump(msgList);

  if ( !jumpPid.isEmpty() ) {
    int jumpIndex = getScriptIndex(jumpPid, "");
    if (jumpIndex == -1) {
      _bpelog->message(this, "Cannot jump to " + jumpPid, t_bpelog::error);
      close(error);
      return;
    }
    if (jumpIndex <= iScript) {
      _bpelog->message(this, "Jump backwards to " + jumpPid, t_bpelog::msg);
      for (int ii = jumpIndex; ii <= iScript; ii++) {
        _scriptList->at(ii)->resetStatus();
      }
    }
    else {
      _bpelog->message(this, "Jump forwards to " + jumpPid, t_bpelog::msg);
      for (int ii = iScript + 1; ii < jumpIndex; ii++) {
        _scriptList->at(ii)->setSkipped();
      }
    }
  }

  // Handle Parallel Scripts
  // -----------------------
  QString slavePID = script->getKey("SLAVE");
  if (!slavePID.isEmpty()) {
    _bpelog->message(this, "Initialize parallel script " + slavePID,
                     t_bpelog::msg);
    initParallel(slavePID, inMsg);
  }

  QString sub_pid = script->getKey("SUB_PID");
  if      ( sub_pid == "001") {
    script->setKey("SUB_PID", "000");
  }
  else if ( sub_pid != "000") {
    _scriptList->remove(iScript);
  }

  // Test if all Scripts finished
  // ----------------------------
  for (int ii = 0; ii < (int) _scriptList->count(); ii++) {
    if ( !_scriptList->at(ii)->isFinished() ) {
      slotStartScripts();
      return;
    }
  }
  close(finished);
}

// Put together all server-specific Options
////////////////////////////////////////////////////////////////////////////
void t_server::getServerOptions() {

  _sOptions.insert("BPE_SERVER_HOST", expandEnvVar("${BPE_SERVER_HOST}"));
  _sOptions.insert("BPE_CLIENT"     , _inpfile->getKeySel0("BPE_CLIENT"));
  _sOptions.insert("PORT"           , QString().sprintf("%d", port()));
  _sOptions.insert("CLIENT_ENV"     , _inpfile->getKeySel0("CLIENT_ENV"));
  _sOptions.insert("TASKID",   stripPath(_inpfile->getKeySel0("TASKID")));
  _sOptions.insert("DEBUG"          , _inpfile->getKeySel0("DEBUG"));
  _sOptions.insert("NOCLEAN",         _inpfile->getKeySel0("NOCLEAN"));
  _sOptions.insert("PCFFIL"         , pcfName(false));
  _sOptions.insert("CPUFIL"         , _cpufile->getName());

  _sOptions.insert("SESSION_TABLE",
                   stripPath(initmenu.getKeySel0("SESSION_TABLE")));

  // Set Time Variables into Message
  // -------------------------------
  timeVarsIntoOpt(  0);

  // Campaign and Campaign Path
  // --------------------------
  QString campName;
  QString campPath;
  if (!_reprMode) {
    QString bpeCamp  = _inpfile->getKeySel0("BPE_CAMPAIGN");
    campName = stripPath(bpeCamp);
    campPath = stripFileName(bpeCamp) + r_dir::separator();
  }
  else {
    campName = _inpfile->getKeySel0("REPR_MODE_CAMPAIGN") +
               _sOptions["YEAR"] + _sOptions["SESSION"];
    campPath = _inpfile->getKeySel0("REPR_MODE_PATH");
    t_mainwin::newCamp(_bpe->inpFileNameNewCamp(), campPath + campName);
  }

  _sOptions.insert( "CAMPAIGN", campName);
  _sOptions.insert( "CAMP_PTH", expandEnvVar(campPath));
  _sOptions.insert( "CAMP_DRV", campPath);

  // Path to User-Scripts and Options
  // --------------------------------
  _sOptions.insert( "S_PTH_SCRIPT",
                           expandEnvVar(initmenu.getKeySel0("PTH_SCRIPT")) );
  _sOptions.insert( "S_PTH_OPT",
                           expandEnvVar(initmenu.getKeySel0("PTH_OPT")) );

  // User Name
  // ---------
  _sOptions.insert( "USER", expandEnvVar("${USER}"));

  // Variables defined in BPE Inputfile
  // ----------------------------------
  t_keyword* keyVar = _inpfile->getKey("SERVER_VARIABLES");
  if (keyVar != 0) {
    QString server_variables;
    for (int ii = 0; ii < (int) keyVar->getValList().count(); ii++) {
      QStringList hlpList;
      unilineSplit(keyVar->getValList()[ii], hlpList);
      if (hlpList.count() > 1 && !hlpList[0].isEmpty()) {
        _sOptions.insert( hlpList[0], hlpList[1] );
        if (hlpList[0].find("V_") == 0) {
          hlpList[0] = hlpList[0].mid(2);
        }
        server_variables += "\"" + hlpList[0] + "\" \"" + hlpList[1] + "\" ";
      }
    }
    if (!server_variables.isEmpty()) {
      _sOptions.insert("SERVER_VARIABLES", server_variables);
    }
  }

  // List of All Sessions for the crazy Super-BPE
  // --------------------------------------------
  if (_superBPE) {
    int numSess = _inpfile->getKeySel0("NUM_SESS").toInt();
    int moduloSess = _inpfile->getKeySel0("MODULO_SESS").toInt();
    if (moduloSess == 0) moduloSess = 1;
    Q3PtrList<t_session> sessionList;
    sessionList.setAutoDelete(true);
    for (int ii = 0; ii < abs(numSess); ii += moduloSess) {
      int offset = ii;
      if (numSess < 0) {
        offset = -ii;
      }
      sessionList.append( new t_session( _inpfile->getKeySel0("YEAR"),
                                  _inpfile->getKeySel0("SESSION"), offset) );
    }
    QString hlpStr;
    for (unsigned jj = 0; jj < sessionList.count(); jj++) {
      if (jj > 0) {
        hlpStr += " & ";
      }
      hlpStr += _inpfile->getName()            + " " +
                sessionList.at(jj)->currMJD()  + " " +
                sessionList.at(jj)->sessChar() + " " +
                expandEnvVar( initmenu.getKeySel0("SESSION_TABLE") );
    }
    _sOptions.insert("SBPE_OPTIONS", hlpStr);
  }
}

// Find the Index of the Script
////////////////////////////////////////////////////////////////////////////
int t_server::getScriptIndex( const QString& PID, const QString& SUB_PID ) {
  for (int ii = 0; ii < (int) _scriptList->count(); ii++) {
    if ( _scriptList->at(ii)->getKey("PID")    == PID    &&
         (SUB_PID.isEmpty() ||
          _scriptList->at(ii)->getKey("SUB_PID") == SUB_PID) ) {
      return ii;
    }
  }
  return -1;
}

// Find the Index of the Script (different Version)
////////////////////////////////////////////////////////////////////////////
int t_server::getScriptIndex( const QString& inMsg ) {
  int iScript = getScriptIndex( getKeyValFromMsg(inMsg, "PID"),
                                getKeyValFromMsg(inMsg, "SUB_PID") );
  if (iScript == -1) {
    errormsg("Cannot Find Index of Script: \n" + inMsg);
  }
  return iScript;
}

// Initialize Parallel Scripts
////////////////////////////////////////////////////////////////////////////
void t_server::initParallel(const QString& slavePID, const QString& inMsg) {

  int i0 = getScriptIndex(slavePID, "000");
  if (i0 == -1) {
    errormsg("Wrong Parallel Options");
    return;
  }

  QString     parStr = getKeyValFromMsg(inMsg, "CONTROL_FILE_CONTENT");
  QStringList PARAMs = QStringList::split(QRegExp("\\s*&\\s*"), parStr);

  if ( PARAMs.count() < 1) {
//    errormsg("Empty list of parallel scripts");
    t_script* script = _scriptList->at(i0);
    _bpelog->message(script, "Empty list of parallel scripts", t_bpelog::msg);
    _scriptList->at(i0)->setFinished();
    return;
  }

  _scriptList->at(i0)->setKey("CONTROL_FILE_LINE", PARAMs[0]);
  _scriptList->at(i0)->setKey("SUB_PID", "001");

  int startCount = _scriptList->at(i0)->startCount();

  bool specialCamp = _scriptList->at(i0)->specialCamp();

  if ( !_scriptList->at(i0)->isSkipped() ) {
    for (int ii = 1; ii < (int) PARAMs.count(); ii++) {
      _scriptList->append( new t_script(this,_scriptList->at(i0)->getOptions(),
                                        port(), _bpelog, _reprMode) );
      _scriptList->last()->setSpecialCamp(specialCamp);
      connect( _scriptList->last(), SIGNAL(scriptFinished(const QString&)),
               this, SLOT(slotScriptFinished(const QString&)) );
      _scriptList->last()->setKey("CONTROL_FILE_LINE", PARAMs[ii]);
      QString SUB_PID; SUB_PID.sprintf("%3.3d", ii+1);
      _scriptList->last()->setKey("SUB_PID", SUB_PID);
      _scriptList->last()->setStartCount(startCount);
    }
  }
}

// Set Time Variables into Options
////////////////////////////////////////////////////////////////////////////
void t_server::timeVarsIntoOpt(int offset) {

  QString strYear   ;
  QString strYr_4   ;
  QString strSession;
  QString strDayyear;
  QString strDay    ;
  QString strMonth  ;
  QString strGPSweek;
  QString strDayweek;
  QString strSessID ;
  QString strMJD    ;

  if (offset < 0) {
    strYear.sprintf(   "YEARM%d", -offset);
    strYr_4.sprintf(   "YR_4M%d", -offset);
    strSession.sprintf("SESSM%d", -offset);
    strDayyear.sprintf("DAYYM%d", -offset);
    strDay.sprintf(    "DAYMM%d", -offset);
    strMonth.sprintf(  "MONTM%d", -offset);
    strGPSweek.sprintf("GPSWM%d", -offset);
    strDayweek.sprintf("DAYWM%d", -offset);
    strSessID.sprintf( "SSIDM%d", -offset);
    strMJD.sprintf(    "MJDM%d",  -offset);
  }
  else if (offset == 0) {
    strYear    = "YEAR"   ;
    strYr_4    = "YR_4"   ;
    strSession = "SESSION";
    strDayyear = "DAYYEAR";
    strDay     = "DAY"    ;
    strMonth   = "MONTH"  ;
    strGPSweek = "GPSWEEK";
    strDayweek = "DAYWEEK";
    strSessID  = "SESSID" ;
    strMJD     = "MJD"    ;
  }
  else {
    strYear.sprintf(   "YEARP%d", offset);
    strYr_4.sprintf(   "YR_4P%d", offset);
    strSession.sprintf("SESSP%d", offset);
    strDayyear.sprintf("DAYYP%d", offset);
    strDay.sprintf(    "DAYMP%d", offset);
    strMonth.sprintf(  "MONTP%d", offset);
    strGPSweek.sprintf("GPSWP%d", offset);
    strDayweek.sprintf("DAYWP%d", offset);
    strSessID.sprintf( "SSIDP%d", offset);
    strMJD.sprintf(    "MJDP%d",  offset);
  }

  _sOptions.insert(strYear   , _session->currYear(offset)   );
  _sOptions.insert(strYr_4   , _session->currYr_4(offset)   );
  _sOptions.insert(strSession, _session->currSession(offset));
  _sOptions.insert(strDayyear, _session->currDayyear(offset));
  _sOptions.insert(strDay    , _session->currDay(offset)    );
  _sOptions.insert(strMonth  , _session->currMonth(offset)  );
  _sOptions.insert(strGPSweek, _session->currGPSweek(offset));
  _sOptions.insert(strDayweek, _session->currDayweek(offset));
  _sOptions.insert(strSessID,  _session->sessChar()         );
  _sOptions.insert(strMJD    , _session->currMJD(offset)    );
}

// Close the server
////////////////////////////////////////////////////////////////////////////
void t_server::close(t_status status, bool useBPElog) {

  if (_status != error) {  // error status must remain
    _status = status;
  }

  // If error, wait for running scripts
  // ----------------------------------
  if (status == error && _scriptList) {
    for (unsigned jj = 0; jj < _scriptList->count(); jj++) {
      if (_scriptList->at(jj)->isRunning()) {
        return;
      }
    }
  }

  if (_status != error) {
    if (useBPElog) {
      _bpelog->message(this, "Session finished OK", t_bpelog::msg);
    }
  }
  else {
    if (useBPElog) {
      _bpelog->message(this, "Session finished ERROR", t_bpelog::msg);
    }
  }

  if (_reprMode) {
    QString copyDest;
    bool    remove = false;
    if (_status == error) {
      if      (_inpfile->getKeySel0("REPR_MODE_ON_ERROR") == "remove") {
        remove = true;
      }
      else if (_inpfile->getKeySel0("REPR_MODE_ON_ERROR") == "copy") {
        remove = true;
        copyDest = _inpfile->getKeySel0("REPR_MODE_ON_ERROR_PATH");
      }
    }
    else {
      if      (_inpfile->getKeySel0("REPR_MODE_ON_SUCCESS") == "remove") {
        remove = true;
      }
      else if (_inpfile->getKeySel0("REPR_MODE_ON_SUCCESS") == "copy") {
        remove = true;
        copyDest = _inpfile->getKeySel0("REPR_MODE_ON_SUCCESS_PATH");
      }
    }
    QString sourcePath = _sOptions["CAMP_PTH"] + _sOptions["CAMPAIGN"];
    if (!copyDest.isEmpty()) {
      copyDest = copyDest + r_dir::separator() + _sOptions["CAMPAIGN"];
      r_dir::copyDir(expandEnvVar(sourcePath), expandEnvVar(copyDest));
    }
    if (remove) {
      r_dir::removeDir(expandEnvVar(sourcePath));
    }
  }

  delete _inpfile;    _inpfile    = 0;
  if (_pcffile) {
    r_file hlpFile(_pcffile->getName());
    hlpFile.remove();
    delete _pcffile;    _pcffile    = 0;
  }
  if (_superBPE) {
    delete _cpufile; _cpufile    = 0;
  }
  delete _scriptList; _scriptList = 0;

  _session->rememberEndTime();

  emit serverFinished(_session);
}

//
////////////////////////////////////////////////////////////////////////////
void t_server::slotStartTail() {
  t_server* senderServer = (t_server*) sender();
  if (senderServer->status() == t_server::error) {
    for (_scriptList->first(); _scriptList->current(); _scriptList->next()) {
      t_script* script = _scriptList->current();
      if (script->isSingleton()) {
        script->setSkipped();
      }
    }
  }

  ++_startTailCounter;
  slotStartScripts();
}

// Remove the Script Output Files (Protocol and Log Files)
////////////////////////////////////////////////////////////////////////////
void t_server::removeAllPrtFiles(const QString& taskid,
                                 const QString& year,
                                 const QString& session,
                                 const QString& pth_bpeprt,
                                 const QString& pth_bpelog,
                                 const QString& ext_bpeprt,
                                 const QString& ext_bpelog) {

  // Protocol Files
  // --------------
  r_dir prtDir(pth_bpeprt);
  if (prtDir.exists()) {
    QString prtFilter = taskid + year + session + "_*." + ext_bpeprt;
    QStringListIterator it(prtDir.entryList(prtFilter, QDir::Files));
    while (it.hasNext()) {
      r_file::remove(prtDir.path() + r_dir::separator() + it.next());
    }
  }

  // Log Files
  // ---------
  r_dir logDir(pth_bpelog);
  if (logDir.exists()) {
    QString logFilter = taskid + year + session + "_*." + ext_bpelog;
    QStringListIterator it(logDir.entryList(logFilter, QDir::Files));
    while (it.hasNext()) {
      r_file::remove(logDir.path() + r_dir::separator() + it.next());
    }
  }
}
