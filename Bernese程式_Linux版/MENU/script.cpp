
/* -------------------------------------------------------------------------
 * Bernese GPS Software Version 5.0
 * -------------------------------------------------------------------------
 *
 * Class:      t_script
 *
 * Purpose:    This class implements a connection to the BPE script
 *
 * Author:     L. Mervart
 *
 * Created:    19-DEC-2000
 *
 * Changes:
 *
 * Copyright:  Astronomical Institute
 *              University of Berne
 *                  Switzerland
 * -----------------------------------------------------------------------*/

#include <stdlib.h>
#include <qstring.h>
#include <qregexp.h>
#include "runpgm.h"

#include "script.h"
#include "menutils.h"
#include "initmenu.h"
#include "server.h"
#include "r_file.h"
#include "r_dir.h"

// Constructor
////////////////////////////////////////////////////////////////////////////
t_script::t_script(t_server* server, const QMap<QString, QString>& options,
                   int port, t_bpelog* bpelog, bool reprMode) : QObject() {
  _server      = server;
  _socket      = 0;
  _options     = options;
  _serverPort  = port;
  _CPUnickname = "";
  _bpelog      = bpelog;
  _reprMode    = reprMode;
  if ( getKey("DEBUG") == "1" ) {
    _debug = true;
  }
  else {
    _debug = false;
  }
  if ( getKey("SINGLETON") == "Y" ) {
    _singleton = true;
  }
  else {
    _singleton = false;
  }
  resetStatus();

  _timer = new QTimer(this);
  connect(_timer, SIGNAL(timeout()), this, SLOT(slotTimeout()));

  _startCount  = 0;
  _specialCamp = false;

  _idStr = 0;

  // Variables for statistics
  // ------------------------
  _rerunCount = 0;
  _timeCpu    = 0.0;
  _timePgm    = 0.0;
  _timeAux    = 0.0;
  _timeDelay  = 0.0;
  _timeQueue  = 0.0;
}

// Destructor
////////////////////////////////////////////////////////////////////////////
t_script::~t_script() {
  if (_socket) {
    _socket->close();
  }
  delete _timer;
}

// Try to Start the Client
////////////////////////////////////////////////////////////////////////////
int t_script::startClient(const QString& cpuNickname,
                          const QString& cpuCommand, int maxWait) {

  _timeStartClient.start();

  setRunning();
  _CPUnickname = cpuNickname;
  _options.insert("CPU", cpuNickname);

  QString argv;
  argv.sprintf("%s %d %s %s", getKey("BPE_SERVER_HOST").ascii(),
               _serverPort, getKey("PID").ascii(), getKey("SUB_PID").ascii());

  QString logFile = getKey("PTH_BPELOG") +
                    getKey("TASKID") + getKey("YEAR") +
                    getKey("SESSION") + "_" + getKey("PID") + "_" +
                    getKey("SUB_PID") + "." + getKey("EXT_BPELOG");

  QString command = cpuCommand;
  command.replace(QRegExp("<COMMAND>"), getKey("BPE_CLIENT"));
  command.replace(QRegExp("<ARGV>"), argv);
  command.replace(QRegExp("<LOG>"), logFile);

  _bpelog->message(this, command, t_bpelog::debug);

  // Start the Timer
  // ---------------
  if (maxWait > 0) {
    _timer->start(maxWait*1000, true);
  }

#ifdef WIN32
  command = command.simplified();
  QStringList hlp = command.split(' ', QString::SkipEmptyParts);
  QStringList args;
  command = hlp[0];
  for (int ii = 1; ii < hlp.size(); ++ii) {
    args << hlp[ii];
  }
  int irc = 0;
  QtConcurrent::run( crepro, command, args.join(" "), true, false, false);
#else
  int irc = crepro(command, "", false, false);
#endif

  if (irc == 0) {
    _bpelog->message(this, "Client started", t_bpelog::msg);
  }
  else {
    _bpelog->message(this, "Client started   ERROR", t_bpelog::msg);
  }

  return irc;
}

// Set the Socket
////////////////////////////////////////////////////////////////////////////
void t_script::setSocket(Q3Socket* newSocket) {

  // Stop the Timer
  // --------------
  if (_timer->isActive()) {
    _timer->stop();
  }

  delete _socket;
  _socket = newSocket;
  connect(_socket, SIGNAL(readyRead()), this, SLOT(slotReadLine()));
  startUserScript();

  _bpelog->message(this, "Script started", t_bpelog::msg);
}

// Start the User Script
////////////////////////////////////////////////////////////////////////////
void t_script::startUserScript() {

  _timeStartScript.start();
  _timeQueue = _timeStartClient.elapsed() / 1e3;

  _options.insert("COMMAND", "START");

  updateScriptAddedVariables();

  QString optStr;
  QMap<QString,QString>::ConstIterator it;
  for (it = _options.begin(); it != _options.end(); ++it ) {
    if (it != _options.begin()) {
      optStr += " && ";
    }
    optStr += it.key() + "=" + it.data();
  }

  ++_startCount;
  optStr += " && STARTCOUNT=" + QString().sprintf("%d", _startCount);

  _socket->writeBlock(optStr+"\n", optStr.length()+1);
  _socket->flush();
  _bpelog->message(this, optStr, t_bpelog::debug);
}

// Try to read one line and respond to it
////////////////////////////////////////////////////////////////////////////
void t_script::slotReadLine() {
  if (_socket->canReadLine()) {
    QString  inMsg = _socket->readLine() ;

    _bpelog->message(this, inMsg.replace(QRegExp("\n"),""), t_bpelog::debug);

    QString setVar = getKeyValFromMsg(inMsg, "SETVAR");
    if (!setVar.isEmpty()) {
      QStringList hlp; unilineSplit(setVar, hlp);
      for (int ii = 0;  ii < hlp.size()-1; ii+=2) {
        QString key   = hlp[ii];
        QString value = hlp[ii+1];
        if (key.length() == 1) {
          key = '$' + key;
        }
        else {
          key = "$(" + key + ')';
        }
        initmenu.addScriptVariable(session(), key, value);
      }
    }

    if (getKeyValFromMsg(inMsg, "MESSAGE") == "FINISHED") {
      stopClient(inMsg);
    }
  }
}

// Stop the Client
////////////////////////////////////////////////////////////////////////////
void t_script::stopClient(const QString& msg) {

  fillTimeStatistics(msg);

  QString  quitMsg = "COMMAND=QUIT\n";
  _socket->writeBlock(quitMsg, quitMsg.length());
  _socket->flush();
  _bpelog->message(this, quitMsg, t_bpelog::debug);
  emit scriptFinished(msg);
}

// Get a Key Value
////////////////////////////////////////////////////////////////////////////
QString t_script::getKey(const QString& key) const {
  if (_options.contains(key)) {
    return _options[key];
  }
  else {
    return "";
  }
}

// Set a Key Value
////////////////////////////////////////////////////////////////////////////
void t_script::setKey(const QString& key, const QString& value) {
  _options.insert(key,value);
}

// Check the input options
////////////////////////////////////////////////////////////////////////////
int t_script::checkOptions(Q3PtrList<t_script>* scriptList) {

  int irc = 0;

  // Check the list of scripts that have to be waiting for
  // -----------------------------------------------------
  QStringList listPIDs = QStringList::split( QRegExp("\\s"),
                                             getKey("WAIT_FOR") );

  for (int ii = 0; ii < listPIDs.count(); ii++) {
    if ( listPIDs[ii] == pid() ) {
      ++irc;
      _bpelog->message(this, "Cannot wait for itself ", t_bpelog::error);
    }
    bool found = false;
    for (unsigned iScript = 0; iScript < scriptList->count(); iScript++) {
      if ( listPIDs[ii] == scriptList->at(iScript)->getKey("PID") ) {
        found = true;
        break;
      }
    }
    if (!found) {
      ++irc;
      _bpelog->message(this, "Cannot wait for script " + listPIDs[ii],
                       t_bpelog::error);
    }
  }

  // Check the list of potential jumps
  // ---------------------------------
  QStringList jumpList = jumps();
  for (int jj = 0; jj < jumpList.count(); jj++) {
    bool found = false;
    for (unsigned iScript = 0; iScript < scriptList->count(); iScript++) {
      if ( jumpList[jj] == scriptList->at(iScript)->getKey("PID") ) {
        found = true;
        break;
      }
    }
    if (!found) {
      ++irc;
      _bpelog->message(this, "Cannot jump to " + jumpList[jj],
                       t_bpelog::error);
    }
  }

  // Check the OPT directory for existence
  // -------------------------------------
  r_dir optDir( expandEnvVar( initmenu.getPath("PTH_OPT") +
                             getKey("OPT_DIR") ) );
  if (!optDir.exists()) {
    ++irc;
    _bpelog->message(this, "Directory " + getKey("OPT_DIR") +
                     " does not exist", t_bpelog::error);
  }

  // Check the CPU File for required CPU
  // -----------------------------------
  int maxJobs = _server->cpufile()->maxJobs(getKey("REQ_CPU"));
  if (maxJobs < 0) {
    ++irc;
    _bpelog->message(this, "Required CPU " + getKey("REQ_CPU") +
                     " not available", t_bpelog::error);
  }
  else if (maxJobs == 0) {
    ++irc;
    _bpelog->message(this, "Required CPU " + getKey("REQ_CPU") +
                     " with MAXJ = 0", t_bpelog::error);
  }

  // Check the script for existence
  // ------------------------------
  r_file scriptFile( expandEnvVar( initmenu.getPath("PTH_SCRIPT") +
                             getKey("SCRIPT") ) );
  if (!scriptFile.exists()) {
    ++irc;
    _bpelog->message(this, "Script file " + getKey("SCRIPT") +
                     " does not exist", t_bpelog::error);
  }

  // Check if directories exist
  // --------------------------
  r_dir prtDir(getKey("PTH_BPEPRT"));
  if (!prtDir.exists()) {
    ++irc;
    _bpelog->message(this, "Directory " + getKey("PTH_BPEPRT") +
                     " does not exist", t_bpelog::error);
  }
  r_dir logDir(getKey("PTH_BPELOG"));
  if (!logDir.exists()) {
    ++irc;
    _bpelog->message(this, "Directory " + getKey("PTH_BPELOG") +
                     " does not exist", t_bpelog::error);
  }

  // Check the Campaign
  // ------------------
  if (!_specialCamp && !_reprMode) {
    QString v_camp = getKey("V_CAMP");
    if ( !v_camp.isEmpty() && v_camp != getKey("CAMPAIGN") ) {
      ++irc;
      _bpelog->message(this, "Required campaign " + v_camp, t_bpelog::error);
    }
  }

  return irc;
}

// Jump ?
////////////////////////////////////////////////////////////////////////////
QString t_script::jump(const QStringList& msgList) {
  if (getKey("PARAM1") == "NEXTJOB") {
    for (unsigned ii = msgList.count()-1; (int) ii >= 0; ii--) {
      int index = msgList[ii].find("SCRIPT  STARTED");
      if (index != -1) {
        break;
      }
      index = msgList[ii].find("GOTO PID");
      if (index != -1) {
        return msgList[ii].mid(index+9).stripWhiteSpace();
      }
    }
  }
  return "";
}

QStringList t_script::jumps() {
  QStringList list = params();
  if (list.count() > 0 && list[0] == "NEXTJOB") {
    list.remove( list.begin() );
    return list;
  }
  else {
    return QStringList();
  }
}

//
////////////////////////////////////////////////////////////////////////////
bool t_script::cont_err() const {
  if (getKey("PARAM1") == "CONT_ERR") {
    return true;
  }
  else {
    return false;
  }
}

// Get the list of parameters
////////////////////////////////////////////////////////////////////////////
QStringList t_script::params() {
  QStringList list;
  for (unsigned ii = 1; ; ii++) {
    QString keyPar; keyPar.sprintf("PARAM%d", ii);
    QString value = getKey(keyPar);
    if (value.isEmpty()) {
      break;
    }
    else {
      list.append(value);
    }
  }
  return list;
}

// Status
////////////////////////////////////////////////////////////////////////////
void t_script::resetStatus() {
  if (getKey("SKIP") == "TRUE") {
    _status = skipped;
  }
  else {
    _status = waiting;
  }
}

void t_script::setRunning() {
  _status = running;
}

void t_script::setFinished() {
  _status = finished;
}

void t_script::setSkipped() {
  if (_status == waiting) {
    _status = skipped;
  }
}

bool t_script::isFinished() const {
  if (_status == finished || _status == skipped) {
    return true;
  }
  else {
    return false;
  }
}

bool t_script::isRunning() const {
  if (_status == running) {
    return true;
  }
  else {
    return false;
  }
}

bool t_script::canBeStarted() {
  if      (_status == waiting || _status == skipped) {
    return true;
  }

  // Check for special file IDyyssss_PID_SUB.JOB
  // -------------------------------------------
  else if (_status == running) {
    QString jobFile = getKey("PTH_BPEPRT") + getKey("TASKID") + getKey("YEAR") +
                      session() + "_" + pid() + "_" + subPid() + ".JOB";
    if (r_file::exists(jobFile)) {
      _bpelog->message(this, "Script re-started because of file " + jobFile,
                       t_bpelog::msg);
      r_file::remove(jobFile);
      _status = waiting;
      return true;
    }
  }

  return false;
}

bool t_script::isSkipped() const {
  if (_status == skipped) {
    return true;
  }
  else {
    return false;
  }
}

// Remove the Script Output Files (Protocol and Log Files)
////////////////////////////////////////////////////////////////////////////
void t_script::removePrtFile() {

  // Protocol Files
  // --------------
  r_dir prtDir(getKey("PTH_BPEPRT"));
  if (prtDir.exists()) {
    QString prtFilter = getKey("TASKID") + getKey("YEAR") +
                        getKey("SESSION") + "_" + getKey("PID") + "_*." +
                        getKey("EXT_BPEPRT");
    QStringListIterator it(prtDir.entryList(prtFilter, QDir::Files));
    while (it.hasNext()) {
      r_file::remove(prtDir.path() + r_dir::separator() + it.next());
    }
  }

  // Log Files
  // ---------
  r_dir logDir(getKey("PTH_BPELOG"));
  if (logDir.exists()) {
    QString logFilter = getKey("TASKID") + getKey("YEAR") +
                        getKey("SESSION") + "_" + getKey("PID") + "_*." +
                        getKey("EXT_BPELOG");
    QStringListIterator it(logDir.entryList(logFilter, QDir::Files));
    while (it.hasNext()) {
      r_file::remove(logDir.path() + r_dir::separator() + it.next());
    }
  }
}

// Timeout - Remote Machine not Responding
////////////////////////////////////////////////////////////////////////////
void t_script::slotTimeout() {

  setFinished();
  _bpelog->message(this, _CPUnickname + ": timeout", t_bpelog::error);
  _server->cpufile()->freeCPU(idStr(), _CPUnickname);
  _server->close(t_server::error);
}

// Status as a string
////////////////////////////////////////////////////////////////////////////
const char* t_script::status() const {
  if      (_status == waiting) {
    return "waiting";
  }
  else if (_status == skipped) {
    return "skipped";
  }
  else if (_status == running) {
    return "running";
  }
  else if (_status == finished) {
    return "finished";
  }
  return "unknown";
}

// Unique ID String
////////////////////////////////////////////////////////////////////////////
QString t_script::idStr() const {
  if (!_idStr) {
    _idStr = new QString( stripExtension(getKey("PCFFIL")) + "_" +
                          getKey("YEAR")                   + "_" +
                          session()                        + "_" +
                          pid()                            + "_" +
                          (subPid().isEmpty() ? QString("000") : subPid()) );
  }
  return *_idStr;
}

//
////////////////////////////////////////////////////////////////////////////
void t_script::fillTimeStatistics(const QString& msg) {

  _rerunCount = getKeyValFromMsg(msg, "RERUN_COUNT").toInt();

  _timeCpu    = _timeStartScript.elapsed() / 1e3;
  _timePgm    = getKeyValFromMsg(msg, "TIME_PGM").toDouble();
  _timeAux    = getKeyValFromMsg(msg, "TIME_SCRIPT").toDouble() - _timePgm;
  _timeDelay  = _timeCpu - _timePgm - _timeAux;
}

// Add Variables defined by Previous Scripts
////////////////////////////////////////////////////////////////////////////
void t_script::updateScriptAddedVariables() {

  QMap<QString, QString> serverVariables;


  if (_options.contains("SERVER_VARIABLES")) {
    double  oldMJD  = initmenu.getMJD();
    QString oldSess = initmenu.getSessChar();
    initmenu.setMJD( getKey("MJD").toDouble() );
    initmenu.setSessChar( getKey("SESSION") );

    QStringList hlp; unilineSplit(_options["SERVER_VARIABLES"], hlp);
    for (int ii = 0;  ii < hlp.size()-1; ii+=2) {
      QString key   = hlp[ii];
      QString value = hlp[ii+1];
      initmenu.evalDollar(value);
      serverVariables.insert(key, value);
    }

    initmenu.setMJD(oldMJD);
    initmenu.setSessChar(oldSess);
  }

  QMapIterator<QString, QString> it1(initmenu.scriptVariables(session()));
  while (it1.hasNext()) {
    it1.next();
    QString key   = it1.key();
    QString value = it1.value();
    if      (key.indexOf("$(") == 0) {
      key = key.mid(2, key.length()-3);
    }
    else if (key.indexOf("$") == 0) {
      key = key.mid(1);
    }
    serverVariables.insert(key, value);
  }

  QString serverVariablesStr;
  QMapIterator<QString, QString> it2(serverVariables);
  while (it2.hasNext()) {
    it2.next();
    QString key   = it2.key();
    QString value = it2.value();
    _options.insert("V_"+key, value);
    serverVariablesStr += "\"" + key + "\" \"" + value + "\" ";
  }

  if (!serverVariablesStr.isEmpty()) {
    _options.insert("SERVER_VARIABLES", serverVariablesStr);
  }
}
