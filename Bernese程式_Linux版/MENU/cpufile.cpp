
/* -------------------------------------------------------------------------
 * Bernese GPS Software Version 5.0
 * -------------------------------------------------------------------------
 *
 * Class:      t_cpufile
 *
 * Purpose:    This class implements handling of CPU control
 *
 * Author:     L. Mervart
 *
 * Created:    22-DEC-2000
 *
 * Changes:
 *
 * Copyright:  Astronomical Institute
 *              University of Berne
 *                  Switzerland
 * -----------------------------------------------------------------------*/

#ifdef WIN32
#include <windows.h>
#else
#include <unistd.h>
#endif

#include <qregexp.h>
//Added by qt3to4:
#include <Q3ValueList>

#include "cpufile.h"
#include "r_dir.h"
#include "r_file.h"
#include "errormsg.h"
#include "menutils.h"
#include "initmenu.h"
#include "script.h"


// Constructor
////////////////////////////////////////////////////////////////////////////
t_cpufile::t_cpufile(const char* fileName) {

  _fileName = fileName;
  _inpfile  = 0;
  _cpu      = 0;
  _ok       = false;

  QString dirName = "CPU_" + stripPath(fileName, true);

  int oldLogModus = initmenu.getLogModus();
  initmenu.setLogModus(t_initmenu::quiet);
  QString dirPath = initmenu.getPath("PTH_CPUNUM");
  initmenu.setLogModus(oldLogModus);
  if ( !dirPath.isEmpty() ) {
    dirPath += r_dir::separator() + dirName;
  }
  else {
    dirPath = initmenu.getPath("PTH_SCR") + r_dir::separator() + dirName;
  }

  _lockDir = new r_dir(expandEnvVar(dirPath));
  bool lockDirF = true;
  if (!_lockDir->exists()) {
    if ( !_lockDir->mkdir(_lockDir->path(), true) ) {
      errormsg("Error creating scratch directory for CPU file locks");
      lockDirF = false;
    }
  }
  if (lockDirF) {
    readFile();
  }
}

// Destructor
////////////////////////////////////////////////////////////////////////////
t_cpufile::~t_cpufile() {
  if (_cpu) {
    Q3ValueList<t_cpu*>::Iterator it;
    for (it = _cpu->begin(); it != _cpu->end(); it++) {
      delete (*it);
    }
    delete _cpu;
  }
  delete _inpfile;
  delete _lockDir;
}

//// // Read the CPU File
//// ////////////////////////////////////////////////////////////////////////////
//// void t_cpufile::readFile() {
////
////   if (_cpu != 0) {
////     QValueList<t_cpu*>::Iterator it;
////     for (it = _cpu->begin(); it != _cpu->end(); it++) {
////       (*it)->readNumJobs();
////     }
////   }
////   else {
////     _inpfile = new t_inpfile (_fileName);
////     if (_inpfile->getName().isEmpty()) {
////       errormsg("Error creating CPU File");
////       return;
////     }
////
////     t_keyword* key = _inpfile->getKey("LIST_OF_CPUS");
////     if (key == 0) {
////       errormsg("LIST_OF_CPUS not found in " + _inpfile->getName() +
////                "\nThis may not be a CPU File!");
////       return;
////     }
////
////     unsigned size = key->getSelList().count();
////     if (size == 0) {
////       errormsg( "Empty list of CPUs in " + _inpfile->getName() );
////       return;
////     }
////
////     _cpu = new QValueList<t_cpu*>;
////
////     bool cpuOK = true;
////     for (int ii = 0; ii < (int) size; ii++) {
////       _cpu->append(new t_cpu( key->getSelList()[ii], _lockDir )) ;
////       if (!_cpu->last()->ok()) {
////         cpuOK = false;
////       }
////     }
////     _ok = cpuOK;
////   }
//// }

// Read the CPU File
////////////////////////////////////////////////////////////////////////////
void t_cpufile::readFile() {

  if (_cpu) {
    Q3ValueList<t_cpu*>::Iterator it;
    for (it = _cpu->begin(); it != _cpu->end(); it++) {
      delete (*it);
    }
    delete _cpu;
  }
  delete _inpfile;

  _inpfile = new t_inpfile (_fileName);
  if (_inpfile->getName().isEmpty()) {
    errormsg("Error creating CPU File");
    _ok = false;
    return;
  }

  t_keyword* key = _inpfile->getKey("LIST_OF_CPUS");
  if (key == 0) {
    errormsg("LIST_OF_CPUS not found in " + _inpfile->getName() +
             "\nThis may not be a CPU File!");
    _ok = false;
    return;
  }

  unsigned size = key->getSelList().count();
  if (size == 0) {
    errormsg( "Empty list of CPUs in " + _inpfile->getName() );
    _ok = false;
    return;
  }

  _cpu = new Q3ValueList<t_cpu*>;

  bool cpuOK = true;
  for (int ii = 0; ii < (int) size; ii++) {
    _cpu->append(new t_cpu( key->getSelList()[ii], _lockDir )) ;
    if (!_cpu->last()->ok()) {
      cpuOK = false;
    }
    else {
      _cpu->last()->readNumJobs();
    }
  }
  _ok = cpuOK;
}

// Find a CPU
////////////////////////////////////////////////////////////////////////////
bool t_cpufile::findFreeCPU(const QString& idStr, const QString& reqCpuName,
                            QString& cpuNickname, QString& cpuCommand,
                            int& maxWait) {

  readFile();

  Q3ValueList<t_cpu*>::Iterator foundIt = _cpu->end();

  // First Try to Find the exact match
  // ---------------------------------
  Q3ValueList<t_cpu*>::Iterator it;
  for (it = _cpu->begin(); it != _cpu->end(); it++) {
    if ( (*it)->match(reqCpuName) == t_cpu::exact_match &&
         (*it)->numJobs() < (*it)->maxJobs()            ) {
      foundIt = it;
      break;
    }
  }

  // If no exact match, look for the speed match
  // -------------------------------------------
  if (foundIt == _cpu->end()) {
    for (it = _cpu->begin(); it != _cpu->end(); it++) {
      int maxJobs = (*it)->maxJobs();
      if (reqCpuName == "IDLE") {
        if (maxJobs > 0) {
          maxJobs = 1;
        }
      }
      if ( (*it)->match(reqCpuName) == t_cpu::speed_match &&
           (*it)->numJobs() < maxJobs                     ) {
        foundIt = it;
        break;
      }
    }
  }

  // Update number of running jobs
  // -----------------------------
  if ( foundIt != _cpu->end() ) {
    cpuNickname = (*foundIt)->name();
    cpuCommand  = (*foundIt)->command();
    maxWait     = (*foundIt)->maxWait();
    if (!idStr.isEmpty()) {
      (*foundIt)->incrementNumJobs(idStr);
    }
    return true;
  }
  else {
    return false;
  }
}

// Free CPU
////////////////////////////////////////////////////////////////////////////
void t_cpufile::freeCPU(const QString& idStr, const QString& cpuNickname) {

  Q3ValueList<t_cpu*>::Iterator it;
  for (it = _cpu->begin(); it != _cpu->end(); it++) {
    if ( (*it)->match(cpuNickname) == t_cpu::exact_match ) {
      (*it)->decrementNumJobs(idStr);
      break;
    }
  }
}

// Maximal Number of Jobs (return -1 if no CPU)
////////////////////////////////////////////////////////////////////////////
int t_cpufile::maxJobs(const QString& cpuNickname) const {

  int sumMaxJobs = -1;

  Q3ValueList<t_cpu*>::Iterator it;
  for (it = _cpu->begin(); it != _cpu->end(); it++) {
    if ( (*it)->match(cpuNickname) ) {
      if (sumMaxJobs == -1) {
        sumMaxJobs = 0;
      }
      sumMaxJobs += (*it)->maxJobs();
    }
  }
  return sumMaxJobs;
}

// Check for Suspended PCF
////////////////////////////////////////////////////////////////////////////
bool t_cpufile::suspended(const QString& pcfName) const {

  if (_inpfile) {
    t_keyword*  suspendKey = _inpfile->getKey("SUSPEND_PCF");
    if (suspendKey) {
      QDir    dir      = QDir(expandEnvVar(initmenu.getPath("PTH_PCF")));
      QString fileName = stripPath(expandEnvVar(pcfName));

      QStringList suspendList = suspendKey->getSelList();
      for (int ii = 0; ii < suspendList.count(); ii++) {
        QStringList hlp; unilineSplit(suspendList[ii], hlp);

        bool matches = false;
        if (hlp.count() > 0) {
          QStringList entries = dir.entryList(hlp[0]);
          for (int ii = 0; ii < entries.size(); ii++) {
            if (entries.at(ii) == fileName) {
              matches = true;
              break;
            }
          }
        }

        if (matches) {
          if (hlp.count() < 2 || hlp[1].isEmpty()) {
            return true;
          }
          else {
            QFileInfo fInfo(expandEnvVar(hlp[1]));
            if (fInfo.exists()) {
              if (hlp.count() < 3 || hlp[2].isEmpty()) {
                return true;
              }
              else {
                int sec = fInfo.lastModified().secsTo(QDateTime::currentDateTime());
                if (sec < hlp[2].toInt()) {
                  return true;
                }
              }
            }
          }
        }
      }
    }
  }
  return false;
}

// Reset All CPUs
////////////////////////////////////////////////////////////////////////////
void t_cpufile::resetCPU() {

  QStringList entry = _lockDir->entryList(QDir::Files);
  for (QStringList::Iterator it = entry.begin(); it != entry.end(); ++it) {
    QString name = _lockDir->path() + r_dir::separator() + (*it);
    r_file::remove(name);
  }
}

// Constructor
////////////////////////////////////////////////////////////////////////////
t_cpufile::t_cpu::t_cpu(const QString& line, r_dir* lockDir) {
  QStringList hlp;
  unilineSplit( line, hlp );
  _name    = hlp[0];
  _command = hlp[1];
  _speed   = hlp[2];
  _maxJobs = hlp[3].toInt();
  _numJobs = 0;
  if ( hlp.count() > 5 && !hlp[5].isEmpty() ) {
    _maxWait = hlp[5].toInt();
  }
  else {
    _maxWait = 0;
  }
  _ok      = true;

  // Lock Directory and File
  // -----------------------
  _lockDir  = lockDir;

  // Command
  // -------
  int index = _command.find("<COMMAND>");
  if (index == -1) {
    _ok = false;
    errormsg("<COMMAND> part not found for CPU " + _name);
  }

  // List of Arguments
  // -----------------
  index = _command.find("<ARGV>");
  if (index == -1) {
    _ok = false;
    errormsg("<ARGV> part not found for CPU " + _name);
  }
  readNumJobs();
}

// Destructor
////////////////////////////////////////////////////////////////////////////
t_cpufile::t_cpu::~t_cpu() {
}

// Compose a uniline-string
////////////////////////////////////////////////////////////////////////////
QString t_cpufile::t_cpu::toString() const {
  return '"' + _name    + '"' + " " +
         '"' + _command + '"' + " " +
         '"' + _speed   + '"' + " " +
         '"' + QString().sprintf("%d", _maxJobs) + '"' + " " +
         '"' + QString().sprintf("%d", _numJobs) + '"' + " " +
         '"' + QString().sprintf("%d", _maxWait) + '"';
}

// Match the required name against available nickname
////////////////////////////////////////////////////////////////////////////
t_cpufile::t_cpu::t_match t_cpufile::t_cpu::match(const QString& rqNam) const {

  if      (rqNam == name()) {
    return exact_match;
  }
  else if (rqNam == speed()) {
    return speed_match;
  }
  else if (rqNam == "" || rqNam == "ANY" || rqNam == "IDLE") {
    return speed_match;
  }
  else {
    return no_match;
  }
}

// Read Number of running Jobs
////////////////////////////////////////////////////////////////////////////
void t_cpufile::t_cpu::readNumJobs() {

  const QRegExp rex(name() + ".*", false, true);

  _numJobs = 0;

  QStringList entry = _lockDir->entryList(QDir::Files);
  for (QStringList::Iterator it = entry.begin(); it != entry.end(); ++it) {
    if ( rex.exactMatch(*it) ) {
      ++_numJobs;
    }
  }
}

// Name of the Lock File
////////////////////////////////////////////////////////////////////////////
QString t_cpufile::t_cpu::lockFileName(const QString& idStr) {

  return _lockDir->path() + r_dir::separator() +
             name() + QString().sprintf(".%d_", initmenu.pid()) + idStr;
}

// Increment the Number of running Jobs
////////////////////////////////////////////////////////////////////////////
void t_cpufile::t_cpu::incrementNumJobs(const QString& idStr) {

  r_file lockFile( lockFileName(idStr) );

  if ( lockFile.open(QIODevice::WriteOnly) ) {
    lockFile.close();
    ++_numJobs;
  }
}

// Decrement the Number of running Jobs
////////////////////////////////////////////////////////////////////////////
void t_cpufile::t_cpu::decrementNumJobs(const QString& idStr) {

  r_file lockFile( lockFileName(idStr) );

  if (lockFile.exists()) {
    lockFile.remove();
    --_numJobs;
    if (_numJobs < 0) {
      _numJobs = 0;
    }
  }
}

//
////////////////////////////////////////////////////////////////////////////
void t_cpufile::updateTimeStatistics(const t_script* script, bool /* error */,
                                     const QString& taskid) {
  QString cpuName = script->getCPUnickname();
  QString pid     = script->pid() + "_" + script->subPid();
  QString sess    = (taskid.length() > 6) ? taskid.mid(2) : taskid;

  t_timeStatistics& ts = _ts[cpuName];

  ts.numJobs    += 1;
  ts.cpuTotal   += script->timeCpu();
  ts.queueTotal += script->timeQueue();

  if (ts.queueMaxSess.isEmpty()) {
    ts.cpuMax       = script->timeCpu();
    ts.cpuMin       = script->timeCpu();
    ts.queueMaxSess = sess;
    ts.queueMaxPid  = pid;
    ts.queueMax     = script->timeQueue();
  }
  else {
    if (ts.cpuMax < script->timeCpu()) {
      ts.cpuMax = script->timeCpu();
    }
    if (ts.cpuMin > script->timeCpu()) {
      ts.cpuMin = script->timeCpu();
    }
    if (ts.queueMax < script->timeQueue()) {
      ts.queueMaxSess = sess;
      ts.queueMaxPid  = pid;
      ts.queueMax     = script->timeQueue();
    }
  }
}

//
////////////////////////////////////////////////////////////////////////////
QString t_cpufile::printTimeStatistics() {
  QString msg;

  msg += "\n\nCPU-related BPE statistics:\n"
             "===========================\n";

  msg += _fileName + "\n";

  msg += "-----------------------------------------------------------------------------\n"
          "                            CPU time in sec     Queuing time                 \n"
          "CPU name  # of jobs    Total  Mean   Max   Min  Total   Max  Session  PID_SUB\n"
          "-----------------------------------------------------------------------------\n";

  QMapIterator<QString, t_timeStatistics> it(_ts);
  while (it.hasNext()) {
    const t_timeStatistics& ts = it.next().value();
    QString cpuName = it.key().leftJustified(8,' ');
    msg += QString().sprintf("%s  %9d  %7d %5d %5d %5d  %5d %5d   %s  %s\n",
                             cpuName.toAscii().data(), ts.numJobs,
                             int(ts.cpuTotal), int(ts.cpuTotal/ts.numJobs),
                             int(ts.cpuMax), int(ts.cpuMin),
                             int(ts.queueTotal), int(ts.queueMax),
                             ts.queueMaxSess.toAscii().data(),
                             ts.queueMaxPid.toAscii().data());
  }

  msg += "-----------------------------------------------------------------------------\n";
  return msg;
}
