
/* -------------------------------------------------------------------------
 * Bernese GPS Software Version 5.0
 * -------------------------------------------------------------------------
 *
 * Functions:  menutils
 *
 * Purpose:    Several global commonly used functions.
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

#ifdef WIN32
#include <windows.h>
#else
#include <unistd.h>
#endif

#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <zlib.h>
#include <qregexp.h>
#include <qtextstream.h>

#include "menutils.h"
#include "errormsg.h"
#include "initmenu.h"
#include "r_file.h"
#include "r_dir.h"

extern QString celMechRoot;

// Expand Environment Variable
////////////////////////////////////////////////////////////////////////////
QString expandEnvVar(const QString& str, int* systemVarNotFound) {
  QString localStr = str;

  int i1;
  int i2 = 0;

  if (systemVarNotFound != 0) {
    *systemVarNotFound = 0;
  }

  for (;;) {
    i1 = localStr.find('$', i2);
    if ( i1 == -1 || i1 == (int) localStr.length() ) break;

    QString varName;

    if (localStr[i1+1] == '{') {
      i2 = localStr.find('}',i1);
      if (i2 == -1) {
        errormsg("Mismatched Parentheses in\n" + str);
        break;
      }
      varName = localStr.mid(i1+2, i2-i1-2);
    }
    else {
      i2 = i1 + 1;
      varName  = localStr.mid(i1+1, 1);
    }

    if ( varName == "CM" && !celMechRoot.isEmpty() ) {
      localStr = localStr.replace(i1, i2-i1+1, celMechRoot);
    } else {
      QString varValue = initmenu.getenv(varName);
      if ( varValue.isEmpty() ) {
        if (systemVarNotFound != 0) {
          *systemVarNotFound = 1;
        }
        else {
          errormsg("System Variable not Defined:\n$" + varName);
        }
      }
      else {
        localStr = localStr.replace(i1, i2-i1+1, varValue);
      }
    }
  }
  return localStr;
}

// Strip Path and Extension
////////////////////////////////////////////////////////////////////////////
QString stripPath(const QString& str, bool stripExtension) {

  QString localStr;
  if (stripExtension) {
    localStr = ::stripExtension(str);
  }
  else {
    localStr = str;
  }
  int sepIndex = localStr.findRev(QRegExp("[/\\\\]"));
  return localStr.mid(sepIndex+1).stripWhiteSpace();
}

QString stripExtension(const QString& str, QString* ext) {

  int sepIndex = str.findRev(QRegExp("[/\\\\]"));
  int dotIndex = str.find('.', sepIndex + 1);
  if (dotIndex != -1) {
    if (ext != 0) {
      *ext = str.mid(dotIndex+1).stripWhiteSpace();
    }
    return QString(str.left(dotIndex).stripWhiteSpace());
  }
  else {
    if (ext != 0) {
      *ext = "";
    }
    return QString(str.stripWhiteSpace());
  }
}

// Strip File Name, Return the Path
////////////////////////////////////////////////////////////////////////////
QString stripFileName(const QString& str) {

  int index = str.findRev(QRegExp("[/\\\\]"));

  if (index != -1) {
    return str.left(index);
  }
  else {
    return "";
  }
}

// Split Uniline
////////////////////////////////////////////////////////////////////////////
void unilineSplit(const QString& uniline, QStringList& outList) {
  outList = QStringList::split(QRegExp("\"\\s+\""), uniline, true);

  for (int ii = 0; ii < (int) outList.count(); ii++) {
    QString str = outList[ii];
    if (str.length() >= 2 && str[0] == '#' && str[(int)str.length()-1] == '#'){
      str = str.mid(1,str.length()-2);
    }
    outList[ii] = str.replace(QRegExp("\""),"").stripWhiteSpace();
  }
}

// Split Cards
////////////////////////////////////////////////////////////////////////////
void cardSplit(const QString& inpStr, QStringList& outList) {
  if (inpStr.find('"') == -1) {
    outList = QStringList::split(QRegExp("\\s+"), inpStr);
  }
  else {
    unilineSplit(inpStr, outList);
  }
}

// Get a key value from the BPE message
////////////////////////////////////////////////////////////////////////////
QString getKeyValFromMsg(const QString& inMsg, const QString& key) {
  QStringList hlpList = QStringList::split(QRegExp("\\s*&&\\s*"), inMsg);
  for (int ii = 0; ii < (int) hlpList.count(); ii++) {
    QStringList hlpList2 = QStringList::split(QRegExp("="), hlpList[ii]);
    if (hlpList2.count() == 2 && hlpList2[0] == key) {
      return hlpList2[1].stripWhiteSpace();
    }
  }
  return "";
}

// Copy a File
////////////////////////////////////////////////////////////////////////////
t_irc fileCopy(const QString& source, const QString& target) {

  r_file inFile( expandEnvVar(source) );

  if (initmenu.modemMode()) {
    return inFile.copy(target);
  }

  if (! inFile.open(QIODevice::ReadOnly)) {
    errormsg("Could not read file " + source);
    return failure;
  }

  r_file outFile( expandEnvVar(target) );

  // Create Directory if necessary
  // -----------------------------
  r_dir  dir( stripFileName(outFile.name()) );
  if (!dir.exists()) {
    dir.mkdir( dir.path() );
  }

  if (! outFile.open(QIODevice::WriteOnly)) {
    errormsg("Could not write into file " + target);
    return failure;
  }

  QByteArray buffer = inFile.readAll();
  inFile.close();

  outFile.writeBlock(buffer.data(), buffer.size());
  outFile.close();

  return success;
}

// Read File Content into a String
////////////////////////////////////////////////////////////////////////////
QString fileToString(const QString& fileName) {

  r_file inFile( expandEnvVar(fileName) );
  if (! inFile.open(QIODevice::ReadOnly)) {
    errormsg("Could not read file " + fileName);
    return "";
  }

  QByteArray buffer = inFile.readAll();
  inFile.close();

  buffer.resize(buffer.size()+1);
  buffer[int(buffer.size()-1)] = '\0';
  QString str(buffer);

  return str;
}

// Find the Programs in a Script
////////////////////////////////////////////////////////////////////////////
QStringList* findPgmInScript(const QString& script) {

  QString pthScript = expandEnvVar( initmenu.getPath("PTH_SCRIPT") ) + script;

  r_file inFile(pthScript);
  if (! inFile.open(QIODevice::ReadOnly)) {
    return 0;
  }

  QStringList* pgmList = new QStringList;

  QString line;
  QString foundPgm;
  while ( !inFile.eof() ) {
    line =  inFile.readLine();
    if ( line.stripWhiteSpace()[0] == '#' ) continue;
    int ind0 = line.find( QRegExp("PGMNAM\\s*=\\s*") );
    if (ind0 != -1) {
      int ind1 = line.find('"', ind0);
      int ind2 = line.find('"', ind1+1);
      foundPgm = line.mid(ind1+1, ind2-ind1-1);
    }
    if (!foundPgm.isEmpty()) {
      ind0 = line.find( QRegExp("RUN_PGMS") );
      if (ind0 != -1) {
        pgmList->append(foundPgm);
      }
    }
  }

  inFile.close();

  return pgmList;
}

// Split the Condition Line
////////////////////////////////////////////////////////////////////////////
void splitCondLine(const QString& line, QStringList& condKeyName,
                   QStringList& condRelOper, QStringList& condValue,
                   QStringList& condLogOper) {

  QString condLine = line;
  condLine.replace(QRegExp("#")," ");
  condLine.replace(QRegExp("BEGIN_PANEL")," ");
  condLine = condLine.simplifyWhiteSpace();

  QTextStream inLine(&condLine, QIODevice::ReadOnly);

  QString hlp;

  while (!inLine.atEnd()) {
    inLine >> hlp; condKeyName.append(hlp);
    inLine >> hlp; condRelOper.append(hlp);
    inLine >> hlp; if (hlp == "_") hlp = ""; condValue.append(hlp);
    if (!inLine.atEnd()) {
      inLine >> hlp; condLogOper.append(hlp);
    }
  }
}

// Evaluate All Logical Tags
////////////////////////////////////////////////////////////////////////////
bool evalLogTags(const QStringList& keyValue, const QStringList& condRelOper,
            const QStringList& condValue, const QStringList& condLogOper) {

  bool* logTag = new bool[keyValue.count()];

  for (int ii = 0; ii < keyValue.count(); ii++) {
    if (keyValue[ii] == "*") {
      logTag[ii] = true;
    }
    else {
      if      (condRelOper[ii] == "=" || condRelOper[ii] == "==") {
        logTag[ii] = (keyValue[ii] == condValue[ii]);
      }
      else if (condRelOper[ii] == "/" || condRelOper[ii] == "/=") {
        logTag[ii] = (keyValue[ii] != condValue[ii]);
      }
      else if (condRelOper[ii] == ">" ) {
        logTag[ii] = (keyValue[ii].toDouble() > condValue[ii].toDouble());
      }
      else if (condRelOper[ii] == ">=" ) {
        logTag[ii] = (keyValue[ii].toDouble() >= condValue[ii].toDouble());
      }
      else if (condRelOper[ii] == "<" ) {
        logTag[ii] = (keyValue[ii].toDouble() < condValue[ii].toDouble());
      }
      else if (condRelOper[ii] == "<=" ) {
        logTag[ii] = (keyValue[ii].toDouble() <= condValue[ii].toDouble());
      }
      else {
        errormsg("Unknown operator: " + condRelOper[ii]);
        logTag[ii] = true;
      }
    }
  }

  // Join the Logical Tags
  // ---------------------
  bool returnValue = logTag[0];
  for (int kk = 1; kk < keyValue.count(); kk++) {
    if (condLogOper[kk-1] == "OR") {
      returnValue = ( returnValue || logTag[kk] );
    }
    else {
      returnValue = ( returnValue && logTag[kk] );
    }
  }
  delete[] logTag;
  return returnValue;
}

// Append newDir into the list of directories (wildcards allowed)
////////////////////////////////////////////////////////////////////////////
void appendDirs(QStringList& dirList, const QString& line) {

  r_dir newDir(expandEnvVar(line));
  QString path    = newDir.path();
  QString dirName = newDir.dirName();

  if (dirName.find('*') != -1 || dirName.find('?') != -1) {
    path.truncate( path.length() - dirName.length() );
    r_dir hlpDir(path);
    QStringList subDirs = hlpDir.entryList(dirName, QDir::Dirs);
    for (int ii = 0; ii < subDirs.count(); ii++) {
      if (subDirs[ii] != "." && subDirs[ii] != "..") {
        dirList.append( path + subDirs[ii] );
      }
    }
  }
  else {
    dirList.append(path);
  }
}

// Sleep (seconds)
////////////////////////////////////////////////////////////////////////////
void mysleep(double sleepTime) {
#ifdef WIN32
    Sleep(  (unsigned long) (sleepTime*1.e3) );
#else
    usleep( (unsigned long) (sleepTime*1.e6) );
#endif
}

// Sleep (seconds)
////////////////////////////////////////////////////////////////////////////
QString  stripBnpPrefix(const QString fileName) {

  QString localName = fileName.stripWhiteSpace();

  int len = initmenu.bnpPrefix().length();
  return localName.mid(len).stripWhiteSpace();
}

// Compress String using Zlib
////////////////////////////////////////////////////////////////////////////
QByteArray compress(const QString& srcStr) {

  uLong  sourceLen = srcStr.length();
  Bytef* source    = new Bytef[sourceLen];
  uLong  destLen   = (uLong) (ceil(1.1 * sourceLen) + 12);
  Bytef* dest      = new Bytef[destLen];

  memcpy(source, srcStr.latin1(), sourceLen);
  int irc = compress(dest, &destLen, source, sourceLen);
  if (irc != Z_OK) {
    errormsg(QString().sprintf("Error %d in compress", irc));
    return QByteArray();
  }

  QByteArray result(destLen);
  memcpy(result.data(), dest, destLen);

  return result;
}

// Decompress String using Zlib
////////////////////////////////////////////////////////////////////////////
QString uncompress(const QByteArray& srcStr) {

  uLong  sourceLen = srcStr.size();
  uLong  destLen   = 20 * sourceLen;
  Bytef* dest      = new Bytef[destLen+1];

  int irc = uncompress(dest, &destLen, (Bytef*) srcStr.data(), sourceLen);
  if (irc != Z_OK) {
    errormsg(QString().sprintf("Error %d in uncompress", irc));
    return QByteArray();
  }

  char* result = new char[destLen+1];
  memcpy(result, dest, destLen);
  result[destLen] = '\0';

  return QString(result);
}

