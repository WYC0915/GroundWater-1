
/* -------------------------------------------------------------------------
 * Bernese GPS Software Version 5.0
 * -------------------------------------------------------------------------
 *
 * Class:      t_inpfile
 *
 * Purpose:    This class stores and handles one input file.
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

#include <qglobal.h>
#include <qstring.h>
#include <q3dict.h>
//Added by qt3to4:
#include <QTextStream>

#include "inpfile.h"
#include "initmenu.h"
#include "errormsg.h"
#include "menutils.h"
#include "lockfile.h"
#include "r_file.h"

// Constructor
////////////////////////////////////////////////////////////////////////////
t_inpfile::t_inpfile(const QString& fileName, int menuItem, bool expandSelLists) {
  _status   = t_inpfile::status_OK;
  _panels   = 0;
  _keys     = 0;
  _fileName = expandEnvVar(fileName);
  _menuItem = menuItem;
  _expandSelLists = expandSelLists;
  readFile();
}

// Destructor
////////////////////////////////////////////////////////////////////////////
t_inpfile::~t_inpfile() {
  if (_panels) {
    _panels->clear();
    delete _panels;
  }
  if (_keys) {
    _keys->clear();
    delete _keys;
  }

  unlockFile( _fileName );
}

// Name of the input file
////////////////////////////////////////////////////////////////////////////
QString t_inpfile::getName() {
  if (_status != t_inpfile::status_NOT_OPENED) {
    return _fileName;
  }
  else {
    return "";
  }
}

// Read Input File
////////////////////////////////////////////////////////////////////////////
void t_inpfile::readFile() {
  r_file inpFile(_fileName);

  if ( !inpFile.open(QIODevice::ReadOnly | QIODevice::Text) ) {
    errormsg("Cannot open INP file " + _fileName);
    _status = t_inpfile::status_NOT_OPENED;
    return ;
  }

  _keys   = new Q3PtrList<t_keyword>(); _keys->setAutoDelete(true);
  _panels = new Q3PtrList<t_panel>();  _panels->setAutoDelete(true);

  QString line;
  while ( !inpFile.eof() ) {
    line = inpFile.readLine().stripWhiteSpace();
    if (line.isEmpty()) continue;
    if (line.contains("BEGIN_PANEL")) break;
    if (line.contains("END OF INPUT")) continue;

    _keys->append( new t_keyword(line, inpFile, this) );
  }

  int panNum = 0;
  while ( !inpFile.eof() ) {
    ++panNum;
    _panels->append( new t_panel(line, inpFile, _keys, panNum) );
    do {
      line = inpFile.readLine();
    } while ( !inpFile.eof() && !line.contains("BEGIN_PANEL") );
  }

  inpFile.close();
}

// Return the Pointer to the next Panel
////////////////////////////////////////////////////////////////////////////
t_panel* t_inpfile::nextPan(void) {
  if (_panels->next()) {
    return _panels->current();
  }
  else {
    _panels->last();
    return 0;
  }
}

// Return the Pointer to the previous Panel
////////////////////////////////////////////////////////////////////////////
t_panel* t_inpfile::prevPan(void) {
  if (_panels->prev()) {
    return _panels->current();
  }
  else {
    _panels->first();
    return 0;
  }
}

// Set Current Active Panel
////////////////////////////////////////////////////////////////////////////
void t_inpfile::setCurPan(t_panel* currentPanel) {
  _panels->find(currentPanel);
}

// Save the Input File
////////////////////////////////////////////////////////////////////////////
void t_inpfile::save() {

  QString outFileName = _fileName;
  QString inpPath     = initmenu.getKeySel0("PTH_INP");
  QString outPath     = initmenu.getKeySel0("PTH_INP_OUT");

  int index = outFileName.find(inpPath);

  if (index != -1) {
    outFileName.replace(index, inpPath.length(), outPath);
  }
  save(outFileName);
}

// Save the Input File
////////////////////////////////////////////////////////////////////////////
void t_inpfile::save(const QString& outFileName) {
  QString hlpFileName = outFileName;
  r_file outFile(expandEnvVar(hlpFileName));
  if ( !outFile.open(QIODevice::WriteOnly) ) {
    errormsg("Cannot write into file " + outFileName);
    return ;
  }

  this->expandSelList();

  for (int ii = 0; ii < (int) _keys->count(); ii++) {
    outFile << *_keys->at(ii);
  }

  for (int kk = 0; kk < (int) _panels->count(); kk++) {
    outFile << *_panels->at(kk);
  }

  outFile.close();
}

// Return the Pointer to the Specified Keyword
////////////////////////////////////////////////////////////////////////////
t_keyword* t_inpfile::getKey(const QString& keyName) {
  for( int ii = 0; ii < (int) _keys->count(); ii++) {
    if (_keys->at(ii)->getName() == keyName) {
      return _keys->at(ii);
    }
  }
  return 0;
}

// Return the First Item of the Selection List of the required Keyword
////////////////////////////////////////////////////////////////////////////
QString t_inpfile::getKeySel0(const QString& keyName) {
  t_keyword* key = getKey(keyName);
  if (key == 0 || key->getSelList().count() == 0) {
    return "";
  }
  else {
    return key->getSelList()[0].stripWhiteSpace();
  }
}

// Standard Output File Name
////////////////////////////////////////////////////////////////////////////
QString t_inpfile::defaultStdOutFileName(bool stepDefaultSysout) {
  QString baseName = stripPath(_fileName, true);

  QString outDir = initmenu.getPath("DIR_OUT");
  if (baseName == "UPDPAN" || baseName == "CHNGEN") {
    outDir = initmenu.getPath("PTH_OUT");
  }

  r_file ioFile(expandEnvVar(outDir + baseName + ".J"));

  if ( !lockFile(ioFile.name(), 600, 0.1) ) {
    return outDir + baseName + ".XXX";
  }

  if ( !ioFile.open(QIODevice::ReadWrite | QIODevice::Text) ) {
    errormsg("Cannot Open File " + ioFile.name());
    unlockFile(ioFile.name());
    return outDir + baseName + ".XXX";
  }

  int nextExt = 0;

  if (ioFile.size() > 0) {
    ioFile.setf(QTextStream::dec);
    ioFile >> nextExt;
//  ioFile.rewind();
    ioFile.close(); ioFile.remove(); ioFile.open(QIODevice::WriteOnly | QIODevice::Text);
    if (!stepDefaultSysout) {
      --nextExt;
    }
   }

  int maxJobNum = initmenu.getKeySel0("MAXJOBNUM").toInt();
  if (maxJobNum < 0)   maxJobNum = 0;
  if (maxJobNum > 999) maxJobNum = 999;

  QString extStr;
  if (maxJobNum <= 99) {
    extStr.sprintf(".L%2.2d",nextExt);
  }
  else {
    extStr.sprintf(".%3.3d",nextExt);
  }

  t_keyword* editDescKey = initmenu.getKey("EDIT_KEYWORD");
  if (!editDescKey) {
    if (++nextExt > maxJobNum) {
      nextExt = 0;
    }
    QString hlp;
    hlp.sprintf(" %3d     NEXT OUTPUT-FILE EXTENSION", nextExt);
    ioFile << hlp << '\n';
  }

  ioFile.close();
  unlockFile(ioFile.name());
  return outDir + baseName + extStr;
}

// Expand selList of all Keywords
////////////////////////////////////////////////////////////////////////////
void t_inpfile::expandSelList(bool stepDefaultSysout) {

  bool except = initmenu.getExcept();
  initmenu.setExcept(true);
  try {

    _stdOutFileName = "";
    _errorFileName  = "";

    // Find All Pointers that should be updated
    // ----------------------------------------
    Q3Dict<t_keyword> pointers;
    for (unsigned jj = 0; jj < _keys->count(); jj++) {
      QString pointerName = _keys->at(jj)->getDesc()->pointer();
      if (!pointerName.isEmpty() && !pointers.find( pointerName )) {
        if (_keys->at(jj)->getDesc()->widget()          == "selwin" &&
            _keys->at(jj)->getValue().find('*')         == -1       &&
            _keys->at(jj)->getValue().find('?')         == -1       &&
            _keys->at(jj)->getValue().stripWhiteSpace() != "$first" ) {
          continue;
        }
        for (unsigned pp = 0; pp < _keys->count(); pp++) {
          if (_keys->at(pp)->getName() == pointerName) {
            pointers.insert( pointerName, _keys->at(pp) );
            _keys->at(pp)->updateSel();
            break;
          }
        }
      }
    }

    t_keyword* sysodefKey = getKey("SYSODEF");
    t_keyword* errmrgKey  = getKey("ERRMRG");

    // Update Keys and Expand Selection Lists
    // --------------------------------------
    for (int ii = 0; ii < (int) _keys->count(); ii++) {
      if ( _keys->at(ii)->evalUpdateIfSave() ) {
        _keys->at(ii)->updateSel(false);
      }
      else if ( !_keys->at(ii)->getDesc()->menuaux() ) {
        _keys->at(ii)->updateSel(false); // Pointers need not be updated any more
      }
      _keys->at(ii)->expandSelList();

      // Handle SYSOUT
      // -------------
      if ( _keys->at(ii)->getName() == "SYSOUT" ) {
        if      (sysodefKey == 0) { // old panels
          if ( _keys->at(ii)->getValue().stripWhiteSpace().isEmpty() ) {
            _keys->at(ii)->storeSel(defaultStdOutFileName(stepDefaultSysout));
            _keys->at(ii)->setValue("");
          }
        }
        else if (sysodefKey->getSelList().value(0) == "1" ||
                 _keys->at(ii)->getValue().stripWhiteSpace().isEmpty()) {
          _keys->at(ii)->storeSel(defaultStdOutFileName(stepDefaultSysout));
        }
        _stdOutFileName = _keys->at(ii)->getSelList().value(0);
      }

      // Handle SYSERR
      // -------------
      if ( _keys->at(ii)->getName() == "SYSERR" ) {
        if      (errmrgKey == 0) {  // old panels
          if (_keys->at(ii)->getSelList().count() > 0) {
            _errorFileName = _keys->at(ii)->getSelList().value(0);
          }
        }
        else if (errmrgKey->getSelList().value(0) == "0") {
          _errorFileName = _keys->at(ii)->getSelList().value(0);
        }
      }
    }
  }
  catch (const QString& str) {
    initmenu.setExcept(except);
    _status = t_inpfile::status_ERROR;
    if (except) {
      throw str;
    }
    else {
      errormsg(str);
      return;
    }
  }
  initmenu.setExcept(except);
}

// Expand selList of all Keywords
////////////////////////////////////////////////////////////////////////////
bool t_inpfile::hasPanels() {
  if (_panels->count() > 0) {
    return true;
  }
  else {
    errormsg("No panels found in " + _fileName);
    return false;
  }
}

