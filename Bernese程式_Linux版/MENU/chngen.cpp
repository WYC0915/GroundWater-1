
/* -------------------------------------------------------------------------
 * Bernese GPS Software Version 5.0
 * -------------------------------------------------------------------------
 *
 * Functions:  chngen
 *
 * Purpose:    Change Keyword Values
 *
 * Author:     L. Mervart

 *
 * Created:    22-AUG-2001
 *
 * Changes:
 *
 * Copyright:  Astronomical Institute
 *              University of Berne
 *                  Switzerland
 * -----------------------------------------------------------------------*/

#include <qstringlist.h>

#include "chngen.h"
#include "inpfile.h"
#include "initmenu.h"
#include "errormsg.h"
#include "menutils.h"
#include "r_file.h"
#include "r_dir.h"

//
////////////////////////////////////////////////////////////////////////////
void chngen(const QString& inpFileName)
{
  // Read all options
  // ----------------
  t_inpfile* inpfile   = new t_inpfile(inpFileName);

  QStringList updFiles;
  QStringList updDirs;
  QString     updLst;

  // Select Files or Directories
  // ---------------------------
  if      (inpfile->getKeySel0("RADIO_UPDFIL") == "1") {
    t_keyword* keyUPDFIL = inpfile->getKey("UPDFIL");
    if (keyUPDFIL) {
      keyUPDFIL->updateSel();
      keyUPDFIL->expandSelList();
      updFiles = QStringList( keyUPDFIL->getSelList() );
      for (int ii = 0; ii < updFiles.count(); ii++) {
        updFiles[ii] = expandEnvVar( updFiles[ii] );
      }
    }
  }
  else if (inpfile->getKeySel0("RADIO_UPDDIR") == "1") {
    appendDirs( updDirs, inpfile->getKeySel0("UPDDIR") );
  }
  else if (inpfile->getKeySel0("RADIO_UPDLST") == "1") {
    t_keyword* keyUPDLST = inpfile->getKey("UPDLST");
    if (keyUPDLST) {
      if (keyUPDLST->getSelList().count() > 0) {
        keyUPDLST->updateSel();
        keyUPDLST->expandSelList();
        updLst = keyUPDLST->getSelList()[0];
      }
    }
  }

  if ( updFiles.count() == 0 && updDirs.count() == 0 && updLst.isEmpty() ) {
    errormsg("chngen: wrong options");
    delete inpfile;
    return;
  }

  // Concatenate the List of Directories
  // -----------------------------------
  if ( ! updLst.isEmpty() ) {
    r_file updLstFile( expandEnvVar(updLst) );
    if ( !updLstFile.open(QIODevice::ReadOnly | QIODevice::Text) ) {
      errormsg("Cannot open file " + updLst);
      delete inpfile;
      return ;
    }

    QString line;
    while ( !updLstFile.eof() ) {
      line = updLstFile.readLine().simplifyWhiteSpace();
      if (line.isEmpty()) continue;
      appendDirs( updDirs, line.stripWhiteSpace() );
    }
  }

  // Concatenate the List of All Files
  // ---------------------------------
  QString nameFilter =  "*." + initmenu.getKeySel0("EXT_INP") +
                       ";*." + initmenu.getKeySel0("EXT_INP") + "_??";
  for (int iDir = 0; iDir < updDirs.count(); iDir++) {
    r_dir curDir( expandEnvVar(updDirs[iDir]) );
    QStringList hlpLst = curDir.entryList( nameFilter, QDir::Files);
    for (int ii = 0; ii < hlpLst.count(); ii++) {
      updFiles.append( curDir.path() + r_dir::separator() + hlpLst[ii] );
    }
  }

  // Prepare the List of All Keywords
  // --------------------------------
  Q3PtrList<t_keyword>*  keys = inpfile->getKeys();
  QStringList keyNames;
  QStringList oldValues;
  QStringList newValues;
  for (unsigned iKey = 0; iKey < keys->count(); iKey++) {
    QString name = keys->at(iKey)->getName();
    if ( name.find("NEW_") == 0 ) {
      name = name.mid(4);
      if ( inpfile->getKeySel0("CHK_" + name) == "1" ) {
        keyNames.append(name);
        oldValues.append( inpfile->getKeySel0("OLD_" + name) );
        newValues.append( inpfile->getKeySel0("NEW_" + name) );
      }
    }
  }

  if ( inpfile->getKeySel0("ADVANCED_USERS") == "1") {
    t_keyword* keyUni = inpfile->getKey("UNIVERSAL");
    if (keyUni) {
      keyUni->updateSel();
      keyUni->expandSelList();
      QStringList lines = QStringList( keyUni->getSelList() );
      for (int ii = 0; ii < lines.count(); ii++) {
        QStringList hlp;
        unilineSplit( lines[ii], hlp);
        if (!hlp[0].isEmpty()) {
          keyNames.append( hlp[0] );
          oldValues.append( hlp[1] );
          newValues.append( hlp[2] );
        }
      }
    }
  }

  QString msg;

  // Loop Over All Files
  // -------------------
  for (int iFil = 0; iFil < updFiles.count(); iFil++) {
    t_inpfile* inOutFile = new t_inpfile( updFiles[iFil] );
    Q3PtrList<t_keyword>* inOutKeys = inOutFile->getKeys();
    for (int ii = 0; ii < keyNames.count(); ii++) {
      for (unsigned iKey = 0; iKey < inOutKeys->count(); iKey++) {
        if ( inOutKeys->at(iKey)->getName() == keyNames[ii] &&
             (oldValues[ii].isEmpty() ||
              oldValues[ii] == inOutKeys->at(iKey)->getValue()) ) {
          inOutKeys->at(iKey)->setValue( newValues[ii] );
          msg += "File: " + updFiles[iFil] + " Key: " + keyNames[ii] +
                   " Old: " + oldValues[ii] + " New: " + newValues[ii] + "\n";
          break;
        }
      }
    }
    int oldLogModus = initmenu.getLogModus();
    initmenu.setLogModus(t_initmenu::quiet);
    inOutFile->save();
    initmenu.setLogModus(oldLogModus);
    delete inOutFile;
  }

  // Write the Log
  // -------------
  inpfile->expandSelList(false);
  r_file outFile( expandEnvVar(inpfile->getStdOutFileName()) );
  if ( !outFile.open(QIODevice::WriteOnly | QIODevice::Text) ) {
    errormsg("Cannot write into file " + inpfile->getStdOutFileName());
    delete inpfile;
    return ;
  }
  outFile.writeBlock( msg, msg.length() );
  outFile.close();

  delete inpfile;
}
