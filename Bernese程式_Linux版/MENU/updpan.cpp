
/* -------------------------------------------------------------------------
 * Bernese GPS Software Version 5.0
 * -------------------------------------------------------------------------
 *
 * Functions:  updpan
 *
 * Purpose:    Update Panel (Input) File
 *
 * Author:     L. Mervart
 *
 * Created:    27-JUN-2000
 *
 * Changes:
 *
 * Copyright:  Astronomical Institute
 *              University of Berne
 *                  Switzerland
 * -----------------------------------------------------------------------*/

#include <qapplication.h>
#include <qstringlist.h>
#include <qmessagebox.h>
#include <qregexp.h>

#include "updpan.h"
#include "inpfile.h"
#include "initmenu.h"
#include "errormsg.h"
#include "menutils.h"
#include "myfildlg.h"
#include "r_file.h"
#include "r_dir.h"

void writeLog(t_inpfile* inpfile, const QString& msg);

//
////////////////////////////////////////////////////////////////////////////
void updpan(const QString& inpFileName) {

  t_inpfile*  inpfile = 0;

  QString     updOpt  = "UPDATE";
  QString     exiOpt  = "EXISTING";
  QString     msg;

  QStringList masterPanels;
  QString     masterPanelsDir;
  QStringList newPanDir;

  // Select Master File and Update Directory
  // ---------------------------------------
  if (inpFileName == QString::null) {
    QString masterPan = t_myfildlg::getOpenFileName(QString::null,
                     QString::null, 0, 0, "Select Master (Skeleton) File");
    if (masterPan.isEmpty()) {
      return;
    }
    else {
      masterPanels.append( stripPath(masterPan) );
      masterPanelsDir = stripFileName(masterPan) + r_dir::separator();
    }
    QString panDir = t_myfildlg::getExistingDirectory(QString::null,
              0, 0, "Select directory with file(s) that should be updated");
    if (panDir.isEmpty()) {
      return;
    }
    else {
      newPanDir.append(panDir);
    }
  }

  // Read options from input file
  // ----------------------------
  else {

    inpfile = new t_inpfile(inpFileName);

    // Return if inpfile not correctly read
    // ------------------------------------
    if (inpfile->getName().isEmpty()) {
      delete inpfile;
      return;
    }

    t_keyword*  keyOLDPAN       = inpfile->getKey("OLDPAN");
    masterPanels    = keyOLDPAN->getSelList();
    masterPanelsDir = initmenu.getPath(keyOLDPAN->getDesc()->path());

    QString     updDir;
    QString     updLst;
    if (inpfile->getKeySel0("RADIO_UPDDIR") == "1") {
      updDir = inpfile->getKeySel0("UPDDIR");
    }
    else if (inpfile->getKeySel0("RADIO_UPDLST") == "1") {
      t_keyword* keyUPDLST = inpfile->getKey("UPDLST");
      if (keyUPDLST) {
        keyUPDLST->updateSel();
        keyUPDLST->expandSelList();
        updLst = keyUPDLST->getSelList()[0];
      }
    }

    if ( updDir.isEmpty() && updLst.isEmpty() ) {
      errormsg("updpan: wrong options");
      delete inpfile;
      return;
    }

    updOpt = inpfile->getKey("UPDOPT")->getSelList()[0];
    exiOpt = inpfile->getKey("EXIOPT")->getSelList()[0];

    // Confirm the dangerous "COPY" option
    // -----------------------------------
    if (updOpt == "COPY") {
      QApplication::restoreOverrideCursor();
      if ( QMessageBox::warning(0, "Question", "Options stored in your panels "
                                "will be overwritten. Do you wish to continue ?",
                                QMessageBox::Yes | QMessageBox::Default,
                                QMessageBox::No ) != QMessageBox::Yes ) {
        msg = "Run canceled on user's request";
        writeLog(inpfile, msg);
        delete inpfile;
        return;
      }
      QApplication::setOverrideCursor( Qt::waitCursor );
    }

    // List of Directories with new Panels
    // -----------------------------------
    if ( ! updDir.isEmpty() ) {
      appendDirs( newPanDir, updDir.stripWhiteSpace() );
    }

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
        appendDirs( newPanDir, line.stripWhiteSpace() );
      }
    }
  }

  // Loop over all input panels
  // --------------------------
  for ( int iMst = 0; iMst < (int) masterPanels.count(); iMst++) {
    for ( int iNew = 0; iNew < (int) newPanDir.count(); iNew++) {

      t_inpfile masterPan(masterPanelsDir + masterPanels[iMst]);
      if (masterPan.getName().isEmpty()) {
        delete inpfile;
        return;
      }

      // Loop over all possible Job IDs
      // ------------------------------
      r_dir newDir( expandEnvVar(newPanDir[iNew]) );
      newDir.setNameFilter(masterPanels[iMst] + "*");

      QStringList newNames;
      QStringList entry = newDir.entryList(QDir::Files);
      QRegExp rex(masterPanels[iMst] + "($|_[^\\.]+)", false);
      for (QStringList::Iterator it = entry.begin(); it != entry.end(); ++it) {
        if ( rex.exactMatch(*it) ) {
          newNames.append(*it);
        }
      }

      if (!newDir.exists(masterPanels[iMst]) && exiOpt == "ALL") {
        newNames.append(masterPanels[iMst]);
      }

      for (int jj = 0; jj < newNames.count(); jj++) {
        QString newPanName = newDir.path() + r_dir::separator() + newNames[jj];

        r_file newPanFile( expandEnvVar(newPanName) );
        if (newPanFile.exists() && updOpt == "UPDATE") {
          fileCopy(newPanName, stripExtension(newPanName) + ".old");
          t_inpfile newPan(newPanName);
          Q3PtrList<t_keyword>*  mstKeys = masterPan.getKeys();
          Q3PtrList<t_keyword>*  newKeys = newPan.getKeys();

          int foundKeys = 0;
          for ( int kMst = 0; kMst < (int) mstKeys->count(); kMst++ ) {
            for ( int kNew = 0; kNew < (int) newKeys->count(); kNew++ ) {
              QString mstName = mstKeys->at(kMst)->getName();
              QString newName = newKeys->at(kNew)->getName();
              if (mstName == newName) {
                ++foundKeys;
                QString widget = mstKeys->at(kMst)->getDesc()->widget();
                if (widget                    != "comment" &&
                    mstName.left(6)           != "DESCR_"  &&
                    mstName.left(4)           != "MSG_"    &&
                    mstName.find("_TXT_COL_") == -1         ) {
                  mstKeys->at(kMst)->setValueList(
                              newKeys->at(kNew)->getValList() );
                  mstKeys->at(kMst)->setSelList(
                              newKeys->at(kNew)->getSelList() );
                }
                break;
              }
            }
          }
          msg += "UPDATE " + newPanName +
            QString().sprintf(" # keys = %3d, new = %2d, del = %2d\n",
                              mstKeys->count(),
                              mstKeys->count() - foundKeys,
                              newKeys->count() - foundKeys);

          int oldLogModus = initmenu.getLogModus();
          initmenu.setLogModus(t_initmenu::quiet);
          masterPan.save(newPanName);
          initmenu.setLogModus(oldLogModus);
        }
        else {
          msg += "COPY " + newPanName + "\n";
          fileCopy(expandEnvVar(masterPan.getName()),
                   expandEnvVar(newPanName) );
        }
      }
    }
  }

  if (inpfile) {
    writeLog(inpfile, msg);
  }
  else {
    errormsg(msg);
  }

  delete inpfile;
}

// Write the log
////////////////////////////////////////////////////////////////////////////
void writeLog(t_inpfile* inpfile, const QString& msg) {
  inpfile->expandSelList(false);
  r_file outFile( expandEnvVar(inpfile->getStdOutFileName()) );
  if ( !outFile.open(QIODevice::WriteOnly | QIODevice::Text) ) {
    errormsg("Cannot write into file " + inpfile->getStdOutFileName());
  }
  else {
    outFile.writeBlock( msg, msg.length() );
    outFile.close();
  }
}

