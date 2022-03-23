
/* -------------------------------------------------------------------------
 * Bernese GPS Software Version 5.0
 * -------------------------------------------------------------------------
 *
 * Function:   lockFile, unlockFile
 *
 * Purpose:    Lock or unlock a file
 *
 * Author:     L. Mervart
 *
 * Created:    17-NOV-2001
 *
 * Changes:
 *
 * Copyright:  Astronomical Institute
 *              University of Berne
 *                  Switzerland
 * -----------------------------------------------------------------------*/

#include <qstringlist.h>

#include "lockfile.h"
#include "initmenu.h"
#include "errormsg.h"
#include "menutils.h"
#include "r_file.h"

extern QStringList lockFiles;

// Lock the File
////////////////////////////////////////////////////////////////////////////
bool lockFile(const QString& fileName, int maxTrials, double sleepTime) {

  r_file lk_file( fileName + "_lk" );

  for (int iTrial = 0; iTrial < maxTrials; iTrial++) {
    if (!lk_file.exists() &&
        lk_file.open(QIODevice::WriteOnly | QIODevice::Append)) {

      QString pidStr; pidStr.sprintf("%d\n", initmenu.pid());
      lk_file << pidStr;
      lk_file.close();

      if (lk_file.open(QIODevice::ReadOnly)) {
        int pid = 0;
        lk_file >> pid;
        lk_file.close();

        if (pid == initmenu.pid()) {
          lockFiles.append(lk_file.absFilePath());
          return true;
        }
      }
    }
    mysleep(sleepTime);
  }
  errormsg("lockFile: file\n" + fileName + "\ncannot be locked");
  return false;
}

// Unlock the File
////////////////////////////////////////////////////////////////////////////
bool unlockFile(const QString& fileName) {

  r_file lk_file( fileName + "_lk" );

  if ( lk_file.exists() ) {

    int pid = 0;
    if (lk_file.open(QIODevice::ReadOnly)) {
      lk_file >> pid;
      lk_file.close();
    }

    if (pid != initmenu.pid() || !lk_file.remove() ) {
      errormsg("lockFile: cannot unlock file\n" + fileName);
      return false;
    }
  }

  QStringList::Iterator it = lockFiles.find(lk_file.absFilePath());
  if (it != lockFiles.end()) {
    lockFiles.remove(it);
  }
  return true;
}
