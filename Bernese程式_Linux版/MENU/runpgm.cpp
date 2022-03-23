
/* -------------------------------------------------------------------------
 * Bernese GPS Software Version 5.0
 * -------------------------------------------------------------------------
 *
 * Function:   runpgm
 *
 * Purpose:    Run External (usually Fortran) Program
 *
 * Author:     L. Mervart
 *
 * Created:    27-JUL-2000
 *
 * Changes:
 *
 * Copyright:  Astronomical Institute
 *              University of Berne
 *                  Switzerland
 * -----------------------------------------------------------------------*/

#include <QtCore>
#include <QtGui>
#include <stdlib.h>

#include "runpgm.h"
#include "initmenu.h"
#include "menutils.h"
#include "errormsg.h"
#include "textwin.h"
#include "bpe.h"
#include "lockfile.h"
#include "r_file.h"
#include "r_dir.h"

#ifdef WIN32

#ifdef UNICODE
#undef UNICODE
#endif

#include <windows.h>
#include <string.h>
#endif

// Run external program
////////////////////////////////////////////////////////////////////////////
void runpgm(QString inpFileName, const QString& errFileName,
            t_canvas* canvas, bool backgroundAllowed,
            bool mayUseEcho) {

  // Resolve the Program Name
  // ------------------------
  QString hlpFileName = inpFileName;
  r_dir   hlpDir( expandEnvVar(hlpFileName) );
  QString exeProgram = hlpDir.dirName().stripWhiteSpace();

  int     dotPosition = exeProgram.find('.');
  if (dotPosition != -1) {
    exeProgram.truncate(dotPosition);
  }
  exeProgram = initmenu.getCmdPath(exeProgram) + exeProgram ;

  QString message = "Running " + exeProgram + " ...";

  // Check if the Executable File exists
  // -----------------------------------
  QString exeExt = "";
#ifdef WIN32
  if (!initmenu.modemMode()) {
    exeExt = ".EXE";
  }
#endif
  if (! r_file::exists( expandEnvVar(exeProgram) + exeExt ) ) {
    errormsg("Program not found: " + expandEnvVar(exeProgram) + exeExt);
    return;
  }

  // Delete old error message file
  // -----------------------------
  if (r_file::exists(errFileName)) {
    r_file::remove(errFileName);
  }

  // Foreground or background ?
  // --------------------------
  bool background = false;
  if (backgroundAllowed && initmenu.getKeySel0("CMD_BACKGROUND") == "1") {
    background = true;
  }
  else if (initmenu.getKeySel0("CMD_USE_CPUFILE") == "1") {
    t_cpufile cpufile(initmenu.getKeySel0("CPU_FILE"));
    if (!cpufile.ok()) {
      errormsg("Wrong CPU File: " + cpufile.getName());
      return;
    }

    QString reqCpuName = initmenu.getKeySel0("CMD_CPUNAME");
    QString cpuNickname;
    QString cpuCommand;
    int     maxWait;

    cpufile.findFreeCPU("", reqCpuName, cpuNickname, cpuCommand, maxWait);

    cpuCommand.replace(QRegExp("<COMMAND>"), exeProgram);
    cpuCommand.replace(QRegExp("<ARGV>"), inpFileName);
    cpuCommand.replace(QRegExp("<LOG>"), initmenu.getKeySel0("CMD_LOGFILE"));

    exeProgram  = cpuCommand;
    inpFileName = "";
#ifdef WIN32
    background = true;
#else
    background = false;
#endif
  }

  // Run the Program
  // ---------------
  canvas->showMessage(message);
  QApplication::setOverrideCursor( Qt::waitCursor );
  crepro(exeProgram, inpFileName, background, mayUseEcho);
  QApplication::restoreOverrideCursor();
  //////  canvas->erase();

  // Display new error message file
  // ------------------------------
  if ( r_file::exists(errFileName) ) {
    r_file errFile(errFileName);
    if (errFile.size() > 0) {
      new t_textwin(0, true, errFileName, t_textwin::browse);
    }
  }
}

// Start a new Process
////////////////////////////////////////////////////////////////////////////
#ifndef WIN32
int crepro(const QString& exeProgram, const QString& inpFileName,
           bool background, bool mayUseEcho, bool /* detached */) {
#else
int crepro(const QString& exeProgram, const QString& inpFileName,
           bool background, bool mayUseEcho, bool detached) {
#endif

  QString command;

  // Unix Version and Modem Mode
  // ---------------------------
  if (mayUseEcho) {
    command = expandEnvVar("echo " + inpFileName + " | " + exeProgram);
  }
  else {
    command = expandEnvVar(exeProgram + " " + inpFileName);
  }
#ifndef WIN32
  if (background) {
    command += " &";
  }
#endif
  if (initmenu.modemMode()){
    return r_file::run(command);
  }
  else {

#ifndef WIN32
    return system( command.ascii() );
#else
    QProcess prc;
    QString l_exeProgram = expandEnvVar(exeProgram);
    QStringList args;
    if (inpFileName.indexOf('"') == -1) {
      args = inpFileName.split(' ', QString::SkipEmptyParts);
    }
    else {
      args << inpFileName;
    }
    QMutableListIterator<QString> it(args);
    while (it.hasNext()) {
      QString& arg = it.next();
      arg = expandEnvVar(arg);
    }
    ////    QMessageBox::warning(0, "", l_exeProgram + "\n\n" + args.join("\n"));
    if (detached) {
      prc.startDetached(l_exeProgram, args);
    }
    else {
      prc.start(l_exeProgram, args);
      prc.waitForFinished(-1);
    }
    return 0;
#endif
  }
}

// Start a new BPE Server
////////////////////////////////////////////////////////////////////////////
void runbpe(const QString& inpFileName) {

  // Open the auxiliary input file
  // -----------------------------
  r_file inpFile( initmenu.auxInpName() );
  if ( !lockFile(inpFile.name(), 50, 0.1) ) {
    errormsg("Cannot lock file " + inpFile.name());
    return;
  }
  if ( !inpFile.open(QIODevice::WriteOnly | QIODevice::Text) ) {
    errormsg("Cannot write into file " + inpFile.name());
    return;
  }

  // Write the keywords
  // ------------------
  t_keyword keyModus("MODUS");
  if (!initmenu.modemMode()){
    keyModus.storeSel(QString("INTERACTIVE"));
  }
  inpFile << keyModus;

  t_keyword keyRunBPE("RUN_BPE");
  keyRunBPE.storeSel(inpFileName);
  inpFile << keyRunBPE;

  inpFile.close();
  unlockFile(inpFile.name());

  // Start the Server
  // ----------------
  QString exeFile = initmenu.modemMode() ? QString("${XQ}/menu") :
                                                          initmenu.menuName();

  QString menuInpFileName = initmenu.primaryInputFileName() + "_BPE";
  if (fileCopy(initmenu.primaryInputFileName(), menuInpFileName) != success) {
    errormsg("Cannot write into file " + menuInpFileName);
    return;
  }

  crepro(exeFile,
         menuInpFileName + " " + inpFile.name(),
         true, false, true);
}
