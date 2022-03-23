
/* -------------------------------------------------------------------------
 * Bernese GPS Software Version 5.0
 * -------------------------------------------------------------------------
 *
 * Function:   main
 *
 * Purpose:    The application menu starts here. argv[1] is the name of the
 *             primary menu input file (e.g. MENU.INP), argv[2] is used to
 *             switch into the non-interactive mode. It is the name of the
 *             auxiliary menu input file. In this file the names of the
 *             main program input files (original and processed) are
 *             specified. Additionally the auxiliary menu input file may
 *             contain any number of keywords for the menu initialization.
 *             Keyword specified in the auxiliary menu input file has
 *             higher priority than any other menu-initialization
 *             keywords.
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

#include <iostream>
#include <QtCore>
#include <q3network.h>

#include "mainwin.h"
#include "myapp.h"
#include "initmenu.h"
#include "inpfile.h"
#include "errormsg.h"
#include "menutils.h"
#include "bpe.h"
#include "r_file.h"
#include "bnp.h"
#include "updpan.h"

QString celMechRoot;
unsigned MAXFILENAMELENGTH = 32;

int main ( int argc, char* argv[] ) {

  initmenu.setMenuName(argv[0]);

  initmenu.setLogModus(t_initmenu::normal);

  // Modem-friendly Mode
  // -------------------
  if (argc >= 3 && QString(argv[2]) == "MODEM") {
    if (argc >= 4) {
      initmenu.setHost(argv[3]);
    }
    if (argc >= 5) {
      initmenu.setPort(QString(argv[4]).toInt());
    }
    argc = 2;
    initmenu.setModemMode(true);
    q3InitNetworkProtocols();
    Q3NetworkProtocol::registerNetworkProtocol( "file",
                                        new Q3NetworkProtocolFactory<bnp> );
    Q3NetworkProtocol::registerNetworkProtocol( "bnp",
                                        new Q3NetworkProtocolFactory<bnp> );
  }

  // Two Arguments -> Standard Interactive Mode
  // ------------------------------------------
  if (argc == 2) {
    t_myapp myapp( argc, argv );
    myapp.setStyle(new QWindowsStyle());
#ifdef WIN32
    myapp.setWindowIcon(QPixmap(":menu.png"));
#endif
    if (initmenu.readInput(argv[1])) {
      return 1;
    }
    t_mainwin* mainwin = new t_mainwin("");
    myapp.setMainWidget(mainwin);
    initmenu.setIntModus(t_initmenu::INT);
    return myapp.exec();
  }

  // Three Arguments
  // ---------------
  else if (argc == 3) {
    if (initmenu.readInput(argv[1], argv[2])) {
      return 1;
    }
    QString inpFileName = initmenu.getKeySel0( "INP_FILE_NAME" );
    QString outFileName = initmenu.getKeySel0( "OUT_FILE_NAME" );
    QString modus       = initmenu.getKeySel0( "MODUS" );
    QString runPCF      = initmenu.getKeySel0( "RUN_BPE" );
    QString printPID    = initmenu.getKeySel0( "PRINT_PID" );
    QString resetCPU    = initmenu.getKeySel0( "RESETCPU" );
    QString updPan      = initmenu.getKeySel0( "UPDPAN" );

    // Interactive Mode
    // ----------------
    if (modus == "INTERACTIVE") {
      initmenu.setIntModus(t_initmenu::INT);
      t_myapp myapp( argc, argv );
      myapp.setStyle(new QWindowsStyle());
#ifdef WIN32
      myapp.setWindowIcon(QPixmap(":menu.png"));
#endif
      if (runPCF.isEmpty()) {
        t_mainwin* mainwin = 0;
        QString updPanInp = stripPath(initmenu.getInpFileName("UPDPAN_INP"));
        if (inpFileName.indexOf(updPanInp) != -1) {
          mainwin = new t_mainwin("");
          mainwin->slotMenu(MENU_ID_UPDPAN);
        }
        else {
          mainwin = new t_mainwin(inpFileName);
        }
        myapp.setMainWidget(mainwin);
      }
      else {
        if (printPID == "1") {
          errormsg("PID = " + QString().sprintf("%d", initmenu.pid()));
        }
        new t_bpe(0, runPCF);
      }
      return myapp.exec();
    }

    // Non-Interactive Mode
    // --------------------
    else {
      if (! runPCF.isEmpty()) {
        t_myapp myapp( argc, argv, false );
        if (printPID == "1") {
          errormsg("PID = " + QString().sprintf("%d", initmenu.pid()));
        }
        new t_bpe(0, runPCF);
////        while (true) {
////          myapp.processEvents();
////#ifndef WIN32
////          mysleep(1);
////#endif
////        }
        return myapp.exec();  // in Qt 4.0 needs too much CPU
      }
      else if (! resetCPU.isEmpty()) {
        t_cpufile cpufile(resetCPU);
        if (cpufile.ok()) {
          cpufile.resetCPU();
          errormsg("CPU File " + cpufile.getName() + " has been reset");
          return 0;
        }
        else {
          return 1;
        }
      }
      else if (! updPan.isEmpty()) {
        updpan(updPan);
        return 0;
      }
      else {
        if (inpFileName.isEmpty() || outFileName.isEmpty()) {
          errormsg("At least one name is empty: " +
                    inpFileName + " " + outFileName);
          return 1;
        }

        t_inpfile* inpfile = new t_inpfile( inpFileName );
        inpfile->save( outFileName );
        return 0;
      }
    }
  }

  // Print the Help
  // --------------
#ifdef WIN32
  t_myapp myapp( argc, argv );
  myapp.setWindowIcon(QPixmap(":menu.png"));
  initmenu.setIntModus(t_initmenu::INT);
#endif
  errormsg("Usage:                                                \n"
           "menu MENU.INP                ... interactive mode     \n"
           "menu MENU.INP MENU_xxx.INP   ... non-interactive mode \n");
  return 1;
}
