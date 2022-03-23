
/* -------------------------------------------------------------------------
 * Bernese GPS Software Version 5.0
 * -------------------------------------------------------------------------
 *
 * Class:      t_pcffile
 *
 * Purpose:    This class implements handling of Process Control File
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

#include <qregexp.h>

#include "pcffile.h"
#include "errormsg.h"
#include "menutils.h"
#include "initmenu.h"
#include "r_dir.h"

// Constructor
////////////////////////////////////////////////////////////////////////////
t_pcffile::t_pcffile(const char* pcfFileName) : t_inpfile(pcfFileName) {
  _listOfScripts = this->getKey("LIST_OF_SCRIPTS");
}

// Destructor
////////////////////////////////////////////////////////////////////////////
t_pcffile::~t_pcffile() {
}

// Return Number of Scripts
////////////////////////////////////////////////////////////////////////////
int t_pcffile::getNumberOfScripts() {
  if (_listOfScripts == 0 || _listOfScripts->getSelList().count() == 0) {
    errormsg("No scripts in PCF");
    return 0;
  }
  return (int) _listOfScripts->getSelList().count();
}

// Name of the original PCFile (without path and extension)
////////////////////////////////////////////////////////////////////////////
QString t_pcffile::getOrigName(bool stripExtension) {
   return stripPath( getKey("PCFFILRS")->getSelList()[0], stripExtension );
}

// Add Script-specific Options
////////////////////////////////////////////////////////////////////////////
int t_pcffile::addToOptions(QMap<QString, QString>& options,
                            int iScript, bool& specialCamp) {

  // Basic Options
  // -------------
  QStringList optList;
  unilineSplit( _listOfScripts->getSelList()[iScript], optList );

  if (optList.size() < 3 ||
      optList[0].isEmpty() || optList[1].isEmpty() || optList[2].isEmpty()) {
    errormsg("PID, SCRIPT, and OPT_DIR are mandarory!\n" +
             QString().sprintf(
             "They are not given for script number %d",iScript+1));
    return 1;
  }

  if (!options.contains("PID"))     options.insert("PID"     , optList[0]);
  if (!options.contains("SUB_PID")) options.insert("SUB_PID" , "000"     );
  if (!options.contains("SCRIPT"))  options.insert("SCRIPT"  , optList[1]);
  if (!options.contains("OPT_DIR")) options.insert("OPT_DIR" , optList[2]);

  if (optList[3].isEmpty()) {
    specialCamp = false;
  }
  else {
    specialCamp = true;

    options.insert("CAMPAIGN", optList[3]);

    // It may happen that the path to this campaign differs from the default
    // ---------------------------------------------------------------------
    t_keyword* key = initmenu.getKey("CAMPAIGN");
    int index = -1;
    for (int ii = 0; ii < key->getSelList().count(); ii++) {
      QString hlp = key->getSelList()[ii];
      if (hlp.find( options["CAMPAIGN"] ) != -1) {
        if (index != -1) {
          errormsg("More than one campaign found in the list" +
                                                        options["CAMPAIGN"]);
          return 1;
        }
        else {
          index = ii;
        }
      }
    }
    if (index == -1) {
      errormsg("Campaign not found in the list: " + options["CAMPAIGN"]);
      return 1;
    }
    QStringList hlp;
    unilineSplit(key->getSelList()[index], hlp);
    hlp[0].truncate( hlp[0].length() - options["CAMPAIGN"].length() );
    options["CAMP_DRV"] = hlp[0];
    options["CAMP_PTH"] = expandEnvVar(hlp[0]);
  }

  if (!options.contains("REQ_CPU")) options.insert("REQ_CPU" , optList[4]);

  if (optList[5] == "S") {
    options.insert("SINGLETON", "Y");
  }
  else {
    options.insert("SINGLETON", "N");
  }

  QString waitStr;
  for (int ii = 6; ii < optList.count(); ii++) {
    waitStr += optList[ii] + " ";
  }
  if (!options.contains("WAIT_FOR")) options.insert("WAIT_FOR", waitStr);

  // Output Directory (for script output)
  // ------------------------------------
  QString prtDir = initmenu.getKeySel0("DIR_BPEPRT");
  if (prtDir.isEmpty()) {
    errormsg("Keyword DIR_BPEPRT not found\n"
             "update the correspondent menu input file "
             "(MENU_EXT.INP by default)");
    return 1;
  }
  QString fullCmp = options["CAMP_PTH"]+options["CAMPAIGN"]+r_dir::separator();
  prtDir = fullCmp + prtDir + r_dir::separator();
  r_dir tstPrtDir(prtDir);
  if (!tstPrtDir.exists()) {
    errormsg("BPE-Protocol Directory " + prtDir + " does not exist");
    return 1;
  }
  options.insert("PTH_BPEPRT", prtDir);
  options.insert("EXT_BPEPRT", initmenu.getKeySel0("EXT_BPEPRT"));

  QString logDir = initmenu.getKeySel0("DIR_BPELOG");
  if (logDir.isEmpty()) {
    errormsg("Keyword DIR_BPELOG not found\n"
             "update the correspondent menu input file "
             "(MENU_EXT.INP by default)");
    return 1;
  }
  logDir = fullCmp + logDir + r_dir::separator();
  r_dir tstLogDir(logDir);
  if (!tstLogDir.exists()) {
    errormsg("BPE-Log Directory " + logDir + " does not exist");
    return 1;
  }
  options.insert("PTH_BPELOG", logDir);
  options.insert("EXT_BPELOG", initmenu.getKeySel0("EXT_BPELOG"));

  // Specials: SKIP, PARALLEL etc.
  // -----------------------------
  t_keyword* key = this->getKey("SPECIALS");
  if (key != 0) {
    for (int jj = 0; jj < key->getSelList().count(); jj++) {
      QStringList hlp;
      unilineSplit(key->getSelList()[jj], hlp);

      if ( hlp.count() > 3 && hlp[3] == "SKIP"    &&
                              hlp[0] == optList[0] ) {
          if (!options.contains("SKIP")) options.insert("SKIP", "TRUE");
      }

      if ( hlp.count() > 5 && hlp[3] == "PARALLEL" &&
                              hlp[5] == optList[0] ) {
        options.insert("SLAVE", hlp[0]);
      }

    }
  }

  // Script Parameters: PID SCRIPT OPT_DIR PARAM1 PARAM2 PARAM3 PARAM4 ...
  // ---------------------------------------------------------------------
  key = this->getKey("PARAMETERS");
  if (key != 0) {
    for (int jj = 0; jj < key->getSelList().count(); jj++) {
      QStringList hlp;
      unilineSplit(key->getSelList()[jj], hlp);
      if (hlp.count() > 0 && hlp[0] == optList[0]) {
        for (int kk = 3; kk < hlp.count(); kk++) {
          QString keyPar;
          keyPar.sprintf("PARAM%d", kk-2);
          if (!options.contains(keyPar)) options.insert(keyPar, hlp[kk]);
        }
      }
    }
  }

  return 0;
}

