
/* -------------------------------------------------------------------------
 * Bernese GPS Software Version 5.0
 * -------------------------------------------------------------------------
 *
 * Class:      t_initmenu
 *
 * Purpose:    This class handles the general menu options given in menu
 *             input files. One instance of this class - initmenu - is a
 *             global variable used in many classes to access the menu
 *             configuration.
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

#include <stdlib.h>
#include <qregexp.h>
#include <q3urloperator.h>

#ifdef WIN32
#include <process.h>
#else
#include <unistd.h>
#endif

#include "initmenu.h"
#include "menutils.h"
#include "session.h"
#include "errormsg.h"
#include "r_dir.h"
#include "bnp.h"

// Global Variable
////////////////////////////////////////////////////////////////////////////
t_initmenu initmenu;

// Static constant dummy variable
////////////////////////////////////////////////////////////////////////////
const QMap<QString, QString> t_initmenu::_emptyMap;

// Constructor
////////////////////////////////////////////////////////////////////////////
t_initmenu::t_initmenu() {
  _exc = false;
  _pid = getpid();
  _primaryInputFileName   = "";
  _auxiliaryInputFileName = "";
  _initFonts = false;
  setIntModus(t_initmenu::NOINT);
  _urlOp = 0;
  _host  = "localhost";
  _port  = 1967;
  _environment = 0;
}

// Destructor
////////////////////////////////////////////////////////////////////////////
t_initmenu::~t_initmenu() {
}

// Delete all input files
////////////////////////////////////////////////////////////////////////////
void t_initmenu::clearInpfiles() {
  for (unsigned ii = 0; ii < _inpfiles.count(); ii++) {
    delete _inpfiles.at(ii);
  }
  _inpfiles.clear();
}

// Read All Menu Input Files - Non Interactive Mode
////////////////////////////////////////////////////////////////////////////
int t_initmenu::readInput(const QString& inpFileName1,
                          const QString& inpFileName2) {
  _primaryInputFileName   = inpFileName1;
  _auxiliaryInputFileName = inpFileName2;
  return readInput();
}


// Read All Menu Input Files - Interactive Mode
////////////////////////////////////////////////////////////////////////////
int t_initmenu::readInput(const QString& inpFileName) {
  _primaryInputFileName   = inpFileName;
  return readInput();
}

// Read All Menu Input Files (Both Modes)
////////////////////////////////////////////////////////////////////////////
int t_initmenu::readInput() {

  static bool first = true;

  if (modemMode() && !first) {
    return 0;
  }

  clearInpfiles();

  if ( ! _auxiliaryInputFileName.isEmpty() ) {
    _inpfiles.append( new t_inpfile(_auxiliaryInputFileName  ) );
    if ( _inpfiles.last()->getName() == "" ) {
      return 1;
    }
  }
  _inpfiles.append(new t_inpfile( _primaryInputFileName));
  if ( _inpfiles.last()->getName() == "" ) {
    return 1;
  }
  QString hlpStr;
  hlpStr = expandEnvVar( getKeySel0("MENU_EXT_INP") );
  if (!hlpStr.isEmpty()) {
    _inpfiles.append(new t_inpfile( hlpStr));
    if ( _inpfiles.last()->getName() == "" ) {
      return 1;
    }
  }
  hlpStr = expandEnvVar( getKeySel0("MENU_VAR_INP") );
  if (!hlpStr.isEmpty()) {
    _inpfiles.append(new t_inpfile( hlpStr));
    if ( _inpfiles.last()->getName() == "" ) {
      return 1;
    }
  }

  // Active Campaign and List of Sessions
  // ------------------------------------
  if (first) {
    _activeCamp = getKeySel0("ACTIVE_CAMPAIGN");
    updateListOfSessions();
  }

  hlpStr = expandEnvVar( getKeySel0("MENU_CMP_INP") );
  if (!hlpStr.isEmpty()) {
    _inpfiles.append(new t_inpfile( hlpStr));
    if ( _inpfiles.last()->getName() == "" ) {
      return 1;
    }
  }
  hlpStr = expandEnvVar( getKeySel0("MENU_PGM_INP") );
  if (!hlpStr.isEmpty()) {
    _inpfiles.append(new t_inpfile( hlpStr));
    if ( _inpfiles.last()->getName() == "" ) {
      return 1;
    }
  }

  if (first) {
    first = false;

    // MJD and Session
    // ---------------
    _MJD = getKeySel0("MODJULDATE").toDouble();
    _sessChar = getKeySel0("SESSION_CHAR").stripWhiteSpace().right(1);
    _jobID    = getKeySel0("JOB_ID").stripWhiteSpace().right(2);
  }

  // Envirionment List (for remote mode)
  // -----------------------------------
  if ( modemMode() && _environment == 0) {
    _environment = new Q3Dict<QString>;
    t_keyword* envKey = getKey("ENVIR_LIST");
    if (envKey) {
      QString names = envKey->getSelList().join(" ").replace(QRegExp("\""),"");
      Q3UrlOperator* op = (Q3UrlOperator*) urlOp();
      connect(op,   SIGNAL( data(const QByteArray&, Q3NetworkOperation*) ),
              this, SLOT(slotData(const QByteArray&, Q3NetworkOperation*)));
      op->setPath("?ENVIRONMENT " + names);
      _oper =  op->get();
      bnp::waitForFinish(_oper) ;
    }
  }

  return 0;
}

// Read and initialize the fonts
////////////////////////////////////////////////////////////////////////////
void t_initmenu::initFonts() {

  if (_initFonts) return;

  _initFonts = true;

  QString fontTypeBase = getKeySel0("FONTTYPE_BASE");
  int fontSizeBase = getKeySel0("FONTSIZE_BASE").toInt();
  if (fontTypeBase.isEmpty()) {
    fontTypeBase = DEFAULT_FONTTYPE_BASE;
  }
  if (fontSizeBase == 0) {
    fontSizeBase = DEFAULT_FONTSIZE_BASE;
  }

  _fontBase = QFont(fontTypeBase, fontSizeBase);

  if (getKeySel0("FONTBOLD_BASE") == "1") {
    _fontBase.setBold(true);
  }
  if (getKeySel0("FONTITALIC_BASE") == "1") {
    _fontBase.setItalic(true);
  }
  if (getKeySel0("FONTSTRIKEOUT_BASE") == "1") {
    _fontBase.setStrikeOut(true);
  }
  if (getKeySel0("FONTUNDERLINE_BASE") == "1") {
    _fontBase.setUnderline(true);
  }

  QString fontTypeLarge = getKeySel0("FONTTYPE_LARGE");
  int     fontSizeLarge = getKeySel0("FONTSIZE_LARGE").toInt();
  if ( fontTypeLarge.isEmpty() ) {
    fontTypeLarge = DEFAULT_FONTTYPE_LARGE;
  }
  if (fontSizeLarge == 0) {
    fontSizeLarge = DEFAULT_FONTSIZE_LARGE;
  }

  _fontLarge = QFont(fontTypeLarge, fontSizeLarge);

  if ( QFontMetrics(_fontLarge).width('i') !=
       QFontMetrics(_fontLarge).width('W') ) {
    _fontLarge = QFont(DEFAULT_FONTTYPE_LARGE, DEFAULT_FONTSIZE_LARGE);
  }

  if (getKeySel0("FONTBOLD_LARGE") == "1") {
    _fontLarge.setBold(true);
  }
  if (getKeySel0("FONTITALIC_LARGE") == "1") {
    _fontLarge.setItalic(true);
  }
  if (getKeySel0("FONTSTRIKEOUT_LARGE") == "1") {
    _fontLarge.setStrikeOut(true);
  }
  if (getKeySel0("FONTUNDERLINE_LARGE") == "1") {
    _fontLarge.setUnderline(true);
  }

  QString fontTypeSmall = getKeySel0("FONTTYPE_SMALL");
  int     fontSizeSmall = getKeySel0("FONTSIZE_SMALL").toInt();
  if (fontTypeSmall.isEmpty()) {
    fontTypeSmall = DEFAULT_FONTTYPE_SMALL;
  }
  if (fontSizeSmall == 0) {
    fontSizeSmall = DEFAULT_FONTSIZE_SMALL;
  }

  _fontSmall = QFont(fontTypeSmall, fontSizeSmall);

  if ( QFontMetrics(_fontSmall).width('i') !=
       QFontMetrics(_fontSmall).width('W') ) {
    _fontSmall = QFont(DEFAULT_FONTTYPE_SMALL, DEFAULT_FONTSIZE_SMALL);
  }

  if (getKeySel0("FONTBOLD_SMALL") == "1") {
    _fontSmall.setBold(true);
  }
  if (getKeySel0("FONTITALIC_SMALL") == "1") {
    _fontSmall.setItalic(true);
  }
  if (getKeySel0("FONTSTRIKEOUT_SMALL") == "1") {
    _fontSmall.setStrikeOut(true);
  }
  if (getKeySel0("FONTUNDERLINE_SMALL") == "1") {
    _fontSmall.setUnderline(true);
  }

}

// Update the List of Sessions
////////////////////////////////////////////////////////////////////////////
int t_initmenu::updateListOfSessions() {

  _list_of_sessions[0].clear();
  _list_of_sessions[1].clear();

  QString session_table;
  t_keyword* tabKey = getKey("SESSION_TABLE");
  if (tabKey) {
    tabKey->updateSel();
    session_table = getKeySel0("SESSION_TABLE");
    if (!session_table.isEmpty()) {
      t_inpfile sesFile(session_table);
      if ( !sesFile.getName().isEmpty() ) {
        t_keyword* key = sesFile.getKey("LIST_OF_SESSIONS");
        if (key) {
          bool wildSess = false;
          bool fixSess  = false;
          for (int jj = 0; jj < key->getSelList().count(); jj++) {
            QStringList hlpList;
            unilineSplit(key->getSelList()[jj], hlpList);
            _list_of_sessions[0].append(hlpList[0]);
            _list_of_sessions[1].append(hlpList[1]);

            if (_list_of_sessions[0].last().left(3) == "???" ) {
              wildSess = true;
            }
            else {
              fixSess  = true;
            }
          }
          if (wildSess && fixSess) {
            errormsg("Mixture of wildcard session definitions and\n"
                     "fixed sessions in " + sesFile.getName());
          }
          return 0;
        }
      }
    }
  }
  _list_of_sessions[0].append("???" + getKeySel0("SESSION_CHAR"));
  _list_of_sessions[1].append("");
  if (!session_table.isEmpty()) {
    errormsg("Using standard session definition");
  }
  return 1;
}

// Return the Pointer to the required Keyword
////////////////////////////////////////////////////////////////////////////
t_keyword* t_initmenu::getKey(const QString& keyName) {
  t_keyword* retVal = 0;

  for (int ii = 0; ii < (int) _inpfiles.count(); ii++) {
    retVal = _inpfiles.at(ii)->getKey(keyName);
    if (retVal != 0) break;
  }

  if ( getIntModus() == t_initmenu::INT &&
      retVal == 0          &&
      !keyName.isEmpty()   &&
      keyName != "EDIT_KEYWORD"  &&
      keyName != "NO_PTH"  &&
      keyName != "NO_DIR"  &&
      keyName != "NO_EXT"  &&
      keyName != "EXT_ANY" &&
      keyName != "SERVER_VARIABLES" &&
      keyName != "OUT_FILE_NAME") {
    errormsg("Cannot find the keyword " + keyName);
  }

  return retVal;
}

// Return the First Item of the Selection List of the required Keyword
////////////////////////////////////////////////////////////////////////////
QString t_initmenu::getKeySel0(const QString& keyName) {
  t_keyword* key = getKey(keyName);
  if (key) {
    key->updateSel();
    key->expandSelList();
    if (key->getSelList().count() > 0) {
      return key->getSelList()[0].stripWhiteSpace();
    }
  }
  return "";
}

// Return the full Name of the Input File
////////////////////////////////////////////////////////////////////////////
QString t_initmenu::getInpFileName(const QString& keyName) {
  t_keyword* key = getKey(keyName);
  if (key == 0) {
    errormsg("Cannot Find the Name of Input File\n"
             "Keyword: " + keyName);
    return "";
  }
  else if (key->getSelList().count() == 0) {
    return "";
  }
  else {
    key->updateSel();
    key->expandSelList();
    QString inpFileName = key->getSelList()[0];
    return expandEnvVar(inpFileName);
  }
}

// Return the full Path based on the specified Keyword
////////////////////////////////////////////////////////////////////////////
QString t_initmenu::getPath(const QString& keyName) {
  t_keyword* key = getKey(keyName);
  if (key == 0 || key->getSelList().size() == 0) {
    return "";
  }
  else {
    QString dirName = key->getSelList()[0];
    dirName.stripWhiteSpace();

    if (keyName.contains("DIR_")) {
      return getActiveCamp() + r_dir::separator() + dirName + r_dir::separator();
    }
    else {
      if ( dirName.isEmpty() ) {
        return "";
      }
      else {
        return dirName + r_dir::separator();
      }
    }
  }
}

// Return the full path to the executable command
////////////////////////////////////////////////////////////////////////////
QString t_initmenu::getCmdPath(const QString& exeFileName) {

  t_keyword* specPathKey = getKey("SPECIAL_PATH");
  if (specPathKey) {
    const QStringList specPathList = specPathKey->getSelList();
    for (int ii = 0; ii < specPathList.count(); ii++) {
      QStringList hlp;
      unilineSplit(specPathList[ii], hlp);
      if (hlp[0] == exeFileName) {
        return QString(hlp[1] + r_dir::separator());
      }
    }
  }

  return getPath("CMD_PATH");
}

// Resolve the standard Menu Dollar-Variables (private)
////////////////////////////////////////////////////////////////////////////
void t_initmenu::evalDollarStandard(QString& ioString) {

  // Standard Menu Variables (Year etc.) without Ranges (simpler syntax)
  // -------------------------------------------------------------------
  int indBase = 0;
  while ( indBase != -1) {
    indBase = ioString.find(QRegExp("\\$[^+-]*[+-]\\d"));
    if ( indBase != -1 ) {
      int i1 = ioString.find(QRegExp("[+-]\\d"), indBase);
      int offset = ioString.mid(i1,2).toInt();
      if ( evalDollarHlp(_MJD, _sessChar, ioString, offset) == 0) {
        break;
      }
    }
  }

  // Standard Menu Variables (Year etc.) without Ranges (syntax with brackets)
  // -------------------------------------------------------------------------
  indBase = 0;
  while ( indBase != -1) {
    indBase = ioString.find(QRegExp("\\$[^+-]*[+-]\\(\\d+\\)"));
    if ( indBase != -1 ) {
      int i1 = ioString.find('(', indBase);
      int i2 = ioString.find(')', indBase);
      int offset = ioString.mid(i1+1,i2-i1-1).toInt();
      if (ioString[i1-1] == '-') {
        offset = -offset;
      }
      if ( evalDollarHlp(_MJD, _sessChar, ioString, offset, i2-i1) == 0) {
        break;
      }
    }
  }
}

// Resolve the Dollar-Variables (standard and user-defined)
////////////////////////////////////////////////////////////////////////////
void t_initmenu::evalDollar(QString& ioString) {

  // Standard Menu Variables
  // -----------------------
  evalDollarStandard(ioString);

  // Variables added by Clients
  // --------------------------
  QMapIterator<QString, QString> it(scriptVariables(_sessChar));
  while (it.hasNext()) {
    it.next();
    QString key   = it.key();
    QString value = it.value();

    int index = ioString.find(key);
    while (index != -1) {
      ioString.replace(index, key.length(), value);
      index = ioString.find(key, index+1);
    }
  }

  // PCF-Variables
  // -------------
  QString server_variables = initmenu.getKeySel0("SERVER_VARIABLES");
  if (!server_variables.isEmpty()) {
    QStringList hlp; unilineSplit( QString(server_variables), hlp );
    for (int ii = 1; ii < hlp.count(); ii += 2) {

      QString pcfVarName  = hlp[ii-1];
      QString pcfVarValue = hlp[ii];

      if (pcfVarName.length() == 1) {
        pcfVarName = '$' + pcfVarName;
      }
      else {
        pcfVarName = "$(" + pcfVarName + ')';
      }

      int index = ioString.find(pcfVarName);
      while (index != -1) {
        ioString.replace(index, pcfVarName.length(), pcfVarValue);
        index = ioString.find(pcfVarName, index+1);
      }
    }
  }

  // User-defined Variables
  // ----------------------
  t_keyword* keyUserVar = getKey("USERVAR");
  QStringList userVar;
  if (keyUserVar) {
    userVar = keyUserVar->getSelList();
  }
  for (int iVar = 0; iVar < (int) userVar.count(); iVar++) {
    int q1 = userVar[iVar].find('"', 0);
    int q2 = userVar[iVar].find('"', q1+1);
    int q3 = userVar[iVar].find('"', q2+1);
    int q4 = userVar[iVar].find('"', q3+1);

    int nameLen = q2 - q1 - 1;

    QString userVarName;

    if      (nameLen == 1) {
      userVarName  = '$'  + userVar[iVar].mid(q1+1, nameLen);
    }
    else if (nameLen > 1) {
      userVarName  = "$(" + userVar[iVar].mid(q1+1, nameLen) + ')';
    }
    else {
      continue;
    }

    int index = ioString.find(userVarName);

    while (index != -1) {
      QString userVarValue = userVar[iVar].mid(q3+1,q4-q3-1);
      ioString.replace(index, userVarName.length(), userVarValue);
      index = ioString.find(userVarName, index+1);
    }
  }

  // Some User-Variable may evolve into standard Variable - repeat eval
  // ------------------------------------------------------------------
  evalDollarStandard(ioString);

  // Special Case - $Y as 2-character Year
  // -------------------------------------
  int index = ioString.find("$Y");
  while (index != -1) {
    QString hlpString;
    int       year;
    double    doy;
    t_juldat juldat;
    juldat.setMJD(_MJD);
    juldat.getDoY(year, doy);

    hlpString.sprintf("%2.2d", year % 100);
    ioString.replace(index, 2, hlpString);
    index = ioString.find("$Y", index+1);
  }

  // Special Case - $J 2-Character Job ID
  // ------------------------------------
  index = ioString.find("$J");
  while (index != -1) {
    ioString.replace(index, 2, getKeySel0("JOB_ID"));
    index = ioString.find("$J", index+1);
  }

  // Special Case - $M as 2-Character Month
  // --------------------------------------
  index = ioString.find("$M");
  while (index != -1) {
    int       yy, mm;
    double    dd;
    t_juldat juldat;
    juldat.setMJD(_MJD);
    juldat.getYMD(yy, mm, dd);
    QString hlpString;
    hlpString.sprintf("%2.2d", mm);
    ioString.replace(index, 2, hlpString);
    index = ioString.find("$M", index+1);
  }

  // Special Case - $D as 2-Character Day of Month
  // ---------------------------------------------
  index = ioString.find("$D");
  while (index != -1) {
    int       yy, mm;
    double    dd;
    t_juldat juldat;
    juldat.setMJD(_MJD);
    juldat.getYMD(yy, mm, dd);
    QString hlpString;
    hlpString.sprintf("%2.2d", int(dd));
    ioString.replace(index, 2, hlpString);
    index = ioString.find("$D", index+1);
  }
}


// Resolve the Dollar-Variables (standard and user-defined)
////////////////////////////////////////////////////////////////////////////
void t_initmenu::evalDollar(const QString varName,
                            QStringList& list, bool reallyEval) {

  QString localVarName = varName.stripWhiteSpace();
  localVarName.replace("~~","+-");

  // Quick Return
  // ------------
  if (localVarName.isEmpty()) {
    list.clear();
    return;
  }

  // Standard Menu Variables (Year, DoY etc.) with Ranges
  // ----------------------------------------------------
  list.clear();

  if (!reallyEval) {
    list.append(localVarName);
    return;
  }

  if ( localVarName.find("+-") == -1 ) {
    list.append(localVarName);
  }
  else {
    int range[2];

    range[0] =  getKeySel0("VAR_MINUS").toInt();
    range[1] =  getKeySel0("VAR_PLUS").toInt();

    // Loop over all Range Values
    // --------------------------
    for (int ii = range[0]; ii <= range[1]; ii++) {
      QString resString = localVarName;
      evalDollarHlp(_MJD, _sessChar, resString, ii);
      list.append(resString);
    }
  }

  // User-defined variables and Standard Variables without Ranges
  // ------------------------------------------------------------
  for (int ii = 0; ii < (int) list.count(); ii++) {
    evalDollar(list[ii]);
  }

  // Make the List unique
  // --------------------
  QStringList hlpList = list;
  list.clear();
  for (int jj = 0; jj < hlpList.count(); jj++) {
    if (jj == 0 || hlpList[jj] != hlpList[jj-1]) {
      list.append( hlpList[jj] );
    }
  }
}

// Auxiliary Function handling Dollar Variables (static function)
////////////////////////////////////////////////////////////////////////////
int t_initmenu::evalDollarHlp(double MJD, const QString& sessChar,
                              QString& ioString, int offset, int addLen) {

  // Create the t_session object
  // ---------------------------
  t_juldat juldat;
  juldat.setMJD(MJD);
  int    year;
  double doy;
  juldat.getDoY(year, doy);

  t_session ss( QString().sprintf("%4.4d", year),
                QString().sprintf("%3.3d", (int) doy) + sessChar, offset);

  if (!ss.foundInListOfSessions()) {
    _list_of_sessions[0].append("???" + sessChar);
    _list_of_sessions[1].append("");
  }

  // Get the corresponding session-dependent string
  // ----------------------------------------------
  QString hlpString ;
  int index;

  QString ff = "[\\d-]";
  if (addLen > 0) {
    ff = "\\(";
  }

  if ( (index = ioString.find( QRegExp("\\$YMD_STR[+-]" + ff) )) != -1) {
    hlpString = ss.currYr_4() + " " + ss.currMonth() + " " + ss.currDay();
  }
  else if ( (index = ioString.find( QRegExp("\\$YSS[+-]" + ff) )) != -1) {
    hlpString = ss.currYear() + ss.currDayyear() + ss.sessChar();
  }
  else if ( (index = ioString.find( QRegExp("\\$YD[+-]" + ff) )) != -1) {
    hlpString = ss.currYear() + ss.currDayyear();
  }
  else if ( (index = ioString.find( QRegExp("\\$WD[+-]" + ff) )) != -1) {
    hlpString = ss.currGPSweek() + ss.currDayweek();
  }
  else if ( (index = ioString.find( QRegExp("\\$JD[+-]" + ff) )) != -1) {
    hlpString = ss.currMJD();
  }
  else if ( (index = ioString.find( QRegExp("\\$M[+-]" + ff) )) != -1) {
    hlpString = ss.currYear() + ss.currMonth();
  }
  else if ( (index = ioString.find( QRegExp("\\$W[+-]" + ff) )) != -1) {
    hlpString = ss.currGPSweek();
  }
  else if ( (index = ioString.find( QRegExp("\\$Y[+-]" + ff) )) != -1) {
    hlpString = ss.currYr_4();
  }
  else if ( (index = ioString.find( QRegExp("\\$S[+-]" + ff) )) != -1) {
    hlpString = ss.currDayyear() + ss.sessChar();
  }
  else if ( (index = ioString.find( QRegExp("\\$[+-]" + ff) )) != -1) {
    hlpString = ss.currDayyear();
  }

  // Replace the $-string
  // --------------------
  if (index != -1) {
    ioString.replace(index, hlpString.length() + addLen,
                     hlpString.stripWhiteSpace());
    return 1;
  }
  else {
    return 0;
  }
}

// Save the key and the corresponding input file
////////////////////////////////////////////////////////////////////////////
void t_initmenu::saveKey(const QString& keyName, const QString& value) {
  t_keyword* key = getKey(keyName);
  if (!key) {
    errormsg("Cannot find the keyword " + keyName);
    return;
  }
  key->storeSel(value);
  key->saveInputFile();
}

// Save the Active Campaign, Session, and Font Selection
////////////////////////////////////////////////////////////////////////////
void t_initmenu::setFontBase(const QFont& newFont) {
  _fontBase = newFont;
  saveKey("FONTTYPE_BASE", newFont.family());
  saveKey("FONTSIZE_BASE", QString().sprintf("%d",newFont.pointSize()));
  if (newFont.bold()) {
    saveKey("FONTBOLD_BASE", "1");
  }
  else {
    saveKey("FONTBOLD_BASE", "0");
  }
  if (newFont.italic()) {
    saveKey("FONTITALIC_BASE", "1");
  }
  else {
    saveKey("FONTITALIC_BASE", "0");
  }
  if (newFont.strikeOut()) {
    saveKey("FONTSTRIKEOUT_BASE", "1");
  }
  else {
    saveKey("FONTSTRIKEOUT_BASE", "0");
  }
  if (newFont.underline()) {
    saveKey("FONTUNDERLINE_BASE", "1");
  }
  else {
    saveKey("FONTUNDERLINE_BASE", "0");
  }
}

void t_initmenu::setFontLarge(const QFont& newFont) {
  _fontLarge = newFont;
  saveKey("FONTTYPE_LARGE", newFont.family());
  saveKey("FONTSIZE_LARGE", QString().sprintf("%d",newFont.pointSize()));
  if (newFont.bold()) {
    saveKey("FONTBOLD_LARGE", "1");
  }
  else {
    saveKey("FONTBOLD_LARGE", "0");
  }
  if (newFont.italic()) {
    saveKey("FONTITALIC_LARGE", "1");
  }
  else {
    saveKey("FONTITALIC_LARGE", "0");
  }
  if (newFont.strikeOut()) {
    saveKey("FONTSTRIKEOUT_LARGE", "1");
  }
  else {
    saveKey("FONTSTRIKEOUT_LARGE", "0");
  }
  if (newFont.underline()) {
    saveKey("FONTUNDERLINE_LARGE", "1");
  }
  else {
    saveKey("FONTUNDERLINE_LARGE", "0");
  }
}

void t_initmenu::setFontSmall(const QFont& newFont) {
  _fontSmall = newFont;
  saveKey("FONTTYPE_SMALL", newFont.family());
  saveKey("FONTSIZE_SMALL", QString().sprintf("%d",newFont.pointSize()));
  if (newFont.bold()) {
    saveKey("FONTBOLD_SMALL", "1");
  }
  else {
    saveKey("FONTBOLD_SMALL", "0");
  }
  if (newFont.italic()) {
    saveKey("FONTITALIC_SMALL", "1");
  }
  else {
    saveKey("FONTITALIC_SMALL", "0");
  }
  if (newFont.strikeOut()) {
    saveKey("FONTSTRIKEOUT_SMALL", "1");
  }
  else {
    saveKey("FONTSTRIKEOUT_SMALL", "0");
  }
  if (newFont.underline()) {
    saveKey("FONTUNDERLINE_SMALL", "1");
  }
  else {
    saveKey("FONTUNDERLINE_SMALL", "0");
  }
}

void t_initmenu::setActiveCamp(const QString& newCamp) {
  _activeCamp = newCamp;
  saveKey("ACTIVE_CAMPAIGN", _activeCamp);
  updateListOfSessions();
}

void t_initmenu::setMJD(double newMJD) {
  _MJD = newMJD;
  saveKey("MODJULDATE", QString().sprintf("%f", _MJD));
}

void t_initmenu::setSessChar(const QString& newSessChar) {
  _sessChar = newSessChar.stripWhiteSpace().right(1);
  saveKey("SESSION_CHAR", _sessChar);
}

void t_initmenu::setJobID(const QString& newJobID) {
  _jobID = newJobID.stripWhiteSpace().right(2);
  saveKey("JOB_ID", _jobID);
}

// Unique String (used e.g. for scratch file names)
////////////////////////////////////////////////////////////////////////////
QString t_initmenu::uniqueString() {
  static int nn = 0;
  ++nn;
  return QString().sprintf("%d_%d", pid(), nn);
}

// Name of the Auxiliary File
////////////////////////////////////////////////////////////////////////////
QString t_initmenu::auxInpName() {
  return expandEnvVar(getPath("PTH_SCR") +
                              getInpFileName("MENUAUX_INP"));
}

// New data available (receiving the environment)
////////////////////////////////////////////////////////////////////////////
void t_initmenu::slotData(const QByteArray& buffer, Q3NetworkOperation* op) {

  if (_oper != op) {
    return;
  }

  QStringList hlp = QStringList::split("\n", QString(buffer));

  QStringList::ConstIterator it;
  for (it = hlp.begin(); it != hlp.end(); ++it) {
    QStringList hlp2 = QStringList::split(" = ", *it);
    QString  name  = hlp2[0];
    QString* value = new QString(hlp2[1]);
    _environment->replace(name, value);
  }
  disconnect(this);
}

// Buffered Environment Variables
////////////////////////////////////////////////////////////////////////////
QString t_initmenu::getenv(const QString& name) const {
  if (! initmenu.modemMode()) {
    return QString( ::getenv(name) );
  }
  else if (_environment) {
    QString* value = _environment->find(name);
    if (value) {
      return *value;
    }
  }
  return QString("${%1}").arg(name);
}
