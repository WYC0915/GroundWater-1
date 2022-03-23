
/* -------------------------------------------------------------------------
 * Bernese GPS Software Version 5.0
 * -------------------------------------------------------------------------
 *
 * Class:      t_keyword
 *
 * Purpose:    This is a very central class implementing the keyword.
 *
 * Author:     L. Mervart
 *
 * Created:    18-APR-2000
 *
 * Changes:
 *
 * Copyright:  Astronomical Institute
 *             University of Bern
 *             Switzerland
 * -----------------------------------------------------------------------*/

#include <qregexp.h>
#include <QTextStream>

#include "keyword.h"
#include "initmenu.h"
#include "menutils.h"
#include "inpfile.h"
#include "errormsg.h"
#include "textwin.h"
#include "field.h"
#include "runpgm.h"
#include "r_file.h"
#include "r_dir.h"

#define MENUAUX_IRCODE_NOT_FOUND     "9"
#define MENUAUX_IRCODE_FATAL_ERROR   "5"
#define MENUAUX_IRCODE_FORTRAN_ERROR "2"
#define MENUAUX_IRCODE_OK            "0"

extern QString celMechRoot;

// Constructor
////////////////////////////////////////////////////////////////////////////
t_keyword::t_keyword(const char* newName) {
  inpfile = 0;
  field   = 0;
  desc    = new t_keydesc();
  name    = newName;
  _freeFormat        = false;
  _freeFormatNumCols = 0;
}

// Constructor - read the Keyword
////////////////////////////////////////////////////////////////////////////
t_keyword::t_keyword(QString& line, r_file& in, t_inpfile* inpFile) {
  inpfile = inpFile;
  field   = 0;
  desc    = new t_keydesc();
  _freeFormat        = false;
  _freeFormatNumCols = 0;

  read(line, in);

  // Initialize the update flag and connect the corresponding slots
  // --------------------------------------------------------------
  updateFlag = t_keyword::update_always;

  QStringList otherKeysList = desc->updateIfChanged();

  if (otherKeysList.count() > 0) {
    updateFlag = t_keyword::update_no;
    for (int ii = 0; ii < otherKeysList.count(); ii++) {
      t_keyword* otherKey = inpfile->getKey(otherKeysList[ii]);
      if (otherKey) {
        connect( otherKey, SIGNAL(keyChanged(t_keyword*)),
                 this, SLOT(slotOtherKeyChanged(t_keyword*)) );
      }
    }
  }
}

// Destructor
////////////////////////////////////////////////////////////////////////////
t_keyword::~t_keyword() {
  delete desc;
}

// Read the Keyword from line and text stream
////////////////////////////////////////////////////////////////////////////
void t_keyword::read(QString& line, r_file& in) {
  line.stripWhiteSpace();

  while ( !in.eof() && ( line.isEmpty() || line[0] == '!' ) ) {
    if ( !line.isEmpty() ) {
      comment.append( line ) ;
    }
    line = in.readLine().stripWhiteSpace() ;
  }

  QTextStream inLine(&line, QIODevice::ReadOnly);

  int num;

  inLine >> name >> num;

  // Read the selection list, remove leading and trailing double quotes
  // ------------------------------------------------------------------
  if (num == 1) {
    QString hlp = inLine.read().stripWhiteSpace();
    if (hlp[0]              == '"') hlp[0]              = ' ';
    if (hlp[(int)hlp.length()-1] == '"') hlp[(int)hlp.length()-1] = ' ';
    selList.append( hlp.stripWhiteSpace() );
  }
  else if (num > 1) {
    for (int ii=0; ii<num; ii++) {
      QString hlp = in.readLine().stripWhiteSpace();
      if (hlp[0]              == '"') hlp[0]              = ' ';
      if (hlp[(int)hlp.length()-1] == '"') hlp[(int)hlp.length()-1] = ' ';
      selList.append( hlp.stripWhiteSpace() );
    }
  }
  else if (num < 0) {
    _freeFormat = true;
    inLine >> _freeFormatNumCols;
    do {
      line = in.readLine().stripWhiteSpace();
      if (!line.isEmpty() && line[0] != '!' && line[0] != '#') {
        QStringList hlp = line.split(QRegExp("\\s"),QString::SkipEmptyParts);
        QString sel;
        for (int ic = 0 ; ic < _freeFormatNumCols-1; ic++) {
          sel += '"' + hlp[ic] + "\" ";
        }
        sel += '"';
        for (int ic = _freeFormatNumCols-1; ic < hlp.count(); ic++) {
          sel += hlp[ic].replace('"','\'') + ' ';
        }
        sel[sel.length()-1] = '"';
        selList.append( sel );
      }
    } while ( !in.eof() && line[0] != '#' );
  }

  // Read the Description Line(s) and the Value list
  // -----------------------------------------------
  QStringList descList;
  if ( line[0] != '#' ) {
    line = in.readLine().stripWhiteSpace();
  }
  while (line.find("##") == 0) {     // new description format
    descList.append(line);
    line = in.readLine().stripWhiteSpace();
  }

  if (descList.count() > 0) {
    desc->setDescList(descList);  // new description format
  }
  else if (line.find("#") == 0) {
    desc->setDescLine(line);      // old description format
    line = in.readLine().stripWhiteSpace();
  }

  // If uniline, add the leading and trailing double quotes again
  // ------------------------------------------------------------
  if (desc->widget() == "uniline") {
    if (selList.count() == 0) {
      selList.append("");
    }
    for (int ii = 0; ii < (int) selList.count(); ii++) {
      selList[ii] = '"' + selList[ii] + '"';
    }
  }

  // Save the Old Selection List
  // ---------------------------
  originalSelList = selList;

  // If default path, remove the old path from the selection list
  // ------------------------------------------------------------
  if ( !desc->path().isEmpty() && desc->path() != "PTH_ANY") {
    for (int ii = 0; ii < (int) selList.count(); ii++) {
      selList[ii] = stripPath(selList[ii]);
    }
  }
  panel = 0;

  // Read the valueList (if available)
  // ---------------------------------
  if (!desc->isEmpty() && desc->widget() != "initmenu" ) {
    while (line.find("#") == 0) {
      valueList.append( line.replace(QRegExp("#"),"").stripWhiteSpace() );
      line = in.readLine().stripWhiteSpace();
    }
    if (valueList.count() == 0) {
      valueList = selList;
    }
  }
}

// Update the Selection List according to current Key Value
////////////////////////////////////////////////////////////////////////////
int t_keyword::updateSel(bool pointerUpdate, bool menuauxFlg) {

  int irc = 0;

  // Check if the update is permitted/required
  // -----------------------------------------
  if (updateFlag == t_keyword::update_no) {
    QStringList otherKeysList = desc->updateIfChanged();

    if (otherKeysList.count() > 0) {
      for (int ii = 0; ii < otherKeysList.count(); ii++) {
        t_keyword* otherKey = inpfile->getKey(otherKeysList[ii]);
        if (otherKey) {
          otherKey->slotThisKeyMayChanged(menuauxFlg);
        }
      }
    }
  }

  QStringList otherKeysList = desc->updateIfActive();
  if (otherKeysList.count() > 0) {
    bool active = false;
    for (int ii = 0; ii < otherKeysList.count(); ii++) {
      t_keyword* otherKey = inpfile->getKey(otherKeysList[ii]);
      if (otherKey && otherKey->evalActiveIf()) {
        active = true;
      }
    }
    if (!active) {
      return irc;
    }
  }

  if (updateFlag == t_keyword::update_no) {
    return irc;
  }
  else if (updateFlag == t_keyword::update_yes) {
    updateFlag = t_keyword::update_no;
  }

  // Special Case - use auxiliary Fortran Program to prepare the List
  // ----------------------------------------------------------------
  if ( desc->menuaux() && menuauxFlg) {
    irc += menuaux();
  }

  // Normal Case - uniline
  // ---------------------
  else if (desc->widget() == "uniline") {
    selList = valueList;
  }

  // Normal Case - lineedit
  // ----------------------
  else if (desc->widget() == "lineedit") {
    if ( valueList.count() > 0 && !valueList[0].isEmpty() ) {
      if (!getDotExtension().isEmpty()) {
        valueList[0] = stripExtension(valueList[0]);
      }
    }
  }

  // Normal Case - selwin
  // --------------------
  else if (desc->widget() == "selwin") {
    if (valueList.count() > 0 && valueList[0] == "SELECTED") {
      QStringList hlpList(selList);
      selList.clear();
      for (int ii = 0; ii < hlpList.count(); ii++) {
        QString hlp = hlpList[ii];
        if (!getDotExtension().isEmpty()) {
          hlp = stripExtension(hlp);
        }
        if (!getPath().isEmpty()) {
          hlp = stripPath(hlp, false);
        }
        selList.append( hlp + getDotExtension() );
      }
      // Check for file existence
      // ------------------------
      if (initmenu.getIntModus() == t_initmenu::NOINT) {
        QStringList oldSelList(selList);
        QStringList newSelList;
        setValue("*");
        irc += updateSel();
        for (int iold = 0; iold < (int) oldSelList.count(); iold++) {
          for (int inew = 0; inew < (int) selList.count(); inew++) {
            if (selList[inew].find(oldSelList[iold]) != -1) {
              newSelList.append(oldSelList[iold]);
              break;
            }
          }
        }
        selList = newSelList;
        setValue("SELECTED");
      }
    }
    else { // New Selection (value != "SELECTED")

      if ( valueList.count() > 0 && !valueList[0].isEmpty() ) {
        if (!getDotExtension().isEmpty()) {
          valueList[0] = stripExtension(valueList[0]);
        }
      }

      selList.clear();

      if ( !desc->pointer().isEmpty() ) { // Select from a list of another key
        t_keyword* keyHlp = inpfile->getKey( desc->pointer() );

        // Try to use the keyword from initmenu input files (MENU_*.INP)
        // -------------------------------------------------------------
        if (keyHlp == 0) {
          keyHlp = initmenu.getKey( desc->pointer() );
        }

        if (keyHlp) {
          if (pointerUpdate) {
            irc += keyHlp->updateSel();
          }
          QString valueTxt;
          if (valueList.count() > 0) valueTxt = valueList[0].stripWhiteSpace();
          if      (valueTxt == "$first") {
            selList.append(keyHlp->getSelList()[0]);
          }
          else if (valueTxt == "*") {
            selList = keyHlp->getSelList();
          }
          else if ( valueTxt.find('*') != -1 || valueTxt.find('?') != -1 ||
                    (valueTxt.find('[') != -1 && valueTxt.find(']') != -1) ) {
            QRegExp rg(valueTxt, false, true);
            selList.clear();
            QStringList allList = keyHlp->getSelList();
            for (int ii = 0; ii < allList.count(); ii++) {
              if (rg.indexIn(allList[ii]) != -1) {
                selList.append(allList[ii]);
              }
            }
          }
        }
      }
      else if ( desc->maxFiles() ) { // Select file from a directory list
        QString filter;
        if (valueList.count() > 0) filter = valueList[0].stripWhiteSpace();
        if (!filter.isEmpty()) {
          QStringList flts;
          initmenu.evalDollar(filter, flts, desc->evalDollar());
          QStringList::Iterator it;
          for (it = flts.begin(); it != flts.end(); ++it) {
            (*it) = (*it) + getDotExtension();
          }
          r_dir dir( expandEnvVar(getPath()) );
          dir.setFilter(QDir::Files);
          selList = dir.entryList(flts.join(";"), QDir::Files);
        }
      }
      else { // Value may expand into a list
        if (valueList.count() > 0) {
          initmenu.evalDollar(valueList[0], selList, desc->evalDollar());
        }
      }
    }
  }
  return irc;
}

// Store the resulting selection List
////////////////////////////////////////////////////////////////////////////
void t_keyword::storeSel(const QStringList& inpList) {
  selList = inpList;
}

// Write the Input File
////////////////////////////////////////////////////////////////////////////
void t_keyword::saveInputFile() {
  inpfile->save();
}

// Store one String as the resulting selection List
////////////////////////////////////////////////////////////////////////////
void t_keyword::storeSel(const QString& str) {
  selList.clear();
  selList.append(str);
}

// Update Value According to Selection List
////////////////////////////////////////////////////////////////////////////
void t_keyword::setValueAccordingToSel() {

  if (selList.count() == 0) {
    setValue(" ");
  }
  else if (selList.count() == 1) {
    QString hlp = selList[0].stripWhiteSpace();
    if (hlp[0]                   == '"') hlp[0]                   = ' ';
    if (hlp[(int)hlp.length()-1] == '"') hlp[(int)hlp.length()-1] = ' ';

#ifdef CELMECH
    if (desc->path() == "PTH_ANY") {
      QString longString = expandEnvVar(celMechRoot);
#ifdef WIN32
      longString.replace(QRegExp("/"), "\\");
      longString = longString.upper();
      hlp.replace(QRegExp("/"), "\\");
      hlp = hlp.upper();
#else
      longString.replace(QRegExp("\\"), "/");
      hlp.replace(QRegExp("\\"), "/");
#endif
      int index = hlp.find(longString);
      if (index != -1) {
        hlp.replace(index, longString.length(), "${CM}");
      }
    }
#endif
    setValue(hlp.stripWhiteSpace());
  }
  else {
    setValue("SELECTED");
  }
}

// Set Value
////////////////////////////////////////////////////////////////////////////
void t_keyword::setValue (const QString& newValue ) {
  valueList.clear();
  valueList.append( newValue.stripWhiteSpace() ) ;
  if (! getDotExtension().isEmpty()) {
    valueList[0] = stripExtension(valueList[0]);
  }
}

// Expand Selection List - Resolve Path, Extension and Dollar Variables
////////////////////////////////////////////////////////////////////////////
void  t_keyword::expandSelList() {

  if ( desc->isEmpty() ) return;

  if (inpfile && !inpfile->expandSelLists()) {
    selList = valueList;
    return;
  }

  if (name.indexOf("SERVER_VARIABLES") != -1) {
    selList = valueList;
    return;
  }

  if (desc->widget() == "uniline") {
    selList = valueList;
    for (int ii = 0; ii < (int) selList.count(); ii++) {
      if (desc->evalDollar()) {
        initmenu.evalDollar(selList[ii]);
      }
    }
  }

  else if (desc->widget() == "initmenu") {
    selList.clear();
    t_keyword* env = initmenu.getKey(desc->pointer());
    if (env) {
      if      (desc->pointer().find("DIR_") == 0) {
        if (valueList.count() == 0) {
          valueList << "";
        }
        valueList[0] = initmenu.getPath(desc->pointer());
        initmenu.evalDollar(valueList[0], selList, desc->evalDollar());
      }
      else if (desc->pointer() == "ENVIR_LIST") {
        for (int ii = 0; ii < env->getSelList().count(); ii++) {
          QString hlp1 = env->getSelList()[ii];
          hlp1 = hlp1.replace('"'," ").stripWhiteSpace();

          QString hlp2 = expandEnvVar("${" + hlp1 + "}");
          selList.append('"' + hlp1 + '"' + ' ' + '"' + hlp2 + '"');
        }
      }
      else {
        env->updateSel();
        env->expandSelList();
        selList = env->getSelList();
      }
    }
  }
  else if (desc->widget() == "selwin" || desc->widget() == "lineedit") {
    if (valueList.count() > 0) {
      if ( desc->widget() == "lineedit"  ||
           ( valueList[0] != "SELECTED"   &&
             valueList[0].find('*') == -1 &&
             valueList[0].find('?') == -1 &&
             (valueList[0].find('[') == -1 || valueList[0].find(']') == -1) &&
             valueList[0].stripWhiteSpace() != "$first" ) ) {
        initmenu.evalDollar(valueList[0], selList, desc->evalDollar());
      }
    }
    for (int ii = 0; ii < (int) selList.count(); ii++) {
      QString baseName = selList[ii];
      if ( !desc->path().isEmpty() && desc->path() != "PTH_ANY") {
        baseName = stripPath(baseName);
      }
      if (! baseName.isEmpty()) {
        selList[ii] = getPath() + baseName;
        QString postfix = getDotExtension();
        if (! postfix.isEmpty() && postfix.find('*') == -1) {
          int pos = selList[ii].find(postfix);
          if (pos == -1) pos = selList[ii].find(postfix.lower());
          if (pos == -1) {
            selList[ii] = selList[ii] + postfix;
          }
        }
      }
      else {
        selList[ii] = "";
      }
    }
  }
  else {
    if (valueList.count() > 0) {
      initmenu.evalDollar(valueList[0], selList, desc->evalDollar());
    }
  }
}

// Get the full Path
////////////////////////////////////////////////////////////////////////////
QString t_keyword::getPath() const {
  if ( !desc->path().isEmpty() ) {
    QString hlp = initmenu.getPath(desc->path());
    if (desc->evalDollar()) {
      initmenu.evalDollar(hlp);
    }
    return hlp;
  }
  return "";
}

// Get the Default Extension
////////////////////////////////////////////////////////////////////////////
QString t_keyword::getDotExtension() const {
  if ( !desc->ext().isEmpty() ) {
    QString hlp = initmenu.getKeySel0(desc->ext());
    if (desc->evalDollar()) {
      initmenu.evalDollar(hlp);
    }
    if (! hlp.isEmpty() ) {
      return "." + hlp;
    }
  }
  return "";
}

// Output Operator
////////////////////////////////////////////////////////////////////////////
r_file& operator<<(r_file& os, t_keyword& key) {

  // Prevent saving non-active file names
  // ------------------------------------
  if (! key.evalActiveIf()                 &&
      ! key.getDesc()->path().isEmpty()    &&
      key.getDesc()->widget() != "uniline" &&
      key.getValue() != "SELECTED"          ) {
    if (key.getValue().isEmpty()) {
      key.setValueAccordingToSel();
    }
    if (key.getName() != "SYSOUT") {
      key.selList.clear();
    }
  }

  os << "\n";

  int ii;

  for (ii = 0; ii < (int) key.comment.count(); ii++) {
    os << key.comment[ii] << "\n";
  }

  if (key._freeFormat) {
    os << key.name << " -1 " << key._freeFormatNumCols << "\n";
    for (ii = 0; ii < (int) key.selList.count(); ii++) {
      QString hlp = key.selList[ii].replace('"',"");
      os << "  " << hlp << "\n";
    }
  }
  else {
    QStringList selList_sav;
    if (key.inpfile && key.inpfile->expandSelLists() &&
        key.getValue().find("~~") != -1) {
      selList_sav = key.selList;
      key.selList.clear();
      for (int ii = 0; ii < selList_sav.count(); ++ii) {
        if (r_file::exists(expandEnvVar(selList_sav[ii]))) {
          key.selList << selList_sav[ii];
        }
      }
    }

    os << key.name << " " << key.selList.count()   ;

    if (key.selList.count() != 1) os << "\n" ;

    for (ii = 0; ii < (int) key.selList.count(); ii++) {
      QString hlp = key.selList[ii].stripWhiteSpace();

      os << "  " ;
      if (hlp.length() > 0) {
        if (hlp[0] != '"' ) os << '"' ;
        os << hlp  ;
        if (hlp[(int)hlp.length()-1] != '"' ) os << '"' ;
      }
      else {
        os << '"' << '"';
      }
      os << "\n" ;
    }

    if (key.inpfile && key.inpfile->expandSelLists() &&
        key.getValue().find("~~") != -1) {
      key.selList = selList_sav;
    }
  }

  // Save the description line and the value line(s)
  // ----------------------------------------------
  if ( !key.desc->isEmpty() ) {
    os << key.desc->formatDescList();

    if (!key._freeFormat) {
      if ( key.desc->widget() != "initmenu" && key.valueList != key.selList ) {
        for (ii = 0; ii < (int) key.valueList.count(); ii++) {
          os << "  # " << key.valueList[ii] << "\n";
        }
      }
    }
  }
  return os;
}

// Set the Description
////////////////////////////////////////////////////////////////////////////
void t_keyword::setDesc  (const char* descLine) {
  desc->setDescLine(descLine);
}

// Interaction with an auxiliary Fortran program (private)
////////////////////////////////////////////////////////////////////////////
int t_keyword::menuaux() {

  if (!evalActiveIf()) {
    return 0;
  }

  Q3PtrList<t_keyword>* keyList = new Q3PtrList<t_keyword>;

  // Loop over all remaining keywords
  // --------------------------------
  for (int ii = 0; ii < (int) desc->menuauxKeys().count(); ii++) {
    if (inpfile) {
      t_keyword* keyHlp = inpfile->getKey(desc->menuauxKeys()[ii]);
      if (keyHlp) {
        keyList->append(keyHlp);
        keyList->last()->expandSelList();
      }
    }
  }

  int irc = menuaux( desc->action(), keyList );
  delete keyList;
  return irc;
}

// Interaction with an auxiliary Fortran program (public)
////////////////////////////////////////////////////////////////////////////
int t_keyword::menuaux(const QString& action, Q3PtrList<t_keyword>* keyList) {
  QString uniqueStr = "_" + initmenu.uniqueString();

  // Write the keywords
  // ------------------
  r_file inpFile( initmenu.auxInpName() + uniqueStr );

  if ( !inpFile.open(QIODevice::WriteOnly) ) {
    errormsg("Cannot write into file " + inpFile.name());
    return 1;
  }

  for (unsigned ii = 0; ii < keyList->count(); ii++) {
    inpFile << *(keyList->at(ii));
  }

  // Add some standard keywords
  // --------------------------
  t_keyword* keyHlp;
  keyHlp = new t_keyword("ENVIRONMENT");
  keyHlp->setDesc("initmenu ENVIR_LIST");
  keyHlp->expandSelList();
  inpFile << *keyHlp;
  delete keyHlp;

  keyHlp = new t_keyword("CONST");
  keyHlp->storeSel(initmenu.getPath("PTH_GEN") +
                   initmenu.getInpFileName("GEN_CONST_FILE"));
  inpFile << *keyHlp;
  delete keyHlp;

  keyHlp = new t_keyword("DATUM");
  keyHlp->storeSel(initmenu.getPath("PTH_GEN") +
                   initmenu.getInpFileName("GEN_DATUM_FILE"));
  inpFile << *keyHlp;
  delete keyHlp;

  keyHlp = new t_keyword("SYSOUT");
  inpFile << *keyHlp;
  delete keyHlp;

  keyHlp = new t_keyword("SYSERR");
  QString errorFileName = initmenu.getPath("PTH_SCR") +
                          initmenu.getInpFileName("MENUAUX_ERR") + uniqueStr;
  keyHlp->storeSel(errorFileName);
  inpFile << *keyHlp;
  delete keyHlp;

  keyHlp = new t_keyword("MENUAUX_ACTION");
  keyHlp->storeSel(action);
  inpFile << *keyHlp;
  delete keyHlp;

  keyHlp = new t_keyword("MENUAUX_IRCODE");
  keyHlp->storeSel(QString(MENUAUX_IRCODE_NOT_FOUND) );
  inpFile << *keyHlp;
  delete keyHlp;

  inpFile.close();

  // Execute the Auxiliary Program
  // -----------------------------
  QString exeProgram = initmenu.getCmdPath("MENUAUX") +
                       initmenu.getKeySel0("MENUAUX_EXE");
  crepro(exeProgram, inpFile.name(), false, true);

  // Read the file writen by the auxiliary program
  // ---------------------------------------------
  t_inpfile resultInput(inpFile.name());
  if (resultInput.status() != t_inpfile::status_OK) {
    return 1;
  }
  QString ircString = resultInput.getKeySel0("MENUAUX_IRCODE");

  r_file errorFile( expandEnvVar(errorFileName) ) ;

  // Auxiliary Program Run O.K., Read the selList
  // --------------------------------------------
  if (ircString == MENUAUX_IRCODE_OK) {
    if ( desc->widget() != "dummy") {
      t_keyword* resultKey = resultInput.getKey("MENUAUX_RESULT");
      if (resultKey) {
        setSelList(resultKey->getSelList());
        if (desc->widget() == "uniline") {
          valueList = selList;
          selList.clear();
        }
        if (desc->widget() == "lineedit" && selList.count() > 0) {
          valueList[0] = selList[0];
        }
      }
    }
  }

  // Error in auxiliary program
  // --------------------------
  else {
    QString msg;
    if      (ircString == MENUAUX_IRCODE_NOT_FOUND) {
      msg = "Cannot find or execute " + exeProgram;
    }
    else if (ircString == MENUAUX_IRCODE_FATAL_ERROR) {
      msg = "Fatal Error in " + exeProgram;
    }
    else if (ircString == MENUAUX_IRCODE_FORTRAN_ERROR) {
      msg = "Error in " + exeProgram ;
      if (errorFile.exists() && errorFile.size() > 0) {
        msg += ":\n\n" + fileToString(errorFileName);
      }
    }
    else {
      msg = "Unknown menuaux return code " + ircString;
      ircString = MENUAUX_IRCODE_FATAL_ERROR;
    }

    if ( initmenu.getExcept() ) {
      throw "t_keyword::menuaux: " + msg;
    }
    else if (initmenu.getLogModus() != t_initmenu::quiet) {
      errormsg(msg, true);
    }
  }

  if (initmenu.getKeySel0("MENUAUX_DEBUG") != "1") {
    if (errorFile.exists()) {
      errorFile.remove();
    }
    inpFile.remove();
  }

  return ircString.toInt();
}

// This Key Changed Slot
////////////////////////////////////////////////////////////////////////////
void t_keyword::slotThisKeyMayChanged() {
  slotThisKeyMayChanged(true);
}

void t_keyword::slotThisKeyMayChanged(bool menuauxFlg) {

  QStringList saveList = selList;

  if (field) {
    field->updateKeyValue();
  }
  updateSel(false, menuauxFlg);  // no pointer update
  expandSelList();

  if (originalSelList != selList) {
    originalSelList = selList;
    emit keyChanged(this);
  }

  selList = saveList;
}

// Other Key Changed Slot
////////////////////////////////////////////////////////////////////////////
void t_keyword::slotOtherKeyChanged(t_keyword* key) {

  QStringList otherKeysList = desc->updateIfChanged();

  if (otherKeysList.count() > 0) {
    for (int ii = 0; ii < otherKeysList.count(); ii++) {
      t_keyword* otherKey = inpfile->getKey(otherKeysList[ii]);
      if (otherKey == key) {
        updateFlag = true;
        break;
      }
    }
  }
}

// Get The Value
////////////////////////////////////////////////////////////////////////////
QString t_keyword::getValue(void) const {
  if (valueList.count() > 0) {
    return valueList[0];
  }
  else {
    return "";
  }
}

// Get the First Item from the Selection List
////////////////////////////////////////////////////////////////////////////
QString t_keyword::getSel0(void) const {
  if (selList.count() > 0) {
    return selList[0];
  }
  else {
    return "";
  }
}

// Decide if active (enabled)
////////////////////////////////////////////////////////////////////////////
bool  t_keyword::evalActiveIf() const {
  return evalIf( desc->activeIf() );
}

// Decide if update before save
////////////////////////////////////////////////////////////////////////////
bool  t_keyword::evalUpdateIfSave() const {
  QString condString = desc->updateIfSave();
  if ( condString.isEmpty() ) {
    condString = "false";
  }
  return evalIf(condString);
}

// Evaluate the condition string
////////////////////////////////////////////////////////////////////////////
bool  t_keyword::evalIf(const QString& condString) const {

  if      (condString.isEmpty()) {
    return true;
  }
  else if (condString == "true") {
    return true;
  }
  else if (condString == "false") {
    return false;
  }


  QStringList condKeyName;
  QStringList keyValue;
  QStringList condRelOper;
  QStringList condValue;
  QStringList condLogOper;

  splitCondLine(condString, condKeyName, condRelOper, condValue, condLogOper);

  for (int ii = 0; ii < condKeyName.count(); ii++) {
    t_keyword* keyHlp = inpfile->getKey( condKeyName[ii] );
    if (keyHlp) {
      if ( keyHlp->evalActiveIf() ) {
        keyValue.append(keyHlp->getValue().stripWhiteSpace());
      }
      else {
        keyValue.append("inactive");
      }
    }
    else {
      keyValue.append("*");
    }
  }

  return evalLogTags(keyValue, condRelOper, condValue, condLogOper);
}
