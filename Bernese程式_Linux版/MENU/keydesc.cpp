
/* -------------------------------------------------------------------------
 * Bernese GPS Software Version 5.0
 * -------------------------------------------------------------------------
 *
 * Class:      t_keydesc
 *
 * Purpose:    This class contains the description of the keyword
 *
 * Author:     L. Mervart
 *
 * Created:    10-AUG-2001
 *
 * Changes:
 *
 * Copyright:  Astronomical Institute
 *              University of Berne
 *                  Switzerland
 * -----------------------------------------------------------------------*/

#include <stdlib.h>
#include <qstringlist.h>
#include <qregexp.h>
#include "keydesc.h"
#include "menutils.h"
#include "errormsg.h"

// Constructor
////////////////////////////////////////////////////////////////////////////
t_keydesc::t_keydesc() {
}

// Destructor
////////////////////////////////////////////////////////////////////////////
t_keydesc::~t_keydesc() {
}

// Set the Description Values (new Format)
////////////////////////////////////////////////////////////////////////////
void t_keydesc::setDescList(const QStringList& list) {

  QString     hlpLine;

  for (int ii = 0; ii < list.count(); ii++) {
    hlpLine = list[ii];
    hlpLine.replace( QRegExp("#")," " );
    descList += QStringList::split(';', hlpLine);
  }
  for (int jj = 0; jj < descList.count(); jj++) {
    descList[jj] = descList[jj].stripWhiteSpace();
  }
}

// Get the Description Value
////////////////////////////////////////////////////////////////////////////
QString t_keydesc::getVal(const QString& key, unsigned ic) const {

  QString keyHlp = key;
  if (ic) {
    keyHlp = keyHlp + QString().sprintf(".%d", ic);
  }

  for (int ii = 0; ii < descList.count(); ii++) {
    if (descList[ii].find( QRegExp(keyHlp + "\\s*=") ) == 0) {
      int index = descList[ii].find("=");
      return descList[ii].mid(index+1).stripWhiteSpace();
    }
  }
  return "";
}

// Set the Description Values (old Format)
////////////////////////////////////////////////////////////////////////////
void t_keydesc::setDescLine(const QString& descLine) {

  QString hlpLine = descLine;
  hlpLine.replace( QRegExp("#")," " );
  hlpLine = hlpLine.stripWhiteSpace();
  QStringList oldDesc = QStringList::split(QRegExp("\\s"), hlpLine);

  if (oldDesc.count() > 0) {
    descList.append("widget = " + oldDesc[0]);
  }
  else {
    return;
  }

  // Special Case - MENUAUX
  // ----------------------
  if (oldDesc.count() > 1 &&  oldDesc[1] == "MENUAUX") {
    descList.append("menuaux = " + oldDesc[1]) ;
    if (oldDesc.count() > 2) {
      descList.append("action = " + oldDesc[2]);
    }
    if (oldDesc.count() > 3) {
      descList.append("menuauxkeys = " + oldDesc[3]);
      for (int ii = 4; ii < oldDesc.count(); ii++) {
        descList.last() += " " + oldDesc[ii];
      }
    }
    return;
  }

  // Normal case
  // ----------
  if (getVal("widget") == "initmenu") {
    if (oldDesc.count() > 1) descList.append("pointer = " + oldDesc[1]) ;
  }

  else if (getVal("widget")  == "comment") {
  }

  else if (getVal("widget")  == "uniline") {
  }

  else if (getVal("widget")  == "selwin") {
    if (oldDesc.count() > 1) descList.append("path = "     + oldDesc[1]);
    if (oldDesc.count() > 2) {
      if (oldDesc[2].find("EXT") != -1) {
        descList.append("ext = "      + oldDesc[2]);
        if (oldDesc.count() > 3) {
          int maxf = oldDesc[3].toInt();
          descList.append(QString().sprintf("maxfiles = %d", abs(maxf)));
          if (maxf < 0) {
            descList.append("emptyallowed = true");
          }
        }
        if (oldDesc.count() > 4) descList.append("pointer = "  + oldDesc[4]);
      }
      else {
        int maxf = oldDesc[2].toInt();
        descList.append(QString().sprintf("maxfiles = %d", abs(maxf)));
        if (maxf < 0) {
          descList.append("emptyallowed = true");
        }
      }
    }
  }

  else if (getVal("widget")  == "combobox") {
    if (oldDesc.count() > 1) descList.append("editable = " + oldDesc[1]);
    if (oldDesc.count() > 2) descList.append("cards = "    + oldDesc[2]);
    for (int ii = 3; ii < oldDesc.count(); ii++) {
      descList.last() += " " + oldDesc[ii];
    }
  }

  else if (getVal("widget")  == "lineedit") {
    if (oldDesc.count() > 1) descList.append("path = "     + oldDesc[1]);
    if (oldDesc.count() > 2) descList.append("ext = "      + oldDesc[2]);
    if (oldDesc.count() > 3) descList.append("maxfiles = " + oldDesc[3]);
  }

  else if (getVal("widget")  == "spinbox") {
    if (oldDesc.count() > 1) descList.append("range = " + oldDesc[1]);
    for (int ii = 2; ii < oldDesc.count() && ii <= 3; ii++) {
      descList.last() += " " + oldDesc[ii];
    }
  }

  else if (getVal("widget")  == "checkbox") {
  }

  else if (getVal("widget")  == "radiobutton") {
    if (oldDesc.count() > 1) descList.append("group = " + oldDesc[1]);
    if (oldDesc.count() > 2) descList.append("radiokeys = " + oldDesc[2]);
    for (int ii = 3; ii < oldDesc.count(); ii++) {
      descList.last() += " " + oldDesc[ii];
    }
  }

  else {
    errormsg("Wrong keyword description:\n" + descList[0]);
  }
}


// Several Short Functions
////////////////////////////////////////////////////////////////////////////
bool t_keydesc::isEmpty() const {
  return getVal("widget").isEmpty();
}

bool t_keydesc::isEditable(unsigned ic) const {
  if ( getVal("editable", ic) == "false" ) {
    return false;
  }
  else {
    return true;
  }
}

bool t_keydesc::menuaux() const {
  if ( getVal("menuaux").isEmpty() ) {
    return false;
  }
  else {
    return true;
  }
}

int t_keydesc::maxFiles() const {
  if ( getVal("maxfiles").isEmpty() ) {
    return 9999;
  }
  else {
    return getVal("maxfiles").toInt();
  }
}

int t_keydesc::range(int ii) const {
  QStringList hlp = QStringList::split(QRegExp("\\s"), getVal("range"));
  if (hlp.count() > ii) {
    return hlp[ii].toInt();
  }
  else {
    return 1;
  }
}

QStringList   t_keydesc::menuauxKeys() const {
  return QStringList::split(QRegExp("\\s"), getVal("menuauxkeys"));
}

QStringList  t_keydesc::radioKeys() const {
  return QStringList::split(QRegExp("\\s"), getVal("radiokeys"));
}

QStringList  t_keydesc::cards() const {
  QStringList hlp;
  cardSplit(getVal("cards"), hlp);
  return hlp;
}

QStringList  t_keydesc::updateIfChanged() const {
  return QStringList::split(QRegExp("\\s"), getVal("updateifchanged"));
}

QStringList  t_keydesc::updateIfActive() const {
  return QStringList::split(QRegExp("\\s"), getVal("updateifactive"));
}

// Format Description List for Output
////////////////////////////////////////////////////////////////////////////
QString t_keydesc::formatDescList() const {

  const int maxLineLength = 75;
  QString outString;
  int ii = 0;

  while ( ii < descList.count() ) {
    QString hlpStr = "  ## " + descList[ii];
    while ( ++ii < descList.count()                                  &&
            hlpStr.length() + descList[ii].length() <  maxLineLength) {
      hlpStr += "; " + descList[ii];
    }
    outString += hlpStr + "\n";
  }
  return outString;
}

// Several Checks
////////////////////////////////////////////////////////////////////////////
QString t_keydesc::check_type(unsigned ic) const {
  return getVal("check_type", ic);
}

int t_keydesc::check_strlen(unsigned ic) const {
  QString val = getVal("check_strlen", ic);
  if (val.isEmpty()) {
    return 0;
  }
  else {
    return val.toInt();
  }
}

bool t_keydesc::check_min(double& min, unsigned ic) const {
  QString val = getVal("check_min", ic);
  if (val.isEmpty()) {
    min = 0.0;
    return false;
  }
  else {
    min = val.toDouble();
    return true;
  }
}

bool t_keydesc::check_max(double& max, unsigned ic) const {
  QString val = getVal("check_max", ic);
  if (val.isEmpty()) {
    max = 0.0;
    return false;
  }
  else {
    max = val.toDouble();
    return true;
  }
}

QString t_keydesc::check_countas(unsigned ic) const {
  return getVal("check_countas", ic);
}

