
/* -------------------------------------------------------------------------
 * Bernese GPS Software Version 5.0
 * -------------------------------------------------------------------------
 *
 * Class:      t_panel
 *
 * Purpose:    This class implements one panel.
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

#include <qregexp.h>
#include <qtextstream.h>

#include "panel.h"
#include "initmenu.h"
#include "errormsg.h"
#include "menutils.h"
#include "r_file.h"

// Constructor - read the Panel
////////////////////////////////////////////////////////////////////////////
t_panel::t_panel(QString& line, r_file& in, Q3PtrList<t_keyword>* keys,
                 int panNum) {
  width       = 0;
  panelNumber = panNum;
  numKeys     = 0;

  if (initmenu.getIntModus() == t_initmenu::INT) {
    buttonGroup = new QButtonGroup;
  }
  else {
    buttonGroup = 0;
  }

  // Read the List of Conditions
  // ---------------------------
  QStringList condKeyName;
  splitCondLine(line, condKeyName, condRelOper, condValue, condLogOper);

  for (int kk = 0; kk < condKeyName.count(); kk++) {
    t_keyword* foundKey = 0;
    for (unsigned jj = 0; jj < keys->count(); jj++) {
      if ( keys->at(jj)->getName() == condKeyName[kk] ) {
        foundKey = keys->at(jj);
        break;
      }
    }
    condKey.append(foundKey);
  }

  // Continue to Read the Panel
  // --------------------------
  panelLine.append( line );
  while ( !in.eof() ) {
    line =  in.readLine();
    if ( line.stripWhiteSpace().isEmpty() ||
         line.stripWhiteSpace()[0] != '#' ) {
      errormsg("Panel corrupted: ");
      break;
    }
    panelLine.append( line );
    if (panelLine.last().contains("END_PANEL")) break;
  }

  for (int ii = 0; ii < (int) panelLine.count(); ii++) {
    int posLast = panelLine[ii].findRev('#');
    if (width < posLast) {
      width = posLast;
    }
    QString keyString(panelLine[ii].mid(posLast+1));
    QTextStream inKey(&keyString, QIODevice::ReadOnly);

    int posBeg;
    int posEnd = -1;

    while ( (posBeg = panelLine[ii].find('>', posEnd+1)) > -1) {
      posEnd = panelLine[ii].find('<', posBeg+1);

      if (posEnd == -1) {
        errormsg("Panel Corrupted: '>' without '<'");
        break;
      }

      QString keyName;

      inKey >> keyName;

      if ( keyName.isEmpty() ) {
        errormsg("Panel Corrupted: keyword not found");
        break;
      }

      bool found = false;
      for (int kk = 0; kk < (int) keys->count(); kk++) {
        if ( keys->at(kk)->getName() == keyName ) {
          ++numKeys;
          found = true;
          keys->at(kk)->setNumberInPanel(numKeys);
          keys->at(kk)->setPanel(panelNumber);
          keys->at(kk)->setLine(ii);
          keys->at(kk)->setPosBeg(posBeg);
          keys->at(kk)->setPosEnd(posEnd);
          keys->at(kk)->setMask( panelLine[ii].mid(posBeg+2,posEnd-posBeg-3) );
        }
      }
      if (!found) {
        _missingKeys += " " + keyName;
      }
    }
  }
}

// Destructor
////////////////////////////////////////////////////////////////////////////
t_panel::~t_panel() {
  delete buttonGroup ;
}

// Output Operator
////////////////////////////////////////////////////////////////////////////
r_file& operator<<(r_file& os, const t_panel& panel) {
  os << '\n' << '\n';

  for (int ii = 0; ii < (int) panel.panelLine.count(); ii++) {
    os << panel.panelLine[ii] << '\n';
  }
  return os;
}

// Decide if the Panel is visible or not
////////////////////////////////////////////////////////////////////////////
bool t_panel::isVisible() {
  QStringList keyValue;
  for (unsigned jj = 0; jj < condKey.count(); jj++) {
    if (condKey.at(jj) != 0) {
      if ( condKey.at(jj)->evalActiveIf() ) {
        condKey.at(jj)->updateSel();
        keyValue.append(condKey.at(jj)->getValue().stripWhiteSpace());
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

