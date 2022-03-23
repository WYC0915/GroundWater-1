
/* -------------------------------------------------------------------------
 * Bernese GPS Software Version 5.0
 * -------------------------------------------------------------------------
 *
 * Class:      t_bpeinp
 *
 * Purpose:    This class stores and handles one input file for the BPE
 *
 * Author:     L. Mervart
 *
 * Created:    04-MAY-2002
 *
 * Changes:
 *
 * Copyright:  Astronomical Institute
 *              University of Berne
 *                  Switzerland
 * -----------------------------------------------------------------------*/

#include "bpeinp.h"
#include "errormsg.h"

// Constructor
////////////////////////////////////////////////////////////////////////////
t_bpeinp::t_bpeinp(const QString& inpFileName, bool superBPE) :
  t_inpfile(inpFileName) {

  _superBPE = superBPE;
}

// Destructor
////////////////////////////////////////////////////////////////////////////
t_bpeinp::~t_bpeinp() {
}

// Return the Pointer to the Specified Keyword
////////////////////////////////////////////////////////////////////////////
t_keyword* t_bpeinp::getKey(const QString& keyName) {

  if (_superBPE) {
    QString hlp = "S_" + keyName;
    t_keyword* key = t_inpfile::getKey(hlp);
    if (key) {
      return key;
    }
  }

  return t_inpfile::getKey(keyName);
}

// Return the First Item of the Selection List of the required Keyword
////////////////////////////////////////////////////////////////////////////
QString t_bpeinp::getKeySel0(const QString& keyName) {

  if (_superBPE) {
    QString hlp = "S_" + keyName;
    t_keyword* key = t_inpfile::getKey(hlp);
    if (key) {
      return t_inpfile::getKeySel0(hlp);
    }
  }

  return t_inpfile::getKeySel0(keyName);
}
