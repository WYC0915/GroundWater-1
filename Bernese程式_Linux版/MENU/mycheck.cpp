
/* -------------------------------------------------------------------------
 * Bernese GPS Software Version 5.0
 * -------------------------------------------------------------------------
 *
 * Class:      t_mycheck
 *
 * Purpose:    Re-implements the QCheckBox class.
 *
 * Author:     L. Mervart
 *
 * Created:    13-AUG-2000
 *
 * Changes:
 *
 * Copyright:  Astronomical Institute
 *              University of Berne
 *                  Switzerland
 * -----------------------------------------------------------------------*/

#include <qdrawutil.h>
#include <qpixmap.h>
#include <qpixmapcache.h>
#include <qbitmap.h>
#include <qapplication.h>
#include <qstyle.h>
#include <qpainter.h>

#include "mycheck.h"

// Constructor
////////////////////////////////////////////////////////////////////////////
t_mycheck::t_mycheck(QWidget* parent, const char* name)
  : QCheckBox(parent, name) {
  _menstyle = new t_menstyle();
  this->setStyle(_menstyle);
}

// Destructor
////////////////////////////////////////////////////////////////////////////
t_mycheck::~t_mycheck() {
  /////  delete _menstyle;
}

