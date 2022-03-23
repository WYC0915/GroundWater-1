
/* -------------------------------------------------------------------------
 * Bernese GPS Software Version 5.0
 * -------------------------------------------------------------------------
 *
 * Class:      t_myspinbox
 *
 * Purpose:    Re-implements the QSpinbox class.
 *
 * Author:     L. Mervart
 *
 * Created:    11-NOV-2001
 *
 * Changes:
 *
 * Copyright:  Astronomical Institute
 *              University of Berne
 *                  Switzerland
 * -----------------------------------------------------------------------*/

#include <qlineedit.h>

#include "myspinbox.h"
#include "errormsg.h"

// Constructor
////////////////////////////////////////////////////////////////////////////
t_myspinbox::t_myspinbox( int minValue, int maxValue, int step,
                          QWidget* parent, const char* name )
  : QSpinBox(minValue - step, maxValue, step, parent, name) {
  this->lineEdit()->setName(name);
  setSpecialValueText("$(" + QString(name) + ")");
}

// Destructor
////////////////////////////////////////////////////////////////////////////
t_myspinbox::~t_myspinbox()
{
}

// Re-Implemented Function
////////////////////////////////////////////////////////////////////////////
void t_myspinbox::interpretText() {

  static bool aktive = false;

  if (!aktive) {
    aktive = true;
    int  rawVal = valueFromText(text());
    QSpinBox::interpretText();
    if (rawVal != value() &&
        (rawVal < minValue() || rawVal > maxValue())) {
      errormsg(QString().sprintf("Field %s:\nUser Input Changed to %d\n",
                                 name(), value()));
    }
    aktive = false;
  }
}
