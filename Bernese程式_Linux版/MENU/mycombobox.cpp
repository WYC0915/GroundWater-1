
/* -------------------------------------------------------------------------
 * Bernese GPS Software Version 5.0
 * -------------------------------------------------------------------------
 *
 * Class:      t_mycombobox
 *
 * Purpose:    Re-implements the QComboBox class.
 *
 * Author:     L. Mervart
 *
 * Created:    25-OCT-2003
 *
 * Changes:
 *
 * Copyright:  Astronomical Institute
 *              University of Berne
 *                  Switzerland
 * -----------------------------------------------------------------------*/

#include <q3listbox.h>

#include "mycombobox.h"
#include "errormsg.h"

// Constructor
////////////////////////////////////////////////////////////////////////////
t_mycombobox::t_mycombobox( bool rw, QWidget* parent, const char* name)
  : QComboBox(rw, parent, name) {

}

// Destructor
////////////////////////////////////////////////////////////////////////////
t_mycombobox::~t_mycombobox() {
}

// Re-Implemented Function
////////////////////////////////////////////////////////////////////////////
void t_mycombobox::setEnabled(bool enabled) {
  //if (! enabled) {
  //  listBox()->hide();
  //}
  QComboBox::setEnabled(enabled);
}
