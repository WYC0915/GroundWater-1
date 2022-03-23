
/* -------------------------------------------------------------------------
 * Bernese GPS Software Version 5.0
 * -------------------------------------------------------------------------
 *
 * Class:      t_myapp
 *
 * Purpose:    Re-implements the QApplication class.
 *
 * Author:     L. Mervart
 *
 * Created:    06-Sep-2006
 *
 * Changes:
 *
 * Copyright:  Astronomical Institute
 *              University of Berne
 *                  Switzerland
 * -----------------------------------------------------------------------*/

#include "myapp.h"

// Constructor 1
////////////////////////////////////////////////////////////////////////////
t_myapp::t_myapp(int &argc, char **argv) : QApplication(argc, argv) {
}

// Constructor 2
////////////////////////////////////////////////////////////////////////////
t_myapp::t_myapp(int &argc, char **argv, bool GUIenabled) :
  QApplication(argc, argv, GUIenabled) {
}

// Destructor
////////////////////////////////////////////////////////////////////////////
t_myapp::~t_myapp() {
}

// Event Handler - block FocuOut Events (problem with crash on remote display)
////////////////////////////////////////////////////////////////////////////
bool t_myapp::notify(QObject *receiver, QEvent *e){
  if (e->type() == QEvent::FocusOut) {
    return QApplication::notify(receiver, e);
  }
  else {
    return QApplication::notify(receiver, e);
  }
}

