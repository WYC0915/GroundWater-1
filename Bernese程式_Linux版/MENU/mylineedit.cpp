
/* -------------------------------------------------------------------------
 * Bernese GPS Software Version 5.0
 * -------------------------------------------------------------------------
 *
 * Class:      t_mylineedit
 *
 * Purpose:    Re-implements the QLineEdit class.
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

#include "mylineedit.h"
#include "errormsg.h"
//Added by qt3to4:
#include <QKeyEvent>

// Constructor
////////////////////////////////////////////////////////////////////////////
t_mylineedit::t_mylineedit(QWidget* parent, const char* name)
  : QLineEdit(parent, name) {
  _hash = false;
  connect( this, SIGNAL(textChanged(const QString&)),
           this, SLOT(slotTextChanged()) );
}

// Destructor
////////////////////////////////////////////////////////////////////////////
t_mylineedit::~t_mylineedit()
{
}

// Get Text + Hashes
////////////////////////////////////////////////////////////////////////////
QString t_mylineedit::text() const {
  if (_hash) {
    return '#' + QLineEdit::text() + '#';
  }
  else {
    return QLineEdit::text();
  }
}

// Set Text + Remove Hashes
////////////////////////////////////////////////////////////////////////////
void t_mylineedit::setText( const QString & str) {
  if (str.length() >= 2 && str[0] == '#' && str[(int)str.length()-1] == '#') {
    _hash = true;
    QLineEdit::setText(str.mid(1,str.length()-2));
    setReadOnly(true);
    setEnabled(false);
  }
  else {
    QLineEdit::setText(str);
  }
}

// Reset Focus
////////////////////////////////////////////////////////////////////////////
void t_mylineedit::keyPressEvent(QKeyEvent* e) {
  setFocus();
  QLineEdit::keyPressEvent(e);
}

// Slot Text Changed
////////////////////////////////////////////////////////////////////////////
void t_mylineedit::slotTextChanged() {
  emit textChanged(this);
}
