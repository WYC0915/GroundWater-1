
/* -------------------------------------------------------------------------
 * Bernese GPS Software Version 5.0
 * -------------------------------------------------------------------------
 *
 * Class:      t_field
 *
 * Purpose:    This class implements the common behavior of different
 *             widgets (e.g. lineedit, uniline, selwin etc.)
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

#include "field.h"
#include "mycombobox.h"
#include "initmenu.h"
#include "selwin.h"
#include "uniline.h"
#include "mycheck.h"
#include "errormsg.h"
#include "menutils.h"
#include "canvas.h"
#include "myspinbox.h"
#include "r_file.h"

extern unsigned MAXFILENAMELENGTH;

// Constructor
////////////////////////////////////////////////////////////////////////////
t_field::t_field(t_canvas* canvas, t_keyword* key, t_panel* panel) {
  _canvas = canvas;
  _key    = key;
  _ok     = true;

  int col   = _key->getPosBeg() + 2;
  int row   = _key->getLine()   - 1;
  int width = _key->getWidth()  - 4;

  _key->setField(this);
  connect( _key, SIGNAL(keyChanged(t_keyword*)),
           _canvas, SLOT(slotFieldChanged()) );



  if (_key->getDesc()->widget() == "initmenu") {
    _widget = new QLabel( initmenu.getKeySel0(_key->getDesc()->pointer()) );
    _canvas->addWidget( _widget, row, col, 1, width);
  }

  else if (_key->getDesc()->widget() == "comment") {
    QString comment = _key->getValue();
    initmenu.evalDollar(comment);
    _widget = new QLabel( comment, _canvas );
    _canvas->addWidget( _widget, row, col, 1, width );
  }

  else if (_key->getDesc()->widget() == "uniline") {
    if ( _key->getDesc()->menuaux() ) {
      _key->updateSel();
    }
    _widget = new t_uniline(_canvas, _key, row, col);
  }

  else if (_key->getDesc()->widget() == "selwin") {
    _widget = new t_selwin(_canvas, 0, _key, row, col, width);
    ( (t_selwin*) _widget )->blockSignals(true);
    ( (t_selwin*) _widget )->setText(_key->getValue());
    ( (t_selwin*) _widget )->blockSignals(false);
    connect( (t_selwin*)_widget, SIGNAL(changed()),
             _key, SLOT(slotThisKeyMayChanged()) );
  }

  else if (_key->getDesc()->widget() == "combobox") {
    if (_key->getDesc()->isEditable()) {
      _widget = new t_mycombobox(true, _canvas, _key->getName().latin1());
    }
    else {
      _widget = new t_mycombobox(false, _canvas, _key->getName().latin1());
    }
    connect( (t_mycombobox*)_widget, SIGNAL(activated(const QString&)),
             _key, SLOT(slotThisKeyMayChanged()) );
    connect( (t_mycombobox*)_widget, SIGNAL(textChanged(const QString &)),
             _key, SLOT(slotThisKeyMayChanged()) );

    int currentItem = -1;
    QString value = _key->getValue().simplifyWhiteSpace();
    for (int ii = 0; ii < (int) _key->getDesc()->cards().count(); ii++) {
      ( (t_mycombobox*) _widget )->insertItem(_key->getDesc()->cards()[ii]);
      if (_key->getDesc()->cards()[ii] == value) {
        currentItem = ( (t_mycombobox*) _widget )->count() - 1;
      }
    }
    ( (t_mycombobox*) _widget )->insertItem("$(" + _key->getName() + ")");
    if (currentItem == -1) {
      ( (t_mycombobox*) _widget)->insertItem(value,0);
      currentItem = 0;
    }
    ( (t_mycombobox*) _widget)->setCurrentItem(currentItem);
    _canvas->addWidget( _widget, row, col, 1, width + 3, key->getName());
  }

  else if (_key->getDesc()->widget() == "lineedit") {
    if (_key->getDesc()->menuaux()) {
      _key->updateSel();
    }
    _widget = new QLineEdit(_canvas, _key->getName().latin1());
    connect( (QLineEdit*)_widget, SIGNAL(textChanged(const QString &)),
             _key, SLOT(slotThisKeyMayChanged()) );
    ( (QLineEdit*) _widget )->setText(_key->getValue());
    _canvas->addWidget( _widget, row, col, 1, width, key->getName() );
  }

  else if (_key->getDesc()->widget() == "spinbox") {
    _widget = new t_myspinbox(_key->getDesc()->range(0),
                              _key->getDesc()->range(1),
                              _key->getDesc()->range(2),
                              _canvas, _key->getName().latin1());
    t_myspinbox* spinbox = (t_myspinbox*)_widget;
    connect(spinbox, SIGNAL(valueChanged(const QString &)),
            _key, SLOT(slotThisKeyMayChanged()) );
    QString valStr = _key->getValue();
    if (valStr == spinbox->specialValueText()) {
      spinbox->setValue( spinbox->minimum() );
    }
    else {
      spinbox->setValue( valStr.toInt() );
    }
    _canvas->addWidget( _widget, row, col, 1, width + 3, key->getName() );
  }

  else if (_key->getDesc()->widget() == "checkbox") {
    _widget = new t_mycheck(_canvas, _key->getName().latin1());
    connect( (t_mycheck*)_widget, SIGNAL(toggled(bool)),
             _key, SLOT(slotThisKeyMayChanged()) );
    if (_key->getValue().toInt() == 1) {
      ( (t_mycheck*) _widget )->setChecked(true);
    }
    else {
      ( (t_mycheck*) _widget )->setChecked(false);
    }
    ( (t_mycheck*) _widget )->setFocusPolicy(Qt::StrongFocus);
    _canvas->addWidget( _widget, row, col, 1, width, key->getName() );
  }

  else if (_key->getDesc()->widget() == "radiobutton") {
    _widget = new QRadioButton(_canvas, _key->getName().latin1());
    connect( (QRadioButton*)_widget, SIGNAL(toggled(bool)),
             _key, SLOT(slotThisKeyMayChanged()) );
    connect( _key, SIGNAL(keyChanged(t_keyword*)),
             _canvas, SLOT(slotRadioButtonChanged()) );

    panel->getButtonGroup()->insert( (QRadioButton*) _widget );

    if (_key->getValue().toInt() == 1) {
      ( (QRadioButton*) _widget )->setChecked(true);
    }
    else {
      ( (QRadioButton*) _widget )->setChecked(false);
    }
    ( (QRadioButton*) _widget )->setFocusPolicy(Qt::StrongFocus);
    _canvas->addWidget( _widget, row, col, 1, width, key->getName() );
  }

  else {
    errormsg("Keyword " + _key->getName() + ":\n" +
             "empty or unknown widget type " + _key->getDesc()->widget());
    _ok = false;
  }

  // Enable/Disable the field
  // ------------------------
  if (_key->evalActiveIf()) {
    this->setEnabled(true);
  }
  else {
    this->setEnabled(false);
  }
}

// Destructor
////////////////////////////////////////////////////////////////////////////
t_field::~t_field() {
  _key->setField(0);
  delete _widget;
}

// Update Key Value
////////////////////////////////////////////////////////////////////////////
void t_field::updateKeyValue() {
  if (_key->getDesc()->widget() == "initmenu") {
  }

  else if (_key->getDesc()->widget() == "comment") {
  }

  else if (_key->getDesc()->widget() == "uniline") {
    _key->setValueList( ((t_uniline*)_widget)->text() );
  }

  else if (_key->getDesc()->widget() == "selwin") {
    _key->setValue( ((t_selwin*) _widget)->text() );
  }

  else if (_key->getDesc()->widget() == "combobox") {
    _key->setValue( ((t_mycombobox*) _widget)->currentText() );
  }

  else if (_key->getDesc()->widget() == "lineedit") {
    _key->setValue( ((QLineEdit*) _widget)->text() );
  }

  else if (_key->getDesc()->widget() == "spinbox") {
    ((t_myspinbox*) _widget)->interpretText();
    _key->setValue( ((t_myspinbox*) _widget)->text() );
  }

  else if (_key->getDesc()->widget() == "checkbox") {
    if ( ((t_mycheck*) _widget)->isChecked() ) {
      _key->setValue(QString("1"));
    }
    else {
      _key->setValue(QString("0"));
    }
  }

  else if (_key->getDesc()->widget() == "radiobutton") {
    if ( ((QRadioButton*) _widget)->isChecked() ) {
      _key->setValue(QString("1"));
    }
    else {
      _key->setValue(QString("0"));
    }
  }
}

// Check User Input
////////////////////////////////////////////////////////////////////////////
void t_field::checkKey(QString& msg) {

  if ( !_widget->isEnabled() ) {
    return;
  }

  QString msgLocal;

  if (_key->getDesc()->widget() == "uniline") {
    _key->updateSel(true, false);
  }
  else {
    _key->updateSel();
  }
  _key->expandSelList();

  // Check the number of selected files
  // ----------------------------------
  if ( _key->getDesc()->widget() == "selwin"      &&
      ( _key->getDesc()->path().contains("DIR_") ||
        _key->getDesc()->path().contains("PTH_") ) ) {
    if ( _key->getDesc()->emptyAllowed() != "true" &&
         _key->getSelList().count() == 0 ) {
      msgLocal += "\nEmpty Selection: " + _key->getName();
    }
    if (abs(_key->getDesc()->maxFiles()) < (int) _key->getSelList().count()) {
      msgLocal += "\nToo many Selected Items: " + _key->getName();
    }
  }

  // Check the number of lines for lineedit
  // --------------------------------------
  if (_key->getDesc()->widget() == "lineedit") {
    if (_key->getSelList().count() > 1                &&
        _key->getDesc()->multiLineAllowed() != "true" ) {
      msgLocal += "\nMultiple lines not allowed: " + _key->getName();
    }
    if (_key->getSelList().count() == 0                &&
        _key->getDesc()->emptyAllowed() == "false" ) {
      msgLocal += "\nEmpty Selection: " + _key->getName();
    }
  }

  // Check the number of selected items
  // ----------------------------------
  QString pointerName = _key->getDesc()->check_countas();
  if (!pointerName.isEmpty()) {
    t_keyword* keyPointer = _key->getInpfile()->getKey(pointerName);
    if (keyPointer) {
      if (keyPointer->getDesc()->widget() == "uniline") {
        keyPointer->updateSel(true, false);
      }
      else {
        keyPointer->updateSel();
      }
      keyPointer->expandSelList();
      if (_key->selListCount() != keyPointer->selListCount()) {
        msgLocal += "\nWrong Number of Selected Items: " + _key->getName();
      }
    }
    else {
      msgLocal += "\nKeyword not found: " + pointerName;
    }
  }

  // Check the editable fields
  // -------------------------
  for (int ii = 0; ii < _key->getSelList().count(); ii++) {
    if (_key->getDesc()->widget() == "uniline") {
      QStringList hlp;
      unilineSplit(_key->getSelList()[ii], hlp);
      QString msgHlp;
      bool emptyUniline = true;
      for (int jj = 0; jj < hlp.count(); jj++) {
        if (hlp[jj][0] == '#') {
          continue;
        }
        msgHlp += checkField(hlp[jj], jj+1);
        if (!hlp[jj].isEmpty()) {
          emptyUniline = false;
        }
      }
      if (!emptyUniline) {
        msgLocal += msgHlp;
      }
    }
    else {
      msgLocal += checkField(_key->getSelList()[ii]);
    }
  }

  if (!msgLocal.isEmpty()) {
    QString keyName = _key->getName();
    QString descrTxt = _key->getInpfile()->getKeySel0("DESCR_" + keyName);
    QString msgTxt   = _key->getInpfile()->getKeySel0("MSG_"   + keyName);
    if      (!descrTxt.isEmpty()) {
      msg += "\n" + descrTxt + msgLocal;
    }
    else if (!msgTxt.isEmpty()) {
      msg += "\n" + msgTxt + msgLocal;
    }
    else {
      msg += "\nKeyword: " +  keyName + msgLocal;
    }
  }
}

// Check User Input (one input field)
////////////////////////////////////////////////////////////////////////////
QString t_field::checkField(const QString& inp, unsigned ic) {

  bool    ok;

  // Check the Length of the Resulting String
  // ----------------------------------------
  int maxLength = _key->getDesc()->check_strlen(ic);

  if ( _key->getDesc()->path().contains("DIR_") ||
       _key->getDesc()->path().contains("PTH_") ) {
    if (maxLength == 0) {
      maxLength = MAXFILENAMELENGTH;
    }
  }

  if (maxLength > 0) {
    QString hlp = inp.stripWhiteSpace();
    if (hlp.length() > maxLength) {
      return "\nInput value too long: " + hlp;
    }
  }

  // Check the File existence
  // ------------------------
  if ( _key->getDesc()->widget() == "selwin"       &&
       ( _key->getDesc()->path().contains("DIR_") ||
         _key->getDesc()->path().contains("PTH_") ) ) {
    if (_key->getValue().find("~~") == -1) {
      int systemVarNotFound = 0;
      if ( !r_file::exists(expandEnvVar(inp, &systemVarNotFound)) ) {
        if (systemVarNotFound == 0) {
          return "\nFile does not exist: " + inp;
        }
      }
    }
  }

  // Check the Input Type (integer / real / date / time)
  // ---------------------------------------------------
  if      (_key->getDesc()->check_type(ic) == "integer") {
    inp.toInt(&ok);
    if (!ok && !inp.isEmpty()) {
      return "\nField " + inp + " is not integer";
    }
  }
  else if (_key->getDesc()->check_type(ic) == "real") {
    QString inpLocal = inp;
    inpLocal = inpLocal.replace(QRegExp("[dD]"), "e");
    inpLocal.toDouble(&ok);
    if (!ok && !inp.isEmpty()) {
      return "\nField " + inp + " is not real";
    }
  }
  else if (_key->getDesc()->check_type(ic) == "date") {
    QStringList ymd = QStringList::split(QRegExp("\\s+"), inp);
    if (ymd.count() != 3) {
      ok = false;
    }
    else {
      ymd[0].toInt(&ok);
      if (ok) {
        int month = ymd[1].toInt(&ok);
        if (ok) {
          if (month < 1 || month > 12) {
            ok = false;
          }
          else {
            int day = ymd[2].toInt(&ok);
            if (ok) {
              if (day < 1 || day > 31) {
                ok = false;
              }
            }
          }
        }
      }
    }
    if (!ok) {
      return "\nField " + inp + " is not date";
    }
  }
  else if (_key->getDesc()->check_type(ic) == "time") {
    QStringList hms = QStringList::split(QRegExp("\\s+"), inp);
    if (hms.count() != 3) {
      ok = false;
    }
    else {
      hms[0].toInt(&ok);
      if (ok) {
        int min = hms[1].toInt(&ok);
        if (ok) {
          if (min < 0 || min > 59) {
            ok = false;
          }
          else {
            int sec = hms[2].toInt(&ok);
            if (ok) {
              if (sec < 0 || sec > 59) {
                ok = false;
              }
            }
          }
        }
      }
    }
    if (!ok) {
      return "\nField " + inp + " is not time";
    }
  }

  // Check the Range
  // ---------------
  double min;
  if (_key->getDesc()->check_min(min, ic)) {
    QString inpLocal = inp;
    inpLocal = inpLocal.replace(QRegExp("[dD]"), "e");
    double val = inpLocal.toDouble(&ok);
    if (inp.isEmpty()) {
      val = 0.0;
      ok  = true;
    }
    if (val < min || !ok) {
      return "\nInput value out of range: " + inp;
    }
  }
  double max;
  if (_key->getDesc()->check_max(max, ic)) {
    QString inpLocal = inp;
    inpLocal = inpLocal.replace(QRegExp("[dD]"), "e");
    double val = inpLocal.toDouble(&ok);
    if (inp.isEmpty()) {
      val = 0.0;
      ok  = true;
    }
    if (val > max || !ok) {
      return "\nInput value out of range: " + inp;
    }
  }

  return "";
}

// Enable or Disable
////////////////////////////////////////////////////////////////////////////
void t_field::setEnabled(bool enable) {
  _enabled = enable;
  if      ( _key->getDesc()->widget() == "combobox" ) {
    ( (t_mycombobox*) _widget)->setEnabled(enable);
  }
  else if ( _key->getDesc()->widget() == "selwin" ) {
    ( (t_selwin*) _widget)->setEnabled(enable);
  }
  else {
    _widget->setEnabled(enable);
  }
}

// Set Value
////////////////////////////////////////////////////////////////////////////
void t_field::setValue(const QString& value) {
  if (_key->getDesc()->widget() == "selwin") {
    ((t_selwin*) _widget)->setText(value);
  }
  else if (_key->getDesc()->widget() == "lineedit") {
    ((QLineEdit*) _widget)->setText(value);
  }
  else if (_key->getDesc()->widget() == "checkbox") {
    if (value.isEmpty() || value.toInt() == 0) {
      ((t_mycheck*) _widget)->setChecked(false);
    }
    else {
      ((t_mycheck*) _widget)->setChecked(true);
    }
  }
}

// Set Button Group
////////////////////////////////////////////////////////////////////////////
void t_field::setButtonGroup(QButtonGroup* bg) {
  bg->addButton( (QRadioButton*) _widget );
}


