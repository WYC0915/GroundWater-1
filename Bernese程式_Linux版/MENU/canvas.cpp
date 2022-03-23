
/* -------------------------------------------------------------------------
 * Bernese GPS Software Version 5.0
 * -------------------------------------------------------------------------
 *
 * Class:      t_canvas
 *
 * Purpose:    This class displays one panel at time.
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

#include <q3accel.h>

#include "canvas.h"
#include "menu.h"
#include "keyword.h"
#include "initmenu.h"
#include "errormsg.h"
#include "uniline.h"

// Constructor
////////////////////////////////////////////////////////////////////////////
t_canvas::t_canvas(QWidget* parent) : QWidget(parent) {

  _layout   = new QGridLayout(this);
  _layout->setSpacing(0);
  setLayout(_layout);
  _panel    = 0;

  this->setFont( initmenu.getFontLarge() );

  _fields      = new Q3PtrList<t_field>();
  _buttonGroup = new Q3Dict<QButtonGroup>;
}

// Destructor
////////////////////////////////////////////////////////////////////////////
t_canvas::~t_canvas() {
  delete _buttonGroup;
  delete _fields;
}

// Display one Panel
////////////////////////////////////////////////////////////////////////////
void t_canvas::showPanel(t_inpfile* inpfile) {

  QApplication::setOverrideCursor( Qt::waitCursor );

  _panel = inpfile->getCurPan();

  for (int ii = 1; ii < ((int) _panel->getPanelLine().count())-1; ii++) {
    QString line = _panel->getPanelLine()[ii];
    line.truncate( line.findRev('#') );
    line[ line.find('#') ] = ' ';

    if (line.stripWhiteSpace().isEmpty()) {
      QLabel* label = new QLabel(line, this);
      addWidget(label, ii-1, 0, 1, line.length());
      continue;
    }

    QStringListIterator it(
          line.split(QRegExp(">[^<]+<|\\s{2,}"),QString::SkipEmptyParts) );
    int icLast = -1;
    while (it.hasNext()) {
      QString txt = it.next().stripWhiteSpace();
      if (!txt.isEmpty()) {
        QLabel* label = new QLabel(this);
        label->setScaledContents(true);
        label->setAlignment(Qt::AlignLeft);
        int ic = line.indexOf(txt, icLast+1);
        icLast = ic + txt.length();
        txt.replace(QRegExp("@")," ");
        label->setText(txt);
        addWidget(label, ii-1, ic, 1, txt.length());
        if (ii == 1) {
          QFont font = label->font();
          font.setBold(true);
          label->setFont(font);
        }
      }
    }
  }

  for (int jj = 1 ; jj <= _panel->getNumKeys(); jj++) {
    for (int kk = 0 ; kk < (int) inpfile->getKeys()->count(); kk++) {
      t_keyword* key = inpfile->getKeys()->at(kk);
      if (key->getPanel() == _panel->getNumber() &&
          key->getNumberInPanel() == jj         ) {
        t_field* field = new t_field(this, key, _panel);
        if (field->ok()) {
          _fields->append( field );
        }
        else {
          delete field;
        }
        if (!key->getDotExtension().isEmpty() &&
             key->getDesc()->widget() == "lineedit"  ) {
          QString extStr = key->getDotExtension().mid(1);
          QLabel* label = new QLabel(extStr, this);
          addWidget(label, key->getLine()-1,
                    key->getPosBeg()+key->getWidth()-1, 1, 3, key->getName());
        }

        if (key->getDesc()->widget() == "radiobutton") {
          QString group = key->getDesc()->group();
          QButtonGroup* bg = _buttonGroup->find( group );
          if (bg == 0) {
            bg = new QButtonGroup(this);
            _buttonGroup->insert( group, bg );
            //////            bg->hide();
          }
          _fields->last()->setButtonGroup(bg);
        }
      }
    }
  }

  show();

  slotRadioButtonChanged();
  QApplication::restoreOverrideCursor();

  if (_panel->getNumber() > 1) {
    for ( _fields->first(); _fields->current(); _fields->next() ) {
      if ( _fields->current()->enabled() ) {
        _fields->current()->setFocus();
        break;
      }
    }
  }

  if (!_panel->missingKeys().isEmpty()) {
    errormsg("Keys not found in current panel:\n" + _panel->missingKeys());
  }
}

// Print a message
////////////////////////////////////////////////////////////////////////////
void t_canvas::showMessage(const QString& /* message */) {
////  if (!_panel) {
////    erase();
////    int fontW = QFontMetrics(this->font()).width('W');
////    int xMsg  = width()/2  - message.length()*fontW/2;
////    int yMsg  = height()/2;
////    drawText(xMsg, yMsg, message);
////  }
}

// Save all Keyword belonging to the Panel
////////////////////////////////////////////////////////////////////////////
void t_canvas::savePanel() {
  for (int ii = 0; ii < (int) _fields->count(); ii++) {
    _fields->at(ii)->updateKeyValue();
  }
}

// Check all Keywords belonging to the Panel
////////////////////////////////////////////////////////////////////////////
int t_canvas::checkPanel(int currentSlotMenuItem) {

  const unsigned maxMsgLines = 10;
  QString msg;

  if (currentSlotMenuItem == MENU_ID_UPDPAN ||
      currentSlotMenuItem == MENU_ID_EDTINP ||
      currentSlotMenuItem == MENU_ID_EDTBPE) {
    return QMessageBox::Yes;
  }

  if (currentSlotMenuItem == MENU_ID_ABBREV) {
    checkAbbreviations(msg);
  }
  else {
    for (int ii = 0; ii < (int) _fields->count(); ii++) {
      _fields->at(ii)->checkKey(msg);
    }
  }
  if (!msg.isEmpty()) {

    if (msg.count('\n') > (int) maxMsgLines) {
      int index = -1;
      for (unsigned ii = 0; ii < maxMsgLines; ii++) {
        index = msg.find('\n', index+1);
      }
      msg.truncate(index);
      msg = msg + "\n... \n(List of messages truncated)";
    }

    QMessageBox mb( "Check Panel", msg, QMessageBox::Warning,
           QMessageBox::No | QMessageBox::Default | QMessageBox::Escape,
           QMessageBox::Yes, QMessageBox::NoButton);
    mb.setButtonText( QMessageBox::No,  "Back" );
    mb.setButtonText( QMessageBox::Yes, "Ignore" );
    return mb.exec();
  }
  else {
    return QMessageBox::Yes;
  }
}

// Field Changed Slot
////////////////////////////////////////////////////////////////////////////
void  t_canvas::slotFieldChanged() {
  for (unsigned ii = 0; ii < _fields->count(); ii++) {
    if (_fields->at(ii)->getKey()->evalActiveIf()) {
      _fields->at(ii)->setEnabled(true);
    }
    else {
      _fields->at(ii)->setEnabled(false);
    }
  }
  slotRadioButtonChanged();
  emit fieldChanged();
}

// Radio Button Changed Slot
////////////////////////////////////////////////////////////////////////////
void t_canvas::slotRadioButtonChanged() {
  savePanel();
  QStringList enableList;
  QStringList disableList;
  int ii, jj;
  for (ii = 0; ii < (int) _fields->count(); ii++) {
    t_keyword* key = _fields->at(ii)->getKey();
    if (key->getDesc()->widget() == "radiobutton") {
      if (key->getField()->enabled()) {
        for (jj = 0; jj < (int) key->getDesc()->radioKeys().count(); jj++) {
          if (key->getValue() == "1") {
            enableList.append(key->getDesc()->radioKeys()[jj]);
          }
          else {
            disableList.append(key->getDesc()->radioKeys()[jj]);
          }
        }
      }
      else {
        for (jj = 0; jj < (int) key->getDesc()->radioKeys().count(); jj++) {
          disableList.append(key->getDesc()->radioKeys()[jj]);
        }
      }
    }
  }
  for (ii = 0; ii < (int) _fields->count(); ii++) {
    t_keyword* key = _fields->at(ii)->getKey();
    for (jj = 0; jj < (int) enableList.count(); jj++) {
      if (key->getName() == enableList[jj]) {
        _fields->at(ii)->setEnabled(true);
      }
    }
    for (jj = 0; jj < (int) disableList.count(); jj++) {
      if (key->getName() == disableList[jj]) {
        _fields->at(ii)->setEnabled(false);
      }
    }
  }
}

// Check Abbreviation Table
////////////////////////////////////////////////////////////////////////////
void t_canvas::checkAbbreviations(QString& msg) {
  t_keyword* abbrevListKey = 0;
  for (int ii = 0; ii < (int) _fields->count(); ii++) {
     t_keyword* hlpKey = _fields->at(ii)->getKey();
     if (hlpKey->getName() == "LIST_OF_ABBREV") {
        abbrevListKey = hlpKey;
        break;
     }
  }

  if (abbrevListKey) {
    // Create the List of all Abbreviations
    // ------------------------------------
    QStringList station;
    QStringList abb4;
    QStringList abb2;
    for (int ii = 0; ii < (int) abbrevListKey->getValList().count(); ii++) {
      QStringList hlpList = QStringList::split(QRegExp("\"\\s\""),
                        abbrevListKey->getValList()[ii], true);
      if (hlpList.count() < 3) {
        errormsg("canvas.cpp: hlpList.count() < 3");
        return;
      }
      for (int jj = 0; jj < (int) hlpList.count(); jj++) {
        hlpList[jj] = hlpList[jj].replace(QRegExp("\"")," ").stripWhiteSpace();
      }
      station.append(hlpList[0]);
      abb4.append(hlpList[1]);
      abb2.append(hlpList[2]);

      // Check the Correct Length of the Abbreviations
      // ----------------------------------------------
      if (abb4.last().length() != 4) {
         QString hlp;
         hlp.sprintf("Wrong 4-ch Abbreviation '%s' of Station '%s'",
                     abb4.last().ascii(), station.last().ascii());
         msg = msg + hlp + "\n";
      }
      if (abb2.last().length() != 2) {
         QString hlp;
         hlp.sprintf("Wrong 2-ch Abbreviation '%s' of Station '%s'",
                     abb2.last().ascii(), station.last().ascii());
         msg = msg + hlp + "\n";
      }
    }

    // Check the both Lists of Abbreviations for Duplicities
    // -----------------------------------------------------
    for (int i4 = 0; i4 < (int) abb4.count(); i4++) {
      for (int ii = i4 + 1; ii < (int) abb4.count(); ii++) {
        if (abb4[i4] == abb4[ii]) {
           QString hlp;
           hlp.sprintf("Same Abbreviation '%s' of Stations '%s' and '%s'",
                  abb4[i4].ascii(), station[i4].ascii(), station[ii].ascii());
           msg = msg + hlp + "\n";
        }
        if (abb2[i4] == abb2[ii]) {
           QString hlp;
           hlp.sprintf("Same Abbreviation '%s' of Stations '%s' and '%s'",
                  abb2[i4].ascii(), station[i4].ascii(), station[ii].ascii());
           msg = msg + hlp + "\n";
        }
      }
    }
  }
}

// Check Abbreviation Table
////////////////////////////////////////////////////////////////////////////
QString t_canvas::focusWidgetName() {

  QWidget* ww = focusWidget();
  if (ww) {
    return ww->objectName();
  }
  else {
    return "";
  }
}

//
////////////////////////////////////////////////////////////////////////////
void t_canvas::addWidget(QWidget* widget, int row, int col,
                         int rSpan, int cSpan, const QString& toolTip) {

  const int fontW = QFontMetrics(this->font()).width('W');
  const int fontH = QFontMetrics(this->font()).height();

  for (int ii = row; ii < row+rSpan; ii++) {
    _layout->setRowMinimumHeight(ii, fontH+4);
  }
  for (int ii = col; ii < col+cSpan; ii++) {
    _layout->setColumnMinimumWidth(ii, fontW+1);
  }

  t_uniline* uniline = dynamic_cast<t_uniline*>(widget);
  if (uniline) {
    _layout->addWidget(widget, row, col, rSpan, cSpan);
  }
  else {
    QSize size(cSpan*fontW+6, rSpan*fontH+4);
    widget->setMinimumSize(size);
    widget->setMaximumSize(size);
    _layout->addWidget(widget, row, col, rSpan, cSpan,
                       Qt::AlignLeft | Qt::AlignTop);
  }

  setMinimumSize(_layout->minimumSize());

  // Tool Tip (keyword name)
  // -----------------------
  if (!toolTip.isEmpty()) {
    widget->setToolTip(toolTip);
  }
}


