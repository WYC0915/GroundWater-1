
/* -------------------------------------------------------------------------
 * Bernese GPS Software Version 5.0
 * -------------------------------------------------------------------------
 *
 * Class:      t_selwin
 *
 * Purpose:    This class implements a new widget that consists of an
 *             editable field and a selection button. After pressing the
 *             button the selection dialog is shown.
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

#include <stdlib.h>
#include <qregexp.h>
#include <q3urloperator.h>

#include "selwin.h"
#include "myfildlg.h"
#include "initmenu.h"
#include "seldial.h"
#include "menutils.h"
#include "canvas.h"

// Constructor
////////////////////////////////////////////////////////////////////////////
t_selwin::t_selwin(t_canvas* canvas, QGridLayout* layout, t_keyword* Key,
                   int row, int col, int colSpan) :
                   QLineEdit(canvas ? canvas : layout->parentWidget(),
                             Key->getName().latin1()) {

  int fontW = 0;

  if (canvas) {
    canvas->addWidget(this, row, col, 1, colSpan, Key->getName());
  }
  else {
    fontW = QFontMetrics(this->font()).width('W');
    setMaximumWidth(11*fontW+2);
    layout->addWidget(this, row, col);
  }

  key = Key;

  if (canvas) {
    selButton = new QPushButton(canvas, Key->getName().latin1());
    canvas->addWidget(selButton, row, col+colSpan, 1, 3, Key->getName());
  }
  else {
    selButton = new QPushButton(layout->parentWidget(),
                                Key->getName().latin1());
    selButton->setMaximumWidth(5*fontW+2);
    layout->addWidget(selButton, row, col+1);
  }
  selButton->setText( key->getDotExtension().mid(1) );
  connect( selButton, SIGNAL(clicked()), this, SLOT(slotEvalList()) );

  timer = new QTimer(this);

  connect( this, SIGNAL(textChanged(const QString &)),
           this, SLOT(slotTextChanged()) );

  connect( timer, SIGNAL(timeout()),
           this, SLOT(slotTimerTimeout()) );
}

// Destructor
////////////////////////////////////////////////////////////////////////////
t_selwin::~t_selwin() {
  delete timer;
  delete selButton;
}

// Something has changed - start the timer
////////////////////////////////////////////////////////////////////////////
void t_selwin::slotTextChanged() {
  if (timer->isActive()) {
    timer->stop();
  }
  timer->start(1000);
}

void t_selwin::slotTimerTimeout() {
  timer->stop();
  emit changed();
}

// Show Selection Dialog (version using seldial dialog)
////////////////////////////////////////////////////////////////////////////
void t_selwin::slotEvalListSeldial(void) {
  QStringList oldSelList( key->getSelList() );

  if (key->getDesc()->maxFiles() != 0) key->setValue("*");
  if (key->updateSel() != 0) {  // test possible menuaux problem
    return;
  }
  QStringList maxSelList( key->getSelList() );

  // beg test
  QStringList::Iterator it;
  for (it = maxSelList.begin(); it != maxSelList.end(); ++it) {
    QStringList hlp; unilineSplit(*it, hlp);
    if (hlp.count() > 0) {
      (*it) = hlp[0];
    }
  }
  // end test

  key->storeSel(oldSelList);

////  key->setValue( this->text() );
////  key->updateSel();
////  QStringList newSelList( key->getSelList() );
////  key->storeSel(oldSelList);

  QStringList newSelList( oldSelList );

  bool multi = true;
  if ( abs( key->getDesc()->maxFiles() ) == 1) {
    multi = false;
  }
  t_seldial* dialog = new t_seldial(this, key->getName(),
                                    maxSelList, newSelList, multi);

  // Run the Dialog
  // --------------
  int irc = dialog->exec();

  if (irc == QDialog::Accepted) {
    key->storeSel(newSelList);
    key->setValueAccordingToSel();
    setText(key->getValue());
    emit changed();
  }
  delete dialog;
}

// Show Selection Dialog
////////////////////////////////////////////////////////////////////////////
void t_selwin::slotEvalList(void) {

  if ( !key->getDesc()->pointer().isEmpty() ||
       key->getDesc()->path().isEmpty() ) {
    slotEvalListSeldial();
    return;
  }
  bool multi = true;
  if ( abs( key->getDesc()->maxFiles() ) == 1) multi = false;

  t_myfildlg* dialog = new t_myfildlg(this, key, true, multi);

  QString value     = key->getValue();
  initmenu.evalDollar(value);
  QString directory = key->getPath();

#ifdef CELMECH
  if (key->getDesc()->path() == "PTH_ANY") {
    QString fileName = stripPath(value);
    directory        = stripFileName(value);
    value = fileName;
    dialog->setSelection(fileName);
  }
#endif

  // Set the Default Directory
  // -------------------------
  if (initmenu.modemMode()) {
    Q3UrlOperator* urlOp = (Q3UrlOperator*) initmenu.urlOp();
    urlOp->setPath( expandEnvVar(directory) );
    dialog->setUrl(*urlOp);
  }
  else {
    dialog->setDir(expandEnvVar(directory));
  }

  // Prepare the virtual Selection List
  // ----------------------------------
  QStringList oldSelList( key->getSelList() );
  key->setValue( this->text() );
  key->updateSel();
  QStringList virtualSelList( key->getSelList() );
  key->storeSel(oldSelList);

  // Filter the List of Files
  // ------------------------
  QString filter;
  if (this->text().isEmpty()) {
    filter = "*" + key->getDotExtension();
  }
  else {
    if (this->text() == "SELECTED") {
      QStringList::Iterator it;
      for (it = virtualSelList.begin(); it != virtualSelList.end(); ++it) {
        filter = filter + (*it) + " ";
      }
    }
    else {
      QStringList filterList;
      initmenu.evalDollar(this->text(), filterList, true);
      QStringList::Iterator it;
      for (it = filterList.begin(); it != filterList.end(); ++it) {
        filter = filter + (*it) + key->getDotExtension() + " ";
      }
    }
    filter = filter + ";;*" + key->getDotExtension();
  }

#ifdef CELMECH
  if (key->getDesc()->path() == "PTH_ANY") {
    if ( key->getDotExtension().isEmpty() ) {
      filter = "*";
    }
    else {
      filter = "*;;*" + key->getDotExtension();
    }
  }
#endif

  dialog->setFilters(filter);

  // Run the dialog
  // --------------
  int irc = dialog->exec();

  if (irc == QDialog::Accepted) {
    QStringList newSelList;
    if (multi) {
      newSelList = dialog->selectedFiles();
    }
    else {
      newSelList.append(dialog->selectedFile());
    }

#ifdef CELMECH
    extern QString celMechRoot;
#ifdef WIN32
    QString rpl = celMechRoot.replace(QRegExp("\\"), "/").upper();
#else
    QString rpl = celMechRoot;
#endif
    QStringList::Iterator it;
    for (it = newSelList.begin(); it != newSelList.end(); ++it) {
#ifdef WIN32
      (*it) = (*it).upper();
#endif
      (*it) = (*it).replace(QRegExp(rpl), "${CM}");
    }
#endif

    if (key->getDesc()->path() != "PTH_ANY") {
      bool stripExt = true;
      if (key->getDotExtension() == "") stripExt = false;

      QStringList::Iterator it;
      for (it = newSelList.begin(); it != newSelList.end(); ++it) {
        (*it) = stripPath(*it, stripExt);
      }
    }

    key->storeSel(newSelList);
    key->setValueAccordingToSel();
    setText(key->getValue());
    emit changed();
  }
  delete dialog;
}

// Enable or Disable
////////////////////////////////////////////////////////////////////////////
void t_selwin::setEnabled(bool enable) {
  QLineEdit::setEnabled(enable);
  selButton->setEnabled(enable);
}
