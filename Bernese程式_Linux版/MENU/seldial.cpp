
/* -------------------------------------------------------------------------
 * Bernese GPS Software Version 5.0
 * -------------------------------------------------------------------------
 *
 * Class:      t_seldial
 *
 * Purpose:    This class implements a general selection dialog.
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

#include <QResizeEvent>

#include "seldial.h"
#include "initmenu.h"

// Constructor
////////////////////////////////////////////////////////////////////////////
t_seldial::t_seldial(QWidget *parent, const QString caption,
  const QStringList& inpList, QStringList& outList, bool multi) :
  QDialog(parent, caption, true) {

  p_outList = &outList;

  listBox = new QListWidget(this);
  if (multi) {
    listBox->setSelectionMode(QAbstractItemView::ExtendedSelection);
  }
  else {
    listBox->setSelectionMode(QAbstractItemView::SingleSelection);
  }

  listBox->addItems(inpList);

  for (int ii = 0; ii < listBox->count(); ii++) {
    for (int jj = 0; jj < outList.count(); jj++) {
      QListWidgetItem* it = listBox->item(ii);
      if (it->text() ==       outList[jj]       ||
          it->text() == '"' + outList[jj] + '"') {
        listBox->setItemSelected(it, true);
        listBox->setCurrentItem(it);
      }
    }
  }

  allButton = new QPushButton("ALL", this);
  connect( allButton, SIGNAL(clicked()), this, SLOT(selAll()) );
  allButton->setEnabled(multi);

  okButton = new QPushButton("OK", this);
  connect( okButton, SIGNAL(clicked()), this, SLOT(accept()) );

  cancelButton = new QPushButton("Cancel", this);
  connect( cancelButton, SIGNAL(clicked()), this, SLOT(reject()) );

  setCaption(caption);

  setFont( initmenu.getFontSmall() );

  int fontW = QFontMetrics(initmenu.getFontSmall()).width('W');
  resize(60*fontW, 40*fontW+30);
}

// Destructor
////////////////////////////////////////////////////////////////////////////
t_seldial::~t_seldial() {
  delete listBox;
  delete allButton;
  delete okButton;
  delete cancelButton;
}

// Response to the Window Resize
///////////////////////////////////////////////////////////////////////////////
void t_seldial::resizeEvent(QResizeEvent *e) {
  this->resize(e->size());

  listBox->setGeometry(0, 0, width(), height()-30);

  allButton->setGeometry(             0, height()-30, width()/3, 30);
  okButton->setGeometry(      width()/3, height()-30, width()/3, 30 );
  cancelButton->setGeometry(width()/3*2, height()-30, width()/3, 30 );
}

// Accept Selection
////////////////////////////////////////////////////////////////////////////
void t_seldial::accept(void) {
  p_outList->clear();

  for (int ii = 0; ii < listBox->count(); ii++) {
    QListWidgetItem* it = listBox->item(ii);
    if (listBox->isItemSelected(it)) {
      p_outList->append(it->text());
    }
  }

  QDialog::accept();
}

// Select All Items
////////////////////////////////////////////////////////////////////////////
void t_seldial::selAll(void) {
  for (int ii = 0; ii < listBox->count(); ii++) {
    QListWidgetItem* it = listBox->item(ii);
    listBox->setItemSelected(it,true);
  }
}
