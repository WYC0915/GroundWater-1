
/* -------------------------------------------------------------------------
 * Bernese GPS Software Version 5.0
 * -------------------------------------------------------------------------
 *
 * Class:      t_bpedial
 *
 * Purpose:    This class displays a dialog window with a BPE log

 * Author:     L. Mervart
 *
 * Created:    23-APR-2001
 *
 * Changes:
 *
 * Copyright:  Astronomical Institute
 *              University of Berne
 *                  Switzerland
 * -----------------------------------------------------------------------*/

#include <stdlib.h>
#include <qmessagebox.h>
//Added by qt3to4:
#include <QResizeEvent>

#include "bpedial.h"
#include "initmenu.h"
#include "bpe.h"
#include "errormsg.h"

// Constructor
////////////////////////////////////////////////////////////////////////////
t_bpedial::t_bpedial(QWidget* parent, const QString& caption, t_bpe* bpe) :
                     QDialog(parent, "", false) {
  this->setCaption(caption);

  _browser = new Q3TextView(this);
  _browser->setFont( initmenu.getFontSmall() );
  _browser->show();

  int xx, yy;
  if (parent) {
    xx = parent->x() + 50;
    yy = parent->y() + 50;
  } else {
    xx = 50;
    yy = 50;
  }
  int fontH = QFontMetrics(_browser->font()).height();
  int fontW = QFontMetrics(_browser->font()).width('W');
  this->setGeometry(xx, yy, 85*fontW, 25*fontH);

  _closeButton = new QPushButton("Close", this);
  connect(_closeButton, SIGNAL(clicked()), this, SLOT(slotClose()));
  _closeButton->setEnabled(false);
  _closeButton->show();

  _killButton = new QPushButton("Kill", this);
  connect(_killButton, SIGNAL(clicked()), this, SLOT(slotKill()));
  _killButton->show();

  _bpe = bpe;

  this->show();
}


// Destructor
////////////////////////////////////////////////////////////////////////////
t_bpedial::~t_bpedial() {
  if (_browser) {
    _browser->close(true); _browser = 0;
  }
  if (_closeButton) {
    _closeButton->close(true); _closeButton = 0;
  }
  if (_killButton) {
    _killButton->close(true); _killButton = 0;
  }
}

// Close Event
////////////////////////////////////////////////////////////////////////////
void t_bpedial::closeEvent(QCloseEvent* event) {
  if (_bpe && QMessageBox::warning(0, "Warning",
                          "Do you really want to kill the BPE server?",
                          QMessageBox::Yes | QMessageBox::Default,
                          QMessageBox::No) == QMessageBox::No ) {
    event->ignore();
  }
  else {
    event->accept();
  }
}

// Close
////////////////////////////////////////////////////////////////////////////
void t_bpedial::slotClose() {
  if (this->parent()) {
    close(true);
  }
  else {
    qApp->quit();
  }
}

// Response to the Window Resize
///////////////////////////////////////////////////////////////////////////////
void t_bpedial::resizeEvent(QResizeEvent *e) {
  this->resize(e->size());
  _browser->resize(e->size().width(), e->size().height()-20);
  _browser->move(0,0);
  _closeButton->setGeometry( 0, this->height()-20, 50, 20);
  _killButton->setGeometry( 70, this->height()-20, 50, 20);
}

// Close the Window
////////////////////////////////////////////////////////////////////////////
bool t_bpedial::close(bool alsoDelete) {
  if (_bpe && QMessageBox::warning(0, "Warning",
                          "Do you really want to kill the BPE server?",
                          QMessageBox::Yes | QMessageBox::Default,
                          QMessageBox::No) == QMessageBox::No ) {
    return false;
  }
  delete _bpe; _bpe = 0;
  qApp->quit();
  return QDialog::close(alsoDelete);
}

void t_bpedial::slotKill() {
  if (_bpe && QMessageBox::warning(0, "Warning",
                          "Do you really want to kill the BPE server?",
                          QMessageBox::Yes | QMessageBox::Default,
                          QMessageBox::No) == QMessageBox::No ) {
    return;
  }
  delete _bpe; _bpe = 0;
  qApp->quit();
  QDialog::close();
}

// Add a message to the browser
////////////////////////////////////////////////////////////////////////////
void t_bpedial::addText(const QString& msg, int maxLines) {
  if (!_browser) return;
  setText(_browser->text() + msg, maxLines);
}

// Display a message
////////////////////////////////////////////////////////////////////////////
void t_bpedial::setText(const QString& msg, int maxLines) {

  if (!_browser) return;

  if (maxLines == 0) {
    _browser->setText(msg);
  }
  else {
    int numLines = 0;
    int index    = 0;
    while ( (index = msg.findRev('\n', index-1)) != -1) {
      ++numLines;
      if (numLines > maxLines) {
        break;
      }
    }
    _browser->setText( msg.mid(index+1) );
  }
};

// Enable close button
////////////////////////////////////////////////////////////////////////////
void t_bpedial::enableClose() {
  _bpe = 0;
  _closeButton->setEnabled(true);
  _killButton->setEnabled(false);
};

