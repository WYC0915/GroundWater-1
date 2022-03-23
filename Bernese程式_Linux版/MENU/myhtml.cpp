
/* -------------------------------------------------------------------------
 * Bernese GPS Software Version 5.0
 * -------------------------------------------------------------------------
 *
 * Class:      t_myhtml
 *
 * Purpose:    Re-implements the QTextBrowser class.
 *
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

#include <iostream>

#include "myhtml.h"
#include "errormsg.h"
#include "initmenu.h"
#include "menutils.h"
#include "r_file.h"

using namespace std;

// Constructor
////////////////////////////////////////////////////////////////////////////
t_myhtml::t_myhtml( QWidget* parent ) : QTextBrowser(parent) {
  _parent = parent;
  connect(this, SIGNAL(sourceChanged(const QUrl&)),
          this, SLOT(slotSourceChanged(const QUrl&)));
}

// Destructor
////////////////////////////////////////////////////////////////////////////
t_myhtml::~t_myhtml() {
}

// Re-Implementation of the setSource
////////////////////////////////////////////////////////////////////////////
void t_myhtml::setSource(const QUrl& url) {

  QString fileName = expandEnvVar( url.toLocalFile() );
  QString anchor   = url.fragment();

  if (initmenu.modemMode()) {
    r_file hlpFile(fileName);
    if (hlpFile.open(QIODevice::ReadOnly | QIODevice::Text) ) {
      QByteArray hlp = hlpFile.readAll();
      if (!isHtmlFile(fileName)) {
        hlp = "<pre>\n" + hlp + "</pre>\n";
      }
      QTextBrowser::setHtml(hlp);
    }
  }
  else {
    QUrl lUrl = QUrl::fromLocalFile(fileName);
    lUrl.setFragment(anchor);
    QTextBrowser::setSource(lUrl);
  }
}

// Re-Implementation of the setSource
////////////////////////////////////////////////////////////////////////////
void t_myhtml::slotSourceChanged(const QUrl& url) {
  QString fileName = expandEnvVar( url.toLocalFile() );
  _parent->setWindowTitle(fileName);
  if (!isHtmlFile(fileName)) {
    QFile hlpFile(fileName);
    if (hlpFile.open(QIODevice::ReadOnly | QIODevice::Text) ) {
      QByteArray hlp = "<pre>\n" + hlpFile.readAll() + "</pre>\n";
      QTextBrowser::setHtml(hlp);
    }
  }
}

//
////////////////////////////////////////////////////////////////////////////
bool t_myhtml::isHtmlFile(const QString& fileName) {
  QString ext; stripExtension(fileName, &ext);
  if (ext == "HTM"  || ext == "htm"  ||
      ext == "HTML" || ext == "html" ||
      ext == initmenu.getKeySel0("EXT_HLP") ) {
    return true;
  }
  else {
    return false;
  }
}
