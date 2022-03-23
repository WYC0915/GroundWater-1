
/* -------------------------------------------------------------------------
 * Bernese GPS Software Version 5.0
 * -------------------------------------------------------------------------
 *
 * Class:      t_textwin
 *
 * Purpose:    This class implements a text editor/browser. According to the
 *             modus it is possible to preview a html help file, or to
 *             preview or edit a plain text file.
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

#include <qmessagebox.h>
#include "textwin.h"
#include "initmenu.h"
#include "errormsg.h"
#include "menutils.h"
#include "r_file.h"

// Constructor
////////////////////////////////////////////////////////////////////////////
t_textwin::t_textwin(QWidget* parent, bool modal, const QString& fileName,
                     Modus modus, const QString& keyName,
                     const QString& action) :
  QDialog(parent, 0, modal, Qt::WDestructiveClose) {
  _browser      = 0;
  _editor       = 0;
  _saveButton   = 0;
  _closeButton  = 0;
  _forwButton   = 0;
  _backButton   = 0;
  _searchInput  = 0;
  _searchForwardsButton = 0;
  _searchBackwardsButton = 0;
  if (fileName.isEmpty()) {
    close();
  }
  else {
    displayFile(fileName, modus, keyName, action);
    if (modal) {
      this->exec();
    }
    else {
      this->show();
    }
  }
}

// Destructor
////////////////////////////////////////////////////////////////////////////
t_textwin::~t_textwin() {
  delete _browser     ;
  delete _editor      ;
  delete _saveButton  ;
  delete _closeButton ;
  delete _forwButton  ;
  delete _backButton  ;
  delete _searchInput ;
  delete _searchForwardsButton;
  delete _searchBackwardsButton;
  QDialog::close();
}

// Display File
////////////////////////////////////////////////////////////////////////////
void t_textwin::displayFile(const QString& fileName, Modus modus,
                   const QString& keyName, const QString& action) {

  _fileName = fileName;
  _action   = action;

  // Resolve the File Name and Extension
  // -----------------------------------
  if (modus == html) {
    _fileName = stripPath(_fileName, true);
    _fileName = initmenu.getPath("PTH_HLP") + _fileName + '.' +
                initmenu.getKeySel0("EXT_HLP");
  }

  // Set the Caption
  // ---------------
  setCaption(_fileName);

  // Expand the File Name and Check if the File Exists
  // -------------------------------------------------
  _showFile = expandEnvVar(_fileName);
  if ( !r_file::exists(_showFile) ) {
    errormsg("File does not exist: " + _fileName);
    close();
    return;
  }

  // Browse a HTML (usually help) File
  // ---------------------------------
  if (modus == html) {
    _browser = new t_myhtml(this);
    _browser->setSource(QUrl::fromLocalFile(_showFile));
    _browser->scrollToAnchor(keyName.stripWhiteSpace());

    _forwButton = new QPushButton("-->", this);
    connect( _forwButton, SIGNAL(clicked()), _browser, SLOT(forward()) );
    connect( _browser, SIGNAL(forwardAvailable(bool)),
             _forwButton, SLOT(setEnabled(bool)) );
    _forwButton->setEnabled(false);

    _backButton = new QPushButton("<--", this);
    connect( _backButton, SIGNAL(clicked()), _browser, SLOT(backward()) );
    connect( _browser, SIGNAL(backwardAvailable(bool)),
             _backButton, SLOT(setEnabled(bool)) );
    _backButton->setEnabled(false);

    int fontH = QFontMetrics(_browser->font()).height();
    int fontW = QFontMetrics(_browser->font()).width('W');
    this->resize(50*fontW, 25*fontH);
  }

  // Browse or Edit a Plain Text File
  // --------------------------------
  else if (modus == browse || modus == edit) {

    // Convert a binary file using MENUAUX Fortran Program
    // ---------------------------------------------------
    if (!_action.isEmpty()) {
      _showFile = expandEnvVar( initmenu.getPath("PTH_SCR") +
                                stripPath(_showFile) + "_ascii" );
      Q3PtrList<t_keyword>* hlp = new Q3PtrList<t_keyword>;
      hlp->setAutoDelete(true);
      hlp->append(new t_keyword("BINARY_FILE"));
      hlp->last()->storeSel(expandEnvVar(_fileName));
      if (_action == "OBSFILE") {
        hlp->append(new t_keyword("BINARY_FILE_2"));
        QString ext;
        QString fileName2 = stripExtension(_fileName, &ext);
        if      ( ext == initmenu.getKeySel0("EXT_CZH") ) {
          fileName2 += "." + initmenu.getKeySel0("EXT_CZO");
        }
        else if ( ext == initmenu.getKeySel0("EXT_PZH") ) {
          fileName2 += "." + initmenu.getKeySel0("EXT_PZO");
        }
        else if ( ext == initmenu.getKeySel0("EXT_CSH") ) {
          fileName2 += "." + initmenu.getKeySel0("EXT_CSO");
        }
        else if ( ext == initmenu.getKeySel0("EXT_PSH") ) {
          fileName2 += "." + initmenu.getKeySel0("EXT_PSO");
        }
        else {
          errormsg("displayFile: wrong extension " + ext);
          delete hlp;
          close();
          return;
        }
        hlp->last()->storeSel(expandEnvVar(fileName2));
        _showFile = stripExtension(_showFile) + ".ascii";
      }
      hlp->append(new t_keyword("ASCII_FILE"));
      hlp->last()->storeSel(expandEnvVar(_showFile));

      t_keyword dummy("DUMMY");
      dummy.menuaux("B2A_" + _action, hlp);
      delete hlp;
    }

    _editor = new QTextEdit(this);
    _editor->setLineWrapMode(QTextEdit::NoWrap);
    _editor->setFont( initmenu.getFontSmall() );
    int fontH = QFontMetrics(_editor->font()).height();
    int fontW = QFontMetrics(_editor->font()).width('W');
    this->resize(84*fontW, 30*fontH);

    r_file inFile(_showFile);
    if ( !inFile.open(QIODevice::ReadOnly) ) {
      errormsg("Cannot open file " + _showFile);
      close();
      return;
    }
    _editor->setText(inFile.readAll());
    inFile.close();
    if (_editor->document()) _editor->document()->setModified(false);

    if (modus == browse) {
      _editor->setReadOnly(true);
    }
    else {
      _saveButton = new QPushButton("Save", this);
      connect( _saveButton, SIGNAL(clicked()), this, SLOT(slotSave()) );
    }
  }

  _searchInput = new QLineEdit(this);
  connect( _searchInput, SIGNAL(textChanged(const QString &)),
           this, SLOT(slotEnableSearchButton(const QString&)) );

  _searchForwardsButton = new QPushButton("next", this);
  connect( _searchForwardsButton, SIGNAL(clicked()), this, SLOT(slotSearchForwards()) );
  _searchForwardsButton->setEnabled(false);

  _searchBackwardsButton = new QPushButton("prev", this);
  connect( _searchBackwardsButton, SIGNAL(clicked()), this, SLOT(slotSearchBackwards()) );
  _searchBackwardsButton->setEnabled(false);

  _closeButton = new QPushButton("Close", this);
  connect( _closeButton, SIGNAL(clicked()), this, SLOT(slotClose()) );

  // Define the Layout
  // -----------------
  QVBoxLayout* vLayout = new QVBoxLayout(this);
  QHBoxLayout* hLayout = new QHBoxLayout;

  if (_browser) {
    vLayout->addWidget(_browser);
  }
  if (_editor) {
    vLayout->addWidget(_editor);
  }

  if (_searchInput && _searchForwardsButton && _searchBackwardsButton) {
    hLayout->addWidget(new QLabel("Find"));
    hLayout->addWidget(_searchInput);
    hLayout->addWidget(_searchForwardsButton);
    hLayout->addWidget(_searchBackwardsButton);
    hLayout->addStretch(1);
  }

  if (_forwButton && _backButton) {
    hLayout->addWidget(_forwButton);
    hLayout->addWidget(_backButton);
    hLayout->addStretch(1);
  }

  if (_closeButton) {
    hLayout->addWidget(_closeButton);
  }
  if (_saveButton) {
    hLayout->addWidget(_saveButton);
  }

  vLayout->addLayout(hLayout);
}

// Close the Window
////////////////////////////////////////////////////////////////////////////
void t_textwin::slotClose() {
  if (_editor && _editor->document() && _editor->document()->isModified()) {
    switch ( QMessageBox::warning(0, "Warning",
             "File not saved. Save it now?",
             QMessageBox::Yes    | QMessageBox::Default,
             QMessageBox::No,
             QMessageBox::Cancel | QMessageBox::Escape) ) {
    case QMessageBox::Yes:
      slotSave();
      break;
    case QMessageBox::No:
      close();
      break;
    case QMessageBox::Cancel:
      return;
      break;
    }
  }
  else {
    close();
  }
}

// Save the Edited File
////////////////////////////////////////////////////////////////////////////
void t_textwin::slotSave() {
  if (_editor && _editor->document() && _editor->document()->isModified()) {
    r_file outFile(_showFile);
    if ( !outFile.open(QIODevice::WriteOnly | QIODevice::Text) ) {
      errormsg("Cannot write into file " + _showFile);
      return ;
    }
    outFile.writeBlock( _editor->text(), _editor->text().length() );
    outFile.close();
    if (!_action.isEmpty()) {
      Q3PtrList<t_keyword>* hlp = new Q3PtrList<t_keyword>;
      hlp->setAutoDelete(true);
      hlp->append(new t_keyword("BINARY_FILE"));
      hlp->last()->storeSel(expandEnvVar(_fileName));
      hlp->append(new t_keyword("ASCII_FILE"));
      hlp->last()->storeSel(expandEnvVar(_showFile));

      t_keyword dummy("DUMMY");
      dummy.menuaux("A2B_" + _action, hlp);
      delete hlp;
    }
  }
  close();
}

// Search
////////////////////////////////////////////////////////////////////////////
void t_textwin::slotEnableSearchButton(const QString& text) {
  if (text.isEmpty()) {
    _searchForwardsButton->setEnabled(false);
    _searchBackwardsButton->setEnabled(false);
  }
  else {
    _searchForwardsButton->setEnabled(true);
    _searchBackwardsButton->setEnabled(true);
  }
}

void t_textwin::slotSearchForwards() {
  if (_editor) {
    _editor->find(_searchInput->text());
  }
  else if (_browser) {
    _browser->find(_searchInput->text());
  }
}

void t_textwin::slotSearchBackwards() {
  if (_editor) {
    _editor->find(_searchInput->text(), QTextDocument::FindBackward);
  }
  else if (_browser) {
    _browser->find(_searchInput->text(), QTextDocument::FindBackward);
  }
}

// Close
////////////////////////////////////////////////////////////////////////////
bool t_textwin::close() {

  if (!_action.isEmpty() && !_showFile.isEmpty()) {
    r_file hlp( expandEnvVar(_showFile) ) ;
    if (hlp.exists()) {
      hlp.remove();
    }
  }
  return QDialog::close();
}

// Scroll to given key
////////////////////////////////////////////////////////////////////////////
void t_textwin::scrollToAnchor(const QString& anchor) {
  if (_browser) {
    _browser->scrollToAnchor(anchor);
  }
}
