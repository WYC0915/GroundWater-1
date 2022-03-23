
/* -------------------------------------------------------------------------
 * Bernese GPS Software Version 5.0
 * -------------------------------------------------------------------------
 *
 * Class:      t_myfildlg
 *
 * Purpose:    Re-implements the QFileDialog class.
 *
 * Author:     L. Mervart
 *
 * Created:    16-OCT-2000
 *
 * Changes:
 *
 * Copyright:  Astronomical Institute
 *              University of Berne
 *                  Switzerland
 * -----------------------------------------------------------------------*/

#include <qobject.h>
#include <qpushbutton.h>
#include <qcombobox.h>
//Added by qt3to4:
#include <QPixmap>

#include "myfildlg.h"
#include "textwin.h"
#include "initmenu.h"
#include "menutils.h"
#include "errormsg.h"

static const char* const open_xpm[]={
    "16 16 6 1",
    ". c None",
    "b c #ffff00",
    "d c #000000",
    "* c #999999",
    "c c #cccccc",
    "a c #ffffff",
    "................",
    "................",
    "...*****........",
    "..*aaaaa*.......",
    ".*abcbcba******.",
    ".*acbcbcaaaaaa*d",
    ".*abcbcbcbcbcb*d",
    "*************b*d",
    "*aaaaaaaaaa**c*d",
    "*abcbcbcbcbbd**d",
    ".*abcbcbcbcbcd*d",
    ".*acbcbcbcbcbd*d",
    "..*acbcbcbcbb*dd",
    "..*************d",
    "...ddddddddddddd",
    "................"};

static QPixmap* openFolderIcon = 0;

// Constructor
////////////////////////////////////////////////////////////////////////////
t_myfildlg::t_myfildlg( QWidget* parent, t_keyword* key,
                        bool modal, bool multi)
  : Q3FileDialog(parent, key->getName(), modal) {

  _key = key;

  setCaption(key->getName());

  _browseButton = new QPushButton("Browse", this);
  addWidgets(0, 0, _browseButton);
  connect( _browseButton, SIGNAL(clicked()), this, SLOT(slotBrowse()));

  if (multi) {
    setMode(Q3FileDialog::ExistingFiles);
    _allButton = new QPushButton("Select All", this);
    addWidgets(0, 0, _allButton);
    connect( _allButton, SIGNAL(clicked()), this, SLOT(slotSelectAll()));
  }
  else {
    _allButton = 0;
    setMode(Q3FileDialog::ExistingFile);
  }
}

// Destructor
////////////////////////////////////////////////////////////////////////////
t_myfildlg::~t_myfildlg() {
  delete _browseButton;
  delete _allButton;
}

// Select All Files
////////////////////////////////////////////////////////////////////////////
void t_myfildlg::slotSelectAll() {
  selectAll(true);
}

// Browse the selected file
////////////////////////////////////////////////////////////////////////////
void t_myfildlg::slotBrowse() {

    QString action;

  QString ext = _key->getDotExtension();
  int index = ext.find(".");
  if (index != -1) {
    ext = ext.mid(index+1);
  }

#ifndef CELMECH
  if ( ext == initmenu.getKeySel0("EXT_CZH") ||
       ext == initmenu.getKeySel0("EXT_PZH") ||
       ext == initmenu.getKeySel0("EXT_CSH") ||
       ext == initmenu.getKeySel0("EXT_PSH") ) {
    action = "OBSHEAD";
  }

  if ( ext == initmenu.getKeySel0("EXT_STD") ||
       ext == initmenu.getKeySel0("EXT_RPR") ||
       ext == initmenu.getKeySel0("EXT_NEQ") ||
       ext == initmenu.getKeySel0("EXT_RES") ) {
    return;
  }
#endif

  QString fileName;
  if      (mode() == Q3FileDialog::ExistingFile) {
    fileName = selectedFile();
  }
  else if (mode() == Q3FileDialog::ExistingFiles &&
           selectedFiles().count() > 0) {
    fileName = selectedFiles()[0];
  }

  if (!stripPath(fileName).isEmpty()) {
    if (initmenu.modemMode()) {
      fileName = stripBnpPrefix(fileName);
    }
    new t_textwin(this, true, fileName, t_textwin::browse, "", action);
  }
}

// Get File Name (read-only)
////////////////////////////////////////////////////////////////////////////
QString t_myfildlg::getOpenFileName( const QString &initially,
                                     const QString &filter,
                                     QWidget *parent, const char* name,
                                     const QString &caption,
                                     QString *selectedFilter,
                                     bool resolveSymlinks) {
  if (!initmenu.modemMode()){
    return Q3FileDialog::getOpenFileName(initially, filter, parent, name,
                                caption, selectedFilter, resolveSymlinks);
  }
  else {
    return t_myfildlg::getFileName(initially, filter, parent, name, caption);
  }
}

// Get File Names (read-only)
////////////////////////////////////////////////////////////////////////////
QStringList t_myfildlg::getOpenFileNames( const QString &filter,
                                          const QString &dir,
                                          QWidget *parent,
                                          const char* name,
                                          const QString &caption,
                                          QString *selectedFilter,
                                          bool resolveSymlinks) {
  QStringList ans;
  if (!initmenu.modemMode()){
    ans = Q3FileDialog::getOpenFileNames(filter, dir, parent, name, caption,
                                         selectedFilter, resolveSymlinks);
  }
  else {
    ans.append( t_myfildlg::getFileName(dir, filter, parent, name, caption) );
  }
  ans.sort();
  return ans;
}

// Get File Name (file may or may not exist)
////////////////////////////////////////////////////////////////////////////
QString t_myfildlg::getFileName(const QString& dir,
                                const QString& filter,
                                QWidget*       parent,
                                const char*    name,
                                const QString& caption,
                                const QString& addDir) {

  QString lDir = stripFileName(dir);
  QString sel  = stripPath(dir);

  Q3FileDialog* fd = new Q3FileDialog(lDir, filter, parent, name, true);

  if (initmenu.modemMode()){
    fd->setUrl( initmenu.bnpPrefix() + lDir );
  }

  if (!sel.isEmpty()) {
    fd->setSelection(sel);
  }

  fd->setMode( Q3FileDialog::AnyFile );
  fd->setCaption(caption);

  if (addDir != QString::null) {
    QListIterator<QComboBox*> it( fd->findChildren<QComboBox*>() );
    while ( it.hasNext() ) {
      QComboBox* obj = it.next();
      if ( QString(obj->name()) == "directory history/editor" ) {
        if (! openFolderIcon ) {
          openFolderIcon = new QPixmap( (const char **)open_xpm);
        }
        obj->insertItem(*openFolderIcon, addDir);
      }
    }
  }

  QListIterator<QPushButton*> it( fd->findChildren<QPushButton*>() );
  while (it.hasNext()) {
    QPushButton* btn = it.next();
    if (btn->text().indexOf("Save") != -1) {
      btn->setText("Ok");
    }
  }

  QString fileName;
  if (fd->exec() == QDialog::Accepted) {
    fileName = fd->selectedFile();
    if (initmenu.modemMode()) {
      fileName = stripBnpPrefix(fileName);
    }
  }

  delete fd;
  return fileName;
}

