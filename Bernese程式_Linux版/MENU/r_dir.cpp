
/* -------------------------------------------------------------------------
 * Bernese GPS Software Version 5.1
 * -------------------------------------------------------------------------
 *
 * Class:      r_dir
 *
 * Purpose:    Remote directory (re-implements the QDir class)
 *
 * Author:     L. Mervart
 *
 * Created:    25-APR-2004
 *
 * Changes:
 *
 * Copyright:  Astronomical Institute
 *              University of Berne
 *                  Switzerland
 * -----------------------------------------------------------------------*/

#include <QtNetwork/qurlinfo.h>
#include <qstringlist.h>
#include <Q3ValueList>

#include "r_dir.h"
#include "initmenu.h"
#include "bnp.h"
#include "errormsg.h"
#include "menutils.h"

// Constructor
////////////////////////////////////////////////////////////////////////////
r_dir::r_dir(const QString& path, const QString& nameFilter,
             QDir::SortFlags sortSpec, QDir::Filters filterSpec) : QObject() {

  _dir   = new QDir(path, nameFilter, sortSpec, filterSpec);
  _urlOp = 0;
  _oper  = 0;

  if ( initmenu.modemMode() ) {
    if ( initmenu.urlOp() == 0 ) {
      _urlOp = new Q3UrlOperator();
      _urlOp->setProtocol("bnp");
      _urlOp->setHost( initmenu.host() );
      _urlOp->setPort( initmenu.port() );
      initmenu.setUrlOp(_urlOp);
    }
    else {
      _urlOp = (Q3UrlOperator*) initmenu.urlOp();
    }
    connect(_urlOp, SIGNAL( data(const QByteArray&, Q3NetworkOperation*) ),
            this,   SLOT(slotData(const QByteArray&, Q3NetworkOperation*)));

    connect(_urlOp, SIGNAL(newChildren(const Q3ValueList<QUrlInfo>&,
                                        Q3NetworkOperation*)),
            this,   SLOT(slotNewChildren(const Q3ValueList<QUrlInfo>&,
                                         Q3NetworkOperation*)));
  }
}

// Destructor
////////////////////////////////////////////////////////////////////////////
r_dir::~r_dir() {
  delete _dir;
}

// Separator
////////////////////////////////////////////////////////////////////////////
QChar r_dir::separator() {
  if (initmenu.modemMode()) {
    return '/';
  }
  else {
    return QDir::separator();
  }
}

// Director Name
////////////////////////////////////////////////////////////////////////////
QString r_dir::dirName() const {
  return _dir->dirName();
}

// Set Filter
////////////////////////////////////////////////////////////////////////////
void r_dir::setFilter(QDir::Filters filterSpec) {
  _dir->setFilter(filterSpec);
}

// Set Name Filter
////////////////////////////////////////////////////////////////////////////
void r_dir::setNameFilter(const QString& nameFilter) {
  _dir->setNameFilter(nameFilter);
}

// Exist ?
////////////////////////////////////////////////////////////////////////////
bool r_dir::exists() const {
  if ( ! initmenu.modemMode() ) {
    return _dir->exists();
  }
  else {
    _urlOp->setPath( dirName() + "?exist" );
    _urlOp->setNameFilter("");
    _oper = _urlOp->listChildren();
    bnp::waitForFinish(_oper) ;
    return _ansFlag != 0;
  }
}

// Exist ?
////////////////////////////////////////////////////////////////////////////
bool r_dir::exists(const QString& name, bool /* acceptAbsPath */) {
  if ( ! initmenu.modemMode() ) {
    return _dir->exists(name);
  }
  else {
    r_dir hlpDir(name);
    return hlpDir.exists();
  }
}

// Path
////////////////////////////////////////////////////////////////////////////
QString r_dir::path() const {
  return _dir->path();
}

// Make new Directory
////////////////////////////////////////////////////////////////////////////
bool r_dir::mkdir(const QString& dirName, bool acceptAbsPath) const {
  if (!initmenu.modemMode()) {
    return _dir->mkdir(dirName, acceptAbsPath);
  }
  else {
    return false;  // not yet implemented
  }
}

// List of Entries
////////////////////////////////////////////////////////////////////////////
QStringList r_dir::entryList(QDir::Filters filterSpec,
                             QDir::SortFlags sortSpec) const {
  if (!initmenu.modemMode()) {
    return _dir->entryList(filterSpec, sortSpec);
  }
  else {
    return entryList("*", filterSpec, sortSpec);
  }
}

// List of Entries
////////////////////////////////////////////////////////////////////////////
QStringList r_dir::entryList(const QString& nameFilter,
                             QDir::Filters filterSpec,
                             QDir::SortFlags sortSpec) const {
  if (!initmenu.modemMode()) {
    return _dir->entryList(nameFilter, filterSpec, sortSpec);
  }
  else {
    _children.clear();
    _urlOp->setPath( path() );
    _urlOp->setNameFilter(nameFilter);
    _oper = _urlOp->listChildren();
    bnp::waitForFinish(_oper) ;

    QStringList entries;
    Q3ValueList<QUrlInfo>::ConstIterator it;
    for (it = _children.begin(); it != _children.end(); it++) {
      if ( ( (*it).isFile() && (filterSpec & QDir::Files) ) ||
           ( (*it).isDir()  && (filterSpec & QDir::Dirs)  ) ) {
        entries.append( (*it).name() );
      }
    }
    return entries;
  }
}

// New data available
////////////////////////////////////////////////////////////////////////////
void r_dir::slotData(const QByteArray& buffer, Q3NetworkOperation* op) {

  if (_oper != op) {
    return;
  }

  if      (buffer.size() == 0) {
    _ansFlag = 0;
  }
  else if (buffer[0] == 'Y') {
    _ansFlag = 1;
  }
  else if (buffer[0] == 'N') {
    _ansFlag = 0;
  }
  else {
    _ansFlag = QString(buffer).toInt();
  }
}

// New Children available
////////////////////////////////////////////////////////////////////////////
void r_dir::slotNewChildren(const Q3ValueList<QUrlInfo>& urlInfo,
                            Q3NetworkOperation* op) {

  if (_oper != op) {
    return;
  }

  Q3ValueList<QUrlInfo>::ConstIterator it;
  for (it = urlInfo.begin(); it != urlInfo.end(); it++) {
    _children.append(*it);
  }
}

// Remove Directory Recursively (static)
////////////////////////////////////////////////////////////////////////////
bool r_dir::removeDir(const QString& dirName) {

  if (initmenu.modemMode()) {
    return false;  // not yet implemented
  }

  QDir dir(dirName);

  if (dir.exists()) {
    Q_FOREACH(QFileInfo info, dir.entryInfoList(QDir::NoDotAndDotDot |
                                                QDir::System         |
                                                QDir::Hidden         |
                                                QDir::AllDirs        |
                                                QDir::Files, QDir::DirsFirst)) {
      if (info.isDir()) {
        if (!r_dir::removeDir(info.absoluteFilePath())) {
          return false;
        }
      }
      else {
        if (!QFile::remove(info.absoluteFilePath())) {
          return false;
        }
      }
    }
    if (!dir.rmdir(dirName)) {
      return false;
    }
  }
  return true;
}

// Copy Directory Recursively (static)
////////////////////////////////////////////////////////////////////////////
bool r_dir::copyDir(const QString& fromPath, const QString& toPath) {

  if (initmenu.modemMode()) {
    return false;  // not yet implemented
  }

  QDir targetDir(toPath);
  if (!targetDir.exists()) {
    targetDir.mkpath(toPath);
  }

  QDir dir(fromPath);

  if (dir.exists()) {
    Q_FOREACH(QFileInfo info, dir.entryInfoList(QDir::NoDotAndDotDot |
                                                QDir::System         |
                                                QDir::Hidden         |
                                                QDir::AllDirs        |
                                                QDir::Files, QDir::DirsFirst)) {

      QString target = toPath + r_dir::separator() + info.fileName();

      if (info.isDir()) {
        r_dir::copyDir(info.absoluteFilePath(), target);
      }
      else {
        if (fileCopy(info.absoluteFilePath(), target) != success) {
          return false;
        }
      }
    }
    return true;
  }

  return true;
}

