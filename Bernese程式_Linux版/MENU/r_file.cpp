
/* -------------------------------------------------------------------------
 * Bernese GPS Software Version 5.1
 * -------------------------------------------------------------------------
 *
 * Class:      r_file
 *
 * Purpose:    Remote file (re-implements the QFile class)
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

#include <qtextstream.h>

#include "r_file.h"
#include "initmenu.h"
#include "menutils.h"
#include "errormsg.h"
#include "bnp.h"

// Constructor
////////////////////////////////////////////////////////////////////////////
r_file::r_file(const QString& name) : QObject() {
  _file       = new QFile(name);
  _firstWrite = true;
  _ts         = 0;
  _info       = 0;
  _mode       = QIODevice::NotOpen;
  _ansFlag    = 0;

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
  }
  else {
    _urlOp = 0;
  }

  _oper = 0;
}

// Destructor
////////////////////////////////////////////////////////////////////////////
r_file::~r_file() {
  delete _file;
  delete _ts;
  delete _info;
}

// (Local) Name of the File
////////////////////////////////////////////////////////////////////////////
QString r_file::name() const {
  return _file->name();
}

// (Local) Absolute Path of the File
////////////////////////////////////////////////////////////////////////////
QString r_file::absFilePath() {
  if (!_info) {
    _info = new QFileInfo(*_file);
  }
  return _info->absFilePath();
}

// Open the File
////////////////////////////////////////////////////////////////////////////
bool r_file::open(QIODevice::OpenMode mode) {

  _mode = mode;

  if (mode & QIODevice::ReadOnly) { // may be read-only or read-write
    if ( readBuffer() != success) {
      return false;
    }
  }
  _ts = new QTextStream(&_buffer, mode);
  return true;
}

// Close the File
////////////////////////////////////////////////////////////////////////////
void r_file::close() {
  delete _ts; _ts = 0;
  delete _info; _info = 0;
  this->flush();
}

// Flush the File
////////////////////////////////////////////////////////////////////////////
void r_file::flush(bool append) {

  if (_mode & QIODevice::WriteOnly) { // may be write-only or read-write

    // Local File
    // ----------
    if ( ! initmenu.modemMode() ) {
      QIODevice::OpenMode mode = _mode;
      if (_firstWrite) {
        _firstWrite = false;
      }
      else if (append) {
        mode |= QIODevice::Append;
      }
      if (_file->open(mode) ) {
        _file->writeBlock(_buffer.data(), _buffer.size());
        _file->close();
        _buffer.clear();
        delete _ts;
        _ts = new QTextStream(&_buffer, _mode);
      }
      else {
        errormsg("Cannot write into file " + name());
      }
    }

    // Network
    // -------
    else {
      _urlOp->setPath(name());
      _oper = _urlOp->put(_buffer);
      bnp::waitForFinish(_oper) ;
      _buffer.clear();
    }
  }
}

// Size of the buffer
////////////////////////////////////////////////////////////////////////////
qlonglong r_file::size() const {
  if ( ! initmenu.modemMode() ) {
    return _file->size();
  }
  else {
    if (_ts) {
      return _buffer.size();
    }
    else {
      _urlOp->setPath( name() + "?size" );
      _urlOp->setNameFilter("");
      _oper = _urlOp->listChildren();
      bnp::waitForFinish(_oper) ;
      return _ansFlag;
    }
  }
}

// Set Position
////////////////////////////////////////////////////////////////////////////
bool r_file::rewind() {
  delete _ts;
  _ts = new QTextStream(&_buffer, _mode);
  return _ts != 0;
}

// Read One Line
////////////////////////////////////////////////////////////////////////////
QString r_file::readLine() {
  return _ts->readLine();
}

// Read All Data
////////////////////////////////////////////////////////////////////////////
QByteArray r_file::readAll() {
  return _buffer;
}

// Write
////////////////////////////////////////////////////////////////////////////
void r_file::writeBlock(const char* data, Q_ULONG len) {
  QIODevice* dev = _ts->device();
  if (dev) {
    dev->write(data, len);
  }
}

// Set Flags
////////////////////////////////////////////////////////////////////////////
int r_file::setf(int bits) {
  return _ts->setf(bits);
}

// End Of File
////////////////////////////////////////////////////////////////////////////
bool r_file::eof() const {
  return _ts->atEnd();
}

// Input operators
////////////////////////////////////////////////////////////////////////////
r_file& r_file::operator>>(signed int& nn) {
  *_ts >> nn;
  return *this;
}

r_file& r_file::operator>>(unsigned int& nn) {
  *_ts >> nn;
  return *this;
}

r_file& r_file::operator>>(QString& ss) {
  *_ts >> ss;
  return *this;
}

// Output operators
////////////////////////////////////////////////////////////////////////////
r_file& r_file::operator<<(signed int nn) {
  *_ts << nn;
  return *this;
}

r_file& r_file::operator<<(unsigned int nn) {
  *_ts << nn;
  return *this;
}

r_file& r_file::operator<<(unsigned long nn) {
  *_ts << nn;
  return *this;
}

r_file& r_file::operator<<(char cc) {
  *_ts << cc;
  return *this;
}

r_file& r_file::operator<<(const QString& ss) {
  *_ts << ss;
  return *this;
}

// Exists or not?
////////////////////////////////////////////////////////////////////////////
bool r_file::exists() const {
  if ( ! initmenu.modemMode() ) {
    return _file->exists();
  }
  else {
    _urlOp->setPath( name() + "?exist" );
    _urlOp->setNameFilter("");
    _oper = _urlOp->listChildren();
    bnp::waitForFinish(_oper) ;
    return _ansFlag != 0;
  }
}

// Exists or not (static function)?
////////////////////////////////////////////////////////////////////////////
bool r_file::exists(const QString &fileName) {
  if ( ! initmenu.modemMode() ) {
    return QFile::exists(fileName);
  }
  else {
    r_file hlpFile(fileName);
    return hlpFile.exists();
  }
}

// Remove
////////////////////////////////////////////////////////////////////////////
bool r_file::remove() {
  if ( ! initmenu.modemMode() ) {
    return _file->remove();
  }
  else {
    _urlOp->setPath( name() );
    _oper = _urlOp->remove(name());
    bnp::waitForFinish(_oper) ;
    return _ansFlag != 0;
  }
}

// Remove (static function)
////////////////////////////////////////////////////////////////////////////
bool r_file::remove(const QString& fileName) {
  if ( ! initmenu.modemMode() ) {
    return QFile::remove(fileName);
  }
  else {
    r_file hlpFile(fileName);
    return hlpFile.remove();
  }
}

// Copy
////////////////////////////////////////////////////////////////////////////
t_irc r_file::copy(const QString& target) {
  if ( ! initmenu.modemMode() ) {
    return fileCopy(name(), target);
  }
  else {
    _urlOp->setPath( "COPY " + name() + " " + target);
    _oper = _urlOp->listChildren();
    bnp::waitForFinish(_oper) ;
    return _ansFlag ? success : failure;
  }
}

// Execute a Command
////////////////////////////////////////////////////////////////////////////
bool r_file::run() {
  if ( ! initmenu.modemMode() ) {
    return false;
  }
  else {
    _urlOp->setPath( "RUN " + name() );
    _urlOp->setNameFilter("");
    _oper = _urlOp->listChildren();
    bnp::waitForFinish(_oper) ;
    return _ansFlag != 0;
  }
}

// Execute a Command
////////////////////////////////////////////////////////////////////////////
bool r_file::run(const QString& cmd) {
  if ( ! initmenu.modemMode() ) {
    return false;
  }
  else {
    r_file hlpFile(cmd);
    return hlpFile.run();
  }
}

// Read Buffer (private)
////////////////////////////////////////////////////////////////////////////
t_irc r_file::readBuffer() {
  if ( ! initmenu.modemMode() ) {
    if ( _file->open(_mode) ) {
      _buffer = _file->readAll();
      _file->close();
      return success;
    }
  }
  else {
    _urlOp->setPath(name());
    _oper = _urlOp->get();
    bnp::waitForFinish(_oper) ;
    if (_oper->state() == Q3NetworkProtocol::StDone) {
      return success;
    }
    else {
      _buffer.resize(0);
      return failure;
    }
  }
  return failure;
}

// New data available
////////////////////////////////////////////////////////////////////////////
void r_file::slotData(const QByteArray& buffer, Q3NetworkOperation* op) {

  if (_oper != op) {
    return;
  }

  if (op->operation() == Q3NetworkProtocol::OpGet) {
    _buffer.duplicate(buffer);
  }
  else {
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
}
