
/* -------------------------------------------------------------------------
 * Bernese GPS Software Version 5.1
 * -------------------------------------------------------------------------
 *
 * Class:      bnp
 *
 * Purpose:    Bernese Network Protocol
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
//Added by qt3to4:
#include <Q3ValueList>
#include <stdlib.h>
#include <q3urloperator.h>
#include <qstringlist.h>
#include <qregexp.h>
#include <qapplication.h>

#include "bnp.h"
#include "r_dir.h"
#include "menutils.h"
#include "errormsg.h"

const char   LASTLINE   = '.';
const QString EOM        = "\n.\n";
const QString msg_accept = "CONNECTION ACCEPTED";

// Constructor
////////////////////////////////////////////////////////////////////////////
bnp::bnp() : Q3NetworkProtocol() {
  _connectionReady = false;
  _socket          = new Q3Socket(this);

  connect(_socket, SIGNAL(hostFound()),        this, SLOT(hostFound()));
  connect(_socket, SIGNAL(connected()),        this, SLOT(connected()));
  connect(_socket, SIGNAL(connectionClosed()), this, SLOT(closed()));
  connect(_socket, SIGNAL(readyRead()),        this, SLOT(readyRead()));
  connect(_socket, SIGNAL(error(int)),         this, SLOT(error(int)));
}

// Destructor
////////////////////////////////////////////////////////////////////////////
bnp::~bnp() {
  close();
  delete _socket;
}

// List Children
////////////////////////////////////////////////////////////////////////////
void bnp::operationListChildren(Q3NetworkOperation* op) {
  op->setState( StInProgress );

  QString cmd;

  // RUN Operation (not available in QUrlOperator)
  // ---------------------------------------------
  if      (url()->path().find("RUN ") == 0) {
    cmd = url()->path();
  }
  // COPY Operation (ineffective in QUrlOperator - it uses GET and PUT)
  // ------------------------------------------------------------------
  else if (url()->path().find("COPY ") == 0) {
    cmd = url()->path();
  }
  // List Children Operation
  // -----------------------
  else {
    cmd = "DIR " + url()->path();
    if ( !url()->nameFilter().isEmpty() ) {
      cmd += "?" + url()->nameFilter();
    }
  }
  writeCommand(cmd);
}

// Send the Command
////////////////////////////////////////////////////////////////////////////
void bnp::writeCommand(const QString& cmd) {
  QByteArray cmdBytes = compress(cmd);
  _socket->writeBlock( cmdBytes.data(), cmdBytes.size() );
  _socket->writeBlock( EOM.latin1(), EOM.length() );
}

// Create new Directory
////////////////////////////////////////////////////////////////////////////
void bnp::operationMkDir(Q3NetworkOperation* op) {
  op->setState( StInProgress );
  QString dirName = op->arg(0);
  dirName.replace(QRegExp(" "), "_");
  QString cmd = "MKDIR " + dirName;
  writeCommand(cmd);
}

// Get File
////////////////////////////////////////////////////////////////////////////
void bnp::operationGet(Q3NetworkOperation* op) {
  op->setState( StInProgress );
  QString cmd     = "GET " + url()->path();
  writeCommand(cmd);
}

// Put File
////////////////////////////////////////////////////////////////////////////
void bnp::operationPut(Q3NetworkOperation* op) {
  op->setState( StInProgress );
  QString cmd = "PUT " + url()->path() + "\n" + QString(op->rawArg(1));
  writeCommand(cmd);
}

// Remove File
////////////////////////////////////////////////////////////////////////////
void bnp::operationRemove(Q3NetworkOperation* op) {
  op->setState( StInProgress );
  QString cmd     = "REM " + url()->path();
  writeCommand(cmd);
}

// Check Connection
////////////////////////////////////////////////////////////////////////////
bool bnp::checkConnection(Q3NetworkOperation* ) {

  if ( _socket->isOpen() && _connectionReady ) {
    return true;
  }
  if (_socket->isOpen()) {
    return false;
  }
  if (_socket->state() == Q3Socket::Connecting) {
    return false;
  }

  _connectionReady = false;
  _socket->connectToHost(url()->host(), url()->port());
  return false;
}

// Close Connection
////////////////////////////////////////////////////////////////////////////
void bnp::close() {
  if ( _socket->isOpen() ) {
    writeCommand("QUIT");
    _socket->flush();
    _socket->close();
  }
}

// Supported Operations
////////////////////////////////////////////////////////////////////////////
int bnp::supportedOperations() const {
  return OpListChildren | OpGet | OpPut | OpRemove | OpMkDir;
}

// Host found (slot)
////////////////////////////////////////////////////////////////////////////
void bnp::hostFound() {
  if ( url() ) {
    emit connectionStateChanged(ConHostFound,
                                tr("Host %1 found").arg(url()->host()));
  }
  else {
    emit connectionStateChanged(ConHostFound, tr("Host found"));
  }
}

// Connected (slot)
////////////////////////////////////////////////////////////////////////////
void bnp::connected() {
  if (url()) {
    emit connectionStateChanged(ConConnected,
                                tr("Connected to host %1").arg(url()->host()));
  }
  else {
    emit connectionStateChanged(ConConnected, tr("Connected to host"));
  }
}

// Closed (slot)
////////////////////////////////////////////////////////////////////////////
void bnp::closed() {
  if ( url() ) {
    emit connectionStateChanged(ConClosed,
                    tr("Connection to %1 closed").arg(url()->host()));
  }
  else {
    emit connectionStateChanged(ConClosed, tr("Connection closed"));
  }
}

// Data available (slot)
////////////////////////////////////////////////////////////////////////////
void bnp::readyRead() {

  // Read as many bytes as possible, add them to the buffer
  // ------------------------------------------------------
  for (;;) {
    unsigned dataLen = _socket->bytesAvailable();
    if (dataLen == 0) break;

    char* data = new char[dataLen];
    _socket->readBlock(data, dataLen);

    _buffer.detach();
    unsigned oldSize = _buffer.size();
    _buffer.resize(oldSize + dataLen);
    memcpy(_buffer.data() + oldSize, data, dataLen);
  }

  // Check the End of Message
  // ------------------------
  int bufLen = _buffer.size();
  if (bufLen >= 3                    &&
      _buffer[bufLen-3] == '\n'      &&
      _buffer[bufLen-2] == LASTLINE  &&
      _buffer[bufLen-1] == '\n'      ) {
    _buffer.detach();
    _buffer.resize(bufLen - 3);

    QString str;
    if (_buffer.size() > 0) {
      str = uncompress(_buffer);
    }

    unsigned newLen = str.length();
    _buffer.detach();
    _buffer.resize(newLen);
    memcpy(_buffer.data(), str.latin1(), newLen);
    processBuffer();
  }
}

// Process the entire buffer
////////////////////////////////////////////////////////////////////////////
void bnp::processBuffer() {

  // Process welcome message
  // -----------------------
  if (!_connectionReady) {
    QString hlp(_buffer);
    if ( hlp.stripWhiteSpace() == msg_accept ) {
      _connectionReady = true;
    }
    _buffer.truncate(0);
    return;
  }

  Q3NetworkOperation* op       = operationInProgress();
  int                operType = op->operation();
  if      (operType == Q3NetworkProtocol::OpListChildren) {
    Q3ValueList<QUrlInfo> urlList = parseDir();
    emit newChildren( urlList, op );
  }
  else if (operType == Q3NetworkProtocol::OpMkDir) {
    QString dirName = op->arg(0);
    dirName.replace(QRegExp(" "), "_");

    QUrlInfo inf;
    inf.setName(dirName);
    inf.setDir(true);
    emit createdDirectory(inf, op);
    emit newChild(inf, op);
  }
  else if (operType == Q3NetworkProtocol::OpGet) {
  }
  else if (operType == Q3NetworkProtocol::OpPut) {
  }
  else if (operType == Q3NetworkProtocol::OpRemove) {
    emit removed( op );
  }

  op->setState( StDone );

  emit data( _buffer, op );
  emit finished( op );

  _buffer.truncate(0);
}

// Process all events, with for operation finish (static)
////////////////////////////////////////////////////////////////////////////
void bnp::waitForFinish(const Q3NetworkOperation* op) {
  QApplication::setOverrideCursor( Qt::forbiddenCursor );
  while ( op->state() == Q3NetworkProtocol::StWaiting    ||
          op->state() == Q3NetworkProtocol::StInProgress ) {
    qApp->processEvents();
  }
  QApplication::restoreOverrideCursor();
}

// Error (slot)
////////////////////////////////////////////////////////////////////////////
void bnp::error( int code ) {
  if ( code == Q3Socket::ErrHostNotFound ||
     code == Q3Socket::ErrConnectionRefused ) {
    // this signal is called if connecting to the server failed
    if ( operationInProgress() ) {
      QString msg = tr( "Host not found or couldn't connect to: \n" + url()->host() );
      operationInProgress()->setState( StFailed );
      operationInProgress()->setProtocolDetail( msg );
      operationInProgress()->setErrorCode( (int)ErrHostNotFound );
      clearOperationQueue();
      emit finished( operationInProgress() );
    }
  }
}

// Read the Directory
////////////////////////////////////////////////////////////////////////////
Q3ValueList<QUrlInfo> bnp::parseDir() {

  emit start( operationInProgress() );

  Q3ValueList<QUrlInfo> urlList;

  QStringList files = QStringList::split( "\n", QString(_buffer));

  QStringList::ConstIterator it;
  for (it = files.begin(); it != files.end(); ++it) {
    if (*it == msg_accept || *it == "Y" || *it == "N") {
      continue;
    }
    QStringList hlp = QStringList::split(" ", *it);
    QUrlInfo inf;
    inf.setName(hlp[0]);
    inf.setPermissions(hlp[1].toInt());
    if (hlp[2] == "d") {
      inf.setDir(true);
    }
    else {
      inf.setDir(false);
    }
    inf.setFile( !inf.isDir() );
    urlList.append(inf);
  }

  return urlList;
}
