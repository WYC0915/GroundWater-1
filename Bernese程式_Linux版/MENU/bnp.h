
#ifndef BNP_H
#define BNP_H

#include <q3socket.h>
#include <q3networkprotocol.h>
#include <qstringlist.h>
//Added by qt3to4:
#include <Q3ValueList>

class bnp : public Q3NetworkProtocol
{
    Q_OBJECT

public:
    bnp();
    virtual ~bnp();
    virtual int supportedOperations() const;
    static void waitForFinish(const Q3NetworkOperation* op);

protected:
    virtual void operationListChildren(Q3NetworkOperation* op);
    virtual void operationGet(Q3NetworkOperation* op);
    virtual void operationPut(Q3NetworkOperation* op);
    virtual void operationRemove(Q3NetworkOperation* op);
    virtual void operationMkDir(Q3NetworkOperation* op);

    Q3Socket* _socket;
    bool     _connectionReady;

protected slots:
    void hostFound();
    void connected();
    void closed();
    void readyRead();
    void error(int);

private:
    bool                 checkConnection(Q3NetworkOperation* op);
    void                 close();
    void                 readDir();
    void                 readFile();
    void                 processBuffer();
    void                 writeCommand(const QString& cmd);
    Q3ValueList<QUrlInfo> parseDir();

    QByteArray _buffer;
};

#endif
