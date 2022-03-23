
#ifndef R_FILE_H
#define R_FILE_H

#include <qobject.h>
#include <qfile.h>
#include <qfileinfo.h>
#include <q3urloperator.h>
//Added by qt3to4:
#include <QTextStream>

#include "menu.h"

class QTextStream;

class r_file : public QObject {

  Q_OBJECT

  public:
    r_file(const QString& name);
    virtual ~r_file();

    QString           name()  const;
    QString           absFilePath();
    bool              open(QIODevice::OpenMode mode);
    void              close();
    void              flush(bool append = true);
    QIODevice::Offset size() const;
    bool              exists() const;
    static bool       exists(const QString &fileName);
    bool              remove();
    static bool       remove(const QString& fileName);
    bool              rewind();
    bool              run();
    static bool       run(const QString& cmd);
    t_irc             copy(const QString& target);

    QString           readLine();
    QByteArray        readAll();
    void              writeBlock(const char* data, Q_ULONG len);

    int               setf(int bits);
    bool              eof() const;
    r_file&           operator >> (signed int& nn);
    r_file&           operator >> (unsigned int& nn);
    r_file&           operator >> (QString& ss);
    r_file&           operator << (char cc);
    r_file&           operator << (signed int nn);
    r_file&           operator << (unsigned int nn);
    r_file&           operator << (const QString& ss);
    r_file&           operator << (unsigned long nn);

  private slots:
    void slotData(const QByteArray& buffer, Q3NetworkOperation* op);

  private:
    t_irc readBuffer();

    QFile*                            _file;
    QTextStream*                      _ts;
    QFileInfo*                        _info;
    QByteArray                        _buffer;
    QIODevice::OpenMode               _mode;
    bool                              _firstWrite;
    Q3UrlOperator*                    _urlOp;
    mutable int                       _ansFlag;
    mutable const Q3NetworkOperation* _oper;
};

#endif

