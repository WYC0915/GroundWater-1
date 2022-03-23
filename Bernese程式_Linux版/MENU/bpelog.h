
#ifndef BPELOG_H
#define BPELOG_H

#include <qstring.h>
#include "bpedial.h"

class t_script;
class t_server;
class t_bpe;
class r_file;

class t_bpelog : public QObject {

  public:
    enum t_msgtype {msg, error, debug, summary};

    t_bpelog(QWidget* parent, const QString& caption,
             const QString& logFileName, const QString& errFileName,
             t_bpe* bpe);
    ~t_bpelog();

    void message(t_script* script, const QString& msg, t_msgtype msgtype);
    void message(t_server* server, const QString& msg, t_msgtype msgtype);
    void message(const QString& msg, t_msgtype msgtype, int maxLines = 0);

  private:
    t_bpedial*  _bpedial;
    r_file*     _logFile;
    r_file*     _errFile;
};

#endif

