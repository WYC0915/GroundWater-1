
#ifndef BPE_H
#define BPE_H

#include <qstring.h>
#include <qwidget.h>
#include <qdatetime.h>
#include <qtimer.h>

#include "server.h"
#include "session.h"
#include "bpelog.h"

class t_script;

class t_bpe : public QObject {
  Q_OBJECT

  private:
    enum t_sbpe {serial, parallel, crazy};

  public:
    t_bpe(QWidget* parent, const QString& inpFileName);
    ~t_bpe();
    void updateTimeStatistics(const t_script* script, bool error,
                              const QString& taskid);
    QString printTimeStatistics();
    QString printTimeStatisticsSessions();
    const QString& inpFileNameNewCamp() const {return _inpFileNameNewCamp;}
    bool    timeExpired() const {return _timeExpired;}

  private slots:
    void slotServerFinished(const t_session*);
    void slotWriteStatus();

  private:
    class t_timeStatistics {
     public:
      t_timeStatistics() {
        numRuns  = 0;
        cpu      = 0.0;
        pgm      = 0.0;
        aux      = 0.0;
        cpuMax   = 0.0;
        numRerun = 0;
        numError = 0;
      }
      ~t_timeStatistics() {
      }
      QString scriptName;
      QString optDir;
      int     numRuns;
      double  cpu;
      double  pgm;
      double  aux;
      double  cpuMax;
      QString cpuMaxSess;
      QString cpuMaxPid;
      int     numRerun;
      int     numError;
    };
    void cleanAll();
    void startSessions();
    void startServer(int sessIndex);
    void startMessage();
    void endMessage(int numFinished, int numErrors);
    int  findServerIndex(const t_session* session);
    Q3PtrList<t_session> _sessionList;
    Q3PtrList<t_server>  _serverList;
    t_bpelog*        _bpelog;
    QString          _inpFileName;
    QString          _inpFileNameNewCamp;
    t_sbpe           _superBPE;
    unsigned         _maxParallel;
    int              _maxNumErrors;
    t_cpufile*       _cpufile;
    QTime            _startTime;
    r_file*          _statusFile;
    QTimer*          _statusTimer;
    QString          _pcfName;
    QString          _pcfNamePath;
    bool             _reprMode;
    QMap<QString, t_timeStatistics> _ts;
    bool             _timeExpired;
    int              _bpeMaxtime;
    double           _bpeTotalCPUtime;
};

#endif

