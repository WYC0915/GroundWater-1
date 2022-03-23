
#ifndef SERVER_H
#define SERVER_H

#include <qstring.h>
#include <q3serversocket.h>
#include <Q3PtrList>
#include <qtimer.h>
#include <qmap.h>

#include "script.h"
#include "bpeinp.h"
#include "pcffile.h"
#include "cpufile.h"
#include "bpelog.h"
#include "session.h"

class t_bpe;

class t_server : public Q3ServerSocket {
  Q_OBJECT

  public:
    enum t_status {waiting, running, finished, error};

  public:
    t_server(t_bpe* bpe, const QString& inpFileName, t_cpufile* cpufile,
             t_session* session, t_bpelog* bpelog, bool superBPE,
             bool firstSession, int numSessions, bool waitForHeader,
             bool reprMode);
    ~t_server();
    void  start();
    t_status  status() const {return _status;}
    const t_session* session() const {return _session;}
    bool debug() {return _debug;}
    QString pcfName(bool stripExtension) {
      return _pcffile->getOrigName(stripExtension);
    }
    void close(t_status status, bool useBPElog = true);
    t_cpufile* cpufile() {return _cpufile;}
    const Q3PtrList<t_script>* scriptList() const {return _scriptList;}
    const t_script* scriptAt(int index) const {return _scriptList->at(index);}

  signals:
    void serverFinished(const t_session* session);
    void singletonFinished(const QString& inMsg);

  public slots:
    void slotScriptFinished(const QString& inMsg);
    void slotStartTail();

  private slots:
    void slotStartScripts();

  private:
    void removeAllPrtFiles(const QString& taskid,
                           const QString& year,
                           const QString& session,
                           const QString& pth_bpeprt,
                           const QString& pth_bpelog,
                           const QString& ext_bpeprt,
                           const QString& ext_bpelog);
    void removeAllPrtFiles();
    int  readOptions();
    void newConnection(int socketNumber);
    void getServerOptions();
    bool canBeStarted(int iScript);
    int  getScriptIndex( const QString& PID, const QString& SUB_PID );
    int  getScriptIndex( const QString& inMsg );
    void initParallel(const QString& slavePID, const QString& inMsg);
    void timeVarsIntoOpt(int offset);
    t_bpe*           _bpe;
    t_bpeinp*        _inpfile;
    t_pcffile*       _pcffile;
    t_cpufile*       _cpufile;
    Q3PtrList<t_script>* _scriptList;
    t_bpelog*        _bpelog;
    t_session*       _session;
    QMap<QString,QString> _sOptions;
    t_status         _status;
    int              _cpuUpdRate;
    bool             _superBPE;
    bool             _firstSession;
    int              _numSessions;
    int              _startTailCounter;
    bool             _debug;
    QTimer*          _timer;
    bool             _waitForHeader;
    bool             _reprMode;
};

#endif

