
#ifndef SCRIPT_H
#define SCRIPT_H

#include <q3socket.h>
#include <Q3PtrList>
#include <qmap.h>
#include <qtimer.h>

#include "inpfile.h"
#include "bpelog.h"

class t_server;

class t_script : public QObject {
  Q_OBJECT

  private:
    enum STATUS {waiting, skipped, running, finished};

  public:
    t_script(t_server* server, const QMap<QString, QString>& optionsString,
             int port, t_bpelog* bpelog, bool reprMode);
    ~t_script();

    void           setSocket(Q3Socket* newSocket);
    int            startClient(const QString& cpuNickname,
                               const QString& cpuCommand, int maxWait);
    void           stopClient(const QString& msg);
    QString        getKey(const QString& key) const ;
    void           setKey(const QString& key, const QString& value);
    const QMap<QString, QString>&  getOptions() const {return _options;};
    const QString& getCPUnickname() const {return _CPUnickname;};

    int            checkOptions(Q3PtrList<t_script>* scriptList);
    void           removePrtFile();

    QString pid() const {return getKey("PID");}
    QString subPid() const {return getKey("SUB_PID");}
    QString session() const {return getKey("SESSION");}
    QString idStr() const;
    const char* status() const;

    QString jump(const QStringList& msgList);
    QStringList jumps();
    bool cont_err() const;
    QStringList params();

    void resetStatus();
    void setRunning();
    void setFinished();
    void setSkipped();
    bool isFinished() const;
    bool isRunning() const;
    bool isSkipped() const;
    bool canBeStarted();

    bool debug() const {return _debug;}

    int  startCount() const {return _startCount;}
    void setStartCount(int startCount) {_startCount = startCount;}

    void setSpecialCamp(bool value = true) {_specialCamp = value;}
    bool specialCamp() {return _specialCamp;}

    bool isSingleton() const {return _singleton;}

    int    rerunCount() const {return _rerunCount;}
    double timeCpu()    const {return _timeCpu;}
    double timePgm()    const {return _timePgm;}
    double timeAux()    const {return _timeAux;}
    double timeDelay()  const {return _timeDelay;}
    double timeQueue()  const {return _timeQueue;}

  signals:
    void scriptFinished(const QString& inMsg);

  private slots:
    void slotReadLine();
    void slotTimeout();

  private:
    void      startUserScript();
    void      fillTimeStatistics(const QString& msg);
    void      updateScriptAddedVariables();

    int       _status;
    int       _serverPort;
    Q3Socket*  _socket;
    QMap<QString, QString> _options;
    QString   _CPUnickname;
    t_bpelog* _bpelog;
    bool      _debug;
    QTimer*   _timer;
    t_server* _server;
    int       _startCount;
    bool      _specialCamp;
    bool      _singleton;
    mutable QString*  _idStr;
    bool      _reprMode;

    int    _rerunCount;
    QTime  _timeStartClient;
    QTime  _timeStartScript;
    double _timeCpu;
    double _timePgm;
    double _timeAux;
    double _timeDelay;
    double _timeQueue;
};

#endif
