
#ifndef SESSION_H
#define SESSION_H

#include <QtCore>

#include "juldat.h"

class t_script;

class t_session {
  public:
    enum t_status {waiting, running, finished, error};

  public:
    t_session(const QString& yearStr, const QString& sessStr, int offset);
    ~t_session();

    QString currYear(int offset=0) const ;
    QString currYr_4(int offset=0) const ;
    QString currSession(int offset=0) const ;
    QString currDayyear(int offset=0) const ;
    QString currMJD(int offset=0) const ;
    QString currDay(int offset=0) const ;
    QString currMonth(int offset=0) const ;
    QString currGPSweek(int offset=0) const ;
    QString currDayweek(int offset=0) const ;
    QString sessChar(int offset=0) const;

    t_status status() const {return _status;}
    void setStatus(t_status status) {_status = status;}
    bool foundInListOfSessions() {return _foundInListOfSessions;}

    void    updateTimeStatistics(const t_script* script, bool error,
                                 QString& taskid);
    QString printTimeStatistics();

    void rememberStartTime() {
      _ts.start = QDateTime::currentDateTime();
    }
    void rememberEndTime() {
      _ts.end = QDateTime::currentDateTime();
    }
    const QDateTime statStartTime() const {return _ts.start;}
    const QDateTime statEndTime()   const {return _ts.end;}
    double statBpe()       const {return _ts.start.secsTo(_ts.end);}
    double statCpu()       const {return _ts.cpu;}
    double statPgm()       const {return _ts.pgm;}
    double statAux()       const {return _ts.aux;}
    double statDelay()     const {return _ts.delay;}
    double statQueue()     const {return _ts.queue;}
    int    statNumPid()    const {return _ts.numPid;}
    int    statNumSubPid() const {return _ts.numSubPid;}
    int    statNumOk()     const {return _ts.numOk;}
    int    statNumError()  const {return _ts.numError;}
    int    statNumRerun()  const {return _ts.numRerun;}

  private:
    class t_timeStatistics {
     public:
      t_timeStatistics() {
        cpu       = 0.0;
        pgm       = 0.0;
        aux       = 0.0;
        delay     = 0.0;
        queue     = 0.0;
        numPid    = 0;
        numSubPid = 0;
        numOk     = 0;
        numError  = 0;
        numRerun  = 0;
      }
      ~t_timeStatistics() {
      }
      QDateTime start;
      QDateTime end;
      QString taskid;
      double cpu;
      double pgm;
      double aux;
      double delay;
      double queue;
      double maxCpu;   QString pidMaxCpu;
      double maxPgm;   QString pidMaxPgm;
      double maxAux;   QString pidMaxAux;
      double maxDelay; QString pidMaxDelay;
      double maxQueue; QString pidMaxQueue;
      int    numPid;
      int    numSubPid;
      int    numOk;
      int    numError;
      int    numRerun;
    };
    int      getJuldat(int offset, t_juldat& hlp) const;
    inline int dayOffset(int offset) const;
    double   _MJD;
    t_status _status;
    int      _index;
    bool     _wildCards;
    bool     _foundInListOfSessions;
    t_timeStatistics _ts;
};

#endif
