
#ifndef CPUFILE_H
#define CPUFILE_H

#include <q3valuelist.h>
#include <qstring.h>

#include "inpfile.h"

class r_dir;
class r_file;
class t_script;

class t_cpufile {

  private:
    class t_cpu {
      public:
        enum t_match{ no_match = 0, speed_match, exact_match };

        t_cpu(const QString& line, r_dir* lockDir);
        ~t_cpu();
        QString name()    const {return _name;}
        QString command() const {return _command;}
        QString speed()   const {return _speed;}
        int     maxJobs() const {return _maxJobs;}
        int     numJobs() const {return _numJobs;}
        void    readNumJobs();
        void    incrementNumJobs(const QString& idStr);
        void    decrementNumJobs(const QString& idStr);
        int     maxWait() const {return _maxWait;}
        t_match match(const QString& rqNam) const;
        QString toString() const;
        bool    ok() const {return _ok;}
      private:
        QString lockFileName(const QString& idStr);
        QString _name;
        QString _command;
        QString _speed;
        int     _maxJobs;
        int     _numJobs;
        int     _maxWait;
        bool    _ok;
        r_dir*  _lockDir;
    };

  public:
    t_cpufile(const char* fileName);
    ~t_cpufile();

    bool    findFreeCPU(const QString& idStr, const QString& reqCpuName,
                        QString& cpuNickname, QString& cpuCommand,
                        int& maxWait);
    QString getName() const {return _fileName;}
    int     maxJobs(const QString& cpuNickname) const;
    bool    ok() const {return _ok;}
    void    readFile();
    void    freeCPU(const QString& idStr, const QString& cpuNickname);
    void    resetCPU();
    bool    suspended(const QString& pcfName) const;
    void    updateTimeStatistics(const t_script* script, bool error,
                                 const QString& taskid);
    QString printTimeStatistics();

  private:
    class t_timeStatistics {
     public:
      t_timeStatistics() {
        numJobs    = 0;
        cpuTotal   = 0.0;
        cpuMax     = 0.0;
        cpuMin     = 0.0;
        queueTotal = 0.0;
        queueMax   = 0.0;
      }
      ~t_timeStatistics() {
      }
      int     numJobs;
      double  cpuTotal;
      double  cpuMax;
      double  cpuMin;
      double  queueTotal;
      double  queueMax;
      QString queueMaxSess;
      QString queueMaxPid;
    };
    QString             _fileName;
    Q3ValueList<t_cpu*>* _cpu;
    t_inpfile*          _inpfile;
    bool                _ok;
    r_dir*              _lockDir;
    QMap<QString, t_timeStatistics> _ts;
};

#endif

