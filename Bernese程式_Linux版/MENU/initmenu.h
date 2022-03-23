
#ifndef INITMENU_H
#define INITMENU_H

#include <qstringlist.h>
#include <qfont.h>
#include <q3dict.h>

#include "inpfile.h"
#include "juldat.h"

#define DEFAULT_FONTTYPE_BASE  "Helvetica"
#define DEFAULT_FONTSIZE_BASE  12
#define DEFAULT_FONTTYPE_LARGE "Courier"
#define DEFAULT_FONTSIZE_LARGE 14
#define DEFAULT_FONTTYPE_SMALL "Courier"
#define DEFAULT_FONTSIZE_SMALL 12


class Q3NetworkOperation;

class t_initmenu : public QObject {

  Q_OBJECT

  public:
    enum intModus{INT, NOINT};
    enum logModus{normal, quiet};

    t_initmenu();
    ~t_initmenu();

    void              setMenuName(const QString& menuName) {
                        _menuName = menuName;
                      }
    const QString&    menuName() const {return _menuName;}
    int               readInput(const QString& inpFileName1,
                                const QString& inpFileName2);
    int               readInput(const QString& inpFileName);
    int               readInput();
    t_keyword*        getKey(const QString& keyName);
    QString           getKeySel0(const QString& keyName);
    QString           getPath(const QString& keyName);
    QString           getCmdPath(const QString& keyName);
    QString           getInpFileName(const QString& keyName);
    const QFont&      getFontBase() {initFonts(); return _fontBase;}
    const QFont&      getFontLarge(){initFonts(); return _fontLarge;}
    const QFont&      getFontSmall(){initFonts(); return _fontSmall;}
    void              setFontBase(const QFont& newFont);
    void              setFontLarge(const QFont& newFont);
    void              setFontSmall(const QFont& newFont);
    void              evalDollar(QString& ioString);
    void              evalDollar(const QString varName,
                                 QStringList& list, bool realyEval);
    int               getIntModus(){return _intModus;}
    void              setIntModus(int intModus){_intModus = intModus;}
    int               getLogModus(){return _logModus;}
    bool              getExcept(){return _exc;}
    void              setLogModus(int logModus){_logModus = logModus;}
    void              setExcept(bool except){_exc = except;}
    void              saveKey(const QString& keyName, const QString& value);
    const QString&    getActiveCamp() const {return _activeCamp;}
    void              setActiveCamp(const QString& newCamp);
    double            getMJD() const {return _MJD;}
    void              setMJD(double newMJD);
    const QString&    getSessChar() const {return _sessChar;}
    void              setSessChar(const QString& newSessChar);
    const QString&    getJobID() const {return _jobID;}
    void              setJobID(const QString& newJobID);

    const QStringList& list_of_sessions(unsigned ii = 0) const
                                              {return _list_of_sessions[ii];}
    int               updateListOfSessions();
    const QString&    primaryInputFileName() const {
                        return _primaryInputFileName;
                      }
    int               pid() {return _pid;}
    QString          auxInpName();
    QString          uniqueString();

    const Q3PtrList<t_inpfile>& inpfiles() const {return _inpfiles;}

    bool  modemMode() const {return _modemMode;}
    void  setModemMode(bool modemMode) {_modemMode = modemMode;}
    void* urlOp() {return _urlOp;}
    void  setUrlOp(void* urlOp) {_urlOp = urlOp;}

    QString host() const {return _host;}
    int     port() const {return _port;}
    void    setHost(const QString& host) {_host = host;}
    void    setPort(int port) {_port = port;}
    QString bnpPrefix() const {return "bnp://" + _host + ":" +
                               QString().sprintf("%d", _port);}

    QString getenv(const QString& name) const;

    void addScriptVariable(const QString& session, const QString& key, const QString& value) {
      _scriptVariables[session]._variables[key] = value;
    }

    const QMap<QString, QString>& scriptVariables(const QString& session) const {
      if (_scriptVariables.find(session) != _scriptVariables.end()) {
        return _scriptVariables[session]._variables;
      }
      else {
        return _emptyMap;
      }
    }

  private slots:
    void slotData(const QByteArray& buffer, Q3NetworkOperation* op);

  private:
    class t_scriptVariables {
     public:
      QMap<QString, QString> _variables;
    };

    static const QMap<QString, QString> _emptyMap;

    void              evalDollarStandard(QString& ioString);
    int               evalDollarHlp(double MJD, const QString& sessChar,
                                QString& ioString, int offset, int addLen = 0);
    void              clearInpfiles();
    void              initFonts();
    bool              _initFonts;
    int               _intModus;
    int               _logModus;
    QString           _menuName;
    QString           _primaryInputFileName;
    QString           _auxiliaryInputFileName;
    Q3PtrList<t_inpfile>  _inpfiles;
    QFont             _fontBase;
    QFont             _fontLarge;
    QFont             _fontSmall;
    QString           _activeCamp;
    double            _MJD;
    QString           _sessChar;
    QString           _jobID;
    QStringList       _list_of_sessions[2];
    int               _pid;
    bool              _exc;
    bool              _modemMode;
    void*             _urlOp;
    QString           _host;
    int               _port;
    Q3Dict<QString>*   _environment;
    mutable const Q3NetworkOperation* _oper;
    QMap<QString, t_scriptVariables> _scriptVariables;
};

extern t_initmenu initmenu;

#endif

