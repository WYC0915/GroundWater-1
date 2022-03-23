
#ifndef KEYDESC_H
#define KEYDESC_H

#include <qstring.h>
#include <qstringlist.h>

class t_keydesc
{
  public:
    t_keydesc();
    ~t_keydesc();

    bool        isEmpty()      const;
    bool        isEditable(unsigned ic = 0) const;
    bool        menuaux()      const;
    int         maxFiles()     const;
    int         range(int ii)  const;
    QString     widget()       const {return getVal("widget");}
    QString     path()         const {return getVal("path");}
    QString     ext()          const {return getVal("ext");}
    QString     pointer()      const {return getVal("pointer");}
    QString     action()       const {return getVal("action");}
    QString     group()        const {return getVal("group");}
    QString     emptyAllowed() const {return getVal("emptyallowed");}
    QString     activeIf()     const {return getVal("activeif");}
    QString     updateIfSave() const {return getVal("updateifsave");}
    QString     multiLineAllowed() const {return getVal("multilineallowed");}
    QString     numlines()     const {return getVal("numlines");}
    bool        evalDollar()  const {return getVal("keepvariables") != "true";}

    QString     check_type(unsigned ic = 0) const;
    int         check_strlen(unsigned ic = 0) const;
    bool        check_min(double& min, unsigned ic = 0) const;
    bool        check_max(double& max, unsigned ic = 0) const;
    QString     check_countas(unsigned ic = 0) const;

    QStringList menuauxKeys()  const;
    QStringList radioKeys()    const;
    QStringList cards()        const;
    QStringList updateIfChanged() const;
    QStringList updateIfActive() const;
    QString     formatDescList() const;
    void        setDescLine(const QString& descLine);
    void        setDescList(const QStringList& list);

  private:
    QString     getVal(const QString& key, unsigned ic = 0) const;
    QStringList descList;
};

#endif
