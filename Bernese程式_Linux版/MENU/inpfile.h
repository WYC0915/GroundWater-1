
#ifndef INPFILE_H
#define INPFILE_H

#include <Q3PtrList>
#include <qstring.h>

#include "keyword.h"
#include "panel.h"

class t_inpfile {
  public:
    enum status{status_OK, status_NOT_OPENED, status_ERROR};

    t_inpfile(const QString& inpFileName, int menuItem = -1, bool expandSelLists = true);
    virtual ~t_inpfile();

    QString            getName();
    t_panel*           getCurPan(){return _panels->current();}
    void               setCurPan(t_panel* currentPanel);
    t_panel*           firstPan(){return _panels->first();}
    t_panel*           nextPan();
    t_panel*           prevPan();
    t_keyword*         firstKey(){return _keys->first();}
    t_keyword*         nextKey(){return _keys->next();}
    t_keyword*         currKey(){return _keys->current();}
    Q3PtrList<t_keyword>*  getKeys(){return _keys;}
    virtual t_keyword* getKey(const QString& keyName);
    virtual QString    getKeySel0(const QString& keyName);
    void               save(const QString& outFileName);
    void               save();
    QString            getStdOutFileName(){return _stdOutFileName;}
    QString            getErrorFileName(){return _errorFileName;}
    void               expandSelList(bool stepDefaultSysout = true);
    bool               hasPanels();
    int                status(){return _status;}
    int                menuItem() const {return _menuItem;}
    bool               expandSelLists() const {return _expandSelLists;}

  private:
    void               readFile();
    QString            defaultStdOutFileName(bool stepDefaultSysout);
    int                _status;
    QString            _fileName;
    Q3PtrList<t_keyword>*  _keys;
    Q3PtrList<t_panel>*    _panels;
    QString            _stdOutFileName;
    QString            _errorFileName;
    int                _menuItem;
    bool               _expandSelLists;
};

#endif
