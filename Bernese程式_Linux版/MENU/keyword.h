
#ifndef KEYWORD_H
#define KEYWORD_H

#include <qobject.h>
#include <Q3PtrList>
#include <qstring.h>
#include <qstringlist.h>

#include "keydesc.h"

class t_inpfile;
class t_field;
class r_file;

class t_keyword : public QObject
{
  Q_OBJECT

  public:
    enum updateStatus {update_always, update_yes, update_no};


    t_keyword(const char* newName);
    t_keyword(QString& line, r_file& in, t_inpfile* inpFile);
    ~t_keyword();

    void             setValue (const QString& newValue);
    void             setValueList(const QStringList& newValueList)
                              {valueList = newValueList;}
    void             setSelList(const QStringList& newSelList)
                              {selList = newSelList;}
    void             setMask  (const QString& newMask){mask = newMask;}
    void             setValueAccordingToSel();
    void             setDesc  (const char* descLine);
    void             setPanel (int newPanel ){panel   = newPanel ;}
    void             setNumberInPanel (int number){numberInPanel = number ;}
    void             setLine  (int newLine  ){lineNum = newLine  ;}
    void             setPosBeg(int newPosBeg){posBeg  = newPosBeg;}
    void             setPosEnd(int newPosEnd){posEnd  = newPosEnd;}
    void             setField(t_field* newField){field = newField;}

    const QString&     getName   (void) const {return name;}
    const t_keydesc*   getDesc   (void) const {return desc;}
    QString            getValue  (void) const;
    QString            getSel0   (void) const;
    const QString&     getMask   (void) const {return mask;}
    const QStringList& getSelList(void) const {return selList;}
    const QStringList& getValList(void) const {return valueList;}
    int                selListCount() const {return selList.count();}
    int                getPanel  (void) const {return panel;}
    int                getNumberInPanel (void) const {return numberInPanel;}
    int                getLine   (void) const {return lineNum;}
    int                getPosBeg (void) const {return posBeg;}
    int                getPosEnd (void) const {return posEnd;}
    int                getWidth  (void) const {return posEnd-posBeg+1;}
    t_inpfile*         getInpfile(void) {return inpfile;}
    t_field*           getField(void) {return field;}

    QString            getPath() const ;
    QString            getDotExtension() const ;

    int              updateSel(bool pointerUpdate=true, bool menuauxFlg=true);
    int              menuaux(const QString& action, Q3PtrList<t_keyword>* keyList);
    void             storeSel(const QStringList& inpList);
    void             storeSel(const QString& str);
    void             saveInputFile();
    void             expandSelList();

    bool             evalActiveIf() const;
    bool             evalUpdateIfSave() const;

    friend r_file& operator<<(r_file& os, t_keyword& key);

  private slots:
    void slotThisKeyMayChanged();
    void slotThisKeyMayChanged(bool menuaux);
    void slotOtherKeyChanged(t_keyword* key);

  signals:
    void keyChanged(t_keyword*);

  private:
    int              menuaux();
    void             read(QString& line, r_file& in);
    bool             evalIf(const QString& condString) const;
    t_inpfile*       inpfile;
    t_field*         field;
    QString          name;
    QStringList      comment;
    QStringList      selList;
    QStringList      originalSelList;
    t_keydesc*       desc;
    QStringList      valueList;
    int              updateFlag;
    int              panel;
    int              numberInPanel;
    int              lineNum;
    int              posBeg;
    int              posEnd;
    QString          mask;
    bool             _freeFormat;
    int              _freeFormatNumCols;
};

#endif
