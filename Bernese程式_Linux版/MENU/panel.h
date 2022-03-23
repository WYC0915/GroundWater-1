
#ifndef PANEL_H
#define PANEL_H

#include <QtCore>
#include <QtGui>

#include <Q3PtrList>
#include <qstring.h>
#include <qstringlist.h>

#include "keyword.h"

class r_file;

class t_panel
{
  public:
    t_panel(QString& line, r_file& in, Q3PtrList<t_keyword>* keys,
            int panNum);
    ~t_panel();

    const QStringList&  getPanelLine(void){return panelLine;};
    int                 getNumber(void)  const {return panelNumber;};
    int                 getNumKeys(void) const {return numKeys;};
    int                 getWidth() const {return width;};
    const QString&      missingKeys() const {return _missingKeys;};

    friend r_file& operator<<(r_file& os, const t_panel& panel);

    bool           isVisible();
    QButtonGroup*  getButtonGroup(){return buttonGroup;};

  private:
    int               panelNumber;
    int               numKeys;
    int               width;
    QStringList       panelLine;
    Q3PtrList<t_keyword>  condKey;
    QStringList       condRelOper;
    QStringList       condValue;
    QStringList       condLogOper;
    QButtonGroup*     buttonGroup;
    QString           _missingKeys;
};

#endif
