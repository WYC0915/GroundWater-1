
#ifndef CANVAS_H
#define CANVAS_H

#include <QtCore>
#include <QtGui>
#include <q3dict.h>

#include "inpfile.h"
#include "field.h"
#include "panel.h"

class t_canvas : public QWidget {
  Q_OBJECT

  public:
    t_canvas(QWidget* parent);
    ~t_canvas();

    void    showPanel(t_inpfile* inpfile);
    void    showMessage(const QString& message);
    void    savePanel();
    int     checkPanel(int currentSlotMenuItem);
    void    checkAbbreviations(QString& msg);
    QString focusWidgetName();
    void    addWidget(QWidget* widget, int row, int col, int rSpan, int cSpan,
                      const QString& toolTip = "");

  private slots:
    void  slotFieldChanged();
    void  slotRadioButtonChanged();

  signals:
    void fieldChanged();

  private:
    t_panel*               _panel;
    Q3PtrList<t_field>*    _fields;
    Q3Dict<QButtonGroup>*  _buttonGroup;
    QGridLayout*           _layout;
};

#endif
