
#ifndef SELWIN_H
#define SELWIN_H

#include <QtCore>
#include <QtGui>

#include <Q3PtrList>
#include <q3listbox.h>

#include "keyword.h"

class t_canvas;

class t_selwin : public QLineEdit {
  Q_OBJECT

  public:
    t_selwin(t_canvas* canvas, QGridLayout* layout, t_keyword* Key,
             int row, int col, int colSpan);
    ~t_selwin();
    void setEnabled(bool enable);

  private slots:
    void slotEvalList(void);
    void slotEvalListSeldial(void);
    void slotTextChanged(void);
    void slotTimerTimeout(void);

  signals:
    void changed();

  private:
    t_keyword*      key;
    QPushButton*    selButton;
    QTimer*         timer;
};

#endif



