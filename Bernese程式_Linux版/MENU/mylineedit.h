
#ifndef MYLINEEDIT_H
#define MYLINEEDIT_H

#include <qlineedit.h>
//Added by qt3to4:
#include <QKeyEvent>

class t_mylineedit : public QLineEdit
{
  Q_OBJECT

  public:
    t_mylineedit(QWidget* parent, const char *name);
    ~t_mylineedit();

    QString text() const;
    virtual void setText(const QString & str);

  signals:
    void textChanged(QWidget*);

  protected:
    void keyPressEvent(QKeyEvent* e);

  private slots:
    void slotTextChanged();

  private:
    bool _hash;

};

#endif

