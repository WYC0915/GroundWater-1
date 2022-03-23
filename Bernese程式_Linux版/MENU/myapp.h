
#ifndef MYAPP_H
#define MYAPP_H

#include <QApplication>

class t_myapp : public QApplication {
  public:
    t_myapp(int &argc, char **argv);
    t_myapp(int &argc, char **argv, bool GUIenabled);
    ~t_myapp();
    virtual bool notify(QObject *receiver, QEvent *e);
};

#endif

