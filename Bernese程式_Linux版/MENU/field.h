
#ifndef FIELD_H
#define FIELD_H

#include <QtCore>
#include <QtGui>

class t_canvas;
class t_keyword;
class t_panel;

class t_field : public QObject
{
  Q_OBJECT

  public:
    t_field(t_canvas* canvas, t_keyword* key, t_panel* panel);
    ~t_field();
    void       setFocus(){_widget->setFocus();};
    void       updateKeyValue();
    void       checkKey(QString& msg);
    void       setEnabled(bool enable);
    bool       enabled() const {return _enabled;}
    void       setValue(const QString& value);
    t_keyword* getKey(){return _key;};
    void       setButtonGroup(QButtonGroup* bg);
    bool       ok() const {return _ok;};

  private:
    QString    checkField(const QString& inp, unsigned ic = 0);

    t_canvas*  _canvas;
    t_keyword* _key;
    QWidget*   _widget;
    bool       _ok;
    bool       _enabled;
};

#endif
