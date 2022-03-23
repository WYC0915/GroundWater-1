
#ifndef MYHTML_H
#define MYHTML_H

#include <QtGui>

class t_myhtml : public QTextBrowser {
Q_OBJECT
 public:
  t_myhtml( QWidget* parent=0 );
  ~t_myhtml();
  virtual void setSource(const QUrl& url);
 private slots:
  void slotSourceChanged(const QUrl& src);
 private:
  bool isHtmlFile(const QString& fileName);
  QWidget* _parent;
};

#endif

