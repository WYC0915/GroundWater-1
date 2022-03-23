
#ifndef SELDIAL_H
#define SELDIAL_H

#include <qdialog.h>
#include <qlistwidget.h>
#include <qpushbutton.h>
#include <qstring.h>
#include <qstringlist.h>
//Added by qt3to4:
#include <QResizeEvent>

class t_seldial :
  public QDialog
{
  Q_OBJECT

  public:
    t_seldial(QWidget *parent, const QString caption,
              const QStringList& inpList, QStringList& outList, bool multi);
    ~t_seldial();

  protected:
    void resizeEvent(QResizeEvent *e);

  private slots:
    void accept(void);
    void selAll(void);

  private:
    QListWidget*  listBox;
    QPushButton*  allButton;
    QPushButton*  okButton;
    QPushButton*  cancelButton;
    QStringList*  p_outList;
};

#endif

