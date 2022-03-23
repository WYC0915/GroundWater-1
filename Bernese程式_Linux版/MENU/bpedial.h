
#ifndef BPEDIAL_H
#define BPEDIAL_H

#include <qdialog.h>
#include <qstring.h>
#include <q3textview.h>
#include <qpushbutton.h>
//Added by qt3to4:
#include <QResizeEvent>

class t_bpe;

class t_bpedial : public QDialog
{
  Q_OBJECT

  public:
    t_bpedial(QWidget* parent, const QString& caption, t_bpe* bpe);
    ~t_bpedial();

    virtual void closeEvent(QCloseEvent* event);
    virtual bool close( bool alsoDelete );

    void setText(const QString& msg, int maxLines);
    void addText(const QString& msg, int maxLines);
    void enableClose();

  protected:
    void resizeEvent(QResizeEvent *e);

  private slots:
    void slotKill();
    void slotClose();

  private:
    Q3TextView*    _browser;
    QPushButton*  _closeButton;
    QPushButton*  _killButton;
    t_bpe*        _bpe;
};

#endif

