
#ifndef DATEDIAL_H
#define DATEDIAL_H

#include <QtCore>
#include <QtGui>

#include "juldat.h"
#include "selwin.h"

class t_datedial :
  public QDialog
{
  Q_OBJECT

  public:
    t_datedial(bool showIt = true);
    ~t_datedial();
    void setPlus();
    void setMinus();
  protected:
    virtual void closeEvent(QCloseEvent* event);

  signals:
    void dateAccept();
    void dateClosed();

  private slots:
    void slotPlus();
    void slotMinus();
    void slotOK();
    void compute();
    void today();
    void slotHlp();
    void slotSet();
    void setYMD();
    void setMJD();
    void setGPS();
    void setDOY();

  private:
    void showAll();
    void hideAll();
    t_juldat*     _juldat;

    QLineEdit*    _edt_ymd;
    QLabel*       _lab_ymd;
    QPushButton*  _but_ymd;

    QLineEdit*    _edt_mjd;
    QLabel*       _lab_mjd;
    QPushButton*  _but_mjd;

    QLineEdit*    _edt_gps;
    QLabel*       _lab_gps;
    QPushButton*  _but_gps;

    QLineEdit*    _edt_doy;
    QLabel*       _lab_doy;
    QPushButton*  _but_doy;

    QLineEdit*    _edt_chr;
    QLabel*       _lab_chr;

    t_keyword*    _sessionKey;
    t_selwin*     _sessionTable;
    QLabel*       _lab_tab;

    QLineEdit*    _edt_job;
    QLabel*       _lab_job;

    QPushButton*  _plusButton;
    QPushButton*  _minusButton;
    QPushButton*  _computeButton;
    QPushButton*  _todayButton;
    QPushButton*  _hlpButton;
    QPushButton*  _setButton;
    QPushButton*  _okButton;
    QPushButton*  _cancelButton;
};

#endif



