
#ifndef TEXTWIN_H
#define TEXTWIN_H

#include <qdialog.h>
#include <qpushbutton.h>
#include <qstring.h>
#include <qlineedit.h>
#include "myhtml.h"

class t_textwin : public QDialog {
  Q_OBJECT

  public:
    enum Modus {edit, browse, html};

    t_textwin(QWidget* parent, bool modal, const QString& fileName,
              Modus modus, const QString& keyName = "",
              const QString& action = "");

    ~t_textwin();
    bool close();
    void scrollToAnchor(const QString& anchor);

  private slots:
    void slotClose();
    void slotSave();
    void slotSearchForwards();
    void slotSearchBackwards();
    void slotEnableSearchButton(const QString& text);

  private:
    void displayFile(const QString& fileName, Modus modus,
                    const QString& keyName, const QString& action);

    QString          _fileName;
    QString          _showFile;
    QString          _action;
    QTextEdit*       _editor;
    t_myhtml*        _browser;
    QPushButton*     _closeButton;
    QPushButton*     _saveButton;
    QPushButton*     _forwButton;
    QPushButton*     _backButton;
    QLineEdit*       _searchInput;
    QPushButton*     _searchForwardsButton;
    QPushButton*     _searchBackwardsButton;
};

#endif

