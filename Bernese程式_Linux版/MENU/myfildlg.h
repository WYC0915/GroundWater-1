
#ifndef MYFILDLG_H
#define MYFILDLG_H

#include <q3filedialog.h>
#include <qpushbutton.h>

#include "keyword.h"

class t_myfildlg : public Q3FileDialog {

  Q_OBJECT

  public:
    t_myfildlg( QWidget* parent, t_keyword* key,
                bool modal = true, bool multi = false );
    ~t_myfildlg();


    static QString getFileName(const QString& dir     = QString::null,
                               const QString& filter  = QString::null,
                               QWidget*       parent  = 0,
                               const char*    name    = 0,
                               const QString& caption = QString::null,
                               const QString& addDir  = QString::null);

    static QString getOpenFileName( const QString &initially = QString::null,
                                    const QString &filter = QString::null,
                                    QWidget *parent = 0, const char* name = 0,
                                    const QString &caption = QString::null,
                                    QString *selectedFilter = 0,
                                    bool resolveSymlinks = TRUE);

    static QStringList getOpenFileNames( const QString &filter= QString::null,
                                         const QString &dir = QString::null,
                                         QWidget *parent = 0,
                                         const char* name = 0,
                                         const QString &caption = QString::null,
                                         QString *selectedFilter = 0,
                                         bool resolveSymlinks = TRUE);
  private:
    t_keyword*   _key;
    QPushButton* _browseButton;
    QPushButton* _allButton;

  private slots:
    void slotBrowse();
    void slotSelectAll();
};

#endif

