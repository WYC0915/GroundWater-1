
#ifndef R_DIR_H
#define R_DIR_H

#include <qdir.h>
#include <qobject.h>
#include <q3urloperator.h>
//Added by qt3to4:
#include <Q3ValueList>

class r_dir : public QObject {

  Q_OBJECT

  public:
    r_dir(const QString& path, const QString& nameFilter = QString::null,
          QDir::SortFlags sortSpec = QDir::Name | QDir::IgnoreCase,
          QDir::Filters filterSpec = QDir::All);

   virtual ~r_dir();

    static QChar separator();
    QString     dirName() const;
    void        setFilter(QDir::Filters filterSpec );
    void        setNameFilter(const QString& nameFilter);
    bool        exists() const;
    bool        exists(const QString& name, bool acceptAbsPath = TRUE );
    QString     path() const;
    bool        mkdir(const QString& dirName, bool acceptAbsPath = TRUE) const;
    static bool removeDir(const QString& dirName);
    static bool copyDir(const QString& fromPath, const QString& toPath);

    QStringList entryList(QDir::Filters filterSpec = QDir::NoFilter,
                          QDir::SortFlags sortSpec   = QDir::NoSort) const;

    QStringList entryList(const QString& nameFilter,
                          QDir::Filters filterSpec = QDir::NoFilter,
                          QDir::SortFlags sortSpec   = QDir::NoSort) const;

  private slots:
    void slotData(const QByteArray& buffer, Q3NetworkOperation* op);
    void slotNewChildren(const Q3ValueList<QUrlInfo>& urlInfo,
                         Q3NetworkOperation* op);

  private:
    QDir*                            _dir;
    Q3UrlOperator*                    _urlOp;
    mutable int                      _ansFlag;
    mutable const Q3NetworkOperation* _oper;
    mutable Q3ValueList<QUrlInfo>     _children;
};
#endif

