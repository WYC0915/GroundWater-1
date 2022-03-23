
#ifndef UNILINE_H
#define UNILINE_H

#include <QtCore>
#include <QtGui>

class t_canvas;
class t_keyword;
class t_uniline;

class t_tabPressEater : public QObject {
  Q_OBJECT
  protected:
    bool eventFilter(QObject* obj, QEvent* event);
};

class t_delegate : public QItemDelegate {
  public:
    t_delegate(QObject* parent = 0) : QItemDelegate(parent) {};
    ~t_delegate() {};
    virtual QWidget* createEditor(QWidget* parent,
                                  const QStyleOptionViewItem& option,
                                  const QModelIndex & index ) const;
    void setUniline(t_uniline* uniline){_uniline = uniline;}
  private:
    t_uniline* _uniline;
};

class t_uniline : public QTableWidget {

  Q_OBJECT

  friend class t_delegate;

  public:
    t_uniline(t_canvas* canvas, t_keyword* key, int row, int col);
    ~t_uniline();
    QStringList text();
    void setEditor(QWidget* editor) {_editor = editor;}
    QWidget* editor(){return _editor;}

  protected slots:
    virtual void closeEditor(QWidget* editor,
                             QAbstractItemDelegate::EndEditHint hint);
  private slots:
    void    slotDelete(QTableWidgetItem* item);
    void    slotRepeat(QTableWidgetItem* item);

  private:
    void addButtons(int iRow);
    t_tabPressEater* tabPressEater() {return &_tabPressEater;}
    int        _numButtons;
    t_delegate _itemDelegate;
    QWidget*   _editor;
    t_tabPressEater _tabPressEater;
    t_keyword* _key;
    bool* _prefix_hash;
};

#endif
