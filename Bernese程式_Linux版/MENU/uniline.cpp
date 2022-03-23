
/* -------------------------------------------------------------------------
 * Bernese GPS Software Version 5.0
 * -------------------------------------------------------------------------
 *
 * Class:      t_uniline
 *
 * Purpose:    This class reimplements the QTableWidget class
 *
 * Author:     L. Mervart
 *
 * Created:    18-APR-2000
 *
 * Changes:
 *
 * Copyright:  Astronomical Institute
 *              University of Berne
 *                  Switzerland
 * -----------------------------------------------------------------------*/

#include "uniline.h"
#include "menutils.h"
#include "errormsg.h"
#include "canvas.h"
#include "keyword.h"

static const char * plus_xpm[] = {
"16 16 2 1",
"  c #FFFFFFFFFFFF",
". c #00000000FFFF",
"                ",
"      ...       ",
"      ...       ",
"      ...       ",
"      ...       ",
"      ...       ",
" .............. ",
" .............. ",
" .............. ",
"      ...       ",
"      ...       ",
"      ...       ",
"      ...       ",
"      ...       ",
"                ",
"                "};

static const char * minus_xpm[] = {
"16 16 2 1",
"         c #FFFFFFFFFFFF",
".        c #00000000FFFF",
"                ",
"                ",
"                ",
"                ",
"                ",
"                ",
" .............. ",
" .............. ",
" .............. ",
"                ",
"                ",
"                ",
"                ",
"                ",
"                ",
"                "};

//
////////////////////////////////////////////////////////////////////////////
QWidget* t_delegate::createEditor(QWidget* parent,
                                  const QStyleOptionViewItem& option,
                                  const QModelIndex & index ) const {
  if (_uniline->editor()) {
    return 0;
  }
  QWidget* editor = QItemDelegate::createEditor(parent, option, index);
  _uniline->setEditor(editor);
  editor->setObjectName(_uniline->_key->getName().latin1());
  editor->installEventFilter(_uniline->tabPressEater());
  editor->setBackgroundRole(QPalette::AlternateBase);
  return editor;
}

// Constructor
////////////////////////////////////////////////////////////////////////////
t_uniline::t_uniline(t_canvas* canvas, t_keyword* key, int row, int col) :
                     QTableWidget(canvas) {

  _key = key;

  _prefix_hash = 0;

  // Parse the widget mask
  // ---------------------
  QString     mask   = key->getMask();
  QStringList labels = mask.split(QRegExp("\\s"),QString::SkipEmptyParts);

  bool allEditable = true;
  for (int ic = 1; ic <= labels.size(); ic++) {
    if ( !key->getDesc()->isEditable(ic) ) {
      allEditable = false;
      break;
    }
  }
  if (allEditable) {
    _numButtons = 2;
    labels.append("__");
    labels.append("__");
  }
  else {
    _numButtons = 0;
  }

  setColumnCount(labels.size());

  // List of Strings to be displayed
  // -------------------------------
  QStringList textList = key->getValList();

  // Default first row (if not available)
  // ------------------------------------
  if (textList.size() == 0) {
    QString line;
    for (int ii = 0; ii < labels.size(); ii++) {
      line += QString("\" \"");
    }
    textList << line;
  }

  // Fill the table
  // --------------
  QStringListIterator ir(textList); int iRow = -1;

  while (ir.hasNext()) {
    insertRow(++iRow);

    QString uLine = ir.next();
    QStringList colList = QStringList::split(QRegExp("\"\\s+\""), uLine, true);

    if (!_prefix_hash) {
      _prefix_hash = new bool[colList.size()];
    }

    QStringListIterator ic(colList); int iCol = -1;
    while (ic.hasNext()) {
      ++iCol;
      QString txt = ic.next();
      txt = txt.replace(QRegExp("\""),"");;
      bool editable = true;
      if (txt.indexOf('#') == 0 && txt.lastIndexOf('#') == txt.length() - 1 &&
          txt.length() > 1) {
        txt = txt.mid(1,txt.length()-2);
        editable = false;
        _prefix_hash[iCol] = true;
      }
      else {
        _prefix_hash[iCol] = false;
      }
      if (!_key->getDesc()->isEditable(iCol+1)) {
        editable = false;
      }
      QTableWidgetItem* nItem = new QTableWidgetItem(txt);
      if ( labels[iCol].indexOf('_') == 0 ) {
        nItem->setTextAlignment(Qt::AlignRight | Qt::AlignVCenter);
      }
      else {
        nItem->setTextAlignment(Qt::AlignLeft | Qt::AlignVCenter);
      }
      if ( !editable || !key->getDesc()->isEditable(iCol+1) ) {
        nItem->setFlags(nItem->flags() & ~Qt::ItemIsEnabled);
        nItem->setFlags(nItem->flags() & ~Qt::ItemIsEditable);
      }
      setItem(iRow, iCol, nItem);
      ////      resizeColumnToContents(iCol);
    }
    addButtons(iRow);
  }

  // A few general settings
  // ----------------------
  horizontalHeader()->setResizeMode(QHeaderView::Custom);
  int ww = QFontMetrics(canvas->font()).width('W');
  for (int iCol = 0; iCol < columnCount(); iCol++) {
    horizontalHeader()->resizeSection(iCol, labels[iCol].length() * ww + ww);
    labels[iCol].replace('_', ' ');
  }
  setHorizontalHeaderLabels(labels);
  horizontalHeader()->setStretchLastSection(true);
  verticalHeader()->hide();
  setVerticalScrollBarPolicy(Qt::ScrollBarAlwaysOn);
  setSelectionMode(QAbstractItemView::NoSelection);
  setEditTriggers(QAbstractItemView::DoubleClicked);

  if (_numButtons > 0) {
    connect(this, SIGNAL(itemClicked(QTableWidgetItem*)),
            this, SLOT(slotRepeat(QTableWidgetItem*)));
    connect(this, SIGNAL(itemClicked(QTableWidgetItem*)),
            this, SLOT(slotDelete(QTableWidgetItem*)));
  }

  _editor = 0;

  this->setItemDelegate(&_itemDelegate);
  _itemDelegate.setUniline(this);

  // Place widget on canvas
  // ----------------------
  int rSpan;
  if (key->getDesc() && !key->getDesc()->numlines().isEmpty()) {
    rSpan = key->getDesc()->numlines().toInt();
  }
  else {
    rSpan = textList.size();
    if (rSpan <  5) rSpan =  5;
    if (rSpan > 30) rSpan = 30;
  }

  int cSpan = mask.length();

  setObjectName(key->getName().latin1());

  canvas->addWidget(this, row, col, rSpan, cSpan, key->getName());
}

// Destructor
////////////////////////////////////////////////////////////////////////////
t_uniline::~t_uniline() {
  delete [] _prefix_hash;
}

// Text (all rows)
////////////////////////////////////////////////////////////////////////////
QStringList t_uniline::text() {
  if (_editor) {
    commitData(_editor);
    closeEditor(_editor, QAbstractItemDelegate::NoHint);
  }
  QStringList textList;
  for (int iRow = 0; iRow < rowCount(); iRow++) {
    QString line;
    for (int iCol = 0; iCol < columnCount()-_numButtons; iCol++) {
      QString txt;
      if (item(iRow, iCol)) {
        txt = item(iRow, iCol)->text().stripWhiteSpace();
      }
      line += '"';
      if (_prefix_hash && _prefix_hash[iCol]) {
        line += '#';
      }
      line += txt;
      if (_prefix_hash && _prefix_hash[iCol]) {
        line += '#';
      }
      line += '"' + QString(" ");
    }
    textList.append(line);
  }
  return textList;
}

// Delete Row
////////////////////////////////////////////////////////////////////////////
void t_uniline::slotDelete(QTableWidgetItem* it) {
  if (_numButtons != 2 || column(it) != columnCount() - 1) {
    return;
  }
  if (rowCount() > 1) {
    removeRow(row(it));
  }
}

// Repeat Row
////////////////////////////////////////////////////////////////////////////
void t_uniline::slotRepeat(QTableWidgetItem* it) {

  if (_numButtons != 2 || column(it) != columnCount() - 2) {
    return;
  }

  int oldRow = row(it);
  int newRow = oldRow + 1;
  insertRow(newRow);

  for (int iCol = 0; iCol < columnCount()-2; iCol++) {
    if ( item(oldRow,iCol) ) {
      QTableWidgetItem* nItem = item(oldRow,iCol)->clone();
      setItem(newRow, iCol, nItem);
    }
  }

  addButtons(newRow);
}

// Add Buttons
////////////////////////////////////////////////////////////////////////////
void t_uniline::addButtons(int iRow) {
  const QPixmap plusXPM( plus_xpm );
  const QPixmap minusXPM( minus_xpm );
  const QIcon   plusIcon( plusXPM );
  const QIcon   minusIcon( minusXPM );

  if (_numButtons > 0) {
    QTableWidgetItem* nItem = new QTableWidgetItem();
    nItem->setIcon(minusIcon);
    nItem->setTextAlignment(Qt::AlignCenter);
    nItem->setFlags(nItem->flags() & ~Qt::ItemIsEditable);
    setItem(iRow, columnCount()-1, nItem);
    ////    resizeColumnToContents(columnCount()-1);
  }
  if (_numButtons > 1) {
    QTableWidgetItem* nItem = new QTableWidgetItem();
    nItem->setIcon(plusIcon);
    nItem->setTextAlignment(Qt::AlignCenter);
    nItem->setFlags(nItem->flags() & ~Qt::ItemIsEditable);
    setItem(iRow, columnCount()-2, nItem);
    ////    resizeColumnToContents(columnCount()-2);
  }
}

// Close Editor
////////////////////////////////////////////////////////////////////////////
void t_uniline::closeEditor(QWidget* editor,
                            QAbstractItemDelegate::EndEditHint hint) {
  _editor = 0;
  QAbstractItemView::closeEditor(editor, hint);
}

// Tabular Press Eater
////////////////////////////////////////////////////////////////////////////
bool t_tabPressEater::eventFilter(QObject* obj, QEvent* event) {
  if (event->type() == QEvent::KeyPress) {
    QKeyEvent* keyEvent = static_cast<QKeyEvent *>(event);
    if (keyEvent->key() == Qt::Key_Tab) {
      return true;
    }
  }
  return QObject::eventFilter(obj, event);
}
